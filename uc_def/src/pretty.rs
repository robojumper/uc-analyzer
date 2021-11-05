use std::io;

use crate::{
    ClassDef, ClassHeader, ConstDef, DelegateDef, EnumDef, FuncDef, Hir, Identifier, StructDef, Ty,
    VarDef,
};

pub trait RefLookup {
    type From;
    fn lookup<'a>(&self, i: &'a Self::From) -> &'a str;
}

pub struct IdentifierFormat;

impl RefLookup for IdentifierFormat {
    type From = Identifier;

    fn lookup<'a>(&self, i: &'a Self::From) -> &'a str {
        i.as_ref()
    }
}

struct PPrinter<W: io::Write, R: RefLookup> {
    lk: R,
    w: W,
    indent: Vec<u8>,
}

impl<W: io::Write, R: RefLookup> PPrinter<W, R> {
    fn format_i(&mut self, i: &R::From) -> io::Result<()> {
        self.w.write_all(self.lk.lookup(i).as_bytes())
    }

    fn format_veci(&mut self, veci: &[R::From], sep: &'static [u8]) -> io::Result<()> {
        veci.iter()
            .map(|i| self.lk.lookup(i).as_bytes())
            .intersperse(sep)
            .map(|s| self.w.write_all(s))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(())
    }

    fn format_class(&mut self, header: &ClassDef<R::From>) -> io::Result<()> {
        match &header.kind {
            ClassHeader::Class {
                extends,
                implements,
                within,
                ..
            } => {
                self.w.write_all(b"class ")?;
                self.w.write_all(header.name.as_ref().as_bytes())?;
                match extends {
                    Some(i) => {
                        self.w.write_all(b" extends ")?;
                        self.format_i(i)?;
                    }
                    None => {}
                }
                match within {
                    Some(i) => {
                        self.w.write_all(b" within ")?;
                        self.format_i(i)?;
                    }
                    None => {}
                }
                match &implements[..] {
                    [] => {}
                    [x @ ..] => {
                        self.w.write_all(b" implements(")?;
                        self.format_veci(x, b", ")?;
                        self.w.write_all(b")")?
                    }
                }
            }
            ClassHeader::Interface { extends } => {
                self.w.write_all(b"interface ")?;
                self.w.write_all(header.name.as_ref().as_bytes())?;
                match extends {
                    Some(i) => {
                        self.w.write_all(b" extends ")?;
                        self.format_i(i)?;
                    }
                    None => {}
                }
            }
        }

        self.w.write_all(b";\n\n")?;

        Ok(())
    }

    fn format_struct(&mut self, s: &StructDef<R::From>) -> io::Result<()> {
        self.w.write_all(b"struct ")?;
        self.w.write_all(s.name.as_ref().as_bytes())?;
        match &s.extends {
            Some(i) => {
                self.w.write_all(b" extends ")?;
                self.format_i(i)?;
            }
            None => {}
        }
        self.w.write_all(b" {\n")?;
        self.indent.extend_from_slice(b"    ");

        for var in &s.fields {
            self.format_var(var)?;
        }

        self.indent.truncate(self.indent.len() - 4);

        self.w.write_all(b"};\n\n")?;
        Ok(())
    }

    fn format_ty(&mut self, s: &Ty<R::From>) -> io::Result<()> {
        match s {
            Ty::Simple(i) => {
                self.format_i(i)?;
            }
            Ty::Array(i) => {
                self.w.write_all(b"array<")?;
                self.format_i(i)?;
                self.w.write_all(b">")?;
            }
            Ty::Class(ci) => {
                self.w.write_all(b"class")?;
                if let Some(i) = ci {
                    self.w.write_all(b"<")?;
                    self.format_i(i)?;
                    self.w.write_all(b">")?;
                }
            }
            Ty::Delegate(parts) => {
                self.w.write_all(b"delegate(")?;
                match &parts[..] {
                    [] => {}
                    [x @ ..] => {
                        self.format_veci(x, b".")?;
                    }
                }
                self.w.write_all(b")")?;
            }
        }
        Ok(())
    }

    fn format_enum(&mut self, s: &EnumDef) -> io::Result<()> {
        self.w.write_all(b"enum ")?;
        self.w.write_all(s.name.as_ref().as_bytes())?;
        self.w.write_all(b" {\n")?;
        for var in &s.variants {
            self.w.write_all(b"    ")?;
            self.w.write_all(var.as_ref().as_bytes())?;
            self.w.write_all(b",\n")?;
        }
        self.w.write_all(b"};\n\n")?;
        Ok(())
    }

    fn format_const(&mut self, c: &ConstDef) -> io::Result<()> {
        self.w.write_all(b"const ")?;
        self.w.write_all(c.name.as_ref().as_bytes())?;
        self.w.write_all(b" = ")?;
        self.w.write_fmt(format_args!("{:?}", c.val))?;
        self.w.write_all(b";\n")?;
        Ok(())
    }

    fn format_var(&mut self, s: &VarDef<R::From>) -> io::Result<()> {
        self.w.write_all(&self.indent)?;
        self.w.write_all(b"var ")?;
        self.format_ty(&s.ty)?;
        self.w.write_all(b" ")?;

        for (idx, inst) in s.names.iter().enumerate() {
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            match &inst.count {
                crate::DimCount::None => {}
                crate::DimCount::Number(n) => {
                    self.w.write_all(b"[")?;
                    self.w.write_fmt(format_args!("{}", n))?;
                    self.w.write_all(b"]")?;
                }
                crate::DimCount::Complex(ci) => {
                    self.w.write_all(b"[")?;
                    self.format_veci(ci, b".")?;
                    self.w.write_all(b"]")?;
                }
            }
            if idx != s.names.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }

        self.w.write_all(b";\n")?;

        Ok(())
    }

    fn format_del(&mut self, f: &DelegateDef<R::From>) -> io::Result<()> {
        self.w.write_all(b"delegate ")?;
        if let Some(ty) = &f.sig.ret_ty {
            self.format_ty(ty)?;
            self.w.write_all(b" ")?;
        }
        self.w.write_all(f.name.as_ref().as_bytes())?;
        self.w.write_all(b"(")?;
        for (idx, inst) in f.sig.args.iter().enumerate() {
            self.format_ty(&inst.ty)?;
            self.w.write_all(b" ")?;
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            if let Some(c) = &inst.val {
                self.w.write_all(b" = ")?;
                self.w.write_fmt(format_args!("{:?}", c))?;
            }

            if idx != f.sig.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b");\n")?;
        Ok(())
    }

    fn format_func(&mut self, f: &FuncDef<R::From>) -> io::Result<()> {
        self.w.write_all(b"function ")?;
        if let Some(ty) = &f.sig.ret_ty {
            self.format_ty(ty)?;
            self.w.write_all(b" ")?;
        }
        self.w.write_all(f.name.as_ref().as_bytes())?;
        self.w.write_all(b"(")?;
        for (idx, inst) in f.sig.args.iter().enumerate() {
            self.format_ty(&inst.ty)?;
            self.w.write_all(b" ")?;
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            if let Some(c) = &inst.val {
                self.w.write_all(b" = ")?;
                self.w.write_fmt(format_args!("{:?}", c))?;
            }

            if idx != f.sig.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b")")?;

        match &f.body {
            // TODO
            Some(_b) => self.w.write_all(b" { ... }\n")?,
            None => self.w.write_all(b";\n")?,
        }
        Ok(())
    }
}

pub fn format_hir<W: io::Write, I, R: RefLookup<From = I>>(
    hir: &Hir<I>,
    w: &mut W,
    r: R,
) -> io::Result<()> {
    let mut printer = PPrinter {
        lk: r,
        w,
        indent: vec![],
    };
    printer.format_class(&hir.header)?;
    printer.w.write_all(b"\n")?;

    for s in &hir.structs {
        printer.format_struct(s)?;
    }
    printer.w.write_all(b"\n")?;

    for e in &hir.enums {
        printer.format_enum(e)?;
    }
    printer.w.write_all(b"\n")?;

    for c in &hir.consts {
        printer.format_const(c)?;
    }
    printer.w.write_all(b"\n")?;

    for var in &hir.vars {
        printer.format_var(var)?;
    }
    printer.w.write_all(b"\n")?;

    for del in &hir.dels {
        printer.format_del(del)?;
    }
    printer.w.write_all(b"\n")?;

    for f in &hir.funcs {
        printer.format_func(f)?;
    }

    Ok(())
}

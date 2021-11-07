use std::{fmt, io};

use crate::{
    BaseExpr, ClassDef, ClassHeader, ConstDef, DelegateDef, EnumDef, FuncBody, FuncDef, Hir,
    Identifier, Local, StructDef, Ty, VarDef, VarInstance,
};

mod stmts;

pub trait RefLookup {
    type From: fmt::Debug;
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
    fn indent_incr(&mut self) {
        self.indent.extend_from_slice(b"    ");
    }

    fn indent_decr(&mut self) {
        self.indent.truncate(self.indent.len() - 4);
    }

    fn indent(&mut self) -> io::Result<()> {
        self.w.write_all(&self.indent)
    }

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
        self.indent_incr();

        for var in &s.fields {
            self.format_var(var)?;
        }

        self.indent_decr();

        self.w.write_all(b"};\n\n")?;
        Ok(())
    }

    fn format_ty(&mut self, s: &Ty<R::From>) -> io::Result<()> {
        match s {
            Ty::Simple(i) => {
                self.format_i(i)?;
            }
            Ty::Qualified(i) => {
                self.format_veci(i, b".")?;
            }
            Ty::Array(i) => {
                self.w.write_all(b"array<")?;
                self.format_ty(i)?;
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
        self.indent()?;
        self.w.write_all(b"var ")?;
        self.format_ty(&s.ty)?;
        self.w.write_all(b" ")?;

        self.format_instances(&s.names)?;

        self.w.write_all(b";\n")?;

        Ok(())
    }

    fn format_instances(&mut self, i: &[VarInstance<R::From>]) -> io::Result<()> {
        for (idx, inst) in i.iter().enumerate() {
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
            if idx != i.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
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
            if let Some(c) = &inst.def {
                self.w.write_all(b" = ")?;
                self.w.write_fmt(format_args!("{:?}", c))?;
            }

            if idx != f.sig.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b")")?;
        match &f.body {
            Some(b) => {
                self.format_body(b)?;
            }
            None => self.w.write_all(b";\n")?,
        }
        Ok(())
    }

    fn format_func(&mut self, f: &FuncDef<R::From>) -> io::Result<()> {
        self.w.write_all(b"function ")?;
        if let Some(ty) = &f.sig.ret_ty {
            self.format_ty(ty)?;
            self.w.write_all(b" ")?;
        }
        match &f.name {
            crate::FuncName::Oper(op) => self.format_op(op)?,
            crate::FuncName::Iden(i) => self.w.write_all(i.as_ref().as_bytes())?,
        }

        self.w.write_all(b"(")?;
        for (idx, inst) in f.sig.args.iter().enumerate() {
            self.format_ty(&inst.ty)?;
            self.w.write_all(b" ")?;
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            if let Some(c) = &inst.def {
                self.w.write_all(b" = ")?;
                self.format_expr(c)?;
            }

            if idx != f.sig.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b")")?;

        match &f.body {
            Some(b) => {
                self.format_body(b)?;
            }
            None => self.w.write_all(b";\n")?,
        }
        Ok(())
    }

    fn format_body(&mut self, f: &FuncBody<R::From>) -> io::Result<()> {
        self.w.write_all(b" {\n")?;
        self.indent_incr();

        for local in &f.locals {
            self.format_local(local)?;
        }

        for stmt in &f.statements {
            self.indent()?;
            self.format_statement(stmt)?;
            self.w.write_all(b"\n")?;
        }

        self.indent_decr();
        self.w.write_all(b"}\n")?;
        Ok(())
    }

    fn format_local(&mut self, l: &Local<R::From>) -> io::Result<()> {
        self.indent()?;
        self.w.write_all(b"local ")?;
        self.format_ty(&l.ty)?;
        self.w.write_all(b" ")?;

        self.format_instances(&l.names)?;

        self.w.write_all(b";\n")?;

        Ok(())
    }
}

pub fn format_base_expr<W: io::Write, I, R: RefLookup<From = I>>(
    expr: &BaseExpr<I>,
    w: &mut W,
    r: R,
) -> io::Result<()> {
    let mut printer = PPrinter {
        lk: r,
        w,
        indent: vec![],
    };
    printer.format_expr(expr)
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

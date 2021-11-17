use std::io;

use uc_def::{ArgFlags, FuncFlags};

use crate::{
    ClassDef, ClassHeader, ConstDef, DimCount, EnumDef, Expr, FuncBody, FuncDef, FuncName, FuncSig,
    Hir, Identifier, Local, StateDef, StructDef, Ty, VarDef, VarInstance,
};

mod stmts;

struct PPrinter<W: io::Write> {
    w: W,
    indent: Vec<u8>,
}

impl<W: io::Write> PPrinter<W> {
    fn indent_incr(&mut self) {
        self.indent.extend_from_slice(b"    ");
    }

    fn indent_decr(&mut self) {
        self.indent.truncate(self.indent.len() - 4);
    }

    fn indent(&mut self) -> io::Result<()> {
        self.w.write_all(&self.indent)
    }

    fn format_i(&mut self, i: &Identifier) -> io::Result<()> {
        self.w.write_all(i.as_ref().as_bytes())
    }

    fn format_veci(&mut self, veci: &[Identifier], sep: &'static [u8]) -> io::Result<()> {
        veci.iter()
            .map(|i| i.as_ref().as_bytes())
            .intersperse(sep)
            .map(|s| self.w.write_all(s))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(())
    }

    fn format_class(&mut self, header: &ClassDef) -> io::Result<()> {
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

    fn format_struct(&mut self, s: &StructDef) -> io::Result<()> {
        self.w.write_all(b"struct ")?;
        self.w.write_all(s.name.as_ref().as_bytes())?;
        match &s.extends {
            Some(i) => {
                self.w.write_all(b" extends ")?;
                self.format_veci(i, b".")?;
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

    fn format_ty(&mut self, s: &Ty) -> io::Result<()> {
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

    fn format_var(&mut self, s: &VarDef) -> io::Result<()> {
        self.indent()?;
        self.w.write_all(b"var ")?;
        self.format_ty(&s.ty)?;
        self.w.write_all(b" ")?;

        self.format_instances(&s.names)?;

        self.w.write_all(b";\n")?;

        Ok(())
    }

    fn format_instances(&mut self, i: &[VarInstance]) -> io::Result<()> {
        for (idx, inst) in i.iter().enumerate() {
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            match &inst.count {
                DimCount::None => {}
                DimCount::Number(n) => {
                    self.w.write_all(b"[")?;
                    self.w.write_fmt(format_args!("{}", n))?;
                    self.w.write_all(b"]")?;
                }
                DimCount::Complex(ci) => {
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

    fn format_func(&mut self, f: &FuncDef) -> io::Result<()> {
        self.indent()?;
        if f.mods.flags.contains(FuncFlags::OPERATOR) {
            self.w.write_all(b"operator ")?;
        } else if f.mods.flags.contains(FuncFlags::ITERATOR) {
            self.w.write_all(b"iterator ")?;
        } else if f.mods.flags.contains(FuncFlags::DELEGATE) {
            self.w.write_all(b"delegate ")?;
        } else {
            self.w.write_all(b"function ")?;
        }

        self.format_sig(&f.sig, &f.name)?;

        match &f.body {
            Some(b) => {
                self.format_body(b)?;
            }
            None => self.w.write_all(b";\n")?,
        }
        Ok(())
    }

    fn format_sig(&mut self, f: &FuncSig, name: &FuncName) -> io::Result<()> {
        if let Some(ty) = &f.ret_ty {
            self.format_ty(ty)?;
            self.w.write_all(b" ")?;
        }

        match name {
            FuncName::Oper(op) => {
                self.format_op(op)?;
                self.w.write_all(b" ")?;
            }
            FuncName::Iden(i) => self.w.write_all(i.as_ref().as_bytes())?,
        }

        self.w.write_all(b"(")?;
        for (idx, inst) in f.args.iter().enumerate() {
            if inst.mods.flags.contains(ArgFlags::OUT) {
                self.w.write_all(b"out ")?;
            }
            self.format_ty(&inst.ty)?;
            self.w.write_all(b" ")?;
            self.w.write_all(inst.name.as_ref().as_bytes())?;
            if let Some(c) = &inst.def {
                self.w.write_all(b" = ")?;
                self.format_expr(c)?;
            }

            if idx != f.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b")")?;

        Ok(())
    }

    fn format_body(&mut self, f: &FuncBody) -> io::Result<()> {
        self.w.write_all(b" {\n")?;
        self.indent_incr();

        for local in &f.locals {
            self.format_local(local)?;
        }

        self.w.write_all(b"\n")?;
        for c in &f.consts {
            self.indent()?;
            self.format_const(c)?;
        }
        self.w.write_all(b"\n")?;

        for stmt in &f.statements {
            self.indent()?;
            self.format_statement(stmt)?;
            self.w.write_all(b"\n")?;
        }

        self.indent_decr();
        self.indent()?;
        self.w.write_all(b"}\n")?;
        Ok(())
    }

    fn format_local(&mut self, l: &Local) -> io::Result<()> {
        self.indent()?;
        self.w.write_all(b"local ")?;
        self.format_ty(&l.ty)?;
        self.w.write_all(b" ")?;

        self.format_instances(&l.names)?;

        self.w.write_all(b";\n")?;

        Ok(())
    }

    fn format_state(&mut self, s: &StateDef) -> io::Result<()> {
        self.w.write_all(b"state ")?;
        self.format_i(&s.name)?;
        if let Some(extends) = &s.extends {
            self.w.write_all(b" extends ")?;
            self.format_i(extends)?;
        }
        self.w.write_all(b" {\n")?;
        self.indent_incr();

        for f in &s.funcs {
            self.format_func(f)?;
        }

        for stmt in &s.statements {
            self.indent()?;
            self.format_statement(stmt)?;
            self.w.write_all(b"\n")?;
        }

        self.indent_decr();
        self.w.write_all(b"}\n")?;

        Ok(())
    }
}

pub fn format_base_expr<W: io::Write>(expr: &Expr, w: &mut W) -> io::Result<()> {
    let mut printer = PPrinter { w, indent: vec![] };
    printer.format_expr(expr)
}

pub fn format_hir<W: io::Write>(hir: &Hir, w: &mut W) -> io::Result<()> {
    let mut printer = PPrinter { w, indent: vec![] };
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

    for state in &hir.states {
        printer.format_state(state)?;
    }
    printer.w.write_all(b"\n")?;

    for f in &hir.funcs {
        printer.format_func(f)?;
    }

    Ok(())
}

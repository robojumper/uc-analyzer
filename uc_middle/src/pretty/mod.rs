use std::io;

use uc_def::{ArgFlags, FuncFlags};
use uc_name::Identifier;

use crate::{
    body::Literal, ty::Ty, ClassKind, Const, DefId, DefKind, Defs, Enum, FuncSig, Function,
    Operator, State, Struct, Var,
};

struct PPrinter<'a, W: io::Write> {
    w: W,
    defs: &'a Defs,
    indent: Vec<u8>,
}

impl<W: io::Write> PPrinter<'_, W> {
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
        self.w(i.as_ref().as_bytes())
    }

    fn w(&mut self, bytes: &[u8]) -> io::Result<()> {
        self.w.write_all(bytes)
    }

    fn format_struct(&mut self, s: &Struct) -> io::Result<()> {
        self.w(b"struct ")?;
        self.format_i(&s.name)?;
        if let Some(e) = s.extends {
            let e_s = self.defs.get_struct(e);
            self.w(b"extends ")?;
            self.format_i(&e_s.name)?;
        }
        self.w(b" {\n")?;
        self.indent_incr();
        for &v in s.vars.iter() {
            self.format_var(self.defs.get_var(v))?;
        }
        self.indent_decr();

        self.w(b"};\n\n")?;
        Ok(())
    }

    fn format_enum(&mut self, e: &Enum) -> io::Result<()> {
        self.w(b"enum ")?;
        self.format_i(&e.name)?;
        self.w(b" {\n")?;
        for &var in e.variants.iter() {
            self.w(b"    ")?;
            self.format_i(&self.defs.get_variant(var).name)?;
            self.w(b",\n")?;
        }
        self.w(b"};\n\n")?;
        Ok(())
    }

    fn format_var(&mut self, v: &Var) -> io::Result<()> {
        self.indent()?;
        self.w(b"var ")?;
        self.format_ty(v.ty.unwrap())?;

        self.w(b" ")?;
        self.format_i(&v.name)?;

        self.w(b";\n")?;

        Ok(())
    }

    fn format_const(&mut self, c: &Const) -> io::Result<()> {
        self.w(b"const ")?;
        self.w(c.name.as_ref().as_bytes())?;
        self.w(b" = ")?;

        self.format_literal(c.val.as_ref().unwrap())?;
        self.w(b";\n")?;
        Ok(())
    }

    fn format_literal(&mut self, l: &Literal) -> io::Result<()> {
        match l {
            Literal::None => self.w(b"none"),
            Literal::Bool(b) => {
                if *b {
                    self.w(b"true")
                } else {
                    self.w(b"false")
                }
            }
            Literal::Int(i) => self.w.write_fmt(format_args!("{}", i)),
            Literal::Float(f) => self.w.write_fmt(format_args!("{}", f)),
            Literal::Name => self.w(b"'some_name'"),
            Literal::String => self.w(b"\"string\""),
            Literal::Byte(b) => self.w.write_fmt(format_args!("{}", b)),
            Literal::Object(d) => {
                self.format_ty(Ty::object_from(*d))?;
                self.w(b"'some_object'")
            }
            Literal::Class(c) => {
                self.w(b"class'")?;
                self.format_ty(Ty::object_from(*c))?;
                self.w(b"'")
            }
            Literal::Struct(s) => self.format_ty(Ty::struct_from(*s)),
        }
    }

    fn format_state(&mut self, s: &State) -> io::Result<()> {
        self.w(b"state ")?;
        self.format_i(&s.name)?;
        if let Some(e) = s.extends {
            self.w(b" extends ")?;
            self.format_i(&self.defs.get_state(e).name)?;
        }
        self.w(b" {\n")?;
        self.indent_incr();
        for &fun in s.funcs.iter() {
            let f = self.defs.get_func(fun);
            self.format_func(f)?;
        }

        self.indent_decr();

        self.w(b"}\n")?;

        Ok(())
    }

    fn format_func(&mut self, f: &Function) -> io::Result<()> {
        self.indent()?;
        if f.flags.contains(FuncFlags::ITERATOR) {
            self.w.write_all(b"iterator ")?;
        } else if f.flags.contains(FuncFlags::DELEGATE) {
            self.w.write_all(b"delegate ")?;
        } else {
            self.w.write_all(b"function ")?;
        }

        self.format_sig(&f.sig, &f.name.as_ref().to_owned())?;

        // TODO: Body

        Ok(())
    }

    fn format_op(&mut self, f: &Operator) -> io::Result<()> {
        self.indent()?;
        if f.flags.contains(FuncFlags::OPERATOR) {
            self.w.write_all(b"operator ")?;
        } else if f.flags.contains(FuncFlags::PREOPERATOR) {
            self.w.write_all(b"preoperator ")?;
        } else if f.flags.contains(FuncFlags::POSTOPERATOR) {
            self.w.write_all(b"postoperator ")?;
        } else {
            unreachable!()
        }

        self.format_sig(&f.sig, &format!("{:?}", f.op))?;

        // TODO: Body

        Ok(())
    }

    fn format_sig(&mut self, f: &FuncSig, name: &str) -> io::Result<()> {
        if let Some(ty) = f.ret_ty {
            self.format_ty(ty)?;
            self.w.write_all(b" ")?;
        }

        self.w(name.as_bytes())?;

        self.w.write_all(b"(")?;
        for (idx, &arg) in f.args.iter().enumerate() {
            let arg_def = self.defs.get_arg(arg);
            if arg_def.flags.contains(ArgFlags::OUT) {
                self.w.write_all(b"out ")?;
            }
            if arg_def.flags.contains(ArgFlags::OPTIONAL) {
                self.w.write_all(b"optional ")?;
            }
            self.format_ty(arg_def.ty)?;
            self.w.write_all(b" ")?;
            self.w.write_all(arg_def.name.as_ref().as_bytes())?;

            if idx != f.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b");\n")?;

        Ok(())
    }

    fn format_ty(&mut self, ty: Ty) -> io::Result<()> {
        let formatted = ty.format_ty(&|def_id| {
            let def = self.defs.get_def(def_id);
            match &def.kind {
                DefKind::Class(c) => c.name.as_ref().to_owned(),
                DefKind::Enum(e) => e.name.as_ref().to_owned(),
                DefKind::Struct(s) => s.name.as_ref().to_owned(),
                DefKind::Function(f) => f.name.as_ref().to_owned(),
                _ => unreachable!(),
            }
        });
        self.w(formatted.as_bytes())
    }

    fn format_class(&mut self, class: DefId) -> io::Result<()> {
        let c = self.defs.get_class(class);
        match c.kind.as_ref().unwrap() {
            ClassKind::Class { extends, implements, within } => {
                self.w(b"class ")?;
                self.format_i(&c.name)?;
                if let Some(e) = extends {
                    self.w(b" extends ")?;
                    self.format_i(&self.defs.get_class(*e).name)?;
                }
                if let Some(e) = within {
                    self.w(b" within ")?;
                    self.format_i(&self.defs.get_class(*e).name)?;
                }
                if implements.len() > 0 {
                    self.w(b" implements(")?;
                    implements
                        .iter()
                        .map(|i| self.defs.get_class(*i).name.as_ref())
                        .intersperse(", ")
                        .map(|s| self.w(s.as_bytes()))
                        .collect::<Result<Vec<_>, _>>()?;
                    self.w(b" implements)")?;
                }
            }
            ClassKind::Interface { extends } => {
                self.w(b"interface ")?;
                self.format_i(&c.name)?;
                if let Some(e) = extends {
                    self.w(b" extends ")?;
                    self.format_i(&self.defs.get_class(*e).name)?;
                }
            }
        }
        self.w(b";\n\n")?;

        for &i in &c.items {
            let def = self.defs.get_def(i);
            match &def.kind {
                DefKind::Enum(e) => self.format_enum(e)?,
                DefKind::Struct(s) => self.format_struct(s)?,
                DefKind::Var(v) => self.format_var(v)?,
                DefKind::Const(c) => self.format_const(c)?,
                DefKind::State(s) => self.format_state(s)?,
                DefKind::Operator(o) => self.format_op(o)?,
                DefKind::Function(f) => self.format_func(f)?,
                _ => todo!(),
            }
        }

        Ok(())
    }
}

pub fn format_file<W: io::Write>(defs: &Defs, class: DefId, w: &mut W) -> io::Result<()> {
    let mut printer = PPrinter { w, defs, indent: vec![] };
    printer.format_class(class)?;
    Ok(())
}

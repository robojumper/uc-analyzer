use std::{io, ops::RangeBounds, slice::SliceIndex};

use uc_def::{ArgFlags, FuncFlags};
use uc_name::Identifier;

use crate::{
    body::{self, BlockId, Body, Expr, ExprId, Literal, Receiver, Statement, StmtId},
    ty::Ty,
    ClassKind, Const, DefId, DefKind, Defs, Enum, FuncContents, FuncSig, Function, Operator, State,
    Struct, Var,
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
        self.w(self.render_ty(v.ty.unwrap()).as_bytes())?;

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
            Literal::Name(i) => {
                self.w(b"'")?;
                self.format_i(i)?;
                self.w(b"'")
            }
            Literal::String(s) => {
                self.w(b"\"")?;
                self.w(s.as_bytes())?;
                self.w(b"\"")
            }
            Literal::Byte(b) => self.w.write_fmt(format_args!("{}", b)),
            Literal::Object(d) => {
                self.w(self.render_ty(Ty::object_from(*d)).as_bytes())?;
                self.w(b"'some_object'")
            }
            Literal::Class(c) => {
                self.w(b"class'")?;
                self.w(self.render_ty(Ty::object_from(*c)).as_bytes())?;
                self.w(b"'")
            }
            Literal::Struct(s) => self.w(self.render_ty(Ty::struct_from(*s)).as_bytes()),
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

        if let Some(b) = s.contents.as_ref() {
            self.format_body(b)?;
        } else {
            self.w(b"/* body had error */")?;
        }

        self.indent_decr();

        self.w(b"}\n")?;

        Ok(())
    }

    fn format_func(&mut self, f: &Function) -> io::Result<()> {
        self.indent()?;
        if f.flags.contains(FuncFlags::ITERATOR) {
            self.w(b"iterator ")?;
        } else if f.flags.contains(FuncFlags::DELEGATE) {
            self.w(b"delegate ")?;
        } else {
            self.w(b"function ")?;
        }

        self.format_sig(&f.sig, &f.name.as_ref().to_owned())?;

        if let Some(c) = &f.contents {
            self.w(b" {\n")?;
            self.format_contents(c)?;
            self.indent()?;
            self.w(b"}\n")?;
        } else {
            self.w(b";\n")?;
        }

        Ok(())
    }

    fn format_op(&mut self, f: &Operator) -> io::Result<()> {
        self.indent()?;
        if f.flags.contains(FuncFlags::OPERATOR) {
            self.w(b"operator ")?;
        } else if f.flags.contains(FuncFlags::PREOPERATOR) {
            self.w(b"preoperator ")?;
        } else if f.flags.contains(FuncFlags::POSTOPERATOR) {
            self.w(b"postoperator ")?;
        } else {
            unreachable!()
        }

        self.format_sig(&f.sig, &format!("{:?}", f.op))?;

        if let Some(c) = &f.contents {
            self.w(b" {\n")?;
            self.format_contents(c)?;
            self.indent()?;
            self.w(b"}\n")?;
        } else {
            self.w(b";\n")?;
        }

        Ok(())
    }

    fn format_contents(&mut self, c: &FuncContents) -> io::Result<()> {
        self.indent_incr();
        for &l in c.locals.iter() {
            self.indent()?;
            self.w(b"local ")?;
            let def = self.defs.get_local(l);
            self.w(self.render_ty(def.ty).as_bytes())?;
            self.w(b" ")?;
            self.format_i(&def.name)?;
            self.w(b";\n")?;
        }
        self.w(b"\n")?;

        for &c in c.consts.iter() {
            self.indent()?;
            self.w(b"const ")?;
            let def = self.defs.get_const(c);
            self.w(b" ")?;
            self.format_i(&def.name)?;
            self.w(b" = ")?;
            self.format_literal(def.val.as_ref().unwrap())?;
            self.w(b";\n")?;
        }
        self.w(b"\n")?;

        if let Some(b) = c.statements.as_ref() {
            self.format_body(b)?;
        } else {
            self.w(b"/* body had error */")?;
        }

        self.indent_decr();

        Ok(())
    }

    fn format_body(&mut self, b: &Body) -> io::Result<()> {
        let entry = b.get_block(b.get_entry());
        for &s in entry.stmts.iter() {
            self.indent()?;
            self.format_stmt(s, b)?;
            self.w(b"\n")?;
        }

        Ok(())
    }

    fn format_block(&mut self, b: BlockId, body: &Body) -> io::Result<()> {
        let block = body.get_block(b);
        self.w(b"{\n")?;
        self.indent_incr();
        for &s in block.stmts.iter() {
            self.indent()?;
            self.format_stmt(s, body)?;
            self.w(b"\n")?;
        }
        self.indent_decr();
        self.indent()?;
        self.w(b"}")
    }

    fn format_stmt(&mut self, id: StmtId, body: &Body) -> io::Result<()> {
        let s = body.get_stmt(id);
        match &s.kind {
            body::StatementKind::Expr(e) => {
                self.format_expr(*e, body)?;
                self.w(b";")?;
            }
            body::StatementKind::If(cond, then, otherwise) => {
                self.w(b"if (")?;
                self.format_expr(*cond, body)?;
                self.w(b") ")?;
                self.format_block(*then, body)?;
                if let Some(o) = otherwise {
                    self.w(b" else ")?;
                    self.format_block(*o, body)?;
                }
                self.w(b"\n")?;
            }
            body::StatementKind::Loop(entry, retry, run, _) => {
                self.w(b"loop (/*entry*/ ")?;
                if let Some(entry) = entry {
                    self.format_stmt(*entry, body)?;
                } else {
                    self.w(b";")?;
                }
                self.w(b" /*retry*/ ")?;
                if let Some(retry) = retry {
                    self.format_stmt(*retry, body)?;
                } else {
                    self.w(b";")?;
                }
                self.w(b") ")?;
                self.format_block(*run, body)?;
            }
            body::StatementKind::Switch(scrutinee, cases, default, statements) => {
                self.w(b"switch (")?;
                self.format_expr(*scrutinee, body)?;
                self.w(b", ")?;
                for &(expr, skip) in cases.iter() {
                    self.format_expr(expr, body)?;
                    self.w(b" => ")?;
                    self.w.write_fmt(format_args!("{}", skip))?;
                    self.w(b", ")?;
                }
                self.w(b"default = ")?;
                if let Some(def) = default {
                    self.w.write_fmt(format_args!("{}", def))?;
                } else {
                    self.w(b"none")?;
                }
                self.w(b") ")?;
                self.format_block(*statements, body)?;
                self.w(b"\n")?;
            }
            body::StatementKind::Return(e) => {
                self.w(b"return")?;
                if let Some(e) = e {
                    self.w(b" ")?;
                    self.format_expr(*e, body)?;
                }
                self.w(b";")?;
            }
            body::StatementKind::Break => {
                self.w(b"break;")?;
            }
            body::StatementKind::Continue => {
                self.w(b"continue;")?;
            }
            body::StatementKind::Assign(l, r) => {
                self.format_expr(*l, body)?;
                self.w(b" = ")?;
                self.format_expr(*r, body)?;
                self.w(b";")?;
            }
            body::StatementKind::Label => self.w(b"label:")?,
        }
        Ok(())
    }

    fn format_receiver(&mut self, c: &Receiver, body: &Body) -> io::Result<()> {
        match c {
            Receiver::Super(def_id) => {
                self.w(b"super(")?;
                self.format_i(&self.defs.get_class(*def_id).name)?;
                self.w(b")")?;
            }
            Receiver::Global => self.w(b"global")?,
            Receiver::Static(expr) => {
                self.format_expr(*expr, body)?;
                self.w(b".static")?;
            }
            Receiver::StaticSelf => self.w(b"StaticSelf")?,
            Receiver::Expr(expr) => {
                self.format_expr(*expr, body)?;
            }
        }
        Ok(())
    }

    fn format_args(&mut self, args: &[Option<ExprId>], body: &Body) -> io::Result<()> {
        self.w(b"(")?;
        self.format_args_list(args, body)?;
        self.w(b")")
    }

    fn format_args_list(&mut self, args: &[Option<ExprId>], body: &Body) -> io::Result<()> {
        for (idx, &arg) in args.iter().enumerate() {
            self.format_opt_expr(arg, body)?;
            if idx != args.len() - 1 {
                self.w(b", ")?;
            }
        }
        Ok(())
    }

    fn format_opt_expr(&mut self, arg: Option<ExprId>, body: &Body) -> io::Result<()> {
        if let Some(a) = arg { self.format_expr(a, body) } else { self.w(b"/*default*/") }
    }

    fn format_expr(&mut self, e: ExprId, body: &Body) -> io::Result<()> {
        let expr = body.get_expr(e);
        match &expr.kind {
            body::ExprKind::Value(v) => match v {
                body::ValueExprKind::Lit(l) => self.format_literal(l),
                body::ValueExprKind::DelegateCreation(recv, func) => {
                    self.format_receiver(recv, body)?;
                    self.w(b".")?;
                    self.format_i(&self.defs.get_func(*func).name)
                }
                body::ValueExprKind::FuncCall(recv, func, args) => {
                    self.format_receiver(recv, body)?;
                    self.w(b".")?;
                    self.format_i(&self.defs.get_func(*func).name)?;
                    self.format_args(args, body)
                }
                body::ValueExprKind::DelegateCall(expr, _, args) => {
                    self.format_expr(*expr, body)?;
                    self.w(b"/*delegate*/")?;
                    self.format_args(args, body)
                }
                body::ValueExprKind::DynArrayIntrinsic(expr, op) => {
                    self.format_expr(*expr, body)?;
                    self.w(b".")?;
                    match op {
                        body::DynArrayOpKind::FindElem(e) => {
                            self.w(b"Find(")?;
                            self.format_expr(*e, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::FindField(field, val) => {
                            self.w(b".Find('")?;
                            self.format_i(&self.defs.get_var(*field).name)?;
                            self.w(b"', ")?;
                            self.format_expr(*val, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::Add(e) => {
                            self.w(b"Add(")?;
                            self.format_expr(*e, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::AddItem(e) => {
                            self.w(b"AddItem(")?;
                            self.format_expr(*e, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::Insert(idx, num) => {
                            self.w(b"Insert(")?;
                            self.format_expr(*idx, body)?;
                            self.w(b", ")?;
                            self.format_expr(*num, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::InsertItem(idx, item) => {
                            self.w(b"InsertItem(")?;
                            self.format_expr(*idx, body)?;
                            self.w(b", ")?;
                            self.format_expr(*item, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::Remove(idx, num) => {
                            self.w(b"Remove(")?;
                            self.format_expr(*idx, body)?;
                            self.w(b", ")?;
                            self.format_expr(*num, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::RemoveItem(item) => {
                            self.w(b"RemoveItem(")?;
                            self.format_expr(*item, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::Sort(cmp) => {
                            self.w(b"Sort(")?;
                            self.format_expr(*cmp, body)?;
                            self.w(b")")
                        }
                        body::DynArrayOpKind::RandomizeOrder => self.w(b"RandomizeOrder()"),
                    }
                }
                body::ValueExprKind::ForeachIntrinsic(it_id, op) => match op {
                    body::ForeachOpKind::Create(recv, fun, args) => {
                        self.w(b"__iterator_create")?;
                        self.w(b"(")?;
                        self.w.write_fmt(format_args!("{}", it_id))?;
                        self.w(b", ")?;
                        self.format_receiver(recv, body)?;
                        self.w(b".")?;
                        self.format_i(&self.defs.get_func(*fun).name)?;
                        self.w(b", ")?;
                        self.format_args_list(args, body)?;
                        self.w(b")")
                    }
                    body::ForeachOpKind::HasNext => {
                        self.w(b"__iterator_has_next(")?;
                        self.w.write_fmt(format_args!("{}", it_id))?;
                        self.w(b")")
                    }
                    body::ForeachOpKind::Next(args) => {
                        self.w(b"__iterator_next(")?;
                        self.w.write_fmt(format_args!("{}", it_id))?;
                        self.w(b", ")?;
                        self.format_args_list(args, body)?;
                        self.w(b")")
                    }
                },
                body::ValueExprKind::OpCall(op_id, arg, arg2) => {
                    self.w(b"(")?;
                    let op_def = self.defs.get_op(*op_id);
                    let mut name = format!("op{:?}", op_def.op);
                    for arg in op_def.sig.args.iter() {
                        let arg_def = self.defs.get_arg(*arg);
                        name += &self.render_ty(arg_def.ty);
                    }

                    if op_def.flags.contains(FuncFlags::PREOPERATOR) {
                        self.w(name.as_bytes())?;
                        self.w(b" ")?;
                        self.format_expr(*arg, body)?;
                    } else if op_def.flags.contains(FuncFlags::POSTOPERATOR) {
                        self.format_expr(*arg, body)?;
                        self.w(b" ")?;
                        self.w(name.as_bytes())?;
                    } else {
                        assert!(op_def.flags.contains(FuncFlags::OPERATOR));
                        self.format_expr(*arg, body)?;
                        self.w(b" ")?;
                        self.w(name.as_bytes())?;
                        self.w(b" ")?;
                        self.format_expr(arg2.unwrap(), body)?
                    }

                    self.w(b")")
                }
                body::ValueExprKind::TernaryOp(cond, then, alt) => {
                    self.w(b"((")?;
                    self.format_expr(*cond, body)?;
                    self.w(b") ? ")?;
                    self.format_expr(*then, body)?;
                    self.w(b" : ")?;
                    self.format_expr(*alt, body)?;
                    self.w(b")")
                }
                body::ValueExprKind::StructComparison(left, right, eq) => {
                    self.w(b"(")?;
                    self.format_expr(*left, body)?;
                    if *eq {
                        self.w(b" opEqEqStructStruct ")?;
                    } else {
                        self.w(b" opBangEqStructStruct ")?;
                    }
                    self.format_expr(*right, body)?;
                    self.w(b")")
                }
                body::ValueExprKind::DelegateComparison(left, right, eq) => {
                    self.w(b"(")?;
                    self.format_expr(*left, body)?;
                    if *eq {
                        self.w(b" opEqEqDelDel ")?;
                    } else {
                        self.w(b" opBangEqDelDel ")?;
                    }
                    self.format_expr(*right, body)?;
                    self.w(b")")
                }
                body::ValueExprKind::NewExpr(outer, name, class, templ) => {
                    self.w(b"new (")?;
                    self.format_opt_expr(*outer, body)?;
                    self.w(b", ")?;
                    self.format_opt_expr(*name, body)?;
                    self.w(b") ")?;
                    self.format_expr(*class, body)?;
                    self.w(b" (")?;
                    self.format_opt_expr(*templ, body)?;
                    self.w(b")")
                }
                body::ValueExprKind::CastExpr(to, expr, _) => {
                    self.w(self.render_ty(*to).as_bytes())?;
                    self.w(b"(")?;
                    self.format_expr(*expr, body)?;
                    self.w(b")")
                }
            },
            body::ExprKind::Place(p) => match p {
                body::PlaceExprKind::SelfAccess => self.w(b"Self"),
                body::PlaceExprKind::Local(l) => self.format_i(&self.defs.get_local(*l).name),
                body::PlaceExprKind::Arg(a) => self.format_i(&self.defs.get_arg(*a).name),
                body::PlaceExprKind::Index(array, index) => {
                    self.format_expr(*array, body)?;
                    self.w(b"[")?;
                    self.format_expr(*index, body)?;
                    self.w(b"]")
                }
                body::PlaceExprKind::Field(recv, field) => {
                    self.format_receiver(recv, body)?;
                    self.w(b".")?;
                    self.format_i(&self.defs.get_var(*field).name)
                }
                body::PlaceExprKind::DynArrayLen(expr) => {
                    self.format_expr(*expr, body)?;
                    self.w(b".Length")
                }
            },
        }
    }

    fn format_sig(&mut self, f: &FuncSig, name: &str) -> io::Result<()> {
        if let Some(ty) = f.ret_ty {
            self.w(self.render_ty(ty).as_bytes())?;
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
            self.w(self.render_ty(arg_def.ty).as_bytes())?;
            self.w.write_all(b" ")?;
            self.w.write_all(arg_def.name.as_ref().as_bytes())?;

            if idx != f.args.len() - 1 {
                self.w.write_all(b", ")?;
            }
        }
        self.w.write_all(b")")?;

        Ok(())
    }

    fn render_ty(&self, ty: Ty) -> String {
        ty.format_ty(&|def_id| {
            let def = self.defs.get_def(def_id);
            match &def.kind {
                DefKind::Class(c) => c.name.as_ref().to_owned(),
                DefKind::Enum(e) => e.name.as_ref().to_owned(),
                DefKind::Struct(s) => s.name.as_ref().to_owned(),
                DefKind::Function(f) => f.name.as_ref().to_owned(),
                _ => unreachable!(),
            }
        })
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
                _ => unreachable!(),
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

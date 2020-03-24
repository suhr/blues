use std::collections::HashMap;
use std::path::Path;
use std::{cmp::Ordering, path::PathBuf};
use annotate_snippets::snippet::Snippet;
use pest::iterators::Pair;
use pest_derive::Parser;
use smol_str::SmolStr;

use wrapterm::*;

use crate::ast::{TyDecl, TyExpr};

const PRELUDE: &'static str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/lib/prelude.blues"));

#[derive(Parser)]
#[grammar = "blues.pest"]
pub struct BluesParser;

fn pair_location(pair: &Pair<Rule>, file_id: usize) -> Location {
    Location {
        file_id,
        span: Span {
            from: pair.as_span().start(),
            to: pair.as_span().end(),
        }
    }
}

enum RawExpr {
    Bind {
        head: Ranged<SmolStr>,
        vars: Ranged<Vec<Ranged<crate::ast::Var>>>
    },
    Infix {
        head: Box<Ranged<RawExpr>>,
        tail: Vec<InfixTail>
    },
    App(Vec<Ranged<RawExpr>>),
    List {
        heads: Ranged<Vec<Ranged<RawExpr>>>,
        tail: Box<Ranged<RawExpr>>,
    },
    Name(SmolStr),
    Integer(crate::ast::Integer),
    Float(SmolStr),
    String(SmolStr),
}

struct InfixTail {
    op: Ranged<SmolStr>,
    expr: Ranged<RawExpr>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModulePath {
    pub mods: Vec<SmolStr>
}

struct UseDecl {
    path: Ranged<ModulePath>,
    items: Vec<Ranged<UseItem>>
}

enum UseItem {
    Glob,
    Name(SmolStr),
    Op(SmolStr),
}

fn parse_mod_decl(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<SmolStr> {
    let name = pair.into_inner().next().unwrap();
    let loc = pair_location(&name, file_id);

    Ranged {
        data: name.as_str().into(),
        range: Some(loc)
    }
}

fn parse_use_decl(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<UseDecl> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap();
    let seq = inner.next();

    if let Some(seq) = seq {
        let seq = seq.into_inner();
        for p in seq {
            match p.as_rule() {
                Rule::glob => todo!(),
                Rule::op => todo!(),
                Rule::name => todo!(),
                _ => unreachable!()
            }
        }
    }
    todo!()
}

fn parse_op_decl(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<crate::ast::OpDecl> {
    let mut inner = pair.into_inner();
    let assoc = inner.next().unwrap();
    let op = inner.next().unwrap();
    let prec = inner.next();
    todo!()
}

fn parse_ty_def(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> TyDecl {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap();
    let ty = inner.next().unwrap();

    crate::ast::TyDecl {
        bindings: vec![],
        name: Ranged {
            data: name.as_str().into(),
            range: Some(pair_location(&name, file_id))
        },
        ty: parse_ty_expr(errata, ty, file_id)
    }
}

fn parse_ty_expr(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<TyExpr> {
    let loc = pair_location(&pair, file_id);

    let expr = match pair.as_rule() {
        Rule::ty_arr => {
            let mut inner = pair.into_inner();

            let mut exprs = vec![
                parse_ty_expr(errata, inner.next().unwrap(), file_id)
            ];
            let right = parse_ty_expr(errata, inner.next().unwrap(), file_id);

            if let TyExpr::Arr(es) = right.data {
                exprs.extend(es);
            }
            else {
                exprs.push(right);
            }

            TyExpr::Arr(exprs)
        },
        Rule::ty_parren =>
            return parse_ty_expr(errata, pair.into_inner().next().unwrap(), file_id),
        Rule::ty_app => {
            let mut inner = pair.into_inner();

            let mut exprs = vec![
                parse_ty_expr(errata, inner.next().unwrap(), file_id)
            ];
            let mut right = inner.next().unwrap();
            loop {
                if right.as_rule() != Rule::ty_app { break }

                inner = right.into_inner();
                let e1 = inner.next().unwrap();
                exprs.push(parse_ty_expr(errata, e1, file_id));

                right = inner.next().unwrap();
            }

            exprs.push(parse_ty_expr(errata, right, file_id));

            TyExpr::App(exprs)
        },
        Rule::name =>
            TyExpr::Name(pair.as_str().into()),
        _ => todo!()
    };

    Ranged {
        data: expr,
        range: Some(loc)
    }
}

#[allow(unconditional_recursion)] 
fn parse_raw_expr(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let mut inner = pair.into_inner();

    let head = inner.next().unwrap();

    let mut expr = match head.as_rule() {
        Rule::cl_bind =>
            parse_bind(errata, head, file_id),
        Rule::cl_app =>
            parse_app(errata, head, file_id),
        Rule::cl_encl =>
            parse_raw_expr(errata, head, file_id),
        Rule::cl_list => todo!(),
        _ =>
            parse_value(errata, head, file_id),
    };

    let tail = inner.next();
    if let Some(tail) = tail {
        todo!()
    }

    todo!()
}

fn parse_bind(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    todo!()
}

fn parse_app(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();
    let mut exprs = vec![
        parse_raw_expr(errata, inner.next().unwrap(), file_id)
    ];
    let mut right = inner.next().unwrap();
    loop {
        if right.as_rule() != Rule::cl_app { break }

        inner = right.into_inner();
        exprs.push(
            parse_raw_expr(errata, inner.next().unwrap(), file_id)
        );

        right = inner.next().unwrap()
    }

    exprs.push(parse_raw_expr(errata, right, file_id));

    Ranged {
        data: RawExpr::App(exprs),
        range: Some(loc)
    }
}

fn parse_list(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    todo!()
}

fn parse_value(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    todo!()
}

struct RawModule {
    path: ModulePath,
    mods: Vec<Ranged<ModulePath>>,
    uses: Vec<Ranged<UseDecl>>,
    ops: Vec<Ranged<crate::ast::OpDecl>>,
    ty_decls: Vec<crate::ast::TyDecl>,
    clauses: Vec<Ranged<RawExpr>>,
    queries: Vec<Ranged<RawExpr>>,
}

struct LoadTask {
    module_name: SmolStr,
    base_path: PathBuf,
    parent: Option<(ModulePath, Location)>
}

pub enum LoadErrorKind {
    IoError(std::io::Error),
    PestError(pest::error::Error<Rule>),
}

impl Report for LoadErrorKind {
    fn to_snippet(&self, _sourcer: &Sourcer) -> Snippet {
        match self {
            LoadErrorKind::IoError(e) => {
                let msg = format!("{}", e);
                sourceless_error("io error", &msg)
            },
            LoadErrorKind::PestError(e) => {
                // TODO: make a proper error message
                let msg = format!("{}", e);
                sourceless_error("parse error", &msg)
            },
            _ => todo!()
        }
    }

    fn file_id(&self) -> Option<usize> {
        use LoadErrorKind as E;

        match self {
            _ =>
                None,
        }
    }
}

pub struct ModuleLoader<'a> {
    files: FileTable,
    errata: &'a mut Errata,
    queue: Vec<LoadTask>,
    loaded: Vec<RawModule>,
}

fn get_module_name(path: &Path) -> SmolStr {
    let name = path.file_stem().unwrap().to_string_lossy();

    name.as_ref().into()
}

fn get_module_path(name: &str, base: &Path, parent: Option<&str>)
-> Result<PathBuf, LoadErrorKind> {
    todo!()
}

impl<'a> ModuleLoader<'a> {
    fn load_file<P>(&mut self, path: P) -> Result<(), ()>
    where P: AsRef<Path> {
        let module_name = get_module_name(path.as_ref());
        let base_path = path.as_ref().parent().unwrap().to_owned();

        let task = LoadTask {
            module_name,
            base_path,
            parent: None
        };

        self.queue.push(task);
        self.run_loader()
    }

    fn run_loader(&mut self) -> Result<(), ()> {
        self.load_prelude();

        while let Some(task) = self.queue.pop() {
            self.run_task(task)?
        }

        Ok(())
    }

    fn add_task(&mut self, name: Ranged<SmolStr>, parent: ModulePath) -> Result<(), ()> {
        todo!()
    }

    fn run_task(&mut self, task: LoadTask) -> Result<(), ()> {
        use pest::Parser;

        let parent = task.parent
            .as_ref()
            .map(|p| p.0.mods.last().unwrap().as_str());
        let path = get_module_path(&task.module_name, &task.base_path, parent);
        let path = self.errata.push_err(path)?;

        let file_id = self.files.append(path.clone());
        if file_id == self.files.files().len() - 1 {
            // self.errata.push_err(todo!())?
        }

        let source = std::fs::read_to_string(&path)
            .map_err(LoadErrorKind::IoError);
        let source = self.errata.push_err(source)?;

        let pair = BluesParser::parse(Rule::root, &source)
            .map(|mut p| p.next().unwrap())
            .map_err(LoadErrorKind::PestError);
        let pair = self.errata.push_err(pair)?;

        self.load_module(pair, &task, file_id)
    }

    fn load_prelude(&mut self) {}

    fn load_module(
        &mut self,
        pair: Pair<Rule>,
        task: &LoadTask,
        file_id: usize
    ) -> Result<(), ()> {
        let iter = pair.into_inner()
            .filter(|r| r.as_rule() != Rule::EOI);

        let path = task.parent.as_ref()
            .map(|p| {
                let mut p = p.0.clone();
                p.mods.push(task.module_name.clone());
                p
            })
            .unwrap_or_else(|| ModulePath {
                mods: vec![task.module_name.clone()]
            });

        let mut module = RawModule {
            path,
            mods: vec![],
            uses: vec![],
            ops: vec![],
            ty_decls: vec![],
            clauses: vec![],
            queries: vec![],
        };

        let mut is_ok = true;
        for p in iter {
            match p.as_rule() {
                Rule::mod_decl => {
                    let decl = parse_mod_decl(self.errata, p, file_id);

                    self.add_task(decl, module.path.clone())
                        .unwrap_or_else(|()| is_ok = false);
                },
                Rule::use_decl => {
                    let decl = parse_use_decl(self.errata, p, file_id);

                    module.uses.push(decl)
                },
                Rule::infix_decl => {
                    let decl = parse_op_decl(self.errata, p, file_id);

                    module.ops.push(decl)
                },
                Rule::ty_def => {
                    let ty = parse_ty_def(self.errata, p, file_id);

                    module.ty_decls.push(ty)
                },
                Rule::clause => {
                    let inner = p.into_inner().next().unwrap();
                    let expr = parse_raw_expr(self.errata, inner, file_id);

                    module.clauses.push(expr)
                },
                Rule::query => {
                    let inner = p.into_inner().next().unwrap();
                    let expr = parse_raw_expr(self.errata, inner, file_id);

                    module.clauses.push(expr)
                },
                _ => todo!()
            }
        }
        todo!()
    }
}

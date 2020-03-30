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

fn convert_pair<F, T>(pair: Pair<Rule>, file_id: usize, map: F) -> Ranged<T>
where F: FnOnce(Pair<Rule>) -> T {
    let loc = pair_location(&pair, file_id);

    Ranged {
        data: map(pair),
        range: Some(loc)
    }
}

enum RawExpr {
    Bind {
        head: Option<Ranged<SmolStr>>,
        vars: Ranged<Vec<Ranged<crate::ast::Var>>>,
        body: Box<Ranged<RawExpr>>,
    },
    Infix {
        head: Box<Ranged<RawExpr>>,
        tail: Vec<InfixTail>
    },
    App(Vec<Ranged<RawExpr>>),
    List {
        heads: Ranged<Vec<Ranged<RawExpr>>>,
        tail: Option<Box<Ranged<RawExpr>>>,
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

#[derive(Default, Debug, Clone, PartialEq)]
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

fn as_path(path: &str) -> ModulePath {
    let mods = path.split('.')
        .map(|s| s.into())
        .collect();

    ModulePath { mods }
}

fn parse_mod_decl(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<SmolStr> {
    convert_pair(pair, file_id, |p| p.as_str().into())
}

fn parse_use_decl(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<UseDecl> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();
    let path =
        convert_pair(inner.next().unwrap(), file_id, |p| as_path(p.as_str()));

    let mut use_decl = UseDecl {
        path,
        items: vec![]
    };

    let seq = inner.next();

    if let Some(seq) = seq {
        let seq = seq.into_inner();
        for p in seq {
            let item = convert_pair(p, file_id, |p| match p.as_rule() {
                Rule::glob =>
                    UseItem::Glob,
                Rule::op =>
                    UseItem::Op(p.as_str().into()),
                Rule::name =>
                    UseItem::Name(p.as_str().into()),
                _ => unreachable!()
            });

            use_decl.items.push(item)
        }
    }

    Ranged {
        data: use_decl,
        range: Some(loc)
    }
}

fn parse_op_decl(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<crate::ast::OpDecl> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();

    let assoc = convert_pair(inner.next().unwrap(), file_id, |p|
        match p.as_str() {
            "n" => crate::ast::Assoc::None,
            "l" => crate::ast::Assoc::Left,
            "r" => crate::ast::Assoc::Right,
            _ => unreachable!()
        }
    );

    let op = convert_pair(inner.next().unwrap(), file_id, |p|
        p.as_str().into()
    );

    let rel_pair = inner.next();
    let mut rel = None;
    if let Some(p) = rel_pair {
        let loc = pair_location(&p, file_id);
        let mut inner = p.into_inner();

        let ord = match inner.next().unwrap().as_str() {
            "<" => Ordering::Less,
            "=" => Ordering::Equal,
            ">" => Ordering::Greater,
            _ => unreachable!()
        };

        let op = inner.next().unwrap().as_str().into();

        rel = Some(Ranged {
            data: (ord, op),
            range: Some(loc)
        })
    }

    Ranged {
        data: crate::ast::OpDecl {
            op, assoc, rel
        },
        range: Some(loc)
    }
}

fn parse_ty_def(
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
        ty: parse_ty_expr(ty, file_id)
    }
}

fn parse_ty_expr(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<TyExpr> {
    let loc = pair_location(&pair, file_id);

    let expr = match pair.as_rule() {
        Rule::ty_arr => {
            let mut inner = pair.into_inner();

            let mut exprs = vec![
                parse_ty_expr(inner.next().unwrap(), file_id)
            ];
            let right = parse_ty_expr(inner.next().unwrap(), file_id);

            if let TyExpr::Arr(es) = right.data {
                exprs.extend(es);
            }
            else {
                exprs.push(right);
            }

            TyExpr::Arr(exprs)
        },
        Rule::ty_parren =>
            return parse_ty_expr(pair.into_inner().next().unwrap(), file_id),
        Rule::ty_app => {
            let mut inner = pair.into_inner();

            let mut exprs = vec![
                parse_ty_expr(inner.next().unwrap(), file_id)
            ];
            let mut right = inner.next().unwrap();
            loop {
                if right.as_rule() != Rule::ty_app { break }

                inner = right.into_inner();
                let e1 = inner.next().unwrap();
                exprs.push(parse_ty_expr(e1, file_id));

                right = inner.next().unwrap();
            }

            exprs.push(parse_ty_expr(right, file_id));

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
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();

    let expr = parse_infixless(inner.next().unwrap(), file_id);

    let mut infix_tail: Vec<InfixTail> = vec![];
    while let Some(tail) = inner.next() {
        let mut tail_inner = tail.into_inner();
        let op = convert_pair(
            tail_inner.next().unwrap(),
            file_id,
            |p| p.as_str().into()
        );
        let expr = parse_infixless(tail_inner.next().unwrap(), file_id);
        infix_tail.push(InfixTail { op, expr });

        inner = tail_inner
    }

    if infix_tail.is_empty() {
        expr
    }
    else {
        Ranged {
            data: RawExpr::Infix {
                head: Box::new(expr),
                tail: infix_tail
            },
            range: Some(loc)
        }
    }
}

fn parse_infixless(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    match pair.as_rule() {
        Rule::cl_bind =>
            parse_bind(pair, file_id),
        Rule::cl_app =>
            parse_app(pair, file_id),
        Rule::cl_encl =>
            parse_raw_expr(pair, file_id),
        Rule::cl_list => todo!(),
        _ =>
            parse_value(pair, file_id),
    }
}

fn parse_infix_tail(
    pair: Pair<Rule>,
    file_id: usize
) -> InfixTail {
    let mut inner = pair.into_inner();
    
    let op = convert_pair(inner.next().unwrap(), file_id, |p| p.as_str().into());
    let expr = parse_raw_expr(inner.next().unwrap(), file_id);

    InfixTail { op, expr }
}

fn parse_bind(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();

    let head = inner.next().unwrap()
        .into_inner().next()
        .map(|head| Ranged {
            data: head.as_str().into(),
            range: Some(pair_location(&head, file_id))
        });

    let bind_list = inner.next().unwrap();
    let bind_loc = pair_location(&bind_list, file_id);
    let vars = bind_list.into_inner()
        .map(|p| {
            let loc = pair_location(&p, file_id);
            let mut inner = p.into_inner();

            let name = convert_pair(inner.next().unwrap(), file_id, |p|
                p.as_str().into()
            );
            let ty = inner.next()
                .map(|p| parse_ty_expr(p, file_id));

            Ranged {
                data: crate::ast::Var::Raw {
                    name, ty
                },
                range: Some(loc)
            }
        })
        .collect();
    let vars = Ranged {
        data: vars,
        range: Some(bind_loc)
    };

    let body = Box::new(parse_raw_expr(inner.next().unwrap(), file_id));

    Ranged {
        data: RawExpr::Bind {
            head, vars, body
        },
        range: Some(loc)
    }
}

fn parse_app(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();
    let mut exprs = vec![
        parse_raw_expr(inner.next().unwrap(), file_id)
    ];
    let mut right = inner.next().unwrap();
    loop {
        if right.as_rule() != Rule::cl_app { break }

        inner = right.into_inner();
        exprs.push(
            parse_raw_expr(inner.next().unwrap(), file_id)
        );

        right = inner.next().unwrap()
    }

    exprs.push(parse_raw_expr(right, file_id));

    Ranged {
        data: RawExpr::App(exprs),
        range: Some(loc)
    }
}

fn parse_list(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();

    let heads = convert_pair(inner.next().unwrap(), file_id, |p| {
        let seq: Vec<_> = p.into_inner()
            .map(|p| parse_raw_expr(p, file_id))
            .collect();

        seq
    });

    let tail = inner.next()
        .map(|p| Box::new(
            parse_raw_expr(p.into_inner().next().unwrap(), file_id)
        ));

    Ranged {
        data: RawExpr::List { heads, tail },
        range: Some(loc),
    }
}


fn parse_value(
    pair: Pair<Rule>,
    file_id: usize
) -> Ranged<RawExpr> {
    use crate::ast::Integer;

    convert_pair(pair, file_id, |p| {
        match p.as_rule() {
            Rule::fullname =>
                RawExpr::Name(p.as_str().into()),
            Rule::decimal =>
                RawExpr::Integer(Integer::Decimal(p.as_str().into())),
            Rule::hexdecimal =>
                RawExpr::Integer(Integer::Hexdecimal(p.as_str().into())),
            Rule::string =>
                RawExpr::String(p.as_str().into()),
            _ => unreachable!()
        }
    })
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

#[derive(Debug, Clone)]
struct LoadTask {
    module_name: SmolStr,
    base_path: PathBuf,
    parent_path: Option<ModulePath>,
    import_location: Option<Location>,
}

pub enum LoadErrorKind {
    IoError(std::io::Error, Option<Location>),
    PestError(pest::error::Error<Rule>),
    FileNotFound(PathBuf, Option<Location>),
    AlreadyLoaded(Location),
}

impl Report for LoadErrorKind {
    fn to_snippet(&self, _sourcer: &Sourcer) -> Snippet {
        match self {
            LoadErrorKind::IoError(e, _) => {
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
        // use LoadErrorKind as E;

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

fn get_module_path(task: &LoadTask) -> Result<PathBuf, LoadErrorKind> {
    let parent = task.parent_path
        .as_ref()
        .map(|p| p.mods.last().unwrap().as_str());
    let mut path = task.base_path.to_owned();
    if let Some(parent) = parent { path.push(parent) }

    let file_name = format!("{}.blues", task.module_name);
    path.push(file_name);

    if !path.is_file() {
        let loc = task.import_location;
        return Err(LoadErrorKind::FileNotFound(path, loc))
    }
    else { Ok(path) }
}

impl<'a> ModuleLoader<'a> {
    fn load_file<P>(&mut self, path: P) -> Result<(), ()>
    where P: AsRef<Path> {
        let module_name = get_module_name(path.as_ref());
        let base_path = path.as_ref().parent().unwrap().to_owned();

        let task = LoadTask {
            module_name,
            base_path,
            parent_path: None,
            import_location: None,
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

    fn add_task(&mut self, name: Ranged<SmolStr>, parent: LoadTask) -> Result<(), ()> {
        let mut base_path = parent.base_path;
        let mut parent_path: ModulePath = Default::default();

        if let Some(path) = parent.parent_path {
            base_path.push(path.mods.last().unwrap().as_str());
            parent_path.mods.extend(path.mods);
        }
       
        parent_path.mods.push(parent.module_name);

        let task = LoadTask {
            module_name: name.data,
            base_path,
            parent_path: Some(parent_path),
            import_location: name.range
        };

        self.queue.push(task);

        Ok(())
    }

    fn run_task(&mut self, task: LoadTask) -> Result<(), ()> {
        use pest::Parser;

        let import_loc = task.import_location;
        let path = get_module_path(&task);
        let path = self.errata.push_err(path)?;

        let file_id = self.files.append(path.clone());
        if file_id == self.files.files().len() - 1 {
            let err = LoadErrorKind::AlreadyLoaded(import_loc.unwrap());
            self.errata.push_err(Err(err))?;
        }

        let source = std::fs::read_to_string(&path)
            .map_err(|e| LoadErrorKind::IoError(e, import_loc));
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

        let path = task.parent_path.as_ref()
            .map(|p| {
                let mut p = p.clone();
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
                    let decl = parse_mod_decl(p, file_id);

                    self.add_task(decl, task.clone())
                        .unwrap_or_else(|()| is_ok = false);
                },
                Rule::use_decl => {
                    let decl = parse_use_decl(p, file_id);

                    module.uses.push(decl)
                },
                Rule::infix_decl => {
                    let decl = parse_op_decl(p, file_id);

                    module.ops.push(decl)
                },
                Rule::ty_def => {
                    let ty = parse_ty_def(p, file_id);

                    module.ty_decls.push(ty)
                },
                Rule::clause => {
                    let inner = p.into_inner().next().unwrap();
                    let expr = parse_raw_expr(inner, file_id);

                    module.clauses.push(expr)
                },
                Rule::query => {
                    let inner = p.into_inner().next().unwrap();
                    let expr = parse_raw_expr(inner, file_id);

                    module.clauses.push(expr)
                },
                _ => unreachable!()
            }
        }

        Ok(())
    }
}

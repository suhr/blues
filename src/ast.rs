use std::path::Path;
use std::path::PathBuf;
use annotate_snippets::snippet::Snippet;
use pest::iterators::Pair;
use pest_derive::Parser;
use smol_str::SmolStr;

use wrapterm::*;

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

pub fn parse_file(path: &std::path::Path) {
    use {BluesParser as M, Rule as R};
    use pest::Parser;

    let source = std::fs::read_to_string(path).unwrap();
    drop(
        M::parse(R::root, &source)
            .map(|mut p| p.next().unwrap())
            .unwrap()
    )
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    name: SmolStr,
    ty_decls: Vec<TyDecl>,
    clauses: Vec<Clause>,
    queries: Vec<Query>,
}

impl Module {
    fn new(name: SmolStr) -> Self {
        Module {
            name,
            ty_decls: vec![],
            clauses: vec![],
            queries: vec![]
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Clause {
    bindings: Vec<Ranged<Var>>,
    root: Ranged<Atomic>,
    goal: Option<Ranged<GExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
struct Query {
    expr: Ranged<GExpr>
}

enum Decl {

}

#[derive(Debug, Clone, PartialEq)]
struct TyDecl {
    name: Ranged<SmolStr>,
    ty: Ranged<TyExpr>,
}

#[derive(Debug, Clone, PartialEq)]
enum TyExpr {
    Pi {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<TyExpr>>
    },
    Arr(Vec<Ranged<TyExpr>>),
    App(Vec<Ranged<TyExpr>>),
    Name(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
enum Atomic {
    Lambda {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<Atomic>>
    },
    App(Vec<Ranged<Atomic>>),
    Name(SmolStr),
    Integer(Integer),
    Float(SmolStr),
    String(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
enum Integer {
    Decimal(SmolStr),
    Hexdecimal(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
enum GExpr {
    Atom(Atomic),
    And(Vec<Ranged<GExpr>>),
    Or(Vec<Ranged<GExpr>>),
    Imp {
        left: Box<Ranged<DExpr>>,
        right: Box<Ranged<GExpr>>,
    },
    Pi {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<GExpr>>
    },
    Sigma {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<GExpr>>
    },
}

#[derive(Debug, Clone, PartialEq)]
enum DExpr {
    Atom(Atomic),
    Coimp {
        left: Ranged<Atomic>,
        right: Ranged<GExpr>
    },
    And(Vec<Ranged<DExpr>>),
    Pi {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<DExpr>>
    },
}

#[derive(Debug, Clone, PartialEq)]
struct Var {
    name: Ranged<SmolStr>,
    ty: Option<Ranged<TyExpr>>,
}

enum LoadErrorKind {
    IoError(std::io::Error),
    PestError(pest::error::Error<Rule>),
    NotAtomic(Location),
    NotDExpr(Location),
    NotGExpr(Location),
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
            E::NotAtomic(loc)
            | E::NotDExpr(loc)
            | E::NotGExpr(loc) =>
                Some(loc.file_id),
            _ =>
                None,
        }
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct FileLoader {
    files: FileTable,
    cur_file_id: usize,
}

impl FileLoader {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_file_table(self) -> FileTable {
        self.files
    }
}

fn parse_module(
    name: SmolStr,
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize,
) -> Result<Module, ()> {
    let iter = pair.into_inner()
        .filter(|r| r.as_rule() != Rule::EOI);

    let mut module = Module::new(name);

    let mut is_ok = true;
    for p in iter {
        match p.as_rule() {
            Rule::mod_decl
            | Rule::use_decl => todo!(),
            Rule::ty_def => {
                if let Ok(ty) = parse_ty_def(errata, p, file_id) {
                    module.ty_decls.push(ty)
                }
                else { is_ok = false; break }
            },
            Rule::clause => {
                let inner = p.into_inner().next().unwrap();
                let dexpr = parse_d_expr(errata, inner, file_id)?;

                push_clauses(&mut module.clauses, dexpr)
            },
            Rule::query => {
                let inner = p.into_inner().next().unwrap();
                let gexpr = parse_g_expr(errata, inner, file_id)?;

                module.queries.push(Query {
                    expr: gexpr
                })
            },
            _ => todo!(),
        }
    }

    if is_ok { Ok(module) }
    else { Err(()) }
}

fn parse_ty_def(errata: &mut Errata, pair: Pair<Rule>, file_id: usize) -> Result<TyDecl, ()> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap();
    let ty = inner.next().unwrap();

    Ok(TyDecl {
        name: Ranged {
            data: name.as_str().into(),
            range: Some(pair_location(&name, file_id))
        },
        ty: parse_ty_expr(errata, ty, file_id)?
    })
}

fn parse_ty_expr(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Result<Ranged<TyExpr>, ()> {
    let loc = pair_location(&pair, file_id);

    let expr = match pair.as_rule() {
        Rule::ty_pi => {
            let mut inner = pair.into_inner();
            let bindings = parse_bind_var(errata, inner.next().unwrap(), file_id)?;
            let expr = parse_ty_expr(errata, inner.next().unwrap(), file_id)?;

            TyExpr::Pi {
                bindings: vec![bindings],
                expr: Box::new(expr)
            }
        },
        Rule::ty_arr => {
            let mut inner = pair.into_inner();

            let mut exprs = vec![
                parse_ty_expr(errata, inner.next().unwrap(), file_id)?
            ];
            let right = parse_ty_expr(errata, inner.next().unwrap(), file_id)?;

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
                parse_ty_expr(errata, inner.next().unwrap(), file_id)?
            ];
            let mut right = inner.next().unwrap();
            loop {
                if right.as_rule() != Rule::ty_app { break }

                inner = right.into_inner();
                let e1 = inner.next().unwrap();
                exprs.push(parse_ty_expr(errata, e1, file_id)?);

                right = inner.next().unwrap();
            }

            exprs.push(parse_ty_expr(errata, right, file_id)?);

            TyExpr::App(exprs)
        },
        Rule::name =>
            TyExpr::Name(pair.as_str().into()),
        _ => todo!()
    };

    Ok(Ranged {
        data: expr,
        range: Some(loc)
    })
}

fn parse_bind_var(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize,
) -> Result<Ranged<Var>, ()> {
    let loc = pair_location(&pair, file_id);
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap();
    let ty = inner.next()
        .map(|p| parse_ty_expr(errata, p, file_id))
        .map_or(Ok(None), |r| r.map(Some))?;

    let var = Var {
        name: Ranged {
            data: name.as_str().into(),
            range: Some(pair_location(&name, file_id))
        },
        ty
    };

    Ok(Ranged {
        data: var,
        range: Some(loc)
    })
}

fn push_clauses(clauses: &mut Vec<Clause>, mut dexpr: Ranged<DExpr>) {
    if let DExpr::And(es) = dexpr.data {
        for e in es {
            push_clauses(clauses, e)
        }
        return
    }
    
    let mut vars = vec![];
    while let DExpr::Pi { bindings, expr } = dexpr.data {
        vars.extend(bindings);
        dexpr = *expr
    }

    let range = dexpr.range;
    let (atomic, goal) = match dexpr.data {
        DExpr::And(es) => {
            for e in es {
                let dexpr = DExpr::Pi {
                    bindings: vars.clone(),
                    expr: Box::new(e)
                };

                push_clauses(clauses, Ranged::new(dexpr))
            }

            return
        },
        DExpr::Coimp { left, right } =>
            (left, Some(right)),
        DExpr::Atom(a) => {
            let atomic = Ranged {
                data: a,
                range
            };
            (atomic, None)
        },
        _ => unreachable!()
    };

    let clause = Clause {
        bindings: vars,
        root: atomic,
        goal,
    };

    clauses.push(clause)
}

fn is_atomic(rule: Rule) -> bool {
    match rule {
        Rule::cl_encl
        | Rule::cl_lam
        | Rule::cl_app
        | Rule::name
        | Rule::decimal
        | Rule::hexdecimal
        | Rule::string =>
            true,
        _ =>
            false
    }
}

fn parse_atomic(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Result<Ranged<Atomic>, ()> {
    let rule = pair.as_rule();
    let loc = pair_location(&pair, file_id);

    let atomic = match rule {
        Rule::cl_encl =>
            return parse_atomic(errata, pair.into_inner().next().unwrap(), file_id),
        Rule::cl_lam => {
            let mut inner = pair.into_inner();
            let bindings = parse_bind_var(errata, inner.next().unwrap(), file_id)?;
            let expr = parse_atomic(errata, inner.next().unwrap(), file_id)?;

            Atomic::Lambda {
                bindings: vec![bindings],
                expr: Box::new(expr)
            }
        },
        Rule::cl_app => {
            let mut inner = pair.into_inner();
            let mut exprs = vec![
                parse_atomic(errata, inner.next().unwrap(), file_id)?
            ];
            let mut right = inner.next().unwrap();
            loop {
                if right.as_rule() != Rule::cl_app { break }

                inner = right.into_inner();
                exprs.push(parse_atomic(errata, inner.next().unwrap(), file_id)?);

                right = inner.next().unwrap()
            }

            exprs.push(parse_atomic(errata, right, file_id)?);

            Atomic::App(exprs)
        },
        Rule::name =>
            Atomic::Name(pair.as_str().into()),
        Rule::decimal =>
            Atomic::Integer(Integer::Decimal(pair.as_str().into())),
        Rule::hexdecimal =>
            Atomic::Integer(Integer::Hexdecimal(pair.as_str().into())),
        Rule::string =>
            Atomic::String(pair.as_str().into()),
        _ =>
            return errata.push_err(Err(LoadErrorKind::NotAtomic(loc)))

    };

    Ok(Ranged {
        data: atomic,
        range: Some(loc)
    })
}

fn parse_d_expr(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Result<Ranged<DExpr>, ()> {
    let rule = pair.as_rule();
    let loc = pair_location(&pair, file_id);

    let expr = match rule {
        Rule::cl_encl =>
            return parse_d_expr(errata, pair.into_inner().next().unwrap(), file_id),
        Rule::cl_coimp
        | Rule::cl_imp => {
            let mut inner = pair.into_inner();
            let left = inner.next().unwrap();
            let right = inner.next().unwrap();

            let (left, right) =
                if rule == Rule::cl_coimp { (left, right) }
                else { (right, left) };

            let atomic = parse_atomic(errata, left, file_id)?;
            let gexpr = parse_g_expr(errata, right, file_id)?;

            DExpr::Coimp {
                left: atomic,
                right: gexpr,
            }
        },
        Rule::cl_and => {
            let mut inner = pair.into_inner();
            let mut exprs = vec![];

            let left = parse_d_expr(errata, inner.next().unwrap(), file_id)?;
            let right = parse_d_expr(errata, inner.next().unwrap(), file_id)?;

            if let Ranged { data: DExpr::And(es), .. } = left {
                exprs.extend(es)
            }
            else {
                exprs.push(left)
            }

            if let Ranged { data: DExpr::And(es), .. } = right {
                exprs.extend(es)
            }
            else {
                exprs.push(right)
            }

            DExpr::And(exprs)
        },
        Rule::cl_pi => {
            let mut inner = pair.into_inner();
            let bv = parse_bind_var(errata, inner.next().unwrap(), file_id)?;
            let expr = parse_d_expr(errata, inner.next().unwrap(), file_id)?;

            DExpr::Pi {
                bindings: vec![bv],
                expr: Box::new(expr),
            }
        },
        r if is_atomic(r) => {
            let pair = pair.into_inner().next().unwrap();

            DExpr::Atom(parse_atomic(errata, pair, file_id)?.data)
        },
        _ =>
            errata.push_err(Err(LoadErrorKind::NotDExpr(loc)))?
    };

    Ok(Ranged {
        data: expr,
        range: Some(loc)
    })
}

fn parse_g_expr(
    errata: &mut Errata,
    pair: Pair<Rule>,
    file_id: usize
) -> Result<Ranged<GExpr>, ()> {
    let rule = pair.as_rule();
    let loc = pair_location(&pair, file_id);
    let expr = match rule {
        Rule::cl_and => {
            let mut inner = pair.into_inner();
            let mut exprs = vec![];

            let left = parse_g_expr(errata, inner.next().unwrap(), file_id)?;
            let right = parse_g_expr(errata, inner.next().unwrap(), file_id)?;

            if let Ranged { data: GExpr::And(es), .. } = left {
                exprs.extend(es)
            }
            else {
                exprs.push(left)
            }

            if let Ranged { data: GExpr::And(es), .. } = right {
                exprs.extend(es)
            }
            else {
                exprs.push(right)
            }

            GExpr::And(exprs)
        },
        Rule::cl_or => {
            let mut inner = pair.into_inner();
            let mut exprs = vec![];

            let left = parse_g_expr(errata, inner.next().unwrap(), file_id)?;
            let right = parse_g_expr(errata, inner.next().unwrap(), file_id)?;

            if let Ranged { data: GExpr::Or(es), .. } = left {
                exprs.extend(es)
            }
            else {
                exprs.push(left)
            }

            if let Ranged { data: GExpr::Or(es), .. } = right {
                exprs.extend(es)
            }
            else {
                exprs.push(right)
            }

            GExpr::Or(exprs)
        },
        Rule::cl_sigma => {
            let mut inner = pair.into_inner();
            let bv = parse_bind_var(errata, inner.next().unwrap(), file_id)?;
            let expr = parse_g_expr(errata, inner.next().unwrap(), file_id)?;

            GExpr::Sigma {
                bindings: vec![bv],
                expr: Box::new(expr),
            }
        },
        Rule::cl_pi => {
            let mut inner = pair.into_inner();
            let bv = parse_bind_var(errata, inner.next().unwrap(), file_id)?;
            let expr = parse_g_expr(errata, inner.next().unwrap(), file_id)?;

            GExpr::Pi {
                bindings: vec![bv],
                expr: Box::new(expr),
            }
        },
        Rule::cl_imp
        | Rule::cl_coimp => {
            let mut inner = pair.into_inner();
            let left = inner.next().unwrap();
            let right = inner.next().unwrap();

            let (left, right) =
                if rule == Rule::cl_imp { (left, right) }
                else { (right, left) };

            let dexpr = parse_d_expr(errata, left, file_id)?;
            let gexpr = parse_g_expr(errata, right, file_id)?;

            GExpr::Imp {
                left: Box::new(dexpr),
                right: Box::new(gexpr)
            }
        },
        r if is_atomic(r) => {
            let pair = pair.into_inner().next().unwrap();

            GExpr::Atom(parse_atomic(errata, pair, file_id)?.data)
        },
        _ => 
            errata.push_err(Err(LoadErrorKind::NotGExpr(loc)))?,
    };

    Ok(Ranged {
        data: expr,
        range: Some(loc)
    })
}

fn get_module_name(path: &Path) -> SmolStr {
    let name = path.file_stem().unwrap().to_string_lossy();

    name.as_ref().into()
}

impl Pass<PathBuf, Module> for FileLoader {
    fn run(&mut self, errata: &mut Errata, path: PathBuf) -> Result<Module, ()> {
        use pest::Parser;

        let source = std::fs::read_to_string(&path)
            .map_err(LoadErrorKind::IoError);
        let source = errata.push_err(source)?;

        let module_name = get_module_name(&path);
        let file_id = self.files.append(path);

        let pair = BluesParser::parse(Rule::root, &source)
            .map(|mut p| p.next().unwrap())
            .map_err(LoadErrorKind::PestError);
        let pair = errata.push_err(pair)?;

        parse_module(module_name, errata, pair, file_id)
    }
}

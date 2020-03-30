use std::cmp::Ordering;
use smol_str::SmolStr;

use wrapterm::*;

const LOGIC_PREDS: &'static [&'static str] = &[
    "pi",
    "sigma",
    ":-",
    ";",
    ",",
    "=>",
];

const BUILTIN_TYPES: &'static [&'static str] = &[
    "type",
    "o",
    "nat",
    "int",
    "string",
    "list"
];

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Assoc {
    None,
    Left,
    Right
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpDecl {
    pub op: Ranged<SmolStr>,
    pub assoc: Ranged<Assoc>,
    pub rel: Option<Ranged<(Ordering, SmolStr)>>
}

// fn builtin_decls() -> Vec<OpDecl> {
//     use Ordering::*;
//     use Assoc as A;

//     vec![
//         OpDecl {
//             op: ":-".into(),
//             assoc: A::None,
//             rel: None
//         },
//         OpDecl {
//             op: ";".into(),
//             assoc: A::Right,
//             rel: Some((Greater, ":-".into()))
//         },
//         OpDecl {
//             op: ",".into(),
//             assoc: A::Right,
//             rel: Some((Greater, ";".into()))
//         },
//         OpDecl {
//             op: "=>".into(),
//             assoc: A::Right,
//             rel: Some((Greater, ",".into()))
//         }
//     ]
// }

// pub struct OpLattice {
//     ops: Vec<(SmolStr, usize, Assoc)>,
//     lattice: bit_matrix::BitMatrix
// }

// impl OpLattice {
//     pub fn from_decls(decls: &[OpDecl]) -> OpLattice {
//         todo!()
//     }

//     pub fn cmp(&self, left: SmolStr, right: SmolStr) -> Option<Ordering> {
//         todo!()
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Raw {
        name: Ranged<SmolStr>,
        ty: Option<Ranged<TyExpr>>
    },
    Free {
        id: usize,
        ty: Option<Ranged<TyExpr>>
    },
    Bound {
        id: usize,
        ty: Option<Ranged<TyExpr>>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    name: SmolStr,
    ops: Vec<OpDecl>,
    ty_decls: Vec<TyDecl>,
    clauses: Vec<Clause>,
    queries: Vec<Ranged<GExpr>>,
}

impl Module {
    fn new(name: SmolStr) -> Self {
        Module {
            name,
            ops: vec![],
            ty_decls: vec![],
            clauses: vec![],
            queries: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyDecl {
    pub bindings: Vec<Ranged<Var>>,
    pub name: Ranged<SmolStr>,
    pub ty: Ranged<TyExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyExpr {
    Arr(Vec<Ranged<TyExpr>>),
    App(Vec<Ranged<TyExpr>>),
    Name(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
struct Clause {
    bindings: Vec<Ranged<Var>>,
    root: Ranged<SmolStr>,
    args: Vec<Ranged<Atomic>>,
    goal: Option<Ranged<GExpr>>,
}


#[derive(Debug, Clone, PartialEq)]
enum Atomic {
    Lambda {
        bindings: Vec<Ranged<Var>>,
        expr: Box<Ranged<Atomic>>
    },
    App(Ranged<SmolStr>, Vec<Ranged<Atomic>>),
    Name(SmolStr),
    Integer(Integer),
    Float(SmolStr),
    String(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Integer {
    Decimal(SmolStr),
    Hexdecimal(SmolStr),
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
enum GExpr {
    Atom(Atomic),
    Imp {
        left: Box<Ranged<DExpr>>,
        right: Box<Ranged<GExpr>>,
    },
}

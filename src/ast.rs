use std::collections::HashMap;

#[derive(Debug)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug)]
pub enum AstExpr {
    Binop(String, Box<AstExpr>, Box<AstExpr>),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstExpr>),
    Let(Vec<(String, AstExpr)>, Box<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    Num(f64),
    Id(String),
    Val(SciVal),   // used internally, no syntactical representation
}

#[derive(Debug)]
pub enum SciVal {
    Matrix(usize, usize, Vec<f64>),  // numrows, numcols, index = row*numcols + col
    Number(f64),
    Closure(HashMap<String, SciVal>, Vec<String>, Box<AstExpr>),
    Comclos(Box<SciVal>, Box<SciVal>),
}

impl ToOwned for AstExpr {
    type Owned = AstExpr;

    fn to_owned(&self) -> AstExpr {
        match self {
            AstExpr::Binop(op, lhs, rhs) => AstExpr::Binop(op.to_owned(), Box::new((*lhs).to_owned()), Box::new((*rhs).to_owned())),
            AstExpr::Lambda(params, inner_expr) => AstExpr::Lambda(params.to_vec(), Box::new((*inner_expr).to_owned())),
            AstExpr::FunApp(f, args) => {
                let mut newargs: Vec<AstExpr> = vec![];
                for i in 0..args.len() {
                    newargs.push(args[i].to_owned());
                }
                AstExpr::FunApp(Box::new((*f).to_owned()), newargs)
            }
            AstExpr::Let(bindings, e) => {
                let mut newbindings: Vec<(String, AstExpr)> = vec![];
                for i in 0..bindings.len() {
                    let (v, e1) = &bindings[i];
                    newbindings.push((v.to_owned(), e1.to_owned()));
                }
                AstExpr::Let(newbindings, Box::new((*e).to_owned()))
            }
            AstExpr::Matrix(r, c, v) => {
                let mut newv: Vec<AstExpr> = vec![];
                for i in 0..v.len() {
                    newv.push(v[i].to_owned());
                }
                AstExpr::Matrix(*r, *c, newv)
            }
            AstExpr::Num(n) => AstExpr::Num(*n),
            AstExpr::Id(x) => AstExpr::Id(x.to_owned()),
            AstExpr::Val(v) => AstExpr::Val(v.to_owned())
        }
    }
}

impl ToOwned for SciVal {
    type Owned = SciVal;

    fn to_owned(&self) -> SciVal {
        match self {
            SciVal::Matrix(r, c, v) => SciVal::Matrix(*r, *c, v.to_vec()),
            SciVal::Number(n) => SciVal::Number(*n),
            SciVal::Closure(env, params, inner_expr) => {
                let mut new_env: HashMap<String, SciVal> = HashMap::new();
                for (key, val) in env.iter() {
                    new_env.insert(key.to_owned(), val.to_owned());
                }
                SciVal::Closure(new_env, params.to_vec(), Box::new((*inner_expr).to_owned()))
            }
            SciVal::Comclos(cls1, cls2) => SciVal::Comclos(Box::new((*cls1).to_owned()), Box::new((*cls2).to_owned()))
        }
    }
}

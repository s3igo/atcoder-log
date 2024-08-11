use bpaf::{literal, Bpaf, Parser};

// Use `()` for enum variant restrictions

#[derive(Debug, Clone, Bpaf)]
pub enum Lang {
    Rust(#[bpaf(external(rust))] ()),
    Ocaml(#[bpaf(external(ocaml))] ()),
    Haskell(#[bpaf(external(haskell))] ()),
    Nim(#[bpaf(external(nim))] ()),
}

impl Lang {
    pub fn extension(&self) -> &'static str {
        match self {
            Lang::Rust(_) => "rs",
            Lang::Ocaml(_) => "ml",
            Lang::Haskell(_) => "hs",
            Lang::Nim(_) => "nim",
        }
    }
}

macro_rules! generate_parsers {
    ($($lang:ident),*) => {
        $(
            fn $lang() -> impl Parser<()> {
                literal(stringify!($lang))
            }
        )*
    };
}

generate_parsers!(rust, ocaml, haskell, nim);

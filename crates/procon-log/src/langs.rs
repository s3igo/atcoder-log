use std::fmt;

use bpaf::{literal, Bpaf, Parser};

// Use `()` due to restrictions on enum variants of bpaf

#[derive(Debug, Clone, Bpaf)]
pub enum Lang {
    Rust(#[bpaf(external(rust))] ()),
    Ocaml(#[bpaf(external(ocaml))] ()),
    Haskell(#[bpaf(external(haskell))] ()),
    Nim(#[bpaf(external(nim))] ()),
    Cpp(#[bpaf(external(cpp))] ()),
    Crystal(#[bpaf(external(crystal))] ()),
}

impl fmt::Display for Lang {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lang::Rust(_) => write!(f, "rust"),
            Lang::Ocaml(_) => write!(f, "ocaml"),
            Lang::Haskell(_) => write!(f, "haskell"),
            Lang::Nim(_) => write!(f, "nim"),
            Lang::Cpp(_) => write!(f, "cpp"),
            Lang::Crystal(_) => write!(f, "crystal"),
        }
    }
}

impl Lang {
    pub(crate) fn extension(&self) -> &'static str {
        match self {
            Lang::Rust(_) => "rs",
            Lang::Ocaml(_) => "ml",
            Lang::Haskell(_) => "hs",
            Lang::Nim(_) => "nim",
            Lang::Cpp(_) => "cpp",
            Lang::Crystal(_) => "cr",
        }
    }

    pub(crate) fn entrypoint(&self) -> String {
        match self {
            Lang::Rust(_) => "src/main.rs".to_string(),
            Lang::Haskell(_) => "app/Main.hs".to_string(),
            _ => format!("main.{}", self.extension()),
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

generate_parsers!(rust, ocaml, haskell, nim, cpp, crystal);

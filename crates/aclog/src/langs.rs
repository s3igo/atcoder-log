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
            Self::Rust(()) => write!(f, "rust"),
            Self::Ocaml(()) => write!(f, "ocaml"),
            Self::Haskell(()) => write!(f, "haskell"),
            Self::Nim(()) => write!(f, "nim"),
            Self::Cpp(()) => write!(f, "cpp"),
            Self::Crystal(()) => write!(f, "crystal"),
        }
    }
}

impl Lang {
    pub(crate) const fn extension(&self) -> &'static str {
        match self {
            Self::Rust(()) => "rs",
            Self::Ocaml(()) => "ml",
            Self::Haskell(()) => "hs",
            Self::Nim(()) => "nim",
            Self::Cpp(()) => "cpp",
            Self::Crystal(()) => "cr",
        }
    }

    pub(crate) fn entrypoint(&self) -> String {
        match self {
            Self::Rust(()) => "src/main.rs".to_string(),
            Self::Haskell(()) => "app/Main.hs".to_string(),
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

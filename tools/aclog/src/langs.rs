use std::fmt;

use anyhow::{Result, bail};
use bpaf::{Bpaf, Parser, literal};

// Use `()` due to restrictions on enum variants of bpaf

#[derive(Debug, Clone, Bpaf)]
pub enum Lang {
    Rust(#[bpaf(external(rust))] ()),
    Ocaml(#[bpaf(external(ocaml))] ()),
    Haskell(#[bpaf(external(haskell))] ()),
    Nim(#[bpaf(external(nim))] ()),
    Cpp(#[bpaf(external(cpp))] ()),
    Crystal(#[bpaf(external(crystal))] ()),
    Zig(#[bpaf(external(zig))] ()),
    Fsharp(#[bpaf(external(fsharp))] ()),
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
            Self::Zig(()) => write!(f, "zig"),
            Self::Fsharp(()) => write!(f, "fsharp"),
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
            Self::Zig(()) => "zig",
            Self::Fsharp(()) => "fs",
        }
    }

    pub(crate) fn entrypoint(&self) -> String {
        match self {
            Self::Rust(()) => "src/main.rs".to_string(),
            Self::Haskell(()) => "app/Main.hs".to_string(),
            Self::Zig(()) => "src/main.zig".to_string(),
            Self::Fsharp(()) => "Main.fs".to_string(),
            _ => format!("main.{}", self.extension()),
        }
    }

    /// Convert a string representation to a Lang enum
    ///
    /// # Errors
    /// Returns an error if the string doesn't match any supported language
    pub fn from(lang_str: &str) -> Result<Self> {
        match lang_str {
            "rust" => Ok(Self::Rust(())),
            "ocaml" => Ok(Self::Ocaml(())),
            "haskell" => Ok(Self::Haskell(())),
            "nim" => Ok(Self::Nim(())),
            "cpp" => Ok(Self::Cpp(())),
            "crystal" => Ok(Self::Crystal(())),
            "zig" => Ok(Self::Zig(())),
            "fsharp" => Ok(Self::Fsharp(())),
            _ => bail!("Unsupported language: {lang_str}"),
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

generate_parsers!(rust, ocaml, haskell, nim, cpp, crystal, zig, fsharp);

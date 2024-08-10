use std::path::PathBuf;

use bpaf::{literal, Bpaf, Parser};

macro_rules! generate_parsers {
    ($($lang:ident),*) => {
        $(
            fn $lang() -> impl Parser<bool> {
                literal(stringify!($lang)).map(|_| true)
            }
        )*
    };
}

generate_parsers!(ocaml, rust, haskell, nim);

#[derive(Debug, Clone, Bpaf)]
pub enum Lang {
    Rust {
        #[bpaf(external)]
        rust: bool,
    },
    Ocaml {
        #[bpaf(external)]
        ocaml: bool,
    },
    Haskell {
        #[bpaf(external)]
        haskell: bool,
    },
    Nim {
        #[bpaf(external)]
        nim: bool,
    },
}

/// procon-log: A tool for managing competitive programming logs
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Args {
    #[bpaf(command)]
    Open {
        #[bpaf(external)]
        lang: Lang,
        #[bpaf(positional("FILE"))]
        file: PathBuf,
    },
    #[bpaf(command)]
    Test {
        #[bpaf(external)]
        lang: Lang,
        #[bpaf(positional("FILE"))]
        file: PathBuf,
    },
    #[bpaf(command)]
    Submit {
        #[bpaf(external)]
        lang: Lang,
        #[bpaf(positional("FILE"))]
        file: PathBuf,
    },
    #[bpaf(command)]
    Save {
        #[bpaf(external)]
        lang: Lang,
        #[bpaf(positional("FILE"))]
        file: PathBuf,
    },
    #[bpaf(command)]
    Clear {
        #[bpaf(external)]
        lang: Lang,
        #[bpaf(positional("FILE"))]
        file: PathBuf,
    },
}

impl Args {
    pub fn parse() -> Self {
        args().run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_args_invariants() {
        args().check_invariants(false);
    }
}

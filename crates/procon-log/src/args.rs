use bpaf::Bpaf;

use crate::cmds::{clear, open, save, submit, test};

/// procon-log: A tool for managing competitive programming logs
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Args {
    /// Open a file to solve a task
    #[bpaf(command)]
    Open(#[bpaf(external(open::open))] open::Open),

    #[bpaf(command)]
    Test(#[bpaf(external(test::test))] test::Test),

    #[bpaf(command)]
    Submit(#[bpaf(external(submit::submit))] submit::Submit),

    #[bpaf(command)]
    Save(#[bpaf(external(save::save))] save::Save),

    #[bpaf(command)]
    Clear(#[bpaf(external(clear::clear))] clear::Clear),
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

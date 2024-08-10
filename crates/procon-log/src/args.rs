use bpaf::Bpaf;

use crate::cmds;

/// procon-log: A tool for managing competitive programming logs
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Args {
    #[bpaf(command)]
    Open(#[bpaf(external(cmds::open::open))] cmds::open::Open),

    #[bpaf(command)]
    Test(#[bpaf(external(cmds::test::test))] cmds::test::Test),

    #[bpaf(command)]
    Submit(#[bpaf(external(cmds::submit::submit))] cmds::submit::Submit),

    #[bpaf(command)]
    Save(#[bpaf(external(cmds::save::save))] cmds::save::Save),

    #[bpaf(command)]
    Clear(#[bpaf(external(cmds::clear::clear))] cmds::clear::Clear),
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

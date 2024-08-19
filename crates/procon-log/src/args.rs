use bpaf::Bpaf;

use crate::cmds::open;

/// procon-log: A tool for managing competitive programming logs
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Args {
    /// Open a file to solve a task
    #[bpaf(command, short('o'))]
    Open(#[bpaf(external(open::open))] open::Open),
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

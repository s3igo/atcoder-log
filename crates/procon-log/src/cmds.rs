mod open;
use anyhow::Result;
use bpaf::Bpaf;

pub trait Run {
    fn run(&self) -> Result<()>;
}

/// procon-log: A tool for managing competitive programming logs
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Cmds {
    /// Open a file to solve a task
    #[bpaf(command, short('o'))]
    Open(#[bpaf(external(open::open))] open::Open),
}

impl Cmds {
    pub fn dispatch() -> Result<()> {
        match cmds().run() {
            Self::Open(cmd) => cmd.run(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cmds_invariants() {
        cmds().check_invariants(false);
    }
}

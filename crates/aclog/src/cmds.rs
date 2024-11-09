mod open;
use bpaf::Bpaf;

pub trait Run {
    /// # Errors
    /// Returns an error if the command fails
    fn run(&self) -> anyhow::Result<()>;
}

/// aclog: A tool for managing AtCoder logs
#[allow(clippy::doc_markdown)]
#[derive(Debug, Clone, Bpaf)]
#[bpaf(options, version)]
pub enum Cmds {
    /// Open a file to solve a task
    #[bpaf(command, short('o'))]
    Open(#[bpaf(external(open::open))] open::Open),
}

impl Cmds {
    /// # Errors
    /// Returns an error if the command fails
    pub fn dispatch() -> anyhow::Result<()> {
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

use anyhow::Result;
use procon_log::{args::Args, cmds::Run as _};

fn main() -> Result<()> {
    match Args::parse() {
        Args::Open(cmd) => cmd.run()?,
    }

    Ok(())
}

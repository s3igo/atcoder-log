use anyhow::Result;
use procon_log::{args::Args, cmds::Run as _};

fn main() -> Result<()> {
    match Args::parse() {
        Args::Open(cmd) => cmd.run()?,
        Args::Test(cmd) => cmd.run()?,
        Args::Submit(cmd) => cmd.run()?,
        Args::Save(cmd) => cmd.run()?,
        Args::Clear(cmd) => cmd.run()?,
    }

    Ok(())
}

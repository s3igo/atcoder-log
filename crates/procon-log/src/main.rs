use procon_log::{args::Args, cmds::Run as _};

fn main() {
    match Args::parse() {
        Args::Open(cmd) => cmd.run().unwrap(),
        Args::Test(cmd) => cmd.run().unwrap(),
        Args::Submit(cmd) => cmd.run().unwrap(),
        Args::Save(cmd) => cmd.run().unwrap(),
        Args::Clear(cmd) => cmd.run().unwrap(),
    }
}

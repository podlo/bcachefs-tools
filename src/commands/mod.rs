use clap::Subcommand;

pub mod logger;
pub mod mount;
pub mod list;
pub mod completions;
pub mod subvolume;
pub mod debug;

pub use mount::mount;
pub use list::list;
pub use completions::completions;
pub use subvolume::subvolume;
pub use debug::debug;
pub use debug::list_bkeys;

#[derive(clap::Parser, Debug)]
#[command(name = "bcachefs")]
pub struct Cli {
    #[command(subcommand)]
    subcommands: Subcommands,
}

#[derive(Subcommand, Debug)]
enum Subcommands {
    List(list::Cli),
    Mount(mount::Cli),
    Completions(completions::Cli),
    #[command(visible_aliases = ["subvol"])]
    Subvolume(subvolume::Cli),
}

#[macro_export]
macro_rules! c_str {
    ($lit:expr) => {
        unsafe {
            std::ffi::CStr::from_ptr(concat!($lit, "\0").as_ptr() as *const std::os::raw::c_char)
                .to_bytes_with_nul()
                .as_ptr() as *const std::os::raw::c_char
        }
    };
}

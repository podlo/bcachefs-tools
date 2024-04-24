use clap::Parser;
use std::ffi::{c_char, CString};
use std::io::{BufRead, Write};

use bch_bindgen::bcachefs;
use bch_bindgen::c;
use bch_bindgen::fs::Fs;

mod bkey_types;
mod parser;

use bch_bindgen::c::bpos;

/// Debug a bcachefs filesystem.
#[derive(Parser, Debug)]
pub struct Cli {
    #[arg(required(true))]
    devices: Vec<std::path::PathBuf>,
}

#[derive(Debug)]
enum DebugCommand {
    Dump(DumpCommand),
    Update(UpdateCommand),
}

#[derive(Debug)]
struct DumpCommand {
    btree: String,
    bpos: bpos,
}

#[derive(Debug)]
struct UpdateCommand {
    btree: String,
    bpos: bpos,
    bkey: String,
    field: String,
    value: u64,
}

fn update(fs: &Fs, type_list: &bkey_types::BkeyTypes, cmd: UpdateCommand) {
    let bkey = CString::new(cmd.bkey.clone()).unwrap();
    let bkey = bkey.as_ptr() as *const c_char;

    let id: bch_bindgen::c::btree_id = cmd.btree.parse().expect("no such btree");

    if let Some((size, offset)) = type_list.get_member_layout(&cmd.bkey, &cmd.field) {
        let update = c::bkey_update {
            id,
            bkey,
            offset,
            size,
            value: cmd.value,
        };
        unsafe {
            c::cmd_update_bkey(fs.raw, update, cmd.bpos);
        }
    } else {
        println!("unknown field '{}'", cmd.field);
    }
}

fn dump(fs: &Fs, cmd: DumpCommand) {
    let id: bch_bindgen::c::btree_id = cmd.btree.parse().expect("no such btree");

    unsafe {
        c::cmd_dump_bkey(fs.raw, id, cmd.bpos);
    }
}

pub fn debug(argv: Vec<String>) -> i32 {
    fn prompt() {
        print!("bcachefs> ");
        std::io::stdout().flush().unwrap();
    }

    let opt = Cli::parse_from(argv);

    let fs_opts: bcachefs::bch_opts = Default::default();
    let fs = match Fs::open(&opt.devices, fs_opts) {
        Ok(fs) => fs,
        Err(_) => {
            return 1;
        }
    };

    let type_list = bkey_types::get_bkey_type_info();

    prompt();
    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if let Some(cmd) = parser::parse_command(&line) {
            match cmd {
                // usage: dump <btree_type> <bpos>
                DebugCommand::Dump(cmd) => dump(&fs, cmd),
                // usage: update <btree_type> <bpos> <bkey_type>.<field>=<value>
                DebugCommand::Update(cmd) => update(&fs, &type_list, cmd),
            }
        } else {
            println!("failed to parse a command");
        };
        prompt();
    }

    0
}

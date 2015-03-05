// Copyright 2014 The rust-pcre authors.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This is a port of the pcre project's `pcredemo` sample using rust-pcre bindings.

#![feature(exit_status, io)]
#![allow(unused_must_use)]

extern crate collect;
extern crate getopts;
extern crate pcre;

use collect::enum_set::EnumSet;
use getopts::Options;
use pcre::{Match, Pcre, pcre_version};
use std::collections::BTreeMap;
use std::env;
use std::io::prelude::*;
use std::io::stderr;

fn print_usage(program: &String) {
    println!("Usage: {} [options] pattern subject", program);
    println!("Options:");
    println!("    -g                  Find all matches");
    println!("    -h, --help          Print usage and exit");
    println!("    --version           Print version information and exit");
}

fn print_version_info() {
    println!("rust-pcre 0.1 compiled against libpcre {}", pcre_version());
}

fn print_match(m: &Match, name_table: &BTreeMap<String, Vec<usize>>) {
    println!("Match succeeded at offset {}", m.group_start(0));

    // Show captured substrings by number.
    let mut i = 0;
    while i < m.string_count() {
        println!("{:2}: {}", i, m.group(i));
        i += 1;
    }

    let name_count = name_table.len();
    if name_count <= 0 {
        println!("No named substrings");
    } else {
        println!("Named substrings:");
        for (name, n_vec) in name_table.iter() {
            for n in n_vec.iter() {
                println!("({}) {}: {}", *n, *name, m.group(*n));
            }
        }
    }
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let program = &args[0];

    let mut opts = Options::new();
    opts.optflag("g", "", "find all matches")
        .optflag("h", "help", "print usage and exit")
        .optflag("", "version", "print version information and exit");

    let opt_matches = match opts.parse(&args[1..]) {
        Ok(m)  => m,
        Err(f) => {
            writeln!(&mut stderr(), "Error: {}", f.to_err_msg());
            env::set_exit_status(1);
            return;
        }
    };

    if opt_matches.opt_present("h") || opt_matches.opt_present("help") {
        print_usage(program);
        return;
    }

    if opt_matches.opt_present("version") {
        print_version_info();
        return;
    }

    let find_all = opt_matches.opt_present("g");
    if opt_matches.free.len() == 0 {
        writeln!(&mut stderr(), "Error: No pattern");
        env::set_exit_status(1);
        return;
    } else if opt_matches.free.len() == 1 {
        writeln!(&mut stderr(), "Error: No subject");
        env::set_exit_status(1);
        return;
    } else if opt_matches.free.len() > 2 {
        writeln!(&mut stderr(), "Error: Too many command line arguments");
        env::set_exit_status(1);
        return;
    }

    let pattern = &opt_matches.free[0];
    let subject = &opt_matches.free[1];

    let mut compile_options = EnumSet::new();
    compile_options.insert(pcre::CompileOption::DupNames);
    let re = match Pcre::compile_with_options(&pattern,
                                              &compile_options) {
        Err(err) => {
            writeln!(&mut stderr(),
                     "Error: The pattern could not be compiled: {}", err);
            env::set_exit_status(1);
            return;
        },
        Ok(re) => re
    };
    let name_table = re.name_table();

    let opt_m = re.exec(subject);
    let m = match opt_m {
        None => {
            println!("No match");
            env::set_exit_status(1);
            return;
        }
        Some(m) => m
    };
    print_match(&m, &name_table);

    if find_all {
        let mut start_offset = m.group_end(0);
        loop {
            let opt_m = re.exec_from(subject, start_offset);
            let m = match opt_m {
                None => {
                    println!("\nNo more matches");
                    return;
                }
                Some(m) => m
            };

            println!("");
            print_match(&m, &name_table);

            start_offset = m.group_end(0);
        }
    }
}

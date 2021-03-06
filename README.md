# rust-pcre
[Rust](https://github.com/mozilla/rust) 0.12.0-pre wrapper for [libpcre](http://pcre.org/) 8.20+.

[![Build Status](https://travis-ci.org/cadencemarseille/rust-pcre.png?branch=master)](https://travis-ci.org/cadencemarseille/rust-pcre) (with [Hans Jørgen Hoel's rust-nightly](http://hiho.io/rust-ci/))

## Quick Start

**Note:** If you see linker errors when running `make install` (for example, "undefined reference to \`pcre_free_study'"), you might need to export an environment variable, `PCRE_LIBDIR`, that points to the directory containing libpcre's import library.

### Debian

Debian Squeeze's package for libpcre is for version 8.02 of the library, which is too old. If running Debian Squeeze, you will have to compile libpcre 8.20+ from source. (Tip: [GNU Stow](http://www.gnu.org/software/stow/) is an excellent tool for maintaining the `/usr/local` directory.)

On Debian Wheezy and newer, install the `libpcre3-dev` package:

    sudo apt-get install libpcre3-dev

Then `make install`.


### Fedora

Install the `pcre-devel` package. Then `make install`.

### Mac OS X

Mac OS 10.7 ships with version 8.02 of libpcre, so you'll need to install a newer version of the pcre library.

[Homebrew](http://brew.sh/) is highly recommended for installing this project's dependencies. With Homebrew, installing the latest versions of Rust and libpcre is as simple as:

    brew install rust pcre

To upgrade:

    brew update && brew upgrade rust pcre

With Rust and libpcre 8.20+ installed:

    make install

### Ubuntu
The libpcre packages for Ubuntu 10.04 LTS 'Lucid Lynx' and Ubuntu 12.04 LTS 'Precise Pangolin' are too old. If running lucid or precise, you will have to compile libpcre 8.20+ from source. (Tip: [GNU Stow](http://www.gnu.org/software/stow/) is an excellent tool for maintaining the `/usr/local` directory.)

On Ubuntu 12.10 'Quantal Quetzal' and newer, install the `libpcre3-dev` package:

    sudo apt-get install libpcre3-dev

Then `make install`.

## Usage
The basic use of the library involves compiling a pattern regular expression:

    let re = match Pcre::compile(pattern) {
        Err(err) => {
        	// compilation failed
        	return;
        },
        Ok(re) => re
    };

You can also pass options:

    let mut compile_options: EnumSet<CompileOption> = EnumSet::empty();
    compile_options.add(pcre::Caseless)
    let re = Pcre::compile_with_options(pattern, &compile_options).unwrap();

To test against a subject string, use one of the exec(), exec_from(), or exec_from_with_options() methods. For example:

    let opt_m = re.exec(subject);
    let m = match opt_m {
        None => { println("No match"); return; },
        Some(m) => m
    };

See the [source of `pcredemo`](https://github.com/cadencemarseille/rust-pcre/blob/master/src/pcredemo/main.rs) for a complete example.

You can view the latest documentation online at: http://www.rust-ci.org/cadencemarseille/rust-pcre/doc/pcre/

## Development

Patches and GitHub pull requests (PRs) are always welcome.

To run the tests:

    make test

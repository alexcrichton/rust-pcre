// Copyright 2014 The rust-pcre authors.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![crate_name = "pcre"]
#![crate_type = "lib"]
#![feature(unsafe_destructor, collections, std_misc)]
#![allow(dead_code, bad_style)]

#[macro_use] extern crate log;
extern crate collect;
extern crate libc;

use collect::enum_set::{CLike, EnumSet};
use libc::{c_int, c_uchar, c_ulong, c_void};
use std::collections::BTreeMap;
use std::ffi::{CString, CStr};
use std::ptr;
use std::str;

mod detail;

#[derive(Clone, Copy)]
pub enum CompileOption {
    Caseless = 0x00000001,
    Multiline = 0x00000002,
    DotAll = 0x00000004,
    Extended = 0x00000008,
    Anchored = 0x00000010,
    DollarEndOnly = 0x00000020,
    Extra = 0x00000040,
    Ungreedy = 0x00000200,
    NoAutoCapture = 0x00001000,
    AutoCallout = 0x00004000,
    FirstLine = 0x00040000,
    DupNames = 0x00080000,
    NewlineCR = 0x00100000,
    NewlineLF = 0x00200000,
    NewlineCRLF = 0x00300000,
    NewlineAny = 0x00400000,
    NewlineAnyCRLF = 0x00500000,
    BsrAnyCRLF = 0x00800000,
    BsrUnicode = 0x01000000,
    JavaScriptCompat = 0x02000000,
    Ucp = 0x20000000
}

#[derive(Clone, Copy)]
pub enum ExecOption {
    Anchored = 0x00000010,
    NotBol = 0x00000080,
    NotEol = 0x00000100,
    NotEmpty = 0x00000400,
    PartialSoft = 0x00008000,
    NewlineCR = 0x00100000,
    NewlineLF = 0x00200000,
    NewlineCRLF = 0x00300000,
    NewlineAny = 0x00400000,
    NewlineAnyCRLF = 0x00500000,
    BsrAnyCRLF = 0x00800000,
    BsrUnicode = 0x01000000,
    NoStartOptimise = 0x04000000,
    PartialHard = 0x08000000,
    NotEmptyAtStart = 0x10000000
}

pub const ExecPartial: ExecOption = ExecOption::PartialSoft;
pub const ExecNoStartOptimize: ExecOption = ExecOption::NoStartOptimise;

#[derive(Clone, Copy)]
pub enum ExtraOption {
    StudyData = 0x0001,
    MatchLimit = 0x0002,
    CalloutData = 0x0004,
    Tables = 0x0008,
    MatchLimitRecursion = 0x0010,
    Mark = 0x0020,
    ExecutableJit = 0x0040
}

#[derive(Clone, Copy)]
pub enum StudyOption {
    JitCompile = 0x0001,
    JitPartialSoftCompile = 0x0002,
    JitPartialHardCompile = 0x0004,

    /// Always create an extra block. Note: Requires PCRE version 8.32 or later.
    ExtraNeeded = 0x0008
}

#[derive(Debug)]
pub struct CompilationError {

    opt_err: Option<String>,

    erroffset: c_int

}

/// Wrapper for libpcre's `pcre` object (representing a compiled regular expression).
#[repr(C)]
pub struct Pcre {

    code: *const detail::pcre,

    extra: *mut PcreExtra,

    capture_count_: c_int,

    /// A spot to place a pointer-to-mark name string.
    mark_: *mut c_uchar

}

#[repr(C)]
pub struct PcreExtra {
    flags: c_ulong,
    study_data: *mut c_void,
    match_limit_: c_ulong,
    callout_data: *mut c_void,
    tables: *const c_uchar,
    match_limit_recursion_: c_ulong,
    mark: *mut *mut c_uchar,
    executable_jit: *mut c_void
}

/// Represents a match of a subject string against a regular expression.
pub struct Match<'a> {
    subject: &'a str,
    partial_ovector: Vec<c_int>,
    string_count_: c_int
}

/// Iterator type for iterating matches within a subject string.
pub struct MatchIterator<'a> {
    code: *const detail::pcre,
    extra: *const PcreExtra,
    capture_count: c_int,
    subject: &'a str,
    offset: c_int,
    options: EnumSet<ExecOption>,
    ovector: Vec<c_int>
}

impl CLike for CompileOption {
    unsafe fn from_u32(n: u32) -> CompileOption {
        match n {
            1 => CompileOption::Caseless,
            2 => CompileOption::Multiline,
            3 => CompileOption::DotAll,
            4 => CompileOption::Extended,
            5 => CompileOption::Anchored,
            6 => CompileOption::DollarEndOnly,
            7 => CompileOption::Extra,
            8 => CompileOption::Ungreedy,
            9 => CompileOption::NoAutoCapture,
            10 => CompileOption::AutoCallout,
            11 => CompileOption::FirstLine,
            12 => CompileOption::DupNames,
            13 => CompileOption::NewlineCR,
            14 => CompileOption::NewlineLF,
            15 => CompileOption::NewlineCRLF,
            16 => CompileOption::NewlineAny,
            17 => CompileOption::NewlineAnyCRLF,
            18 => CompileOption::BsrAnyCRLF,
            19 => CompileOption::BsrUnicode,
            20 => CompileOption::JavaScriptCompat,
            21 => CompileOption::Ucp,
            _ => panic!("unknown CompileOption number {}", n)
        }
    }

    fn to_u32(&self) -> u32 {
        match *self {
            CompileOption::Caseless => 1,
            CompileOption::Multiline => 2,
            CompileOption::DotAll => 3,
            CompileOption::Extended => 4,
            CompileOption::Anchored => 5,
            CompileOption::DollarEndOnly => 6,
            CompileOption::Extra => 7,
            CompileOption::Ungreedy => 8,
            CompileOption::NoAutoCapture => 9,
            CompileOption::AutoCallout => 10,
            CompileOption::FirstLine => 11,
            CompileOption::DupNames => 12,
            CompileOption::NewlineCR => 13,
            CompileOption::NewlineLF => 14,
            CompileOption::NewlineCRLF => 15,
            CompileOption::NewlineAny => 16,
            CompileOption::NewlineAnyCRLF => 17,
            CompileOption::BsrAnyCRLF => 18,
            CompileOption::BsrUnicode => 19,
            CompileOption::JavaScriptCompat => 20,
            CompileOption::Ucp => 21,
        }
    }
}

impl CLike for ExecOption {
    unsafe fn from_u32(n: u32) -> ExecOption {
        match n {
            1 => ExecOption::Anchored,
            2 => ExecOption::NotBol,
            3 => ExecOption::NotEol,
            4 => ExecOption::NotEmpty,
            5 => ExecOption::PartialSoft,
            6 => ExecOption::NewlineCR,
            7 => ExecOption::NewlineLF,
            8 => ExecOption::NewlineCRLF,
            9 => ExecOption::NewlineAny,
            10 => ExecOption::NewlineAnyCRLF,
            11 => ExecOption::BsrAnyCRLF,
            12 => ExecOption::BsrUnicode,
            13 => ExecOption::NoStartOptimise,
            14 => ExecOption::PartialHard,
            15 => ExecOption::NotEmptyAtStart,
            _ => panic!("unknown ExecOption::Option number {}", n)
        }
    }

    fn to_u32(&self) -> u32 {
        match *self {
            ExecOption::Anchored => 1,
            ExecOption::NotBol => 2,
            ExecOption::NotEol => 3,
            ExecOption::NotEmpty => 4,
            ExecOption::PartialSoft => 5,
            ExecOption::NewlineCR => 6,
            ExecOption::NewlineLF => 7,
            ExecOption::NewlineCRLF => 8,
            ExecOption::NewlineAny => 9,
            ExecOption::NewlineAnyCRLF => 10,
            ExecOption::BsrAnyCRLF => 11,
            ExecOption::BsrUnicode => 12,
            ExecOption::NoStartOptimise => 13,
            ExecOption::PartialHard => 14,
            ExecOption::NotEmptyAtStart => 15,
        }
    }
}

impl CLike for ExtraOption {
    unsafe fn from_u32(n: u32) -> ExtraOption {
        match n {
            1 => ExtraOption::StudyData,
            2 => ExtraOption::MatchLimit,
            3 => ExtraOption::CalloutData,
            4 => ExtraOption::Tables,
            5 => ExtraOption::MatchLimitRecursion,
            6 => ExtraOption::Mark,
            7 => ExtraOption::ExecutableJit,
            _ => panic!("unknown ExtraOption::Option number {}", n),
        }
    }

    fn to_u32(&self) -> u32 {
        match *self {
            ExtraOption::StudyData => 1,
            ExtraOption::MatchLimit => 2,
            ExtraOption::CalloutData => 3,
            ExtraOption::Tables => 4,
            ExtraOption::MatchLimitRecursion => 5,
            ExtraOption::Mark => 6,
            ExtraOption::ExecutableJit => 7,
        }
    }
}

impl CLike for StudyOption {
    unsafe fn from_u32(n: u32) -> StudyOption {
        match n {
            1 => StudyOption::JitCompile,
            2 => StudyOption::JitPartialSoftCompile,
            3 => StudyOption::JitPartialHardCompile,
            4 => StudyOption::ExtraNeeded,
            _ => panic!("unknown StudyOption::Option number {}", n)
        }
    }

    fn to_u32(&self) -> u32 {
        match *self {
            StudyOption::JitCompile => 1,
            StudyOption::JitPartialSoftCompile => 2,
            StudyOption::JitPartialHardCompile => 3,
            StudyOption::ExtraNeeded => 4,
        }
    }
}

impl CompilationError {
    pub fn message(&self) -> Option<String> {
        self.opt_err.clone()
    }

    pub fn offset(&self) -> usize {
        self.erroffset as usize
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.opt_err {
            None => write!(f, "compilation failed at offset {}",
                           self.erroffset),
            Some(ref s) => write!(f, "compilation failed at offset {}: {}",
                                  self.erroffset, s)
        }
    }
}

impl Pcre {
    /// Compiles the given regular expression.
    ///
    /// # Argument
    /// * `pattern` - The regular expression.
    pub fn compile(pattern: &str) -> Result<Pcre, CompilationError> {
        Pcre::compile_with_options(pattern, &EnumSet::new())
    }

    /// Compiles a regular expression using the given bitwise-OR'd options
    /// `options`.
    ///
    /// # Arguments
    /// * `pattern` - The regular expression.
    /// * `options` - Bitwise-OR'd compilation options. See the libpcre manpages,
    ///   `man 3 pcre_compile`, for more information.
    pub fn compile_with_options(pattern: &str, options: &EnumSet<CompileOption>)
                                -> Result<Pcre, CompilationError> {
        let pattern = CString::new(pattern).unwrap();
        unsafe {
            // Use the default character tables.
            let tableptr: *const c_uchar = ptr::null();
            match detail::pcre_compile(pattern.as_ptr(), options, tableptr) {
                Err((opt_err, erroffset)) => Err(CompilationError {
                    opt_err: opt_err,
                    erroffset: erroffset
                }),
                Ok(mut_code) => {
                    let code = mut_code as *const detail::pcre;
                    assert!(!code.is_null());
                    // Take a reference.
                    detail::pcre_refcount(code as *mut detail::pcre, 1);

                    let extra: *mut PcreExtra = ptr::null_mut();

                    let mut capture_count: c_int = 0;
                    detail::pcre_fullinfo(code, extra as *const PcreExtra,
                                          detail::PCRE_INFO_CAPTURECOUNT,
                                          &mut capture_count as *mut _ as *mut _);

                    Ok(Pcre {
                        code: code,
                        extra: extra,
                        capture_count_: capture_count,
                        mark_: ptr::null_mut()
                    })
                }
            }
        }
    }

    /// Returns the number of capture groups in the regular expression,
    /// including one for each named capture group.
    ///
    /// This count does not include "group 0", which is the full substring
    /// within a subject string that matches the regular expression.
    ///
    /// # See also
    /// * [name_count()](#method.name_count) - Returns the number of named
    ///   capture groups.
    pub fn capture_count(&self) -> usize {
        self.capture_count_ as usize
    }

    /// Enables the use of the mark field when matching the compiled regular
    /// expression. The pattern must have been previously studied and an extra
    /// block must have been created.
    ///
    /// To ensure that an extra block has been created, call
    /// [study_with_options()](#method.study_with_options) passing the
    /// [`StudyExtraNeeded`](enum.StudyOption.html#variant.StudyExtraNeeded)
    /// study option.
    ///
    /// # Return value
    ///
    /// `true` if the use of the mark field could be enabled. `false` otherwise,
    /// which signifies that an extra block needs to be created.
    pub fn enable_mark(&mut self) -> bool {
        unsafe {
            if self.extra.is_null() {
                false
            } else {
                (*self.extra).set_mark(&mut self.mark_);
                true
            }
        }
    }

    /// Returns the extra block, if one has been created.
    pub fn extra(&mut self) -> Option<&mut PcreExtra> {
        unsafe {
            if self.extra.is_null() {
                None
            } else {
                Some(&mut *(self.extra))
            }
        }
    }

    /// Matches the compiled regular expression against a given subject string
    /// `subject`.  If no match is found, then `None` is returned. Otherwise, a
    /// `Match` object is returned which provides access to the captured
    /// substrings as slices of the subject string.
    ///
    /// # Argument
    ///
    /// * `subject` - The subject string.
    ///
    /// # Performance notes
    ///
    /// This method is intended to be used to find individual matches. If
    /// multiple matches are desired, then a `MatchIterator` should be used
    /// because it is more efficient.
    ///
    /// If a regular expression will be used often, it might be worth studying
    /// it to possibly speed up matching. See the [study()](#method.study)
    /// method.
    #[inline]
    pub fn exec<'a>(&self, subject: &'a str) -> Option<Match<'a>> {
        self.exec_from(subject, 0)
    }

    /// Matches the compiled regular expression against a given subject string
    /// `subject` starting at offset `startoffset` within the subject string. If
    /// no match is found, then `None` is returned. Otherwise, a `Match` object
    /// is returned which provides access to the captured substrings as slices
    /// of the subject string.
    ///
    /// # Arguments
    /// * `subject` - The subject string.
    /// * `startoffset` - Starting offset within `subject` at which to begin
    ///   looking for a match.
    ///
    /// # Performance notes
    ///
    /// This method is intended to be used to find
    /// individual matches. If multiple matches are desired, then a
    /// `MatchIterator` should be used because it is more efficient.
    ///
    /// If a regular expression will be used often, it might be worth studying
    /// it to possibly speed up matching. See the [study()](#method.study)
    /// method.
    #[inline]
    pub fn exec_from<'a>(&self, subject: &'a str, startoffset: usize)
                         -> Option<Match<'a>> {
        self.exec_from_with_options(subject, startoffset, &EnumSet::new())
    }

    /// Matches the compiled regular expression against a given subject string
    /// `subject` starting at offset `startoffset` within the subject string and
    /// using the given bitwise-OR'd matching options `options`. If no match is
    /// found, then `None` is returned. Otherwise, a `Match` object is returned
    /// which provides access to the captured substrings as slices of the
    /// subject string.
    ///
    /// # Arguments
    ///
    /// * `subject` - The subject string.
    /// * `startoffset` - Starting offset within `subject` at which to begin
    ///   looking for a match.
    /// * `options` - Bitwise-OR'd matching options. See the libpcre manpages,
    ///   `man 3 pcre_exec`, for more information.
    ///
    /// # Performance notes
    ///
    /// This method is intended to be used to find individual matches. If
    /// multiple matches are desired, then a `MatchIterator` should be used
    /// because it is more efficient.
    ///
    /// If a regular expression will be used often, it might be worth studying
    /// it to possibly speed up matching. See the [study()](#method.study)
    /// method.
    #[inline]
    pub fn exec_from_with_options<'a>(&self, subject: &'a str,
                                      startoffset: usize,
                                      options: &EnumSet<ExecOption>)
                                      -> Option<Match<'a>> {
        let ovecsize = (self.capture_count_ + 1) * 3;
        let mut ovector = vec![0; ovecsize as usize];

        unsafe {
            let rc = detail::pcre_exec(self.code,
                                       self.extra as *const PcreExtra,
                                       subject.as_ptr() as *const _,
                                       subject.len() as c_int,
                                       startoffset as c_int,
                                       options,
                                       ovector.as_mut_ptr(),
                                       ovecsize as c_int);
            if rc >= 0 {
                Some(Match {
                    subject: subject,
                    partial_ovector: ovector[..((self.capture_count_ + 1) * 2) as usize].to_vec(),
                    string_count_: rc
                })
            } else {
                None
            }
        }
    }

    /// Returns the mark name from PCRE if set.
    ///
    /// # Return value
    /// `Some(str)` if PCRE returned a value for the mark.
    /// `None` if either there was no mark set or
    /// [enable_mark()](#method.enable_mark) was not called, or was
    /// unsuccessful.
    #[inline]
    pub fn mark(&self) -> Option<&str> {
        unsafe {
            if self.mark_.is_null() {
                None
            } else {
                let ptr = CStr::from_ptr(self.mark_ as *const _).to_bytes();
                str::from_utf8(ptr).ok()
            }
        }
    }

    /// Creates a `MatchIterator` for iterating through matches within the given
    /// subject string `subject`.
    ///
    /// # Argument
    /// * `subject` - The subject string.
    #[inline]
    pub fn matches<'a>(&self, subject: &'a str) -> MatchIterator<'a> {
        self.matches_with_options(subject, &EnumSet::new())
    }

    /// Creates a `MatchIterator` for iterating through matches within the given
    /// subject string `subject` using the given bitwise-OR'd matching options
    /// `options`.
    ///
    /// # Arguments
    /// * `subject` - The subject string.
    /// * `options` - Bitwise-OR'd matching options. See the libpcre manpages,
    ///   `man 3 pcre_exec`,
    ///   for more information.
    #[inline]
    pub fn matches_with_options<'a>(&self, subject: &'a str,
                                    options: &EnumSet<ExecOption>)
                                    -> MatchIterator<'a> {
        unsafe {
            let ovecsize = (self.capture_count_ + 1) * 3;
            MatchIterator {
                code: {
                    detail::pcre_refcount(self.code as *mut detail::pcre, 1);
                    self.code
                },
                extra: self.extra as *const PcreExtra,
                capture_count: self.capture_count_,
                subject: subject,
                offset: 0,
                options: options.clone(),
                ovector: vec![0; ovecsize as usize],
            }
        }
    }

    /// Returns the number of named capture groups in the regular expression.
    pub fn name_count(&self) -> usize {
        unsafe {
            let mut name_count: c_int = 0;
            detail::pcre_fullinfo(self.code, self.extra as *const PcreExtra,
                                  detail::PCRE_INFO_NAMECOUNT,
                                  &mut name_count as *mut _ as *mut _);
            name_count as usize
        }
    }

    /// Creates a name-to-number translation table that maps the name of each
    /// named capture group to the assigned group numbers.
    ///
    /// The value type of the returned `BTreeMap` is a `uint` vector because
    /// there can be more than one group number for a given name if the
    /// PCRE_DUPNAMES option is used when compiling the regular expression.
    pub fn name_table(&self) -> BTreeMap<String, Vec<usize>> {
        unsafe {
            let name_count = self.name_count();
            let mut tabptr: *const c_uchar = ptr::null();
            detail::pcre_fullinfo(self.code,
                                  self.extra as *const PcreExtra,
                                  detail::PCRE_INFO_NAMETABLE,
                                  &mut tabptr as *mut _ as *mut _);
            let mut name_entry_size: c_int = 0;
            detail::pcre_fullinfo(self.code,
                                  self.extra as *const PcreExtra,
                                  detail::PCRE_INFO_NAMEENTRYSIZE,
                                  &mut name_entry_size as *mut _ as *mut _);

            let mut name_table = BTreeMap::new();

            let mut i = 0;
            while i < name_count {
                let n: usize = ((ptr::read(tabptr) as usize) << 8) |
                               (ptr::read(tabptr.offset(1)) as usize);
                let name_cstring = CStr::from_ptr(tabptr.offset(2) as *const _);
                let name = str::from_utf8(name_cstring.to_bytes())
                               .unwrap().to_string();
                name_table.entry(name).get().unwrap_or_else(|v| {
                    v.insert(Vec::new())
                }).push(n);
                tabptr = tabptr.offset(name_entry_size as isize);
                i += 1;
            }

            name_table
        }
    }

    /// Studies the regular expression to see if additional information can be
    /// extracted which might speed up matching.
    ///
    /// # Return value
    /// `true` if additional information could be extracted. `false` otherwise.
    pub fn study(&mut self) -> bool {
        self.study_with_options(&EnumSet::new())
    }

    /// Studies the regular expression using the given bitwise-OR'd study
    /// options `options` to see if additional information can be extracted
    /// which might speed up matching.
    ///
    /// # Argument
    /// * `options` - Study options. See the libpcre manpages, `man 3
    ///   pcre_study`, for more information about each option.
    ///
    /// # Return value
    /// `true` if additional information could be extracted or the
    /// [`StudyExtraNeeded`](enum.StudyOption.html#variant.StudyExtraNeeded)
    /// option was passed. `false` otherwise.
    pub fn study_with_options(&mut self, options: &EnumSet<StudyOption>) -> bool {
        unsafe {
            // If something else has a reference to `code` then it probably has
            // a pointer to the current study data (if any). Thus, we shouldn't
            // free the current study data in that case.
            if detail::pcre_refcount(self.code as *mut detail::pcre, 0) != 1 {
                false
            } else {
                // Free any current study data.
                detail::pcre_free_study(self.extra as *mut PcreExtra);
                self.extra = ptr::null_mut();

                let extra = detail::pcre_study(self.code, options);
                self.extra = extra;
                !extra.is_null()
            }
        }
    }
}

impl Drop for Pcre {
    fn drop(&mut self) {
        unsafe {
            if detail::pcre_refcount(self.code as *mut detail::pcre, -1) == 0 {
                detail::pcre_free_study(self.extra as *mut PcreExtra);
                detail::pcre_free(self.code as *mut detail::pcre as *mut c_void);
            }
            self.extra = ptr::null_mut();
            self.code = ptr::null();
        }
    }
}

impl PcreExtra {
    /// Returns the match limit, if previously set by
    /// [set_match_limit()](#method.set_match_limit).
    ///
    /// The default value for this limit is set when PCRE is built. The default
    /// default is 10 million.
    pub fn match_limit(&self) -> Option<usize> {
        if (self.flags & (ExtraOption::MatchLimit as c_ulong)) == 0 {
            None
        } else {
            Some(self.match_limit_ as usize)
        }
    }

    /// Returns the recursion depth limit, if previously set by
    /// [set_match_limit_recursion()](#method.set_match_limit_recursion).
    ///
    /// The default value for this limit is set when PCRE is built.
    pub fn match_limit_recursion(&self) -> Option<usize> {
        if (self.flags & (ExtraOption::MatchLimitRecursion as c_ulong)) == 0 {
            None
        } else {
            Some(self.match_limit_recursion_ as usize)
        }
    }

    /// Sets the mark field.
    pub unsafe fn set_mark(&mut self, mark: &mut *mut c_uchar) {
        self.flags |= ExtraOption::Mark as c_ulong;
        self.mark = mark as *mut *mut c_uchar;
    }

    /// Sets the match limit to `limit` instead of using PCRE's default.
    pub fn set_match_limit(&mut self, limit: u32) {
        self.flags |= ExtraOption::MatchLimit as c_ulong;
        self.match_limit_ = limit as c_ulong;
    }

    /// Sets the recursion depth limit to `limit` instead of using PCRE's default.
    pub fn set_match_limit_recursion(&mut self, limit: u32) {
        self.flags |= ExtraOption::MatchLimitRecursion as c_ulong;
        self.match_limit_ = limit as c_ulong;
    }

    /// Unsets the mark field. PCRE will not save mark names when matching the
    /// compiled regular expression.
    pub fn unset_mark(&mut self) {
        self.flags &= !(ExtraOption::Mark as c_ulong);
        self.mark = ptr::null_mut();
    }
}

impl<'a> Match<'a> {
    /// Returns the start index within the subject string of capture group `n`.
    pub fn group_start(&self, n: usize) -> usize {
        self.partial_ovector[n * 2] as usize
    }

    /// Returns the end index within the subject string of capture group `n`.
    pub fn group_end(&self, n: usize) -> usize {
        self.partial_ovector[n * 2 + 1] as usize
    }

    /// Returns the length of the substring for capture group `n`.
    pub fn group_len(&self, n: usize) -> usize {
        let group_offsets = &self.partial_ovector[n * 2..];
        (group_offsets[1] - group_offsets[0]) as usize
    }

    /// Returns the substring for capture group `n` as a slice.
    #[inline]
    pub fn group(&'a self, n: usize) -> &'a str {
        let group_offsets = &self.partial_ovector[n * 2..];
        let start = group_offsets[0] as usize;
        let end = group_offsets[1] as usize;
        &self.subject[start..end]
    }

    /// Returns the number of substrings captured.
    pub fn string_count(&self) -> usize {
        self.string_count_ as usize
    }
}

impl<'a> Clone for MatchIterator<'a> {
    #[inline]
    fn clone(&self) -> MatchIterator<'a> {
        unsafe {
            MatchIterator {
                code: { detail::pcre_refcount(self.code as *mut detail::pcre, 1); self.code },
                extra: self.extra,
                capture_count: self.capture_count,
                subject: self.subject,
                offset: self.offset,
                options: self.options,
                ovector: self.ovector.clone()
            }
        }
    }
}

#[unsafe_destructor]
impl<'a> Drop for MatchIterator<'a> {
    fn drop(&mut self) {
        unsafe {
            if detail::pcre_refcount(self.code as *mut detail::pcre, -1) == 0 {
                detail::pcre_free_study(self.extra as *mut PcreExtra);
                detail::pcre_free(self.code as *mut detail::pcre as *mut c_void);
            }
            self.extra = ptr::null();
            self.code = ptr::null();
        }
    }
}

impl<'a> Iterator for MatchIterator<'a> {
    type Item = Match<'a>;

    #[inline]
    fn next(&mut self) -> Option<Match<'a>> {
        unsafe {
            let rc = detail::pcre_exec(self.code, self.extra,
                                       self.subject.as_ptr() as *const _,
                                       self.subject.len() as c_int,
                                       self.offset,
                                       &self.options,
                                       self.ovector.as_mut_ptr(),
                                       self.ovector.len() as c_int);
            if rc >= 0 {
                // Update the iterator state.
                self.offset = self.ovector[1];

                Some(Match {
                    subject: self.subject,
                    partial_ovector: self.ovector[..((self.capture_count + 1) * 2) as usize].to_vec(),
                    string_count_: rc,
                })
            } else {
                None
            }
        }
    }
}

/// Returns libpcre version information.
pub fn pcre_version() -> String {
    detail::pcre_version()
}

/*
 * Copyright (c) 2020, Aaron Miller
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
extern crate x11_dl;

use thiserror::Error;

use std::ffi::{c_void, CString};
use std::fmt;
use std::mem;
use std::os::raw::{c_int, c_uint};
use std::ptr;
use std::rc::Rc;
use std::slice;
use x11_dl::xlib as X;

#[derive(Error, Debug)]
pub enum XError {
    #[error("cannot connect to X server")]
    ConnectFail,
    #[error("failed to get child windows of root window")]
    QueryRootTreeFail,
    #[error("failed to get window info / attributes")]
    WinInfoFail,
    #[error("failed to move window {}", window_id)]
    MoveWindowFail { window_id: u64 },
    #[error("failed to get cursor position")]
    GetCursorPosFail,
}

struct Display {
    display: *mut X::_XDisplay,
    pub xlib: Rc<X::Xlib>,
    pub screen: c_int,
    pub root_win: u64,
}

impl Display {
    fn root_tree(&self) -> Result<Tree, XError> {
        // we won't use root_return or parent_return
        let mut root_return = mem::MaybeUninit::<X::Window>::uninit();
        let mut parent_return = mem::MaybeUninit::<X::Window>::uninit();
        let mut children = mem::MaybeUninit::<*mut X::Window>::uninit();
        let mut n_children = mem::MaybeUninit::<c_uint>::uninit();
        let status = unsafe {
            (self.xlib.XQueryTree)(
                self.display,
                self.root_win,
                root_return.as_mut_ptr(),
                parent_return.as_mut_ptr(),
                children.as_mut_ptr(),
                n_children.as_mut_ptr(),
            )
        };
        if status == 0 {
            return Err(XError::QueryRootTreeFail);
        }
        unsafe {
            Ok(Tree {
                children: children.assume_init(),
                children_len: n_children.assume_init() as usize,
                xlib: self.xlib.clone(),
            })
        }
    }

    fn wininfo(&self, win: X::Window) -> Result<WinInfo, XError> {
        let mut attr = mem::MaybeUninit::<X::XWindowAttributes>::uninit();
        let status =
            unsafe { (self.xlib.XGetWindowAttributes)(self.display, win, attr.as_mut_ptr()) };
        if status == 0 {
            return Err(XError::WinInfoFail);
        }
        let attr = unsafe { attr.assume_init() };
        Ok(WinInfo {
            id: win,
            height: attr.height,
            width: attr.width,
            x: attr.x,
            y: attr.y,
            viewable: attr.map_state == X::IsViewable,
            wm_should_ignore: attr.override_redirect != 0,
        })
    }

    fn move_window(
        &self,
        wininfo: &WinInfo,
        xoffset: c_int,
        yoffset: c_int,
    ) -> Result<WinInfo, XError> {
        let value_mask = 0x3; // update X and Y; see https://tronche.com/gui/x/xlib/window/configure.html#XWindowChanges
                              // The following is not actually mutated
        let mut win_changes = X::XWindowChanges {
            x: wininfo.x + xoffset,
            y: wininfo.y + yoffset,
            width: 0,
            height: 0,
            border_width: 0,
            sibling: 0,
            stack_mode: 0,
        };
        let status = unsafe {
            (self.xlib.XReconfigureWMWindow)(
                self.display,
                wininfo.id,
                self.screen,
                value_mask,
                &mut win_changes as *mut X::XWindowChanges,
            )
        };
        if status == 0 {
            return Err(XError::MoveWindowFail {
                window_id: wininfo.id,
            });
        }
        let mut new_wininfo = wininfo.clone();
        new_wininfo.x += xoffset;
        new_wininfo.y += yoffset;
        Ok(new_wininfo)
    }

    fn cursor_position(&self) -> Result<(c_int, c_int), XError> {
        let mut root_return = mem::MaybeUninit::<X::Window>::uninit(); // unused
        let mut child_return = mem::MaybeUninit::<X::Window>::uninit(); // unused
        let mut root_x_return = mem::MaybeUninit::<c_int>::uninit();
        let mut root_y_return = mem::MaybeUninit::<c_int>::uninit();
        let mut win_x_return = mem::MaybeUninit::<c_int>::uninit(); // unused
        let mut win_y_return = mem::MaybeUninit::<c_int>::uninit(); // unused
        let mut mask_return = mem::MaybeUninit::<c_uint>::uninit(); // unused
        unsafe {
            (self.xlib.XQueryPointer)(
                self.display,
                self.root_win,
                root_return.as_mut_ptr(),
                child_return.as_mut_ptr(),
                root_x_return.as_mut_ptr(),
                root_y_return.as_mut_ptr(),
                win_x_return.as_mut_ptr(),
                win_y_return.as_mut_ptr(),
                mask_return.as_mut_ptr(),
            )
        };
        // TODO: error handling?!?!?
        unsafe { Ok((root_x_return.assume_init(), root_y_return.assume_init())) }
    }
}

impl Drop for Display {
    fn drop(&mut self) {
        unsafe {
            (self.xlib.XCloseDisplay)(self.display);
        }
    }
}

struct Tree {
    // TODO: make a slice
    children: *mut X::Window,
    children_len: usize,
    xlib: Rc<X::Xlib>,
}

impl Tree {
    fn as_slice(&self) -> &[X::Window] {
        unsafe {
            if self.children_len == 0 {
                slice::from_raw_parts(ptr::NonNull::dangling().as_ptr(), 0)
            } else {
                slice::from_raw_parts(self.children, self.children_len)
            }
        }
    }
}

impl Drop for Tree {
    fn drop(&mut self) {
        unsafe {
            (self.xlib.XFree)(self.children as *mut c_void);
        }
    }
}

#[derive(Debug, Clone)]
struct WinInfo {
    id: u64,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    viewable: bool,
    wm_should_ignore: bool,
}

impl fmt::Display for WinInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let hidden_str = if !self.viewable {
            "(HIDDEN)"
        } else {
            "        "
        };
        write!(
            f,
            "WinInfo{}: id 0x{:0>7x} pos ({:>5}, {:>5}) dim ({:>5}, {:>5}) wm_shld_ign {}",
            hidden_str, self.id, self.x, self.y, self.width, self.height, self.wm_should_ignore
        )
    }
}

fn connect(xlib: Rc<X::Xlib>, display_name: Option<&str>) -> Result<Display, XError> {
    let name = if let Some(dname) = display_name {
        dname
    } else {
        ":0"
    };
    let display_name = CString::new(name).unwrap();
    let display = unsafe { (xlib.XOpenDisplay)(display_name.as_ptr()) };
    if display.is_null() {
        return Err(XError::ConnectFail);
    }

    // Store the screen because it'll come in handy
    let screen = unsafe { (xlib.XDefaultScreen)(display) };

    let root_win = unsafe { (xlib.XDefaultRootWindow)(display) };

    Ok(Display {
        display,
        xlib,
        screen,
        root_win,
    })
}

fn calc_1d_offset(high: c_int, low: c_int, target: c_int) -> Option<c_int> {
    if target < low {
        Some(target - low)
    } else if target > high {
        Some(target - high)
    } else {
        None
    }
}

fn calc_2d_offset(wininfo: &WinInfo, x: c_int, y: c_int) -> Option<(c_int, c_int)> {
    let xoffset = calc_1d_offset(wininfo.x + wininfo.width - 1, wininfo.x, x);
    let yoffset = calc_1d_offset(wininfo.y + wininfo.height - 1, wininfo.y, y);
    if xoffset.is_some() || yoffset.is_some() {
        Some((xoffset.unwrap_or(0), yoffset.unwrap_or(0)))
    } else {
        None
    }
}

fn main() {
    let xlib = X::Xlib::open().unwrap();
    let xlib = Rc::new(xlib);
    let display = connect(xlib.clone(), None).unwrap();

    let tree = display.root_tree().unwrap();
    let slice = tree.as_slice();
    println!("tree slice len: {}", slice.len());
    let wininfos: Vec<_> = slice
        .iter()
        .map(|win| display.wininfo(*win).unwrap())
        .collect();
    let mut filtered_count = 0;
    let filtered_wininfos: Vec<_> = wininfos
        .into_iter()
        .filter(|wininfo| !wininfo.wm_should_ignore && wininfo.width > 10 && wininfo.height > 10)
        .collect();
    for (i, wininfo) in filtered_wininfos.iter().enumerate() {
        println!("{:>4}: {}", i, wininfo);
        filtered_count += 1;
    }
    println!("FILTERED COUNT: {}", filtered_count);

    let (xcursor, ycursor) = display.cursor_position().unwrap();
    println!(
        "asking the window manager to move windows to ({}, {})...",
        xcursor, ycursor
    );
    for wininfo in filtered_wininfos.iter() {
        if let Some((xoffset, yoffset)) = calc_2d_offset(wininfo, xcursor, ycursor) {
            println!(
                "MOVE win with id 0x{:0>7x} by ({:>5}, {:>5})",
                wininfo.id, xoffset, yoffset
            );
            display.move_window(wininfo, xoffset, yoffset).unwrap();
        }
    }
}

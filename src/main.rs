extern crate x11_dl;

use thiserror::Error;

use std::ffi::{c_void, CString};
use std::fmt;
use std::mem;
use std::os::raw::c_uint;
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
}

struct Display {
    display: *mut X::_XDisplay,
    xlib: Rc<X::Xlib>,
}

impl Display {
    fn root_tree(&self) -> Result<Tree, XError> {
        let root_win = unsafe { (self.xlib.XDefaultRootWindow)(self.display) };
        // we won't use these two
        let mut root_return = mem::MaybeUninit::<X::Window>::uninit();
        let mut parent_return = mem::MaybeUninit::<X::Window>::uninit();
        let mut children = mem::MaybeUninit::<*mut X::Window>::uninit();
        let mut n_children = mem::MaybeUninit::<c_uint>::uninit();
        let status = unsafe {
            (self.xlib.XQueryTree)(
                self.display,
                root_win,
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

    Ok(Display { display, xlib })
}

fn main() {
    let xlib = X::Xlib::open().unwrap();
    let display = connect(Rc::new(xlib), None).unwrap();

    let tree = display.root_tree().unwrap();
    let slice = tree.as_slice();
    println!("tree slice len: {}", slice.len());
    let wininfos: Vec<_> = slice
        .iter()
        .map(|win| display.wininfo(*win).unwrap())
        .collect();
    let mut filtered_count = 0;
    for (i, wininfo) in wininfos
        .iter()
        .filter(|&wininfo| !wininfo.wm_should_ignore && wininfo.width > 10 && wininfo.height > 10)
        .enumerate()
    {
        println!("{:>4}: {}", i, wininfo);
        filtered_count += 1;
    }
    println!("FILTERED COUNT: {}", filtered_count);
}

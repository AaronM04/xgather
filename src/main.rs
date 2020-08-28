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
}

struct Display {
    display: *mut X::_XDisplay,
    pub xlib: Rc<X::Xlib>,
    pub screen: c_int,
}

impl Display {
    fn root_tree(&self) -> Result<Tree, XError> {
        let root_win = unsafe { (self.xlib.XDefaultRootWindow)(self.display) };
        // we won't use root_return or parent_return
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

    Ok(Display {
        display,
        xlib,
        screen,
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
    if xoffset.is_some() && yoffset.is_some() {
        Some((xoffset.unwrap(), yoffset.unwrap()))
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
        .filter_map(|wininfo| {
            // using filter_map instead of filter because of weird borrow issue I don't have time
            // to debug
            if !wininfo.wm_should_ignore && wininfo.width > 10 && wininfo.height > 10 {
                Some(wininfo)
            } else {
                None
            }
        })
        .collect();
    for (i, wininfo) in filtered_wininfos.iter().enumerate() {
        println!("{:>4}: {}", i, wininfo);
        filtered_count += 1;
    }
    println!("FILTERED COUNT: {}", filtered_count);

    println!("asking the window manager to move windows...");
    let (pointerx, pointery) = (500, 2000); //XXX get pointer pos
    for wininfo in filtered_wininfos.iter() {
        if let Some((xoffset, yoffset)) = calc_2d_offset(wininfo, pointerx, pointery) {
            println!("MOVE win with id 0x{:0>7x} by ({:>5}, {:>5})", wininfo.id, xoffset, yoffset);
            display.move_window(wininfo, xoffset, yoffset).unwrap();
        }
    }
}

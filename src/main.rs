extern crate x11_dl;

use thiserror::Error;

use x11_dl::xlib as X;
use std::ffi::CString;

#[derive(Error, Debug)]
pub enum XError {
    #[error("cannot connect to X server")]
    ConnectFail,
}


struct Display<'x> {
    display: *mut X::_XDisplay,
    xlib: &'x X::Xlib,
}

impl<'x> Display<'x> {
    fn root_tree(&mut self) -> Result<Tree<'x>, XError> {

        let root_win = unsafe {
            (self.xlib.XDefaultRootWindow)(self.display)
        };
        //XXX
    }
}

impl<'x> Drop for Display<'x> {
    fn drop(&mut self) {
        unsafe {
            (self.xlib.XCloseDisplay)(self.display);
        }
    }
}


fn main() {
    let xlib = X::Xlib::open().unwrap();
    let display = connect(&xlib, None).unwrap();

    let tree = display.root_tree().unwrap();
}

fn connect<'x>(xlib: &'x X::Xlib, display_name: Option<&str>) -> Result<Display<'x>, XError> {
    let name = if let Some(dname) = display_name { dname } else { ":0" };
    let display_name = CString::new(name).unwrap();
    let display = unsafe {
        (xlib.XOpenDisplay)(display_name.as_ptr())
    };
    if display.is_null() {
        return Err(XError::ConnectFail);
    }

    Ok(Display { display, xlib })
}

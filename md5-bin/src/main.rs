use std::ffi::{c_void, CString};
use std::mem::forget;

#[no_mangle]
pub extern "C" fn md5(input: *mut i8) -> *const i8 {
    let s = CString::new(format!(
        "{:x}",
        md5::compute(unsafe { CString::from_raw(input) }.into_bytes())
    ))
    .unwrap();
    let p = s.as_ptr();
    forget(s);
    p
}

#[no_mangle]
pub fn alloc(size: usize) -> *mut c_void {
    let mut buf = Vec::with_capacity(size);
    let p = buf.as_mut_ptr();
    forget(buf);
    p
}

fn main() {}

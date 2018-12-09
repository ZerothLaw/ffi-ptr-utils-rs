# ffi-ptr-utils-rs
Utilities for handling pointers in FFI contexts.

This crate provides one container type, three traits, and 4 placeholder implementations of those traits. 

The container type is `Ptr<T, impl PtrAllocator<T>, impl PtrClone<T>, impl PtrDestructor<T>>`. It represents
a non-null `*mut T` and if `T: Default`, can be created via `Default::default`, and if `T: Clone`, then 
`Ptr<T>` can be cloned. 

The `Ptr<T>` container makes it easy to pass around and safely handle a raw pointer in Rust contexts, 
including safe disposal or accessing the raw underlying pointer in order to pass it to an FFI context. 

## Safety 
It should be noted that the default destructor - `DefaultDestructor` - does **not** free `T`.  

If a basic `std::ptr::drop_in_place` call is safe to drop your `*mut T`, then use `DropDestructor`. 

## Usage 

```
extern crate ffi_ptr_utils;

use ffi_ptr_utils::{Ptr, DropDestructor};

fn main() -> Result<(), i32> {
    {
        let mut pv = Ptr::<_, _, _, DropDestructor>::heap(1000u16);
        //c_ffi_func(pv.consume())
    }
    Ok(())
}
```
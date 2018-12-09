#![no_std]
// lib.rs - MIT License
//  MIT License
//  Copyright (c) 2018 Tyler Laing (ZerothLaw)
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

//! # ffi-ptr-utils crate #
//! 
//! ## Description ## 
//! 
//! Helper container and types for handling pointer types, useful for FFI purposes.
//! 
//! The core type is [`Ptr<T>`]. There are also associated traits to implement 
//! different types of behavior, related to allocation, cloning, and freeing. 
//! 
//! The full type signature is `Ptr<T, PtrAllocator<T>, PtrClone<T>, PtrDestructor<T>>`. 
//! 
//! ## Traits ##
//! 
//!   * `PtrAllocator<T>` - Responsible for allocation of new Ts. 
//!   * `PtrClone<T>` - Responsible for cloning the `*mut T`. Either the pointer itself 
//!         is cloned ( so that no new T is allocated ) or a new T is allocated and a
//!         pointer is created which is stored in the container. 
//!   * `PtrDestructor<T>` - Responsible for deallocating the T if desired. 
//!  
//! ## Usage example ##
//! 
//! ```
//! extern crate ffi_ptr_utils;
//! 
//! use std::rc::Rc;
//! use std::alloc::System;
//! 
//! #[global_allocator]
//! static GLOBAL: System = System;
//! 
//! use ffi_ptr_utils::{Ptr, DefaultAllocator, DefaultClone, DropDestructor};
//! 
//! fn main() {
//!     let mut pv = Rc::new(12345678u32);
//!     let weak = Rc::downgrade(&pv);
//!     let p: Ptr<_, DefaultAllocator, DefaultClone, DropDestructor> = Ptr::with_checked(&mut pv as &mut _).unwrap();
//!     drop(p);
//!     assert_eq!(weak.upgrade().is_none(), true);
//! }
//! ```
//! 

#![doc(html_root_url = "https://docs.rs/ffi_ptr_utils/0.1.0")]

#![feature(optin_builtin_traits)]

#![feature(alloc)]
extern crate alloc;


#[cfg(std)]use std::alloc::{System};
 
#[cfg(std)] 
#[global_allocator]
static GLOBAL: System = System;

use alloc::boxed::Box;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::ptr::{drop_in_place, NonNull};

macro_rules! heap_alloc {
    ($value:expr) => {
        Box::into_raw(Box::new($value))
    };
}

/// Allocation trait, which is responsible for allocating a `*mut T`. 
pub trait PtrAllocator<T> {
    /// Workhorse allocation method
    fn allocate() -> *mut T;
}

/// Allocation trait for cloning the *mut T. Specific meaning is up to
/// the implementation. The common meaning (when you clone a `NonNull<T>` 
/// for example) is that a new pointer is created which points to the 
/// same memory location. Another meaning is cloning the `T` itself. 
pub trait PtrClone<T> {
    /// Clones `&T` - whatever that means in the implementation
    fn clone(t: &T) -> *mut T;
}

/// Destructor trait for handling destructing the pointer. The guarantee that 
/// `Ptr<T>` makes is that the `*mut T` will *never* be null. Whether the memory 
/// location the pointer addresses is initialized, allocated, aligned, etc, is 
/// not guaranteed. 
pub trait PtrDestructor<T> {
    /// Potentially frees the `T`.
    fn drop(ptr: *mut T);
}

/// Placeholder default type that implements `PtrAllocator<T>`
#[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DefaultAllocator;

/// Placeholder default type that implements `PtrClone<T>`
#[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DefaultClone;

/// Placeholder default type that implements `PtrDestructor<T>`
#[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DefaultDestructor;

/// Basic placeholder type for default drop-in-place destructor for pointers.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DropDestructor;

impl<T> PtrAllocator<T> for DefaultAllocator
where
    T: Default,
{
    /// Implementation of PtrAllocator<T> where T: Default. 
    fn allocate() -> *mut T {
        heap_alloc!(T::default())
    }
}

impl<T> PtrClone<T> for DefaultClone
where
    T: Clone,
{
    /// Implementation of PtrClone<T> where T: Clone. 
    /// This implementation allocates a new T and returns a raw pointer.
    fn clone(t: &T) -> *mut T {
        let new_t = <T as Clone>::clone(t);
        heap_alloc!(new_t)
    }
}

impl<T> PtrDestructor<T> for DefaultDestructor {
    /// Implementation of PtrDestructor<T>. 
    /// 
    /// ## Safety ##
    /// 
    /// *THIS LEAKS `T`.* 
    fn drop(_ptr: *mut T) {
        //do nothing
        //LEAKS T
    }
}

impl<T> PtrDestructor<T> for DropDestructor {
    /// Implementation of PtrDestructor<T> where the pointer is 
    /// dropped in place. 
    fn drop(ptr: *mut T) {
        unsafe {
            // Ptr<T> guarantees *mut T passed in here will NOT be null.
            drop_in_place(ptr);
        }
    }
}

/// Main pointer container type. Guarantees that the underlying pointer is non-null. 
/// Interacts meaningfully with the given implementations of the traits 
/// `PtrAllocator<T>`, `PtrClone<T>`, and `PtrDestructor<T>`. 
/// 
/// ## Invariance ## 
/// 
/// `Ptr<T>` is covariant across T, like `NonNull<T>`.   
#[derive(Debug, Eq, Ord)]
pub struct Ptr<T, A = DefaultAllocator, C = DefaultClone, D = DefaultDestructor>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    inner: Option<NonNull<T>>,
    _marker: PhantomData<T>,
    _mark_a: PhantomData<A>,
    _mark_c: PhantomData<C>,
    _mark_d: PhantomData<D>,
}

/// Negative-implementation in case NonNull<T> ever becomes `Send`
impl<T, A, C, D> !Send for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>, 
{}

/// Negative-implementation in case NonNull<T> ever becomes `Sync`
impl<T, A, C, D> !Sync for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>, 
{}

impl<T, A, C, D> Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    /// Constructs a new `Ptr<T>` from a `NonNull<T>`. 
    pub fn new(nn: NonNull<T>) -> Ptr<T, A, C, D> {
        Ptr {
            inner: Some(nn),
            _marker: PhantomData,
            _mark_a: PhantomData,
            _mark_c: PhantomData,
            _mark_d: PhantomData,
        }
    }

    /// Allocates a new `Ptr<T>` from a non-null `*mut T`. Will return `None` 
    /// if pointer is null.
    pub fn with_checked(ptr: *mut T) -> Option<Ptr<T, A, C, D>> {
        match NonNull::new(ptr) {
            Some(nn) => Some(Ptr::new(nn)),
            None => None,
        }
    }

    /// Obtain access to the inner pointer.
    pub fn as_ptr(&self) -> *mut T {
        match &self.inner {
            Some(ref nn) => nn.as_ptr(),
            None => unreachable!(),
        }
    }

    /// Obtain an `&T`. This method is unsafe because the NonNull implementation
    /// is unsafe. 
    pub unsafe fn as_ref(&self) -> &T {
        match &self.inner {
            Some(ref nn) => nn.as_ref(),
            None => unreachable!(),
        }
    }
    /// Obtain an `&mut T`. This method is unsafe because the NonNull implementation
    /// is unsafe. 
    pub unsafe fn as_mut(&mut self) -> &mut T {
        match &mut self.inner {
            Some(ref mut nn) => nn.as_mut(),
            None => unreachable!(),
        }
    }

    /// Cast one or more of the generic arguments into a different type. 
    /// IE. `Ptr<T, A, PtrClone1, D>` -> `Ptr<T, A, PtrClone2, D>`. 
    pub fn cast<Q, B, L, E>(&mut self) -> Ptr<Q, B, L, E>
    where
        B: PtrAllocator<Q>,
        L: PtrClone<Q>,
        E: PtrDestructor<Q>,
    {
        match self.inner.take() {
            Some(nn) => Ptr::new(nn.cast()),
            None => unreachable!(),
        }
    }

    /// Cast the `PtrClone<T>` implementation to a different one. 
    pub fn cast_clone<L>(&mut self) -> Ptr<T, A, L, D>
    where
        L: PtrClone<T>,
    {
        self.cast()
    }

    /// Cast the `PtrDestructor<T>` implementation to a different one. 
    pub fn cast_dtor<E>(&mut self) -> Ptr<T, A, C, E>
    where
        E: PtrDestructor<T>,
    {
        self.cast()
    }

    /// Consume the container and obtain access to the underlying pointer. 
    /// Make sure the T is going to be freed or else it will leak. 
    pub unsafe fn consume(mut self) -> *mut T {
        // Take to avoid drop destructor from freeing `T`. 
        match self.inner.take() {
            Some(nn) => nn.as_ptr(), 
            None => unreachable!()
        }
    }

    /// Places a `T` on the heap and returns a `Ptr<T>` container for the pointer. 
    pub fn heap(value: T) -> Ptr<T, A, C, D> {
        Ptr::with_checked(heap_alloc!(value)).unwrap()
    }
}

impl<T, A, C, D> Default for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
    T: Default
{
    fn default() -> Self {
        Ptr::with_checked(A::allocate()).unwrap()
    }
}

impl<T, A, C, D> Clone for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
    T: Clone,
{
    fn clone(&self) -> Self {
        match &self.inner {
            Some(ref nn) => {
                let t = unsafe { &*nn.as_ptr() };
                Ptr::with_checked(C::clone(t)).unwrap()
            }
            None => unimplemented!(),
        }
    }
}

impl<T, A, C, D> Drop for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn drop(&mut self) {
        let nn = match self.inner.take() {
            Some(nn) => nn,
            None => return,
        };
        D::drop(nn.as_ptr());
    }
}

impl<'a, T, A, C, D> From<&'a mut T> for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn from(reference: &'a mut T) -> Self {
        Ptr::new(NonNull::from(reference))
    }
}

impl<'a, T, A, C, D> From<&'a T> for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn from(reference: &'a T) -> Self {
        Ptr::new(NonNull::from(reference))
    }
}

impl<T, A, C, D> AsRef<T> for Ptr<T, A, C, D> 
where
    A: PtrAllocator<T>, 
    C: PtrClone<T>, 
    D: PtrDestructor<T>
{
    fn as_ref(&self) -> &T {
        unsafe {self.as_ref()}
    }
}

impl<T, A, C, D> AsMut<T> for Ptr<T, A, C, D> 
where
    A: PtrAllocator<T>, 
    C: PtrClone<T>, 
    D: PtrDestructor<T>
{
    fn as_mut(&mut self) -> &mut T {
        unsafe {self.as_mut()}
    }
}

impl<T, A, C, D> Hash for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.inner {
            Some(ref nn) => nn.hash(state),
            None => unreachable!(),
        }
    }
}

impl<T, A, C, D> PartialOrd for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.inner, &other.inner) {
            (Some(ref nn), Some(ref onn)) => nn.partial_cmp(onn),
            (Some(_), None) => Some(Ordering::Greater),
            (None, Some(_)) => Some(Ordering::Less),
            (None, None) => Some(Ordering::Equal),
        }
    }
}

impl<T, A, C, D> PartialEq for Ptr<T, A, C, D>
where
    A: PtrAllocator<T>,
    C: PtrClone<T>,
    D: PtrDestructor<T>,
{
    fn eq(&self, other: &Self) -> bool {
        match (&self.inner, &other.inner) {
            (Some(ref nn), Some(ref onn)) => nn.eq(onn),
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use alloc::rc::Rc;
    #[test]
    fn run() {
        let mut pv = Rc::new(1000u16);
        let weak = Rc::downgrade(&pv);
        let ptr: Ptr<_, DefaultAllocator, DefaultClone, DefaultDestructor> =
            Ptr::with_checked(&mut pv as *mut _).unwrap();
        drop(ptr);
        assert_eq!(weak.upgrade().is_none(), false);

        let mut ptr: Ptr<_, DefaultAllocator, DefaultClone, DefaultDestructor> =
            Ptr::with_checked(&mut pv as *mut _).unwrap();
        let dptr = ptr.cast_dtor::<DropDestructor>();
        drop(dptr);
        assert_eq!(weak.upgrade().is_none(), true);
    }
}

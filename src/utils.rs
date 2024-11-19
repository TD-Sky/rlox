use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct RcCell<T> {
    inner: Rc<RefCell<T>>,
}

impl<T> Clone for RcCell<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T> RcCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(value)),
        }
    }

    pub fn borrow(&self) -> Ref<'_, T> {
        self.inner.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.inner.borrow_mut()
    }
}

impl<T> From<T> for RcCell<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

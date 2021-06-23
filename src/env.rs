#![allow(dead_code)]

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env<T>(Rc<Entry<T>>);

#[derive(Debug)]
enum Entry<T> {
    Empty,
    Extend(Env<T>, String, T),
}

impl<T> Env<T>
where
    T: Clone,
{
    pub fn empty() -> Env<T> {
        let head = Rc::new(Entry::Empty);
        Env(head)
    }

    pub fn extend(&self, x: &str, v: T) -> Env<T> {
        let link = Entry::Extend(self.clone(), String::from(x), v.clone());
        let head = Rc::new(link);
        Env(head)
    }

    pub fn lookup(&self, x: &str) -> Option<T> {
        match &*self.0 {
            Entry::Empty => None,
            Entry::Extend(env, y, v) => {
                if *x == *y {
                    Some(v.clone())
                } else {
                    env.lookup(x)
                }
            }
        }
    }
}

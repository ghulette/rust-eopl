use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env<T> {
    entry: Rc<Entry<T>>,
}

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
        let entry = Rc::new(Entry::Empty);
        Env { entry }
    }

    pub fn extend(&self, x: &str, v: T) -> Env<T> {
        let entry = Rc::new(Entry::Extend(self.clone(), String::from(x), v.clone()));
        Env { entry }
    }

    pub fn lookup(&self, x: &str) -> Option<T> {
        match &*self.entry {
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

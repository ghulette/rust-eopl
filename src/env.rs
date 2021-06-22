#[derive(Debug, Clone)]
pub enum Env<'a, T> {
    Empty,
    Extend(&'a Env<'a, T>, String, T),
}

impl<'a, T: Copy> Env<'_, T> {
    pub fn empty() -> Env<'a, T> {
        Env::Empty
    }

    pub fn extend(&'a self, x: &str, v: T) -> Env<'a, T> {
        Env::Extend(self, String::from(x), v)
    }

    pub fn apply(&self, x: &str) -> Option<T> {
        match self {
            Env::Empty => None,
            Env::Extend(env, y, v) => {
                if *x == *y {
                    Some(*v)
                } else {
                    env.apply(x)
                }
            }
        }
    }
}

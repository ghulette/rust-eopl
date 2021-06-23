use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env {
    head : Rc<Link>
}

#[derive(Debug)]
enum Link {
    Empty,
    Extend(Env, String, i32)
}

impl Env {
    pub fn empty() -> Env {
        let head = Rc::new(Link::Empty);
        Env { head }
    }

    pub fn extend(&self, x: &str, v: i32) -> Env {
        let link = Link::Extend(self.clone(), String::from(x), v);
        let head = Rc::new(link);
        Env { head }
    }

    pub fn apply(&self, x: &str) -> Option<i32> {
        match &*self.head {
            Link::Empty => None,
            Link::Extend(env, y, v) => {
                if *x == *y {
                    Some(*v)
                } else {
                    env.apply(x)
                }
            }
        }
    }
}

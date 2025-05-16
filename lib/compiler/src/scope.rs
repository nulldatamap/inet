use std::{collections::{hash_map::Entry, HashMap}, fmt};
use std::hash::Hash;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Option<String>, usize);

impl Name {
    pub fn unknown() -> Name {
        Name(None, 0)
    }

    pub fn from_id(id: usize) -> Name {
        Name(None, id)
    }

    pub fn global(n: impl ToString) -> Name {
        Name(Some(n.to_string()), 0)
    }

    pub fn new(n: impl ToString, id: usize) -> Name {
        Name(Some(n.to_string()), id)
    }

    pub fn into_string(self) -> String {
        assert_eq!(self.1, 0);
        self.0.unwrap()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_none() && self.1 == 0 {
            return f.write_str("<unknown>");
        }

        let n = self.0.as_ref().map(|x| &x[..]).unwrap_or("__gen");
        if self.1 == 0 {
            f.write_str(n)
        } else {
            write!(f, "{}#{}", n, self.1)
        }
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Slot<V> {
    pub prev: Option<Box<Slot<V>>>,
    pub value: V,
}

impl<V> Slot<V> {
    fn new(v: V) -> Slot<V> {
        Slot {
            prev: None,
            value: v
        }
    }
}


pub struct Scope<K, V> {
    layers: Vec<HashMap<K, Slot<V>>>,
}

impl<K, V> Scope<K, V> where K: Eq + Hash + Clone + fmt::Display {
    pub fn new() -> Scope<K, V> {
        Scope { layers: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        self.layers.is_empty()
    }

    pub fn enter(&mut self) {
        self.layers.push(HashMap::new())
    }

    pub fn leave(&mut self) {
        self.layers.pop().unwrap();
    }

    pub fn define(&mut self, n: K, v: V) {
        let slot = Slot::new(v);
        match self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n)
        {
            Entry::Occupied(mut e) => {
                let old_slot = e.insert(slot);
                let _ = e.get_mut().prev.insert(Box::new(old_slot));
            }
            Entry::Vacant(e) => {
                e.insert(slot);
            }
        }
    }

    pub fn undefine(&mut self, n: &K) -> V {
        if let Entry::Occupied(mut e) = self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n.clone())
        {
            if let Some(prev_slot) = e.get_mut().prev.take() {
                e.insert(*prev_slot).value
            } else {
                e.remove().value
            }
        } else {
            panic!("can't undefine non-exisitng variable `{}`", n);
        }
    }

    pub fn lookup(&mut self, n: &K) -> Option<&mut V> {
        for layer in self.layers.iter_mut().rev() {
            if let Some(slot) = layer.get_mut(n) {
                return Some(&mut slot.value);
            }
        }

        None
    }
}

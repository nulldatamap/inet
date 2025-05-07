use std::collections::{hash_map::Entry, HashMap};


pub struct Slot<T> {
    pub prev: Option<Box<Slot<T>>>,
    pub name: String,
    pub value: T,
}

impl<T> Slot<T> {
    fn new(name: String, v: T) -> Slot<T> {
        Slot {
            prev: None,
            name,
            value: v
        }
    }
}


pub struct Scope<T> {
    layers: Vec<HashMap<String, Slot<T>>>,
}

impl<T> Scope<T> {
    pub fn new() -> Scope<T> {
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

    pub fn define(&mut self, n: &str, v: T) {
        let slot = Slot::new(n.to_string(), v);
        match self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n.to_string())
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

    pub fn undefine(&mut self, n: &str) -> T {
        if let Entry::Occupied(mut e) = self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n.to_string())
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

    pub fn lookup(&mut self, n: &str) -> Option<&mut Slot<T>> {
        for layer in self.layers.iter_mut().rev() {
            if let Some(slot) = layer.get_mut(n) {
                return Some(slot);
            }
        }

        None
    }
}

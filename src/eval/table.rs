use std::collections::{HashMap, HashSet};

pub struct Table<T> {
    pub values: HashMap<usize, T>,
    pub current_id: usize,
    pub free_ids: Vec<usize>,
    pub marked: HashSet<usize>
}

impl<T: Clone> Table<T> {
    pub fn new() -> Self {
        Self { values: HashMap::new(), current_id: 0, free_ids: vec![], marked: HashSet::new() }
    }

    pub fn get_id(&mut self) -> usize {
        self.free_ids.last()
            .and_then(|id| Some(*id))
            .unwrap_or_else(|| {
                let id = self.current_id;
                self.current_id += 1;
                id
            })
    }

    pub fn get(&self, id: &usize) -> &T {
        self.values.get(id).unwrap()
    }

    pub fn get_mut(&mut self, id: &usize) -> &mut T {
        self.values.get_mut(id).unwrap()
    }

    pub fn make(&mut self, value: T) -> usize {
        let id = self.get_id();
        self.values.insert(id, value);
        id
    }

    pub fn copy(&mut self, id: &usize) -> usize {
        let value = self.get(id);
        self.make(value.clone())
    }

    pub fn sweep(&mut self) {
        let ptr_to_table = self as *mut Table<T>;  

        for id in self.values.keys() {
            if self.marked.contains(id) {
                self.marked.remove(id);
            } else {
                // Because we're removing the values we currently look,
                // it's safe to continue to iterate, we have no chance to
                // encounter them again (at least i think that is what happens)
                unsafe /* 😨 */ {
                    (*ptr_to_table).values.remove(id);
                }
            }
        }
    }

}
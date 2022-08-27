use sha2::Digest;

pub trait StableHash<DB: ?Sized, DigestState = sha2::Sha256> {
    fn update_hash(&self, state: &mut DigestState, db: &DB);
}

macro_rules! impl_integer_hash {
    {@$name:ident} => {
        impl<DB: ?Sized, Dig: Digest> StableHash<DB, Dig> for $name {
            fn update_hash(&self, state: &mut Dig, _: &DB) {
                state.update(self.to_le_bytes())
            }
        }
    };
    {$($name:ident)+} => {
        $(
            impl_integer_hash!{@$name}
        )*
    }
}

impl_integer_hash! {i8 u8 i16 u16 i32 u32 i64 u64 i128 u128}

impl<DB: ?Sized, Dig: Digest> StableHash<DB, Dig> for usize {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        let x: u64 = (*self).try_into().expect("usize too big for hash");
        x.update_hash(state, db)
    }
}


impl<DB: ?Sized, Dig: Digest> StableHash<DB, Dig> for isize {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        let x: u64 = (*self).try_into().expect("isize too big for hash");
        x.update_hash(state, db)
    }
}

impl<DB: ?Sized, Dig: Digest, T: StableHash<DB, Dig>> StableHash<DB, Dig> for Option<T> {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        match self {
            Some(x) => {
                state.update([0]);
                x.update_hash(state, db);
            }
            None => state.update([1]),
        }
    }
}

impl<DB: ?Sized, Dig: Digest> StableHash<DB, Dig> for [u8] {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        self.len().update_hash(state, db);
        state.update(self)
    }
}

impl<DB: ?Sized, Dig, T: StableHash<DB, Dig> + ?Sized> StableHash<DB, Dig> for &T {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        (*self).update_hash(state, db)
    }
}

impl<DB: ?Sized, Dig: Digest> StableHash<DB, Dig> for String {
    fn update_hash(&self, state: &mut Dig, db: &DB) {
        self.as_bytes().update_hash(state, db)
    }
}

pub fn hash_array<DB: ?Sized, Dig: Digest, T: StableHash<DB, Dig>>(
    array: &[T],
    state: &mut Dig,
    db: &DB,
) {
    let len: u64 = array.len().try_into().expect("slice too long");
    len.update_hash(state, db);
    for x in array {
        x.update_hash(state, db);
    }
}

use rand::Rng;

const PROB_WIDTH: usize = 16;

#[repr(C, align(64))]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RandomArray([u32; PROB_WIDTH]);

/// large safe primes less than 2^31
/// the points are evaluated modulo these primes
const PRIMES: RandomArray = RandomArray([
    2147483579, 2147483123, 2147482763, 2147481563, 2147481143, 2147480927, 2147480327, 2147479823,
    2147479787, 2147478899, 2147478083, 2147477807, 2147477687, 2147477159, 2147473283, 2147473127,
]);

/// modular negative inverse of PRIMES mod 2^32
const PRIMES_INV: RandomArray = RandomArray([
    591336077, 3734576325, 322729181, 4277457837, 3251658873, 3523893601, 2929915209, 3291685393,
    1348782141, 2124873797, 2783432085, 309199409, 3430714105, 635739881, 4042821845, 160637737,
]);

/// 2^32 mod PRIMES (one in Montgomery form)
const ONE: RandomArray = RandomArray([
    138, 1050, 1770, 4170, 5010, 5442, 6642, 7650, 7722, 9498, 11130, 11682, 11922, 12978, 20730,
    21042,
]);

/// 2^64 mod PRIME2S
const CONV_FACTOR: RandomArray = RandomArray([
    19044, 1102500, 3132900, 17388900, 25100100, 29615364, 44116164, 58522500, 59629284, 90212004,
    123876900, 136469124, 142134084, 168428484, 429732900, 442765764,
]);

const LARGEST_NON_WRAPPING: u32 = {
    let mut smallest = u32::MAX;
    let mut i = 0;
    while i < PROB_WIDTH {
        let ratio = PRIMES.0[i] / ONE.0[i];
        if ratio < smallest {
            smallest = ratio;
        }
        i += 1;
    }
    smallest
};

pub const ZERO: RandomArray = RandomArray([0; PROB_WIDTH]);

// u32::MAX is not a valid value for a RandomArray, so we can use it to
// indicate uninitialized values
pub const UNINIT: RandomArray = RandomArray([u32::MAX; PROB_WIDTH]);

impl RandomArray {
    pub fn is_uninit(&self) -> bool {
        self.0[0] == u32::MAX
    }

    pub fn add_array(&mut self, lhs: &RandomArray, rhs: &RandomArray) {
        for i in 0..PROB_WIDTH {
            self.0[i] = lhs.0[i] + rhs.0[i];
            if self.0[i] >= PRIMES.0[i] {
                self.0[i] -= PRIMES.0[i];
            }
        }
    }

    pub fn neg_array(&mut self, val: &RandomArray) {
        for i in 0..PROB_WIDTH {
            self.0[i] = PRIMES.0[i] - val.0[i];
            if self.0[i] == PRIMES.0[i] {
                self.0[i] = 0;
            }
        }
    }

    pub fn mul_array(&mut self, lhs: &RandomArray, rhs: &RandomArray) {
        // REDC-based multiplication
        // (RandomArray is always in Montgomery form)
        for i in 0..PROB_WIDTH {
            let prod = lhs.0[i] as u64 * rhs.0[i] as u64;
            let m = (prod as u32).wrapping_mul(PRIMES_INV.0[i]);
            let rres = prod + m as u64 * PRIMES.0[i] as u64;
            let mut res = (rres >> 32) as u32;
            if res >= PRIMES.0[i] {
                res -= PRIMES.0[i];
            }
            self.0[i] = res;
        }
    }

    pub fn array_from_const(&mut self, val: i128) {
        if val == 0 {
            *self = ZERO;
            return;
        }
        if val == 1 {
            *self = ONE;
            return;
        }
        let mut arr = RandomArray([0; PROB_WIDTH]);
        if val.unsigned_abs() < LARGEST_NON_WRAPPING as u128 {
            for i in 0..PROB_WIDTH {
                arr.0[i] = val.unsigned_abs() as u32 * ONE.0[i];
            }
            if val < 0 {
                self.neg_array(&arr);
            } else {
                *self = arr;
            }
            return;
        }
        for i in 0..PROB_WIDTH {
            arr.0[i] = (val % PRIMES.0[i] as i128) as u32;
        }
        self.mul_array(&arr, &CONV_FACTOR);
    }

    pub fn random_element(&mut self) {
        const PRIME_MASK: u32 = (1 << 31) - 1;
        let mut rng = rand::thread_rng();
        loop {
            rng.fill(&mut self.0);
            // the probability of rejecting any of the elements of the array
            // is ~3*10^-5, so it's better to not early-exit since this
            // enables auto-vectorization
            let mut reject = false;
            for i in 0..PROB_WIDTH {
                self.0[i] &= PRIME_MASK;
                if self.0[i] >= PRIMES.0[i] {
                    reject = true;
                }
            }
            if !reject {
                return;
            }
        }
    }
}

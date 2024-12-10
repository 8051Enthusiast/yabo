use std::num::NonZeroU32;

use crate::Arena;

#[derive(Clone, Debug)]
enum RowBits {
    Outer(Box<[u64]>),
    Inner(u64),
}

impl Default for RowBits {
    fn default() -> Self {
        RowBits::Inner(0)
    }
}

impl RowBits {
    fn with_capacity(capacity: usize) -> Self {
        RowBits::Outer(vec![0; capacity / 64 + 1].into_boxed_slice())
    }

    fn bits(&self) -> &[u64] {
        match self {
            RowBits::Outer(slice) => slice,
            RowBits::Inner(inner) => std::slice::from_ref(inner),
        }
    }

    fn bits_mut(&mut self) -> &mut [u64] {
        match self {
            RowBits::Outer(slice) => slice,
            RowBits::Inner(inner) => std::slice::from_mut(inner),
        }
    }

    fn bit_capacity(&self) -> usize {
        self.bits().len() * 64
    }

    fn get(&self, bit: usize) -> bool {
        if bit >= self.bit_capacity() {
            return false;
        }
        match self {
            RowBits::Outer(slice) => {
                let (word, bit) = (bit / 64, bit % 64);
                slice[word] & (1 << bit) != 0
            }
            RowBits::Inner(inner) => inner & (1 << bit) != 0,
        }
    }

    fn set(&mut self, bit: usize) {
        if bit >= self.bit_capacity() {
            let mut new_vec = Vec::with_capacity(bit / 64 + 1);
            new_vec.extend_from_slice(self.bits());
            new_vec.resize(bit / 64 + 1, 0);
            *self = RowBits::Outer(new_vec.into_boxed_slice());
        }
        match self {
            RowBits::Outer(slice) => {
                let (word, bit) = (bit / 64, bit % 64);
                slice[word] |= 1 << bit;
            }
            RowBits::Inner(inner) => *inner |= 1 << bit,
        }
    }

    fn clear(&mut self, bit: usize) {
        match self {
            RowBits::Outer(slice) => {
                let (word, bit) = (bit / 64, bit % 64);
                slice[word] &= !(1 << bit);
            }
            RowBits::Inner(inner) => *inner &= !(1 << bit),
        }
    }

    fn subrange(&self, start: usize, end: usize) -> Self {
        let mut new = RowBits::Inner(0);
        for i in (start..end).rev() {
            if self.get(i) {
                new.set(i - start);
            }
        }
        new
    }

    fn has_active_bits_above_or_at(&self, bit: u32) -> bool {
        let (word, bit) = (bit as usize / 64, bit as usize % 64);
        let bits = self.bits();
        if word >= bits.len() {
            return false;
        }
        if bits[word] & !((1 << bit) - 1) != 0 {
            return true;
        }
        bits.iter().skip(word + 1).any(|&x| x != 0)
    }

    fn extract_bound_bits(&mut self, bound_bits: u32) -> RowBits {
        let mut ret = Self::with_capacity(bound_bits as usize);
        for i in 0..(bound_bits as usize) {
            if self.get(i) {
                self.clear(i);
                ret.set(i);
            }
        }
        ret
    }

    const fn inner<const N: usize>(value: [bool; N]) -> Self {
        const { assert!(N <= 64) };
        let mut ret = 0;
        let mut idx = 0;
        while idx < N {
            ret |= (value[idx] as u64) << idx;
            idx += 1;
        }
        RowBits::Inner(ret)
    }
}

impl std::ops::BitOrAssign<&RowBits> for RowBits {
    fn bitor_assign(&mut self, rhs: &RowBits) {
        let mut new = if rhs.bit_capacity() > self.bit_capacity() {
            let mut new_vec = Vec::with_capacity(rhs.bits().len());
            new_vec.extend_from_slice(self.bits());
            new_vec.resize(rhs.bits().len(), 0);
            RowBits::Outer(new_vec.into_boxed_slice())
        } else {
            std::mem::take(self)
        };
        for (lhs, rhs) in new.bits_mut().iter_mut().zip(rhs.bits()) {
            *lhs |= rhs;
        }
        *self = new;
    }
}

impl PartialEq for RowBits {
    fn eq(&self, other: &Self) -> bool {
        let lhs = self.bits();
        let rhs = other.bits();
        let common_len = lhs.len().min(rhs.len());
        if lhs[..common_len] != rhs[..common_len] {
            return false;
        }
        let longer = match lhs.len().cmp(&rhs.len()) {
            std::cmp::Ordering::Less => rhs,
            std::cmp::Ordering::Equal => return true,
            std::cmp::Ordering::Greater => lhs,
        };
        longer[common_len..].iter().all(|&x| x == 0)
    }
}

impl Eq for RowBits {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarRow {
    bound_bits: u32,
    total_bits: NonZeroU32,
    bits: RowBits,
}

impl VarRow {
    pub fn empty(bound_bits: u32, total_bits: u32) -> Self {
        assert!(bound_bits <= total_bits);
        Self {
            bound_bits,
            total_bits: NonZeroU32::new(total_bits + 1).unwrap(),
            bits: RowBits::Inner(0),
        }
    }

    pub fn bound_bits(&self) -> u32 {
        self.bound_bits
    }

    pub fn total_bits(&self) -> u32 {
        self.total_bits.get() - 1
    }

    fn get(&self, bit: u32) -> bool {
        bit < self.total_bits() && self.bits.get(bit as usize)
    }

    fn set(&mut self, bit: u32) {
        assert!(bit < self.total_bits());
        self.bits.set(bit as usize);
    }

    fn active_bits(&self) -> impl DoubleEndedIterator<Item = u32> + '_ {
        (0..self.total_bits())
            .enumerate()
            .filter_map(move |(i, bit)| self.get(bit).then_some(i as u32))
    }

    fn subrange(&self, start: u32, end: u32) -> Self {
        assert!(start <= end);
        let total_bits = end - start;
        Self {
            bound_bits: self.bound_bits.saturating_sub(start).min(total_bits),
            total_bits: NonZeroU32::new(total_bits + 1).unwrap(),
            bits: self.bits.subrange(start as usize, end as usize),
        }
    }

    fn concat(&self, other: &Self) -> Self {
        let total_bits = self.total_bits() + other.total_bits();
        assert!(self.bound_bits == self.total_bits() || other.bound_bits == 0);
        let bound_bits = if self.bound_bits == self.total_bits() {
            self.total_bits() + other.bound_bits
        } else {
            self.bound_bits
        };
        let mut bits = RowBits::Inner(0);
        for bit in self
            .active_bits()
            .chain(other.active_bits().map(|bit| bit + self.total_bits()))
            .rev()
        {
            bits.set(bit as usize);
        }
        Self {
            bound_bits,
            total_bits: NonZeroU32::new(total_bits + 1).unwrap(),
            bits,
        }
    }

    fn expand_typevar(
        &self,
        columns: &[TypeVarOccurence],
        typevar_widths: &[u32],
        var_idx: u32,
    ) -> Self {
        let new_total_bits = expanded_count(self.total_bits(), columns, typevar_widths);
        let new_bound_bits = expanded_count(self.bound_bits, columns, typevar_widths);
        let new_bits = RowBits::with_capacity(new_total_bits as usize);
        let mut ret = Self {
            bound_bits: new_bound_bits,
            total_bits: NonZeroU32::new(new_total_bits + 1).unwrap(),
            bits: new_bits,
        };
        let mut bit_idx = 0;
        let mut column_var_idx = 0;
        for bit in 0..self.total_bits() {
            let (n, i) = if columns.get(column_var_idx).map(|x| x.matrix_idx) == Some(bit) {
                let n = typevar_widths[columns[column_var_idx].typevar_idx as usize];
                column_var_idx += 1;
                (n, var_idx)
            } else {
                (1, 0)
            };
            if self.get(bit) {
                ret.set(bit_idx + i)
            }
            bit_idx += n;
        }
        ret
    }

    fn has_active_bits_above_or_at(&self, bit: u32) -> bool {
        self.bits.has_active_bits_above_or_at(bit)
    }

    fn extract_bound_bits(&mut self) -> Self {
        let bits = self.bits.extract_bound_bits(self.bound_bits);
        let mut ret = Self::empty(self.bound_bits, self.bound_bits);
        ret.bits = bits;
        ret
    }

    fn add_bound_vars(&self, count: u32) -> Self {
        let new_bound_bits = self.bound_bits() + count;
        let new_total_bits = self.total_bits() + count;
        let mut new_bits = VarRow::empty(new_bound_bits, new_total_bits);
        for bit in self.active_bits() {
            if bit < self.bound_bits() {
                new_bits.set(bit);
            } else {
                new_bits.set(bit + count);
            }
        }
        new_bits
    }

    fn set_bound_bits(&mut self, bound_bits: u32) {
        assert!(bound_bits <= self.total_bits());
        self.bound_bits = bound_bits;
    }

    fn is_empty(&self) -> bool {
        !self.bits.has_active_bits_above_or_at(0)
    }

    const fn inner<const N: usize>(value: [bool; N]) -> Self {
        const { assert!(N <= 64) };
        let total_bits = const {
            let Some(total_bits) = NonZeroU32::new(N as u32 + 1) else {
                unreachable!();
            };
            total_bits
        };
        Self {
            bound_bits: 0,
            total_bits,
            bits: RowBits::inner(value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowParseError {
    UnexpectedChar(char),
    DuplicateBar,
}

impl std::fmt::Display for RowParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RowParseError::UnexpectedChar(c) => write!(f, "unexpected character: '{}'", c),
            RowParseError::DuplicateBar => write!(f, "duplicate bar"),
        }
    }
}

impl std::str::FromStr for VarRow {
    type Err = RowParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut idx = 0;
        let mut bound_bits = None;
        let mut ret = RowBits::Inner(0);
        for c in s.chars() {
            match c {
                '0' => idx += 1,
                '1' => {
                    ret.set(idx);
                    idx += 1;
                }
                '|' => {
                    if bound_bits.is_some() {
                        return Err(RowParseError::DuplicateBar);
                    }
                    bound_bits = Some(idx);
                }
                ' ' => {}
                otherwise => return Err(RowParseError::UnexpectedChar(otherwise)),
            }
        }
        let bound_bits = bound_bits.unwrap_or(idx);
        Ok(VarRow {
            bound_bits: bound_bits as u32,
            total_bits: NonZeroU32::new(idx as u32 + 1).unwrap(),
            bits: ret,
        })
    }
}

impl std::fmt::Display for VarRow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.total_bits() == 0 {
            return write!(f, "-");
        }
        for i in 0..self.total_bits() {
            if i == self.bound_bits() {
                write!(f, " | ")?;
            } else if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", self.get(i) as u8)?;
        }
        Ok(())
    }
}

impl std::ops::BitOrAssign<&VarRow> for VarRow {
    fn bitor_assign(&mut self, rhs: &VarRow) {
        assert_eq!(self.bound_bits(), rhs.bound_bits());
        self.bits |= &rhs.bits;
        self.total_bits = self.total_bits.max(rhs.total_bits);
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Row {
    #[default]
    True,
    Vars(VarRow),
}

impl Row {
    pub fn set(&mut self, bit: u32) {
        if let Row::Vars(row) = self {
            row.set(bit);
        }
    }

    pub fn subrange(&self, start: u32, end: u32) -> Self {
        match self {
            Row::True => Row::True,
            Row::Vars(row) => Row::Vars(row.subrange(start, end)),
        }
    }

    pub fn concat(&self, other: &Row) -> Self {
        match (self, other) {
            (Row::True, _) | (_, Row::True) => Row::True,
            (Row::Vars(lhs), Row::Vars(rhs)) => Row::Vars(lhs.concat(rhs)),
        }
    }

    pub fn expand_typevar(
        &self,
        columns: &[TypeVarOccurence],
        typevar_widths: &[u32],
        var_idx: u32,
    ) -> Self {
        match self {
            Row::True => Row::True,
            Row::Vars(row) => Row::Vars(row.expand_typevar(columns, typevar_widths, var_idx)),
        }
    }

    pub fn map<T>(&self, f: impl FnOnce(&VarRow) -> T) -> Option<T> {
        match self {
            Row::True => None,
            Row::Vars(row) => Some(f(row)),
        }
    }

    pub fn substitute_with_true_from(&self, bit: u32) -> Self {
        match self {
            Row::True => Row::True,
            Row::Vars(row) => {
                if row.has_active_bits_above_or_at(bit) {
                    Row::True
                } else {
                    Row::Vars(row.clone())
                }
            }
        }
    }

    pub fn add_bound_vars(&self, count: u32) -> Self {
        match self {
            Row::True => Row::True,
            Row::Vars(row) => Row::Vars(row.add_bound_vars(count)),
        }
    }

    pub fn set_bound_bits(&mut self, bound_bits: u32) {
        if let Row::Vars(row) = self {
            row.set_bound_bits(bound_bits);
        }
    }

    pub fn extract_bound_bits(&mut self, bound_bits: u32) -> Row {
        match self {
            Row::True => {
                *self = Row::Vars(VarRow::empty(bound_bits, bound_bits));
                Row::True
            }
            Row::Vars(var_row) => Row::Vars(var_row.extract_bound_bits()),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Row::True => false,
            Row::Vars(row) => row.is_empty(),
        }
    }

    pub fn is_true(&self) -> bool {
        *self == Row::True
    }

    pub const fn const_inner<const N: usize>(value: [bool; N]) -> Self {
        Row::Vars(VarRow::inner(value))
    }
}

impl std::ops::BitOrAssign<&Row> for Row {
    fn bitor_assign(&mut self, rhs: &Row) {
        match (self, rhs) {
            (Row::True, _) => {}
            (lhs, Row::True) => *lhs = Row::True,
            (Row::Vars(lhs), Row::Vars(rhs)) => *lhs |= rhs,
        }
    }
}

impl std::str::FromStr for Row {
    type Err = RowParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.trim() == "T" {
            Ok(Row::True)
        } else {
            s.parse().map(Row::Vars)
        }
    }
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Row::True => write!(f, "T"),
            Row::Vars(row) => write!(f, "{}", row),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Matrix<'a> {
    rows: &'a [Row],
}

#[derive(Clone, Copy, Debug)]
pub struct MatrixView<'a> {
    rows: [u32; 2],
    matrix: Matrix<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect<T> {
    pub matrix: T,
    pub bound: u32,
    pub total: u32,
}

impl<T> Rect<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Rect<U> {
        Rect {
            matrix: f(self.matrix),
            bound: self.bound,
            total: self.total,
        }
    }
}

impl<'a> Rect<MatrixView<'a>> {
    pub fn as_matrix(self) -> Rect<Matrix<'a>> {
        self.map(|x| x.as_matrix())
    }
}

impl<'a> Matrix<'a> {
    pub const fn from_rows(rows: &'a [Row]) -> Self {
        Self { rows }
    }

    pub fn view(self, row: std::ops::Range<u32>) -> MatrixView<'a> {
        MatrixView {
            rows: [row.start, row.end],
            matrix: self,
        }
    }

    pub fn row_count(&self) -> u32 {
        self.rows.len().try_into().unwrap()
    }

    pub fn rows(&self) -> &'a [Row] {
        self.rows
    }

    pub fn rect(self, bound: u32, total: u32) -> Rect<Matrix<'a>> {
        Rect {
            matrix: self,
            bound,
            total,
        }
    }
}

impl std::fmt::Display for Matrix<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in self.rows {
            writeln!(f, "{}", row)?;
        }
        Ok(())
    }
}

impl<'a> MatrixView<'a> {
    pub fn as_matrix(&self) -> Matrix<'a> {
        let [start, end] = self.rows;
        Matrix {
            rows: &self.matrix.rows[start as usize..end as usize],
        }
    }

    pub fn view_row_count(&self) -> u32 {
        self.rows[1] - self.rows[0]
    }

    pub fn subview(&self, range: std::ops::Range<u32>) -> MatrixView<'a> {
        let [start, end] = self.rows;
        assert!(range.end <= end - start);
        MatrixView {
            rows: [start + range.start, start + range.end],
            matrix: self.matrix,
        }
    }

    pub fn subview_from(&self, start: u32) -> MatrixView<'a> {
        let [old_start, new_end] = self.rows;
        let new_start = old_start + start;
        if new_start > new_end {
            panic!("out of bounds")
        }
        MatrixView {
            rows: [new_start, new_end],
            matrix: self.matrix,
        }
    }

    pub fn next_subview(&self, size: u32) -> MatrixView<'a> {
        let [_, end] = self.rows;
        let new_start = end;
        let new_end = end + size;
        if new_end > self.matrix.row_count() {
            panic!("out of bounds")
        }
        MatrixView {
            rows: [new_start, new_end],
            matrix: self.matrix,
        }
    }

    pub fn with_length(&self, length: u32) -> MatrixView<'a> {
        let [start, _] = self.rows;
        MatrixView {
            rows: [start, start + length],
            matrix: self.matrix,
        }
    }

    /// Runs a function on a subview, and returns the parent view with adjusted bounds
    pub fn transform_with_subview<'b>(
        &self,
        subview: MatrixView,
        transformed_subview: MatrixView<'b>,
    ) -> MatrixView<'b> {
        let [subview_start, subview_end] = subview.rows;
        let [view_start, view_end] = self.rows;
        assert!(subview_start >= view_start);
        assert!(subview_end <= view_end);
        let [new_subview_start, new_subview_end] = transformed_subview.rows;
        assert!(new_subview_start == subview_start);
        let length_diff = new_subview_end as i64 - subview_end as i64;
        let new_view_end = view_end as i64 + length_diff;

        MatrixView {
            rows: [view_start, new_view_end as u32],
            matrix: transformed_subview.matrix,
        }
    }

    pub fn rect(&self, col: u32, arg_col: u32) -> Rect<MatrixView> {
        Rect {
            matrix: *self,
            bound: col,
            total: arg_col,
        }
    }
}

pub fn multiply<'a>(out: &'a mut [Row], lhs: Matrix<'a>, rhs: Rect<Matrix<'a>>) {
    assert_eq!(lhs.rows.len(), out.len());
    for (out_row, a_row) in out.iter_mut().zip(lhs.rows.iter()) {
        let Row::Vars(a_row) = a_row else {
            *out_row = Row::True;
            continue;
        };
        let total_bits = a_row.total_bits() - rhs.matrix.row_count() + rhs.total;
        *out_row = Row::Vars(VarRow::empty(rhs.bound, total_bits));
        // rev() so that the row gets allocated to the right size on the first iteration
        for bit in a_row.active_bits().rev() {
            if let Some(row) = rhs.matrix.rows.get(bit as usize) {
                *out_row |= row;
            } else {
                let extra_bit_offset = bit - rhs.matrix.row_count() + rhs.total;
                out_row.set(extra_bit_offset);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct MatrixArena<'arena> {
    arena: &'arena Arena,
}

pub struct TypeVarOccurence {
    pub matrix_idx: u32,
    pub typevar_idx: u32,
}

fn expanded_count(count: u32, occurences: &[TypeVarOccurence], typevar_widths: &[u32]) -> u32 {
    let mut ret = count;
    for occ in occurences {
        if occ.matrix_idx >= count {
            break;
        }
        ret = ret + typevar_widths[occ.typevar_idx as usize] - 1;
    }
    ret
}

impl<'arena> MatrixArena<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self { arena }
    }

    pub fn new_matrix(&self, rows: impl IntoIterator<Item = Row>) -> Matrix<'arena> {
        Matrix {
            rows: self.arena.alloc_extend(rows),
        }
    }

    pub fn new_mutable_rows(&self, rows: impl IntoIterator<Item = Row>) -> &mut [Row] {
        self.arena.alloc_extend(rows)
    }

    pub fn new_from_str(&self, s: &str) -> Result<Matrix<'arena>, RowParseError> {
        let rows = s
            .lines()
            .map(|line| line.parse())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.new_matrix(rows))
    }

    pub fn new_zeroes(&self, rows: u32, bound: u32, total: u32) -> Rect<Matrix<'arena>> {
        let empty_row = Row::Vars(VarRow::empty(bound, total));
        Rect {
            matrix: self.new_matrix(std::iter::repeat(empty_row).take(rows as usize)),
            bound,
            total,
        }
    }

    pub fn identity(&self, size: u32) -> Matrix<'arena> {
        let out = self.arena.alloc_extend((0..size).map(|i| {
            let mut row = VarRow::empty(0, size);
            row.set(i);
            Row::Vars(row)
        }));
        Matrix { rows: out }
    }

    pub fn column_subrange(&self, matrix: Matrix, start: u32, end: u32) -> Matrix<'arena> {
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(matrix.row_count() as usize));
        for (out_row, row) in out.iter_mut().zip(matrix.rows) {
            *out_row = row.subrange(start, end);
        }
        Matrix { rows: out }
    }

    pub fn multiply(&self, lhs: Matrix, rhs: Rect<Matrix>) -> Matrix<'arena> {
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(lhs.row_count() as usize));
        multiply(out, lhs, rhs);
        Matrix { rows: out }
    }

    pub fn multiply_rhs_view(&self, lhs: Matrix, rhs: Rect<MatrixView>) -> MatrixView<'arena> {
        let new_row_count =
            rhs.matrix.matrix.row_count() - rhs.matrix.view_row_count() + lhs.row_count();
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(new_row_count as usize));
        let [start, rhs_end] = rhs.matrix.rows;
        let end = start + lhs.row_count();
        out[..start as usize].clone_from_slice(&rhs.matrix.matrix.rows[..start as usize]);
        out[end as usize..].clone_from_slice(&rhs.matrix.matrix.rows[rhs_end as usize..]);
        multiply(&mut out[start as usize..end as usize], lhs, rhs.as_matrix());
        let matrix = Matrix { rows: out };
        MatrixView {
            rows: [start, end],
            matrix,
        }
    }

    pub fn replace_columns(
        &self,
        lhs: MatrixView,
        rhs: Rect<Matrix>,
        start: u32,
        end: u32,
    ) -> MatrixView<'arena> {
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(lhs.matrix.row_count() as usize));
        let [start_row, end_row] = lhs.rows;
        out[..start_row as usize].clone_from_slice(&lhs.matrix.rows[..start_row as usize]);
        out[end_row as usize..].clone_from_slice(&lhs.matrix.rows[end_row as usize..]);
        for ((out_row, row), replaced) in out[start_row as usize..end_row as usize]
            .iter_mut()
            .zip(lhs.as_matrix().rows.iter())
            .zip(rhs.matrix.rows)
        {
            *out_row = row
                .subrange(0, start)
                .concat(replaced)
                .concat(&row.subrange(end, row.map(|x| x.total_bits()).unwrap_or(end)));
        }
        let matrix = Matrix { rows: out };
        MatrixView {
            rows: [start_row, end_row],
            matrix,
        }
    }

    pub fn multiply_lhs_view_column_subrange(
        &self,
        lhs: MatrixView,
        rhs: Rect<Matrix>,
        start: u32,
    ) -> MatrixView<'arena> {
        let end = start + rhs.matrix.row_count();
        let lhs_columns = self.column_subrange(lhs.as_matrix(), start, end);
        let replaced = Rect {
            matrix: self.multiply(lhs_columns, rhs),
            ..rhs
        };
        self.replace_columns(lhs, replaced, start, end)
    }

    pub fn partial_apply(&self, lhs: Matrix, rhs: Rect<Matrix>, start: u32) -> Matrix<'arena> {
        let end = start + rhs.matrix.row_count();
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(lhs.row_count() as usize));
        let lhs_columns = self.column_subrange(lhs, start, end);
        multiply(out, lhs_columns, rhs);
        for (out, lhs) in out.iter_mut().zip(lhs.rows) {
            *out |= &lhs
                .subrange(0, start)
                .concat(&lhs.subrange(end, lhs.map(|x| x.total_bits()).unwrap_or(end).max(end)));
        }
        Matrix { rows: out }
    }

    /// replaces matrix entries that are in row/column with the given typevar indices
    /// with a nxn identity/zero matrix corresponding to the original matrix entry,
    /// expanding the rows/columns as necessary
    pub fn replace_typevar(
        &self,
        matrix: Matrix,
        rows: &[TypeVarOccurence],
        columns: &[TypeVarOccurence],
        typevar_widths: &[u32],
    ) -> Matrix<'arena> {
        let new_rows = expanded_count(matrix.row_count(), rows, typevar_widths);
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(new_rows as usize));
        let mut out_idx = 0;
        let mut row_var_idx = 0;
        for (i, row) in matrix.rows.iter().enumerate() {
            for j in 0..if rows.get(row_var_idx).map(|x| x.matrix_idx) == Some(i as u32) {
                let n = typevar_widths[rows[row_var_idx].typevar_idx as usize];
                row_var_idx += 1;
                n
            } else {
                1
            } {
                let new_row = row.expand_typevar(columns, typevar_widths, j);
                out[out_idx] = new_row;
                out_idx += 1;
            }
        }
        Matrix { rows: out }
    }

    pub fn substitute_with_true_from(&self, matrix: Matrix, bit: u32) -> Matrix<'arena> {
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(matrix.row_count() as usize));
        for (out_row, row) in out.iter_mut().zip(matrix.rows) {
            *out_row = row.substitute_with_true_from(bit);
        }
        Matrix { rows: out }
    }

    pub fn replace_view_content(
        &self,
        matrix: MatrixView,
        replacement: Matrix,
    ) -> MatrixView<'arena> {
        let [start, end] = matrix.rows;
        let new_end = start + replacement.row_count();
        let total_len = matrix.matrix.row_count() + replacement.row_count() - (end - start);
        let out = self
            .arena
            .alloc_extend(std::iter::repeat(Row::True).take(total_len as usize));
        out[..start as usize].clone_from_slice(&matrix.matrix.rows[..start as usize]);
        out[start as usize..new_end as usize].clone_from_slice(replacement.rows);
        out[new_end as usize..].clone_from_slice(&matrix.matrix.rows[end as usize..]);
        let ret = Matrix { rows: out };
        MatrixView {
            rows: [start, new_end],
            matrix: ret,
        }
    }

    pub fn concatenate_rows<'a>(
        &self,
        matrices: impl IntoIterator<Item = Matrix<'a>>,
    ) -> Matrix<'arena> {
        Matrix {
            rows: self
                .arena
                .alloc_extend(matrices.into_iter().flat_map(|x| x.rows).cloned()),
        }
    }

    pub fn add_bound_vars(&self, matrix: Matrix, count: u32) -> Matrix<'arena> {
        Matrix {
            rows: self
                .arena
                .alloc_extend(matrix.rows.iter().map(|row| row.add_bound_vars(count))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_row_bits() {
        let mut bits = RowBits::Inner(0);
        assert!(!bits.get(0));
        bits.set(0);
        assert!(bits.get(0));
        bits.set(1);
        assert!(bits.get(1));
        assert!(!bits.get(2));
        bits.set(64);
        assert!(bits.get(64));
        assert!(!bits.get(65));
        bits.set(128);
        assert!(bits.get(128));
        assert!(!bits.get(129));
        bits.set(129);
        assert!(bits.get(129));
    }

    #[test]
    fn test_var_row() {
        let row: VarRow = "1 0 1 0 1 | 1 0 1 0 1".parse().unwrap();
        assert_eq!(row.bound_bits(), 5);
        assert_eq!(row.total_bits(), 10);
        assert!(row.get(0));
        assert!(!row.get(1));
        assert!(row.get(2));
        assert!(!row.get(3));
        assert!(row.get(4));
        assert!(row.get(5));
        assert!(!row.get(6));
        assert!(row.get(7));
        assert!(!row.get(8));
        assert!(row.get(9));
        assert_eq!(
            row.active_bits().collect::<Vec<_>>(),
            vec![0, 2, 4, 5, 7, 9]
        );
    }

    #[test]
    fn test_row() {
        let mut row: Row = "1 0 1 0 1 | 1 0 1 0 1".parse().unwrap();
        assert_eq!(row.to_string(), "1 0 1 0 1 | 1 0 1 0 1");
        assert_eq!(row.subrange(1, 6).to_string(), "0 1 0 1 | 1");
        assert_eq!(row.subrange(0, 4).to_string(), "1 0 1 0");
        assert_eq!(row.subrange(7, 10).to_string(), " | 1 0 1");
        let same_row = row
            .subrange(0, 3)
            .concat(&row.subrange(3, 6))
            .concat(&row.subrange(6, 10));
        assert_eq!(row.to_string(), same_row.to_string());
        row.set(1);
        assert_eq!(row.to_string(), "1 1 1 0 1 | 1 0 1 0 1");
        row |= &"0 0 0 1 0 | 0 0 0 1 0 0 1".parse().unwrap();
        assert_eq!(row.to_string(), "1 1 1 1 1 | 1 0 1 1 1 0 1");
        row |= &Row::True;
        assert_eq!(row.to_string(), "T");
        assert_eq!(row.subrange(0, 1).to_string(), "T");
    }

    #[test]
    fn test_matrix() {
        let arena = typed_arena::Arena::new();
        let arena = MatrixArena::new(&arena);
        let a = arena
            .new_from_str(
                "T
                 0 1 0 | 1
                 0 0 1",
            )
            .unwrap();
        let b = arena
            .new_from_str(
                "1 0 0
                 0 1 0
                 T",
            )
            .unwrap();
        assert_eq!(
            arena.multiply(a, b.rect(3, 3)).to_string(),
            "\
T
0 1 0 | 1
T
"
        );
        let a = arena
            .new_from_str(
                "1 0 1 | 0
                 0 1 0
                 1 1 0 | 0 1
                 0 0 1 | 1 0 1",
            )
            .unwrap();
        let b = arena
            .new_from_str(
                "0 1 0 0 1 | 1
                 1 0 0 0 1 | 0
                 0 0 1 0 1 | 1",
            )
            .unwrap();
        let c = arena.multiply(a, b.rect(5, 6));
        assert_eq!(
            c.to_string(),
            "\
0 1 1 0 1 | 1 0
1 0 0 0 1 | 0
1 1 0 0 1 | 1 0 1
0 0 1 0 1 | 1 1 0 1
"
        );
        let b = arena
            .new_from_str(
                "0 1 0 1 0 | 0
                 0 1 0 0 1 | 1
                 1 0 0 0 1 | 0
                 0 0 1 0 1 | 1
                 T
                 0 1 1 0 0 | 1",
            )
            .unwrap();
        let c = arena.multiply_rhs_view(a, b.view(1..4).rect(5, 6)).matrix;
        assert_eq!(
            c.to_string(),
            "\
0 1 0 1 0 | 0
0 1 1 0 1 | 1 0
1 0 0 0 1 | 0
1 1 0 0 1 | 1 0 1
0 0 1 0 1 | 1 1 0 1
T
0 1 1 0 0 | 1
"
        );
        let d = arena
            .new_from_str(
                " | 1 0 0
                  | 0 0 1",
            )
            .unwrap();
        let e = arena
            .multiply_lhs_view_column_subrange(c.view(3..5), d.rect(0, 3), 5)
            .matrix;
        assert_eq!(
            e.to_string(),
            "\
0 1 0 1 0 | 0
0 1 1 0 1 | 1 0
1 0 0 0 1 | 0
1 1 0 0 1 | 1 0 0 1
0 0 1 0 1 | 1 0 1 0 1
T
0 1 1 0 0 | 1
"
        );
    }

    #[test]
    fn partial_apply() {
        let arena = typed_arena::Arena::new();
        let arena = MatrixArena::new(&arena);
        let a = arena
            .new_from_str(
                "0 1 0 | 1 0
                 1 0 0 | 0 0 1
                 0 0 1 | 0 1 0 0",
            )
            .unwrap();
        let b = arena
            .new_from_str(
                "1 0 1
                 0 1 0",
            )
            .unwrap();
        let c = arena.partial_apply(a, b.rect(3, 3), 3);
        assert_eq!(
            c.to_string(),
            "\
1 1 1
1 0 0 | 1
0 1 1 | 0 0
"
        );
    }

    #[test]
    fn test_replace_typevar() {
        let arena = typed_arena::Arena::new();
        let arena = MatrixArena::new(&arena);
        let a = arena
            .new_from_str(
                "1 0 0 | 0
                 0 0 1
                 0 1 0 | 1 0
                 0 0 0 | 0 1 1
                 T",
            )
            .unwrap();
        let rows = vec![
            TypeVarOccurence {
                matrix_idx: 0,
                typevar_idx: 0,
            },
            TypeVarOccurence {
                matrix_idx: 2,
                typevar_idx: 1,
            },
            TypeVarOccurence {
                matrix_idx: 4,
                typevar_idx: 0,
            },
        ];
        let columns = vec![
            TypeVarOccurence {
                matrix_idx: 0,
                typevar_idx: 0,
            },
            TypeVarOccurence {
                matrix_idx: 1,
                typevar_idx: 1,
            },
            TypeVarOccurence {
                matrix_idx: 3,
                typevar_idx: 1,
            },
        ];
        let typevar_widths = vec![2, 3];
        let b = arena.replace_typevar(a, &rows, &columns, &typevar_widths);
        assert_eq!(
            b.to_string(),
            "\
1 0 0 0 0 0 | 0 0 0
0 1 0 0 0 0 | 0 0 0
0 0 0 0 0 1
0 0 1 0 0 0 | 1 0 0 0
0 0 0 1 0 0 | 0 1 0 0
0 0 0 0 1 0 | 0 0 1 0
0 0 0 0 0 0 | 0 0 0 1 1
T
T
"
        );
    }

    #[test]
    fn test_substitute_with_true_from() {
        let arena = typed_arena::Arena::new();
        let arena = MatrixArena::new(&arena);
        let a = arena.identity(5);
        assert_eq!(
            a.to_string(),
            " | 1 0 0 0 0
 | 0 1 0 0 0
 | 0 0 1 0 0
 | 0 0 0 1 0
 | 0 0 0 0 1
"
        );
        let b = arena.substitute_with_true_from(a, 2);
        assert_eq!(
            b.to_string(),
            " | 1 0 0 0 0
 | 0 1 0 0 0
T
T
T
"
        );
        let c = arena.substitute_with_true_from(a, 0);
        assert_eq!(c.to_string(), "T\nT\nT\nT\nT\n");
        let d = arena.substitute_with_true_from(a, 128);
        assert_eq!(d.to_string(), a.to_string());
        let a = arena.identity(128);
        let b = arena.substitute_with_true_from(a, 63);
        assert_eq!(b.rows[63].to_string(), "T");
    }

    #[test]
    fn test_replace_view_content() {
        let arena = typed_arena::Arena::new();
        let arena = MatrixArena::new(&arena);
        let a = arena
            .new_from_str(
                "1 0 0 | 0
                 0 0 1
                 0 1 0 | 1 0
                 0 0 0 | 0 1 1
                 T",
            )
            .unwrap();
        let b = arena
            .new_from_str(
                "1 0 0
                 0 1 0
                 0 0 1
                 T",
            )
            .unwrap();
        let c = arena.replace_view_content(a.view(1..3), b);
        assert_eq!(
            c.matrix.to_string(),
            "\
1 0 0 | 0
1 0 0
0 1 0
0 0 1
T
0 0 0 | 0 1 1
T
"
        );
        assert_eq!(c.rows, [1, 5]);
    }
}

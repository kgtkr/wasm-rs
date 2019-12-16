pub fn pop_n<T>(vec: &mut Vec<T>, n: usize) -> Vec<T> {
    vec.split_off(vec.len() - n)
}

#[test]
fn test_pop_n() {
    assert_eq!(pop_n::<i32>(&mut vec![], 0), vec![] as Vec<i32>);
    assert_eq!(pop_n::<i32>(&mut vec![1, 2, 3, 4, 5], 2), vec![4, 5]);
}

pub enum Sign {
    Positive,
    Negative,
}

pub fn sign_f32(x: f32) -> Option<Sign> {
    if x.is_sign_positive() {
        Some(Sign::Positive)
    } else if x.is_sign_negative() {
        Some(Sign::Negative)
    } else {
        None
    }
}

pub fn sign_f64(x: f64) -> Option<Sign> {
    if x.is_sign_positive() {
        Some(Sign::Positive)
    } else if x.is_sign_negative() {
        Some(Sign::Negative)
    } else {
        None
    }
}

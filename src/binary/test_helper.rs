use super::{Decoder, Encoder};
use proptest::prelude::*;

pub fn identity_encode_decode<T: Arbitrary + Encoder + Decoder + PartialEq>() {
    proptest!(|(x: T)| {
        assert_eq!(Decoder::decode_to_end(&x.encode_to_vec()), Ok(x));
    });
}

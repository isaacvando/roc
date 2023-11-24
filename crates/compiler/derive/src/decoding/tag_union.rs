use roc_can::expr::Expr;

use crate::util::Env;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

// We want to generate an implementation like this;

// decoder : Decoder [Zero, One a, Two b c] fmt where a implements Decoding, b implements Decoding, c implements Decoding, fmt implements DecoderFormatting
// decoder =
//     nameDecoder =
//         ["Zero", "One", "Two"]
//         |> List.map Str.toUtf8
//         |> discriminant

//     Decode.custom \bytes, fmt ->
//         { result: nameResult, rest: bytesAfterName } = Decode.decodeWith bytes nameDecoder fmt
//         when nameResult is
//             Err _ -> { result: Err TooShort, rest: bytesAfterName }
//             Ok index ->
//                 when index is
//                     0 ->
//                         state = {}
//                         stepper = \_, _ -> TooLong
//                         finalizer = \_ -> Ok Zero

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepper finalizer) fmt

//                     1 ->
//                         state = { e0: Err TooShort }
//                         stepper = \s, i ->
//                             when i is
//                                 0 ->
//                                     Next
//                                         (
//                                             Decode.custom \b, f ->
//                                                 when Decode.decodeWith b Decode.decoder f is
//                                                     { result, rest } ->
//                                                         { result: Result.map result \val -> { s & e0: Ok val }, rest }
//                                         )

//                                 _ -> TooLong
//                         finalizer = \s ->
//                             when s is
//                                 { e0: Ok val } -> Ok (One val)
//                                 _ -> Err TooShort

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepper finalizer) fmt

//                     2 ->
//                         state = { e0: Err TooShort, e1: Err TooShort }
//                         stepper = \s, i ->
//                             when i is
//                                 0 ->
//                                     Next
//                                         (
//                                             Decode.custom \b, f ->
//                                                 when Decode.decodeWith b Decode.decoder f is
//                                                     { result, rest } ->
//                                                         { result: Result.map result \val -> { s & e0: Ok val }, rest }
//                                         )

//                                 1 ->
//                                     Next
//                                         (
//                                             Decode.custom \b, f ->
//                                                 when Decode.decodeWith b Decode.decoder f is
//                                                     { result, rest } ->
//                                                         { result: Result.map result \val -> { s & e1: Ok val }, rest }
//                                         )

//                                 _ -> TooLong
//                         finalizer = \s ->
//                             when s is
//                                 { e0: Ok v0, e1: Ok v1 } -> Ok (Two v0 v1)
//                                 _ -> Err TooShort

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepper finalizer) fmt

//                     _ -> { result: Err TooShort, rest: bytesAfterName }

pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    tags: Vec<(TagName, u16)>,
) -> (Expr, Variable) {
    panic!("decoding tag unions not yet implemented");
}

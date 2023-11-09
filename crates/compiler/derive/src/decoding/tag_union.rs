use crate::util::Env;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;

use roc_can::expr::Expr;
use roc_types::subs::Variable;

// We want to generate an implementation like this:

// decoder : Decoder [Zero, One a, Two b c] fmt where a implements Decoding, b implements Decoding, c implements Decoding, fmt implements DecoderFormatting
// decoder =
//     discDecoder =
//         ["Zero", "One", "Two"]
//         |> List.map Str.toUtf8
//         |> discriminant

//     Decode.custom \bytes, fmt ->
//         discResult = Decode.decodeWith bytes discDecoder fmt
//         when discResult.result is
//             Err _ -> { result: Err TooShort, rest: discResult.rest }
//             Ok index ->
//                 when index is
//                     0 -> { result: Ok Zero, rest: discResult.rest }
//                     1 ->
//                         t1 = Decode.decodeWith discResult.rest Decode.decoder fmt
//                         r =
//                             when t1.result is
//                                 Err _ -> Err TooShort
//                                 Ok val -> Ok (One val)
//                         { result: r, rest: t1.rest }

//                     2 ->
//                         t2 = Decode.decodeWith discResult.rest Decode.decoder fmt
//                         r =
//                             when t2.result is
//                                 Err _ -> Err TooShort
//                                 Ok (val1, val2) -> Ok (Two val1 val2)
//                         { result: r, rest: t2.rest }

//                     _ -> { result: Err TooShort, rest: discResult.rest } # This error should be something different

pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    tags: Vec<(TagName, u16)>,
) -> (Expr, Variable) {
    panic!("not implemented yet");
}

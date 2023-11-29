use roc_can::expr::Expr;

use crate::util::Env;
use roc_module::called_via::CalledVia;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
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
//                         stepElem = \_, _ -> TooLong
//                         finalizer = \_ -> Ok Zero

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepElem finalizer) fmt

//                     1 ->
//                         state = { e0: Err TooShort }
//                         stepElem = \s, i ->
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

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepElem finalizer) fmt

//                     2 ->
//                         state = { e0: Err TooShort, e1: Err TooShort }
//                         stepElem = \s, i ->
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

//                         Decode.decodeWith bytesAfterName (Decode.tuple state stepElem finalizer) fmt

//                     _ -> { result: Err TooShort, rest: bytesAfterName }

pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    tags: Vec<(TagName, u16)>,
) -> (Expr, Variable) {
    name_decoder(env, tags)
}

//     nameDecoder =
//         ["Zero", "One", "Two"]
//         |> List.map Str.toUtf8
//         |> discriminant
fn name_decoder(env: &mut Env, tags: Vec<(TagName, u16)>) -> (Expr, Variable) {
    let list_of_variants_body = tags
        .into_iter()
        .map(|(name, _)| {
            let boxed_str = String::from(name.as_ident_str().as_str()).into_boxed_str();
            Loc::at_zero(Expr::Str(boxed_str))
        })
        .collect();

    let list_of_variants = Expr::List {
        elem_var: Variable::STR,
        loc_elems: list_of_variants_body,
    };

    let list_map_fn = Box::new((
        env.subs.fresh_unnamed_flex_var(),
        Loc::at_zero(Expr::Var(
            Symbol::LIST_MAP,
            env.subs.fresh_unnamed_flex_var(),
        )),
        env.subs.fresh_unnamed_flex_var(),
        env.subs.fresh_unnamed_flex_var(),
    ));

    let list_map_to_utf8_call = Expr::Call(
        list_map_fn,
        vec![
            (
                env.subs.fresh_unnamed_flex_var(),
                Loc::at_zero(Expr::Var(
                    Symbol::STR_TO_UTF8,
                    env.subs.fresh_unnamed_flex_var(),
                )),
            ),
            (
                env.subs.fresh_unnamed_flex_var(),
                Loc::at_zero(list_of_variants),
            ),
        ],
        CalledVia::Space,
    );

    let discriminant_fn = Box::new((
        env.subs.fresh_unnamed_flex_var(),
        Loc::at_zero(Expr::Var(
            Symbol::DECODE_DISCRIMINANT,
            env.subs.fresh_unnamed_flex_var(),
        )),
        env.subs.fresh_unnamed_flex_var(),
        env.subs.fresh_unnamed_flex_var(),
    ));

    let discriminant_call = Expr::Call(
        discriminant_fn,
        vec![(
            env.subs.fresh_unnamed_flex_var(),
            Loc::at_zero(list_map_to_utf8_call),
        )],
        CalledVia::Space,
    );

    (discriminant_call, env.subs.fresh_unnamed_flex_var())
}

// (
//     Expr::Crash {
//         msg: Box::new(Loc::at_zero(Expr::Str("placeholder".into()))),
//         ret_var: env.subs.fresh_unnamed_flex_var(),
//     },
//     env.subs.fresh_unnamed_flex_var(),
// )

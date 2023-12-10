use std::collections::HashMap;

use roc_can::def::Def;
use roc_can::expr::{
    AnnotatedMark, ClosureData, Expr, Field, Recursive, WhenBranch, WhenBranchPattern,
};
use roc_can::pattern::{DestructType, Pattern, RecordDestruct};
use roc_collections::SendMap;
use roc_types::types::RecordField;

use crate::synth_var;
use crate::util::{Env, ExtensionKind};
use roc_module::called_via::CalledVia;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::Loc;
use roc_region::all::Region;
use roc_types::subs::{
    Content, ExhaustiveMark, FlatType, RecordFields, RedundantMark, TagExt, UnionTags, Variable,
};

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
    let var1 = env.subs.fresh_unnamed_flex_var();
    let var2 = env.subs.fresh_unnamed_flex_var();
    let finalizer_var = env.subs.fresh_unnamed_flex_var();

    // finalizer(env, "Foo".into(), vec![var1, var2], finalizer_var)
    // name_decoder(env, tags, var1)
    let mut index_vars = Vec::with_capacity(3 as _);
    let mut state_fields = Vec::with_capacity(3 as _);
    let mut state_field_vars = Vec::with_capacity(3 as _);
    initial_state(
        env,
        3,
        &mut index_vars,
        &mut state_fields,
        &mut state_field_vars,
    );
}

// TODO: this could be discriminant [['O','n','e'],...] instead. Not sure how to construct the list though
// discriminant (List.map Str.toUtf8 ["One", "Two", "Three"])
fn name_decoder(
    env: &mut Env,
    tags: Vec<(TagName, u16)>,
    name_decoder_var: Variable,
) -> (Expr, Variable) {
    let list_of_variants_body = tags
        .into_iter()
        .map(|(name, _)| {
            let boxed_str = String::from(name.as_ident_str().as_str()).into_boxed_str();
            Loc::at_zero(Expr::Str(boxed_str))
        })
        .collect();

    let list_of_variants_var = env.subs.fresh_unnamed_flex_var();
    let list_of_variants = Expr::List {
        elem_var: Variable::STR,
        loc_elems: list_of_variants_body,
    };

    let list_map_return_var = env.subs.fresh_unnamed_flex_var();
    let list_map_closure_var = env.subs.fresh_unnamed_flex_var();
    let list_map_function_var = env.subs.fresh_unnamed_flex_var();
    let list_map_fn = Box::new((
        list_map_function_var,
        Loc::at_zero(Expr::Var(Symbol::LIST_MAP, list_map_function_var)),
        list_map_closure_var,
        list_map_return_var,
    ));

    let str_to_utf8_var = env.subs.fresh_unnamed_flex_var();
    let list_map_to_utf8_call = Expr::Call(
        list_map_fn,
        vec![
            (
                str_to_utf8_var,
                Loc::at_zero(Expr::Var(Symbol::STR_TO_UTF8, str_to_utf8_var)),
            ),
            (list_of_variants_var, Loc::at_zero(list_of_variants)),
        ],
        CalledVia::Space,
    );

    let discriminant_function_var = env.subs.fresh_unnamed_flex_var();
    let discriminant_closure_var = env.subs.fresh_unnamed_flex_var();
    let discriminant_return_var = env.subs.fresh_unnamed_flex_var();
    let discriminant_fn = Box::new((
        discriminant_function_var,
        Loc::at_zero(Expr::Var(
            Symbol::DECODE_DISCRIMINANT,
            discriminant_function_var,
        )),
        discriminant_closure_var,
        discriminant_return_var,
    ));

    let discriminant_call = Expr::Call(
        discriminant_fn,
        vec![(list_map_return_var, Loc::at_zero(list_map_to_utf8_call))],
        CalledVia::Space,
    );

    (discriminant_call, name_decoder_var)
}

// (
//     Expr::Crash {
//         msg: Box::new(Loc::at_zero(Expr::Str("placeholder".into()))),
//         ret_var: env.subs.fresh_unnamed_flex_var(),
//     },
//     env.subs.fresh_unnamed_flex_var(),
// )

// finalizer = \s ->
//     when s is
//         { e0: Ok v0, e1: Ok v1 } -> Ok (Two v0 v1)
//         _ -> Err TooShort
fn finalizer(
    env: &mut Env,
    tag_name: TagName,
    payload_vars: Vec<Variable>,
    finalizer_var: Variable,
) -> (Expr, Variable) {
    let when_var = env.subs.fresh_unnamed_flex_var();
    let s_arg_symbol = env.new_symbol("s");
    let s_arg_variable = env.subs.fresh_unnamed_flex_var();

    let arity = payload_vars.len();

    let mut symbols: Vec<Symbol> = Vec::with_capacity(arity);
    for index in 0..arity {
        symbols.push(env.new_symbol(format!("v{}", index)))
    }

    let mut tag_payload = Vec::with_capacity(arity);
    for index in 0..arity {
        let variable = *payload_vars.get(index).unwrap();
        let expr = Expr::Var(*symbols.get(index).unwrap(), variable);
        tag_payload.push((variable, Loc::at_zero(expr)));
    }

    let tag_var = env.subs.fresh_unnamed_flex_var();
    let tag = Expr::Tag {
        tag_union_var: tag_var,
        ext_var: env.new_ext_var(ExtensionKind::TagUnion),
        name: tag_name,
        arguments: tag_payload,
    };

    let ok_branch_value = Expr::Tag {
        tag_union_var: when_var,
        ext_var: env.new_ext_var(ExtensionKind::TagUnion),
        name: "Ok".into(),
        arguments: vec![(tag_var, Loc::at_zero(tag))],
    };

    let mut record_pattern_destructs = Vec::with_capacity(arity);
    for index in 0..arity {
        let label = format!("e{}", index);
        let pattern_var = env.subs.fresh_unnamed_flex_var();
        let destruct_type = DestructType::Guard(
            pattern_var,
            Loc::at_zero(Pattern::AppliedTag {
                whole_var: env.subs.fresh_unnamed_flex_var(),
                ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                tag_name: "Ok".into(),
                arguments: vec![(
                    env.subs.fresh_unnamed_flex_var(),
                    Loc::at_zero(Pattern::Identifier(*symbols.get(index).unwrap())),
                )],
            }),
        );
        let destruct = Loc::at_zero(RecordDestruct {
            var: env.subs.fresh_unnamed_flex_var(),
            label: label.clone().into(),
            symbol: env.new_symbol(label),
            typ: destruct_type,
        });

        record_pattern_destructs.push(destruct);
    }

    let branches = vec![
        WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::RecordDestructure {
                    whole_var: s_arg_variable,
                    ext_var: env.new_ext_var(ExtensionKind::Record),
                    destructs: record_pattern_destructs,
                }),
                degenerate: false,
            }],
            value: Loc::at_zero(ok_branch_value),
            guard: None,
            redundant: RedundantMark::known_non_redundant(),
        },
        WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: Loc::at_zero(Pattern::Underscore),
                degenerate: false,
            }],
            value: Loc::at_zero(Expr::Tag {
                tag_union_var: when_var,
                ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                name: "Err".into(),
                arguments: vec![{
                    let too_short_var = env.subs.fresh_unnamed_flex_var();
                    (
                        too_short_var,
                        Loc::at_zero(Expr::Tag {
                            tag_union_var: too_short_var,
                            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
                            name: "TooShort".into(),
                            arguments: vec![],
                        }),
                    )
                }],
            }),
            guard: None,
            redundant: RedundantMark::known_non_redundant(),
        },
    ];

    let when = Expr::When {
        loc_cond: Box::new(Loc::at_zero(Expr::Var(s_arg_symbol, s_arg_variable))),
        cond_var: s_arg_variable,
        expr_var: when_var,
        region: Region::zero(),
        branches,
        branches_cond_var: Variable::NAT,
        exhaustive: ExhaustiveMark::known_exhaustive(),
    };

    // TODO: it looks like this needs to be a lambda set var
    let closure_type = env.subs.fresh_unnamed_flex_var();
    let finalizer_symbol = env.new_symbol("finalizer");

    let closure = Expr::Closure(ClosureData {
        function_type: finalizer_var,
        closure_type: closure_type,
        return_type: when_var,
        name: finalizer_symbol,
        captured_symbols: vec![],
        recursive: Recursive::NotRecursive,
        arguments: vec![(
            s_arg_variable,
            AnnotatedMark::known_exhaustive(),
            Loc::at_zero(Pattern::Identifier(s_arg_symbol)),
        )],
        loc_body: Box::new(Loc::at_zero(when)),
    });

    (closure, finalizer_var)
}

// state = { e0: Err TooShort, e1: Err TooShort }
fn initial_state(
    env: &mut Env<'_>,
    arity: u32,
    index_vars: &mut Vec<Variable>,
    state_fields: &mut Vec<Lowercase>,
    state_field_vars: &mut Vec<Variable>,
) -> (Expr, Variable) {
    let mut initial_state_fields = SendMap::default();

    for i in 0..arity {
        let subs = &mut env.subs;
        let index_var = subs.fresh_unnamed_flex_var();

        index_vars.push(index_var);

        let state_field = Lowercase::from(format!("e{i}"));
        state_fields.push(state_field.clone());

        let no_index_label = "TooShort";
        let union_tags = UnionTags::tag_without_arguments(subs, no_index_label.into());
        let no_index_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let no_index = Expr::Tag {
            tag_union_var: no_index_var,
            ext_var: Variable::EMPTY_TAG_UNION,
            name: no_index_label.into(),
            arguments: Vec::new(),
        };
        let err_label = "Err";
        let union_tags = UnionTags::for_result(subs, index_var, no_index_var);
        let result_var = synth_var(
            subs,
            Content::Structure(FlatType::TagUnion(
                union_tags,
                TagExt::Any(Variable::EMPTY_TAG_UNION),
            )),
        );
        let index_expr = Expr::Tag {
            tag_union_var: result_var,
            ext_var: env.new_ext_var(ExtensionKind::TagUnion),
            name: err_label.into(),
            arguments: vec![(no_index_var, Loc::at_zero(no_index))],
        };
        state_field_vars.push(result_var);
        let index = Field {
            var: result_var,
            region: Region::zero(),
            loc_expr: Box::new(Loc::at_zero(index_expr)),
        };

        initial_state_fields.insert(state_field, index);
    }

    let subs = &mut env.subs;
    let record_index_iter = state_fields
        .iter()
        .zip(state_field_vars.iter())
        .map(|(index_name, &var)| (index_name.clone(), RecordField::Required(var)));
    let flat_type = FlatType::Record(
        RecordFields::insert_into_subs(subs, record_index_iter),
        Variable::EMPTY_RECORD,
    );

    let state_record_var = synth_var(subs, Content::Structure(flat_type));

    (
        Expr::Record {
            record_var: state_record_var,
            fields: initial_state_fields,
        },
        state_record_var,
    )
}

use crate::util::Env;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;

use roc_can::expr::Expr;
use roc_types::subs::Variable;

pub(crate) fn decoder(
    env: &mut Env,
    _def_symbol: Symbol,
    tags: Vec<(TagName, u16)>,
) -> (Expr, Variable) {
    panic!("not implemented yet");
}

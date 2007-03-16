val optimising : bool Settings.setting
val inline : Syntax.expression list -> Syntax.expression list
val uniquify_expression : Syntax.RewriteSyntax.rewriter
val optimise_program : (Inferencetypes.environment * Inferencetypes.alias_environment) * Syntax.expression list -> Syntax.expression list

val pure : Syntax.expression -> bool
val sql_aslist : Syntax.RewriteSyntax.rewriter
val lift_lets : Syntax.RewriteSyntax.rewriter
val unused_variables : Syntax.RewriteSyntax.rewriter
val renaming : Syntax.RewriteSyntax.rewriter

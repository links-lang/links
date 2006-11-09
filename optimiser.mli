val optimising : bool Settings.setting
val inline : Syntax.expression list -> Syntax.expression list
val uniquify_expression : Syntax.RewriteSyntax.rewriter
val optimise_program : (Types.environment * Types.alias_environment) * Syntax.expression list -> Syntax.expression list

(in-package :cmp)

(export '(%fn-prototype%
	  *load-time-value-holder-global-var*
	  irc-intrinsic
	  irc-branch-to-and-begin-block
	  +fn-prototype-argument-names+
	  jit-function-name
	  *the-module*
	  *cleavir-compile-hook*
	  *cleavir-compile-file-hook*
	  *llvm-context*
	  with-irbuilder
	  *irbuilder*
	  irc-basic-block-create
	  jit-constant-i32
	  irc-ret-void
	  codegen-literal
	  irc-intrinsic
	  compile-error-if-wrong-number-of-arguments
	  *current-function*
	  *gv-current-function-name*
	  *current-function-name*
	  module-make-global-string
	  *run-time-literal-holder*
	  cmp-log
	  cmp-log-dump-module
	  cmp-log-dump-function
	  irc-environment-activation-frame
	  *run-time-values-table-name*
	  irc-switch
	  %t*%
	  %i1%
	  %size_t%
	  %i32%
	  codegen-rts/symbol
	  irc-begin-block
	  irc-basic-block-create
	  typeid-core-dynamic-go
	  make-calling-convention
	  calling-convention-nargs
	  calling-convention-register-args
	  calling-convention-args
	  calling-convention-copy-args
	  calling-convention-args.gep
	  compile-error-if-not-enough-arguments
	  irc-t
	  ))

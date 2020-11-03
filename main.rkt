#lang racket/base

(require racket/string)
(require racket/set)

(require syntax/strip-context)
(require readline/readline)
(require ffi/unsafe)
(require ffi/unsafe/define)

(require lua/comb/parser)
(require lua/compiler)
(require racket/pretty)

(require lua/locals)

(define intern (ffi-lib #f))
(define (getbuf)
    (get-ffi-obj "rl_line_buffer" intern _string))

(define ns (make-base-namespace))
(namespace-attach-module (current-namespace) 'lua/locals ns)
(define (eval-lua stx)
    (parameterize ((current-namespace ns))
        (namespace-require 'lua/locals) 
        (eval stx ns)))

(set-completion-function!
    (lambda (name)
        (define namestr (symbol->string name))
        (define opts (parse-complete (getbuf) _G))
        (filter (lambda (x) (string-prefix? x namestr))
            (set->list (list->set opts))))
    _symbol)

(define (repl)
    (define src (readline ">>> "))
    (add-history src)
    (define ast (parse-stmt-or-expr src))
    (define stx (compile ast))
    (pretty-print (syntax->datum stx))
    (define result (eval-lua stx))
    (cond
        ((not (void? result)) 
            (displayln (builtin-tostring result))))
    (repl))

(repl)

(provide (all-defined-out))

#lang racket/base

(require racket/port)
(require racket/format)
(require racket/pretty)

(require lua/comb/parser)
(require lua/compiler)

(require syntax/strip-context)

(provide
    (rename-out
        (lang-read-lit read-syntax)))

(define (lang-read-lit src in)
    (strip-context #`(module in racket/base
        (require lua/locals)

        (define has-return #f)
        (define return-value 'nil)

        (void #,(letrec
            ((ast (parse-text (port->string in)))
            (stx (compile ast)))
            stx))
        (void))))


#lang racket/base

(require racket/port)

(require lua/comb/parser)
(require lua/compiler)

(require syntax/strip-context)

(provide
    (rename-out
        (lang-read-lit read-syntax)))


(define (lang-read-lit src in)
    ;;; (strip-context #`(module in racket/base
    (strip-bindings #`(module in racket/base
        (require "../locals.rkt")

        (define has-return #f)
        (define return-value 'nil)

        (void #,(letrec
            ((ast (parse-text (port->string in)))
            (stx (compile ast)))
            stx))
        (void))))


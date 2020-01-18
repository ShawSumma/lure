#lang racket/base

(require racket/port)

(require "./comb/parser.rkt")
(require "./compiler.rkt")

(provide
    (rename-out
        (lang-read read)
        (lang-read-lit read-syntax)))

(require syntax/strip-context)

(define (lang-read in)
    (println in))

(define (lang-read-lit src in)
    (strip-context #`(module in racket/base
        (require "../locals.rkt")

        (define has-return #f)
        (define return-value 'nil)

        (void #,(letrec
            ((ast (parse-text (port->string in)))
            (stx (compile ast)))
            stx))
        (void))))


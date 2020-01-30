#lang racket/base

(require racket/port)
(require racket/format)

(require lua/comb/parser)
(require lua/compiler)
(require lua/locals)

(require syntax/strip-context)

(provide
    (rename-out
        (lang-read-lit read-syntax)))

(define (lang-read-lit src in)
    (strip-context #`(module in racket/base
        (require lua/locals)
        (void #,(letrec
            ((parsed (parse-text (port->string in)))
            (stx (compile parsed)))
            stx))
        (void))))
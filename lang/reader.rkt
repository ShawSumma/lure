#lang racket/base

(require racket/port)
(require racket/format)
(require racket/pretty)

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
        (provide return)
        (define return #,(let
            ((parsed (parse-text (port->string in))))
            (let ((stx (compile parsed)))
                ;;; (pretty-print (syntax->datum stx))
                stx)))
        (void))))
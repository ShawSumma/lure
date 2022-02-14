#lang racket/base

(require racket/port)
(require racket/pretty)

(require "../comb/parser.rkt")
(require "../compiler.rkt")
(require "../locals.rkt")

(require syntax/strip-context)

(provide
    (rename-out
        (lang-read-lit read-syntax)))

(define generate-tempfiles #t)

(define (write-syntax stx from)
    (define filename (string-append (path->string from) ".tmp"))
    (cond
        ((file-exists? filename)
            (delete-file filename)))
    (call-with-output-file filename
        (lambda (file)
            (displayln "#lang racket/base" file)
            (pretty-write
                (list 'require 'lua/locals)
                file)
            (pretty-write
                (list 'provide 'return)
                file)
            (pretty-write
                (list 'define '_ENV '_G)
                file)
            (pretty-write
                (syntax->datum
                    #`(hash-set! _G "require"
                        (hash-ref
                            (hash-ref _G "racket" (void))
                            "tmplib" (void)))) file)
            (pretty-write
                (syntax->datum
                    #`(define return #,stx))
                file))))

(define (lang-read-lit src in)
    (strip-context #`(module in racket/base
        (require lua/locals)
        (provide return)
        (define _ENV _G)
        (define return #,(let
            ((parsed (parse-text (port->string in))))
            (let ((stx (compile parsed)))
                ;;; (pretty-write (syntax->datum stx))
                (cond
                    (generate-tempfiles
                        (write-syntax stx src)))
                stx)))
        (void))))

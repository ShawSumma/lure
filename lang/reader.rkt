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

(define generate-tempfiles #f)

(define (write-syntax stx from)
    (define filename (string-append (path->string from) ".tmp"))
    (cond
        ((file-exists? filename)
            (delete-file filename)))
    (call-with-output-file filename
        (lambda (file)
            (displayln "#lang racket/base" file)
            (displayln "(require lua/locals)" file)
            (displayln "(provide return)" file)
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
        (define return #,(let
            ((parsed (parse-text (port->string in))))
            (let ((stx (compile parsed)))
                (cond
                    (generate-tempfiles
                        (write-syntax stx src)))
                stx)))
        (void))))
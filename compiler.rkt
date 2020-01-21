#lang racket/base

(require racket/set)
(require racket/list)

(define operator-binary-map
    (hash
        ".." 'builtin-concat
        "+" 'builtin-add
        "*" 'builtin-mul
        "-" 'builtin-sub
        "/" 'builtin-div
        "%" 'builtin-mod
        "^" 'builtin-pow
        "==" 'boolean-eq
        "~=" 'boolean-neq
        "<=" '<=
        ">=" '>=
        ">" '>
        "<" '<
        "or" 'boolean-or
        "and" 'boolean-and
        "_" '_))

(define operator-unary-map
    (hash
        "not" 'boolean-not
        "+" '+
        "-" '-
        "_" '_))

(define (compile-operator-binary expr)
    (hash-ref operator-binary-map expr))

(define (compile-operator-unary expr)
    (hash-ref operator-unary-map expr))

(define (compile-string expr)
    #`#,(cadr expr))

(define (compile-number expr)
    #`#,(string->number (cadr expr)))

(define (compile-raw-name expr)
    (string->symbol (cadr expr)))

(define (compile-name expr)
    (define str (cadr expr))
    (cond
        ((equal? str "nil") #`nil)
        ((equal? str "true") #t)
        ((equal? str "false") #f)
        (#t (string->symbol (string-append "lua-" str)))))

(define (ccall expr type)
    (define rev (reverse (cddr expr)))
    (if (or (empty? rev) (not (equal? (caar rev) 'call)))
        #`(#,type #,(compile (cadr expr))
            #,@(map compile (cddr expr)))
        #`(apply #,type #,(compile (cadr expr))
            #,@(map compile (reverse (cdr rev)))
            #,(ccall (car rev) 'call))))

(define (compile-call expr)
    (ccall expr 'ecall))

(define (compile-self-call expr)
    #`(letrec
        ((self #,(compile (cadr expr)))
        (self-func (hash-ref self #,(compile (caddr expr)) nil)))
        (call self-func self #,@(map compile (cdddr expr)))))

(define (find-locals expr)
    (map (lambda (locv) (compile (cadr locv)))
        (filter
            (lambda (sub) (equal? 'local (car sub)))
            (cdr expr))))

(define (compile-block expr)
    (define names (set->list (list->set (find-locals expr))))
    #`((lambda ()
        #,@(map
            (lambda (name)
                #`(define
                    #,name
                    nil))
            names)
        #,@(map
            (lambda (obj)
                #`(if has-return return-value #,(compile obj)))
            (cdr expr))
        nil)))

(define compile-program compile-block)

(define (compile-op-binary expr)
    (if (empty? (cddr expr))
        (compile (cadr expr))
        #`(#,(compile-operator-binary (cadr (caddr expr)))
            #,(compile (cadr expr))
            #,(compile (cons 'op-binary (cdddr expr))))))

(define (compile-op-unary expr)
    #`(#,(compile-operator-unary (cadr expr))
        #,(compile (caddr expr))))

(define (compile-local expr)
    #`(set! #,(compile (cadr expr))
        #,(compile (caddr expr))))

(define (compile-lambda expr)
    #`(lambda (#,@(map compile (cadr expr)))
        (define has-return #f)
        (define return-value (list nil))
        #,(compile (caddr expr))
        return-value))

(define (compile-if expr)
    #`(if #,(compile (cadr expr))
        #,(compile (caddr expr))
        #,(compile (cadddr expr))))

(define (compile-while expr)
    #`((lambda ()
        (define cexpr (lambda () #,(compile (cadr expr))))
        (define (while)
            (define isbroke (equal? has-return 'while))
            (if (and (cexpr) (not has-return))
                (begin
                    #,(compile (caddr expr))
                    (while))
                (if isbroke
                    (set! return-value #f)
                    nil)))
        (while))))

(define (compile-for expr)
    #`(for ((#,(compile (cadr expr))
        (in-range
            #,(compile (caddr expr))
            (+ #,(compile (cadddr expr)) 1)
            #,(compile (cadr (cdddr expr))))))
        #,(compile (caddr (cdddr expr)))))

(define (compile-return expr)
    #`(begin
        (set! has-return #t)
        (set! return-value
            (list #,@(map compile (cdr expr))))))

(define (compile-global expr)
    #`(hash-ref _G #,(cadr expr) nil))

(define (compile-table expr)
    (define local-count 0)
    #`(let ((table (make-hasheq)))
        #,@(map
            (lambda (item)
                (if (equal? (car item) 'list-part)
                    (begin
                        (set! local-count (+ local-count 1))
                        #`(hash-set! table #,local-count #,(compile (cadr item))))
                    #`(hash-set! table #,(compile (cadr item)) #,(compile (caddr item)))))
            (cdr expr))
        table))

(define (compile-index expr)
    #`(hash-ref #,(compile (cadr expr)) #,(compile (caddr expr)) nil))

(define (compile-set expr)
    (if (equal? (caadr expr) 'global)
        #`(hash-set! _G #,(cadadr expr)
            #,(compile (caddr expr)))
        #`(set! #,(compile (cadr expr))
            #,(compile (caddr expr)))))

(define (compile-set-index expr)
    #`(hash-set! #,(compile (cadr expr))
        #,(compile (caddr expr))
        #,(compile (cadddr expr))))



(define (compile expr)
    (define type (car expr))
    (define ret (cond
        ((equal? 'string type) (compile-string expr))
        ((equal? 'number type) (compile-number expr))
        ((equal? 'name type) (compile-name expr))
        ((equal? 'raw-name type) (compile-raw-name expr))
        ((equal? 'global type) (compile-global expr))
        ((equal? 'lambda type) (compile-lambda expr))
        ((equal? 'if type) (compile-if expr))
        ((equal? 'while type) (compile-while expr))
        ((equal? 'for type) (compile-for expr))
        ((equal? 'table type) (compile-table expr))
        ((equal? 'index type) (compile-index expr))
        ((equal? 'op-unary type) (compile-op-unary expr))
        ((equal? 'op-binary type) (compile-op-binary expr))
        ((equal? 'set type) (compile-set expr))
        ((equal? 'set-index type) (compile-set-index expr))
        ((equal? 'call type) (compile-call expr))
        ((equal? 'self-call type) (compile-self-call expr))
        ((equal? 'local type) (compile-local expr))
        ((equal? 'program type) (compile-program expr))
        ((equal? 'block type) (compile-block expr))
        ((equal? 'return type) (compile-return expr))
        (#t (error "internal error: ast"))))
    ret)

(provide (all-defined-out))

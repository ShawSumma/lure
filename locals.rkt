#lang racket/base

(require racket/list)
(require syntax/parse/define)

(define _G (make-hash))

(define nil 'nil)

(define (to-boolean v)
    (and v (not (equal? v 'nil))))

(define-simple-macro (boolean-or a b)
    (let
        ((av a)
        (bv b))
        (if (to-boolean av) av (if (to-boolean bv) bv #f))))

(define-simple-macro (boolean-not a)
    (not (to-boolean a)))

(define-simple-macro (boolean-and a b)
    (let
        ((av a)
        (bv b))
        (if (to-boolean av) (if (to-boolean bv) bv #f) #f)))

(define (call func . args)
    (apply func args))

(define (ecall func . args)
    (define ret (apply call func args))
    (cond
        ((not (list? ret)) ret)
        ((empty? ret) nil)
        (else (car ret))))

(define (binary-arith name func)
    (lambda (a b)
        (if (hash? a)
            (let
                ((amet (hash-ref (lib-getmetatable a) name 'not-found)))
                (if (hash? b)
                    (let
                        ((bmet (hash-ref (lib-getmetatable b) name 'not-found)))
                        (if (equal? amet 'not-found)
                            (if (equal? bmet 'not-found)
                                (error "error: attempt to perform arithmetic on a table value")
                                (call bmet a b))
                            (call amet a b)))
                    (if (equal? amet 'not-found)
                        (error "error: attempt to perform arithmetic on a table value")
                        (call amet a b))))
            (if (hash? b)
                (let
                    ((bmet (hash-ref (lib-getmetatable b) name 'not-found)))
                    (if (equal? bmet 'not-found)
                        (error "error: attempt to perform arithmetic on a table value")
                        (call bmet a b)))
                (func a b)))))
                
(define (builtin-tostring a)
    (if (hash? a)
        (let    
            ((amet (hash-ref (lib-getmetatable a) "__tostring" 'not-found)))
            (if (equal? amet 'not-found)
                "<table>"
                (call amet a)))
        (cond
            ((string? a) a)
            ((equal? a nil) "nil")
            ((procedure? a) "<function>")
            ((boolean? a) (if a "true" "false"))
            ((number? a) (number->string a)))))

(define (builtin-concat a b)
    (if (hash? a)
        (let
            ((amet (hash-ref (lib-getmetatable a) "__concat" 'not-found)))
            (if (hash? b)
                (let
                    ((bmet (hash-ref (lib-getmetatable b) "__concat" 'not-found)))
                    (if (equal? amet 'not-found)
                        (if (equal? bmet 'not-found)
                            (error "error: attempt to concatenate a table value")
                            (call bmet a b))
                        (call amet a b)))
                (if (equal? amet 'not-found)
                    (error "error: attempt to concatenate a table value")
                    (call amet a b))))
        (if (hash? b)
            (let
                ((bmet (hash-ref (lib-getmetatable b) "__concat" 'not-found)))
                (if (equal? bmet 'not-found)
                    (error "error: attempt to concatenate a table value")
                    (call bmet a b)))
            (string-append (builtin-tostring a) (builtin-tostring b)))))

(define builtin-add (binary-arith "__add" +))
(define builtin-sub (binary-arith "__sub" -))
(define builtin-mul (binary-arith "__mul" *))
(define builtin-div (binary-arith "__div" /))
(define builtin-mod (binary-arith "__mod" modulo))
(define builtin-pow (binary-arith "__pow" expt))

(define (boolean-eq . v)
    (apply equal? v))

(define (boolean-neq . v)
    (not (apply equal? v)))
        
(define (lib-setmetatable tab meta)
    (hash-set! tab 'metatable meta))

(define (lib-getmetatable tab)
    (let
        ((tab (hash-ref tab 'metatable 'empty-table)))
        (if (equal? tab 'empty-table)
            (make-hasheq)
            tab)))

(define (lib-print . args)
    (map
        (lambda (arg)
            (cond
                ((not (equal? (car arg) 0)) (display "\t"))
                (else (void)))
            (display (builtin-tostring (cdr arg))))
        (map cons
            (range (length args))
            args))
    (newline))

(hash-set! _G "_G" _G)
(hash-set! _G "getmetatable" lib-getmetatable)
(hash-set! _G "setmetatable" lib-setmetatable)
(hash-set! _G "print" lib-print)

(provide (all-defined-out))
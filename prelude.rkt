#lang racket/base

(define lua.nil (void))

(define lua.tmp (void))

(define (lua.index tab key)
    (hash-ref! tab key lua.nil))

(define (lua.toboolean val)
    (not (or (equal? val #f)) (equal? val lua.nil)))

(define (lua.+ lhs rhs)
    (+ lhs rhs))

(define (lua.- lhs rhs)
    (- lhs rhs))

(define (lua.< lhs rhs)
    (< lhs rhs))

(define (lua.call fun args)
    (apply fun args))

(define (lua.cdr lis)
    (if (null? lis)
        (list)
        (cdr lis)))

(define (lua.car lis)
    (if (null? lis)
        lua.nil
        (car lis)))

(define (lua.tostring obj)
    (cond
        ((void? obj) "nil")
        ((equal? obj #t) "true")
        ((equal? obj #f) "false")
        ((procedure? obj) "<function>")
        ((string? obj) obj)
        ((number? obj) (number->string obj))
        ((hash? obj) "<table>")))

(define _ENV (make-hash))
(hash-set! _ENV "print"
    (lambda args
        (let ((first #f))
            (for ((arg args))
                (if first
                    (display #\tab)
                    (set! first #t))
                (display (lua.tostring arg))))
        (newline)
        (list lua.nil)))
(hash-set! _ENV "_G" _ENV)

#lang racket/base

(require racket/list)
(require racket/string)
(require racket/cmdline)
(require racket/file)

(require syntax/parse/define)

(require lua/compiler)
(require lua/comb/parser)

(define _G (make-hash))
(define io (make-hash))

(define nil (void))
(define nil? void?)

(define (to-boolean v)
    (and v (not (equal? v nil))))

(define-simple-macro (boolean-or a b)
    (let
        ((av a)
        (bv b))
        (if (to-boolean av) av (if (to-boolean bv) bv #f))))

(define-simple-macro (boolean-not a)
    (not (to-boolean a)))

(define (first-after-not-in table n)
    (if (and (hash-has-key? table n) (not (nil? (hash-ref table n nil))))
        (first-after-not-in table (+ n 1))
        n))

(define (length-lua table)
    (- (first-after-not-in table 1) 1))

(define-simple-macro (boolean-and a b)
    (let
        ((av a)
        (bv b))
        (if (to-boolean av) (if (to-boolean bv) bv #f) #f)))

(define make-return list)

(define (ecall func . args)
    (define ret (apply func args))
    (cond
        ((not (list? ret)) ret)
        ((empty? ret) nil)
        (else (car ret))))

(define (call func . args)
    (define concat (hash-ref (lib-getmetatable func) "__call" 'not-found))
    (if (equal? concat 'not-found)
        (apply func args)
        (apply concat func args)))

(define (typeof obj)
    (cond
        ((hash? obj) "table")
        ((nil? obj) "nil")
        ((boolean? obj) "boolean")
        ((string? obj) "string")
        ((procedure? obj) "function")
        ((number? obj) "number")
        (#t "userdata")))

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
                                (ecall bmet a b))
                            (ecall amet a b)))
                    (if (equal? amet 'not-found)
                        (error "error: attempt to perform arithmetic on a table value")
                        (ecall amet a b))))
            (if (hash? b)
                (let
                    ((bmet (hash-ref (lib-getmetatable b) name 'not-found)))
                    (if (equal? bmet 'not-found)
                        (error "error: attempt to perform arithmetic on a table value")
                        (ecall bmet a b)))
                (func a b)))))
                
(define (builtin-tostring a)
    (if (hash? a)
        (let    
            ((amet (hash-ref (lib-getmetatable a) "__tostring" 'not-found)))
            (if (equal? amet 'not-found)
                "<table>"
                (ecall amet a)))
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
                            (ecall bmet a b))
                        (ecall amet a b)))
                (if (equal? amet 'not-found)
                    (error "error: attempt to concatenate a table value")
                    (ecall amet a b))))
        (if (hash? b)
            (let
                ((bmet (hash-ref (lib-getmetatable b) "__concat" 'not-found)))
                (if (equal? bmet 'not-found)
                    (error "error: attempt to concatenate a table value")
                    (ecall bmet a b)))
            (string-append (builtin-tostring a) (builtin-tostring b)))))

(define builtin-add (binary-arith "__add" +))
(define builtin-sub (binary-arith "__sub" -))
(define builtin-mul (binary-arith "__mul" *))
(define builtin-div (binary-arith "__div" /))
(define builtin-mod (binary-arith "__mod" modulo))
(define builtin-pow (binary-arith "__pow" expt))

(define (hash-ref-lua ht key default)
    (cond
        ((and (hash? ht) (hash-has-key? ht key))
            (hash-ref! ht key default))
        ((hash-has-key? (lib-getmetatable ht) "__index")
            (call (hash-ref-lua (lib-getmetatable ht) "__index" nil) ht key))
        (#t nil)))

(define (hash-set-lua! ht key value)
    (hash-set! ht key value))

(define (boolean-eq . v)
    (apply equal? v))

(define (boolean-neq . v)
    (not (apply equal? v)))

(define metatable-sym (gensym))
(define metatable-not-found (gensym))

(define (lib-setmetatable tab meta)
    (hash-set! tab metatable-sym meta))

(define ns (make-base-namespace))
(namespace-attach-module (current-namespace) 'lua/locals ns)
(define (eval-lua stx)
    (parameterize ([current-namespace ns])
        (namespace-require 'lua/locals) 
        (eval stx ns)))

(define (lib-getmetatable tab)
    (if (hash? tab)
        (let
            ((tab (hash-ref tab metatable-sym metatable-not-found)))
            (if (equal? tab metatable-not-found)
                (make-hash)
                tab))
        (make-hash)))

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

(define (lib-type obj)
    (typeof obj))

(define (lib-load src)
    (define ast (parse-text src))
    (define stx (compile ast))
    (define result (lambda () (eval-lua stx)))
    result)

(define (hash-extend-list table n l)
    (cond
        ((not (null? l))
            (begin
                (hash-set! table n (car l))
                (hash-extend-list table (+ n 1) (cdr l))))))

(define (lib-tonumber n)
    (cond
        ((string? n)
            (let 
                ((res (string->number n)))
                (if res res
                    nil)))
        ((number? n)
            n)
        (else nil)))

(define (lib-require spec)
    (define with-slash (string-replace spec "." "/"))
    (define try1 (string-append "./" with-slash ".lua"))
    (define ns (current-namespace))
    (namespace-require try1 ns)
    (list (eval #`return ns)))

(define (lib-racket-require spec)
    (make-hash
        (list
            (cons metatable-sym
                (make-hash
                    (list
                        (cons "__index"
                            (lambda (ht name)
                                (define ns (current-namespace))
                                (namespace-require (string->symbol spec) ns)
                                (eval (string->symbol name) ns)))))))))

(define (lib-racket-require-str spec)
    (make-hash
        (list
            (cons metatable-sym
                (make-hash
                    (list
                        (cons "__index"
                            (lambda (ht name)
                                (define ns (current-namespace))
                                (namespace-require spec ns)
                                (eval (string->symbol name) ns)))))))))

(define lib-racket (make-hash))
(hash-set! lib-racket "lib" lib-racket-require)
(hash-set! lib-racket "open" lib-racket-require-str)

(define lib-math (make-hash))
(hash-set! lib-math "sqrt" sqrt)

(define cl (vector->list (current-command-line-arguments)))
(define lib-arg (make-hash (list (cons 0 "<main>"))))
(define count 0)
(void
    (map
        (lambda (arg)
            (set! count (+ count 1))
            (hash-set! lib-arg count arg))
        cl))

(hash-set! _G "_G" _G)
(hash-set! _G "getmetatable" lib-getmetatable)
(hash-set! _G "setmetatable" lib-setmetatable)
(hash-set! _G "print" lib-print)
(hash-set! _G "load" lib-load)
(hash-set! _G "rawequal" equal?)
(hash-set! _G "rawget" hash-ref)
(hash-set! _G "rawset" hash-set!)
(hash-set! _G "type" lib-type)
(hash-set! _G "tonumber" lib-tonumber)
(hash-set! _G "math" lib-math)
(hash-set! _G "racket" lib-racket)
(hash-set! _G "arg" lib-arg)
(hash-set! _G "require" lib-require)
(hash-set! _G "_VERSION" "Lua 5.3")

(provide (all-defined-out))
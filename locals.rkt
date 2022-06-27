#lang racket/base

(require racket/list)
(require racket/string)
(require racket/file)

(require "compiler.rkt")
(require "comb/parser.rkt")

(define _G (make-hash))
(define io (make-hash))

(define nil (void))
(define (nil? val)
    (or (void? val) (null? val)))

(define (to-boolean v)
    (not (or (equal? v nil) (equal? v #f))))

(define (list-has? lis ent)
    (cond
        ((empty? lis) #f)
        ((equal? (car lis) ent) #t)
        (#t (list-has? (cdr lis) ent))))

(define (no-enter f)
    (define tab (list))
    (lambda args
        (if (list-has? tab args)
            nil
            (begin
                (set! tab (cons args tab))
                (apply f args)))))

(define (table->list tab)
    (define end (length-lua tab))
    (for/list ((i (in-range 1 (+ end 1))))
        (hash-ref-lua tab i 'nil)))

(define-syntax-rule (boolean-or a b)
    (let
        ((av a)
        (bv b))
        (if (to-boolean av) av (if (to-boolean bv) bv #f))))

(define (boolean-not a)
    (not (to-boolean a)))

(define (first-after-not-in table n)
    (if (and (hash-has-key? table n) (not (nil? (hash-ref table n nil))))
        (first-after-not-in table (+ n 1))
        n))

(define (length-lua table)
    (- (first-after-not-in table 1) 1))

(define-syntax-rule (boolean-and a b)
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

(define (builtin-tostring-meta a)
    (let
        ((amet (hash-ref (lib-getmetatable a) "__tostring" 'not-found)))
        (if (equal? amet 'not-found)
            "<table>"
            (ecall amet a))))
(set! builtin-tostring-meta (no-enter builtin-tostring-meta))

(define (builtin-tostring a)
    (if (hash? a)
        (builtin-tostring-meta a)
        (cond
            ((string? a) a)
            ((nil? a) "nil")
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

(define (hash-ref-lua ht key (default nil))
    (cond
        ((and (hash? ht) (hash-has-key? ht key))
            (hash-ref! ht key default))
        ((hash-has-key? (lib-getmetatable ht) "__index")
            (let ((mt (lib-getmetatable ht)))
                (let ((ret (call (hash-ref-lua (lib-getmetatable ht) "__index") ht key)))
                    ret)))
        (#t nil)))

(define (hash-set-lua! ht key value)
    (cond
        ((hash-has-key? (lib-getmetatable ht) "__newindex")
            (call (hash-ref-lua (lib-getmetatable ht) "__newindex") ht key value))
        (#t
            (hash-set! ht key value))))

(define (boolean-eq . v)
    (apply equal? v))

(define (boolean-neq . v)
    (not (apply equal? v)))

(define metatable-sym (gensym))
(define metatable-not-found (gensym))

(define (lib-setmetatable tab meta)
    (hash-set! tab metatable-sym meta)
    tab)

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

(define (lib-racket-tmp-lib spec)
    (define with-slash (string-replace spec "." "/"))
    (define try1 (string-append "./" with-slash ".lua.tmp"))
    (define ns (current-namespace))
    (namespace-require try1 ns)
    (list (eval #`return ns)))

(define (lib-io-write data)
    (display (builtin-tostring data)))

(define (lib-string-len str)
    (string-length str))

(define (lib-string-sub str low high)
    (substring str (- low 1) high))

(define (lib-string-byte str)
    (map char->integer (string->list str)))

(define (lib-table-concat str)
    (string-join (table->list str) ""))

(define lib-racket (make-hash))
(hash-set! lib-racket "lib" lib-racket-require)
(hash-set! lib-racket "open" lib-racket-require-str)
(hash-set! lib-racket "tmplib" lib-racket-tmp-lib)

(define lib-math (make-hash))
(hash-set! lib-math "sqrt" sqrt)
(hash-set! lib-math "floor" floor)
(hash-set! lib-math "ceil" ceiling)

(define lib-io (make-hash))
(hash-set! lib-io "slurp" file->string)
(hash-set! lib-io "write" lib-io-write)

(define lib-string (make-hash))
(hash-set! lib-string "len" lib-string-len)
(hash-set! lib-string "sub" lib-string-sub)
(hash-set! lib-string "byte" lib-string-byte)

(define lib-table (make-hash))
(hash-set! lib-table "concat" lib-table-concat)

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
(hash-set! _G "io" lib-io)
(hash-set! _G "math" lib-math)
(hash-set! _G "string" lib-string)
(hash-set! _G "table" lib-table)
(hash-set! _G "tonumber" lib-tonumber)
(hash-set! _G "getmetatable" lib-getmetatable)
(hash-set! _G "setmetatable" lib-setmetatable)
(hash-set! _G "print" lib-print)
(hash-set! _G "load" lib-load)
(hash-set! _G "rawequal" equal?)
(hash-set! _G "rawget" hash-ref)
(hash-set! _G "rawset" hash-set!)
(hash-set! _G "type" lib-type)
(hash-set! _G "racket" lib-racket)
(hash-set! _G "arg" lib-arg)
(hash-set! _G "require" lib-require)
(hash-set! _G "tostring" builtin-tostring)
(hash-set! _G "_VERSION" "Lua 5.3")

(provide (all-defined-out))

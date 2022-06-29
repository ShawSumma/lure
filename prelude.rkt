#lang racket/base

(require racket/list)
(require racket/file)
(require racket/string)
(require racket/format)

(define lua.meta (gensym))
(define lua.nometa (gensym))

(define lua.tmp (void))

(define lua.nil (void))
(define lua.nil? void?)

(define lua.nil1 (list lua.nil))

(define (lua.index tab key)
    (define value (hash-ref tab key lua.nil))
    (if (lua.nil? value)
        (let ((val (meta-get tab "__index")))
            (if (procedure? val)
                (lua.call val (list tab key))
                lua.nil))
        value))

(define (lua.setindex! tab key value)
    (hash-set! tab key value))

(define (lua.toboolean val)
    (not (or (equal? val #f) (lua.nil? val))))

(define (lua.tonumber val)
    (cond
        ((string? val) (string->number val))
        ((number? val) val)))

(define (get-meta tab)
    (hash-ref tab lua.meta lua.nometa))

(define (meta-get tab name)
    (define meta (get-meta tab))
    (if (hash? meta)
        (lua.index meta name)
        lua.nil))

(define (meta-op op name)
    (lambda (lhs rhs)
        (cond ((not (number? lhs)) (set! lhs (lua.tonumber lhs))))
        (cond ((not (number? rhs)) (set! rhs (lua.tonumber rhs))))
        (if (hash? lhs)
            (let ((got (meta-get lhs name)))
                (if (lua.nil? got)
                    (op lhs rhs)
                    (got lhs rhs)))
            (op lhs rhs))))

(define lua.+ (meta-op + "__add"))
(define lua.- (meta-op - "__sub"))
(define lua.* (meta-op * "__mul"))
(define lua./ (meta-op / "__div"))
(define lua.% (meta-op modulo "__mod"))
(define lua.concat (meta-op (lambda (lhs rhs) (string-append (lua.tostring lhs) (lua.tostring rhs))) "__concat"))

(define (lua.< lhs rhs)
    (< lhs rhs))

(define (lua.<= lhs rhs)
    (<= lhs rhs))

(define (lua.== lhs rhs)
    (equal? lhs rhs))

(define (lua.> lhs rhs) (lua.< rhs lhs))
(define (lua.>= lhs rhs) (lua.<= rhs lhs))
(define (lua.~= lhs rhs) (not (lua.== lhs rhs)))

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua.nil? (hash-ref tab index lua.nil))
        low
        (binary-length tab index)))

(define (lua.length tab)
    (binary-length tab 0))

(define (lua.flat . args)
    (flatten args))

(define (lua.call fun . args)
    (apply fun (lua.flat args)))

(define (lua.list . args)
    args)

(define-syntax lua.or
    (syntax-rules ()
        ((lua.or lhs rhs)
            (let ((test lhs))
                (if (lua.toboolean test) test rhs)))))

(define-syntax lua.and
    (syntax-rules ()
        ((lua.or lhs rhs)
            (let ((test lhs))
                (if (lua.toboolean test) rhs test)))))

(define (lua.method tab name args)
    (define func (lua.index tab name))
    (lua.call func (list tab) args))

(define (lua.cdr lis)
    (if (or (not (list? lis)) (null? lis))
        (list)
        (cdr lis)))

(define (lua.car val)
    (if (list? val)
        (if (null? val)
            lua.nil
            (car val))
        val))

(define (table->list table (from 1))
    (define val (lua.index table from))
    (if (lua.nil? val)
        null
        (cons val
            (table->list table (+ 1 from)))))

(define (lua.tostring obj)
    (cond
        ((lua.nil? obj) "nil")
        ((equal? obj #t) "true")
        ((equal? obj #f) "false")
        ((procedure? obj) "<function>")
        ((string? obj) (string-append "\"" obj "\""))
        ((number? obj) (number->string obj))
        ((hash? obj) "<table>")))

(define arg (vector->list (current-command-line-arguments)))

(define _ENV (make-hash
    (list
        (cons "setmetatable"
            (lambda (tab meta)
                (hash-set! tab lua.meta meta)
                tab))
        (cons "arg"
            (for/hash ((ent arg) (i (in-range 1 (+ 1 (length arg)))))
                (values i ent)))
        (cons "assert"
            (lambda (thing (err lua.nil))
                (cond
                    ((not (lua.toboolean thing))
                        (error (lua.tostring err))))
                thing))
        (cons "type"
            (lambda (arg)
                (cond
                    ((lua.nil? arg) "nil")
                    ((boolean? arg) "boolean")
                    ((procedure? arg) "function")
                    ((string? arg) "string")
                    ((number? arg) "number")
                    ((hash? arg) "table"))))
        (cons "print"
            (lambda args
                (define first #f)
                (for ((arg args))
                    (if first
                        (display #\tab)
                        (set! first #t))
                    (display
                        (if (string? arg)
                            args
                            (lua.tostring arg))))
                (newline)
                lua.nil1))
        (cons "table"
            (make-hash
                (list
                    (cons "concat"
                        (lambda (arg (sep ""))
                            (string-join (table->list arg) sep))))))
        (cons "string"
            (make-hash
                (list
                    (cons "sub"
                        (lambda (str start (end lua.nil) . nils)
                            (list
                                (substring str
                                    (- start 1)
                                    (- (if (lua.nil? end) (string-length str) end) 1)))))
                    (cons "len"
                        (lambda (str)
                            (list (string-length str)))))))
        (cons "io"
            (make-hash
                (list
                    (cons "dump"
                        (lambda (filename data . nils)
                            (display-to-file data filename #:exists 'replace)
                            lua.nil1))
                    (cons "slurp"
                        (lambda (name . nils)
                            (list (file->string name))))))))))
(hash-set! _ENV "_G" _ENV)
(define local-_ENV _ENV)
#lang racket/base

(require racket/file)

(define lua.meta (gensym))
(define lua.nometa (gensym))

(define lua.nil (void))
(define lua.nil? void?)

(define lua.nil1 (list lua.nil))

(define (lua.index tab key)
    (define value (hash-ref tab key lua.nil))
    (if (lua.nil? value)
        (let ((val (meta-get tab "__index")))
            (if (procedure? val)
                (lua.car (lua.call val (list tab key)))
                lua.nil))
        value))

(define (lua.setindex! tab key value)
    (cond
        ((string? key) (string->symbol key)))
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
        (if (hash? lhs)
            (let ((got (meta-get lhs name)))
                (if (lua.nil? got)
                    (op lhs rhs)
                    (lua.car (got lhs rhs))))
            (op lhs rhs))))

(define (number-op fun)
    (lambda args
        (apply fun (map lua.tonumber args))))

(define lua.+ (meta-op (number-op +) "__add"))
(define lua.- (meta-op (number-op -) "__sub"))
(define lua.* (meta-op (number-op *) "__mul"))
(define lua.^ (meta-op (number-op expt) "__pow"))
(define lua./ (meta-op (number-op /) "__div"))
(define lua.% (meta-op (number-op modulo) "__mod"))
(define (lua.concat lhs rhs)
    (if (hash? lhs)
        (let ((got (meta-get lhs "__concat")))
            (if (lua.nil? got)
                (string-append lhs rhs)
                (got lhs rhs)))
        (string-append (lua.tostring lhs) (lua.tostring rhs))))

(define (lua.< lhs rhs)
    (< lhs rhs))

(define (lua.<= lhs rhs)
    (<= lhs rhs))

(define (lua.== lhs rhs)
    (equal? lhs rhs))

(define (lua.> lhs rhs) (lua.< rhs lhs))
(define (lua.>= lhs rhs) (lua.<= rhs lhs))
(define (lua.~= lhs rhs)
    (not (lua.== lhs rhs)))

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua.nil? (hash-ref tab index lua.nil))
        low
        (binary-length tab index)))

(define (lua.length tab)
    (binary-length tab 0))

(define (lua.flat . args)
    (apply append args))

(define (lua.call fun . args)
    (apply fun (apply lua.flat args)))

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

(define (list->table lis (index 1) (table (make-hash)))
    (hash-set! table index lis)
    (if (null? lis)
        table
        (list->table (cdr lis) (+ 1 index) table)))

(define (lua.tostring obj)
    (cond
        ((lua.nil? obj) "nil")
        ((equal? obj #t) "true")
        ((equal? obj #f) "false")
        ((procedure? obj) "<function>")
        ((string? obj) obj)
        ((number? obj) (number->string obj))
        ((hash? obj)
            (let ((fun (meta-get obj "__tostring")))
                (if (lua.nil? fun)
                    "<table>"
                    (lua.car (fun obj)))))
        (#t "<userdata>")))

(define arg (vector->list (current-command-line-arguments)))

(define _ENV (make-hash
    (list
        (cons "racket"
            #t)
        (cons "setmetatable"
            (lambda (tab meta)
                (hash-set! tab lua.meta meta)
                (list tab)))
        (cons "arg"
            (list->table arg))
        (cons "assert"
            (lambda (thing (err lua.nil))
                (cond
                    ((not (lua.toboolean thing))
                        (error (lua.tostring err))))
                (lua.list thing)))
        (cons "tonumber"
            (lambda ((arg lua.nil))
                (lua.list (lua.tonumber arg))))
        (cons "tostring"
            (lambda (arg)
                (lua.list (lua.tostring arg))))
        (cons "type"
            (lambda (arg)
                (lua.list
                    (cond
                        ((lua.nil? arg) "nil")
                        ((boolean? arg) "boolean")
                        ((procedure? arg) "function")
                        ((string? arg) "string")
                        ((number? arg) "number")
                        ((hash? arg) "table")
                        (#t "userdata")))))
        (cons "print"
            (lambda args
                (define first #f)
                (for ((arg args))
                    (if first
                        (display #\tab)
                        (set! first #t))
                    (display
                        (if (string? arg)
                            arg
                            (lua.tostring arg))))
                (newline)
                lua.nil1))
        (cons "table"
            (make-hash
                (list
                    (cons "unpack"
                        (lambda (tab) (table->list tab)))
                    (cons "concat"
                        (lambda (arg)
                            (list
                                (apply string-append (table->list arg))))))))
        (cons "string"
            (make-hash
                (list
                    (cons "byte"
                        (lambda (str)
                            (list (char->integer (car (string->list str))))))
                    (cons "sub"
                        (lambda (str start (end lua.nil) . nils)
                            (list
                                (substring str
                                    (- start 1)
                                    (if (lua.nil? end)
                                        (string-length str)
                                        end)))))
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
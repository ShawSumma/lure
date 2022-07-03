#lang racket/base

(require racket/file)
(require racket/list)
(require racket/generic)

(define-generics lua.value
    (lua.export lua.value)
    (lua.type lua.value)
    (lua.tostring lua.value)
    (lua.tonumber lua.value)
    (lua.true? lua.value)
    (lua.add lua.value rhs)
    (lua.sub lua.value rhs)
    (lua.mul lua.value rhs)
    (lua.div lua.value rhs)
    (lua.mod lua.value rhs)
    (lua.concat lua.value rhs)
    (lua.lt lua.value rhs)
    (lua.le lua.value rhs)
    (lua.eq lua.value rhs)
    (lua.call lua.value . args)
    (lua.index lua.value key)
    (lua.setindex! lua.value key value)
    (lua.method lua.value method . args))

(define lua.raw lua.export)

(struct lua-type-nil ()
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            (void))
        (define (lua.true? val)
            #f)
        (define (lua.type val)
            (lua-type-string "nil"))
        (define (lua.tostring val)
            (lua-type-string "nil"))
        (define (lua.eq val rhs)
            (lua-type-boolean (lua-type-nil? rhs)))
        (define (lua.method val method . args)
            (error "attempt to call method on nil"))))
(struct lua-type-boolean (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            (lua-type-boolean-value val))
        (define (lua.eq val rhs)
            (lua-type-boolean (and (lua-type-boolean? rhs) (eq? (lua-type-boolean-value val) (lua-type-boolean-value rhs)))))
        (define (lua.type val)
            (lua-type-string "boolean"))
        (define (lua.true? val)
            (lua-type-boolean-value val))
        (define (lua.method val method . args)
            (error "attempt to call method on boolean value"))))
(struct lua-type-number (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            (lua-type-number-value val))
        (define (lua.type val)
            (lua-type-string "number"))
        (define (lua.tostring val)
            (lua-type-string (number->string (lua-type-number-value val))))
        (define (lua.tonumber val)
            val)
        (define (lua.concat val rhs)
            (lua-type-string
                (string-append
                    (lua-type-string-value (lua.tostring val))
                    (lua-type-string-value (lua.tostring rhs)))))
        (define (lua.add val rhs)
            (lua-type-number (+ (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.sub val rhs)
            (lua-type-number (- (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.mul val rhs)
            (lua-type-number (* (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.div val rhs)
            (lua-type-number (/ (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.mod val rhs)
            (lua-type-number (modulo (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.lt val rhs)
            (lua-type-boolean (< (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.le val rhs)
            (lua-type-boolean (<= (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.eq val rhs)
            (lua-type-boolean (= (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.true? val)
            #t)
        (define (lua.method val method . args)
            (error "attempt to call method on number"))))
(struct lua-type-string (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            (lua-type-string-value val))
        (define (lua.type val)
            (lua-type-string "string"))
        (define (lua.concat val rhs)
            (lua-type-string
                (string-append
                    (lua-type-string-value val)
                    (lua-type-string-value (lua.tostring rhs)))))
        (define (lua.tostring val)
            val)
        (define (lua.tonumber val)
            (lua-type-number (string->number (lua-type-string-value val))))
        (define (lua.true? val)
            (lua-type-boolean #t))
        (define (lua.eq val rhs)
            (lua-type-boolean (and (lua-type-string? rhs) (equal? (lua-type-string-value val) (lua-type-string-value rhs)))))))
(struct lua-type-table (value (meta #:mutable))
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            val)
        (define (lua.type val)
            (lua-type-string "table"))
        (define (lua.true? val)
            #t)
        (define (lua.setindex! val key value)
            (define meta (lua-get-meta val "__newindex"))
            (if (lua-type-nil? meta)
                (hash-set! (lua-type-table-value val) (lua.raw key) value)
                (lua.call meta val key value)))
        (define (lua.index val key)
            (define ret (hash-ref (lua-type-table-value val) (lua.raw key) 'not-found))
            (if (eq? ret 'not-found)
                (let ((meta (lua-get-meta val "__index")))
                    (if (lua-type-nil? meta)
                        lua.nil
                        (lua.call meta val key)))
                ret))
        (define (lua.call val . args)
            (lua.call (lua.index val "__call") args))
        (define (lua.eq val other)
            (lua-type-boolean (eq? val other)))
        (define (lua.method val method . args)
            (lua.call (lua.index val method) args))))
(struct lua-type-function (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.export val)
            (lua-type-function-value val))
        (define (lua.type val)
            (lua-type-string "function"))
        (define (lua.eq val rhs)
            (lua-type-boolean (and
                (lua-type-function? rhs)
                (equal? (lua-type-function-value val) (lua-type-function-value rhs)))))
        (define (lua.call val . args)
            (apply (lua-type-function-value val) (lua.flat args)))))

(define (lua-get-meta tab name)
    (if (lua-type-table? tab)
        (let ((meta (lua-type-table-meta tab)))
            (if (lua-type-nil? meta)
                lua.nil
                (lua.index meta (lua-type-string name))))
        (error "not implemented: value metatables")))

(define (lua.table (value null))
    (lua-type-table (make-hash value) lua.nil))

(define lua.nil (lua-type-nil))

(define (lua.gt lhs rhs)
    (lua.lt rhs lhs))
(define (lua.ge lhs rhs)
    (lua.le rhs lhs))
(define (lua.ne lhs rhs)
    (lua-type-boolean (not (lua-type-boolean-value (lua.eq lhs rhs)))))

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua-type-nil? (lua.index tab (lua-type-number index)))
        low
        (binary-length tab index)))

(define (lua.length tab)
    (lua-type-number (binary-length tab 0)))

(define (lua.flat . args)
    (flatten args))

(define (lua.list . args)
    args)

(define-syntax lua.or
    (syntax-rules ()
        ((lua.or lhs rhs)
            (let ((test lhs))
                (if (lua.true? test) test rhs)))))

(define-syntax lua.and
    (syntax-rules ()
        ((lua.or lhs rhs)
            (let ((test lhs))
                (if (lua.true? test) rhs test)))))

(define (lua.cdr lis)
    (if (pair? lis)
        (cdr lis)
        lua.nil))

(define (lua.car val)
    (if (pair? val)
        (car val)
        (if (null? val)
            lua.nil
            val)))

(define (table->list table (from 1))
    (define val (lua.index table (lua-type-number from)))
    (if (lua-type-nil? val)
        null
        (cons val
            (table->list table (+ 1 from)))))

(define arg (vector->list (current-command-line-arguments)))

(define _ENV (lua.table
    (list
        (cons "racket"
            (lua-type-boolean #t))
        (cons "setmetatable"
            (lua-type-function (lambda (tab meta . nils)
                    (set-lua-type-table-meta! tab meta)
                    tab)))
        (cons "arg"
            (lua.table
                (for/list ((ent arg) (i (in-range 1 (+ 1 (length arg)))))
                    (cons i (lua-type-string ent)))))
        (cons "assert"
            (lua-type-function (lambda (thing (err lua.nil))
                (cond
                    ((not (lua.true? thing))
                        (error (lua.tostring err))))
                thing)))
        (cons "tostring"
            (lua-type-function (lambda (arg . nils)
                (lua.tostring arg))))
        (cons "type"
            (lua-type-function (lambda (arg . nils)
                (lua.type arg))))
        (cons "print"
            (lua-type-function (lambda args
                (define first #f)
                (for ((arg args))
                    (if first
                        (display #\tab)
                        (set! first #t))
                    (display
                        (lua-type-string-value (lua.tostring arg))))
                (newline)
                (list))))
        (cons "table"
            (lua.table
                (list
                    (cons "unpack"
                        (lua-type-function (lambda (tab . nils)
                            (table->list tab))))
                    (cons "concat"
                        (lua-type-function (lambda (arg . nils)
                            (list
                                (lua-type-string (apply string-append
                                    (map lua-type-string-value (table->list arg)))))))))))
        (cons "string"
            (lua.table
                (list
                    (cons "byte"
                        (lua-type-function (lambda (str . nils)
                            (lua-type-number (char->integer (car (string->list (lua-type-string-value (lua.tostring str)))))))))
                    (cons "sub"
                        (lua-type-function (lambda (str start (end lua.nil) . nils)
                            (lua-type-string
                                (substring (lua-type-string-value (lua.tostring str))
                                    (- (lua-type-number-value start) 1)
                                    (if (lua-type-nil? end)
                                        (string-length str)
                                        (lua-type-number-value end)))))))
                    (cons "len"
                        (lua-type-function (lambda (str . nils)
                            (lua-type-number (string-length (lua-type-string-value (lua.tostring str))))))))))
        (cons "io"
            (lua.table
                (list
                    (cons "dump"
                        (lua-type-function (lambda (filename data . nils)
                            (display-to-file (lua-type-string-value (lua.tostring data)) (lua-type-string-value filename) #:exists 'replace)
                            (list))))
                    (cons "slurp"
                        (lua-type-function (lambda (name . nils)
                            (lua-type-string (file->string (lua-type-string-value name))))))))))))
(lua.setindex! _ENV (lua-type-string "_G") _ENV)
(define local-_ENV _ENV)

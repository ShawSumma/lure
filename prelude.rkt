#lang racket/base

(require racket/file)
(require racket/list)
(require racket/generic)

(define-generics lua.value
    (lua.value.export lua.value)
    (lua.value.tovalues lua.value)
    (lua.value.type lua.value)
    (lua.value.tostring lua.value)
    (lua.value.tonumber lua.value)
    (lua.value.true? lua.value)
    (lua.value.add lua.value rhs)
    (lua.value.sub lua.value rhs)
    (lua.value.mul lua.value rhs)
    (lua.value.div lua.value rhs)
    (lua.value.mod lua.value rhs)
    (lua.value.concat lua.value rhs)
    (lua.value.lt lua.value rhs)
    (lua.value.le lua.value rhs)
    (lua.value.eq lua.value rhs)
    (lua.value.call lua.value . args)
    (lua.value.index lua.value key)
    (lua.value.length lua.value)
    (lua.value.setindex! lua.value key value)
    (lua.value.method lua.value method . args))

(define lua.export lua.value.export)
(define lua.tovalues lua.value.tovalues)
(define lua.type lua.value.type)
(define lua.tostring lua.value.tostring)
(define lua.tonumber lua.value.tonumber)
(define lua.true? lua.value.true?)
(define lua.add lua.value.add)
(define lua.sub lua.value.sub)
(define lua.mul lua.value.mul)
(define lua.div lua.value.div)
(define lua.mod lua.value.mod)
(define lua.concat lua.value.concat)
(define lua.lt lua.value.lt)
(define lua.le lua.value.le)
(define lua.eq lua.value.eq)
(define lua.scall lua.value.call)
(define lua.index lua.value.index)
(define lua.length lua.value.length)
(define lua.setindex! lua.value.setindex!)
(define lua.smethod lua.value.method)

(define-syntax lua.call
    (syntax-rules ()
        ((lua.call fun)
            (lua.scall fun))
        ((lua.call fun args ... end)
            (apply lua.scall fun args ... (lua.tovalues end)))))

(define-syntax lua.method
    (syntax-rules ()
        ((lua.method fun meth)
            (lua.smethod fun meth))
        ((lua.method fun meth args ... end)
            (apply lua.smethod fun args ... (lua.tovalues end)))))

(define (lua.gt lhs rhs)
    (lua.value.lt rhs lhs))
(define (lua.ge lhs rhs)
    (lua.value.le rhs lhs))
(define (lua.ne lhs rhs)
    (lua-type-boolean (not (lua-type-boolean-value (lua.value.eq lhs rhs)))))

(struct lua-type-nil ()
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            (void))
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.true? val)
            #f)
        (define (lua.value.type val)
            (lua-type-string "nil"))
        (define (lua.value.tostring val)
            (lua-type-string "nil"))
        (define (lua.value.eq val rhs)
            (lua-type-boolean (lua-type-nil? rhs)))
        (define (lua.value.method val method . args)
            (error "attempt to call method on nil"))))
(struct lua-type-boolean (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            (lua-type-boolean-value val))
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.eq val rhs)
            (lua-type-boolean (and (lua-type-boolean? rhs) (eq? (lua-type-boolean-value val) (lua-type-boolean-value rhs)))))
        (define (lua.value.type val)
            (lua-type-string "boolean"))
        (define (lua.value.true? val)
            (lua-type-boolean-value val))
        (define (lua.value.method val method . args)
            (error "attempt to call method on boolean value"))))
(struct lua-type-number (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            (lua-type-number-value val))
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.type val)
            (lua-type-string "number"))
        (define (lua.value.tostring val)
            (lua-type-string (number->string (lua-type-number-value val))))
        (define (lua.value.tonumber val)
            val)
        (define (lua.value.concat val rhs)
            (lua-type-string
                (string-append
                    (lua-type-string-value (lua.tostring val))
                    (lua-type-string-value (lua.tostring rhs)))))
        (define (lua.value.add val rhs)
            (lua-type-number (+ (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.sub val rhs)
            (lua-type-number (- (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.mul val rhs)
            (lua-type-number (* (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.div val rhs)
            (lua-type-number (/ (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.mod val rhs)
            (lua-type-number (modulo (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.lt val rhs)
            (lua-type-boolean (< (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.le val rhs)
            (lua-type-boolean (<= (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.eq val rhs)
            (lua-type-boolean (= (lua-type-number-value val) (lua-type-number-value (lua.tonumber rhs)))))
        (define (lua.value.true? val)
            #t)
        (define (lua.value.method val method . args)
            (error "attempt to call method on number"))))
(struct lua-type-string (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            (lua-type-string-value val))
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.type val)
            (lua-type-string "string"))
        (define (lua.value.concat val rhs)
            (lua-type-string
                (string-append
                    (lua-type-string-value val)
                    (lua-type-string-value (lua.tostring rhs)))))
        (define (lua.value.tostring val)
            val)
        (define (lua.value.tonumber val)
            (lua-type-number (string->number (lua-type-string-value val))))
        (define (lua.value.true? val)
            (lua-type-boolean #t))
        (define (lua.value.eq val rhs)
            (lua-type-boolean (and (lua-type-string? rhs) (equal? (lua-type-string-value val) (lua-type-string-value rhs)))))))
(struct lua-type-table (value (meta #:mutable))
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            val)
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.type val)
            (lua-type-string "table"))
        (define (lua.value.true? val)
            #t)
        (define (lua.value.setindex! val key value)
            (define meta (lua-get-meta val "__newindex"))
            (if (lua-type-nil? meta)
                (hash-set! (lua-type-table-value val) (lua.export key) value)
                (lua.call meta val key value)))
        (define (lua.value.index val key)
            (define ret (hash-ref (lua-type-table-value val) (lua.export key) 'not-found))
            (if (eq? ret 'not-found)
                (let ((meta (lua-get-meta val "__index")))
                    (if (lua-type-nil? meta)
                        lua.nil
                        (lua.call meta val key)))
                ret))
        (define (lua.value.length val)
            (lua-type-number (binary-length val 0)))
        (define (lua.value.call val . args)
            (lua.call (lua.index val "__call") args))
        (define (lua.value.eq val other)
            (lua-type-boolean (eq? val other)))
        (define (lua.value.method val method . args)
            (apply lua.scall (lua.index val method) args))))
(struct lua-type-function (value)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export val)
            (lua-type-function-value val))
        (define (lua.value.tovalues val)
            (list val))
        (define (lua.value.type val)
            (lua-type-string "function"))
        (define (lua.value.eq val rhs)
            (lua-type-boolean (and
                (lua-type-function? rhs)
                (equal? (lua-type-function-value val) (lua-type-function-value rhs)))))
        (define (lua.value.call val . args)
            (apply (lua-type-function-value val) args))))

(define (maybe-car v)
    (if (null? v)
        lua.nil
        (car v)))

(struct lua.values (values)
    #:sealed #:authentic
    #:methods gen:lua.value (
        (define (lua.value.export vals)
            (lua.export (maybe-car (lua.values-values vals))))
        (define (lua.value.tovalues vals)
            (lua.values-values vals))
        (define (lua.value.type vals)
            (lua.type (maybe-car (lua.values-values vals))))
        (define (lua.value.tostring vals)
            (lua.tostring (maybe-car (lua.values-values vals))))
        (define (lua.value.tonumber vals)
            (lua.tonumber (maybe-car (lua.values-values vals))))
        (define (lua.value.true? vals)
            (lua.true? (maybe-car (lua.values-values vals))))
        (define (lua.value.add vals rhs)
            (lua.add (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.sub vals rhs)
            (lua.sub (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.mul vals rhs)
            (lua.mul (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.div vals rhs)
            (lua.div (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.mod vals rhs)
            (lua.mod (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.concat vals rhs)
            (lua.concat (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.lt vals rhs)
            (lua.lt (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.le vals rhs)
            (lua.le (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.eq vals rhs)
            (lua.eq (maybe-car (lua.values-values vals)) rhs))
        (define (lua.value.call vals . args)
            (apply lua.scall (maybe-car (lua.values-values vals)) args))
        (define (lua.value.index vals key)
            (lua.index (maybe-car (lua.values-values vals)) key))
        (define (lua.value.length vals)
            (lua.length (maybe-car (lua.values-values vals))))
        (define (lua.value.setindex! vals key value)
            (lua.setindex! (maybe-car (lua.values-values vals)) key value))
        (define (lua.value.method vals method . args)
            (apply lua.smethod (maybe-car (lua.values-values vals)) args))))

(define-syntax lua.car
    (syntax-rules ()
        ((lua.car v) v)))

(define-syntax lua.tail
    (syntax-rules ()
        ((lua.tail end) end)
        ((lua.tail args ... end)
            (lua.values (apply list args ... (lua.tovalues end))))))

(define (lua.cdr val)
    (define rest (lua.tovalues val))
    (if (null? rest)
        (lua.values rest)
        (lua.values (cdr rest))))

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

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua-type-nil? (lua.index tab (lua-type-number index)))
        low
        (binary-length tab index)))

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
                (lua.values (list)))))
        (cons "table"
            (lua.table
                (list
                    (cons "unpack"
                        (lua-type-function (lambda (tab . nils)
                            (table->list tab))))
                    (cons "concat"
                        (lua-type-function (lambda (arg . nils)
                            (lua-type-string (apply string-append
                                (map lua-type-string-value (table->list arg))))))))))
        (cons "string"
            (lua.table
                (list
                    (cons "byte"
                        (lua-type-function (lambda (str . nils)
                            (lua-type-number (char->integer (car (string->list (lua-type-string-value (lua.tostring str)))))))))
                    (cons "sub"
                        (lua-type-function (lambda (str start (end lua.nil) . nils)
                            (set! start (lua.tonumber start))
                            (set! end (lua.tonumber end))
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
                            (lua.values (list)))))
                    (cons "slurp"
                        (lua-type-function (lambda (name . nils)
                            (lua-type-string (file->string (lua-type-string-value name))))))))))))
(lua.setindex! _ENV (lua-type-string "_G") _ENV)
(define local-_ENV _ENV)

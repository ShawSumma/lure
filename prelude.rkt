#lang racket/base

(require racket/file)
(require racket/list)
(require racket/generic)

(define lua.meta (gensym))

; (define lua.export lua.value.export)
; (define lua.type lua.value.type)
; (define lua.tostring lua.value.tostring)
; (define lua.tonumber lua.value.tonumber)
; (define lua.true? lua.value.true?)
; (define lua.add lua.value.add)
; (define lua.sub lua.value.sub)
; (define lua.mul lua.value.mul)
; (define lua.div lua.value.div)
; (define lua.mod lua.value.mod)
; (define lua.concat lua.value.concat)
; (define lua.lt lua.value.lt)
; (define lua.le lua.value.le)
; (define lua.eq lua.value.eq)
; (define lua.call lua.value.call)
; (define lua.index lua.value.index)
; (define lua.length lua.value.length)
; (define lua.setindex! lua.value.setindex!)
; (define ua.method lua.value.method)

(define-syntax-rule (lua.export v) v)
(define-syntax-rule (lua.values-values v) v)

(define-syntax-rule (lua-type-nil) (void))
(define-syntax-rule (lua-type-boolean v) v)
(define-syntax-rule (lua-type-number v) v)
(define-syntax-rule (lua-type-string v) v)
(define-syntax-rule (lua-type-table v) v)
(define-syntax-rule (lua-type-function v) v)

(define-syntax-rule (lua-type-nil? v) (void? v))
(define-syntax-rule (lua-type-boolean? v) (boolean? v))
(define-syntax-rule (lua-type-number? v) (number? v))
(define-syntax-rule (lua-type-string? v) (string? v))
(define-syntax-rule (lua-type-table? v) (hash? v))
(define-syntax-rule (lua-type-function? v) (procedure? v))

(define-syntax-rule (lua.values args ...) (list args ...))
(define-syntax-rule (lua.values? v) (list? v))

(define-syntax-rule (lua.tonumber valv)
    (let ((val valv))
        (cond
            ((number? val) val)
            ((string? val) (string->number val))
            ((hash? val) (lua.call (lua-get-meta val "__tonumber") val))
            (else (error "cannot implicitly convert that to number")))))

(define-syntax-rule (lua.tostring valv)
    (let ((val valv))
        (cond
            ((void? val) "nil")
            ((eq? val #f) "false")
            ((eq? val #t) "true")
            ((number? val) (number->string val))
            ((string? val) val)
            ((hash? val)
                (lua.call (lua-get-meta val "__tonumber") val))
            ((procedure? val) "<function>"))))

(define-syntax-rule (lua.type valv)
    (let ((val valv))
        (cond
            ((void? val) "nil")
            ((boolean? val) "boolean")
            ((number? val) "number")
            ((string? val) "string")
            ((hash? val) "table")
            ((procedure? val) "function"))))

(define-syntax-rule (lua.add lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((number? lhs) (+ lhs (lua.tonumber rhs)))
            ((hash? lhs) (lua.call (lua-get-meta lhs "__add") lhs rhs))
            ((string? lhs) (+ (lua.tonumber lhs) (lua.tonumber rhs))))))

(define-syntax-rule (lua.sub lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((number? lhs) (- lhs (lua.tonumber rhs)))
            ((hash? lhs) (lua.call (lua-get-meta lhs "__sub") lhs rhs))
            ((string? lhs) (- (lua.tonumber lhs) (lua.tonumber rhs))))))

(define-syntax-rule (lua.concat lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((string? lhs) (string-append lhs (lua.tostring rhs)))
            ((hash? lhs) (lua.call (lua-get-meta lhs "__concat") lhs rhs))
            ((number? lhs) (string-append (lua.tostring lhs) (lua.tostring rhs))))))

(define-syntax-rule (lua.eq lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((hash? lhs) (lua.call (lua-get-meta lhs "__eq") lhs rhs))
            (else (equal? lhs rhs)))))

(define-syntax-rule (lua.lt lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((number? lhs) (< lhs (lua.tonumber rhs)))
            ((hash? lhs) (lua.call (lua-get-meta lhs "__lt") lhs rhs))
            (else (< (lua.tonumber lhs) (lua.tonumber rhs))))))

(define-syntax-rule (lua.le lhsv rhsv)
    (let ((lhs lhsv) (rhs rhsv))
        (cond
            ((number? lhs) (<= lhs (lua.tonumber rhs)))
            ((hash? lhs) (lua.call (lua-get-meta lhs "__le") lhs rhs))
            (else (<= (lua.tonumber lhs) (lua.tonumber rhs))))))

(define-syntax-rule (lua-type-table-meta tab)
    (hash-ref tab lua.meta lua.nil))

(define-syntax-rule (set-lua-type-table-meta! tab meta)
    (hash-set! tab lua.meta meta))

(define-syntax-rule (lua.index tabv keyv)
    (let ((tab tabv) (key keyv))
        (cond
            ((hash? tab)
                (let ((val (hash-ref tabv key)))
                    (if (void? val)
                        (lua.call (lua-get-meta tab "__index") val key)
                        val)))
            (else (error "cannot index that")))))

(define-syntax-rule (lua.setindex! tabv keyv valv)
    (let ((tab tabv) (key keyv) (val valv))
        (cond
            ((hash? tab)
                (let ((newindex (lua-get-meta tab "__newindex")))
                    (if (void? newindex)
                        (hash-set! tab key val)
                        (lua.call newindex tab key val))))
            (else (error "cannot set index on that")))))

(define-syntax-rule (lua.true? valv)
    (let ((val valv))
        (and (not (equal? val #f)) (not (void? val)))))

(define-syntax-rule (lua.length valv)
    (let ((val valv))
        (let ((metalen (lua-get-meta val "__length")))
            (if (void? metalen)
                (binary-length val 0)
                (lua.call metalen val)))))

(define-syntax lua.car
    (syntax-rules ()
        ((lua.car (lua.list arg)) arg)
        ((lua.car arg) (lua.fcar arg))))

(define-syntax lua.list
    (syntax-rules ()
        ((lua.list v) v)
        ((lua.list vs ...) (list vs ...))))

(define (lua.tail val)
    (if (list? val)
        val
        (list val)))

(define (lua.call func . args)
    (if (procedure? func)
        (apply func args)
        (apply lua.call (lua-get-meta func "__call") func args)))

(define (lua.fcar val)
    (if (pair? val)
        (car val)
        val))

(define (lua.cdr val)
    (if (pair? val)
        (cdr val)
        null))

(define (lua.gt lhs rhs)
    (not (lua.le lhs rhs)))
(define (lua.ge lhs rhs)
    (not (lua.lt lhs rhs)))
(define (lua.ne lhs rhs)
    (not (lua.eq lhs rhs)))

(define (lua-get-meta tab name)
    (if (hash? tab)
        (let ((meta (lua-type-table-meta tab)))
            (if (lua-type-nil? meta)
                lua.nil
                (lua.index meta (lua-type-string name))))
        (error "not implemented: value metatables")))

(define (lua.table (value null))
    (lua-type-table (make-hash value)))

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
                        (lua.tostring arg)))
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
                                (table->list arg)))))))))
        (cons "string"
            (lua.table
                (list
                    (cons "byte"
                        (lua-type-function (lambda (str . nils)
                            (lua-type-number (char->integer (car (string->list (lua.tostring str))))))))
                    (cons "sub"
                        (lua-type-function (lambda (str start (end lua.nil) . nils)
                            (set! start (lua.tonumber start))
                            (set! end (lua.tonumber end))
                            (set! str (lua.tostring str))
                            (lua-type-string
                                (substring str
                                    (- start 1)
                                    (if (lua-type-nil? end)
                                        (string-length str)
                                        end))))))
                    (cons "len"
                        (lua-type-function (lambda (str . nils)
                            (lua-type-number (string-length (lua.export (lua.tostring str))))))))))
        (cons "io"
            (lua.table
                (list
                    (cons "dump"
                        (lua-type-function (lambda (filename data . nils)
                            (display-to-file (lua.export (lua.tostring data)) (lua.export filename) #:exists 'replace)
                            (lua.values (list)))))
                    (cons "slurp"
                        (lua-type-function (lambda (name . nils)
                            (lua-type-string (file->string (lua.export name))))))))))))
(lua.setindex! _ENV (lua-type-string "_G") _ENV)
(define local-_ENV _ENV)

#lang racket/base

(require racket/set)
(require racket/list)
(require racket/format)

(define (zip lis)
    (map cons (range (length lis)) lis))

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
        "#" 'length-lua
        "+" '+
        "-" '-
        "_" '_))

(define (compile-operator-binary expr)
    (hash-ref operator-binary-map expr))

(define (compile-operator-unary expr)
    (hash-ref operator-unary-map expr))

(define (compile-string expr)
    (cadr expr))

(define (compile-number expr)
    (string->number (cadr expr)))

(define (compile-raw-name expr)
    (string->symbol (cadr expr)))

(define (compile-name expr)
    (define str (cadr expr))
    (cond
        ((equal? str "nil") #`nil)
        ((equal? str "true") #t)
        ((equal? str "false") #f)
        (#t (string->symbol str))))

(define (call->value stx)
    #`(let ((ret #,stx))
        (cond
            ((not (list? ret)) ret)
            ((null? ret) nil)
            (else (car ret)))))

(define (final-call? expr)
    (or
        (and
            (equal? (car expr) 'list-part)
            (final-call? (cadr expr)))
        (equal? (car expr) 'self-call)
        (equal? (car expr) 'varargs-get)
        (equal? (car expr) 'call)))

(define (comp-call-actual fun args)
    (define rev (reverse args))
    (if (and (not (empty? rev)) (final-call? (car rev)))
        #`(let
            ((res
                (apply call #,(compile fun)
                    #,@(map
                        (lambda (arg)
                            (call->value (compile arg)))
                        (reverse (cdr rev)))
                    #,(comp-call (car rev)))))
            (if (list? res) res (list res)))
        #`(let
            ((res
                (call #,(compile fun)
                    #,@(map
                        (lambda (arg)
                            (compile arg))
                        args))))
            (if (list? res) res (list res)))))

(define (comp-call expr)
    (cond
        ((equal? (car expr) 'call)
            (comp-call-actual
                (cadr expr)
                (cddr expr)))
        ((equal? (car expr) 'self-call)
            (comp-self-call expr))
        ((equal? (car expr) 'varargs-get)
            #`varargs)
        (#t (compile expr))))

(define (compile-call expr)
    (call->value (comp-call expr)))

(define (comp-self-call expr)
    (define rev (reverse (cdddr expr)))
    #`(let
        ((self-fun #,(compile (cadr expr)))
        (to-get #,(compile (caddr expr))))
        #,(if (and (not (empty? rev)) (final-call? (car rev)))
            #`(let
                ((res
                    (apply call (hash-ref self-fun to-get nil) self-fun
                        #,@(map
                            (lambda (arg)
                                (call->value (compile arg)))
                            (reverse (cdr rev)))
                        #,(comp-call (car rev)))))
                (if (list? res) res (list res)))
            #`(let
                ((res
                    (call (hash-ref self-fun to-get nil) self-fun
                        #,@(map
                            (lambda (arg)
                                (compile arg))
                            (cdddr expr)))))
                (if (list? res) res (list res))))))

(define (compile-self-call expr)
    (call->value (comp-self-call expr)))

(define (find-locals expr)
    (map
        (lambda (s)
            (if (string? s)
                (string->symbol s)
                s))
        (foldl
            (lambda (x y)
                (if (list? x)
                    (let
                        ((names (cadr x)))
                        (append names y))
                    (cons x y)))
            (list)
            (filter
                (lambda (sub)
                    (or
                        (equal? 'local1 (car sub))
                        (equal? 'local (car sub))))
                (cdr expr)))))

(define (compile-basic-block stmt done)
    #`(lambda () stmt #,(done)))

(define (compile-op-binary expr)
    (if (empty? (cddr expr))
        (compile (cadr expr))
        #`(#,(compile-operator-binary (cadr (caddr expr)))
            #,(compile (cadr expr))
            #,(compile (cons 'op-binary (cdddr expr))))))

(define (compile-op-unary expr)
    #`(#,(compile-operator-unary (cadr expr))
        #,(compile (caddr expr))))

(define fnames (list))

(define last-block (list))
(define block-list (list))
(define func-names (list))

(define (block-pushv block)
    (set! block-list (cons block block-list))
    block)

(define (block-push)
    (define id (length block-list))
    (block-pushv
        (make-hash
            (list
                (cons 'id id)
                (cons 'call (string->symbol (string-append "block-" (number->string id))))
                (cons 'code (list))))))

(define (compile-lambda expr)
    (define last-block-list block-list)
    (define last-last-block last-block)
    (define last-func-names func-names)
    (define last-fnames fnames)
    (set! fnames (list))
    (set! block-list (list))
    (set! last-block (list))
    (compile (caddr expr))
    (define local-names fnames)
    (set! fnames last-fnames)
    (set! func-names last-func-names)
    (set! last-block last-last-block)
    (define forward-block-list (reverse block-list))
    (define set-args
        (map
            (lambda (name)
                (if (equal? (car name) 'varargs)
                    (begin
                        ;;; (set! fnames (cons 'varargs fnames))
                        #`(begin
                            (set! varargs args)))
                    #`(define #,(string->symbol (cadr name))
                        (if (null? args)
                            nil
                            (begin0
                                (car args)
                                (set! args (cdr args)))))))
            (cadr expr)))
    (define define-locals
        (map
            (lambda (n)
                #`(define #,n null))
            local-names))  
    (define define-blocks
        (map
            (lambda (block)
                (define defblock
                    #`(define (#,(hash-ref block 'call))
                        #,@(hash-ref block 'code)
                        #,(hash-ref block 'exit #`(make-return))))
                defblock)
            forward-block-list))
    (define ret
        #`(lambda args
            (define varargs nil)
            #,@set-args
            #,@define-locals
            #,@define-blocks
            (block-0)))
    (set! block-list last-block-list)
    ret)

(define (compile-block-item block item)
    (if (equal? (car item) 'block)
        (compile-block item)
        (let ((comp (compile item)))
            (cond
                ((void? comp)
                    (void))
                ((hash? comp)
                    (error "internal error: compiler flow"))
                (else
                    (hash-set! (car last-block) 'code (append (hash-ref (car last-block) 'code)
                        (list comp))))))))

(define (compile-block expr)
    (define names (set->list (list->set (find-locals expr))))
    (set! fnames (append names fnames))
    (define block (block-push))
    (set! last-block (cons block last-block))
    (map (lambda (stmt) (compile-block-item block stmt)) (cdr expr))
    (define ret (car last-block))
    (set! last-block (cdr last-block))
    block)

(define (compile-if expr)
    (define ent (car last-block))
    (define iftrue (compile (caddr expr)))
    (define after-iftrue (car block-list))
    (define iffalse (compile (cadddr expr)))
    (define after-iffalse (car block-list))
    (define block (block-push))
    (hash-set! ent 'exit
        #`(if (to-boolean #,(compile (cadr expr)))
            (#,(hash-ref iftrue 'call))
            (#,(hash-ref iffalse 'call))))
    (hash-set! after-iftrue 'exit #`(#,(hash-ref block 'call)))
    (hash-set! after-iffalse 'exit #`(#,(hash-ref block 'call)))
    (set! last-block (cons block (cdr last-block)))
    (void))

(define (compile-while expr)
    (define ent (car last-block))
    (define body (compile (caddr expr)))
    (define loopb (block-push))
    (define out-block (block-push))
    (hash-set! body 'exit #`(#,(hash-ref loopb 'call)))
    (hash-set! loopb 'exit
        #`(if (to-boolean #,(compile (cadr expr)))
            (#,(hash-ref body 'call))
            (#,(hash-ref out-block 'call))))
    (hash-set! ent 'exit #`(#,(hash-ref loopb 'call)))
    (set! last-block (cons out-block (cdr last-block)))
    (void))

(define forcount 0)
(define (compile-for expr)
    (set! forcount (+ forcount 1))
    (define for-step
        (string->symbol
            (string-append "for-step-" (number->string forcount))))
    (define for-stop
        (string->symbol
            (string-append "for-stop-" (number->string forcount))))
    (define ent (car last-block))
    (define setup (block-push))
    (define loopb (block-push))
    (define body (compile (caddr (cdddr expr))))
    (define after-body (car block-list))
    (define iter (block-push))
    (define out-block (block-push))
    (define varname (compile (cadr expr)))
    (set! fnames (append (list for-step for-stop varname) fnames))
    (hash-set! setup 'code
        (list
            #`(set! #,varname #,(compile (caddr expr)))
            #`(set! #,for-stop #,(compile (car (cdddr expr))))
            #`(set! #,for-step #,(compile (cadr (cdddr expr))))))
    (hash-set! iter 'code
        (list
            #`(set! #,varname (+ #,varname #,for-step))))
    (hash-set! loopb 'exit
        #`(if (<= #,varname #,for-stop)
            (#,(hash-ref body 'call))
            (#,(hash-ref out-block 'call))))
    (hash-set! after-body 'exit #`(#,(hash-ref iter 'call)))
    (hash-set! iter 'exit #`(#,(hash-ref loopb 'call)))
    (hash-set! setup 'exit #`(#,(hash-ref loopb 'call)))
    (hash-set! ent 'exit #`(#,(hash-ref setup 'call)))
    (set! last-block (cons out-block (cdr last-block)))
    (void))

(define (compile-for-iter expr)
    (set! forcount (+ forcount 1))
    (define for-iterable
        (string->symbol
            (string-append "for-iterable-" (number->string forcount))))
    (define ent (car last-block))
    (define body (compile (cadddr expr)))
    (define setup (block-push))
    (define loopb (block-push))
    (define out-block (block-push))
    (hash-set! body 'exit #`(#,(hash-ref loopb 'call)))
    (define for-gen-iterable (compile (caddr expr)))
    (set! fnames (cons for-iterable fnames))
    (hash-set! setup 'code
        (list
            #`(set! #,for-iterable #,for-gen-iterable)))
    (hash-set! loopb 'exit
        #`(if
            (letrec
                ((set-to-values (#,for-iterable))
                (original-values set-to-values))
                #,@(map
                    (lambda (name)
                        (define symname (string->symbol name))
                        (set! fnames (cons symname fnames))
                        #`(if (null? set-to-values)
                            (set! #,symname nil)
                            (begin
                                (set! #,symname
                                    (car set-to-values))
                                (set! set-to-values (cdr set-to-values)))))
                    (cadr expr))
                (or
                    (null? original-values)
                    (and
                        (null? (cdr original-values))
                        (nil? (car original-values)))))
            (#,(hash-ref out-block 'call))
            (#,(hash-ref body 'call))))
    (hash-set! ent 'exit #`(#,(hash-ref setup 'call)))
    (hash-set! setup 'exit #`(#,(hash-ref loopb 'call)))
    (set! last-block (cons out-block (cdr last-block)))
    (void))

(define (compile-return expr)
    (define ent (car last-block))
    (define rev (reverse (cdr expr)))
    (hash-set! ent 'exit
        (if (and (not (empty? rev)) (final-call? (car rev)))
            #`(apply make-return
                #,@(map compile (reverse (cdr rev)))
                #,(comp-call (car rev)))
            #`(make-return #,@(map compile (cdr expr)))))
    (define block (block-push))
    (set! last-block (cons block (cdr last-block)))
    (void))

(define (compile-global expr)
    #`(hash-ref _G #,(cadr expr) nil))

(define (compile-table expr)
    (define local-count 0)
    (define rev (reverse (cdr expr)))
    (define rest (list))
    (if (and (not (empty? rev)) (final-call? (car rev)))
        #`(let ((table (make-hash)))
            #,@(map
                (lambda (item)
                    (if (equal? (car item) 'list-part)
                        (begin
                            (set! local-count (+ local-count 1))
                            #`(hash-set-lua! table #,local-count #,(compile (cadr item))))
                        #`(hash-set-lua! table #,(compile (cadr item)) #,(compile (caddr item)))))
                (reverse (cdr rev)))
            (hash-extend-list table #,(+ local-count 1) #,(comp-call (cadar rev)))
            table)
        #`(let ((table (make-hash)))
            #,@(map
                (lambda (item)
                    (if (equal? (car item) 'list-part)
                        (begin
                            (set! local-count (+ local-count 1))
                            #`(hash-set-lua! table #,local-count #,(compile (cadr item))))
                        #`(hash-set-lua! table #,(compile (cadr item)) #,(compile (caddr item)))))
                (cdr expr))
            table)))

(define (compile-index expr)
    #`(hash-ref-lua #,(compile (cadr expr)) #,(compile (caddr expr)) nil))

(define (body-values body-expr)
    (define body-rev (reverse body-expr))
    (define body-last (car body-rev))
    (define body-but-last (reverse (cdr body-rev)))
    (if (final-call? body-last)
        #`(apply list
            #,@(map compile body-but-last)
            #,(comp-call body-last))
        #`(list #,@(map compile body-expr))))

(define (compile-local expr)
    #`(let ((set-to-values #,(body-values (caddr expr))))
        #,@(map
                (lambda (name)
                    (define symname (string->symbol name))
                    #`(if (null? set-to-values)
                        (set! #,symname nil)
                        (begin
                            (set! #,symname
                                (car set-to-values))
                            (set! set-to-values (cdr set-to-values)))))
                (cadr expr))))

(define (compile-set expr)
    #`(let ((set-to-values #,(body-values (caddr expr))))
        #,@(map
                (lambda (name)
                    (define how-to-set
                        (cond
                            ((equal? (car name) 'global)
                                (list #`hash-set! #`_G (cadr name)))
                            ((equal? (car name) 'name)
                                (list #`set! (compile name)))
                            ((equal? (car name) 'index)
                                (list #`hash-set!
                                    (compile (cadr name))
                                    (compile (caddr name))))
                            (else (error "internal error: compiler set"))))
                    #`(if (null? set-to-values)
                        (#,@how-to-set nil)
                        (begin
                            (#,@how-to-set
                                (car set-to-values))
                            (set! set-to-values (cdr set-to-values)))))
                (cadr expr))))

(define (compile-set1 expr)
    (define name (cadr expr))
    (define how-to-set
        (cond
            ((equal? (car name) 'global)
                (list #`hash-set! #`_G (cadr name)))
            ((equal? (car name) 'name)
                (list #`set! (compile name)))
            ((equal? (car name) 'index)
                (list #`hash-set!
                    (compile (cadr name))
                    (compile (caddr name))))
            (else (error "internal error: compiler set"))))
    #`(#,@how-to-set #,(compile (caddr expr))))

(define (compile-local1 expr)
    #`(set! #,(compile (cadr expr))
        #,(compile (caddr expr))))

(define (compile-varargs-get expr)
    #`varargs)

(define compile-program compile-block)
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
        ((equal? 'for-range type) (compile-for expr))
        ((equal? 'for-iter type) (compile-for-iter expr))
        ((equal? 'table type) (compile-table expr))
        ((equal? 'index type) (compile-index expr))
        ((equal? 'op-unary type) (compile-op-unary expr))
        ((equal? 'op-binary type) (compile-op-binary expr))
        ((equal? 'set type) (compile-set expr))
        ((equal? 'set1 type) (compile-set1 expr))
        ((equal? 'call type) (compile-call expr))
        ((equal? 'self-call type) (compile-self-call expr))
        ((equal? 'varargs-get type) (compile-varargs-get expr))
        ((equal? 'local type) (compile-local expr))
        ((equal? 'local1 type) (compile-local1 expr))
        ((equal? 'program type) (compile-program expr))
        ((equal? 'block type) (compile-block expr))
        ((equal? 'return type) (compile-return expr))
        (#t (error "internal error: compiler"))))
    ret)

(provide compile)

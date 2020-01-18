#lang racket/base

(require racket/vector)
(require racket/list)

(require data/monad)
(require data/applicative)
(require "./base.rkt")
(require "./combinator.rkt")
(require "./text.rkt")

(define keywords
    (list
        "and" "break" "do" "end" "else" "elseif"
        "end" "for" "function" "if" "in" "local"
        "return" "not" "or" "repeat" "return"
        "until" "then" "until" "while"))

(define locals (list "true" "false" "nil"))

(define (name-defined? name)
    (not
        (empty?
            (filter
                (lambda (local)
                    (equal? name local))
                locals))))

(define (name-okay? name)
    (empty?
        (filter
            (lambda (kw)
                (equal? (list->string name) kw))
            keywords)))

(define-syntax-rule (self/p base)
    (letrec
        ((ret (box void))
        (self (lambda/p (lambda () (unbox ret)))))
        (set-box! ret (lambda/p (lambda () (base self))))
        self))

(define (lambda/p proc)
    (parser
        (lambda (input)
            (parse-input (proc) input))))

(define (first/p . parsers)
    (if (zero? (length (cdr parsers)))
        (car parsers)
        (or/p
            (try/p (car parsers))
            (apply first/p (cdr parsers)))))

(define skip/p
    (many/p
        (first/p
            (do
                (string/p "--")
                (many/p
                    (satisfy/p
                        (lambda (x) (not (equal? x #\newline))))))
            (char/p #\space)
            (char/p #\newline))))

(define ident/p
    (do
        (ident <- (guard/p
            (many+/p
                (or/p
                    letter/p
                    digit/p
                    (char/p #\_)))
            name-okay?))
        (pure (list->string ident))))


(define name/p
    (do
        (str <- ident/p)
        (pure
            (list 
                (if (name-defined? str) 'name 'global)
                    str))))

(define number/p
    (do
        (str <- (many+/p digit/p))
        (pure
            (list 'number (list->string str)))))

(define dstring/p
    (do
        (char/p #\")
        (str <- (many/p
            (satisfy/p
                (lambda (arg)
                    (not (equal? arg #\"))))))
        (char/p #\")
        (pure
            (list 'string (list->string str)))))

(define (bin-expr/p lhs/p rhs/p . parsers)
    (do
        (ret <- (self/p
            (lambda (self)
                (do
                    (lhs <- lhs/p)
                    (rest <- (first/p
                        (do
                            skip/p
                            (op <- (apply first/p parsers))
                            skip/p
                            (rhs <-
                                (if (equal? 'self rhs/p)
                                    self
                                    rhs/p))
                            (pure (cons op rhs)))
                        void/p))
                    (pure
                        (if (void? rest)
                            lhs
                            (let
                                ((op (car rest))
                                (rhs (cdr rest)))
                                (vector-append
                                    (vector lhs)
                                    (if (vector? rhs)
                                        (vector-append (vector (list 'operator op)) rhs)
                                        (vector (list 'operator op) rhs))))))))))
        (pure
            (if (list? ret) ret
                (cons 'op-binary (vector->list ret))))))

(define (prefix-expr/p default/p rhs/p . parsers)
    (self/p
        (lambda (self)
            (first/p
                (do
                    (op <- (apply first/p parsers))
                    skip/p
                    (rhs <-
                        (if (equal? 'self rhs/p)
                            self
                            rhs/p))
                    (pure
                        (list 'op-unary op rhs)))
                default/p))))

(define name-args/p
    (many/p
        (do
            (ret <- name/p)
            skip/p
            (first/p
                (string/p ",")
                void/p)
            skip/p
            (pure
                (begin
                    (set! locals (cons (cadr ret) locals))
                    (list 'name (cadr ret)))))))

(define call-args/p
    (do
        (define count 0)
        (ret <-
            (many/p
                (do
                    skip/p
                    (lambda/p
                        (lambda ()
                            (if (equal? count 0)
                                skip/p
                                (do
                                    skip/p
                                    (string/p ",")
                                    skip/p
                                    (pure (void))))))
                    skip/p
                    (ret <- expr/p)
                    skip/p
                    (pure
                        (begin
                            (set! count (+ count 1))
                            ret)))))
        (pure ret)))

(define parens/p
    (do
        (string/p "(")
        skip/p
        (value <- expr/p)
        skip/p
        (string/p ")")
        (pure value)))

(define table/p
    (do
        (string/p "{")
        skip/p
        (places <- (many/p
            (first/p
                (do
                    (key <- (first/p
                        (do
                            (ident <- ident/p)
                            (pure (list 'string ident)))
                        (do
                            (string/p "[")
                            skip/p
                            (ret <- expr/p)
                            skip/p
                            (string/p "]")
                            (pure ret))))
                    skip/p
                    (string/p "=")
                    skip/p
                    (value <- expr/p)
                    (pure (list 'record-part key value)))
                (do
                    (ex <- expr/p)
                    (pure (list 'list-part ex))))
            #:sep (do
                skip/p
                (first/p
                    (string/p ",")
                    (string/p ";"))
                skip/p
                (pure (void)))))
        skip/p
        (string/p "}")
        (pure (cons 'table places))))

(define lambda-expr/p
    (do
        (string/p "function")
        (define before locals)
        skip/p
        (string/p "(")
        skip/p
        (args <- name-args/p)
        skip/p
        (string/p ")")
        skip/p
        (block <- block-body/p)
        skip/p
        (string/p "end")
        (pure
            (begin
                (set! locals before)
                (list 'lambda args block)))))
    
(define if-stmt/p
    (do
        (string/p "if")
        skip/p
        (test <- expr/p)
        skip/p
        (string/p "then")
        skip/p
        (define before locals)
        (iftrue <- block-body/p)
        (do void/p (pure (set! locals before)))
        skip/p
        (elseif <-
            (first/p
                (do
                    (string/p "else")
                    skip/p
                    (define before locals)
                    (ret <- block-body/p)
                    (do void/p (pure (set! locals before)))
                    skip/p
                    (string/p "end")
                    skip/p
                    (pure ret))
                (do
                    (string/p "end")
                    (pure (void)))))
        skip/p
        (pure (list 'if test
            iftrue
            (if (void? elseif)
                (list 'name "nil")
                elseif)))))

(define while-stmt/p
    (do
        (string/p "while")
        skip/p
        (test <- expr/p)
        skip/p
        (string/p "do")
        skip/p
        (whiletrue <- block-body/p)
        skip/p
        (string/p "end")
        skip/p
        (pure (list 'while test whiletrue))))

(define for-stmt/p
    (do
        (string/p "for")
        skip/p
        (ivar <- ident/p)
        skip/p
        (string/p "=")
        skip/p
        (ival <- expr/p)
        skip/p
        (string/p ",")
        (test <- expr/p)
                skip/p
        (step <- (first/p
            (do
                (string/p ",")
                skip/p
                (ret <- expr/p)
                skip/p
                (pure ret))))
        skip/p
        (string/p "do")
        skip/p
        (forbody <- block-body/p)
        skip/p
        (string/p "end")
        skip/p
        (pure
            (list 'for ivar ival
                (if (void? step) 
                    (list 'number "1")
                    step)
                forbody))))

(define single/p
    (first/p lambda-expr/p number/p dstring/p parens/p table/p name/p))

(define (post-exprs/p first-arg/p many-arg/p)
    (do
        (first <- first-arg/p)
        (define ret first)
        skip/p
        (many-arg/p
            (do
                skip/p
                (first/p
                    (do
                        (string/p ":")
                        skip/p
                        (index <- name/p)
                        skip/p
                        (string/p "(")
                        skip/p
                        (args <- call-args/p)
                        skip/p
                        (string/p ")")
                        (pure (set! ret (append (list 'self-call ret (list 'string (cadr index))) args))))
                    (do
                        (string/p ".")
                        skip/p
                        (index <- ident/p)
                        (pure (set! ret (list 'index ret (list 'string index)))))
                    (do
                        (string/p "[")
                        skip/p
                        (index <- expr/p)
                        skip/p
                        (string/p "]")
                        skip/p
                        (pure (set! ret (list 'index ret index))))
                    (do
                        (string/p "(")
                        skip/p
                        (define count 0)
                        (args <-
                            (many/p
                                (do
                                    skip/p
                                    (lambda/p
                                        (lambda ()
                                            (if (equal? count 0)
                                                skip/p
                                                (do
                                                    skip/p
                                                    (string/p ",")
                                                    skip/p
                                                    (pure (void))))))
                                    skip/p
                                    (ret <- expr/p)
                                    skip/p
                                    (pure
                                        (begin
                                            (set! count (+ count 1))
                                            ret)))))
                        skip/p
                        (string/p ")")
                        (pure (set! ret (append (list 'call ret) args)))))
                skip/p
                (pure void)))
        (pure ret)))

(define call-expr/p (post-exprs/p name/p many+/p))
(define post-expr/p (post-exprs/p single/p many/p))

(define pre-expr/p
    (prefix-expr/p post-expr/p 'self
        (string/p "not")
        (string/p "-")
        (string/p "+")))

(define pow-expr/p
    (bin-expr/p pre-expr/p 'self
        (string/p "**")))

(define mul-expr/p
    (bin-expr/p pow-expr/p 'self
        (string/p "%")
        (string/p "*")
        (string/p "/")))

(define add-expr/p
    (bin-expr/p mul-expr/p 'self
        (string/p "~")
        (string/p "+")
        (string/p "-")))

(define cmp-expr/p
    (bin-expr/p add-expr/p 'self
        (string/p "~=")
        (string/p "==")
        (string/p ">=")
        (string/p "<=")
        (string/p ">")
        (string/p "<")))

(define logic-expr/p
    (bin-expr/p cmp-expr/p 'self
        (string/p "and")
        (string/p "or")))

(define expr/p logic-expr/p)

(define assign-stmt/p
    (do
        (name <- name/p)
        skip/p
        (string/p "=")
        skip/p
        (expr <- expr/p)
        (pure (list 'set name expr))))
    
(define index-assign-stmt/p
    (do
        (fst <- call-expr/p)
        skip/p
        skip/p
        (string/p "=")
        skip/p
        (expr <- expr/p)
        (pure (list 'set-index (cadr fst) (caddr fst) expr))))

(define local-stmt/p
    (do
        (string/p "local")
        skip/p
        (ident <- name/p)
        skip/p
        (string/p "=")
        skip/p
        (expr <- expr/p)
        (pure
            (begin
                (set! locals (cons (cadr ident) locals))
                (list 'local (list 'name (cadr ident)) expr)))))

(define local-function-stmt/p
    (do
        (string/p "local")
        skip/p
        (string/p "function")
        skip/p
        (ident <- name/p)
        (define before locals)
        skip/p
        (string/p "(")
        skip/p
        (args <- name-args/p)
        skip/p
        (string/p ")")
        (do void/p (pure (set! locals (cons (cadr ident) locals))))
        skip/p
        (block <- block-body/p)
        skip/p
        (string/p "end")
        (pure
            (begin
                (set! locals (cons (cadr ident) before))
                (list 'local (list 'name (cadr ident)) (list 'lambda args block))))))

(define assign-function-stmt/p
    (do
        (define before locals)
        (string/p "function")
        skip/p
        (ident <- name/p)
        skip/p
        (string/p "(")
        skip/p
        (args <- name-args/p)
        skip/p
        (string/p ")")
        skip/p
        (block <- block-body/p)
        skip/p
        (string/p "end")
        (pure
            (begin
                (set! locals before)
                (list 'set (list 'global (cadr ident)) (list 'lambda args block))))))

(define table-function-stmt/p
    (do
        (string/p "function")
        skip/p
        (first <- name/p)
        skip/p
        (define before locals)
        (kind <- (first/p (string/p ":") (string/p ".")))
        skip/p
        (index <- ident/p)
        skip/p
        (string/p "(")
        skip/p
        (args <- name-args/p)
        skip/p
        (string/p ")")
        skip/p
        (block <- block-body/p)
        skip/p
        (string/p "end")
        (pure
            (begin
                (set! locals before)
                (list 'set-index first (list 'string index)
                    (list 'lambda
                        (if (equal? kind ":")
                            (cons (list 'name "self") args)
                            args)
                        block))))))

(define return-stmt/p
    (do
        (string/p "return")
        skip/p
        (expr <-
            (first/p
                (many/p #:sep (do skip/p (string/p ",") skip/p) expr/p)
                void/p))
        (pure (cons 'return
            (if (void? expr)
                (list)
                expr)))))

(define expr-stmt/p call-expr/p)

(define stmt/p
    (do
        (define before locals)
        (ret <- (first/p
            local-function-stmt/p
            assign-function-stmt/p
            table-function-stmt/p
            assign-stmt/p
            index-assign-stmt/p
            expr-stmt/p
            if-stmt/p
            while-stmt/p
            for-stmt/p
            local-stmt/p))
        (pure ret)))

(define block-body/p
    (do
        (stmts <- (many/p
            (do
                (ret <- stmt/p)
                skip/p
                (pure ret))))
        (retv <- (first/p return-stmt/p void/p))
        (pure
            (begin
                (if (void? retv)
                    (cons 'block stmts)
                    (list 'return-after (cons 'block stmts) retv))))))

(define all/p
    (do
        skip/p
        (ret <- block-body/p)
        skip/p
        eof/p
        (pure (cons 'program (cdr ret)))))

(define (parse-text text) 
    (parse-result!
        (parse-string all/p text)))

(define (parse-text-expr text) 
    (parse-result!
        (parse-string expr/p text)))

(provide parse-text)

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
        "end" "for" "if" "function" "in" "local"
        "return" "not" "or" "repeat" "return"
        "until" "then" "until" "while"))

(define init-locals
    (list
        (list 2 "true" "true")
        (list 1 "false" "false")
        (list 0 "nil" "nil")))

(define init-next (length init-locals))

(define locals init-locals)
(define next init-next)

(define (add-local name-arg)
    (define name
        (if (string? name-arg)
            name-arg
                (cadr name-arg)))
    (set! locals
        (cons
            (list
                next
                (string-append "local-" (number->string next))
                name)
            locals))
    (set! next (+ next 1))
    (find-name name))

(define scopes (list))

(define (scope)
    (set! scopes (cons locals scopes))
    (void))

(define (unscope)
    (set! locals (car scopes))
    (set! scopes (cdr scopes)))

(define (name-defined? name)
    (not
        (empty?
            (filter
                (lambda (local)
                    (equal? name (caddr local)))
                locals))))

(define (name-okay? name)
    (and
        (not (char-numeric? (car name)))
        (empty?
            (filter
                (lambda (kw)
                    (equal? (list->string name) kw))
                keywords))))

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

(define (find-name arg)
    (define str (if (list? arg) (cadr arg) arg))
    (cadar
        (filter
            (lambda (local)
                (equal? str (caddr local)))
            locals)))

(define name/p
    (do
        (str <- ident/p)
        (pure
            (if (name-defined? str)
                (list 'name (find-name str))
                (list 'global str)))))

(define (comma-sep/p par)
    (many+/p
        #:sep (string/p ",")
        (do
            skip/p
            (ident <- par)
            skip/p
            (pure ident))))

(define idents/p (comma-sep/p ident/p))
(define names/p (comma-sep/p name/p))
(define (exprs/p) (comma-sep/p expr/p))

(define number/p
    (first/p(do
            (str <- (many+/p digit/p))
            (string/p ".")
            (dec <- (many+/p digit/p))
            (string/p "e")
            (pm <- (first/p (string/p "+") (string/p "-")))
            (ex <- (many+/p digit/p))
            (pure
                (list 'number
                    (string-append
                        (list->string str) "."
                        (list->string dec) "e"
                        pm (list->string ex)))))
        (do
            (str <- (many+/p digit/p))
            (string/p ".")
            (dec <- (many+/p digit/p))
            (pure
                (list 'number (string-append (list->string str) "." (list->string dec)))))
        (do
            (str <- (many+/p digit/p))
            (pure
                (list 'number (list->string str))))))

(define dstring/p
    (first/p
        (do
            (char/p #\')
            (str <- (many/p
                (satisfy/p
                    (lambda (arg)
                        (not (equal? arg #\'))))))
            (char/p #\')
            (pure
                (list 'string (list->string str))))
        (do
            (char/p #\")
            (str <- (many/p
                (satisfy/p
                    (lambda (arg)
                        (not (equal? arg #\"))))))
            (char/p #\")
            (pure
                (list 'string (list->string str))))))

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
                    (add-local ret)
                    (list 'name (find-name (cadr ret))))))))

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
        (pure (scope))
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
                (unscope)
                (list 'lambda args block)))))
    
(define if-stmt/p
    (do
        (string/p "if")
        skip/p
        (test <- expr/p)
        skip/p
        (string/p "then")
        skip/p
        (pure (scope))
        (iftrue <- block-body/p)
        (pure (unscope))
        skip/p
        (elseif <-
            (first/p
                (do
                    (string/p "else")
                    skip/p
                    (pure (scope))
                    (ret <- block-body/p)
                    (pure (unscope))
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
                (list 'block)
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
        skip/p
        (max <- expr/p)
        skip/p
        (step <- (first/p
            (do
                (string/p ",")
                skip/p
                (ret <- expr/p)
                skip/p
                (pure ret))
            void/p))
        skip/p
        (pure (scope))
        (define iname (add-local ivar))
        (string/p "do")
        skip/p
        (forbody <- block-body/p)
        skip/p
        (string/p "end")
        (pure (unscope))
        (pure
            (list 'for-range (list 'name iname) ival max
                (if (void? step)
                    (list 'number "1")
                    step)
                forbody))))

(define for-iter-stmt/p
    (do
        (string/p "for")
        skip/p
        (names-raw <- (comma-sep/p name/p))
        skip/p
        (string/p "in")
        skip/p
        (func <- expr/p)
        skip/p
        (pure (scope))
        (define names (map add-local names-raw))
        (string/p "do")
        skip/p
        (forbody <- block-body/p)
        skip/p
        (string/p "end")
        (pure (unscope))
        (pure
            (list 'for-iter names func forbody))))

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
                        (index <- ident/p)
                        skip/p
                        (string/p "(")
                        skip/p
                        (args <- call-args/p)
                        skip/p
                        (string/p ")")
                        (pure (set! ret (append (list 'self-call ret (list 'string index)) args))))
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
                        (tab <- table/p)
                        (pure (set! ret (list 'call ret tab))))
                    (do
                        (str <- dstring/p)
                        (pure (set! ret (list 'call ret str))))
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
        (string/p "#")
        (string/p "not")
        (string/p "-")
        (string/p "+")))

(define pow-expr/p
    (bin-expr/p pre-expr/p 'self
        (string/p "^")))

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
        (name <- (comma-sep/p
            (first/p call-expr/p name/p)))
        skip/p
        (string/p "=")
        skip/p
        (expr <- (exprs/p))
        (pure (list 'set name expr))))

(define local-stmt/p
    (do
        (string/p "local")
        skip/p
        (ident <- idents/p)
        skip/p
        (string/p "=")
        skip/p
        (expr <- (exprs/p))
        (pure
            (begin
                (map add-local ident)
                (list 'local (map find-name ident) expr)))))

(define local-function-stmt/p
    (do
        (string/p "local")
        skip/p
        (string/p "function")
        skip/p
        (ident <- name/p)
        (pure (add-local ident))
        (pure (scope))
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
                (pure (unscope))
                (list 'local1 (list 'name (find-name (cadr ident))) (list 'lambda args block))))))

(define assign-function-stmt/p
    (do
        (pure (scope))
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
                (pure (unscope))
                (list 'set1 (list 'global (cadr ident)) (list 'lambda args block))))))

(define table-function-stmt/p
    (do
        (string/p "function")
        skip/p
        (first <- ident/p)
        skip/p
        (pure (scope))
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
                (pure (unscope))
                (list 'set-index1 first (list 'string index)
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
        (ret <- (first/p
            local-function-stmt/p
            local-stmt/p
            assign-function-stmt/p
            assign-stmt/p
            ;;; table-function-stmt/p
            if-stmt/p
            while-stmt/p
            expr-stmt/p
            for-stmt/p
            for-iter-stmt/p))
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
                    (append (cons 'block stmts) (list retv)))))))

(define (reset-parser)
    (set! next init-next)
    (set! locals init-locals))

(define all/p
    (do
        skip/p
        (ret <- block-body/p)
        skip/p
        eof/p
        (pure (list 'call (list 'lambda (list) ret)))))

(define (parse-text text)
    (reset-parser)
    (parse-result!
        (parse-string all/p text)))

(define (parse-stmt-or-expr text) 
    (reset-parser)
    (parse-result!
        (parse-string
            (first/p
                (do
                    (result <- expr/p)
                    eof/p
                    (pure result))
                all/p)
            text)))

(define (parse-complete text (last (make-hash)))
    (reset-parser)
    (parse-string (first/p all/p expr/p) text)
    (append
        (hash-keys last)
        (map caddr (cdddr (reverse locals)))))

(provide parse-text parse-stmt-or-expr parse-complete)

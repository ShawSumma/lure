(define lua.meta (gensym))
(define lua.nometa (gensym))
(define lua.lencache (gensym))

(define lua.nil '())
(define lua.nil? null?)

(define lua.nil1 (list lua.nil))

(define (file->string name)
    (define lis '())
    (call-with-input-file name
        (lambda (port)
            (define (loop x)
                (cond
                    ((not (eof-object? x))
                        (begin
                            (set! lis (cons x lis))
                            (loop (read-char port))))))
            (loop (read-char port))))
    (list->string (reverse lis)))

(define (lua.newtable . args)
    (define ret (make-eq-hashtable))
    (for-each
        (lambda (value-pairs)
            (for-each
                (lambda (value-pair)
                    (lua.setindex! ret (car value-pair) (cdr value-pair)))
                value-pairs))
        args)
    (binary-length ret 0)
    ret)

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua.nil? (hashtable-ref tab index lua.nil))
        (begin
            (hashtable-set! tab lua.lencache low)
            low)
        (binary-length tab index)))

(define (get-meta tab)
    (hashtable-ref tab lua.meta lua.nil))

(define (meta-get tab name)
    (define meta (get-meta tab))
    (if (hashtable? meta)
        (lua.index meta name)
        lua.nil))

(define (lua.index tab keys)
    (define key (if (string? keys) (string->symbol keys) keys))
    (define value (hashtable-ref tab key lua.nil))
    (if (lua.nil? value)
        (let ((val (meta-get tab "__index")))
            (if (procedure? val)
                (car (val tab key))
                lua.nil))
        value))

(define (lua.setindex! tab keys value)
    (define key (if (string? keys) (string->symbol keys) keys))
    (define last-len (hashtable-ref tab lua.lencache 0))
    (hashtable-set! tab key value)
    (cond
        ((and (number? key) (<= (- 1 key) last-len))
            (binary-length tab (- key 1)))
        ((and (number? key) (lua.nil? value) (>= key last-len))
            (binary-length tab (- key 1)))))

(define (lua.toboolean val)
    (and val (not (eq? val '()))))

(define (lua.tonumber val)
    (cond
        ((string? val) (string->number val))
        ((number? val) val)))

(define (meta-op op name)
    (lambda (lhs rhs)
        (if (hashtable? lhs)
            (let ((got (meta-get lhs name)))
                (if (lua.nil? got)
                    (op lhs rhs)
                    (car (got lhs rhs))))
            (op lhs rhs))))

(define (number-op fun)
    (lambda (lhs rhs)
        (fun (lua.tonumber lhs) (lua.tonumber rhs))))

(define lua.+ (meta-op (number-op +) "__add"))
(define lua.- (meta-op (number-op -) "__sub"))
(define lua.* (meta-op (number-op *) "__mul"))
(define lua.^ (meta-op (number-op expt) "__pow"))
(define lua./ (meta-op (number-op /) "__div"))
(define lua.% (meta-op (number-op modulo) "__mod"))
(define (lua.concat lhs rhs)
    (if (hashtable? lhs)
        (let ((got (meta-get lhs "__concat")))
            (if (lua.nil? got)
                (string-append lhs rhs)
                (got lhs rhs)))
        (string-append (lua.tostring lhs) (lua.tostring rhs))))

(define lua.< <)
(define lua.<= <=)
(define lua.== equal?)

(define (lua.> lhs rhs) (not (lua.<= lhs rhs)))
(define (lua.>= lhs rhs) (not (lua.< lhs rhs)))
(define (lua.~= lhs rhs)
    (not (lua.== lhs rhs)))

(define (binary-length tab low)
    (define index (+ low 1))
    (if (lua.nil? (hashtable-ref tab index lua.nil))
        (begin
            (hashtable-set! tab lua.lencache low)
            low)
        (binary-length tab index)))

(define (lua.length tab)
    (hashtable-ref tab lua.lencache 0))

(define (lua.method tab name args)
    (define func (lua.index tab name))
    (apply func tab args))

(define (table->list table . args)
    (define from (if (pair? args) (car args) 1))
    (define val (lua.index table from))
    (if (lua.nil? val)
        '()
        (cons val
            (table->list table (+ 1 from)))))

(define (list->table lis index table)
    (if (null? lis)
        (begin
            (hashtable-set! table lua.lencache (- index 1))
            table)
        (begin
            (hashtable-set! table index (car lis))
            (list->table (cdr lis) (+ 1 index) table))))

(define (lua.tostring obj)
    (cond
        ((lua.nil? obj) "nil")
        ((equal? obj #t) "true")
        ((equal? obj #f) "false")
        ((procedure? obj) "<function>")
        ((string? obj) obj)
        ((number? obj) (number->string obj))
        ((hashtable? obj)
            (let ((fun (meta-get obj "__tostring")))
                (if (lua.nil? fun)
                    "<table>"
                    (car (fun obj)))))
        (#t "<userdata>")))

(define arg (command-line))

(define _ENV (lua.newtable
    (list
        (cons "setmetatable"
            (lambda (tab meta)
                (hashtable-set! tab lua.meta meta)
                (list tab)))
        (cons "arg"
            (list->table arg 0 (lua.newtable)))
        (cons "assert"
            (lambda (thing . args)
                (define err (if (pair? args) (car args) lua.nil))
                (cond
                    ((not (lua.toboolean thing))
                        (begin
                            (display "error: ")
                            (display (lua.tostring err))
                            (newline))))
                (list thing)))
        (cons "tonumber"
            (lambda args
                (define arg (if (pair? args) (car args) lua.nil))
                (list (lua.tonumber arg))))
        (cons "tostring"
            (lambda (arg)
                (list (lua.tostring arg))))
        (cons "type"
            (lambda (arg)
                (list
                    (cond
                        ((lua.nil? arg) "nil")
                        ((boolean? arg) "boolean")
                        ((procedure? arg) "function")
                        ((string? arg) "string")
                        ((number? arg) "number")
                        ((hashtable? arg) "table")
                        (#t "userdata")))))
        (cons "print"
            (lambda args
                (define first #f)
                (for-each
                    (lambda (arg)
                        (if first
                            (display #\tab)
                            (set! first #t))
                        (display
                            (if (string? arg)
                                arg
                                (lua.tostring arg))))
                    args)
                (newline)
                lua.nil1))
        (cons "table"
            (lua.newtable
                (list
                    (cons "unpack"
                        (lambda (tab) (table->list tab)))
                    (cons "concat"
                        (lambda (arg)
                            (list
                                (apply string-append (table->list arg))))))))
        (cons "string"
            (lua.newtable
                (list
                    (cons "byte"
                        (lambda (str)
                            (list (char->integer (car (string->list str))))))
                    (cons "sub"
                        (lambda (str start . rest)
                            (define end (if (pair? rest) (car rest) lua.nil))
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
            (lua.newtable
                (list
                    (cons "dump"
                        (lambda (filename data . nils)
                            (define out (open-output-file filename 'replace))
                            (display data out)
                            (close-port out)
                            lua.nil1))
                    (cons "slurp"
                        (lambda (name . nils)
                            (list (file->string name))))))))))
(lua.setindex! _ENV "_G" _ENV)
(define local-_ENV _ENV)
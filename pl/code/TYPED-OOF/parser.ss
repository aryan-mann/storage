#!r7rs

;;; A parser for the TYPED-OOF language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 29, 2009
;;; last revised August 2, 2019

;;; This library provides a parser for the TYPED-OOF language
;;; developed by Daniel P. Friedman and Mitchell Wand
;;; in section 9.5 of their book
;;; _Essentials of programming languages_ (third edition).

(define-library (TYPED-OOF parser)
  (export parse-program parse-class-declaration parse-method-declaration
          parse-expression scan&parse)
  (import (scheme base)
          (srfi receive)
          (utilities eopl)
          (utilities character-sources)
          (TYPED-OOF scanner)
          (TYPED-OOF tokens)
          (TYPED-OOF syntax-trees))
  (begin

    ;; The acquire procedure recovers a token
    ;; from a given source,
    ;; signalling an error if none is available.

    ;; acquire-token : Token-source -> Token

    (define acquire-token
      (lambda (token-source)
        (when (token-source 'at-end?)
          (report-unexpected-end-of-source-error))
        (token-source 'get)))

    (define report-unexpected-end-of-source-error
      (lambda ()
        (eopl:error 'acquire-token
                    "The end of the input was encountered unexpectedly.")))

    ;; The match-and-discard procedure
    ;; gets a token from a given source
    ;; and compares its species with the species of the token
    ;; that the parser expects to find.
    ;; If they don't match, an error is reported.

    ;; match-and-discard : Token-source * Token -> ()

    (define match-and-discard
      (lambda (token-source expected)
        (let ((discard (acquire-token token-source)))
          (unless (eq? expected (species discard))
            (report-unexpected-token-error discard expected)))))

    (define report-unexpected-token-error
      (lambda (found expected)
        (eopl:error 'match-and-discard
                    "The token ~a does not match the expected token (~a).~%"
                    found
                    expected)))

    ;; There is a separate parsing procedure
    ;; for each kind of internal node of the syntax tree.
    ;; Interface declarations are parsed separately
    ;; from other class declarations.

    ;; parse-program : Token-source -> Program

    (define parse-program
      (lambda (token-source)

        ;; <program> ::= {<class-declaration>}* <expression>

        (let loop ((reversed-class-declarations '()))
          (cases token (token-source 'peek)
            (class-token ()
              (token-source 'get)
              (loop (cons (parse-class-declaration token-source)
                          reversed-class-declarations)))
            (interface-token ()
              (token-source 'get)
              (loop (cons (parse-interface-declaration token-source)
                          reversed-class-declarations)))
            (else
              (a-program (reverse reversed-class-declarations)
                         (parse-expression token-source)))))))

    ;; parse-class-declaration : Token-source -> ClassDeclaration

    (define parse-class-declaration
      (lambda (token-source)

        ;; <class-declaration> ::= class <identifier> extends <identifier>
        ;;                           {implements <identifier}*
        ;;                           {field <type> <identifier>}*
        ;;                           {<method-declaration>}*

        (let ((class-name (acquire-identifier token-source)))
          (match-and-discard token-source 'extends-token)
          (let ((super-name (acquire-identifier token-source)))
            (let implements-loop ((reversed-interfaces '()))
              (if (eq? (species (token-source 'peek)) 'implements-token)
                  (begin
                    (token-source 'get)
                    (implements-loop (cons (acquire-identifier token-source)
                                           reversed-interfaces)))
                  (let field-loop ((reversed-field-types '())
                                   (reversed-fields '()))
                    (if (equal? (token-source 'peek) (field-token))
                        (begin
                          (token-source 'get)
                          (let* ((field-type (parse-type token-source))
                                 (field-name (acquire-identifier token-source))) 
                            (field-loop (cons field-type reversed-field-types)
                                        (cons field-name reversed-fields))))
                        (let method-loop ((reversed-method-declarations '()))
                          (if (equal? (token-source 'peek) (method-token))
                              (begin
                                (token-source 'get)
                                (method-loop
                                  (cons (parse-method-declaration token-source)
                                        reversed-method-declarations)))
                              (a-class-declaration
                               class-name
                               super-name
                               (reverse reversed-interfaces)
                               (reverse reversed-field-types)
                               (reverse reversed-fields)
                               (reverse reversed-method-declarations))))))))))))

    
    ;; acquire-identifier : Token-source -> Sym

    (define acquire-identifier
      (lambda (token-source)
        (let ((candidate (acquire-token token-source)))
          (cases token candidate
            (numeral-token (num)
              (report-acquire-identifier-error "A numeral"))
            (minus-sign ()
              (report-acquire-identifier-error "A minus sign"))
            (open-parenthesis ()
              (report-acquire-identifier-error "An open parenthesis"))
            (comma ()
              (report-acquire-identifier-error "A comma"))
            (close-parenthesis ()
              (report-acquire-identifier-error "A close parenthesis"))
            (zero?-token ()
              (report-acquire-identifier-error "The keyword zero?"))
            (if-token ()
              (report-acquire-identifier-error "The keyword if"))
            (then-token ()
              (report-acquire-identifier-error "The keyword then"))
            (else-token ()
              (report-acquire-identifier-error "The keyword else"))
            (identifier-token (id) id)
            (let-token ()
              (report-acquire-identifier-error "The keyword let"))
            (equals-sign ()
              (report-acquire-identifier-error "An equals sign"))
            (in-token ()
              (report-acquire-identifier-error "The keyword in"))
            (proc-token ()
              (report-acquire-identifier-error "The keyword proc"))
            (letrec-token ()
              (report-acquire-identifier-error "The keyword letrec"))
            (set-token ()
              (report-acquire-identifier-error "The keyword set"))
            (begin-token ()
              (report-acquire-identifier-error "The keyword begin"))
            (semicolon ()
              (report-acquire-identifier-error "A semicolon"))
            (end-token ()
              (report-acquire-identifier-error "The keyword end"))
            (plus-sign ()
              (report-acquire-identifier-error "A plus sign"))
            (asterisk ()
              (report-acquire-identifier-error "An asterisk"))
            (cons-token ()
              (report-acquire-identifier-error "The keyword cons"))
            (car-token ()
              (report-acquire-identifier-error "The keyword car"))
            (cdr-token ()
              (report-acquire-identifier-error "The keyword cdr"))
            (null?-token ()
              (report-acquire-identifier-error "The keyword null?"))
            (emptylist-token ()
              (report-acquire-identifier-error "The keyword emptylist"))
            (list-token ()
              (report-acquire-identifier-error "The keyword list"))
            (class-token ()
              (report-acquire-identifier-error "The keyword class"))
            (extends-token ()
              (report-acquire-identifier-error "The keyword extends"))
            (field-token ()
              (report-acquire-identifier-error "The keyword field"))
            (method-token ()
              (report-acquire-identifier-error "The keyword method"))
            (new-token ()
              (report-acquire-identifier-error "The keyword new"))
            (send-token ()
              (report-acquire-identifier-error "The keyword send"))
            (super-token ()
              (report-acquire-identifier-error "The keyword super"))
            (self-token ()
              (report-acquire-identifier-error "The keyword self"))
            (int-token ()
              (report-acquire-identifier-error "The keyword int"))
            (bool-token ()
              (report-acquire-identifier-error "The keyword bool"))
            (fieldref-token ()
              (report-acquire-identifier-error "The keyword fieldref"))
            (fieldset-token ()
              (report-acquire-identifier-error "The keyword fieldset"))
            (arrow ()
              (report-acquire-identifier-error "An arrow"))
            (colon ()
              (report-acquire-identifier-error "A colon"))
            (implements-token ()
              (report-acquire-identifier-error "The keyword implements"))
            (interface-token ()
              (report-acquire-identifier-error "The keyword interface"))
            (cast-token ()
              (report-acquire-identifier-error "The keyword cast"))
            (instanceof-token ()
              (report-acquire-identifier-error "The keyword instanceof"))
            (void-token ()
              (report-acquire-identifier-error "The keyword void"))
            (listof-token ()
              (report-acquire-identifier-error "The keyword listof"))
            (underscore ()
              (report-acquire-identifier-error "An underscore"))))))

    ;; report-acquire-identifier-error : String -> ()

    (define report-acquire-identifier-error
      (lambda (bad-token-string)
        (eopl:error 'acquire-identifier
                    "~s was found in place of an identifier.~%"
                    bad-token-string)))

    ;; parse-type : Token-source -> Type

    (define parse-type
      (lambda (token-source)
        (let ((current (acquire-token token-source)))
          (cases token current
            (numeral-token (value)
              (report-bad-type-expression-error "a numeral"))
            (minus-sign ()
              (report-bad-type-expression-error "a minus sign"))

            ;; <type> ::= ( {<type>}*{*} -> <type> )

            (open-parenthesis ()
              (parse-proc-type token-source))

            (comma ()
              (report-bad-type-expression-error "a comma"))
            (close-parenthesis ()
              (report-bad-type-expression-error "a close parenthesis"))
            (zero?-token ()
              (report-bad-type-expression-error "the keyword zero?"))
            (if-token ()
              (report-bad-type-expression-error "the keyword if"))
            (then-token ()
              (report-bad-type-expression-error "the keyword then"))
            (else-token ()
              (report-bad-type-expression-error "the keyword else"))

            ;; <type> ::= <identifier>

            (identifier-token (id)
              (class-type id))

            (let-token ()
              (report-bad-type-expression-error "the keyword let"))
            (equals-sign ()
              (report-bad-type-expression-error "an equals sign"))
            (in-token ()
              (report-bad-type-expression-error "the keyword in"))
            (proc-token ()
              (report-bad-type-expression-error "the keyword proc"))
            (letrec-token ()
              (report-bad-type-expression-error "the keyword letrec"))
            (set-token ()
              (report-bad-type-expression-error "the keyword set"))
            (begin-token ()
              (report-bad-type-expression-error "the keyword begin"))
            (semicolon ()
              (report-bad-type-expression-error "a semicolon"))
            (end-token ()
              (report-bad-type-expression-error "the keyword end"))
            (plus-sign ()
              (report-bad-type-expression-error "a plus sign"))
            (asterisk ()
              (report-bad-type-expression-error "an asterisk"))
            (cons-token ()
              (report-bad-type-expression-error "the keyword cons"))
            (car-token ()
              (report-bad-type-expression-error "the keyword car"))
            (cdr-token ()
              (report-bad-type-expression-error "the keyword cdr"))
            (null?-token ()
              (report-bad-type-expression-error "the keyword null?"))
            (emptylist-token ()
              (report-bad-type-expression-error "the keyword emptylist"))
            (list-token ()
              (report-bad-type-expression-error "the keyword list"))
            (class-token ()
              (report-bad-type-expression-error "the keyword class"))
            (extends-token ()
              (report-bad-type-expression-error "the keyword extends"))
            (field-token ()
              (report-bad-type-expression-error "the keyword field"))
            (method-token ()
              (report-bad-type-expression-error "the keyword method"))
            (new-token ()
              (report-bad-type-expression-error "the keyword new"))
            (send-token ()
              (report-bad-type-expression-error "the keyword send"))
            (super-token ()
              (report-bad-type-expression-error "the keyword super"))
            (self-token ()
              (report-bad-type-expression-error "the keyword self"))

            ;; <type> ::= int

            (int-token ()
              (int-type))

            ;; <type> ::= bool

            (bool-token ()
              (bool-type))

            (arrow ()
              (report-bad-type-expression-error "an arrow"))
            (colon ()
              (report-bad-type-expression-error "a colon"))
            (implements-token ()
              (report-bad-type-expression-error "the keyword implements"))
            (interface-token ()
              (report-bad-type-expression-error "the keyword interface"))
            (cast-token ()
              (report-bad-type-expression-error "the keyword cast"))
            (instanceof-token ()
              (report-bad-type-expression-error "the keyword instanceof"))

            ;; <type> ::= void

            (void-token ()
              (void-type))

            ;; <type> ::= listof <type>

            (listof-token ()
              (list-type (parse-type token-source)))

            (underscore ()
              (report-bad-type-expression-error "an underscore"))))))

    (define report-bad-type-expression-error
      (lambda (bad-token-description)
        (eopl:error 'parse-type
                    "A type expression may not begin with %s.%~"
                    bad-token-description)))

    ;; parse-proc-type : Token-source -> ProcType

    (define parse-proc-type
      (lambda (token-source)
        (let ((arg-types
                (if (equal? (token-source 'peek) (arrow))
                    '()
                    (let loop ((reversed-arg-types
                                 (list (parse-type token-source))))
                      (if (equal? (token-source 'peek) (arrow))
                          (reverse reversed-arg-types)
                          (begin
                            (match-and-discard token-source 'asterisk)
                            (loop (cons (parse-type token-source)
                                        reversed-arg-types))))))))
          (token-source 'get)
          (let ((result-type (parse-type token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (proc-type arg-types result-type)))))

    ;; parse-method-declaration : Token-source -> MethodDeclaration

    (define parse-method-declaration
      (lambda (token-source)

        ;; <method-declaration> ::= method <type> <identifier>
        ;;                            ( {<identifier> : <type> }*{,} ) <expression>

        (let* ((result-type (parse-type token-source))
               (method-name (acquire-identifier token-source)))
          (receive (parameters parameter-types) (parse-parameter-list token-source)
            (a-method-declaration result-type
                                  method-name
                                  parameters
                                  parameter-types
                                  (parse-expression token-source))))))

    ;; parse-parameter-list : Token-source -> ListOf(Sym) * ListOf(Type)

    (define parse-parameter-list
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (if (eq? (species (token-source 'peek)) 'close-parenthesis)
            (begin
              (token-source 'get)
              (values '() '()))
            (let loop ((reversed-parameters '())
                       (reversed-types '()))
              (let ((parameter (acquire-identifier token-source)))
                (match-and-discard token-source 'colon)
                (let* ((ty (parse-type token-source))
                       (next-token (token-source 'peek)))
                  (if (eq? (species next-token) 'close-parenthesis)
                      (begin
                        (token-source 'get)
                        (values (reverse (cons parameter reversed-parameters))
                                (reverse (cons ty reversed-types))))
                      (begin
                        (match-and-discard token-source 'comma)
                        (loop (cons parameter reversed-parameters)
                              (cons ty reversed-types))))))))))

    ;; parse-expression : Token-source -> Expression

    (define parse-expression
      (lambda (token-source)

        ;; Get a token
        ;; and determine which of the analyses of expressions
        ;; should be used.

        (let ((current (acquire-token token-source)))
          (cases token current

            ;; [ARYAN]

            ;; <expression> ::= fieldref <expression> <identifier>

            (fieldref-token ()
              (parse-fieldref token-source))

            
            ;; <expression> ::= fieldset <expression> <identifier> = <expression>

            (fieldset-token ()
              (parse-fieldset token-source))
            
            
            ;; <expression> ::= <numeral>

            (numeral-token (value)
              (const-exp value))

            ;; <expression> ::= - ( <expression> , <expression> )

            (minus-sign ()
              (parse-diff-exp token-source))

            ;; <expression> ::= ( <expression> <expression> )

            (open-parenthesis ()
              (parse-call-exp token-source))

            (comma ()
              (report-bad-initial-token-error "A comma"))
            (close-parenthesis ()
              (report-bad-initial-token-error "A close parenthesis"))

            ;; <expression> ::= zero? ( <expression> )

            (zero?-token ()
              (parse-zero?-exp token-source))

            ;; <expression> ::=
            ;;          if <expression> then <expression> else <expression>

            (if-token ()
              (parse-if-exp token-source))

            (then-token ()
              (report-bad-initial-token-error "The keyword then"))
            (else-token ()
              (report-bad-initial-token-error "The keyword else"))

            ;; <expression> ::= <identifier>

            (identifier-token (id)
              (var-exp id))

            ;; <expression> ::= let {<identifier> = <expression>}* in <expression>

            (let-token ()
              (parse-let-exp token-source))

            (equals-sign ()
              (report-bad-initial-token-error "An equals sign"))
            (in-token ()
              (report-bad-initial-token-error "The keyword in"))

            ;; <expression> ::= proc ( {<identifier> : <type>}*{,} ) <expression>

            (proc-token ()
              (parse-proc-exp token-source))

            ;; <expression> ::= letrec {<type> <identifier>
            ;;                    ( {<identifier> : <type>}*{,} ) = <expression>}*
            ;;                    in <expression>

            (letrec-token ()
              (parse-letrec-exp token-source))

            ;; <expression> ::= set <identifier> = <expression>

            (set-token ()
              (parse-assign-exp token-source))

            ;; <expression> ::= begin <expression> {; <expression>}* end

            (begin-token ()
              (parse-begin-exp token-source))

            (semicolon ()
              (report-bad-initial-token-error "A semicolon"))
            (end-token ()
              (report-bad-initial-token-error "The keyword end"))

            ;; <expression> ::= + ( <expression> , <expression> )

            (plus-sign ()
              (parse-sum-exp token-source))

            ;; <expression> ::= * ( <expression> , <expression> )

            (asterisk ()
              (parse-product-exp token-source))

            ;; <expression> ::= cons ( <expression> , <expression> )

            (cons-token ()
              (parse-cons-exp token-source))

            ;; <expression> ::= car ( <expression> )

            (car-token ()
              (parse-car-exp token-source))

            ;; <expression> ::= cdr ( <expression> )

            (cdr-token ()
              (parse-cdr-exp token-source))

            ;; <expression> ::= null? ( <expression> )

            (null?-token ()
              (parse-null?-exp token-source))

            ;; <expression> ::= emptylist _ <type>

            (emptylist-token ()
              (parse-emptylist-exp token-source))

            ;; <expression> ::= list ( { <expression>+{,} )

            (list-token ()
              (parse-list-exp token-source))

            (class-token ()
              (report-bad-initial-token-error "The keyword class"))
            (extends-token ()
              (report-bad-initial-token-error "The keyword extends"))
            (field-token ()
              (report-bad-initial-token-error "The keyword field"))
            (method-token ()
              (report-bad-initial-token-error "The keyword method"))

            ;; <expression> ::= new <identifier> ( <expression>*{,} )

            (new-token ()
              (parse-new-object-exp token-source))

            ;; <expression> ::= send <expression> <identifier> ( {<expression>}*{,} )

            (send-token ()
              (parse-method-call-exp token-source))

            ;; <expression> ::= super <identifier ( <expression>*{,} )

            (super-token ()
              (parse-super-call-exp token-source))

            ;; <expression> ::= self

            (self-token ()
              (self-exp))

            (int-token ()
              (report-bad-initial-token-error "The keyword int"))
            (bool-token ()
              (report-bad-initial-token-error "The keyword bool"))
            (arrow ()
              (report-bad-initial-token-error "An error"))
            (colon ()
              (report-bad-initial-token-error "A colon"))
            (implements-token ()
              (report-bad-initial-token-error "The keyword implements"))
            (interface-token ()
              (report-bad-initial-token-error "The keyword interface"))

            ;; <expression> ::= cast <expression> <identifier>

            (cast-token ()
              (parse-cast-exp token-source))

            ;; <expression> ::= instanceof <expression> <identifier>

            (instanceof-token ()
              (parse-instanceof-exp token-source))

            (void-token ()
              (report-bad-initial-token-error "The keyword void"))
            (listof-token ()
              (report-bad-initial-token-error "The keyword listof"))
            (underscore ()
              (report-bad-initial-token-error "An underscore"))))))

    ;; report-bad-initial-token-error : String -> ()

    (define report-bad-initial-token-error
      (lambda (bad-token-string)
        (eopl:error 'parse-expression
                    "~s may not occur at the beginning of an expression.~%"
                    bad-token-string)))

    ;; parse-diff-exp : Token-source -> DiffExp

    (define parse-diff-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((minuend (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((subtrahend (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (diff-exp minuend subtrahend)))))

    ;; parse-call-exp : Token-source -> CallExp

    (define parse-call-exp
      (lambda (token-source)
        (let ((operator (parse-expression token-source)))
          (let loop ((reversed-operands '()))
            (if (eq? (species (token-source 'peek)) 'close-parenthesis)
                (begin
                  (token-source 'get)
                  (call-exp operator (reverse reversed-operands)))
                (loop (cons (parse-expression token-source)
                            reversed-operands)))))))

    ;; [Aryan]

    ;; parse-fieldref : Token-Source -> FieldRefExp
    (define parse-fieldref
      (lambda (token-source)
        (let* ((exp (parse-expression token-source))
               (id (acquire-identifier token-source)))
          (fieldref-exp exp id))))

    
    ;; parse-fieldset : Token-Source -> FieldSetExp
    (define parse-fieldset
      (lambda (token-source)
        (let* ((exp (parse-expression token-source))
               (id (acquire-identifier token-source)))
          (match-and-discard token-source 'equals-sign)
          (fieldset-exp exp id (parse-expression token-source)))))
    
    ;; parse-argument-list : Token-source -> Listof(Exp)

    (define parse-argument-list
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (if (eq? (species (token-source 'peek)) 'close-parenthesis)
            (begin
              (token-source 'get)
              '())
            (let loop ((reversed-elements
                         (list (parse-expression token-source))))
              (if (eq? (species (token-source 'peek)) 'close-parenthesis)
                  (begin
                    (token-source 'get)
                    (reverse reversed-elements))
                  (begin
                    (match-and-discard token-source 'comma)
                    (loop (cons (parse-expression token-source)
                                reversed-elements))))))))

    ;; parse-zero?-exp : Token-source -> Zero?Exp

    (define parse-zero?-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((testee (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (zero?-exp testee))))

    ;; parse-if-exp : Token-source -> IfExp

    (define parse-if-exp
      (lambda (token-source)
        (let ((condition (parse-expression token-source)))
          (match-and-discard token-source 'then-token)
          (let ((consequent (parse-expression token-source)))
            (match-and-discard token-source 'else-token)
            (let ((alternative (parse-expression token-source)))
              (if-exp condition consequent alternative))))))

    ;; parse-let-exp : Token-source -> LetExp

    (define parse-let-exp
      (lambda (token-source)
        (let loop ((reversed-bound-vars '())
                   (reversed-bound-values '()))
          (if (eq? (species (token-source 'peek)) 'in-token)
              (begin
                (token-source 'get)
                (let-exp (reverse reversed-bound-vars)
                         (reverse reversed-bound-values)
                         (parse-expression token-source)))
              (let ((bound-var (acquire-identifier token-source)))
                (match-and-discard token-source 'equals-sign)
                (let ((bound-value (parse-expression token-source)))
                  (loop (cons bound-var reversed-bound-vars)
                        (cons bound-value reversed-bound-values))))))))

    ;; parse-proc-exp : Token-source -> ProcExp

    (define parse-proc-exp
      (lambda (token-source)
        (receive (parameters parameter-types) (parse-parameter-list token-source)
          (proc-exp parameters parameter-types (parse-expression token-source)))))

    ;; parse-letrec-exp : Token-source -> LetrecExp

    (define parse-letrec-exp
      (lambda (token-source)
        (let loop ((next-token (token-source 'peek))
                   (reversed-result-types '())
                   (reversed-procedure-names '())
                   (reversed-parameter-lists '())
                   (reversed-parameter-types '())
                   (reversed-procedure-bodies '()))
          (if (eq? (species next-token) 'in-token)
              (begin
                (token-source 'get)
                (letrec-exp (reverse reversed-result-types)
                            (reverse reversed-procedure-names)
                            (reverse reversed-parameter-lists)
                            (reverse reversed-parameter-types)
                            (reverse reversed-procedure-bodies)
                            (parse-expression token-source)))
              (let* ((result-type (parse-type token-source))
                     (procedure-name (acquire-identifier token-source)))
                (receive (parameters parameter-types)
                         (parse-parameter-list token-source)
                  (match-and-discard token-source 'equals-sign)
                  (let ((procedure-body (parse-expression token-source)))
                    (loop (token-source 'peek)
                          (cons result-type reversed-result-types)
                          (cons procedure-name reversed-procedure-names)
                          (cons parameters reversed-parameter-lists)
                          (cons parameter-types reversed-parameter-types)
                          (cons procedure-body reversed-procedure-bodies)))))))))

    ;; parse-assign-exp : Token-source -> AssignExp

    (define parse-assign-exp
      (lambda (token-source)
        (let ((target (acquire-identifier token-source)))
          (match-and-discard token-source 'equals-sign)
          (let ((source (parse-expression token-source)))
            (assign-exp target source)))))
    
    ;; parse-begin-exp : Token-source -> BeginExp

    (define parse-begin-exp
      (lambda (token-source)
        (let ((starter (parse-expression token-source)))
          (let loop ((reversed-sequence '()))
            (let ((next-token (token-source 'peek)))
              (cases token next-token
                (end-token ()
                  (token-source 'get)
                  (begin-exp starter (reverse reversed-sequence)))
                (semicolon ()
                  (token-source 'get)
                  (loop (cons (parse-expression token-source) reversed-sequence)))
                (else (report-misplaced-token-error next-token))))))))

    
    (define report-misplaced-token-error
      (lambda (tok)
        (eopl:error 'parse-begin-exp
                    (string-append "The incorrectly placed token ~a "
                                   " was found in a begin-expression.~%")
                    tok)))

    ;; parse-sum-exp : Token-source -> SumExp

    (define parse-sum-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((augend (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((addend (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (sum-exp augend addend)))))

    ;; parse-product-exp : Token-source -> ProductExp

    (define parse-product-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((multiplicand (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((multiplier (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (product-exp multiplicand multiplier)))))

    ;; parse-cons-exp : Token-source -> ConsExp

    (define parse-cons-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((leader (parse-expression token-source)))
          (match-and-discard token-source 'comma)
          (let ((followers (parse-expression token-source)))
            (match-and-discard token-source 'close-parenthesis)
            (cons-exp leader followers)))))

    ;; parse-car-exp : Token-source -> CarExp

    (define parse-car-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((pair (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (car-exp pair))))

    ;; parse-cdr-exp : Token-source -> CdrExp

    (define parse-cdr-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((pair (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (cdr-exp pair))))

    ;; parse-null?-exp : Token-source -> Null?Exp

    (define parse-null?-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((testee (parse-expression token-source)))
          (match-and-discard token-source 'close-parenthesis)
          (null?-exp testee))))

    ;; parse-emptylist-exp : Token-source -> EmptylistExp

    (define parse-emptylist-exp
      (lambda (token-source)
        (match-and-discard token-source 'underscore)
        (emptylist-exp (parse-type token-source))))

    ;; parse-list-exp : Token-source -> ListExp

    (define parse-list-exp
      (lambda (token-source)
        (match-and-discard token-source 'open-parenthesis)
        (let ((first (parse-expression token-source)))
          (let loop ((reversed-rest '()))
            (if (eq? (species (token-source 'peek)) 'close-parenthesis)
                (begin
                  (token-source 'get)
                  (list-exp first (reverse reversed-rest)))
                (begin
                  (match-and-discard token-source 'comma)
                  (loop (cons (parse-expression token-source)
                              reversed-rest))))))))

    ;; parse-new-object-exp : Token-source -> NewObjectExp

    (define parse-new-object-exp
      (lambda (token-source)
        (let* ((class-name (acquire-identifier token-source))
               (operands (parse-argument-list token-source)))
          (new-object-exp class-name operands))))

    ;; parse-method-call-exp : Token-source -> MethodCallExp

    (define parse-method-call-exp
      (lambda (token-source)
        (let* ((recipient (parse-expression token-source))
               (method-name (acquire-identifier token-source))
               (operands (parse-argument-list token-source)))
          (method-call-exp recipient method-name operands))))

    ;; parse-super-call-exp : Token-source -> SuperCallExp

    (define parse-super-call-exp
      (lambda (token-source)
        (let* ((method-name (acquire-identifier token-source))
               (operands (parse-argument-list token-source)))
          (super-call-exp method-name operands))))

    ;; parse-cast-exp : Token-source -> CastExp

    (define parse-cast-exp
      (lambda (token-source)
        (let* ((body (parse-expression token-source))
               (target-class-name (acquire-identifier token-source)))
          (cast-exp body target-class-name))))

    ;; parse-instanceof-exp : Token-source -> InstanceofExp

    (define parse-instanceof-exp
      (lambda (token-source)
        (let* ((body (parse-expression token-source))
               (test-class-name (acquire-identifier token-source)))
          (instanceof-exp body test-class-name))))

    ;; parse-interface-declaration : Token-source -> ClassDeclaration

    (define parse-interface-declaration
      (lambda (token-source)
        (let ((interface-name (acquire-identifier token-source)))
          (let loop ((reversed-method-decls '()))
            (if (eq? (species (token-source 'peek)) 'method-token)
                (begin
                  (token-source 'get)
                  (loop (cons (parse-abstract-method-declaration token-source)
                              reversed-method-decls)))
                (an-interface-declaration interface-name
                                          (reverse reversed-method-decls)))))))

    ;; parse-abstract-method-declaration : Token-source -> AbstractMethodDeclaration

    (define parse-abstract-method-declaration
      (lambda (token-source)
        (let* ((result-type (parse-type token-source))
               (method-name (acquire-identifier token-source)))
          (receive (parameters parameter-types) (parse-parameter-list token-source)
            (an-abstract-method-declaration result-type
                                            method-name
                                            parameters
                                            parameter-types)))))

    ;; The scan&parse procedure
    ;; takes a string or an input port as its argument
    ;; and returns a syntax tree for the program
    ;; that the source provides.

    ;; scan&parse : SchemeVal -> Program
   
    (define scan&parse
      (lambda (given)
        (let* ((token-source (scanner (make-character-source given)))
               (syntax-tree (parse-program token-source)))
          (if (token-source 'at-end?)
              syntax-tree
              (report-leftover-tokens-error)))))

    (define report-leftover-tokens-error
      (lambda ()
        (eopl:error 'scan&parse
          "There were extra, unusable tokens at the end of the program.")))))


;;; Copyright (C) 2009, 2015, 2019  John David Stone

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; along with this program.
;;; If not, it is available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html.

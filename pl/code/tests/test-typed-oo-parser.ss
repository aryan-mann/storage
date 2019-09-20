#!r7rs

;;; Tests for the (TYPED-OO parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 29, 2009
;;; last revised August 5, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (TYPED-OO scanner)
        (TYPED-OO syntax-trees)
        (TYPED-OO parser))

(suite parse-class-declaration ()

  (test parse-class-declaration:minimal
    (parse-class-declaration (scanner (make-character-source "a extends b 0")))
    1 (class-declaration?)
    (lambda (result)
      (cases class-declaration result
        (a-class-declaration (class-name super-name interface-names
                              field-types field-names method-decls)
          (and (eq? class-name 'a)
               (eq? super-name 'b)
               (null? interface-names)
               (null? field-types)
               (null? field-names)
               (null? method-decls))))))

  (test parse-class-declaration:structured
    (parse-class-declaration
      (scanner (make-character-source "d extends e implements m implements n
                                         field int f
                                         field int g
                                         field int h
                                         method void i (j: int, k: bool, l: int)
                                           set f = +(j, 1)
                                       0")))
    1 (class-declaration?)
    (lambda (result)
      (cases class-declaration result
        (a-class-declaration (class-name super-name interface-names
                              field-types field-names method-decls)
          (and (eq? class-name 'd)
               (eq? super-name 'e)
               (equal? interface-names (list 'm 'n))
               (pair? field-types)
               (cases type (car field-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cdr field-types))
               (cases type (cadr field-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cddr field-types))
               (cases type (caddr field-types)
                 (int-type () #t)
                 (else #f))
               (null? (cdddr field-types))
               (equal? field-names (list 'f 'g 'h))
               (pair? method-decls)
               (cases method-declaration (car method-decls)
                 (a-method-declaration (result-type method-name
                                        parameters parameter-types body)
                   (and (cases type result-type
                          (void-type () #t)
                          (else #f))
                        (eq? method-name 'i)
                        (equal? parameters (list 'j 'k 'l))
                        (pair? parameter-types)
                        (cases type (car parameter-types)
                          (int-type () #t)
                          (else #f))
                        (pair? (cdr parameter-types))
                        (cases type (cadr parameter-types)
                          (bool-type () #t)
                          (else #f))
                        (pair? (cddr parameter-types))
                        (cases type (caddr parameter-types)
                          (int-type () #t)
                          (else #f))
                        (null? (cdddr parameter-types))
                        (cases expression body
                          (assign-exp (target source)
                            (and (eq? target 'f)
                                 (cases expression source
                                   (sum-exp (augend addend)
                                     (and (cases expression augend
                                            (var-exp (id) (eq? id 'j))
                                            (else #f))
                                          (cases expression addend
                                            (const-exp (datum) (= datum 1))
                                            (else #f))))
                                   (else #f))))
                          (else #f)))))
               (null? (cdr method-decls))))))))

(suite parse-method-declaration ()

  (test parse-method-declaration:minimal
    (parse-method-declaration (scanner (make-character-source "void m () 2")))
    1 (method-declaration?)
    (lambda (result)
      (cases method-declaration result
        (a-method-declaration (result-type method-name
                               parameters parameter-types body)
          (and (cases type result-type
                 (void-type () #t)
                 (else #f))
               (eq? method-name 'm)
               (null? parameters)
               (null? parameter-types)
               (cases expression body
                 (const-exp (datum) (= datum 2))
                 (else #f)))))))

  (test parse-method-declaration:typical
    (parse-method-declaration
      (scanner (make-character-source
                 "void initialize (initx: int, inity: int, initcolor: int)
                   begin
                    super initialize(initx, inity);
                    set color = initcolor
                   end")))
    1 (method-declaration?)
    (lambda (result)
      (cases method-declaration result
        (a-method-declaration (result-type method-name
                               parameters parameter-types body)
          (and (cases type result-type
                 (void-type () #t)
                 (else #f))
               (eq? method-name 'initialize)
               (equal? parameters (list 'initx 'inity 'initcolor))
               (pair? parameter-types)
               (cases type (car parameter-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cdr parameter-types))
               (cases type (cadr parameter-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cddr parameter-types))
               (cases type (caddr parameter-types)
                 (int-type () #t)
                 (else #f))
               (null? (cdddr parameter-types))
               (cases expression body
                 (begin-exp (starter sequence)
                   (and (cases expression starter
                          (super-call-exp (method-name operands)
                            (and (eq? method-name 'initialize)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (var-exp (id) (eq? id 'initx))
                                   (else #f))
                                 (pair? (cdr operands))
                                 (cases expression (cadr operands)
                                   (var-exp (id) (eq? id 'inity))
                                   (else #f))
                                 (null? (cddr operands))))
                          (else #f))
                        (pair? sequence)
                        (cases expression (car sequence)
                          (assign-exp (source target)
                            (and (eq? source 'color)
                                 (cases expression target
                                   (var-exp (id) (eq? id 'initcolor))
                                   (else #f))))
                          (else #f))
                        (null? (cdr sequence))))
                 (else #f))))))))


(suite parse-expression ()

  (test parse-expression:short-numeral
    (parse-expression (scanner (make-character-source "3")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum 3))
        (else #f))))

  (test parse-expression:negative-numeral
    (parse-expression (scanner (make-character-source "-4")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum -4))
        (else #f))))

  (test parse-expression:long-numeral
    (parse-expression
      (scanner
        (make-character-source
          "9999999993999999999999999999999999999999999999999")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum)
          (= datum 9999999993999999999999999999999999999999999999999))
        (else #f))))

  (test parse-expression:short-identifier
    (parse-expression (scanner (make-character-source "n")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'n))
        (else #f))))

  (test parse-expression:longer-identifier
    (parse-expression (scanner (make-character-source "foo?2")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'foo?2))
        (else #f))))

  (test parse-expression:diff-exp
    (parse-expression (scanner (make-character-source "-(6, 7)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (diff-exp (minuend subtrahend)
          (and (cases expression minuend
                 (const-exp (datum) (= datum 6))
                 (else #f))
               (cases expression subtrahend
                 (const-exp (datum) (= datum 7))
                 (else #f))))
        (else #f))))
    
  (test parse-expression:zero?-exp
    (parse-expression (scanner (make-character-source "zero?(8)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (zero?-exp (testee)
          (cases expression testee
            (const-exp (datum) (= datum 8))
            (else #f)))
        (else #f))))

  (test parse-expression:if-exp
    (parse-expression (scanner (make-character-source "if o then 9 else 10")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (if-exp (condition consequent alternative)
          (and (cases expression condition
                 (var-exp (id) (eq? id 'o))
                 (else #f))
               (cases expression consequent
                 (const-exp (datum) (= datum 9))
                 (else #f))
               (cases expression alternative
                 (const-exp (datum) (= datum 10))
                 (else #f))))
        (else #f))))

  (test parse-expression:let-exp-no-bindings
    (parse-expression (scanner (make-character-source "let in 11")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (let-exp (bound-vars bound-values body)
          (and (null? bound-vars)
               (null? bound-values)
               (cases expression body
                 (const-exp (datum) (= datum 11))
                 (else #f))))
        (else #f))))

  (test parse-expression:let-exp-single-binding
    (parse-expression (scanner (make-character-source "let p = 12 in 13")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (let-exp (bound-vars bound-values body)
          (and (equal? bound-vars (list 'p))
               (pair? bound-values)
               (cases expression (car bound-values)
                 (const-exp (datum) (= datum 12))
                 (else #f))
               (null? (cdr bound-values))
               (cases expression body
                 (const-exp (datum) (= datum 13))
                 (else #f))))
        (else #f))))

  (test parse-expression:let-exp-multiple-bindings
    (parse-expression
     (scanner (make-character-source "let q = 14
                                          r = 15
                                          s = 16
                                      in 17")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (let-exp (bound-vars bound-values body)
          (and (equal? bound-vars (list 'q 'r 's))
               (pair? bound-values)
               (cases expression (car bound-values)
                 (const-exp (datum) (= datum 14))
                 (else #f))
               (pair? (cdr bound-values))
               (cases expression (cadr bound-values)
                 (const-exp (datum) (= datum 15))
                 (else #f))
               (pair? (cddr bound-values))
               (cases expression (caddr bound-values)
                 (const-exp (datum) (= datum 16))
                 (else #f))
               (null? (cdddr bound-values))
               (cases expression body
                 (const-exp (datum) (= datum 17))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp-no-parameters
    (parse-expression (scanner (make-character-source "proc () 18")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameters parameter-types body)
          (and (null? parameters)
               (null? parameter-types)
               (cases expression body
                 (const-exp (datum) (= datum 18))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp-single-parameter
    (parse-expression (scanner (make-character-source "proc (t: (-> int)) 19")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameters parameter-types body)
          (and (equal? parameters (list 't))
               (pair? parameter-types)
               (cases type (car parameter-types)
                 (proc-type (arg-types result-type)
                   (and (null? arg-types)
                        (cases type result-type
                          (int-type () #t)
                          (else #f))))
                 (else #f))
               (null? (cdr parameter-types))
               (cases expression body
                 (const-exp (datum) (= datum 19))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp-multiple-parameters
    (parse-expression
      (scanner (make-character-source
                 "proc (u: int, v: bool, w: listof int, x: (int * int -> bool)) 20")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameters parameter-types body)
          (and (equal? parameters (list 'u 'v 'w 'x))
               (pair? parameter-types)
               (cases type (car parameter-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cdr parameter-types))
               (cases type (cadr parameter-types)
                 (bool-type () #t)
                 (else #f))
               (pair? (cddr parameter-types))
               (cases type (caddr parameter-types)
                 (list-type (base)
                   (cases type base
                     (int-type () #t)
                     (else #f)))
                 (else #f))
               (pair? (cdddr parameter-types))
               (cases type (cadddr parameter-types)
                 (proc-type (arg-types result-type)
                   (and (pair? arg-types)
                        (cases type (car arg-types)
                          (int-type () #t)
                          (else #f))
                        (pair? (cdr arg-types))
                        (cases type (cadr arg-types)
                          (int-type () #t)
                          (else #f))
                        (null? (cddr arg-types))
                        (cases type result-type
                          (bool-type () #t)
                          (else #f))))
                 (else #f))
               (null? (cddddr parameter-types))
               (cases expression body
                 (const-exp (datum) (= datum 20))
                 (else #f))))
        (else #f))))

  (test parse-expression:call-exp-no-arguments
    (parse-expression (scanner (make-character-source "(y)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (call-exp (operator operands)
          (and (cases expression operator
                 (var-exp (id) (eq? id 'y))
                 (else #f))
               (null? operands)))
        (else #f))))

  (test parse-expression:call-exp-single-argument
    (parse-expression (scanner (make-character-source "(z 21)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (call-exp (operator operands)
          (and (cases expression operator
                 (var-exp (id) (eq? id 'z))
                 (else #f))
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 21))
                 (else #f))
               (null? (cdr operands))))
        (else #f))))

  (test parse-expression:call-exp-multiple-arguments
    (parse-expression (scanner (make-character-source "(a 22 23 24)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (call-exp (operator operands)
          (and (cases expression operator
                 (var-exp (id) (eq? id 'a))
                 (else #f))
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 22))
                 (else #f))
               (pair? (cdr operands))
               (cases expression (cadr operands)
                 (const-exp (datum) (= datum 23))
                 (else #f))
               (pair? (cddr operands))
               (cases expression (caddr operands)
                 (const-exp (datum) (= datum 24))
                 (else #f))
               (null? (cdddr operands))))
        (else #f))))

  (test parse-expression:letrec-exp-no-bindings
    (parse-expression
      (scanner (make-character-source "letrec in 25")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (return-types procedure-names parameter-lists parameter-types
                     procedure-bodies letrec-body)
          (and (null? return-types)
               (null? procedure-names)
               (null? parameter-lists)
               (null? parameter-types)
               (null? procedure-bodies)
               (cases expression letrec-body
                 (const-exp (datum) (= datum 25))
                 (else #f))))
        (else #f))))

  (test parse-expression:letrec-exp-multiple-bindings
    (parse-expression
      (scanner (make-character-source "letrec int b () = 26
                                              int c (d: bool) = 27
                                              int e (f: int, g: listof int, h: bool) = 28
                                       in 29")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (result-types procedure-names parameter-lists parameter-types
                     procedure-bodies letrec-body)
          (and (pair? result-types)
               (cases type (car result-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cdr result-types))
               (cases type (cadr result-types)
                 (int-type () #t)
                 (else #f))
               (pair? (cddr result-types))
               (cases type (caddr result-types)
                 (int-type () #t)
                 (else #f))
               (null? (cdddr result-types))
               (equal? procedure-names (list 'b 'c 'e))
               (equal? parameter-lists (list (list) (list 'd) (list 'f 'g 'h)))
               (pair? parameter-types)
               (null? (car parameter-types))
               (pair? (cdr parameter-types))
               (pair? (cadr parameter-types))
               (cases type (caadr parameter-types)
                 (bool-type () #t)
                 (else #f))
               (null? (cdadr parameter-types))
               (pair? (cddr parameter-types))
               (let ((third-parameter-types (caddr parameter-types)))
                 (and (pair? third-parameter-types)
                      (cases type (car third-parameter-types)
                        (int-type () #t)
                        (else #f))
                      (pair? (cdr third-parameter-types))
                      (cases type (cadr third-parameter-types)
                        (list-type (base)
                          (cases type base
                            (int-type () #t)
                            (else #f)))
                        (else #f))
                      (pair? (cddr third-parameter-types))
                      (cases type (caddr third-parameter-types)
                        (bool-type () #t)
                        (else #f))
                      (null? (cdddr third-parameter-types))))
               (null? (cdddr parameter-types))
               (pair? procedure-bodies)
               (cases expression (car procedure-bodies)
                 (const-exp (datum) (= datum 26))
                 (else #f))
               (pair? (cdr procedure-bodies))
               (cases expression (cadr procedure-bodies)
                 (const-exp (datum) (= datum 27))
                 (else #f))
               (pair? (cddr procedure-bodies))
               (cases expression (caddr procedure-bodies)
                 (const-exp (datum) (= datum 28))
                 (else #f))
               (null? (cdddr procedure-bodies))
               (cases expression letrec-body
                 (const-exp (datum) (= datum 29))
                 (else #f))))
        (else #f))))

  (test parse-expression:assign-exp
    (parse-expression (scanner (make-character-source "set i = 30")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (assign-exp (target source)
          (and (eq? target 'i)
               (cases expression source
                 (const-exp (datum) (= datum 30))
                 (else #f))))
        (else #f))))

  (test parse-expression:begin-exp-minimal
    (parse-expression (scanner (make-character-source "begin 31 end")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (begin-exp (starter sequence)
          (and (cases expression starter
                 (const-exp (datum) (= datum 31))
                 (else #f))
               (null? sequence)))
        (else #f))))

  (test parse-expression:begin-exp-longer-sequence
    (parse-expression
      (scanner (make-character-source "begin 32; 33; 34; 35 end")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (begin-exp (starter sequence)
          (and (cases expression starter
                 (const-exp (datum) (= datum 32))
                 (else #f))
               (pair? sequence)
               (cases expression (car sequence)
                 (const-exp (datum) (= datum 33))
                 (else #f))
               (pair? (cdr sequence))
               (cases expression (cadr sequence)
                 (const-exp (datum) (= datum 34))
                 (else #f))
               (pair? (cddr sequence))
               (cases expression (caddr sequence)
                 (const-exp (datum) (= datum 35))
                 (else #f))
               (null? (cdddr sequence))))
        (else #f))))

  (test parse-expression:sum-exp
    (parse-expression (scanner (make-character-source "+(36,37)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (sum-exp (augend addend)
          (and (cases expression augend
                 (const-exp (datum) (= datum 36))
                 (else #f))
               (cases expression addend
                 (const-exp (datum) (= datum 37))
                 (else #f))))
        (else #f))))

  (test parse-expression:product-exp
    (parse-expression (scanner (make-character-source "*(38,39)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (product-exp (multiplicand multiplier)
          (and (cases expression multiplicand
                 (const-exp (datum) (= datum 38))
                 (else #f))
               (cases expression multiplier
                 (const-exp (datum) (= datum 39))
                 (else #f))))
        (else #f))))

  (test parse-expression:cons-exp
    (parse-expression (scanner (make-character-source "cons(40,j)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (cons-exp (fore aft)
          (and (cases expression fore
                 (const-exp (datum) (= datum 40))
                 (else #f))
               (cases expression aft
                 (var-exp (id) (eq? id 'j))
                 (else #f))))
        (else #f))))

  (test parse-expression:car-exp
    (parse-expression (scanner (make-character-source "car(k)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (car-exp (pair)
          (cases expression pair
            (var-exp (id) (eq? id 'k))
            (else #f)))
        (else #f))))

  (test parse-expression:cdr-exp
    (parse-expression (scanner (make-character-source "cdr(l)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (cdr-exp (pair)
          (cases expression pair
            (var-exp (id) (eq? id 'l))
            (else #f)))
        (else #f))))

  (test parse-expression:null?-exp
    (parse-expression (scanner (make-character-source "null?(m)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (null?-exp (testee)
          (cases expression testee
            (var-exp (id) (eq? id 'm))
            (else #f)))
        (else #f))))

  (test parse-expression:emptylist-exp
    (parse-expression (scanner (make-character-source "emptylist_int")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (emptylist-exp (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f))))

  (test parse-expression:list-exp-single-element
    (parse-expression (scanner (make-character-source "list(41)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (first rest)
          (and (cases expression first
                 (const-exp (datum) (= datum 41))
                 (else #f))
               (null? rest)))
        (else #f))))

  (test parse-expression:list-exp-multiple-elements
    (parse-expression (scanner (make-character-source "list(42, 43, 44)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (first rest)
          (and (cases expression first
                 (const-exp (datum) (= datum 42))
                 (else #f))
               (pair? rest)
               (cases expression (car rest)
                 (const-exp (datum) (= datum 43))
                 (else #f))
               (pair? (cdr rest))
               (cases expression (cadr rest)
                 (const-exp (datum) (= datum 44))
                 (else #f))
               (null? (cddr rest))))
        (else #f))))

  (test parse-expression:new-object-exp-no-operands
    (parse-expression (scanner (make-character-source "new n()")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (new-object-exp (class-name operands)
          (and (eq? class-name 'n)
               (null? operands)))
        (else #f))))

  (test parse-expression:new-object-exp-single-operand
    (parse-expression (scanner (make-character-source "new o(41)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (new-object-exp (class-name operands)
          (and (eq? class-name 'o)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 41))
                 (else #f))
               (null? (cdr operands))))
        (else #f))))

  (test parse-expression:new-object-exp-multiple-operands
    (parse-expression (scanner (make-character-source "new p(42,43,44)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (new-object-exp (class-name operands)
          (and (eq? class-name 'p)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 42))
                 (else #f))
               (pair? (cdr operands))
               (cases expression (cadr operands)
                 (const-exp (datum) (= datum 43))
                 (else #f))
               (pair? (cddr operands))
               (cases expression (caddr operands)
                 (const-exp (datum) (= datum 44))
                 (else #f))
               (null? (cdddr operands))))
        (else #f))))

  (test parse-expression:method-call-exp-no-operands
    (parse-expression (scanner (make-character-source "send q r()")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (method-call-exp (recipient method-name operands)
          (and (cases expression recipient
                 (var-exp (id) (eq? id 'q))
                 (else #f))
               (eq? method-name 'r)
               (null? operands)))
        (else #f))))

  (test parse-expression:method-call-exp-single-operand
    (parse-expression (scanner (make-character-source "send s t(45)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (method-call-exp (recipient method-name operands)
          (and (cases expression recipient
                 (var-exp (id) (eq? id 's))
                 (else #f))
               (eq? method-name 't)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 45))
                 (else #f))
               (null? (cdr operands))))
        (else #f))))

  (test parse-expression:method-call-exp-multiple-operands
    (parse-expression (scanner (make-character-source "send u v(46,47,48)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (method-call-exp (recipient method-name operands)
          (and (cases expression recipient
                 (var-exp (id) (eq? id 'u))
                 (else #f))
               (eq? method-name 'v)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 46))
                 (else #f))
               (pair? (cdr operands))
               (cases expression (cadr operands)
                 (const-exp (datum) (= datum 47))
                 (else #f))
               (pair? (cddr operands))
               (cases expression (caddr operands)
                 (const-exp (datum) (= datum 48))
                 (else #f))
               (null? (cdddr operands))))
        (else #f))))

  (test parse-expression:super-call-exp-no-operands
    (parse-expression (scanner (make-character-source "super w()")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (super-call-exp (method-name operands)
          (and (eq? method-name 'w)
               (null? operands)))
        (else #f))))

  (test parse-expression:super-call-exp-single-operand
    (parse-expression (scanner (make-character-source "super x(49)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (super-call-exp (method-name operands)
          (and (eq? method-name 'x)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 49))
                 (else #f))
               (null? (cdr operands))))
        (else #f))))

  (test parse-expression:super-call-exp-multiple-operands
    (parse-expression (scanner (make-character-source "super y(50,51,52)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (super-call-exp (method-name operands)
          (and (eq? method-name 'y)
               (pair? operands)
               (cases expression (car operands)
                 (const-exp (datum) (= datum 50))
                 (else #f))
               (pair? (cdr operands))
               (cases expression (cadr operands)
                 (const-exp (datum) (= datum 51))
                 (else #f))
               (pair? (cddr operands))
               (cases expression (caddr operands)
                 (const-exp (datum) (= datum 52))
                 (else #f))
               (null? (cdddr operands))))
          (else #f))))

  (test parse-expression:self-exp
    (parse-expression (scanner (make-character-source "self")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (self-exp () #t)
        (else #f))))

  (test parse-expression:cast-exp
    (parse-expression (scanner (make-character-source "cast z a")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (cast-exp (body target-class-name)
          (and (cases expression body
                 (var-exp (id) (eq? id 'z))
                 (else #f))
               (eq? target-class-name 'a)))
        (else #f))))

  (test parse-expression:instanceof-exp
    (parse-expression (scanner (make-character-source "instanceof b c")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (instanceof-exp (body test-class-name)
          (and (cases expression body
                 (var-exp (id) (eq? id 'b))
                 (else #f))
               (eq? test-class-name 'c)))
        (else #f)))))


(suite parse-program ()

  (test parse-program:minimal
    (parse-program (scanner (make-character-source "53")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (null? class-decls)
               (cases expression body
                 (const-exp (datum) (= datum 53))
                 (else #f)))))))

  (test parse-program:page-332-adapted
    (parse-program
      (scanner (make-character-source "class c1 extends object
                                         field int i
                                         field int j
                                         method void initialize (x: int)
                                          begin
                                           set i = x;
                                           set j = -(0,x)
                                          end
                                         method void countup (d: int)
                                          begin
                                           set i = +(i,d);
                                           set j = -(j,d)
                                          end
                                         method listof int getstate () list(i,j)
                                       let t1 = 0
                                           t2 = 0
                                           o1 = new c1(3)
                                       in begin
                                           set t1 = send o1 getstate();
                                           send o1 countup(2);
                                           set t2 = send o1 getstate();
                                           list(t1, t2)
                                          end")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name interface-names
                                       field-types field-names method-decls)
                   (and (eq? class-name 'c1)
                        (eq? super-name 'object)
                        (null? interface-names)
                        (pair? field-types)
                        (cases type (car field-types)
                          (int-type () #t)
                          (else #f))
                        (pair? (cdr field-types))
                        (cases type (cadr field-types)
                          (int-type () #t)
                          (else #f))
                        (null? (cddr field-types))
                        (equal? field-names (list 'i 'j))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (result-type method-name
                                                 parameters parameter-types body)
                            (and (cases type result-type
                                   (void-type () #t)
                                   (else #f))
                                 (eq? method-name 'initialize)
                                 (equal? parameters (list 'x))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (int-type () #t)
                                   (else #f))
                                 (null? (cdr parameter-types))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'i)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'x))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'j)
                                                   (cases expression source
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (const-exp (datum) (zero? datum))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (result-type method-name
                                                 parameters parameter-types body)
                            (and (cases type result-type
                                   (void-type () #t)
                                   (else #f))
                                 (eq? method-name 'countup)
                                 (equal? parameters (list 'd))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (int-type () #t)
                                   (else #f))
                                 (null? (cdr parameter-types))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'i)
                                                   (cases expression source
                                                     (sum-exp (augend addend)
                                                       (and (cases expression augend
                                                              (var-exp (id) (eq? id 'i))
                                                              (else #f))
                                                            (cases expression addend
                                                              (var-exp (id) (eq? id 'd))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'j)
                                                   (cases expression source
                                                     (diff-exp (minuend subtrahend)
                                                        (and (cases expression minuend
                                                               (var-exp (id) (eq? id 'j))
                                                               (else #f))
                                                             (cases expression subtrahend
                                                               (var-exp (id) (eq? id 'd))
                                                               (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (result-type method-name
                                                 parameters parameter-types body)
                            (and (cases type result-type
                                   (list-type (base)
                                     (cases type base
                                       (int-type () #t)
                                       (else #f)))
                                   (else #f))
                                 (eq? method-name 'getstate)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (list-exp (first rest)
                                     (and (cases expression first
                                            (var-exp (id) (eq? id 'i))
                                            (else #f))
                                          (pair? rest)
                                          (cases expression (car rest)
                                            (var-exp (id) (eq? id 'j))
                                            (else #f))
                                          (null? (cdr rest))))
                                   (else #f)))))
                        (null? (cdddr method-decls))))))
               (null? (cdr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 't1 't2 'o1))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (const-exp (datum) (zero? datum))
                          (else #f))
                        (pair? (cdr bound-values))
                        (cases expression (cadr bound-values)
                          (const-exp (datum) (zero? datum))
                          (else #f))
                        (pair? (cddr bound-values))
                        (cases expression (caddr bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'c1)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (const-exp (datum) (= datum 3))
                                   (else #f))
                                 (null? (cdr operands))))
                          (else #f))
                        (null? (cdddr bound-values))
                        (cases expression body
                          (begin-exp (starter sequence)
                            (and (cases expression starter
                                   (assign-exp (target source)
                                     (and (eq? target 't1)
                                          (cases expression source
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o1))
                                                     (else #f))
                                                   (eq? method-name 'getstate)
                                                   (null? operands)))
                                            (else #f))))
                                   (else #f))
                                 (pair? sequence)
                                 (cases expression (car sequence)
                                   (method-call-exp (recipient method-name operands)
                                      (and (cases expression recipient
                                             (var-exp (id) (eq? id 'o1))
                                             (else #f))
                                           (eq? method-name 'countup)
                                           (pair? operands)
                                           (cases expression (car operands)
                                             (const-exp (datum) (= datum 2))
                                             (else #f))
                                           (null? (cdr operands))))
                                   (else #f))
                                 (pair? (cdr sequence))
                                 (cases expression (cadr sequence)
                                   (assign-exp (target source)
                                     (and (eq? target 't2)
                                          (cases expression source
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o1))
                                                     (else #f))
                                                   (eq? method-name 'getstate)
                                                   (null? operands)))
                                            (else #f))))
                                   (else #f))
                                 (pair? (cddr sequence))
                                 (cases expression (caddr sequence)
                                   (list-exp (first rest)
                                     (and (cases expression first
                                            (var-exp (id) (eq? id 't1))
                                            (else #f))
                                          (pair? rest)
                                          (cases expression (car rest)
                                            (var-exp (id) (eq? id 't2))
                                            (else #f))
                                          (null? (cdr rest))))
                                   (else #f))
                                 (null? (cdddr sequence))))
                          (else #f))))
                 (else #f)))))))

(suite scan&parse ()

  (test scan&parse:minimal
    (scan&parse "54")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (null? class-decls)
               (cases expression body
                 (const-exp (datum) (= datum 54))
                 (else #f)))))))

  (test scan&parse:page-354
    (scan&parse "interface tree
                  method int sum()
                  method bool equal (t : tree)

                 class interior-node extends object implements tree
                  field tree left
                  field tree right
                  method void initialize(l : tree, r : tree)
                   begin
                    set left = l; set right = r
                   end
                  method tree getleft () left
                  method tree getright () right
                  method int sum () +(send left sum(), send right sum())
                  method bool equal (t : tree)
                   if instanceof t interior-node
                   then if send left equal(send
                                            cast t interior-node
                                            getleft())
                        then send right equal(send
                                               cast t interior-node
                                               getright())
                        else zero?(1)
                    else zero?(1)

                 class leaf-node extends object implements tree
                  field int value
                  method void initialize (v : int) set value = v
                  method int sum () value
                  method int getvalue () value
                  method bool equal (t : tree)
                   if instanceof t leaf-node
                   then zero?(-(value, send cast t leaf-node getvalue()))
                   else zero?(1)

                 let o1 = new interior-node(
                           new interior-node(
                            new leaf-node(3),
                            new leaf-node(4)),
                          new leaf-node(5))
                 in list(send o1 sum(),
                         if send o1 equal(o1) then 100 else 200)")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (an-interface-declaration (interface-name method-decls)
                   (and (eq? interface-name 'tree)
                        (pair? method-decls)
                        (cases abstract-method-declaration (car method-decls)
                          (an-abstract-method-declaration (result-type method-name parameters parameter-types)
                            (and (cases type result-type
                                   (int-type () #t)
                                   (else #f))
                                 (eq? method-name 'sum)
                                 (null? parameters)
                                 (null? parameter-types))))
                        (pair? (cdr method-decls))
                        (cases abstract-method-declaration (cadr method-decls)
                          (an-abstract-method-declaration (result-type method-name parameters parameter-types)
                            (and (cases type result-type
                                   (bool-type () #t)
                                   (else #f))
                                 (eq? method-name 'equal)
                                 (equal? parameters (list 't))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (null? (cdr parameter-types)))))
                        (null? (cddr method-decls)))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name interface-names
                                       field-types field-names method-decls)
                   (and (eq? class-name 'interior-node)
                        (eq? super-name 'object)
                        (equal? interface-names (list 'tree))
                        (pair? field-types)
                        (cases type (car field-types)
                          (class-type (class-name) (eq? class-name 'tree))
                          (else #f))
                        (pair? (cdr field-types))
                        (cases type (cadr field-types)
                          (class-type (class-name) (eq? class-name 'tree))
                          (else #f))
                        (null? (cddr field-types))
                        (equal? field-names (list 'left 'right))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (void-type () #t)
                                   (else #f))
                                 (eq? method-name 'initialize)
                                 (equal? parameters (list 'l 'r))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (pair? (cdr parameter-types))
                                 (cases type (cadr parameter-types)
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (null? (cddr parameter-types))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'left)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'l))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'right)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'r))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (eq? method-name 'getleft)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'left))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (eq? method-name 'getright)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'right))
                                   (else #f)))))
                        (pair? (cdddr method-decls))
                        (cases method-declaration (cadddr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (int-type () #t)
                                   (else #f))
                                 (eq? method-name 'sum)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (sum-exp (augend addend)
                                     (and (cases expression augend
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'left))
                                                     (else #f))
                                                   (eq? method-name 'sum)
                                                   (null? operands)))
                                            (else #f))
                                          (cases expression addend
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'right))
                                                     (else #f))
                                                   (eq? method-name 'sum)
                                                   (null? operands)))
                                            (else #f))))
                                   (else #f)))))
                        (let ((rest-of-method-decls (cddddr method-decls)))
                          (and (pair? rest-of-method-decls)
                               (cases method-declaration (car rest-of-method-decls)
                                 (a-method-declaration (result-type method-name parameters parameter-types body)
                                   (and (cases type result-type
                                          (bool-type () #t)
                                          (else #f))
                                        (eq? method-name 'equal)
                                        (equal? parameters (list 't))
                                        (pair? parameter-types)
                                        (cases type (car parameter-types)
                                          (class-type (class-name) (eq? class-name 'tree))
                                          (else #f))
                                        (null? (cdr parameter-types))
                                        (cases expression body
                                          (if-exp (condition consequent alternative)
                                            (and (cases expression condition
                                                   (instanceof-exp (body test-class-name)
                                                     (and (cases expression body
                                                            (var-exp (id) (eq? id 't))
                                                            (else #f))
                                                          (eq? test-class-name 'interior-node)))
                                                   (else #f))
                                                 (cases expression consequent
                                                   (if-exp (condition consequent alternative)
                                                     (and (cases expression condition
                                                            (method-call-exp (recipient method-name operands)
                                                              (and (cases expression recipient
                                                                     (var-exp (id) (eq? id 'left))
                                                                     (else #f))
                                                                   (eq? method-name 'equal)
                                                                   (pair? operands)
                                                                   (cases expression (car operands)
                                                                     (method-call-exp (recipient method-name operands)
                                                                       (and (cases expression recipient
                                                                              (cast-exp (body target-class-name)
                                                                                (and (cases expression body
                                                                                       (var-exp (id) (eq? id 't))
                                                                                       (else #f))
                                                                                     (eq? target-class-name 'interior-node)))
                                                                              (else #f))
                                                                            (eq? method-name 'getleft)
                                                                            (null? operands)))
                                                                     (else #f))
                                                                   (null? (cdr operands))))
                                                            (else #f))
                                                          (cases expression consequent
                                                            (method-call-exp (recipient method-name operands)
                                                              (and (cases expression recipient
                                                                     (var-exp (id) (eq? id 'right))
                                                                     (else #f))
                                                                   (eq? method-name 'equal)
                                                                   (pair? operands)
                                                                   (cases expression (car operands)
                                                                     (method-call-exp (recipient method-name operands)
                                                                       (and (cases expression recipient
                                                                              (cast-exp (body target-class-name)
                                                                                (and (cases expression body
                                                                                       (var-exp (id) (eq? id 't))
                                                                                       (else #f))
                                                                                     (eq? target-class-name 'interior-node)))
                                                                              (else #f))
                                                                            (eq? method-name 'getright)
                                                                            (null? operands)))
                                                                     (else #f))
                                                                   (null? (cdr operands))))
                                                            (else #f))
                                                          (cases expression alternative
                                                            (zero?-exp (testee)
                                                              (cases expression testee
                                                                (const-exp (datum) (= datum 1))
                                                                (else #f)))
                                                            (else #f))))
                                                   (else #f))
                                                 (cases expression alternative
                                                   (zero?-exp (testee)
                                                     (cases expression testee
                                                       (const-exp (datum) (= datum 1))
                                                       (else #f)))
                                                   (else #f))))
                                          (else #f)))))
                               (null? (cdr rest-of-method-decls)))))))
               (pair? (cddr class-decls))
               (cases class-declaration (caddr class-decls)
                 (a-class-declaration (class-name super-name interface-names
                                       field-types field-names method-decls)
                   (and (eq? class-name 'leaf-node)
                        (eq? super-name 'object)
                        (equal? interface-names (list 'tree))
                        (pair? field-types)
                        (cases type (car field-types)
                          (int-type () #t)
                          (else #f))
                        (null? (cdr field-types))
                        (equal? field-names (list 'value))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (void-type () #t)
                                   (else #f))
                                 (eq? method-name 'initialize)
                                 (equal? parameters (list 'v))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (int-type () #t)
                                   (else #f))
                                 (null? (cdr parameter-types))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'value)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'v))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (int-type () #t)
                                   (else #f))
                                 (eq? method-name 'sum)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'value))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (int-type () #t)
                                   (else #f))
                                 (eq? method-name 'getvalue)
                                 (null? parameters)
                                 (null? parameter-types)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'value))
                                   (else #f)))))
                        (pair? (cdddr method-decls))
                        (cases method-declaration (cadddr method-decls)
                          (a-method-declaration (result-type method-name parameters parameter-types body)
                            (and (cases type result-type
                                   (bool-type () #t)
                                   (else #f))
                                 (eq? method-name 'equal)
                                 (equal? parameters (list 't))
                                 (pair? parameter-types)
                                 (cases type (car parameter-types)
                                   (class-type (class-name) (eq? class-name 'tree))
                                   (else #f))
                                 (null? (cdr parameter-types))
                                 (cases expression body
                                   (if-exp (condition consequent alternative)
                                     (and (cases expression condition
                                            (instanceof-exp (body test-class-name)
                                              (and (cases expression body
                                                     (var-exp (id) (eq? id 't))
                                                     (else #f))
                                                   (eq? test-class-name 'leaf-node)))
                                            (else #f))
                                          (cases expression consequent
                                            (zero?-exp (testee)
                                              (cases expression testee
                                                (diff-exp (minuend subtrahend)
                                                  (and (cases expression minuend
                                                         (var-exp (id) (eq? id 'value))
                                                         (else #f))
                                                       (cases expression subtrahend
                                                         (method-call-exp (recipient method-name operands)
                                                           (and (cases expression recipient
                                                                  (cast-exp (body target-class-name)
                                                                    (and (cases expression body
                                                                           (var-exp (id) (eq? id 't))
                                                                           (else #f))
                                                                         (eq? target-class-name 'leaf-node)))
                                                                  (else #f))
                                                                (eq? method-name 'getvalue)
                                                                (null? operands)))
                                                         (else #f))))
                                                (else #f)))
                                            (else #f))
                                          (cases expression alternative
                                            (zero?-exp (testee)
                                              (cases expression testee
                                                (const-exp (datum) (= datum 1))
                                                (else #f)))
                                            (else #f))))
                                   (else #f)))))
                        (null? (cddddr method-decls)))))
               (null? (cdddr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o1))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'interior-node)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (new-object-exp (class-name operands)
                                     (and (eq? class-name 'interior-node)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (new-object-exp (class-name operands)
                                              (and (eq? class-name 'leaf-node)
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (const-exp (datum) (= datum 3))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))
                                          (pair? (cdr operands))
                                          (cases expression (cadr operands)
                                            (new-object-exp (class-name operands)
                                              (and (eq? class-name 'leaf-node)
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (const-exp (datum) (= datum 4))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))
                                          (null? (cddr operands))))
                                   (else #f))
                                 (pair? (cdr operands))
                                 (cases expression (cadr operands)
                                   (new-object-exp (class-name operands)
                                     (and (eq? class-name 'leaf-node)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 5))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (null? (cddr operands))))
                          (else #f))
                        (null? (cdr bound-values))
                        (cases expression body
                          (list-exp (first rest)
                            (and (cases expression first
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o1))
                                            (else #f))
                                          (eq? method-name 'sum)
                                          (null? operands)))
                                   (else #f))
                                 (pair? rest)
                                 (cases expression (car rest)
                                   (if-exp (condition consequent alternative)
                                     (and (cases expression condition
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o1))
                                                     (else #f))
                                                   (eq? method-name 'equal)
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (var-exp (id) (eq? id 'o1))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))
                                          (cases expression consequent
                                            (const-exp (datum) (= datum 100))
                                            (else #f))
                                          (cases expression alternative
                                            (const-exp (datum) (= datum 200))
                                            (else #f))))
                                   (else #f))
                                 (null? (cdr rest))))
                          (else #f))))
                 (else #f))))))))


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

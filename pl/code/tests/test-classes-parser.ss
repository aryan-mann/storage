#!r7rs

;;; Tests for the (CLASSES parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 23, 2009
;;; last revised July 31, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CLASSES scanner)
        (CLASSES syntax-trees)
        (CLASSES parser))

(suite parse-class-declaration ()

  (test parse-class-declaration:minimal
    (parse-class-declaration (scanner (make-character-source "a extends b 0")))
    1 (class-declaration?)
    (lambda (result)
      (cases class-declaration result
        (a-class-declaration (class-name super-name field-names method-decls)
          (and (eq? class-name 'a)
               (eq? super-name 'b)
               (null? field-names)
               (null? method-decls))))))

  (test parse-class-declaration:structured
    (parse-class-declaration
      (scanner (make-character-source "d extends e
                                         field f
                                         field g
                                         field h
                                         method i (j, k, l)
                                           set f = +(j, 1)
                                       0")))
    1 (class-declaration?)
    (lambda (result)
      (cases class-declaration result
        (a-class-declaration (class-name super-name field-names method-decls)
          (and (eq? class-name 'd)
               (eq? super-name 'e)
               (equal? field-names (list 'f 'g 'h))
               (pair? method-decls)
               (cases method-declaration (car method-decls)
                 (a-method-declaration (method-name parameters body)
                   (and (eq? method-name 'i)
                        (equal? parameters (list 'j 'k 'l))
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
    (parse-method-declaration (scanner (make-character-source "m () 2")))
    1 (method-declaration?)
    (lambda (result)
      (cases method-declaration result
        (a-method-declaration (method-name parameters body)
          (and (eq? method-name 'm)
               (null? parameters)
               (cases expression body
                 (const-exp (datum) (= datum 2))
                 (else #f)))))))

  (test parse-method-declaration:page-332-bottom
    (parse-method-declaration
      (scanner (make-character-source "initialize (initx, inity, initcolor)
                                        begin
                                         super initialize(initx, inity);
                                         set color = initcolor
                                        end")))
    1 (method-declaration?)
    (lambda (result)
      (cases method-declaration result
        (a-method-declaration (method-name parameters body)
          (and (eq? method-name 'initialize)
               (equal? parameters (list 'initx 'inity 'initcolor))
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
        (proc-exp (parameters body)
          (and (null? parameters)
               (cases expression body
                 (const-exp (datum) (= datum 18))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp-single-parameter
    (parse-expression (scanner (make-character-source "proc (t) 19")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameters body)
          (and (equal? parameters (list 't))
               (cases expression body
                 (const-exp (datum) (= datum 19))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp-multiple-parameters
    (parse-expression (scanner (make-character-source "proc (u, v, w, x) 20")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameters body)
          (and (equal? parameters (list 'u 'v 'w 'x))
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
        (letrec-exp (procedure-names parameter-lists procedure-bodies letrec-body)
          (and (null? procedure-names)
               (null? parameter-lists)
               (null? procedure-bodies)
               (cases expression letrec-body
                 (const-exp (datum) (= datum 25))
                 (else #f))))
        (else #f))))

  (test parse-expression:letrec-exp-multiple-bindings
    (parse-expression
      (scanner (make-character-source "letrec b () = 26
                                              c (d) = 27
                                              e (f, g, h) = 28
                                       in 29")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (procedure-names parameter-lists procedure-bodies letrec-body)
          (and (equal? procedure-names (list 'b 'c 'e))
               (equal? parameter-lists (list (list) (list 'd) (list 'f 'g 'h)))
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
    (parse-expression (scanner (make-character-source "emptylist")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (emptylist-exp () #t)
        (else #f))))

  (test parse-expression:list-exp-no-elements
    (parse-expression (scanner (make-character-source "list()")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements)
          (null? elements))
        (else #f))))

  (test parse-expression:list-exp-single-element
    (parse-expression (scanner (make-character-source "list(41)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements)
          (and (pair? elements)
               (cases expression (car elements)
                 (const-exp (datum) (= datum 41))
                 (else #f))
               (null? (cdr elements))))
        (else #f))))

  (test parse-expression:list-exp-multiple-elements
    (parse-expression (scanner (make-character-source "list(42, 43, 44)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements)
          (and (pair? elements)
               (cases expression (car elements)
                 (const-exp (datum) (= datum 42))
                 (else #f))
               (pair? (cdr elements))
               (cases expression (cadr elements)
                 (const-exp (datum) (= datum 43))
                 (else #f))
               (pair? (cddr elements))
               (cases expression (caddr elements)
                 (const-exp (datum) (= datum 44))
                 (else #f))
               (null? (cdddr elements))))
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

  (test parse-program:page-327
    (parse-program
      (scanner (make-character-source "class c1 extends object
                                        field i
                                        field j
                                        method initialize (x)
                                         begin
                                          set i = x;
                                          set j = -(0,x)
                                         end
                                        method countup (d)
                                         begin
                                          set i = +(i,d);
                                          set j = -(j,d)
                                         end
                                        method getstate () list(i,j)
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
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'c1)
                        (eq? super-name 'object)
                        (equal? field-names (list 'i 'j))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'x))
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
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'countup)
                                 (equal? parameters (list 'd))
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
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'getstate)
                                 (null? parameters)
                                 (cases expression body
                                   (list-exp (elements)
                                     (and (pair? elements)
                                          (cases expression (car elements)
                                            (var-exp (id) (eq? id 'i))
                                            (else #f))
                                          (pair? (cdr elements))
                                          (cases expression (cadr elements)
                                            (var-exp (id) (eq? id 'j))
                                            (else #f))
                                          (null? (cddr elements))))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
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
                            (cases expression starter
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
                              (list-exp (elements)
                                (and (pair? elements)
                                     (cases expression (car elements)
                                       (var-exp (id) (eq? id 't1))
                                       (else #f))
                                     (pair? (cdr elements))
                                     (cases expression (cadr elements)
                                       (var-exp (id) (eq? id 't2))
                                       (else #f))
                                     (null? (cddr elements))))
                              (else #f))
                            (null? (cdddr sequence)))
                          (else #f))))
                 (else #f))))))))        

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

  (test scan&parse:page-328
    (scan&parse "class interior-node extends object
                  field left
                  field right
                  method initialize (l, r)
                   begin
                    set left = l;
                    set right = r
                   end
                  method sum () +(send left sum(),send right sum())
                 class leaf-node extends object
                  field value
                  method initialize (v)
                   set value = v
                  method sum () value
                 let o1 = new interior-node(
                           new interior-node(
                            new leaf-node(3),
                            new leaf-node(4)),
                           new leaf-node(5))
                 in send o1 sum()")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'interior-node)
                        (eq? super-name 'object)
                        (equal? field-names (list 'left 'right))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'l 'r))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'left)
                                                   (cases expression source
                                                     (var-exp (id) 'l)
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'right)
                                                   (cases expression source
                                                     (var-exp (id) 'r)
                                                     (else #f))))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'sum)
                                 (null? parameters)
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
                                   (else #f))))))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'leaf-node)
                        (eq? super-name 'object)
                        (equal? field-names (list 'value))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'v))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'value)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'v))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'sum)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'value))
                                   (else #f)))))
                        (null? (cddr method-decls)))))
               (null? (cddr class-decls))
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
                          (method-call-exp (recipient method-name operands)
                            (and (cases expression recipient
                                   (var-exp (id) (eq? id 'o1))
                                   (else #f))
                                 (eq? method-name 'sum)
                                 (null? operands)))
                          (else #f))))
                 (else #f)))))))

  (test scan&parse:page-329
    (scan&parse "class oddeven extends object
                  method initialize () 1
                  method even (n)
                   if zero?(n) then 1 else send self odd(-(n,1))
                  method odd (n)
                   if zero?(n) then 0 else send self even(-(n,1))
                  let o1 = new oddeven()
                  in send o1 odd(13)")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'oddeven)
                        (eq? super-name 'object)
                        (null? field-names)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 1))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'even)
                                 (equal? parameters (list 'n))
                                 (cases expression body
                                   (if-exp (condition consequent alternative)
                                     (and (cases expression condition
                                            (zero?-exp (testee)
                                               (cases expression testee
                                                 (var-exp (id) (eq? id 'n))
                                                 (else #f)))
                                            (else #f))
                                          (cases expression consequent
                                            (const-exp (datum) (= datum 1))
                                            (else #f))
                                          (cases expression alternative
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (self-exp () #t)
                                                     (else #f))
                                                   (eq? method-name 'odd)
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'n))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'odd)
                                 (equal? parameters (list 'n))
                                 (cases expression body
                                   (if-exp (condition consequent alternative)
                                     (and (cases expression condition
                                            (zero?-exp (testee)
                                               (cases expression testee
                                                 (var-exp (id) (eq? id 'n))
                                                 (else #f)))
                                            (else #f))
                                          (cases expression consequent
                                            (const-exp (datum) (zero? datum))
                                            (else #f))
                                          (cases expression alternative
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (self-exp () #t)
                                                     (else #f))
                                                   (eq? method-name 'even)
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'n))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (null? (cdr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o1))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'oddeven)
                                 (null? operands)))
                          (else #f))
                        (null? (cdr bound-values))
                        (cases expression body
                          (method-call-exp (recipient method-name operands)
                            (and (cases expression recipient
                                   (var-exp (id) (eq? id 'o1))
                                   (else #f))
                                 (eq? method-name 'odd)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (const-exp (datum) (= datum 13))
                                   (else #f))))
                          (else #f))))
                 (else #f)))))))

  (test scan&parse:page-330
    (scan&parse "class point extends object
                  field x
                  field y
                  method initialize (initx, inity)
                   begin
                    set x = initx;
                    set y = inity
                   end
                  method move (dx, dy)
                   begin
                    set x = +(x,dx);
                    set y = +(y,dy)
                   end
                  method get-location () list(x,y)
                 class colorpoint extends point
                  field color
                  method set-color (c) set color = c
                  method get-color () color
                let p = new point(3,4)
                    cp = new colorpoint(10,20)
                in begin
                    send p move(3,4);
                    send cp set-color(87);
                    send cp move(10,20);
                    list(send p get-location(),      % returns (6 8)
                         send cp get-location(),     % returns (20 40)
                         send cp get-color())        % returns 87
                   end")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'point)
                        (eq? super-name 'object)
                        (equal? field-names (list 'x 'y))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'initx 'inity))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'x)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'initx))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'y)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'inity))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'move)
                                 (equal? parameters (list 'dx 'dy))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'x)
                                                   (cases expression source
                                                     (sum-exp (augend addend)
                                                       (and (cases expression augend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression addend
                                                              (var-exp (id) (eq? id 'dx))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'y)
                                                   (cases expression source
                                                     (sum-exp (augend addend)
                                                       (and (cases expression augend
                                                              (var-exp (id) (eq? id 'y))
                                                              (else #f))
                                                            (cases expression addend
                                                              (var-exp (id) (eq? id 'dy))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'get-location)
                                 (null? parameters)
                                 (cases expression body
                                   (list-exp (elements)
                                     (and (pair? elements)
                                          (cases expression (car elements)
                                            (var-exp (id) (eq? id 'x))
                                            (else #f))
                                          (pair? (cdr elements))
                                          (cases expression (cadr elements)
                                            (var-exp (id) (eq? id 'y))
                                            (else #f))
                                          (null? (cddr elements))))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'colorpoint)
                        (eq? super-name 'point)
                        (equal? field-names (list 'color))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'set-color)
                                 (equal? parameters (list 'c))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'color)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'c))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'get-color)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'color))
                                   (else #f)))))
                        (null? (cddr method-decls)))))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'p 'cp))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'point)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (const-exp (datum) (= datum 3))
                                   (else #f))
                                 (pair? (cdr operands))
                                 (cases expression (cadr operands)
                                   (const-exp (datum) (= datum 4))
                                   (else #f))
                                 (null? (cddr operands))))
                          (else #f))
                        (pair? (cdr bound-values))
                        (cases expression (cadr bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'colorpoint)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (const-exp (datum) (= datum 10))
                                   (else #f))
                                 (pair? (cdr operands))
                                 (cases expression (cadr operands)
                                   (const-exp (datum) (= datum 20))
                                   (else #f))
                                 (null? (cddr operands))))
                          (else #f))
                        (null? (cddr bound-values))
                        (cases expression body
                          (begin-exp (starter sequence)
                            (and (cases expression starter
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'p))
                                            (else #f))
                                          (eq? method-name 'move)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 3))
                                            (else #f))
                                          (pair? (cdr operands))
                                          (cases expression (cadr operands)
                                            (const-exp (datum) (= datum 4))
                                            (else #f))
                                          (null? (cddr operands))))
                                   (else #f))
                                 (pair? sequence)
                                 (cases expression (car sequence)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'cp))
                                            (else #f))
                                          (eq? method-name 'set-color)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 87))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (pair? (cdr sequence))
                                 (cases expression (cadr sequence)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'cp))
                                            (else #f))
                                          (eq? method-name 'move)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 10))
                                            (else #f))
                                          (pair? (cdr operands))
                                          (cases expression (cadr operands)
                                            (const-exp (datum) (= datum 20))
                                            (else #f))
                                          (null? (cddr operands))))
                                   (else #f))
                                 (pair? (cddr sequence))
                                 (cases expression (caddr sequence)
                                   (list-exp (elements)
                                     (and (pair? elements)
                                          (cases expression (car elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'p))
                                                     (else #f))
                                                   (eq? method-name 'get-location)
                                                   (null? operands)))
                                            (else #f))
                                          (pair? (cdr elements))
                                          (cases expression (cadr elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'cp))
                                                     (else #f))
                                                   (eq? method-name 'get-location)
                                                   (null? operands)))
                                            (else #f))
                                          (pair? (cddr elements))
                                          (cases expression (caddr elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'cp))
                                                     (else #f))
                                                   (eq? method-name 'get-color)
                                                   (null? operands)))
                                            (else #f))
                                          (null? (cdddr elements))))
                                   (else #f))
                                 (null? (cdddr sequence))))
                          (else #f))))
                 (else #f)))))))

  (test scan&parse:page-331
    (scan&parse "class c1 extends object
                  field x
                  field y
                  method initialize () 1
                  method setx1 (v) set x = v
                  method sety1 (v) set y = v
                  method getx1 () x
                  method gety1 () y
                 class c2 extends c1
                  field y
                  method sety2 (v) set y = v
                  method getx2 () x
                  method gety2 () y
                let o2 = new c2()
                in begin
                    send o2 setx1 (101);
                    send o2 sety1 (102);
                    send o2 sety2 (999);
                    list(send o2 getx1(),     % returns 101
                         send o2 gety1(),     % returns 102
                         send o2 getx2(),     % returns 101
                         send o2 gety2())     % returns 999
                   end")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'c1)
                        (eq? super-name 'object)
                        (equal? field-names (list 'x 'y))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 1))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'setx1)
                                 (equal? parameters (list 'v))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'x)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'v))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'sety1)
                                 (equal? parameters (list 'v))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'y)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'v))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdddr method-decls))
                        (cases method-declaration (cadddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'getx1)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'x))
                                   (else #f)))))
                        (let ((rest-of-method-decls (cddddr method-decls)))
                          (and (pair? rest-of-method-decls)
                               (cases method-declaration (car rest-of-method-decls)
                                 (a-method-declaration (method-name parameters body)
                                   (and (eq? method-name 'gety1)
                                        (null? parameters)
                                        (cases expression body
                                          (var-exp (id) (eq? id 'y))
                                          (else #f)))))
                               (null? (cdr rest-of-method-decls)))))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'c2)
                        (eq? super-name 'c1)
                        (equal? field-names (list 'y))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'sety2)
                                 (equal? parameters (list 'v))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'y)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'v))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'getx2)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'x))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'gety2)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'y))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (null? (cddr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o2))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'c2)
                                 (null? operands)))
                          (else #f))
                        (null? (cdr bound-values))
                        (cases expression body
                          (begin-exp (starter sequence)
                            (and (cases expression starter
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o2))
                                            (else #f))
                                          (eq? method-name 'setx1)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 101))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (pair? sequence)
                                 (cases expression (car sequence)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o2))
                                            (else #f))
                                          (eq? method-name 'sety1)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 102))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (pair? (cdr sequence))
                                 (cases expression (cadr sequence)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o2))
                                            (else #f))
                                          (eq? method-name 'sety2)
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (const-exp (datum) (= datum 999))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (pair? (cddr sequence))
                                 (cases expression (caddr sequence)
                                   (list-exp (elements)
                                     (and (pair? elements)
                                          (cases expression (car elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o2))
                                                     (else #f))
                                                   (eq? method-name 'getx1)
                                                   (null? operands)))
                                            (else #f))
                                          (pair? (cdr elements))
                                          (cases expression (cadr elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o2))
                                                     (else #f))
                                                   (eq? method-name 'gety1)
                                                   (null? operands)))
                                            (else #f))
                                          (pair? (cddr elements))
                                          (cases expression (caddr elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o2))
                                                     (else #f))
                                                   (eq? method-name 'getx2)
                                                   (null? operands)))
                                            (else #f))
                                          (pair? (cdddr elements))
                                          (cases expression (cadddr elements)
                                            (method-call-exp (recipient method-name operands)
                                              (and (cases expression recipient
                                                     (var-exp (id) (eq? id 'o2))
                                                     (else #f))
                                                   (eq? method-name 'gety2)
                                                   (null? operands)))
                                            (else #f))
                                          (null? (cddddr elements))))
                                   (else #f))))
                          (else #f))))
                 (else #f)))))))

  (test scan&parse:page-332
    (scan&parse "class c1 extends object
                  method initialize () 1
                  method m1 () 11
                  method m2 () send self m1()
                 class c2 extends c1
                  method m1 () 22
                 let o1 = new c1() o2 = new c2()
                 in list(send o1 m1(), send o2 m1(), send o2 m2())")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'c1)
                        (eq? super-name 'object)
                        (null? field-names)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 1))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm1)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 11))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm2)
                                 (null? parameters)
                                 (cases expression body
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (self-exp () #t)
                                            (else #f))
                                          (eq? method-name 'm1)
                                          (null? operands)))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'c2)
                        (eq? super-name 'c1)
                        (null? field-names)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm1)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 22))
                                   (else #f)))))
                        (null? (cdr method-decls)))))
               (null? (cddr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o1 'o2))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'c1)
                                 (null? operands)))
                          (else #f))
                        (pair? (cdr bound-values))
                        (cases expression (cadr bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'c2)
                                 (null? operands)))
                          (else #f))
                        (null? (cddr bound-values))
                        (cases expression body
                          (list-exp (elements)
                            (and (pair? elements)
                                 (cases expression (car elements)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o1))
                                            (else #f))
                                          (eq? method-name 'm1)
                                          (null? operands)))
                                   (else #f))
                                 (pair? (cdr elements))
                                 (cases expression (cadr elements)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o2))
                                            (else #f))
                                          (eq? method-name 'm1)
                                          (null? operands)))
                                   (else #f))
                                 (pair? (cddr elements))
                                 (cases expression (caddr elements)
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (var-exp (id) (eq? id 'o2))
                                            (else #f))
                                          (eq? method-name 'm2)
                                          (null? operands)))
                                   (else #f))
                                 (null? (cdddr elements))))
                          (else #f))))
                 (else #f)))))))

  (test scan&parse:page-333
    (scan&parse "class point extends object
                  field x
                  field y
                  method initialize (initx, inity)
                   begin
                    set x = initx;
                    set y = inity
                   end
                  method move (dx, dy)
                   begin
                    set x = +(x,dx);
                    set y = +(y,dy)
                   end
                  method get-location () list(x,y)
                 class colorpoint extends point
                  field color
                  method initialize (initx, inity, initcolor)
                   begin
                    set x = initx;
                    set y = inity;
                    set color = initcolor
                   end
                  method set-color (c) set color = c
                  method get-color () color
                let o1 = new colorpoint(3,4,172)
                in send o1 get-color()")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'point)
                        (eq? super-name 'object)
                        (equal? field-names (list 'x 'y))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'initx 'inity))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'x)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'initx))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'y)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'inity))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'move)
                                 (equal? parameters (list 'dx 'dy))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'x)
                                                   (cases expression source
                                                     (sum-exp (augend addend)
                                                       (and (cases expression augend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression addend
                                                              (var-exp (id) (eq? id 'dx))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'y)
                                                   (cases expression source
                                                     (sum-exp (augend addend)
                                                       (and (cases expression augend
                                                              (var-exp (id) (eq? id 'y))
                                                              (else #f))
                                                            (cases expression addend
                                                              (var-exp (id) (eq? id 'dy))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr sequence))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'get-location)
                                 (null? parameters)
                                 (cases expression body
                                   (list-exp (elements)
                                     (and (pair? elements)
                                          (cases expression (car elements)
                                            (var-exp (id) (eq? id 'x))
                                            (else #f))
                                          (pair? (cdr elements))
                                          (cases expression (cadr elements)
                                            (var-exp (id) (eq? id 'y))
                                            (else #f))
                                          (null? (cddr elements))))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name field-names method-decls)
                   (and (eq? class-name 'colorpoint)
                        (eq? super-name 'point)
                        (equal? field-names (list 'color))
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (equal? parameters (list 'initx 'inity 'initcolor))
                                 (cases expression body
                                   (begin-exp (starter sequence)
                                     (and (cases expression starter
                                            (assign-exp (target source)
                                              (and (eq? target 'x)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'initx))
                                                     (else #f))))
                                            (else #f))
                                          (pair? sequence)
                                          (cases expression (car sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'y)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'inity))
                                                     (else #f))))
                                            (else #f))
                                          (pair? (cdr sequence))
                                          (cases expression (cadr sequence)
                                            (assign-exp (target source)
                                              (and (eq? target 'color)
                                                   (cases expression source
                                                     (var-exp (id) (eq? id 'initcolor))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cddr sequence))))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'set-color)
                                 (equal? parameters (list 'c))
                                 (cases expression body
                                   (assign-exp (target source)
                                     (and (eq? target 'color)
                                          (cases expression source
                                            (var-exp (id) (eq? id 'c))
                                            (else #f))))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'get-color)
                                 (null? parameters)
                                 (cases expression body
                                   (var-exp (id) (eq? id 'color))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o1))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'colorpoint)
                                 (pair? operands)
                                 (cases expression (car operands)
                                   (const-exp (datum) (= datum 3))
                                   (else #f))
                                 (pair? (cdr operands))
                                 (cases expression (cadr operands)
                                   (const-exp (datum) (= datum 4))
                                   (else #f))
                                 (pair? (cddr operands))
                                 (cases expression (caddr operands)
                                   (const-exp (datum) (= datum 172))
                                   (else #f))
                                 (null? (cdddr operands))))
                          (else #f))
                        (null? (cdr bound-values))
                        (cases expression body
                          (method-call-exp (recipient method-name operands)
                            (and (cases expression recipient
                                   (var-exp (id) (eq? id 'o1))
                                   (else #f))
                                 (eq? method-name 'get-color)
                                 (null? operands))))))
                 (else #f)))))))

  (test scan&parse:page-334
    (scan&parse "class c1 extends object
                  method initialize () 1
                  method m1 () send self m2()
                  method m2 () 13
                 class c2 extends c1
                  method m1 () 22
                  method m2 () 23
                  method m3 () super m1()
                 class c3 extends c2
                  method m1 () 32
                  method m2 () 33
                 let o3 = new c3()
                 in send o3 m3()")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (class-decls body)
          (and (pair? class-decls)
               (cases class-declaration (car class-decls)
                 (a-class-declaration (class-name super-name fields method-decls)
                   (and (eq? class-name 'c1)
                        (eq? super-name 'object)
                        (null? fields)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'initialize)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 1))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm1)
                                 (null? parameters)
                                 (cases expression body
                                   (method-call-exp (recipient method-name operands)
                                     (and (cases expression recipient
                                            (self-exp () #t)
                                            (else #f))
                                          (eq? method-name 'm2)
                                          (null? operands)))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm2)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 13))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (pair? (cdr class-decls))
               (cases class-declaration (cadr class-decls)
                 (a-class-declaration (class-name super-name fields method-decls)
                   (and (eq? class-name 'c2)
                        (eq? super-name 'c1)
                        (null? fields)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm1)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 22))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm2)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 23))
                                   (else #f)))))
                        (pair? (cddr method-decls))
                        (cases method-declaration (caddr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm3)
                                 (null? parameters)
                                 (cases expression body
                                   (super-call-exp (method-name operands)
                                     (and (eq? method-name 'm1)
                                          (null? operands)))
                                   (else #f)))))
                        (null? (cdddr method-decls)))))
               (pair? (cddr class-decls))
               (cases class-declaration (caddr class-decls)
                 (a-class-declaration (class-name super-name fields method-decls)
                   (and (eq? class-name 'c3)
                        (eq? super-name 'c2)
                        (null? fields)
                        (pair? method-decls)
                        (cases method-declaration (car method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm1)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 32))
                                   (else #f)))))
                        (pair? (cdr method-decls))
                        (cases method-declaration (cadr method-decls)
                          (a-method-declaration (method-name parameters body)
                            (and (eq? method-name 'm2)
                                 (null? parameters)
                                 (cases expression body
                                   (const-exp (datum) (= datum 33))
                                   (else #f)))))
                        (null? (cddr method-decls)))))
               (null? (cdddr class-decls))
               (cases expression body
                 (let-exp (bound-vars bound-values body)
                   (and (equal? bound-vars (list 'o3))
                        (pair? bound-values)
                        (cases expression (car bound-values)
                          (new-object-exp (class-name operands)
                            (and (eq? class-name 'c3)
                                 (null? operands)))
                          (else #f))
                        (null? (cdr bound-values))
                        (cases expression body
                          (method-call-exp (recipient method-name operands)
                            (and (cases expression recipient
                                   (var-exp (id) (eq? id 'o3))
                                   (else #f))
                                 (eq? method-name 'm3)
                                 (null? operands)))
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

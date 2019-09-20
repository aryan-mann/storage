#!r7rs

;;; Tests for the (TYPED-OO interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 8, 2015
;;; last revised August 1, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities eopl)
        (CLASSES stores)
        (TYPED-OO expvals-and-environments)
        (TYPED-OO interpreter))

(suite run ()

  ;; Minimal expressions of different variants

  (test run:constant
    (run "0")
    1 (expval?)
    (lambda (result)
      (zero? (expval->num result))))

  (test run:diff-exp
    (run "-(1,2)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -1)))

  (test run:zero?-exp-true
    (run "zero?(0)")
    1 (expval?)
    (lambda (result)
      (true? (expval->bool result))))

  (test run:zero?-exp-false
    (run "zero?(3)")
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result))))

  (test run:if-exp-selecting-consequent
    (run "if zero?(0) then 4 else 5")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 4)))

  (test run:if-exp-selecting-alternate
    (run "if zero?(6) then 7 else 8")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 8)))

  (test run:let-exp-no-bindings
    (run "let in 9")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 9)))

  (test run:let-exp-single-binding
    (run "let a = 10 in a")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 10)))

  (test run:let-exp-multiple-bindings
    (run "let b = 11
              c = 12
              d = 13
          in -(b,-(c,d))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 12)))
    
  (test run:proc-exp-no-parameters
    (run "proc () 14")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameters body env)
          (and (null? parameters)
               (cases expression body
                 (const-exp (datum) (= datum 14))
                 (else #f))
               (cases environment env
                 (empty-env () #t)
                 (else #f)))))))

  (test run:proc-exp-single-parameter
    (run "proc (e: int) 15")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameters body env)
          (and (equal? parameters (list 'e))
               (cases expression body
                 (const-exp (datum) (= datum 15))
                 (else #f))
               (cases environment env
                 (empty-env () #t)
                 (else #f)))))))

  (test run:proc-exp-multiple-parameters
    (run "proc (f: bool, g: (bool * int -> int), h: listof int) 16")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameters body env)
          (and (equal? parameters (list 'f 'g 'h))
               (cases expression body
                 (const-exp (datum) (= datum 16))
                 (else #f))
               (cases environment env
                 (empty-env () #t)
                 (else #f)))))))

  (test run:call-exp-no-arguments
    (run "(proc () 17)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 17)))

  (test run:call-exp-single-argument
    (run "(proc (i: int) -(i,18) 19)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 1)))

  (test run:call-exp-multiple-arguments
    (run "(proc (j: int, k: int, l:int) -(j,-(k,l)) 20 21 22)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 21)))

  (test run:letrec-exp-minimal
    (run "letrec in 23")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 23)))

  (test run:letrec-exp-single-binding
    (run "letrec int m(n: int) = if zero?(n) then 24 else (m -(n, 1))
          in (m 25)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 24)))
                            
  (test run:letrec-exp-multiple-bindings
    (run "letrec int o() = (p 26)
                 int p(q: int) = if zero?(q) then 27 else (r -(q,28) 29 30)
                 int r(s: int, t: int, u: int) = (p -(s,-(t,-(u,-1))))
          in (o)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 27)))

  (test run:assign-exp
    (run "let v = 31 in set v = 32")
    1 (expval?)
    (lambda (result)
      (eq? (species result) 'void-val)))

  (test run:begin-exp-minimal
    (run "begin 33 end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 33)))

  (test run:begin-exp-larger
    (run "begin 34; 35; 36; 37 end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 37)))

  (test run:sum-exp
    (run "+(38,39)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 77)))

  (test run:product-exp
    (run "*(40,41)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 1640)))

  (test run:emptylist-exp
    (run "emptylist_listof bool")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:cons-exp
    (run "cons(42,emptylist_int)")
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 42)
             (null? (cdr elms))))))

  (test run:car-exp
    (run "car(cons(43,emptylist_int))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 43)))

  (test run:cdr-exp
    (run "cdr(cons(44,emptylist_int))")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:null?-exp:yes
    (run "null?(emptylist_bool)")
    1 (expval?)
    expval->bool)

  (test run:null?-exp:no
    (run "null?(cons(45,emptylist_int))")
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result))))

  (test run:list-exp-single-operand
    (run "list(46)")
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 46)
             (null? (cdr elms))))))

  (test run:list-exp-multiple-operands
    (run "list(47,48,49)")
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 47)
             (pair? (cdr elms))
             (= (expval->num (cadr elms)) 48)
             (pair? (cddr elms))
             (= (expval->num (caddr elms)) 49)
             (null? (cdddr elms))))))

  (test run:new-object-no-fields-no-operands
    (run "class w extends object
           method int initialize() 50
          new w()")
    1 (expval?)
    (lambda (result)
      (cases object (expval->obj result)
        (an-object (class-name fields)
          (and (eq? class-name 'w)
               (null? fields))))))

  (test run:new-object-multiple-fields-multiple-operands
    (run "class x extends object
           field int y
           field int z
           field int a
           method void initialize(b: int, c: int, d: int, e:int)
            begin
             set y = b;
             set z = c;
             set a = +(d,e)
            end
          new x(52, 53, 54, 55)")
    1 (expval?)
    (lambda (result)
      (cases object (expval->obj result)
        (an-object (class-name fields)
          (and (eq? class-name 'x)
               (pair? fields)
               (= (expval->num (deref (car fields))) 52)
               (pair? (cdr fields))
               (= (expval->num (deref (cadr fields))) 53)
               (pair? (cddr fields))
               (= (expval->num (deref (caddr fields))) 109)
               (null? (cdddr fields)))))))

  (test run:method-call-exp-no-operands
    (run "class f extends object
           method int initialize() 56
           method int g() 57
          let h = new f()
          in send h g()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 57)))

  (test run:method-call-exp-single-operand
    (run "class i extends object
           method int initialize() 58
           method int j(k: int) +(k,59)
          let l = new i()
          in send l j(60)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 119)))

  (test run:method-call-exp-multiple-operands
    (run "class m extends object
           method int initialize() 61
           method int n(o: int, p: int, q: int) +(o,+(p,q))
          let q = new m()
          in send q n(61, 62, 63)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 186)))

  (test run:super-call-exp-no-operands
    (run "class r extends object
           method int initialize() 64
           method int s() 65
          class t extends r
           method int u(v: int) +(super s(),v)
          let w = new t()
          in send w u(66)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 131)))

  (test run:super-call-exp-single-operand
    (run "class x extends object
           method int initialize() 67
           method int y(z: int) +(z,68)
          class a extends x
           method int b() super y(69)
          let c = new a()
          in send c b()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 137)))

  (test run:super-call-exp-multiple-operands
    (run "class d extends object
           method int initialize() 70
           method int e(f: int, g: int, h:int) +(f,+(g,h))
          class i extends d
           method int j() super e(71,72,73)
          let k = new i()
          in send k j()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 216)))

  (test run:self-exp
    (run "class l extends object
           field int m
           method void initialize(n: int) set m = +(n,74)
           method l o() self
          let p = new l(75)
          in send p o()")
    1 (expval?)
    (lambda (result)
      (cases object (expval->obj result)
        (an-object (class-name fields)
          (and (eq? class-name 'l)
               (pair? fields)
               (= (expval->num (deref (car fields))) 149)
               (null? (cdr fields)))))))

  (test run:cast-exp
    (run "interface v
           method int w(x: int)
          class y extends object implements v
           field int z
           method void initialize(a: int)
            set z = a
           method int w(x: int)
            +(z,x)
           let b = proc (c: v)
                    if instanceof c y
                    then send cast c y w(33)
                    else 34
               d = new y(35)
           in (b d)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 68)))

  (test run:instanceof-exp
    (run "class e extends object
           field int f
           method void initialize(g: int)
            set f = g
          class h extends e
          let i = new h(36)
          in instanceof i e")
    1 (expval?)
    (lambda (result)
      (true? (expval->bool result))))

  (test run:page-354
    (run "interface tree
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
    1 (expval?)
    (lambda (result)
      (cases expval result
        (list-val (elements)
          (and (pair? elements)
               (= (expval->num (car elements)) 12)
               (pair? (cdr elements))
               (= (expval->num (cadr elements)) 100)
               (null? (cddr elements))))))))


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

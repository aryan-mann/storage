#!r7rs

;;; Tests for the (CLASSES interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2015
;;; last revised August 1, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities eopl)
        (CLASSES stores)
        (CLASSES expvals-and-environments)
        (CLASSES interpreter))

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
    (run "proc (e) 15")
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
    (run "proc (f, g, h) 16")
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
    (run "(proc (i) -(i,18) 19)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 1)))

  (test run:call-exp-multiple-arguments
    (run "(proc (j, k, l) -(j,-(k,l)) 20 21 22)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 21)))

  (test run:letrec-exp-minimal
    (run "letrec in 23")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 23)))

  (test run:letrec-exp-single-binding
    (run "letrec m(n) = if zero?(n) then 24 else (m -(n, 1))
          in (m 25)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 24)))
                            
  (test run:letrec-exp-multiple-bindings
    (run "letrec o() = (p 26)
                 p(q) = if zero?(q) then 27 else (r -(q,28) 29 30)
                 r(s, t, u) = (p -(s,-(t,-(u,-1))))
          in (o)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 27)))

  (test run:assign-exp
    (run "let v = 31 in set v = 32")
    1 (expval?)
    (lambda (result)
      (and (= (expval->num result) 27)
           (= (expval->num (list-ref (get-store) 0)) 32))))

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
    (run "emptylist")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:cons-exp
    (run "cons(42,emptylist)")
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 42)
             (null? (cdr elms))))))

  (test run:car-exp
    (run "car(cons(43,emptylist))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 43)))

  (test run:cdr-exp
    (run "cdr(cons(44,emptylist))")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:null?-exp:yes
    (run "null?(emptylist)")
    1 (expval?)
    expval->bool)

  (test run:null?-exp:no
    (run "null?(cons(45,emptylist))")
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result))))

  (test run:list-exp-no-operands
    (run "list()")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

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
           method initialize() 50
          new w()")
    1 (expval?)
    (lambda (result)
      (cases object (expval->obj result)
        (an-object (class-name fields)
          (and (eq? class-name 'w)
               (null? fields))))))

  (test run:new-object-multiple-fields-multiple-operands
    (run "class x extends object
           field y
           field z
           field a
           method initialize(b, c, d, e)
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
           method initialize() 56
           method g() 57
          let h = new f()
          in send h g()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 57)))

  (test run:method-call-exp-single-operand
    (run "class i extends object
           method initialize() 58
           method j(k) +(k,59)
          let l = new i()
          in send l j(60)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 119)))

  (test run:method-call-exp-multiple-operands
    (run "class m extends object
           method initialize() 61
           method n(o, p, q) +(o,+(p,q))
          let q = new m()
          in send q n(61, 62, 63)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 186)))

  (test run:super-call-exp-no-operands
    (run "class r extends object
           method initialize() 64
           method s() 65
          class t extends r
           method u(v) +(super s(),v)
          let w = new t()
          in send w u(66)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 131)))

  (test run:super-call-exp-single-operand
    (run "class x extends object
           method initialize() 67
           method y(z) +(z,68)
          class a extends x
           method b() super y(69)
          let c = new a()
          in send c b()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 137)))

  (test run:super-call-exp-multiple-operands
    (run "class d extends object
           method initialize() 70
           method e(f,g,h) +(f,+(g,h))
          class i extends d
           method j() super e(71,72,73)
          let k = new i()
          in send k j()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 216)))

  (test run:self-exp
    (run "class l extends object
           field m
           method initialize(n) set m = +(n,74)
           method o() self
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

  (test run:page-327
    (run "class c1 extends object
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
             end")
    1 (expval?)
    (lambda (result)
      (let ((outer-elements (expval->elements result)))
        (and (pair? outer-elements)
             (let ((first-inner-elements (expval->elements (car outer-elements))))
               (and (pair? first-inner-elements)
                    (= (expval->num (car first-inner-elements)) 3)
                    (pair? (cdr first-inner-elements))
                    (= (expval->num (cadr first-inner-elements)) -3)
                    (null? (cddr first-inner-elements))))
             (pair? (cdr outer-elements))
             (let ((second-inner-elements (expval->elements (cadr outer-elements))))
               (and (pair? second-inner-elements)
                    (= (expval->num (car second-inner-elements)) 5)
                    (pair? (cdr second-inner-elements))
                    (= (expval->num (cadr second-inner-elements)) -5)
                    (null? (cddr second-inner-elements))))
             (null? (cddr outer-elements))))))

  (test run:page-328
    (run "class interior-node extends object
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
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 12)))

  (test run:page-329
    (run "class oddeven extends object
           method initialize () 1
           method even (n)
            if zero?(n) then 1 else send self odd(-(n,1))
           method odd (n)
            if zero?(n) then 0 else send self even(-(n,1))
           let o1 = new oddeven()
           in send o1 odd(13)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 1)))

  (test run:page-330
    (run "class point extends object
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
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (let ((first-inner-elements (expval->elements (car elms))))
               (and (pair? first-inner-elements)
                    (= (expval->num (car first-inner-elements)) 6)
                    (pair? (cdr first-inner-elements))
                    (= (expval->num (cadr first-inner-elements)) 8)
                    (null? (cddr first-inner-elements))))
             (pair? (cdr elms))
             (let ((second-inner-elements (expval->elements (cadr elms))))
               (and (pair? second-inner-elements)
                    (= (expval->num (car second-inner-elements)) 20)
                    (pair? (cdr second-inner-elements))
                    (= (expval->num (cadr second-inner-elements)) 40)
                    (null? (cddr second-inner-elements))))
             (pair? (cddr elms))
             (= (expval->num (caddr elms)) 87)
             (null? (cdddr elms))))))

  (test run:page-331
    (run "class c1 extends object
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
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 101)
             (pair? (cdr elms))
             (= (expval->num (cadr elms)) 102)
             (pair? (cddr elms))
             (= (expval->num (caddr elms)) 101)
             (pair? (cdddr elms))
             (= (expval->num (cadddr elms)) 999)
             (null? (cddddr elms))))))

  (test run:page-332
    (run "class c1 extends object
                  method initialize () 1
                  method m1 () 11
                  method m2 () send self m1()
                 class c2 extends c1
                  method m1 () 22
                 let o1 = new c1() o2 = new c2()
                 in list(send o1 m1(), send o2 m1(), send o2 m2())")
    1 (expval?)
    (lambda (result)
      (let ((elms (expval->elements result)))
        (and (pair? elms)
             (= (expval->num (car elms)) 11)
             (pair? (cdr elms))
             (= (expval->num (cadr elms)) 22)
             (pair? (cddr elms))
             (= (expval->num (caddr elms)) 22)
             (null? (cdddr elms))))))

  (test run:pages-332-and-323
    (run "class point extends object
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
                    super initialize(initx, inity);
                    set color = initcolor
                   end
                  method set-color (c) set color = c
                  method get-color () color
                let o1 = new colorpoint(3,4,172)
                in send o1 get-color()")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 172)))
    
  (test run:page-334
    (run "class c1 extends object
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
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 33))))


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

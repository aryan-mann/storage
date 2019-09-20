#!r7rs

;;; Tests for the (CALL parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 5, 2009
;;; last revised July 31, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CALL scanner)
        (CALL syntax-trees)
        (CALL parser))

(suite parse-expression ()

  (test parse-expression:short-numeral
    (parse-expression (scanner (make-character-source "0")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum 0))
        (else #f))))

  (test parse-expression:negative-numeral
    (parse-expression (scanner (make-character-source "-8128")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum -8128))
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
    (parse-expression (scanner (make-character-source "x")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'x))
        (else #f))))

  (test parse-expression:longer-identifier
    (parse-expression (scanner (make-character-source "foo?2")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'foo?2))
        (else #f))))

  (test parse-expression:diff-exp
    (parse-expression (scanner (make-character-source "-(alpha, beta)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (diff-exp (minuend subtrahend)
          (and (cases expression minuend
                 (var-exp (id) (eq? id 'alpha))
                 (else #f))
               (cases expression subtrahend
                 (var-exp (id) (eq? id 'beta))
                 (else #f))))
        (else #f))))
    
  (test parse-expression:zero?-exp
    (parse-expression (scanner (make-character-source "zero?(delta)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (zero?-exp (testee) (cases expression testee
                              (var-exp (id) (eq? id 'delta))
                              (else #f)))
        (else #f))))

  (test parse-expression:if-exp
    (parse-expression (scanner (make-character-source
                                 "if zero?(7) then eta else 8")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (if-exp (condition consequent alternative)
          (and (cases expression condition
                 (zero?-exp (testee) (cases expression testee
                                       (const-exp (datum) (= datum 7))
                                       (else #f)))
                 (else #f))
               (cases expression consequent
                 (var-exp (id) (eq? id 'eta))
                 (else #f))
               (cases expression alternative
                 (const-exp (datum) (= datum 8))
                 (else #f))))
        (else #f))))

  (test parse-expression:let-exp
    (parse-expression (scanner (make-character-source "let mu = nu in 11")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (let-exp (bound-var bound-value body)
          (and (eq? bound-var 'mu)
               (cases expression bound-value
                 (var-exp (id) (eq? id 'nu))
                 (else #f))
               (cases expression body
                 (const-exp (datum) (= datum 11))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp
    (parse-expression (scanner (make-character-source "proc (rho) 15")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameter body)
          (and (eq? parameter 'rho)
               (cases expression body
                 (const-exp (datum) (= datum 15))
                 (else #f))))
        (else #f))))

  (test parse-expression:call-exp
    (parse-expression (scanner (make-character-source "(upsilon 18)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (call-exp (operator operand)
          (and (cases expression operator
                 (var-exp (id) (eq? id 'upsilon))
                 (else #f))
               (cases expression operand
                 (const-exp (datum) (= datum 18))
                 (else #f))))
        (else #f))))

  (test parse-expression:letrec-exp-no-bindings
    (parse-expression
      (scanner (make-character-source "letrec in 76")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (procedure-names parameters procedure-bodies letrec-body)
          (and (null? procedure-names)
               (null? parameters)
               (null? procedure-bodies)
               (cases expression letrec-body
                 (const-exp (datum) (= datum 76))
                 (else #f))))
        (else #f))))

  (test parse-expression:letrec-exp-multiple-bindings
    (parse-expression
      (scanner (make-character-source "letrec fred (plugh) = 80
                                              xyzzy (thud) = 81
                                              foo (bar) = 82
                                              baz (qux) = 83
                                       in quux")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (procedure-names parameters procedure-bodies letrec-body)
          (and (equal? procedure-names '(fred xyzzy foo baz))
               (equal? parameters '(plugh thud bar qux))
               (cases expression (car procedure-bodies)
                 (const-exp (datum) (= datum 80))
                 (else #f))
               (cases expression (cadr procedure-bodies)
                 (const-exp (datum) (= datum 81))
                 (else #f))
               (cases expression (caddr procedure-bodies)
                 (const-exp (datum) (= datum 82))
                 (else #f))
               (cases expression (cadddr procedure-bodies)
                 (const-exp (datum) (= datum 83))
                 (else #f))
               (cases expression letrec-body
                 (var-exp (id) (eq? id 'quux))
                 (else #f))))
        (else #f))))

  (test parse-expression:assign-exp
    (parse-expression (scanner (make-character-source "set bazola = 88")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (assign-exp (target source)
          (and (eq? target 'bazola)
               (cases expression source
                 (const-exp (datum) (= datum 88))
                 (else #f))))
        (else #f))))

  (test parse-expression:begin-exp-minimal
    (parse-expression (scanner (make-character-source "begin bongo end")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (begin-exp (starter sequence)
          (and (cases expression starter
                 (var-exp (id) (eq? id 'bongo))
                 (else #f))
               (null? sequence)))
        (else #f))))

  (test parse-expression:begin-exp-typical
    (parse-expression
      (scanner (make-character-source "begin zot; 94; blarg; 95 end")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (begin-exp (starter sequence)
          (and (cases expression starter
                 (var-exp (id) (eq? id 'zot))
                 (else #f))
               (pair? sequence)
               (cases expression (car sequence)
                 (const-exp (datum) (= datum 94))
                 (else #f))
               (pair? (cdr sequence))
               (cases expression (cadr sequence)
                 (var-exp (id) (eq? id 'blarg))
                 (else #f))
               (pair? (cddr sequence))
               (cases expression (caddr sequence)
                 (const-exp (datum) (= datum 95))
                 (else #f))
               (null? (cdddr sequence))))
        (else #f)))))


(suite parse-program ()

  (test parse-program:all-in-one
    (parse-program (scanner (make-character-source
                              "if zero?(phi)
                                  then let chi = 16
                                       in begin
                                            set chi = 17 ;
                                            -(phi,chi)
                                          end
                                  else letrec gimel (daleth) = 18
                                              he (waw) = 19
                                       in (proc (aleph) 17 beth)")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (if-exp (condition consequent alternative)
              (and (cases expression condition
                     (zero?-exp (testee) (cases expression testee
                                           (var-exp (id) (eq? id 'phi))
                                           (else #f)))
                     (else #f))
                   (cases expression consequent
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'chi)
                            (cases expression bound-value
                              (const-exp (datum) (= datum 16))
                              (else #f))
                            (cases expression body
                              (begin-exp (starter sequence)
                                (and (cases expression starter
                                       (assign-exp (target source)
                                         (and (eq? target 'chi)
                                              (cases expression source
                                                (const-exp (datum) (= datum 17))
                                                (else #f))))
                                       (else #f))
                                     (pair? sequence)
                                     (cases expression (car sequence)
                                       (diff-exp (minuend subtrahend)
                                         (and (cases expression minuend
                                                (var-exp (id) (eq? id 'phi))
                                                (else #f))
                                              (cases expression subtrahend
                                                (var-exp (id) (eq? id 'chi))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression alternative
                     (letrec-exp (procedure-names parameters procedure-bodies
                                  letrec-body)
                       (and (equal? procedure-names '(gimel he))
                            (equal? parameters '(daleth waw))
                            (cases expression (car procedure-bodies)
                              (const-exp (datum) (= datum 18))
                              (else #f))
                            (cases expression (cadr procedure-bodies)
                              (const-exp (datum) (= datum 19))
                              (else #f))
                            (cases expression letrec-body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (proc-exp (parameter body)
                                         (and (eq? parameter 'aleph)
                                              (cases expression body
                                                (const-exp (datum) (= datum 17))
                                                (else #f))))
                                       (else #f))
                                     (cases expression operand
                                       (var-exp (id) (eq? id 'beth))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f)))))))


(suite scan&parse ()

  (test scan&parse:minimal
    (scan&parse "18")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (const-exp (datum) (= datum 18))
            (else #f))))))

  (test scan&parse:page-131-middle
    (scan&parse "let f = proc (x) set x = 44
                 in let g = proc (y) (f y)
                    in let z = 55
                       in begin (g z); z end")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'f)
                   (cases expression bound-value
                     (proc-exp (parameter body)
                       (and (eq? parameter 'x)
                            (cases expression body
                              (assign-exp (target source)
                                (and (eq? target 'x)
                                     (cases expression source
                                       (const-exp (datum) (= datum 44))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'g)
                            (cases expression bound-value
                              (proc-exp (parameter body)
                                (and (eq? parameter 'y)
                                     (cases expression body
                                       (call-exp (operator operand)
                                         (and (cases expression operator
                                                (var-exp (id) (eq? id 'f))
                                                (else #f))
                                              (cases expression operand
                                                (var-exp (id) (eq? id 'y))
                                                (else #f))))
                                       (else #f))))
                              (else #f))
                            (cases expression body
                              (let-exp (bound-var bound-value body)
                                (and (eq? bound-var 'z)
                                     (cases expression bound-value
                                       (const-exp (datum) (= datum 55))
                                       (else #f))
                                     (cases expression body
                                       (begin-exp (starter sequence)
                                         (and (cases expression starter
                                                (call-exp (operator operand)
                                                  (and (cases expression operator
                                                         (var-exp (id) (eq? id 'g))
                                                         (else #f))
                                                       (cases expression operand
                                                         (var-exp (id) (eq? id 'z))
                                                         (else #f))))
                                                (else #f))
                                              (pair? sequence)
                                              (cases expression (car sequence)
                                                (var-exp (id) (eq? id 'z))
                                                (else #f))
                                              (null? (cdr sequence))))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f))))))

  (test scan&parse:page-131-bottom
    (scan&parse "let swap = proc (x) proc (y)
                             let temp = x
                             in begin
                                 set x = y;
                                 set y = temp
                                end
                 in let a = 33
                    in let b = 44
                       in begin
                           ((swap a) b);
                           -(a,b)
                          end")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'swap)
                   (cases expression bound-value
                     (proc-exp (parameter body)
                       (and (eq? parameter 'x)
                            (cases expression body
                              (proc-exp (parameter body)
                                (and (eq? parameter 'y)
                                     (cases expression body
                                       (let-exp (bound-var bound-value body)
                                         (and (eq? bound-var 'temp)
                                              (cases expression bound-value
                                                (var-exp (id) (eq? id 'x))
                                                (else #f))
                                              (cases expression body
                                                (begin-exp (starter sequence)
                                                  (and (cases expression starter
                                                         (assign-exp (target source)
                                                           (and (eq? target 'x)
                                                                (cases expression source
                                                                  (var-exp (id) (eq? id 'y))
                                                                  (else #f))))
                                                         (else #f))
                                                       (pair? sequence)
                                                       (cases expression (car sequence)
                                                         (assign-exp (target source)
                                                           (and (eq? target 'y)
                                                                (cases expression source
                                                                  (var-exp (id) (eq? id 'temp))
                                                                  (else #f))))
                                                         (else #f))
                                                       (null? (cdr sequence))))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'a)
                            (cases expression bound-value
                              (const-exp (datum) (= datum 33))
                              (else #f))
                            (cases expression body
                              (let-exp (bound-var bound-value body)
                                (and (eq? bound-var 'b)
                                     (cases expression bound-value
                                       (const-exp (datum) (= datum 44))
                                       (else #f))
                                     (cases expression body
                                       (begin-exp (starter sequence)
                                         (and (cases expression starter
                                                (call-exp (operator operand)
                                                  (and (cases expression operator
                                                         (call-exp (operator operand)
                                                           (and (cases expression operator
                                                                  (var-exp (id) (eq? id 'swap))
                                                                  (else #f))
                                                                (cases expression operand
                                                                  (var-exp (id) (eq? id 'a))
                                                                  (else #f))))
                                                         (else #f))
                                                       (cases expression operand
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))))
                                                (else #f))
                                              (pair? sequence)
                                              (cases expression (car sequence)
                                                (diff-exp (minuend subtrahend)
                                                  (and (cases expression minuend
                                                         (var-exp (id) (eq? id 'a))
                                                         (else #f))
                                                       (cases expression subtrahend
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))))
                                                (else #f))
                                              (null? (cdr sequence))))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f))))))

  (test scan&parse:page-133
    (scan&parse "let b = 3
                 in let p = proc(x) proc(y)
                             begin
                              set x = 4;
                              y
                             end
                    in ((p b) b)")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'b)
                   (cases expression bound-value
                     (const-exp (datum) (= datum 3))
                     (else #f))
                   (cases expression body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'p)
                            (cases expression bound-value
                              (proc-exp (parameter body)
                                (and (eq? parameter 'x)
                                     (cases expression body
                                       (proc-exp (parameter body)
                                         (and (eq? parameter 'y)
                                              (cases expression body
                                                (begin-exp (starter sequence)
                                                  (and (cases expression starter
                                                         (assign-exp (target source)
                                                           (and (eq? target 'x)
                                                                (cases expression source
                                                                  (const-exp (datum) (= datum 4))
                                                                  (else #f))))
                                                         (else #f))
                                                       (pair? sequence)
                                                       (cases expression (car sequence)
                                                         (var-exp (id) (eq? id 'y))
                                                         (else #f))
                                                       (null? (cdr sequence))))
                                                (else #f))))
                                       (else #f))))
                              (else #f))
                            (cases expression body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (call-exp (operator operand)
                                         (and (cases expression operator
                                                (var-exp (id) (eq? id 'p))
                                                (else #f))
                                              (cases expression operand
                                                (var-exp (id) (eq? id 'b))
                                                (else #f))))
                                       (else #f))
                                     (cases expression operand
                                       (var-exp (id) (eq? id 'b))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f))))))

  (test scan&parse:page-136
    (scan&parse "letrec infinite-loop (x) = (infinite-loop -(x,-1))
                 in let f = proc (z) 11
                    in (f (infinite-loop 0))")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (letrec-exp (procedure-names parameters procedure-bodies letrec-body)
              (and (equal? procedure-names '(infinite-loop))
                   (equal? parameters '(x))
                   (pair? procedure-bodies)
                   (cases expression (car procedure-bodies)
                     (call-exp (operator operand)
                       (and (cases expression operator
                              (var-exp (id) (eq? id 'infinite-loop))
                              (else #f))
                            (cases expression operand
                              (diff-exp (minuend subtrahend)
                                (and (cases expression minuend
                                       (var-exp (id) (eq? id 'x))
                                       (else #f))
                                     (cases expression subtrahend
                                       (const-exp (datum) (= datum -1))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (null? (cdr procedure-bodies))
                   (cases expression letrec-body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'f)
                            (cases expression bound-value
                              (proc-exp (parameter body)
                                (and (eq? parameter 'z)
                                     (cases expression body
                                       (const-exp (datum) (= datum 11))
                                       (else #f))))
                              (else #f))
                            (cases expression body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) 'f)
                                       (else #f))
                                     (cases expression operand
                                       (call-exp (operator operand)
                                         (and (cases expression operator
                                                (var-exp (id) (eq? id 'infinite-loop))
                                                (else #f))
                                              (cases expression operand
                                                (const-exp (datum) (zero? datum))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f))))))

  (test scan&parse:exercise-4-39
    (scan&parse "let a = 0
                 in let foo = proc (b)
                               -(begin set a = 1; b end,
                                 begin set a = 2; b end)
                    in (foo -(a,1))")                               
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'a)
                   (cases expression bound-value
                     (const-exp (datum) (zero? datum))
                     (else #f))
                   (cases expression body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'foo)
                            (cases expression bound-value
                              (proc-exp (parameter body)
                                (and (eq? parameter 'b)
                                     (cases expression body
                                       (diff-exp (minuend subtrahend)
                                         (and (cases expression minuend
                                                (begin-exp (starter sequence)
                                                  (and (cases expression starter
                                                         (assign-exp (target source)
                                                           (and (eq? target 'a)
                                                                (cases expression source
                                                                  (const-exp (datum) (= datum 1))
                                                                  (else #f))))
                                                         (else #f))
                                                       (pair? sequence)
                                                       (cases expression (car sequence)
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))
                                                       (null? (cdr sequence))))
                                                (else #f))
                                              (cases expression subtrahend
                                                (begin-exp (starter sequence)
                                                  (and (cases expression starter
                                                         (assign-exp (target source)
                                                           (and (eq? target 'a)
                                                                (cases expression source
                                                                  (const-exp (datum) (= datum 2))
                                                                  (else #f))))
                                                         (else #f))
                                                       (pair? sequence)
                                                       (cases expression (car sequence)
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))
                                                       (null? (cdr sequence))))
                                                (else #f))))
                                       (else #f))))
                              (else #f))
                            (cases expression body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) 'foo)
                                       (else #f))
                                     (cases expression operand
                                       (diff-exp (minuend subtrahend)
                                         (and (cases expression minuend
                                                (var-exp (id) (eq? id 'a))
                                                (else #f))
                                              (cases expression subtrahend
                                                (const-exp (datum) (= datum 1))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f)))))))
                                                  

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

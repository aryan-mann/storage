#!r7rs

;;; Tests for the (MUTABLE-PAIRS parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 5, 2009
;;; last revised July 17, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (MUTABLE-PAIRS scanner)
        (MUTABLE-PAIRS syntax-trees)
        (MUTABLE-PAIRS parser))

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
        (else #f))))

  (test parse-expression:newpair-exp
    (parse-expression (scanner (make-character-source "pair(beta,98)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (newpair-exp (left-init right-init)
          (and (cases expression left-init
                 (var-exp (id) (eq? id 'beta))
                 (else #f))
               (cases expression right-init
                 (const-exp (datum) (= datum 98))
                 (else #f))))
        (else #f))))

  (test parse-expression:left-exp
    (parse-expression (scanner (make-character-source "left(epsilon)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (left-exp (pair)
          (cases expression pair
            (var-exp (id) (eq? id 'epsilon))
            (else #f)))
        (else #f))))

  (test parse-expression:right-exp
    (parse-expression (scanner (make-character-source "right(pair(101,eta))")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (right-exp (pair)
          (cases expression pair
            (newpair-exp (left-init right-init)
              (and (cases expression left-init
                     (const-exp (datum) (= datum 101))
                     (else #f))
                   (cases expression right-init
                     (var-exp (id) (eq? id 'eta))
                     (else #f))))
            (else #f)))
        (else #f))))
  
  (test parse-expression:setleft-exp
    (parse-expression (scanner (make-character-source "setleft(mu, 105)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (setleft-exp (target source)
          (and (cases expression target
                 (var-exp (id) (eq? id 'mu))
                 (else #f))
               (cases expression source
                 (const-exp (datum) (= datum 105))
                 (else #f))))
        (else #f))))
  
  (test parse-expression:setright-exp
    (parse-expression (scanner (make-character-source "setright(pi,108)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (setright-exp (target source)
          (and (cases expression target
                 (var-exp (id) (eq? id 'pi))
                 (else #f))
               (cases expression source
                 (const-exp (datum) (= datum 108))
                 (else #f))))
        (else #f)))))



(suite parse-program ()

  (test parse-program:all-in-one
    (parse-program (scanner (make-character-source
                              "let a = pair(2,8)
                               in letrec b (pr)
                                          = if zero?(right(pr))
                                            then pr
                                            else begin
                                                  setleft(pr, right(pr));
                                                  setright(pr, -(left(pr), 1));
                                                  (b pr)
                                                 end
                                 
                                  in begin
                                       set a = (b a);
                                       left(a)
                                     end")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'a)
                   (cases expression bound-value
                     (newpair-exp (left-init right-init)
                       (and (cases expression left-init
                              (const-exp (datum) (= datum 2))
                              (else #f))
                            (cases expression right-init
                              (const-exp (datum) (= datum 8))
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (letrec-exp (procedure-names parameters procedure-bodies letrec-body)
                       (and (equal? procedure-names '(b))
                            (equal? parameters '(pr))
                            (pair? procedure-bodies)
                            (cases expression (car procedure-bodies)
                              (if-exp (condition consequent alternative)
                                (and (cases expression condition
                                       (zero?-exp (testee)
                                         (cases expression testee
                                           (right-exp (pair)
                                             (cases expression pair
                                               (var-exp (id) (eq? id 'pr))
                                               (else #f)))
                                           (else #f)))
                                       (else #f))
                                     (cases expression consequent
                                       (var-exp (id) (eq? id 'pr))
                                       (else #f))
                                     (cases expression alternative
                                       (begin-exp (starter sequence)
                                         (and (cases expression starter
                                                (setleft-exp (target source)
                                                  (and (cases expression target
                                                         (var-exp (id) (eq? id 'pr))
                                                         (else #f))
                                                       (cases expression source
                                                         (right-exp (pair)
                                                           (cases expression pair
                                                             (var-exp (id) (eq? id 'pr))
                                                             (else #f)))
                                                         (else #f))))
                                                (else #f))
                                              (pair? sequence)
                                              (cases expression (car sequence)
                                                (setright-exp (target source)
                                                  (and (cases expression target
                                                         (var-exp (id) (eq? id 'pr))
                                                         (else #f))
                                                       (cases expression source
                                                         (diff-exp (minuend subtrahend)
                                                           (and (cases expression minuend
                                                                  (left-exp (pair)
                                                                    (cases expression pair
                                                                      (var-exp (id) (eq? id 'pr))
                                                                      (else #f)))
                                                                  (else #f))
                                                                (cases expression subtrahend
                                                                  (const-exp (datum) (= datum 1))
                                                                  (else #f))))
                                                         (else #f))))
                                                (else #f))
                                              (pair? (cdr sequence))
                                              (cases expression (cadr sequence)
                                                (call-exp (operator operand)
                                                  (and (cases expression operator
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))
                                                       (cases expression operand
                                                         (var-exp (id) (eq? id 'pr))
                                                         (else #f)))))
                                              (null? (cddr sequence))))
                                       (else #f))))
                              (else #f))
                            (null? (cdr procedure-bodies))
                            (cases expression letrec-body
                              (begin-exp (starter sequence)
                                (and (cases expression starter
                                       (assign-exp (target source)
                                         (and (eq? target 'a)
                                              (cases expression source
                                                (call-exp (operator operand)
                                                  (and (cases expression operator
                                                         (var-exp (id) (eq? id 'b))
                                                         (else #f))
                                                       (cases expression operand
                                                         (var-exp (id) (eq? id 'a))
                                                         (else #f))))
                                                (else #f))))
                                       (else #f))
                                     (pair? sequence)
                                     (cases expression (car sequence)
                                       (left-exp (pair)
                                         (cases expression pair
                                           (var-exp (id) (eq? id 'a))
                                           (else #f)))
                                       (else #f))
                                     (null? (cdr sequence))))
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

  (test scan&parse:swapper
    (scan&parse "let swap = proc (pr)
                             let temp = left(pr)
                             in begin
                                 setleft(pr, right(pr));
                                 setright(pr, temp);
                                 pr
                                end
                 in (swap pair(0,1))")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'swap)
                   (cases expression bound-value
                     (proc-exp (parameter body)
                       (and (eq? parameter 'pr)
                            (cases expression body
                              (let-exp (bound-var bound-value body)
                                (and (eq? bound-var 'temp)
                                     (cases expression bound-value
                                       (left-exp (pair)
                                         (cases expression pair
                                           (var-exp (id) (eq? id 'pr))
                                           (else #f)))
                                       (else #f))
                                     (cases expression body
                                       (begin-exp (starter sequence)
                                         (and (cases expression starter
                                                (setleft-exp (target source)
                                                  (and (cases expression target
                                                         (var-exp (id) (eq? id 'pr))
                                                         (else #f))
                                                       (cases expression source
                                                         (right-exp (pair)
                                                           (cases expression pair
                                                             (var-exp (id) (eq? id 'pr)))))))
                                                (else #f))
                                              (pair? sequence)
                                              (cases expression (car sequence)
                                                (setright-exp (target source)
                                                  (and (cases expression target
                                                         (var-exp (id) (eq? id 'pr))
                                                         (else #f))
                                                       (cases expression source
                                                         (var-exp (id) (eq? id 'temp))
                                                         (else #f))))
                                                (else #f))
                                              (pair? (cdr sequence))
                                              (cases expression (cadr sequence)
                                                (var-exp (id) (eq? id 'pr))
                                                (else #f))
                                              (null? (cddr sequence))))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (call-exp (operator operand)
                       (and (cases expression operator
                              (var-exp (id) (eq? id 'swap))
                              (else #f))
                            (cases expression operand
                              (newpair-exp (left-init right-init)
                                (and (cases expression left-init
                                       (const-exp (datum) (zero? datum))
                                       (else #f))
                                     (cases expression right-init
                                       (const-exp (datum) (= datum 1))
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

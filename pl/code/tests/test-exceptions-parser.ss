#!r7rs

;;; Tests for the (EXCEPTIONS parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 8, 2009
;;; last revised July 24, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (EXCEPTIONS scanner)
        (EXCEPTIONS syntax-trees)
        (EXCEPTIONS parser))

(suite parse-expression ()

  (test parse-expression:try-exp
    (parse-expression
      (scanner (make-character-source "try 22 catch (irritant) 23")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (try-exp (try-body parameter handle-body)
          (and (cases expression try-body
                 (const-exp (datum) (= datum 22))
                 (else #f))
               (eq? parameter 'irritant)
               (cases expression handle-body
                 (const-exp (datum) (= datum 23)))))
        (else #f))))

  (test parse-expression:raise-exp
    (parse-expression (scanner (make-character-source "raise 24")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (raise-exp (body)
          (cases expression body
            (const-exp (datum) (= datum 24))
            (else #f)))
        (else #f))))

  (test parse-expression:cons-exp
    (parse-expression
      (scanner (make-character-source "cons(25,emptylist)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (cons-exp (fore aft)
          (and (cases expression fore
                 (const-exp (datum) 25)
                 (else #f))
               (cases expression aft
                 (emptylist-exp () #t)
                 (else #f))))
        (else #f))))

  (test parse-expression:car-exp
    (parse-expression (scanner (make-character-source "car(alpha)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (car-exp (pair)
          (cases expression pair
            (var-exp (id) (eq? id 'alpha))
            (else #f)))
        (else #f))))

  (test parse-expression:cdr-exp
    (parse-expression (scanner (make-character-source "cdr(beta)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (cdr-exp (pair)
          (cases expression pair
            (var-exp (id) (eq? id 'beta))
            (else #f)))
        (else #f))))

  (test parse-expression:null?-exp
    (parse-expression (scanner (make-character-source "null?(emptylist)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (null?-exp (testee)
          (cases expression testee
            (emptylist-exp () #t)
            (else #f)))
        (else #f))))

  (test parse-expression:emptylist-exp
    (parse-expression (scanner (make-character-source "emptylist")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (emptylist-exp () #t)
        (else #f))))

  (test parse-expression:list-exp-many-elements
    (parse-expression (scanner (make-character-source "list(26,27,28)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements)
          (and (pair? elements)
               (cases expression (car elements)
                 (const-exp (datum) (= datum 26))
                 (else #f))
               (pair? (cdr elements))
               (cases expression (cadr elements)
                 (const-exp (datum) (= datum 27))
                 (else #f))
               (pair? (cddr elements))
               (cases expression (caddr elements)
                 (const-exp (datum) (= datum 28))
                 (else #f))
               (null? (cdddr elements))))
        (else #f))))

  (test parse-expression:list-exp-single-element
    (parse-expression (scanner (make-character-source "list(29)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements)
          (and (pair? elements)
               (cases expression (car elements)
                 (const-exp (datum) (= datum 29))
                 (else #f))
               (null? (cdr elements))))
        (else #f))))

  (test parse-expression:list-exp-no-elements
    (parse-expression (scanner (make-character-source "list()")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (list-exp (elements) (null? elements))
        (else #f)))))
        

(suite parse-program ()

  (test parse-program:all-in-one
    (parse-program (scanner (make-character-source
                              "letrec gamma(delta) = 30
                               in let epsilon = proc (eta) (gamma eta)
                                  in if zero?(-(31, try raise emptylist
                                                    catch (zeta) 32))
                                     then null?(cdr(cons(33, emptylist)))
                                     else car(list(zero?(34),(epsilon 35), 36))")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (letrec-exp (procedure-name parameter procedure-body letrec-body)
              (and (eq? procedure-name 'gamma)
                   (eq? parameter 'delta)
                   (cases expression procedure-body
                     (const-exp (datum) (= datum 30))
                     (else #f))
                   (cases expression letrec-body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'epsilon)
                            (cases expression bound-value
                              (proc-exp (parameter body)
                                (and (eq? parameter 'eta)
                                     (cases expression body
                                       (call-exp (operator operand)
                                         (and (cases expression operator
                                                (var-exp (id) (eq? id 'gamma))
                                                (else #f))
                                              (cases expression operand
                                                (var-exp (id) (eq? id 'eta))
                                                (else #f))))
                                       (else #f))))
                              (else #f))
                            (cases expression body
                              (if-exp (condition consequent alternative)
                                (and (cases expression condition
                                       (zero?-exp (testee)
                                         (cases expression testee
                                           (diff-exp (minuend subtrahend)
                                             (and (cases expression minuend
                                                    (const-exp (datum) (= datum 31))
                                                    (else #f))
                                                  (cases expression subtrahend
                                                    (try-exp (try-body parameter handle-body)
                                                      (and (cases expression try-body
                                                             (raise-exp (body)
                                                               (cases expression body
                                                                 (emptylist-exp () #t)
                                                                 (else #f)))
                                                             (else #f))
                                                           (eq? parameter 'zeta)
                                                           (cases expression handle-body
                                                             (const-exp (datum) (= datum 32))
                                                             (else #f))))
                                                    (else #f))))
                                           (else #f)))
                                       (else #f))
                                     (cases expression consequent
                                       (null?-exp (testee)
                                         (cases expression testee
                                           (cdr-exp (pair)
                                             (cases expression pair
                                               (cons-exp (fore aft)
                                                 (and (cases expression fore
                                                        (const-exp (datum) (= datum 33))
                                                        (else #f))
                                                      (cases expression aft
                                                        (emptylist-exp () #t)
                                                        (else #f))))
                                               (else #f)))
                                           (else #f)))
                                       (else #f))
                                     (cases expression alternative
                                       (car-exp (pair)
                                         (cases expression pair
                                           (list-exp (elements)
                                             (and (pair? elements)
                                                  (cases expression (car elements)
                                                    (zero?-exp (testee)
                                                      (cases expression testee
                                                        (const-exp (datum) (= datum 34))
                                                        (else #f)))
                                                    (else #f))
                                                  (pair? (cdr elements))
                                                  (cases expression (cadr elements)
                                                    (call-exp (operator operand)
                                                      (and (cases expression operator
                                                             (var-exp (id) (eq? id 'epsilon))
                                                             (else #f))
                                                           (cases expression operand
                                                             (const-exp (datum) (= datum 35))
                                                             (else #f))))
                                                    (else #f))
                                                  (pair? (cddr elements))
                                                  (cases expression (caddr elements)
                                                    (const-exp (datum) (= datum 36))
                                                    (else #f))
                                                  (null? (cdddr elements))))
                                           (else #f)))
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

  (test scan&parse:page-173
    (scan&parse "let index
                      = proc (n)
                         letrec inner (lst)
                          = if null?(lst)
                            then raise 99
                            else if zero?(-(car(lst),n))
                                 then 0
                                 else -((inner cdr(lst)), -1)
                         in proc (lst)
                             try (inner lst)
                              catch (x) -1
                 in ((index 5) list(2, 3))")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (let-exp (bound-var bound-value body)
              (and (eq? bound-var 'index)
                   (cases expression bound-value
                     (proc-exp (parameter body)
                       (and (eq? parameter 'n)
                            (cases expression body
                              (letrec-exp (procedure-name parameter procedure-body letrec-body)
                                (and (eq? procedure-name 'inner)
                                     (eq? parameter 'lst)
                                     (cases expression procedure-body
                                       (if-exp (condition consequent alternative)
                                         (and (cases expression condition
                                                (null?-exp (testee)
                                                  (cases expression testee
                                                    (var-exp (id) (eq? id 'lst))
                                                    (else #f)))
                                                (else #f))
                                              (cases expression consequent
                                                (raise-exp (body)
                                                  (cases expression body
                                                    (const-exp (datum) (= datum 99))
                                                    (else #f)))
                                                (else #f))
                                              (cases expression alternative
                                                (if-exp (condition consequent alternative)
                                                  (and (cases expression condition
                                                         (zero?-exp (testee)
                                                           (cases expression testee
                                                             (diff-exp (minuend subtrahend)
                                                               (and (cases expression minuend
                                                                      (car-exp (pair)
                                                                        (cases expression pair
                                                                          (var-exp (id) (eq? id 'lst))
                                                                          (else #f)))
                                                                      (else #f))
                                                                    (cases expression subtrahend
                                                                      (var-exp (id) (eq? id 'n))
                                                                      (else #F))))
                                                             (else #f)))
                                                         (else #f))
                                                       (cases expression consequent
                                                         (const-exp (datum) (zero? datum))
                                                         (else #f))
                                                       (cases expression alternative
                                                         (diff-exp (minuend subtrahend)
                                                           (and (cases expression minuend
                                                                  (call-exp (operator operand)
                                                                    (and (cases expression operator
                                                                           (var-exp (id) (eq? id 'inner))
                                                                           (else #f))
                                                                         (cases expression operand
                                                                           (cdr-exp (pair)
                                                                             (cases expression pair
                                                                               (var-exp (id) (eq? id 'lst))
                                                                               (else #f)))
                                                                           (else #f))))
                                                                  (else #f))
                                                                (cases expression subtrahend
                                                                  (const-exp (datum) (= datum -1))
                                                                  (else #f))))
                                                         (else #f))))
                                                (else #f))))
                                       (else #f))
                                     (cases expression letrec-body
                                       (proc-exp (parameter body)
                                         (and (eq? parameter 'lst)
                                              (cases expression body
                                                (try-exp (try-body parameter handle-body)
                                                  (and (cases expression try-body
                                                         (call-exp (operator operand)
                                                           (and (cases expression operator
                                                                  (var-exp (id) (eq? id 'inner))
                                                                  (else #f))
                                                                (cases expression operand
                                                                  (var-exp (id) (eq? id 'lst))
                                                                  (else #f))))
                                                         (else #f))
                                                       (eq? parameter 'x)
                                                       (cases expression handle-body
                                                         (const-exp (datum) (= datum -1))
                                                         (else #f))))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (call-exp (operator operand)
                       (and (cases expression operator
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) (eq? id 'index))
                                       (else #f))
                                     (cases expression operand
                                       (const-exp (datum) (= datum 5))
                                       (else #f))))
                              (else #f))
                            (cases expression operand
                              (list-exp (elements)
                                (and (pair? elements)
                                     (cases expression (car elements)
                                       (const-exp (datum) (= datum 2))
                                       (else #f))
                                     (pair? (cdr elements))
                                     (cases expression (cadr elements)
                                       (const-exp (datum) (= datum 3))
                                       (else #f))
                                     (null? (cddr elements))))
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

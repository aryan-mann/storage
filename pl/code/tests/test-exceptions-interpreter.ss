#!r7rs

;;; Tests for the (EXCEPTIONS interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created October 16, 2015
;;; last revised July 25, 2019

(import (scheme base)
        (scheme cxr)
        (utilities testing)
        (utilities eopl)
        (EXCEPTIONS expvals-and-environments)
        (EXCEPTIONS interpreter))

(suite run ()

  ;; Minimal expressions of different variants

  (test run:constant
    (run "0")
    1 (expval?)
    (lambda (result)
      (zero? (expval->num result))))

  (test run:variable
    (run "v")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 5)))

  (test run:diff-exp
    (run "-(i, 5)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -4)))

  (test run:zero?-exp-true
    (run "zero?(0)")
    1 (expval?)
    (lambda (result)
      (true? (expval->bool result))))

  (test run:zero?-exp-false
    (run "zero?(6)")
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result))))

  (test run:if-exp-selecting-consequent
    (run "if zero?(0) then 7 else 8")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 7)))

  (test run:if-exp-selecting-alternate
    (run "if zero?(9) then 10 else 11")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 11)))

  (test run:let-exp
    (run "let alpha = 12 in alpha")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 12)))

  (test run:proc-exp
    (run "proc (sigma) 15")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameter body env)
          (and (eq? parameter 'sigma)
               (cases expression body
                 (const-exp (datum) (= datum 15))
                 (else #f))
               (cases environment env
                 (extend-env (var val saved)
                   (and (eq? var 'i)
                        (= (expval->num val) 1)
                        (cases environment saved
                          (extend-env (var val saved)
                            (and (eq? var 'v)
                                 (= (expval->num val) 5)
                                 (cases environment saved
                                   (extend-env (var val saved)
                                     (and (eq? var 'x)
                                          (= (expval->num val) 10)
                                          (cases environment saved
                                            (empty-env () #t)
                                            (else #f))))
                                   (else #f))))
                          (else #f))))
                 (else #f)))))))

  (test run:call-exp
    (run "(proc (phi) -(0, phi) 18)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -18)))

  (test run:letrec-exp
    (run "letrec baz (quux) = 21 in x")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 10)))

  (test run:try-exp-no-exception
    (run "try 22 catch (irritant) 23")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 22)))

  (test run:try-exp-encountering-exception
    (run "try raise 24 catch (irritant) -(irritant,-1)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 25)))

  (test run:raise-exp
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object)
                            "In apply-handler: An exception was raised with no handler in place.\n"))
             'pass)
            (else 'fail))
      (run "raise 26"))
    1 (symbol?)
    (match eq? 'pass))

  (test run:cons-exp
    (run "cons(27,emptylist)")
    1 (expval?)
    (lambda (result)
      (let ((elements (expval->elements result)))
        (and (pair? elements)
             (= (expval->num (car elements)) 27)
             (null? (cdr elements))))))

  (test run:car-exp
    (run "car(list(28,29,30))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 28)))

  (test run:cdr-exp
    (run "cdr(list(31,32,33))")
    1 (expval?)
    (lambda (result)
      (let ((elements (expval->elements result)))
        (and (pair? elements)
             (= (expval->num (car elements)) 32)
             (pair? (cdr elements))
             (= (expval->num (cadr elements)) 33)
             (null? (cddr elements))))))

  (test run:null?-exp-true
    (run "null?(list())")
    1 (expval?)
    (lambda (result)
      (true? (expval->bool result))))

  (test run:null?-exp-false
    (run "null?(cons(34,emptylist))")
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result))))

  (test run:emptylist-exp
    (run "emptylist")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:list-exp-no-elements
    (run "list()")
    1 (expval?)
    (lambda (result)
      (null? (expval->elements result))))

  (test run:list-exp-single-element
    (run "list(34)")
    1 (expval?)
    (lambda (result)
      (let ((elements (expval->elements result)))
        (and (pair? elements)
             (= (expval->num (car elements)) 34)
             (null? (cdr elements))))))

  (test run:list-exp-many-elements
    (run "list(35,36,37,38,39)")
    1 (expval?)
    (lambda (result)
      (let ((elements (expval->elements result)))
        (and (pair? elements)
             (= (expval->num (car elements)) 35)
             (pair? (cdr elements))
             (= (expval->num (cadr elements)) 36)
             (pair? (cddr elements))
             (= (expval->num (caddr elements)) 37)
             (pair? (cdddr elements))
             (= (expval->num (cadddr elements)) 38)
             (let ((rest-of-elements (cddddr elements)))
               (and (pair? rest-of-elements)
                    (= (expval->num (car rest-of-elements)) 39)
                    (null? (cdr rest-of-elements))))))))

  (test run:side-effect
    (let ((out (open-output-string)))
      (parameterize ((current-output-port out))
        (let* ((main-result (run "22"))
               (side-effect (get-output-string out)))
          (close-output-port out)
          (values main-result side-effect))))
    2 (expval? string?)
    (lambda (main-result side-effect)
      (and (= (expval->num main-result) 22)
           (string=? side-effect "End of computation.\n"))))

  ;; Program from the textbook

  (test run:page-173
    (run "let index
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
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -1))))

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

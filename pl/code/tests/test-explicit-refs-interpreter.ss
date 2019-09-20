#!r7rs

;;; Tests for the (EXPLICIT-REFS interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2015
;;; last revised July 17, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (EXPLICIT-REFS stores)
        (EXPLICIT-REFS expvals-and-environments)
        (EXPLICIT-REFS interpreter))

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

  (test run:letrec-exp-minimal
    (run "letrec in x")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 10)))
                            
  (test run:letrec-exp-multiple-bindings
    (run "letrec baz (quux) = 21
                 garply (waldo) = 22
          in v")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 5)))

  (test run:newref-exp
    (run "newref(24)")
    1 (expval?)
    (lambda (result)
      (and (zero? (expval->ref result))
           (let ((sto (get-store)))
             (and (pair? sto)
                  (= (expval->num (car sto)) 24))))))

  (test run:deref-exp
    (run "deref(newref(25))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 25)))

  (test run:setref-exp
    (run "let temp = newref(26)
          in setref(temp,27)")
    1 (expval?)
    (lambda (result)
      (and (= (expval->num result) 23)
           (let ((sto (get-store)))
             (and (pair? sto)
                  (= (expval->num (car sto)) 27))))))
                            
  (test run:begin-exp-minimal
    (run "begin 35 end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 35)))

  (test run:begin-exp-larger
    (run "begin 36; 37; 38; 39 end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 39)))
                            
  ;; Programs from the textbook

  (test run:page-105-top
    (run "let x = newref(0)
          in letrec even(dummy)
                     = if zero?(deref(x))
                       then 1
                       else begin
                             setref(x, -(deref(x), 1));
                             (odd 888)
                            end
                    odd(dummy)
                     = if zero?(deref(x))
                       then 0
                       else begin
                             setref(x, -(deref(x), 1));
                             (even 888)
                            end
             in begin setref(x, 13); (odd 888) end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 1)))

  (test run:page-105-bottom
    (run "let g = let counter = newref(0)
                  in proc (dummy)
                      begin
                       setref(counter, -(deref(counter), -1));
                       deref(counter)
                      end
          in let a = (g 11)
             in let b = (g 11)
                in -(a, b)")
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

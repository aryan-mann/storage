#!r7rs

;;; Tests for the (CALL-BY-REFERENCE interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2015
;;; last revised July 31, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (CALL stores)
        (CALL-BY-REFERENCE expvals-and-environments)
        (CALL-BY-REFERENCE interpreter))

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
                 (extend-env (var loc saved)
                   (and (eq? var 'i)
                        (= (expval->num (list-ref (get-store) loc)) 1)
                        (cases environment saved
                          (extend-env (var loc saved)
                            (and (eq? var 'v)
                                 (= (expval->num (list-ref (get-store) loc)) 5)
                                 (cases environment saved
                                   (extend-env (var loc saved)
                                     (and (eq? var 'x)
                                          (= (expval->num (list-ref (get-store) loc)) 10)
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

  (test run:assign-exp
    (run "set x = 26")
    1 (expval?)
    (lambda (result)
      (and (= (expval->num result) 27)
           (= (expval->num (list-ref (get-store) 2)) 26))))

  (test run:begin-exp-minimal
    (run "begin 35 end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 35)))

  (test run:begin-exp-larger
    (run "begin set i = 36 ; set v = 37 ; set x = 38 ; -(i, -(v, x)) end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 37)))
                            
  ;; Programs from the textbook

  (test run:page-131-middle
    (run "let f = proc (x) set x = 44
          in let g = proc (y) (f y)
             in let z = 55
                in begin (g z); z end")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 44)))

  (test run:page-131-bottom
    (run "let swap = proc (x) proc (y)
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
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 11)))

  (test run:page-133
    (run "let b = 3
          in let p = proc(x) proc(y)
                      begin
                       set x = 4;
                       y
                      end
             in ((p b) b)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 4))))    


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

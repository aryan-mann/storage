#!r7rs

;;; Tests for the (CHECKED interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created November 13, 2015
;;; last revised July 26, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (LETREC expvals-and-environments)
        (CHECKED interpreter))

(suite run ()

  (test run:proc-exp
    (run "proc (sigma : (bool -> int)) 15")
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
    (run "(proc (phi : int) -(0, phi) 18)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -18)))

  (test run:letrec-exp
    (run "letrec int baz (quux : int) = 21 in x")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 10)))
                            
  ;; Programs from the textbook

  (test run:page-82
    (run "letrec int double (x: int)
                  = if zero?(x) then 0 else -((double -(x, 1)), -2)
          in (double 6)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 12)))

  (test run:page-241-top
    (run "proc (x : int) -(x,1)")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameter body saved-env)
          (and (eq? parameter 'x)
               (cases expression body
                 (diff-exp (minuend subtrahend)
                   (and (cases expression minuend
                          (var-exp (id) (eq? id 'x))
                          (else #f))
                        (cases expression subtrahend
                          (const-exp (datum) (= datum 1))
                          (else #f))))
                 (else #f))
               (cases environment saved-env
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

  (test run:page-241-middle
    (run "letrec
           int double (x : int) = if zero?(x)
                                  then 0
                                  else -((double -(x,1)), -2)
          in double")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameter body saved-env)
          (and (eq? parameter 'x)
               (cases expression body
                 (if-exp (condition consequent alternative)
                   (and (cases expression condition
                          (zero?-exp (testee)
                            (cases expression testee
                              (var-exp (id) (eq? id 'x))
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
                                            (var-exp (id) (eq? id 'double))
                                            (else #f))
                                          (cases expression operand
                                            (diff-exp (minuend subtrahend)
                                              (and (cases expression minuend
                                                     (var-exp (id) (eq? id 'x))
                                                     (else #f))
                                                   (cases expression subtrahend
                                                     (const-exp (datum) (= datum 1))
                                                     (else #f))))
                                            (else #f))))
                                   (else #f))
                                 (cases expression subtrahend
                                   (const-exp (datum) (= datum -2))
                                   (else #f))))
                          (else #f))))
                 (else #f))
               (cases environment saved-env
                 (extend-env-rec (p-name b-var body saved)
                   (and (eq? p-name 'double)
                        (eq? b-var 'x)
                        (cases expression body
                          (if-exp (condition consequent alternative)
                            (and (cases expression condition
                                   (zero?-exp (testee)
                                     (cases expression testee
                                       (var-exp (id) (eq? id 'x))
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
                                                     (var-exp (id) (eq? id 'double))
                                                     (else #f))
                                                   (cases expression operand
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (cases expression subtrahend
                                            (const-exp (datum) (= datum -2))
                                            (else #f))))
                                   (else #f))))
                          (else #f))
                        (cases environment saved
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
                          (else #f))))
                 (else #f)))))))

  (test run:page-241-bottom
    (run "proc (f : (bool -> int)) proc (n : int) (f zero?(n))")
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameter body saved-env)
          (and (eq? parameter 'f)
               (cases expression body
                 (proc-exp (parameter ty body)
                   (and (eq? parameter 'n)
                        (cases type ty
                          (int-type () #t)
                          (else #f))
                        (cases expression body
                          (call-exp (operator operand)
                            (and (cases expression operator
                                   (var-exp (id) (eq? id 'f))
                                   (else #f))
                                 (cases expression operand
                                   (zero?-exp (testee)
                                     (cases expression testee
                                       (var-exp (id) (eq? id 'n))
                                       (else #f)))
                                   (else #f))))
                          (else #f))))
                 (else #f))
               (cases environment saved-env
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
                 (else #f))))))))


;;; The type checker should detect type errors in these cases:

;;; (run "foo")
;;; (run "-(x,zero?(0))")
;;; (run "-(zero?(x),1)")
;;; (run "zero?(zero?(2))")
;;; (run "if zero?(x) then 3 else zero?(3)")
;;; (run "let a = zero?(4) in -(a,5)"
;;; (run "(6 7)")

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

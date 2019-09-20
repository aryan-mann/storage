#!r7rs

;;; Tests for the (PROC interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2015
;;; last revised August 5. 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (PROC expvals-and-environments)
        (PROC interpreter))

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
                            
  ;; Programs from the textbook

  (test run:page-60
    (run "-(55, -(x, 11))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 56)))

  (test run:page-64
    (run "-(-(x, 3), -(v, i))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 3)))

  (test run:page-66-top-embedded
    (run "let x = 33 in let y = 22
                        in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 18)))

  (test run:page-66-middle
    (run "let x = 5 in -(x, 3)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 2)))

  (test run:page-66-bottom
    (run "let z = 5
          in let x = 3
             in let y = -(x, 1)      % here x = 3
                in let x = 4
                   in -(z, -(x, y))  % here x = 4")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 3)))

  (test run:page-67
    (run "let x = 7
          in let y = 2
             in let y = let x = -(x,1)
                        in -(x,y)
                in -(-(x,8), y)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -5)))

  (test run:page-75-upper
    (run "let f = proc (x) -(x,11)
          in (f (f 77))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 55)))

  (test run:page-75-lower
    (run "(proc (f) (f (f 77))
           proc (x) -(x,11))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 55)))

  (test run:page-76
    (run "let x = 200
          in let f = proc (z) -(z, x)
             in let x = 100
                in let g = proc (z) -(z, x)
                   in -((f 1), (g 1))")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -100)))

  (test run:page-81
    (run "let makemult = proc (maker)
                           proc (x)
                             if zero?(x)
                             then 0
                             else -(((maker maker) -(x,1)), -4)
          in let times4 = proc (x)
                            ((makemult makemult) x)
             in (times4 3)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 12))))

;; The following expressions should signal errors:

;; (run "-(zero?(0), 5)")
;; (run "-(5, zero?(0))")
;; (run "zero?(zero?(0))")
;; (run "if 0 then 1 else 2")
;; (run "(35 42)")
;; (run "(zero?(0) 42)")
;; (run "-(proc (x) x), 12)")
;; (run "-(13, proc (x) x)")
;; (run "zero?(proc (x) x)")
;; (run "if proc (x) x then 3 else 4")
;; (run "(proc (x) -(x, 1) zero?(0))")

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

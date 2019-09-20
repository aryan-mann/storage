#!r7rs

;;; Tests for the (LET interpreter) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2015
;;; last revised July 10, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (LET expvals)
        (LET interpreter))

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

;;; Programs from the textbook

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
             in let y = let x = -(x, 1)
                        in -(x, y)
                in -(-(x, 8), y)")
    1 (expval?)
    (lambda (result)
      (= (expval->num result) -5))))

;; The following expressions should signal errors:

;; (run "-(zero?(0), 5)")
;; (run "-(5, zero?(0))")
;; (run "zero?(zero?(0))")
;; (run "if 0 then 1 else 2")

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

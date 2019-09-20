#!r7rs

;;; Tests for the (CHECKED syntax-trees) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 25, 2019
;;; last revised July 25, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (CHECKED syntax-trees))

(suite type? ()

  (test type?:int-type
    (type? (int-type))
    1 (true?))

  (test type?:bool-type
    (type? (bool-type))
    1 (true?))

  (test type?:proc-type
    (type? (proc-type (int-type) (bool-type)))
    1 (true?))

  (test type?:no
    (type? (const-exp -1))
    1 (false?)))


(suite expression? ()

  (test expression?:const-exp
    (expression? (const-exp 0))
    1 (true?))

  (test expression?:diff-exp
    (expression? (diff-exp (const-exp 1) (const-exp 2)))
    1 (true?))

  (test expression:zero?-exp
    (expression? (zero?-exp (const-exp 3)))
    1 (true?))

  (test expression?:if-exp
    (expression? (if-exp (zero?-exp (const-exp 4))
                         (const-exp 5)
                         (const-exp 6)))
    1 (true?))

  (test expression?:var-exp
    (expression? (var-exp 'a))
    1 (true?))

  (test expression?:let-exp
    (expression? (let-exp 'b (const-exp 7) (const-exp 8)))
    1 (true?))

  (test expression?:proc-exp
    (expression? (proc-exp 'c (int-type) (const-exp 9)))
    1 (true?))

  (test expression?:call-exp
    (expression? (call-exp (proc-exp 'd (bool-type) (const-exp 10))
                           (const-exp 11)))
    1 (true?))

  (test expression?:letrec-exp
    (expression? (letrec-exp (int-type) 'e 'f (bool-type)
                             (const-exp 12) (const-exp 13)))
    1 (true?))

  (test expression?:no
    (expression? 9)
    1 (false?)))


(suite program? ()

  (test program?:minimal
    (program? (a-program (const-exp 10)))
    1 (true?))

  (test program?:structured
    (program? (a-program
                (letrec-exp (int-type) 'g 'h (int-type)
                  (if-exp (zero?-exp (var-exp 'h))
                          (const-exp 23)
                          (call-exp (var-exp 'g)
                                    (diff-exp (var-exp 'h)
                                              (const-exp 1))))
                  (let-exp 'j
                    (proc-exp 'k (int-type)
                      (zero?-exp (diff-exp (var-exp 'k)
                                           (const-exp 1))))
                    (call-exp (var-exp 'j)
                              (call-exp (var-exp 'g)
                                        (const-exp 42)))))))
    1 (true?))

  (test program?:no
    (program? 14)
    1 (false?)))
                                  

;;; Copyright (C) 2019  John David Stone

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

#!r7rs

;;; Tests for the (EXPLICIT-REFS syntax-trees) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 15, 2019
;;; last revised July 15, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (EXPLICIT-REFS syntax-trees))

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
    (expression? (proc-exp 'c (const-exp 9)))
    1 (true?))

  (test expression?:call-exp
    (expression? (call-exp (proc-exp 'd (const-exp 10))
                           (const-exp 11)))
    1 (true?))

  (test expression?:letrec-exp
    (expression? (letrec-exp '(e) '(f) (list (const-exp 12)) (const-exp 13)))
    1 (true?))

  (test expression?:newref-exp
    (expression? (newref-exp (const-exp 20)))
    1 (true?))

  (test expression?:deref-exp
    (expression? (deref-exp (var-exp 'm)))
    1 (true?))

  (test expression?:setref-exp
    (expression? (setref-exp (var-exp 'n) (const-exp 21)))
    1 (true?))

  (test expression?:begin-exp
    (expression? (begin-exp (const-exp 22)
                            (list (const-exp 23) (const-exp 24))))
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
                (let-exp 'c
                         (proc-exp 'e (newref-exp (const-exp 14)))
                         (if-exp (zero?-exp (deref-exp (var-exp 'e)))
                                 (letrec-exp '(g h i)
                                             '(j k l)
                                             (list (const-exp 17)
                                                   (const-exp 18)
                                                   (const-exp 19))
                                             (diff-exp (const-exp 12)
                                                       (var-exp 'c)))
                                 (begin-exp
                                   (setref-exp (var-exp 'e)
                                               (const-exp 25))
                                   (list
                                     (setref-exp (var-exp 'h)
                                                 (const-exp 26))
                                     (call-exp (var-exp 'g) (const-exp 16))))))))
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

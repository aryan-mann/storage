#!r7rs

;;; Tests for the (CLASSES syntax-trees) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 29, 2019
;;; last revised July 29, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (CLASSES syntax-trees))

(suite expression? ()

  (test expression?:const-exp
    (expression? (const-exp 0))
    1 (true?))

  (test expression?:diff-exp
    (expression? (diff-exp (const-exp 1) (const-exp 2)))
    1 (true?))

  (test expression?:zero?-exp
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

  (test expression?:let-exp-no-bindings
    (expression? (let-exp (list) (list) (const-exp 7)))
    1 (true?))

  (test expression?:let-exp-single-binding
    (expression? (let-exp (list 'b) (list (const-exp 8)) (const-exp 9)))
    1 (true?))

  (test expression?:let-exp-multiple-bindings
    (expression? (let-exp (list 'c 'd 'e 'f)
                          (list (const-exp 10) (const-exp 11)
                                (const-exp 12) (const-exp 13))
                          (const-exp 14)))
    1 (true?))

  (test expression?:proc-exp-no-parameters
    (expression? (proc-exp (list) (const-exp 15)))
    1 (true?))

  (test expression?:proc-exp-single-parameter
    (expression? (proc-exp (list 'g) (const-exp 16)))
    1 (true?))

  (test expression?:proc-exp-multiple-parametern
    (expression? (proc-exp (list 'h 'i 'j) (const-exp 17)))
    1 (true?))

  (test expression?:call-exp-no-arguments
    (expression? (call-exp (proc-exp (list) (const-exp 18)) (list)))
    1 (true?))

  (test expression?:call-exp-single-argument
    (expression? (call-exp (proc-exp (list 'k) (const-exp 19))
                           (list (const-exp 20))))
    1 (true?))

  (test expression?:call-exp-multiple-arguments
    (expression? (call-exp (proc-exp (list 'l 'm 'n 'o) (const-exp 21))
                           (list (const-exp 22) (const-exp 23)
                                 (const-exp 24) (const-exp 25))))
    1 (true?))

  (test expression?:letrec-exp-no-bindings
    (expression? (letrec-exp (list) (list) (list) (const-exp 26)))
    1 (true?))

  (test expression?:letrec-exp-single-binding
    (expression? (letrec-exp (list 'p) (list (list 'q 'r 's))
                             (list (const-exp 27)) (const-exp 28)))
    1 (true?))

  (test expression?:letrec-exp-multiple-bindings
    (expression? (letrec-exp (list 't 'u 'v)
                             (list (list)
                                   (list 'w)
                                   (list 'x 'y 'z 'a))
                             (list (const-exp 29)
                                   (const-exp 30)
                                   (const-exp 31))
                             (const-exp 32)))
    1 (true?))

  (test expression?:assign-exp
    (expression? (assign-exp 'n (const-exp 33)))
    1 (true?))

  (test expression?:begin-exp
    (expression? (begin-exp (const-exp 22)
                            (list (const-exp 34) (const-exp 35))))
    1 (true?))                 

  (test expression?:sum-exp
    (expression? (sum-exp (const-exp 36) (const-exp 37)))
    1 (true?))

  (test expression?:product-exp
    (expression? (product-exp (const-exp 38) (const-exp 39)))
    1 (true?))

  (test expression?:cons-exp
    (expression? (cons-exp (const-exp 40) (var-exp 'b)))
    1 (true?))

  (test expression?:car-exp
    (expression? (car-exp (var-exp 'c)))
    1 (true?))

  (test expression?:cdr-exp
    (expression? (cdr-exp (var-exp 'd)))
    1 (true?))

  (test expression?:null?-exp
    (expression? (null?-exp (var-exp 'e)))
    1 (true?))

  (test expression?:emptylist-exp
    (expression? (emptylist-exp))
    1 (true?))

  (test expression?:list-exp
    (expression? (list-exp (list (const-exp 41) (const-exp 42) (emptylist-exp))))
    1 (true?))

  (test expression?:list-exp-no-elements
    (expression? (list-exp (list)))
    1 (true?))

  (test expression?:new-object-exp-no-operands
    (expression? (new-object-exp 'd (list)))
    1 (true?))

  (test expression?:new-object-exp-single-operand
    (expression? (new-object-exp 'e (list (const-exp 43))))
    1 (true?))

  (test expression?:new-object-exp-multiple-operands
    (expression? (new-object-exp 'f (list (const-exp 44) (const-exp 45)
                                          (const-exp 46) (const-exp 47))))
    1 (true?))

  (test expression?:method-call-exp-no-operands
    (expression? (method-call-exp (var-exp 'g) 'h (list)))
    1 (true?))
                                  
  (test expression?:method-call-exp-single-operand
    (expression? (method-call-exp (var-exp 'i) 'j (list (const-exp 48))))
    1 (true?))
                                  
  (test expression?:method-call-exp-multiple-operands
    (expression? (method-call-exp (var-exp 'k) 'l (list (const-exp 49)
                                                        (const-exp 50)
                                                        (const-exp 51))))
    1 (true?))

  (test expression?:super-call-exp-no-operands
    (expression? (super-call-exp 'm (list)))
    1 (true?))

  (test expression?:super-call-exp-single-operand
    (expression? (super-call-exp 'n (list (const-exp 52))))
    1 (true?))

  (test expression?:super-call-exp-multiple-operands
    (expression? (super-call-exp 'o (list (const-exp 53) (const-exp 54)
                                          (const-exp 55) (const-exp 56))))
    1 (true?))

  (test expression?:self-exp
    (expression? (self-exp))
    1 (true?))

  (test expression?:no
    (expression? 9)
    1 (false?)))


(suite program? ()

  (test program?:minimal
    (program? (a-program (list) (const-exp 10)))
    1 (true?))

  (test program?:page-327
    (program? (a-program
                (list (a-class-declaration 'c1 'object (list 'i 'j)
                        (list (a-method-declaration 'initialize (list 'x)
                                (begin-exp (assign-exp 'i (var-exp 'x))
                                           (list (assign-exp 'j (diff-exp (const-exp 0)
                                                                          (var-exp 'x))))))
                              (a-method-declaration 'countup (list 'd)
                                (begin-exp (assign-exp 'i (sum-exp (var-exp 'i)
                                                                   (var-exp 'd)))
                                           (list (assign-exp 'j (diff-exp (var-exp 'j)
                                                                          (var-exp 'd))))))
                              (a-method-declaration 'getstate (list)
                                (list-exp (list (var-exp 'i) (var-exp 'j)))))))
                (let-exp (list 't1 't2 'o1)
                         (list (const-exp 0)
                               (const-exp 0)
                               (new-object-exp 'c1 (list (const-exp 3))))
                         (begin-exp
                           (assign-exp 't1 (method-call-exp (var-exp 'o1) 'getstate (list)))
                           (list (method-call-exp (var-exp 'o1) 'countup (list (const-exp 2)))
                                 (assign-exp 't2
                                   (method-call-exp (var-exp 'o1) 'getstate (list)))
                                 (list-exp (list (var-exp 't1) (var-exp 't2))))))))
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

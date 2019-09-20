#!r7rs

;;; Tests for the (TYPED-OO syntax-trees) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created August 2, 2019
;;; last revised August 2, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (TYPED-OO syntax-trees))

(suite program? ()

  (test program?:minimal
    (program? (a-program (list) (const-exp 0)))
    1 (true?))

  (test program?:page-354
    (program? (a-program
                (list (an-interface-declaration 'tree
                        (list (an-abstract-method-declaration
                                (int-type) 'sum (list) (list))
                              (an-abstract-method-declaration
                                (bool-type) 'equal (list 't)
                                (list (class-type 'tree)))))
                      (a-class-declaration 'interior-node 'object (list 'tree)
                        (list (class-type 'tree)
                              (class-type 'tree))
                        (list 'left 'right)
                        (list (a-method-declaration (void-type) 'initialize
                                (list 'l 'r)
                                (list (class-type 'tree)
                                      (class-type 'tree))
                                (begin-exp (assign-exp 'left (var-exp 'l))
                                           (list (assign-exp 'right
                                                             (var-exp 'r)))))
                              (a-method-declaration (class-type 'tree) 'getleft
                                (list)
                                (list)
                                (var-exp 'left))
                              (a-method-declaration (class-type 'tree) 'getright
                                (list)
                                (list)
                                (var-exp 'right))
                              (a-method-declaration (int-type) 'sum
                                (list)
                                (list)
                                (sum-exp (method-call-exp (var-exp 'left) 'sum (list))
                                         (method-call-exp (var-exp 'right) 'sum (list))))
                              (a-method-declaration (bool-type) 'equal
                                (list 't)
                                (list (class-type 'tree))
                                (if-exp (instanceof-exp (var-exp 't) 'interior-node)
                                        (if-exp (method-call-exp
                                                  (var-exp 'left)
                                                  'equal
                                                  (list (method-call-exp
                                                          (cast-exp (var-exp 't)
                                                                    'interior-node)
                                                          'getleft
                                                         (list))))
                                                (method-call-exp
                                                  (var-exp 'right)
                                                  'equal
                                                  (list (method-call-exp
                                                          (cast-exp (var-exp 't)
                                                                    'interior-node)
                                                          'getright
                                                          (list))))
                                                (zero?-exp (const-exp 1)))
                                        (zero?-exp (const-exp 1))))))
                      (a-class-declaration 'leaf-node 'object (list 'tree)
                        (list (int-type))
                        (list 'value)
                        (list (a-method-declaration (void-type) 'initialize
                                (list 'v)
                                (list (int-type))
                                (assign-exp 'value (var-exp 'v)))
                              (a-method-declaration (int-type) 'sum
                                (list)
                                (list)
                                (var-exp 'value))
                              (a-method-declaration (int-type) 'getvalue
                                (list)
                                (list)
                                (var-exp 'value))
                              (a-method-declaration (bool-type) 'equal
                                (list 't)
                                (list (class-type 'tree))
                                (if-exp (instanceof-exp (var-exp 't) 'leaf-node)
                                        (zero?-exp
                                          (diff-exp (var-exp 'value)
                                                    (method-call-exp
                                                      (cast-exp (var-exp 't)
                                                                'leaf-node)
                                                      'getvalue
                                                      (list))))
                                        (zero?-exp (const-exp 1)))))))
                (let-exp (list 'o1)
                         (list (new-object-exp 'interior-node
                                 (list (new-object-exp 'interior-node
                                         (list (new-object-exp 'leaf-node
                                                 (list (const-exp 3)))
                                               (new-object-exp 'leaf-node
                                                 (list (const-exp 4)))))
                                       (new-object-exp 'leaf-node
                                         (list (const-exp 5))))))
                         (list-exp (method-call-exp (var-exp 'o1) 'sum (list))
                                   (list (if-exp (method-call-exp (var-exp 'o1)
                                                                  'equal
                                                                  (list (var-exp 'o1)))
                                                 (const-exp 100)
                                                 (const-exp 200)))))))
    1 (true?))

  (test program?:no
    (program? 1)
    1 (false?)))


(suite class-declaration? ()

  (test class-declaration?:minimal-class
    (class-declaration? (a-class-declaration 'a 'b (list) (list) (list) (list)))
    1 (true?))

  (test class-declaration?:typical-class
    (class-declaration? (a-class-declaration 'c 'd (list 'e 'f)
                          (list (int-type) (int-type) (int-type))
                          (list 'g 'h 'i)
                          (list (a-method-declaration (void-type) 'initialize
                                  (list 'j 'k)
                                  (list (int-type) (int-type))
                                  (begin-exp
                                    (assign-exp 'g (sum-exp (var-exp 'j) (var-exp 'k)))
                                    (list
                                      (assign-exp 'h (diff-exp (var-exp 'j)
                                                               (var-exp 'k)))
                                      (assign-exp 'i (product-exp (var-exp 'j)
                                                                  (var-exp 'k))))))
                                (a-method-declaration (list-type (int-type))
                                                      'all-fields
                                  (list)
                                  (list)
                                  (list-exp (var-exp 'g)
                                            (list (var-exp 'h) (var-exp 'i)))))))
    1 (true?))

  (test class-declaration?:minimal-interface
    (class-declaration? (an-interface-declaration 'l (list)))
    1 (true?))

  (test class-declaration?:typical-interface
    (class-declaration? (an-interface-declaration 'm
                          (list (an-abstract-method-declaration (int-type) 'n
                                  (list 'o 'p 'q)
                                  (list (int-type) (bool-type) (list-type (int-type))))
                                (an-abstract-method-declaration (int-type) 'r
                                  (list)
                                  (list)))))
    1 (true?))

  (test class-declaration?:no
    (class-declaration? (a-method-declaration (bool-type) 's
                          (list)
                          (list)
                          (null?-exp (emptylist-exp (int-type)))))
    1 (false?)))


(suite type? ()

  (test type?:int-type
    (type? (int-type))
    1 (true?))

  (test type?:bool-type
    (type? (bool-type))
    1 (true?))

  (test type?:proc-type-minimal
    (type? (proc-type (list) (void-type)))
    1 (true?))

  (test type?:proc-type-typical
    (type? (proc-type (list (proc-type (list (int-type)) (bool-type))
                            (list-type (int-type)))
                      (bool-type)))
    1 (true?))

  (test type?:void-type
    (type? (void-type))
    1 (true?))

  (test type?:class-type
    (type? (class-type 't))
    1 (true?))

  (test type?:list-type
    (type? (list-type (proc-type (list (int-type) (int-type)) (bool-type))))
    1 (true?))

  (test type?:no
    (type? (list))
    1 (false?)))


(suite method-declaration? ()

  (test method-declaration?:minimal
    (method-declaration?
      (a-method-declaration (int-type) 'u (list) (list) (const-exp 2)))
    1 (true?))

  (test method-declaration?:typical
    (method-declaration? (a-method-declaration (void-type) 'move
                           (list 'dx 'dy)
                           (list (int-type) (int-type))
                           (begin-exp
                             (assign-exp 'x (sum-exp (var-exp 'x)
                                                     (var-exp 'dx)))
                             (list (assign-exp 'y (sum-exp (var-exp 'y)
                                                           (var-exp 'dy)))))))
    1 (true?))

  (test method-declaration?:no
    (method-declaration? (an-abstract-method-declaration (void-type) 'move
                           (list 'dx 'dy)
                           (list (int-type) (int-type))))
    1 (false?)))


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
    (expression? (proc-exp (list) (list) (const-exp 15)))
    1 (true?))

  (test expression?:proc-exp-single-parameter
    (expression? (proc-exp (list 'g) (list (int-type)) (const-exp 16)))
    1 (true?))

  (test expression?:proc-exp-multiple-parametern
    (expression? (proc-exp (list 'h 'i 'j)
                           (list (int-type) (bool-type) (int-type))
                           (const-exp 17)))
    1 (true?))

  (test expression?:call-exp-no-arguments
    (expression? (call-exp (proc-exp (list) (list) (const-exp 18)) (list)))
    1 (true?))

  (test expression?:call-exp-single-argument
    (expression? (call-exp (proc-exp (list 'k) (list (int-type)) (const-exp 19))
                           (list (const-exp 20))))
    1 (true?))

  (test expression?:call-exp-multiple-arguments
    (expression? (call-exp (proc-exp (list 'l 'm 'n 'o)
                                     (list (int-type) (int-type)
                                           (int-type) (int-type))
                                     (const-exp 21))
                           (list (const-exp 22) (const-exp 23)
                                 (const-exp 24) (const-exp 25))))
    1 (true?))

  (test expression?:letrec-exp-no-bindings
    (expression? (letrec-exp (list) (list) (list) (list) (list) (const-exp 26)))
    1 (true?))

  (test expression?:letrec-exp-single-binding
    (expression? (letrec-exp (list (int-type))
                             (list 'p)
                             (list (list 'q 'r 's))
                             (list (list (int-type) (int-type) (int-type)))
                             (list (const-exp 27))
                             (const-exp 28)))
    1 (true?))

  (test expression?:letrec-exp-multiple-bindings
    (expression? (letrec-exp (list (int-type) (int-type) (int-type))
                             (list 't 'u 'v)
                             (list (list)
                                   (list 'w)
                                   (list 'x 'y 'z 'a))
                             (list (list)
                                   (list (int-type))
                                   (list (int-type) (int-type)
                                         (int-type) (int-type)))
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
    (expression? (emptylist-exp (proc-type (list (int-type)) (bool-type))))
    1 (true?))

  (test expression?:list-exp
    (expression? (list-exp (const-exp 41) (list (const-exp 42) (const-exp 43))))
    1 (true?))

  (test expression?:list-exp-single-element
    (expression? (list-exp (const-exp 44) (list)))
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

  (test expression?:cast-exp
    (expression? (cast-exp (var-exp 'p) 'q))
    1 (true?))

  (test expression?:instanceof-exp
    (expression? (instanceof-exp (var-exp 'r) 's))
    1 (true?))

  (test expression?:no
    (expression? 57)
    1 (false?)))


(suite abstract-method-declaration? ()

  (test abstract-method-declaration?:minimal
    (abstract-method-declaration?
      (an-abstract-method-declaration (void-type) 't (list) (list)))
    1 (true?))

  (test abstract-method-declaration?:typical
    (abstract-method-declaration?
      (an-abstract-method-declaration (int-type) 'u
        (list 'v)
        (list (class-type 'w))))
    1 (true?))

  (test abstract-method-declaration?:no
    (abstract-method-declaration?
      (a-method-declaration (int-type) 'x
        (list 'y)
        (list (class-type 'z))
        (let-exp (list 'a)
                 (list (method-call-exp (var-exp 'y) 'b (list)))
                 (product-exp (var-exp 'a) (var-exp 'a)))))
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

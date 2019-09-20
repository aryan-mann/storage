#!r7rs

;;; Syntax trees for CLASSES

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 23, 2009
;;; last revised July 29, 2019

;;; This package defines a data type
;;; for abstract syntax trees of the CLASSES programming language,
;;; as described in section 9.3 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (CLASSES syntax-trees)
  (export program? a-program
          class-declaration? a-class-declaration
          method-declaration? a-method-declaration
          expression? const-exp diff-exp zero?-exp if-exp var-exp let-exp
          proc-exp call-exp letrec-exp assign-exp begin-exp sum-exp product-exp
          cons-exp car-exp cdr-exp null?-exp emptylist-exp list-exp
          new-object-exp method-call-exp super-call-exp self-exp)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; Like LETREC, CLASSES allows multiple bindings to be introduced
    ;; in a single letrec-expression.
    ;; Method calls can likewise take multiple parameters
    ;; and so create multiple bindings.
    ;; I have also extended let-expressions, proc-expressions, and procedure calls
    ;; to allow for the possibility of multiple bindings,
    ;; multiple parameters, and multiple arguments respectively.
    ;; Procedures introduced in letrec-expressions
    ;; can also have multiple parameters.

    ;; The grammar for CLASSES is as follows:
    ;;
    ;;            <program> ::= {<class-declaration>}* <expression>
    ;;  <class-declaration> ::= class <identifier extends <identifier>
    ;;                            {field <identifier>}* {<method-declaration>}*
    ;; <method-declaration> ::= method <identifier> ( {<identifier>}*{,} ) <expression>
    ;;         <expression> ::= <numeral>
    ;;                        | - ( <expression> , <expression> )
    ;;                        | zero? ( <expression> )
    ;;                        | if <expression> then <expression> else <expression>
    ;;                        | <identifier>
    ;;                        | let {<identifier> = <expression>}* in <expression>
    ;;                        | proc ( {<identifier>}*{,} ) <expression>
    ;;                        | ( <expression> {<expression>}* )
    ;;                        | letrec {<identifier> ( {<identifier>}*{,} )
    ;;                            = <expression>}*
    ;;                            in <expression>
    ;;                        | set <identifier> = <expression>
    ;;                        | begin <expression> {; <expression>}* end
    ;;                        | + ( <expression> , <expression> )
    ;;                        | * ( <expression> , <expression> )
    ;;                        | cons ( <expression> , <expression> )
    ;;                        | car ( <expression> )
    ;;                        | cdr ( <expression> )
    ;;                        | null? ( <expression> )
    ;;                        | emptylist
    ;;                        | list ( {<expression>}*{,} )
    ;;                        | new <identifier> ( {<expression>}*{,} )
    ;;                        | send <expression> <identifier> ( {<expression>}*{,} )
    ;;                        | super <identifier ( {<expression>}*{,} )
    ;;                        | self
    ;;
    ;; The data type definitions reflect this grammar.

    (define-datatype program program?
      (a-program (class-decls (list-of class-declaration?))
                 (body expression?)))

    (define-datatype class-declaration class-declaration?
      (a-class-declaration (class-name identifier?)
                           (super-name identifier?)
                           (field-names (list-of identifier?))
                           (method-decls (list-of method-declaration?))))

    (define-datatype method-declaration method-declaration?
      (a-method-declaration (method-name identifier?)
                            (parameters (list-of identifier?))
                            (body expression?)))

    (define-datatype expression expression?
      (const-exp (datum exact-integer?))
      (diff-exp (minuend expression?)
                (subtrahend expression?))
      (zero?-exp (testee expression?))
      (if-exp (condition expression?)
              (consequent expression?)
              (alternative expression?))
      (var-exp (id identifier?))
      (let-exp (bound-vars (list-of identifier?))
               (bound-values (list-of expression?))
               (body expression?))
      (proc-exp (parameters (list-of identifier?))
                (body expression?))
      (call-exp (operator expression?)
                (operands (list-of expression?)))
      (letrec-exp (procedure-names (list-of identifier?))
                  (parameter-lists (list-of (list-of identifier?)))
                  (procedure-bodies (list-of expression?))
                  (letrec-body expression?))
      (assign-exp (target identifier?)
                  (source expression?))
      (begin-exp (starter expression?)
                 (sequence (list-of expression?)))
      (sum-exp (augend expression?)
               (addend expression?))
      (product-exp (multiplicand expression?)
                   (multiplier expression?))
      (cons-exp (fore expression?)
                (aft expression?))
      (car-exp (pair expression?))
      (cdr-exp (pair expression?))
      (null?-exp (testee expression?))
      (emptylist-exp)
      (list-exp (elements (list-of expression?)))
      (new-object-exp (class-name identifier?)
                      (operands (list-of expression?)))
      (method-call-exp (recipient expression?)
                       (method-name identifier?)
                       (operands (list-of expression?)))
      (super-call-exp (method-name identifier?)
                      (operands (list-of expression?)))
      (self-exp))))


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

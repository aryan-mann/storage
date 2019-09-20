#!r7rs

;;; Syntax trees for IMPLICIT-REFS

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 18, 2009
;;; last revised July 17, 2019

;;; This package defines a data type
;;; for abstract syntax trees of the IMPLICIT-REFS programming language,
;;; as described in section 4.3 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (IMPLICIT-REFS syntax-trees)
  (export program? a-program
          expression? const-exp diff-exp zero?-exp if-exp var-exp let-exp
          proc-exp call-exp letrec-exp assign-exp begin-exp)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; A sample program at the top of page 117
    ;; of _Essentials of Programming Languages_
    ;; shows that IMPLICIT-REFS allows multiple bindings to be introduced
    ;; in a single letrec-expression.
    ;; I have implemented this feature here.
    ;; However, let-expressions and procedure calls
    ;; still create only single bindings,
    ;; as in PROC and LETREC.

    ;; The same example shows that IMPLICIT-REFS supports begin-expressions,
    ;; which I have implemented here as well.

    ;; The grammar for IMPLICIT-REFS is as follows:
    ;;
    ;;       <program> ::= <expression>
    ;;    <expression> ::= <numeral>
    ;;                   | - ( <expression> , <expression> )
    ;;                   | zero? ( <expression> )
    ;;                   | if <expression> then <expression> else <expression>
    ;;                   | <identifier>
    ;;                   | let <identifier> = <expression> in <expression>
    ;;                   | proc ( <identifier> ) <expresssion>
    ;;                   | ( <expression> <expression> )
    ;;                   | letrec {<identifier> ( <identifier> ) = <expression>}*
    ;;                       in <expression>
    ;;                   | set <identifier> = <expression>
    ;;                   | begin <expression> {; <expression>}* end
    ;;
    ;; The data type definitions reflect this grammar.

    (define-datatype program program?
      (a-program (exp expression?)))

    (define-datatype expression expression?
      (const-exp (datum exact-integer?))
      (diff-exp (minuend expression?)
                (subtrahend expression?))
      (zero?-exp (testee expression?))
      (if-exp (condition expression?)
              (consequent expression?)
              (alternative expression?))
      (var-exp (id identifier?))
      (let-exp (bound-var identifier?)
               (bound-value expression?)
               (body expression?))
      (proc-exp (parameter identifier?)
                (body expression?))
      (call-exp (operator expression?)
                (operand expression?))
      (letrec-exp (procedure-names (list-of identifier?))
                  (parameters (list-of identifier?))
                  (procedure-bodies (list-of expression?))
                  (letrec-body expression?))
      (assign-exp (target identifier?)
                  (source expression?))
      (begin-exp (starter expression?)
                 (sequence (list-of expression?))))))                  

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

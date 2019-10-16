#!r7rs

;;; Syntax trees for LETREC-LCPI

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 3, 2009
;;; last revised July 14, 2019

;;; This package defines a data type
;;; for abstract syntax trees of the LETREC-LCPI programming language,
;;; as described in section 3.4 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (LETREC-LCPI syntax-trees)
  (export program? a-program
          expression? const-exp diff-exp zero?-exp if-exp var-exp let-exp
          proc-exp call-exp
          ;; [Aryan]
          cons-exp null?-exp car-exp cdr-exp empty-list-exp
          ;; [Aryan]
          letrec-exp)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; The grammar for LETREC-LCPI is as follows:
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
    ;;                   | letrec <identifier> ( <identifier> ) = <expression>
    ;;                       in <expression>
    ;;
    ;; The data type definitions exactly reflect this grammar.

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
      ;; [Aryan]

      (cons-exp (thing expression?)
                (other-thing expression?))
      (null?-exp (to-test expression?))
      (car-exp (lst expression?))
      (cdr-exp (lst expression?))
      (empty-list-exp)
      
      ;; [Aryan]
      (letrec-exp (procedure-name identifier?)
                  (parameter identifier?)
                  (procedure-body expression?)
                  (letrec-body expression?))
      )))

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
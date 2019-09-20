#!r7rs

;;; Syntax trees for CHECKED

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 9, 2009
;;; last revised July 25, 2019

;;; This package defines a data type
;;; for abstract syntax trees of the CHECKED programming language,
;;; as described in section 7.3 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (CHECKED syntax-trees)
  (export program? a-program
          expression? const-exp diff-exp zero?-exp if-exp var-exp let-exp
          proc-exp call-exp letrec-exp
          type? int-type bool-type proc-type)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; The grammar for CHECKED is as follows:
    ;;
    ;;       <program> ::= <expression>
    ;;    <expression> ::= <numeral>
    ;;                   | - ( <expression> , <expression> )
    ;;                   | zero? ( <expression> )
    ;;                   | if <expression> then <expression> else <expression>
    ;;                   | <identifier>
    ;;                   | let <identifier> = <expression> in <expression>
    ;;                   | proc ( <identifier> : <type> ) <expresssion>
    ;;                   | ( <expression> <expression> )
    ;;                   | letrec <type> <identifier> ( <identifier> : <type> )
    ;;                       = <expression> in <expression>
    ;;          <type> ::= int
    ;;                   | bool
    ;;                   | ( <type> -> type )
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
                (ty type?)
                (body expression?))
      (call-exp (operator expression?)
                (operand expression?))
      (letrec-exp (return-type type?)
                  (procedure-name identifier?)
                  (parameter identifier?)
                  (parameter-type type?)
                  (procedure-body expression?)
                  (letrec-body expression?)))

    (define-datatype type type?
      (int-type)
      (bool-type)
      (proc-type (arg-type type?)
                 (result-type type?)))))

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

#!r7rs

;;; Syntax trees for MINUS

;;; John David Stone and Emi Bergman-Corbet
;;; Department of Computer Science
;;; Grinnell College

;;; created January 31, 2009
;;; last revised September 13, 2019

;;; This package defines a data type
;;; for abstract syntax trees of the MINUS programming language.

(define-library (MINUS syntax-trees)
  (export program? a-program
          expression? const-exp diff-exp zero?-exp if-exp var-exp let-exp
          minus-exp)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; The grammar for MINUS is as follows:
    ;;
    ;;       <program> ::= <expression>
    ;;    <expression> ::= <numeral>
    ;;                   | - ( <expression> , <expression> )
    ;;                   | zero? ( <expression> )
    ;;                   | if <expression> then <expression> else <expression>
    ;;                   | <identifier>
    ;;                   | let <identifier> = <expression> in <expression>
    ;;                   | minus ( <expression> )
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
      (minus-exp (negand expression?)))))

;;; Copyright (C) 2009, 2015, 2019  John David Stone and Emi Bergman-Corbet

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

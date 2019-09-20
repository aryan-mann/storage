#!r7rs

;;; Tokens in CHECKED

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 9, 2009
;;; last revised July 25, 2019

;;; This library defines a data type
;;; for lexical tokens of the CHECKED programming language,
;;; as described in section 7.3 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

;;; CHECKED has tokens of nineteen kinds:
;;; "numbers" (actually, numerals),
;;; identifiers, minus signs, open- and close-parentheses,
;;; commas, equals signs, colons, arrows
;;; and the keywords zero?, if, then, else, let, in, proc, letrec, int, and bool.

(define-library (CHECKED tokens)
  (export token? numeral-token minus-sign open-parenthesis comma
          close-parenthesis zero?-token if-token then-token else-token
          identifier-token let-token equals-sign in-token proc-token letrec-token
          colon arrow int-token bool-token)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; A single define-datatype declaration
    ;; accommodates all of these tokens as variants.

    (define-datatype token token?
      (numeral-token (value exact-integer?))
      (minus-sign)
      (open-parenthesis)
      (comma)
      (close-parenthesis)
      (zero?-token)
      (if-token)
      (then-token)
      (else-token)
      (identifier-token (id identifier?))
      (let-token)
      (equals-sign)
      (in-token)
      (proc-token)
      (letrec-token)
      (colon)
      (arrow)
      (int-token)
      (bool-token))))

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

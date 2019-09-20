#!r7rs

;;; Tokens in MINUS

;;; John David Stone and Albert Ford
;;; Department of Computer Science
;;; Grinnell College

;;; created January 31, 2009
;;; last revised July 14, 2019

;;; This library defines a data type
;;; for lexical tokens of the MINUS programming language.

;;; MINUS has tokens of fourteenth kinds:
;;; "numbers" (actually, numerals),
;;; identifiers, minus signs, open- and close-parentheses,
;;; commas, equals signs,
;;; and the keywords zero?, if, then, else, let, in, and minus.

(define-library (MINUS tokens)
  (export token? numeral-token minus-sign open-parenthesis comma
          close-parenthesis zero?-token if-token then-token else-token
          identifier-token let-token equals-sign in-token minus-token)
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
      (minus-token))))

;;; Copyright (C) 2009, 2015, 2019  John David Stone and Albert Ford

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

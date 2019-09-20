#!r7rs

;;; Tests for the (CALL tokens) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 18, 2019
;;; last revised July 31, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (CALL tokens))

(suite token? ()

  (test token?:numeral-token
    (token? (numeral-token 42))
    1 (true?))

  (test token?:minus-sign
    (token? (minus-sign))
    1 (true?))            

  (test token?:open-parenthesis
    (token? (open-parenthesis))
    1 (true?))

  (test token?:comma
    (token? (comma))
    1 (true?))

  (test token?:close-parenthesis
    (token? (close-parenthesis))
    1 (true?))

  (test token?:zero?-token
    (token? (zero?-token))
    1 (true?))

  (test token?:if-token
    (token? (if-token))
    1 (true?))

  (test token?:then-token
    (token? (then-token))
    1 (true?))

  (test token?:else-token
    (token? (else-token))
    1 (true?))

  (test token?:identifier-token
    (token? (identifier-token 'foo))
    1 (true?))

  (test token?:let-token
    (token? (let-token))
    1 (true?))

  (test token?:equals-sign
    (token? (equals-sign))
    1 (true?))

  (test token?:in-token
    (token? (in-token))
    1 (true?))

  (test token?:proc-token
    (token? (proc-token))
    1 (true?))

  (test token?:letrec-token
    (token? (letrec-token))
    1 (true?))

  (test token?:set-token
    (token? (set-token))
    1 (true?))

  (test token?:begin-token
    (token? (begin-token))
    1 (true?))

  (test token?:semicolon
    (token? (semicolon))
    1 (true?))

  (test token?:end-token
    (token? (end-token))
    1 (true?))

  (test token?:non-token
    (token? 'bar)
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
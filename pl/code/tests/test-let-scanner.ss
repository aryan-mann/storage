#!r7rs

;;; Tests for the (LET scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 31, 2009
;;; last revised July 18, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (LET tokens)
        (LET scanner))

(suite at-end? ()

  (test at-end?:yes
    ((scanner (make-character-source "")) 'at-end?)
    1 (true?))

  (test at-end?:white-space-only
    ((scanner (make-character-source "     \n\t\n \t ")) 'at-end?)
    1 (true?))

  (test at-end?:no
    ((scanner (make-character-source "foo")) 'at-end?)
    1 (false?))

  (test at-end?:white-space-then-identifier
    ((scanner (make-character-source "    foo  ")) 'at-end?)
    1 (false?)))


(suite peek ()

  (test peek:numeral
    ((scanner (make-character-source "362")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (numeral-token (value) (= value 362))
        (else #f))))

  (test peek:minus-sign
    ((scanner (make-character-source "- 189")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (minus-sign () #t)
        (else #f))))

  (test peek:identifier
    ((scanner (make-character-source "   IF   ")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (identifier-token (id) (eq? id 'IF))
        (else #f)))))


(suite get ()

  (test get:open-parenthesis
    ((scanner (make-character-source "(foo bar))")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (open-parenthesis () #t)
        (else #f))))

  (test get:identifier-with-hyphen-and-question-mark
    ((scanner (make-character-source "infinite-loop?")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (identifier-token (id) (eq? id 'infinite-loop?))
        (else #f))))

  (test get:negative-numeral
    ((scanner (make-character-source "  -3?")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (numeral-token (value) (= value -3))
        (else #f)))))


;;; The following expressions should produce errors.

;;; ((scanner (make-character-source "")) 'get)
;;; ((scanner (make-character-source "")) 'peek)
;;; ((scanner (make-character-source "#")) 'get)
;;; ((scanner (make-character-source "% say no more\n")) 'get)
;;; ((scanner (make-character-source "%%%")) 'get)
;;; ((scanner (make-character-source "?")) 'get)
;;; ((scanner (make-character-source "+")) 'get)

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

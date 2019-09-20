#!r7rs

;;; Tests for the (CHECKED scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 9, 2009
;;; last revised July 26, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CHECKED tokens)
        (CHECKED scanner))

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
        (else #f))))

  (test peek:space-in-letrec
    ((scanner (make-character-source "let rec")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (let-token () #t)
        (else #f))))

  (test peek:different-space-in-letrec
    ((scanner (make-character-source "letre c")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (identifier-token (id) (eq? id 'letre))
        (else #f))))

  (test peek:colon
    ((scanner (make-character-source "  ::  ")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (colon () #t)
        (else #f))))

  (test peek:int-token
    ((scanner (make-character-source "int")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (int-token () #t)
        (else #f))))

  (test peek:not-an-int-token
    ((scanner (make-character-source "in t")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (in-token () #t)
        (else #f))))

  (test peek:bool-token
    ((scanner (make-character-source "bool")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (bool-token () #t)
        (else #f)))))


(suite get ()

  (test get:open-parenthesis
    ((scanner (make-character-source "(foo bar))")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (open-parenthesis () #t)
        (else #f))))

  (test get:letrec-token
    ((scanner (make-character-source "letrec")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (letrec-token () #t)
        (else #f))))

  (test get:arrow
    ((scanner (make-character-source " -> ")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (arrow () #t)
        (else #f))))

  (test get:not-an-arrow
    ((scanner (make-character-source "- >")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (minus-sign () #t)
        (else #f))))

  (test get:double-shaft-not-an-arrow
    ((scanner (make-character-source " => ")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (equals-sign () #t)
        (else #f))))

  (test get:hyphen-ending-identifier
    ((scanner (make-character-source "bool->int")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (identifier-token (id) (eq? id 'bool-))
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
;;; ((scanner (make-character-source "<-")) 'get)

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

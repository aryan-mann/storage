#!r7rs

;;; Tests for the (EXPLICIT-REFS scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 31, 2009
;;; last revised July 12, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (EXPLICIT-REFS tokens)
        (EXPLICIT-REFS scanner))

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

  (test peek:newref-token
    ((scanner (make-character-source "  newref  --")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (newref-token () #t)
        (else #f))))

  (test peek:deref-token
    ((scanner (make-character-source "deref%comment")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (deref-token () #t)
        (else #f))))

  (test peek:setref-token
    ((scanner (make-character-source "setref")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (setref-token () #t)
        (else #f))))

  (test peek:begin-token
    ((scanner (make-character-source "begin   ")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (begin-token () #t)
        (else #f))))

  (test peek:semicolon
    ((scanner (make-character-source "; % semicolon")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (semicolon () #t)
        (else #f))))

  (test peek:end-token
    ((scanner (make-character-source "      end")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (end-token () #t)
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

  (test get:newref-token
    ((scanner (make-character-source "newref")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (newref-token () #t)
        (else #f))))

  (test get:deref-token
    ((scanner (make-character-source "deref")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (deref-token () #t)
        (else #f))))

  (test get:setref-token
    ((scanner (make-character-source "setref")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (setref-token () #t)
        (else #f))))

  (test get:begin-token
    ((scanner (make-character-source "begin")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (begin-token () #t)
        (else #f))))

  (test get:semicolon
    ((scanner (make-character-source ";")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (semicolon () #t)
        (else #f))))

  (test get:end-token
    ((scanner (make-character-source "end")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (end-token () #t)
        (else #f))))

  (test get:negative-numeral
    ((scanner (make-character-source "  -3?")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (numeral-token (value) (= value -3))
        (else #f)))))


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

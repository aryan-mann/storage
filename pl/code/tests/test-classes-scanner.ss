#!r7rs

;;; Tests for the (CLASSES scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 23, 2009
;;; last revised July 26, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CLASSES tokens)
        (CLASSES scanner))

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

  (test peek:set-token
    ((scanner (make-character-source "set ref")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (set-token () #t)
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

  (test peek:plus-sign
    ((scanner (make-character-source "+23")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (plus-sign () #t)
        (else #f))))

  (test peek:asterisk
    ((scanner (make-character-source "***")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (asterisk () #t)
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
        (else #f))))

  (test peek:cons-token
    ((scanner (make-character-source "cons")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (cons-token () #t)
        (else #f))))

  (test peek:car-token
    ((scanner (make-character-source "car")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (car-token () #t)
        (else #f))))

  (test peek:cdr-token
    ((scanner (make-character-source "cdr")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (cdr-token () #t)
        (else #f))))

  (test peek:class-token
    ((scanner (make-character-source "class")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (class-token () #t)
        (else #f))))

  (test peek:field-token
    ((scanner (make-character-source "field")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (field-token () #t)
        (else #f))))

  (test peek:new-token
    ((scanner (make-character-source "new!")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (new-token () #t)
        (else #f))))

  (test peek:super-token
    ((scanner (make-character-source "super")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (super-token () #t)
        (else #f)))))


(suite get ()

  (test get:open-parenthesis
    ((scanner (make-character-source "(foo bar))")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (open-parenthesis () #t)
        (else #f))))

  (test get:set-token
    ((scanner (make-character-source "set")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (set-token () #t)
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
        (else #f))))

  (test get:null?-token
    ((scanner (make-character-source "null?")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (null?-token () #t)
        (else #f))))

  (test get:null-without-ques
    ((scanner (make-character-source "null(foo)")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (identifier-token (id) (eq? id 'null))
        (else #f))))

  (test get:emptylist-token
    ((scanner (make-character-source "emptylist")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (emptylist-token () #t)
        (else #f))))

  (test get:list-token
    ((scanner (make-character-source "list")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (list-token () #t)
        (else #f))))

  (test get:extends-token
    ((scanner (make-character-source "extends")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (extends-token () #t)
        (else #f))))

  (test get:method-token
    ((scanner (make-character-source "method")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (method-token () #t)
        (else #f))))

  (test get:send-token
    ((scanner (make-character-source "send")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (send-token () #t)
        (else #f))))

  (test get:self-token
    ((scanner (make-character-source "self")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (self-token () #t)
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

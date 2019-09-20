#!r7rs

;;; Tests for the (MUTABLE-PAIRS scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 28, 2009
;;; last revised July 17, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (MUTABLE-PAIRS tokens)
        (MUTABLE-PAIRS scanner))

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

  (test peek:pair
    ((scanner (make-character-source "  pair(")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (pair-token () #t)
        (else #f))))

  (test peek:left
    ((scanner (make-character-source "left")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (left-token () #t)
        (else #f))))

  (test peek:right
    ((scanner (make-character-source "\tright")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (right-token () #t)
        (else #f))))

  (test peek:setleft
    ((scanner (make-character-source "setleft")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (setleft-token () #t)
        (else #f))))

  (test peek:tricky-set-left
    ((scanner (make-character-source "set left")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (set-token () #t)
        (else #f))))

  (test peek:setright
    ((scanner (make-character-source "setright")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (setright-token () #t)
        (else #f)))))


(suite get ()

  (test get:close-parenthesis
    ((scanner (make-character-source ")")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (close-parenthesis () #t)
        (else #f))))

  (test get:pair
    ((scanner (make-character-source "pair")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (pair-token () #t)
        (else #f))))

  (test get:left
    ((scanner (make-character-source "left")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (left-token () #t)
        (else #f))))

  (test get:right
    ((scanner (make-character-source "right")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (right-token () #t)
        (else #f))))

  (test get:setleft
    ((scanner (make-character-source "setleft")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (setleft-token () #t)
        (else #f))))

  (test get:setright
    ((scanner (make-character-source "setright")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (setright-token () #t)
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

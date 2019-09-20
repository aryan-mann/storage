#!r7rs

;;; Tests for the (TYPED-OO scanner) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 28, 2009
;;; last revised August 5, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (TYPED-OO tokens)
        (TYPED-OO scanner))

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

  (test peek:int-token
    ((scanner (make-character-source "int")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (int-token () #t)
        (else #f))))

  (test peek:colon
    ((scanner (make-character-source ":::")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (colon () #t)
        (else #f))))

  (test peek:implements-token
    ((scanner (make-character-source "implements")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (implements-token () #t)
        (else #f))))

  (test peek:cast-token
    ((scanner (make-character-source "cast")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (cast-token () #t)
        (else #f))))

  (test peek:void-token
    ((scanner (make-character-source "void")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (void-token () #t)
        (else #f))))

  (test peek:underscore
    ((scanner (make-character-source "_int")) 'peek)
    1 (token?)
    (lambda (result)
      (cases token result
        (underscore () #t)
        (else #f)))))


(suite get ()

  (test get:bool-token
    ((scanner (make-character-source "bool")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (bool-token () #t)
        (else #f))))

  (test get:arrow
    ((scanner (make-character-source "->")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (arrow () #t)
        (else #f))))

  (test get:interface-token
    ((scanner (make-character-source "interface")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (interface-token () #t)
        (else #f))))

  (test get:instanceof-token
    ((scanner (make-character-source "instanceof")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (instanceof-token () #t)
        (else #f))))

  (test get:listof-token
    ((scanner (make-character-source "listof")) 'get)
    1 (token?)
    (lambda (result)
      (cases token result
        (listof-token () #t)
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

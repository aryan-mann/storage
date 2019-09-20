#!r7rs

;;; Tests for the (CHECKED type-checker) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 26, 2019
;;; last revised July 26, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CHECKED syntax-trees)
        (CHECKED parser)
        (CHECKED type-checker))

(suite type-of-program ()

  (test type-of-program:cons
    (type-of-program (scan&parse "0"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:var
    (type-of-program (scan&parse "i"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:diff
    (type-of-program (scan&parse "-(2,v)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:zero?
    (type-of-program (scan&parse "zero?(3)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:if
    (type-of-program (scan&parse "if zero?(4) then 5 else 6"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:let
    (type-of-program (scan&parse "let a = zero?(7) in a"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:proc
    (type-of-program (scan&parse "proc (c: bool) if c then 8 else 9"))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-type result-type)
          (and (cases type arg-type
                 (bool-type () #t)
                 (else #f))
               (cases type result-type
                 (int-type () #t)
                 (else #f))))
        (else #f))))

  (test type-of-program:call
    (type-of-program (scan&parse "(proc (e: int) 10 11)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:letrec
    (type-of-program (scan&parse "letrec int f (g: int)
                                   = if zero?(g) then 12 else (f -(g, 1))
                                  in (f 13)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:page-241-top
    (type-of-program (scan&parse "proc (x : int) -(x,1)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-type result-type)
          (and (cases type arg-type
                 (int-type () #t)
                 (else #f))
               (cases type result-type
                 (int-type () #t)
                 (else #f))))
        (else #f))))

  (test type-of-program:page-241-middle
    (type-of-program
      (scan&parse "letrec
                    int double (x : int) = if zero?(x)
                                           then 0
                                           else -((double -(x,1)), -2)
                   in double"))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-type result-type)
          (and (cases type arg-type
                 (int-type () #t)
                 (else #f))
               (cases type result-type
                 (int-type () #t)
                 (else #f))))
        (else #f))))

  (test type-of-program:page-241-bottom
    (type-of-program
      (scan&parse "proc (f : (bool -> int)) proc (n : int) (f zero?(n))"))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-type result-type)
          (and (cases type arg-type
                 (proc-type (arg-type result-type)
                   (and (cases type arg-type
                          (bool-type () #t)
                          (else #f))
                        (cases type result-type
                          (int-type () #t)
                          (else #f))))
                 (else #f))
               (cases type result-type
                 (proc-type (arg-type result-type)
                   (and (cases type arg-type
                          (int-type () #t)
                          (else #f))
                        (cases type result-type
                          (int-type () #t)
                          (else #f))))
                 (else #f))))
        (else #f)))))


(suite type-to-external-form ()

  (test type-to-external-form:int
    (type-to-external-form (int-type))
    1 (symbol?)
    (match eq? 'int))

  (test type-to-external-form:bool
    (type-to-external-form (bool-type))
    1 (symbol?)
    (match eq? 'bool))

  (test type-to-external-form:pro
    (type-to-external-form (proc-type (proc-type (int-type) (bool-type))
                                      (int-type)))
    1 (list?)
    (match equal? '((int -> bool) -> int))))


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

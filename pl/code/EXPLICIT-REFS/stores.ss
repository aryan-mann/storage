#!r7rs

;;; Stores and references for EXPLICIT-REFS

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 18, 2009
;;; last revised July 16, 2019

;;; This library defines data types
;;; for stores and references
;;; in the EXPLICIT-REFS programming language,
;;; as described in section 4.2 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (EXPLICIT-REFS stores)

  (export empty-store initialize-store! get-store
          reference? newref deref setref!)
  (import (scheme base)
          (utilities eopl))
  (begin

    ;; Storage locations are allocated
    ;; by adding new cons cells
    ;; to the end of a list
    ;; stored in a global variable.

    (define the-store 'uninitialized)

    ;; empty-store : () -> Sto

    (define empty-store
      (lambda () '()))

    ;; initialize-store! : () -> Unspecified

    (define initialize-store!
      (lambda ()
        (set! the-store (empty-store))))

    ;; get-store : () -> Sto

    (define get-store
      (lambda () the-store))

    ;; References are natural numbers
    ;; denoting (zero-based) positions in the-store,
    ;; but are treated as values of a separate data type.

    ;; reference? : SchemeVal -> Bool

    (define reference?
      (lambda (v)
        (and (exact-integer? v)
             (not (negative? v)))))

    ;; newref : ExpVal -> Ref

    (define newref
      (lambda (val)
        (let ((next-ref (length the-store)))
          (set! the-store (append the-store (list val)))
          next-ref)))

    ;; deref : Ref -> ExpVal

    (define deref
      (lambda (ref)
        (list-ref the-store ref)))

    ;; setref! : Ref * ExpVal -> Unspecified

    (define setref!
      (lambda (ref val)
        (set! the-store
          (letrec ((setref-inner
                    (lambda (store1 ref1)
                      (cond ((null? store1)
                             (report-invalid-reference-error ref))
                            ((zero? ref1)
                             (cons val (cdr store1)))
                            (else
                             (cons (car store1)
                                   (setref-inner (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

    (define report-invalid-reference-error
      (lambda (ref)
        (eopl:error 'setref! "The reference ~s is invalid.~%" ref)))))

;;; This code is derived from procedures
;;; published in _Essentials of Programming Languages_,
;;; by Daniel P. Friedman and Mitchell Wand
;;; (Cambridge, Massachusetts: MIT Press, 2008;
;;; ISBN 978-0-262-06279-4).

;;; It was later published on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; This version is
;;; copyright (C) 2009, 2015, 2019 John David Stone
;;; and is likewise published
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

#!r7rs

;;; A type checker for the CHECKED language

;;; Daniel P. Friedman (dfried@cs.indiana.edu)
;;; Mitchell Wand (wand@ccs.neu.edu)

;;; This file provides a type-checker for the CHECKED language
;;; developed by the authors
;;; in section 7.3 of their book
;;; _Essentials of programming languages_ (third edition).

;;; adapted and ported to R7RS Scheme by
;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; last revised July 25, 2019

(define-library (CHECKED type-checker)
  (export type-of-program type-to-external-form)
  (import (scheme base)
          (utilities eopl)
          (CHECKED syntax-trees))
  (begin

    ;; During its static analysis of a program's text,
    ;; the type checker keeps track of the type
    ;; of every bound variable
    ;; in a "type environment" --
    ;; a mapping from identifiers to their declared or inferred types.

    (define-datatype type-environment type-environment?
      (empty-tenv)
      (extend-tenv (var symbol?)
                   (ty type?)
                   (saved type-environment?)))

    ;; The apply-tenv procedure looks up a given symbol
    ;; in a given type environment.

    ;; apply-tenv : Tenv * Sym -> Type

    (define apply-tenv
      (lambda (tenv sought)
        (let kernel ((remaining tenv))
          (cases type-environment remaining
            (empty-tenv ()
              (report-untyped-identifier-error sought))
            (extend-tenv (var ty saved)
              (if (eqv? var sought)
                  ty
                  (kernel saved)))))))

    (define report-untyped-identifier-error
      (lambda (violator)
        (eopl:error 'apply-tenv
                    "No declared or inferred type for ~s.~%"
                    violator)))

    ;; CHECKED programs are evaluated in an initial environment
    ;; containing bindings for a few Roman numerals.
    ;; The init-tenv procedure constructs the corresponding type environment,
    ;; stipulating that these identifiers have numeric values.

    ;; init-tenv : () -> Tenv

    (define init-tenv
      (lambda ()
        (extend-tenv 'i (int-type)
          (extend-tenv 'v (int-type)
            (extend-tenv 'x (int-type)
              (empty-tenv))))))

    ;; The check-equal-type! procedure compares two types
    ;; and reports an error if they are not equal.

    ;; check-equal-type! : Type * Type * Exp -> Unspecified

    (define check-equal-type!
      (lambda (ty1 ty2 exp)
        (unless (equal? ty1 ty2)
          (report-unequal-types ty1 ty2 exp))))

    ;; The report-unequal-types procedure signals a type mismatch
    ;; and provides a helpful error message.

    ;; report-unequal-types : Type * Type * Exp -> (does not return)

    (define report-unequal-types
      (lambda (ty1 ty2 exp)
        (eopl:error 'check-equal-type!
                    "Types didn't match: ~s is not ~s in ~a.~%"
                    (type-to-external-form ty1)
                    (type-to-external-form ty2)
                    exp)))

    ;; The type-to-external-form procedure
    ;; returns a Scheme value
    ;; of which the printed representation
    ;; mimics the external representation of types in CHECKED programs.

    ;; type-to-external-form : Type -> SchemeVal

    (define type-to-external-form
      (lambda (ty)
        (cases type ty
          (int-type () 'int)
          (bool-type () 'bool)
          (proc-type (arg-type result-type)
            (list (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type))))))

    ;;; The type-of-program procedure
    ;;; computes and returns the type of a CHECKED program.

    ;; type-of-program : Program -> Type

    (define type-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (exp) (type-of exp (init-tenv))))))

    ;;; The type-of procedure
    ;;; computes and returns the type of a CHECKED expression.

    ;; type-of : Expression * Tenv -> Type

    (define type-of
      (lambda (exp tenv)
        (cases expression exp
          (const-exp (datum) (int-type))
          (diff-exp (minuend subtrahend)
            (let ((minuend-type (type-of minuend tenv))
                  (subtrahend-type (type-of subtrahend tenv)))
              (check-equal-type! minuend-type (int-type) minuend)
              (check-equal-type! subtrahend-type (int-type) subtrahend)
              (int-type)))
          (zero?-exp (testee)
            (let ((testee-type (type-of testee tenv)))
              (check-equal-type! testee-type (int-type) testee)
              (bool-type)))
          (if-exp (condition consequent alternative)
            (let ((condition-type (type-of condition tenv))
                  (consequent-type (type-of consequent tenv))
                  (alternative-type (type-of alternative tenv)))
              (check-equal-type! condition-type (bool-type) condition)
              (check-equal-type! consequent-type alternative-type exp)
              consequent-type))
          (var-exp (id) (apply-tenv tenv id))
          (let-exp (bound-var bound-value body)
            (let ((bound-value-type (type-of bound-value tenv)))
              (type-of body (extend-tenv bound-var bound-value-type tenv))))
          (proc-exp (parameter parameter-type body)
            (let ((return-type
                    (type-of body
                             (extend-tenv parameter parameter-type tenv))))
              (proc-type parameter-type return-type)))
          (call-exp (operator operand)
            (let ((operator-type (type-of operator tenv))
                  (operand-type (type-of operand tenv)))
              (cases type operator-type
                (int-type ()
                  (report-operator-not-a-proc-type operator-type operator))
                (bool-type ()
                  (report-operator-not-a-proc-type operator-type operator))
                (proc-type (arg-type result-type)
                  (check-equal-type! arg-type operand-type operand)
                  result-type))))
          (letrec-exp (return-type procedure-name parameter parameter-type
                       procedure-body letrec-body)
            (let ((tenv-for-letrec-body
                    (extend-tenv procedure-name
                                 (proc-type parameter-type return-type)
                                 tenv)))
              (let ((procedure-body-type
                      (type-of procedure-body
                               (extend-tenv parameter
                                            parameter-type 
                                            tenv-for-letrec-body))))
                (check-equal-type! procedure-body-type return-type procedure-body)
                (type-of letrec-body tenv-for-letrec-body)))))))

    ;; The report-operator-not-a-proc-type raises an error
    ;; when a value that is not a procedure
    ;; has been discovered in the operator position in a procedure call.

    ;; report-operator-not-a-proc-type : Type * Expression -> (does not return)

    (define report-operator-not-a-proc-type
      (lambda (bad-type bad-operator)
        (eopl:error 'type-of
                    "~a is of type ~s and so cannot be an operator.~%"
                    bad-operator
                    (type-to-external-form bad-type))))))

;;; As mentioned above,
;;; the procedure definitions are derived from the work
;;; of Friedman and Wand,
;;; who made their versions available at
;;; http://www.eopl3.com/code.html
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The modified code presented here
;;; is copyright (C) 2009, 2015, 2019 by John David Stone
;;; is are similarly released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.

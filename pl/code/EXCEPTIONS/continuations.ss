#!r7rs

;;; Continuations for the EXCEPTIONS interpreter

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; created March 8, 2009
;;; last revised July 25, 2019

;;; This module defines a data type
;;; for continuations in the EXCEPTIONS programming language,
;;; as described in section 5.4 of
;;; _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.

(define-library (EXCEPTIONS continuations)
  (export continuation? end-cont diff1-cont diff2-cont zero1-cont if-test-cont
          let-exp-cont rator-cont rand-cont try-cont raise-body-cont cons1-cont
          cons2-cont car1-cont cdr1-cont null1-cont list-element-cont)
  (import (scheme base)
          (utilities eopl)
          (EXCEPTIONS syntax-trees)
          (EXCEPTIONS expvals-and-environments))
  (begin

    (define-datatype continuation continuation?
      (end-cont)
      (diff1-cont (subtrahend expression?)
                  (env environment?)
                  (saved-cont continuation?))
      (diff2-cont (minuend-value expval?)
                  (saved-cont continuation?))
      (zero1-cont (saved-cont continuation?))
      (if-test-cont (consequent expression?)
                    (alternative expression?)
                    (env environment?)
                    (saved-cont continuation?))
      (let-exp-cont (bound-var identifier?)
                    (body expression?)
                    (env environment?)
                    (saved-cont continuation?))
      (rator-cont (operand expression?)
                  (env environment?)
                  (saved-cont continuation?))
      (rand-cont (operator-value expval?)
                 (saved-cont continuation?))
      (try-cont (parameter identifier?)
                (handler-body expression?)
                (env environment?)
                (saved-cont continuation?))
      (raise-body-cont (saved-cont continuation?))
      (cons1-cont (aft expression?)
                  (env environment?)
                  (saved-cont continuation?))
      (cons2-cont (fore-value expval?)
                  (saved-cont continuation?))
      (car1-cont (saved-cont continuation?))
      (cdr1-cont (saved-cont continuation?))
      (null1-cont (saved-cont continuation?))
      (list-element-cont (so-far (list-of expval?))
                         (remaining (list-of expression?))
                         (env environment?)
                         (saved-cont continuation?)))))

;;; This definition is derived from the work
;;; of Friedman and Wand, who made their versions available at
;;; http://www.eopl3.com/code.html under the Creative Commons
;;; Attribution-Noncommercial 3.0 Unported license
;;; (http://creativecommons.org/licenses/by-nc/3.0/).

;;; The modified code presented here
;;; is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported license.

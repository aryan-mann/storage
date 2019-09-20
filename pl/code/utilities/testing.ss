#!r7rs

;;; Syntaxes for testing

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 8, 1998
;;; last revised July 5, 2019

(define-library (utilities testing)
  (export suite test true? false? match)
  (import (scheme base)
          (scheme write))
  (begin

    ;; The suite syntax has as subexpressions
    ;; a name,
    ;; a parenthesis-enclosed sequence
    ;; of zero or more binding specifications,
    ;; and zero or more test-expressions (described below).
    ;; It binds the identifiers in the binding specifications
    ;; to the values of the corresponding expressions,
    ;; evaluates the test-expressions
    ;; in the environment established by those bindings,
    ;; and tallies and reports the number of tests that succeeded
    ;; (returning #t) and the number that failed (returning #f).
    ;; The value of the suite-expression
    ;; is #t if no test failed,
    ;; #f if one or more tests failed.

    (define-syntax suite
      (syntax-rules ()
        ((suite name (setup-binding ...) test ...)
         (let ((successes 0) (failures 0) setup-binding ...)
           (if test
               (set! successes (+ successes 1))
               (set! failures (+ failures 1))) ...
           (display successes)
           (display "-")
           (display failures)
           (display ": ")
           (display 'name)
           (newline)
           (zero? failures)))))

    ;; The test syntax has four or five subexpressions.
    ;;
    ;; * The first is an identifier
    ;;   indicating the nature of the test to be conducted. 
    ;;
    ;; * The second is an expression embodying the test:
    ;;   the values of the expression are the results of the test.
    ;;
    ;; * The third is a numeral
    ;;   indicating how many results the test should produce.
    ;;
    ;; * The fourth is a list of expressions,
    ;;   equal in number to the value of the preceding numeral.
    ;;   The value of each expression in the list is a predicate;
    ;;   the corresponding result is required
    ;;   to satisfy this predicate.
    ;;
    ;; * The fifth, which is optional, is an expression
    ;;   of which the value is a predicate.
    ;;   This predicate is applied to all of the results
    ;;   (as arguments in a single call)
    ;;   and they are required to satisfy it.
    ;;
    ;; The value of the test-expression is #t
    ;; if all of the requirements imposed
    ;; by the third, fourth, and fifth subexpression
    ;; are satisfied,
    ;; #f if any of those subexpressions fails.

    (define-syntax test
      (syntax-rules ()
        ((test name trial count (criterion ...) joint-criterion)
         (call-with-values
           (lambda () trial)
           (lambda results
             (if (and (= count (length results))
                      (let tl ((remaining-results results)
                               (remaining-criteria (list criterion ...)))
                        (or (null? remaining-results)
                            (null? remaining-criteria)
                            (and ((car remaining-criteria)
                                  (car remaining-results))
                                 (tl (cdr remaining-results)
                                     (cdr remaining-criteria)))))
                      (apply joint-criterion results))
                 #t
                 (begin
                   (display "*** Test ")
                   (display 'name)
                   (display " failed.")
                   (newline)
                   (display "--- Failing expression: ")
                   (write 'trial)
                   (newline)
                   (do ((remaining-results results (cdr remaining-results))
                        (counter 0 (+ counter 1)))
                       ((null? remaining-results))
                     (display "--- Result #")
                     (display counter)
                     (display ": ")
                     (write (car remaining-results))
                     (newline))
                   #f)))))
        ((test name trial count (criterion ...))
         (test name trial count (criterion ...) (lambda results #t)))))

    ;; The following procedures are frequently used in tests.

    ;; The true? procedure
    ;; determines whether is argument is specifically the Boolean value #t.

    (define true?
      (lambda (something)
        (eq? something #t)))

    ;; The false? procedure
    ;; determines whether its argument is specifically the Boolean value #f.

    (define false? not)

    ;; The match procedure
    ;; constructs and returns a unary predicate
    ;; that in turn determines whether its argument
    ;; bears a given relationship, expressed by a binary predicate,
    ;; to a given value.

    (define match
      (lambda (relationship value)
        (lambda (something)
          (relationship something value))))))

;;; Copyright (C) 1998, 2011, 2016, 2018, 2019 John David Stone

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

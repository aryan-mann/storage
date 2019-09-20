#!r7rs

;;; Tests for the (EXPLICIT-REFS expvals-and-environments) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 16, 2019
;;; last revised July 17, 2019

(import (scheme base)
        (scheme write)
        (utilities testing)
        (utilities eopl)
        (EXPLICIT-REFS syntax-trees)
        (EXPLICIT-REFS stores)
        (EXPLICIT-REFS expvals-and-environments))

(suite expval? ()

  (test expval?:num-val
    (expval? (num-val 0))
    1 (true?))

  (test expval?:bool-val
    (expval? (bool-val #f))
    1 (true?))

  (test expval?:proc-val
    (expval? (proc-val (a-proc 'foo (const-exp 42) (empty-env))))
    1 (true?))

  (test expval?:ref-val
    (expval? (ref-val 1))
    1 (true?))

  (test expval?:no
    (expval? #t)
    1 (false?)))


(suite expval->num ((error-message
                     (let ((message (open-output-string)))
                       (display "In expval->num: undefined for expressed value "
                                message)
                       (write (bool-val #t) message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->num:yes
    (expval->num (num-val 1))
    1 (exact-integer?)
    (match = 1))

  (test expval->num:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->num (bool-val #t)))
    1 (symbol?)
    (match eq? 'pass)))


(suite expval->bool ((error-message
                     (let ((message (open-output-string)))
                       (display "In expval->bool: undefined for expressed value "
                                message)
                       (write (proc-val (a-proc 'bar (var-exp 'bar) (init-env)))
                              message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->bool:yes
    (expval->bool (bool-val #f))
    1 (false?))

  (test expval->bool:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->bool (proc-val (a-proc 'bar (var-exp 'bar) (init-env)))))
    1 (symbol?)
    (match eq? 'pass)))


(suite expval->proc ((error-message
                      (let ((message (open-output-string)))
                        (display "In expval->proc: undefined for expressed value "
                                 message)
                        (write (ref-val 7) message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->proc:yes
    (expval->proc (proc-val (a-proc 'baz
                                    (const-exp 44)
                                    (extend-env 'l (num-val 50) (empty-env)))))
    1 (proc?)
    (lambda (result)
      (cases proc result
        (a-proc (parameter body saved-env)
          (and (eq? parameter 'baz)
               (cases expression body
                 (const-exp (datum) (= datum 44))
                 (else #f))
               (cases environment saved-env
                 (extend-env (var val saved)
                   (and (eq? var 'l)
                        (cases expval val
                          (num-val (num) (= num 50))
                          (else #f))
                        (cases environment saved
                          (empty-env () #t)
                          (else #f))))))))))

  (test expval->proc:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->proc (ref-val 7)))
    1 (symbol?)
    (match eq? 'pass)))


(suite expval->ref ((error-message
                     (let ((message (open-output-string)))
                       (display "In expval->ref: undefined for expressed value "
                                message)
                       (write (num-val 17) message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->ref:yes
    (expval->ref (ref-val 2))
    1 (reference?)
    (match = 2))

  (test expval->ref:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->ref (num-val 17)))
    1 (symbol?)
    (match eq? 'pass)))


(suite proc? ()

  (test proc?:yes
    (proc? (a-proc 'quux (var-exp 'wombat) (empty-env)))
    1 (true?))

  (test proc?:no
    (proc? "yes")
    1 (false?)))

(suite environment? ()

  (test environment?:empty
    (environment? (empty-env))
    1 (true?))

  (test environment?:non-empty
    (environment? (init-env))
    1 (true?))

  (test environment?:with-recursive-procedures
    (environment? (extend-env 'l (num-val 50)
                    (extend-env-rec '(double treble)
                                    '(x x)
                                    (list (if-exp (zero?-exp (var-exp 'x))
                                                  (const-exp 0)
                                                  (diff-exp
                                                    (call-exp
                                                      (var-exp 'double)
                                                      (diff-exp
                                                        (var-exp 'x)
                                                        (const-exp 1)))
                                                      (const-exp -2)))
                                          (if-exp (zero?-exp (var-exp 'x))
                                                  (const-exp 0)
                                                  (diff-exp
                                                    (call-exp
                                                      (var-exp 'treble)
                                                      (diff-exp
                                                        (var-exp 'x)
                                                        (const-exp 1)))
                                                      (const-exp -3))))
                                    (init-env))))
    1 (true?))

  (test environment?:no
    (environment? 'garply)
    1 (false?)))


(suite apply-env ((em1
                     (let ((message (open-output-string)))
                       (display "In apply-env: No binding for fred was found "
                                message)
                       (display "in environment " message)
                       (write (empty-env) message)
                       (display "." message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result)))
                  (em2
                     (let ((message (open-output-string)))
                       (display "In apply-env: No binding for bork was found "
                                message)
                       (display "in environment " message)
                       (write (init-env) message)
                       (display "." message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result)))
                  (bigenv
                     (extend-env-rec '(double treble)
                                     '(x x)
                                     (list (if-exp (zero?-exp (var-exp 'x))
                                                   (const-exp 0)
                                                   (diff-exp
                                                    (call-exp
                                                     (var-exp 'double)
                                                     (diff-exp
                                                      (var-exp 'x)
                                                      (const-exp 1)))
                                                    (const-exp -2)))
                                           (if-exp (zero?-exp (var-exp 'x))
                                                   (const-exp 0)
                                                   (diff-exp
                                                    (call-exp
                                                     (var-exp 'treble)
                                                     (diff-exp
                                                      (var-exp 'x)
                                                      (const-exp 1)))
                                                    (const-exp -3))))
                       (extend-env 'l (num-val 50) (empty-env)))))

  (test apply-env:immediate-failure
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) em1))
             'pass)
            (else 'fail))
      (apply-env (empty-env) 'fred))
    1 (symbol?)
    (match eq? 'pass))

  (test apply-env:success
    (apply-env (extend-env 'a (num-val 1)
                  (extend-env 'b (num-val 2)
                    (extend-env 'c (num-val 3)
                      (extend-env 'd (num-val 4) (empty-env)))))
               'c)
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 3)))

  (test apply-env:failure-at-end
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) em2))
             'pass)
            (else 'fail))
      (apply-env (init-env) 'bork))
    1 (symbol?)
    (match eq? 'pass))

  (test apply-env:finding-recursive-procedure
    (apply-env bigenv 'treble)
    1 (expval?)
    (lambda (result)
      (cases proc (expval->proc result)
        (a-proc (parameter body saved-env)
          (and (eq? parameter 'x)
               (cases expression body
                 (if-exp (condition consequent alternative)
                   (and (cases expression condition
                          (zero?-exp (testee)
                            (cases expression testee
                              (var-exp (id) (eq? id 'x))
                              (else #f)))
                          (else #f))
                        (cases expression consequent
                          (const-exp (datum) (zero? datum))
                          (else #f))
                        (cases expression alternative
                          (diff-exp (minuend subtrahend)
                            (and (cases expression minuend
                                   (call-exp (operator operand)
                                     (and (cases expression operator
                                            (var-exp (id) (eq? id 'treble))
                                            (else #f))
                                          (cases expression operand
                                            (diff-exp (minuend subtrahend)
                                              (and (cases expression minuend
                                                     (var-exp (id) (eq? id 'x))
                                                     (else #f))
                                                   (cases expression subtrahend
                                                     (const-exp (datum) (= datum 1))
                                                     (else #f))))
                                            (else #f))))
                                   (else #f))
                                 (cases expression subtrahend
                                   (const-exp (datum) (= datum -3))
                                   (else #f))))
                          (else #f))))
                 (else #f))
               (cases environment saved-env
                 (extend-env-rec (p-names b-vars bodies saved)
                   (and (equal? p-names '(double treble))
                        (equal? b-vars '(x x))
                        (pair? bodies)
                        (cases expression (car bodies)
                          (if-exp (condition consequent alternative)
                            (and (cases expression condition
                                   (zero?-exp (testee)
                                     (cases expression testee
                                       (var-exp (id) (eq? id 'x))
                                       (else #f)))
                                   (else #f))
                                 (cases expression consequent
                                   (const-exp (datum) (zero? datum))
                                   (else #f))
                                 (cases expression alternative
                                   (diff-exp (minuend subtrahend)
                                     (and (cases expression minuend
                                            (call-exp (operator operand)
                                              (and (cases expression operator
                                                     (var-exp (id) (eq? id 'double))
                                                     (else #f))
                                                   (cases expression operand
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (cases expression subtrahend
                                            (const-exp (datum) (= datum -2))
                                            (else #f))))
                                   (else #f))))
                          (else #f))
                        (pair? (cdr bodies))
                        (cases expression (cadr bodies)
                          (if-exp (condition consequent alternative)
                            (and (cases expression condition
                                   (zero?-exp (testee)
                                     (cases expression testee
                                       (var-exp (id) (eq? id 'x))
                                       (else #f)))
                                   (else #f))
                                 (cases expression consequent
                                   (const-exp (datum) (zero? datum))
                                   (else #f))
                                 (cases expression alternative
                                   (diff-exp (minuend subtrahend)
                                     (and (cases expression minuend
                                            (call-exp (operator operand)
                                              (and (cases expression operator
                                                     (var-exp (id) (eq? id 'treble))
                                                     (else #f))
                                                   (cases expression operand
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))))
                                            (else #f))
                                          (cases expression subtrahend
                                            (const-exp (datum) (= datum -3))
                                            (else #f))))
                                   (else #f))))
                          (else #f))
                        (null? (cddr bodies))
                        (cases environment saved
                          (extend-env (var val saved)
                            (and (eq? var 'l)
                                 (= (expval->num val) 50)
                                 (cases environment saved
                                   (empty-env () #t)
                                   (else #f))))
                          (else #f))))
                 (else #f))))))))


(suite init-env ()

  (test init-env:check
    (init-env)
    1 (environment?)
    (lambda (result)
      (cases environment result
        (extend-env (var val saved)
          (and (eq? var 'i)
               (= (expval->num val) 1)
               (cases environment saved
                 (extend-env (var val saved)
                   (and (eq? var 'v)
                        (= (expval->num val) 5)
                        (cases environment saved
                          (extend-env (var val saved)
                            (and (eq? var 'x)
                                 (= (expval->num val) 10)
                                 (cases environment saved
                                   (empty-env () #t)
                                   (else #f))))
                          (else #f))))
                 (else #f))))
        (else #f)))))


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
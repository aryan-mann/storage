#!r7rs

;;; Tests for the (CLASSES expvals-and-environments) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 31, 2019
;;; last revised August 1, 2019

(import (scheme base)
        (scheme write)
        (scheme cxr)
        (utilities testing)
        (utilities eopl)
        (CLASSES syntax-trees)
        (CLASSES stores)
        (CLASSES expvals-and-environments))

(suite expval? ()

  (test expval?:num-val
    (expval? (num-val 0))
    1 (true?))

  (test expval?:bool-val
    (expval? (bool-val #f))
    1 (true?))

  (test expval?:proc-val
    (expval? (proc-val (a-proc (list 'foo) (const-exp 42) (empty-env))))
    1 (true?))

  (test expval?:list-val
    (expval? (list-val (list (num-val 1) (bool-val #t) (list-val (list)))))
    1 (true?))

  (test expval?:object-val
    (expval? (object-val (an-object 'bar (list))))
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
                       (write (proc-val (a-proc (list 'bar) (var-exp 'bar) (init-env)))
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
      (expval->bool (proc-val (a-proc (list 'bar) (var-exp 'bar) (init-env)))))
    1 (symbol?)
    (match eq? 'pass)))

(suite expval->proc ((error-message
                      (let ((message (open-output-string)))
                        (display "In expval->proc: undefined for expressed value "
                                 message)
                        (write (list-val (list (num-val 43) (num-val 45))) message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->proc:yes
    (begin
      (initialize-store!)
      (expval->proc (proc-val (a-proc (list 'baz)
                                      (const-exp 44)
                                      (extend-env (list 'l)
                                                  (list (newref (num-val 50)))
                                                  (empty-env))))))
    1 (proc?)
    (lambda (result)
      (cases proc result
        (a-proc (parameters body saved-env)
          (and (equal? parameters (list 'baz))
               (cases expression body
                 (const-exp (datum) (= datum 44))
                 (else #f))
               (cases environment saved-env
                 (extend-env (vars locs saved)
                   (and (equal? vars (list 'l))
                        (pair? locs)
                        (= (expval->num (deref (car locs))) 50)
                        (null? (cdr locs))
                        (cases environment saved
                          (empty-env () #t)
                          (else #f))))))))))

  (test expval->proc:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->proc (list-val (list (num-val 43) (num-val 45)))))
    1 (symbol?)
    (match eq? 'pass)))


(suite expval->elements ((error-message
                          (let ((message (open-output-string)))
                            (display "In expval->elements: undefined for expressed value "
                                     message)
                            (write (object-val (an-object 'm (list))) message)
                            (newline message)
                            (let ((result (get-output-string message)))
                              (close-output-port message)
                              result))))

  (test expval->elements:no-elements
    (expval->elements (list-val (list)))
    1 ((list-of expval?))
    null?)

  (test expval->elements:yes
    (expval->elements (list-val (list (num-val 47) (bool-val #f) (list-val (list)))))
    1 ((list-of expval?))
    (lambda (result)
      (and (pair? result)
           (= (expval->num (car result)) 47)
           (pair? (cdr result))
           (false? (expval->bool (cadr result)))
           (pair? (cddr result))
           (null? (expval->elements (caddr result)))
           (null? (cdddr result)))))

  (test expval->elements:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->elements (object-val (an-object 'm (list)))))
    1 (symbol?)
    (match eq? 'pass)))


(suite expval->obj ((error-message
                     (let ((message (open-output-string)))
                       (display "In expval->obj: undefined for expressed value "
                                message)
                       (write (num-val 48) message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result))))

  (test expval->obj:yes
    (expval->obj (object-val (an-object 'n (list))))
    1 (object?)
    (lambda (result)
      (cases object result
        (an-object (class-name fields)
          (and (eq? class-name 'n)
               (null? fields))))))

  (test expval->obj:no
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) error-message))
             'pass)
            (else 'fail))
      (expval->obj (num-val 48)))
    1 (symbol?)
    (match eq? 'pass)))


(suite proc? ()

  (test proc?:yes
    (proc? (a-proc (list 'quux) (var-exp 'wombat) (empty-env)))
    1 (true?))

  (test proc?:no
    (proc? "yes")
    1 (false?)))


(suite environment? ()

  (test environment?:empty
    (environment? (empty-env))
    1 (true?))

  (test environment?:non-empty
    (begin
      (initialize-store!)
      (environment? (extend-env (list 'i 'v 'x)
                                (map newref
                                     (list (num-val 1) (num-val 5) (num-val 10)))
                                (empty-env))))
    1 (true?))

  (test environment?:with-recursive-procedure
    (begin
      (initialize-store!)
      (environment? (extend-env (list 'l) (list (newref (num-val 50)))
                      (extend-env-rec (list 'double)
                                      (list (list 'x))
                                      (list (if-exp (zero?-exp (var-exp 'x))
                                                    (const-exp 0)
                                                    (diff-exp
                                                      (call-exp
                                                        (var-exp 'double)
                                                        (list (diff-exp (var-exp 'x)
                                                                        (const-exp 1))))
                                                      (const-exp -2))))
                                      (init-env)))))
    1 (true?))

  (test environment?:with-self-and-super
    (environment? (extend-env-with-self-and-super
                    (an-object 'c2 (list))
                    'c1
                    (empty-env)))
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
                       (initialize-store!)
                       (display "In apply-env: No binding for bork was found "
                                message)
                       (display "in environment " message)
                       (write (extend-env (list 'e 'f)
                                          (map newref (list (num-val 5) (num-val 6)))
                                          (extend-env (list 'g 'h)
                                                      (map newref
                                                           (list (num-val 7) (num-val 8)))
                                                      (empty-env)))
                              message)
                       (display "." message)
                       (newline message)
                       (let ((result (get-output-string message)))
                         (close-output-port message)
                         result)))
                  (bigenv
                     (lambda ()
                       (initialize-store!)
                       (extend-env-rec (list 'double)
                                       (list (list 'x))
                                       (list (if-exp (zero?-exp (var-exp 'x))
                                                     (const-exp 0)
                                                     (diff-exp
                                                      (call-exp
                                                       (var-exp 'double)
                                                       (list (diff-exp (var-exp 'x)
                                                                       (const-exp 1))))
                                                      (const-exp -2))))
                                       (extend-env (list 'l)
                                                   (list (newref (num-val 50)))
                                                   (empty-env)))))
                  (env-with-object
                    (lambda ()
                      (initialize-store!)
                      (extend-env (list 'i 'j)
                                  (map newref (list (num-val 9) (num-val 10)))
                                  (extend-env-with-self-and-super
                                    (an-object 'c2 (list))
                                    'c1
                                    (extend-env (list 'k 'l)
                                                (map newref
                                                     (list (num-val 11) (num-val 12)))
                                                (empty-env)))))))

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
    (begin
      (initialize-store!)
      (apply-env (extend-env (list 'a) 
                             (list (newref (num-val 1)))
                             (extend-env (list 'b 'c 'd)
                                         (map newref
                                              (list (num-val 2) (num-val 3) (num-val 4)))
                                         (empty-env)))
               'c))
    1 (reference?)
    (lambda (result)
      (= (expval->num (deref result)) 3)))

  (test apply-env:failure-at-end
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) em2))
             'pass)
            (else 'fail))
      (begin
        (initialize-store!)
        (apply-env (extend-env (list 'e 'f)
                               (map newref (list (num-val 5) (num-val 6)))
                               (extend-env (list 'g 'h)
                                           (map newref 
                                                (list (num-val 7) (num-val 8)))
                                           (empty-env)))
                   'bork)))
    1 (symbol?)
    (match eq? 'pass))

  (test apply-env:finding-recursive-procedure
    (apply-env (bigenv) 'double)
    1 (reference?)
    (lambda (result)
      (cases proc (expval->proc (deref result))
        (a-proc (parameters body saved-env)
          (and (equal? parameters (list 'x))
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
                                   (call-exp (operator operands)
                                     (and (cases expression operator
                                            (var-exp (id) (eq? id 'double))
                                            (else #f))
                                          (pair? operands)
                                          (cases expression (car operands)
                                            (diff-exp (minuend subtrahend)
                                              (and (cases expression minuend
                                                     (var-exp (id) (eq? id 'x))
                                                     (else #f))
                                                   (cases expression subtrahend
                                                     (const-exp (datum) (= datum 1))
                                                     (else #f))))
                                            (else #f))
                                          (null? (cdr operands))))
                                   (else #f))
                                 (cases expression subtrahend
                                   (const-exp (datum) (= datum -2))
                                   (else #f))))
                          (else #f))))
                 (else #f))
               (cases environment saved-env
                 (extend-env-rec (p-names p-lists bodies saved)
                   (and (equal? p-names (list 'double))
                        (equal? p-lists (list (list 'x)))
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
                                            (call-exp (operator operands)
                                              (and (cases expression operator
                                                     (var-exp (id) (eq? id 'double))
                                                     (else #f))
                                                   (pair? operands)
                                                   (cases expression (car operands)
                                                     (diff-exp (minuend subtrahend)
                                                       (and (cases expression minuend
                                                              (var-exp (id) (eq? id 'x))
                                                              (else #f))
                                                            (cases expression subtrahend
                                                              (const-exp (datum) (= datum 1))
                                                              (else #f))))
                                                     (else #f))
                                                   (null? (cdr operands))))
                                            (else #f))
                                          (cases expression subtrahend
                                            (const-exp (datum) (= datum -2))
                                            (else #f))))
                                   (else #f))))
                          (else #f))
                        (cases environment saved
                          (extend-env (vars locs saved)
                            (and (equal? vars (list 'l))
                                 (pair? locs)
                                 (= (expval->num (deref (car locs))) 50)
                                 (null? (cdr locs))
                                 (cases environment saved
                                   (empty-env () #t)
                                   (else #f))))
                          (else #f))))
                 (else #f)))))))

  (test apply-env:finding-self
    (apply-env (env-with-object) '%self)
    1 (object?)
    (lambda (result)
      (cases object result
        (an-object (class-name fields)
          (and (eq? class-name 'c2)
               (null? fields))))))

  (test apply-env:finding-super
    (apply-env (env-with-object) '%super)
    1 (identifier?)
    (match eq? 'c1)))


(suite init-env ()

  (test init-env:check
    (init-env)
    1 (environment?)
    (lambda (result)
      (cases environment result
        (empty-env () #t)
        (else #f)))))


(suite object? ()

  (test object?:yes
    (begin
      (initialize-store!)
      (object? (an-object 'c3 (list (newref (list 'uninitialized-field 'f1))
                                    (newref (list 'uninitialized-field 'f2))
                                    (newref (list 'uninitialized-field 'f3))))))
    1 (true?))

  (test object?:no
    (object? 91)
    1 (false?)))


(suite object->class-name ()

  (test object->class-name:no-surprises
    (begin
      (initialize-store!)
      (object->class-name (an-object 'c4 (list (newref (num-val 92))
                                               (newref (num-val 93))))))
    1 (identifier?)
    (match eq? 'c4)))


(suite object->fields ()

  (test object->fields:no-surprises
    (begin
      (initialize-store!)
      (object->fields (an-object 'c5 (list (newref (num-val 94))))))
    1 ((list-of reference?))
    (lambda (result)
      (and (pair? result)
           (= (expval->num (deref (car result))) 94)
           (null? (cdr result))))))

(suite new-object ((dummy
                      (begin
                        (initialize-store!)
                        (initialize-class-env!
                         (list (a-class-declaration 'c6 'object (list) (list))
                               (a-class-declaration 'c7 'c6
                                                    (list 'f11 'f12 'f13)
                                                    (list)))))))

  (test new-object:no-fields
    (new-object 'c6)
    1 (object?)
    (lambda (result)
      (cases object result
        (an-object (class-name fields)
          (and (eq? class-name 'c6)
               (null? fields))))))

  (test new-object:multiple-fields
    (new-object 'c7)
    1 (object?)
    (lambda (result)
      (cases object result
        (an-object (class-name fields)
          (and (eq? class-name 'c7)
               ((list-of reference?) fields)
               (= (length fields) 3)))))))



(suite method? ()

  (test method?:yes
    (method? (a-method (list 'p1 'p2)
                       (const-exp 95)
                       'object
                       (list 'f14 'f15 'f16)))
    1 (true?))

  (test method?:no
    (method? 'f17)
    1 (false?)))


(suite class? ()

  (test class?:minimal
    (class? (a-class #f '() '()))
    1 (true?))

  (test class?:typical
    (class? (a-class 'object
                     (list 'value)
                     (list (list 'initialize
                                 (a-method (list 'v)
                                           (assign-exp 'value (var-exp 'v))
                                           'object
                                           (list 'value)))
                           (list 'sum
                                 (a-method (list)
                                           (var-exp 'value)
                                           'object
                                           (list 'value))))))
    1 (true?))

  (test class?:no
    (class? "yes!")
    1 (false?)))

(suite initialize-class-env! ()

  (test initialize-class-env!:no-declarations
    (initialize-class-env! (list))
    1 ((lambda (ignored) #t))
    (lambda (ignored)

      (suite no-declarations ()

        (test no-declarations:method-not-found
          (guard (raised-object
                   ((and (error-object? raised-object)
                         (string=? 
                           (error-object-message raised-object)
                           (string-append 
                             "In find-method: "
                             "There is no method named hashcode in class object.\n")))
                    'pass)
                   (else 'fail))
            (find-method 'object 'hashcode))
          1 (symbol?)
          (match eq? 'pass))

        (test no-declarations:class-not-found
          (guard (raised-object
                   ((and (error-object? raised-object)
                         (string=? (error-object-message raised-object)
                                   (string-append
                                     "In lookup-class: "
                                     "unclass is not the name of a class.\n")))
                    'pass)
                   (else 'fail))
             (find-method 'unclass 'unmethod))
          1 (symbol?)
          (match eq? 'pass)))))

  (test initialize-class-env!:some-declarations
    (initialize-class-env! (list (a-class-declaration
                                  'c8
                                  'object
                                  (list)
                                  (list (a-method-declaration
                                         'initialize
                                         (list)
                                         (const-exp 1))))
                                 (a-class-declaration
                                   'c9
                                   'c8
                                   (list 'f17 'f18)
                                   (list (a-method-declaration
                                          'initialize
                                          (list)
                                          (begin-exp
                                            (assign-exp 'f17 (const-exp 96))
                                            (list (assign-exp 'f18 (const-exp 97)))))
                                         (a-method-declaration
                                          'report
                                          (list)
                                          (sum-exp (var-exp 'f18) (var-exp 'f19)))))))
    1 ((lambda (ignored) #t))
    (lambda (ignored)

      (suite some-declarations ()

        (test some-declarations:success
          (find-method 'c9 'report)
          1 (method?)
          (lambda (result)
            (cases method result
              (a-method (parameters body super-name field-names)
                (and (null? parameters)
                     (cases expression body
                       (sum-exp (augend addend)
                         (and (cases expression augend
                                (var-exp (id) (eq? id 'f18))
                                (else #f))
                              (cases expression addend
                                (var-exp (id) (eq? id 'f19))
                                (else #f))))
                       (else #f))
                     (eq? super-name 'c8)
                     (equal? field-names (list 'f17 'f18)))))))

        (test some-declarations:method-not-found
          (guard (raised-object
                   ((and (error-object? raised-object)
                         (string=? 
                           (error-object-message raised-object)
                           (string-append
                             "In find-method: "
                             "There is no method named report in class c8.\n")))
                    'pass)
                   (else 'fail))
            (find-method 'c8 'report))
          1 (symbol?)
          (match eq? 'pass))

        (test some-declarations:class-not-found
           (guard (raised-object
                   ((and (error-object? raised-object)
                         (string=? (error-object-message raised-object)
                                   "In lookup-class: c10 is not the name of a class.\n"))
                    'pass)
                   (else 'fail))
             (find-method 'c10 'report))
           1 (symbol?)
           (match eq? 'pass))))))


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

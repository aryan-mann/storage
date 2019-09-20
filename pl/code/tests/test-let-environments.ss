#!r7rs

;;; Tests for the (LET environments) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 9, 2019
;;; last revised August 5, 2019

(import (scheme base)
        (scheme write)
        (utilities testing)
        (utilities eopl)
        (LET expvals)
        (LET environments))

(suite environment? ()

  (test environment?:empty-env
    (environment? (empty-env))
    1 (true?))

  (test environment?:small-env
    (environment? (extend-env 'a (num-val 0)
                    (extend-env 'b (num-val 1)
                      (empty-env))))
    1 (true?))
 
  (test environment?:no
    (environment? "not an environment")
    1 (false?)))


(suite apply-env ((sample-env (extend-env 'c (num-val 2)
                                (extend-env 'd (num-val 3)
                                  (extend-env 'e (num-val 4)
                                    (extend-env 'f (num-val 5) (empty-env)))))))

  (test apply-env:empty-env
    (let* ((em-port (open-output-string))
           (em (begin
                 (display "In apply-env: " em-port)
                 (display "No binding for g was found in environment " em-port)
                 (write (empty-env) em-port)
                 (display "." em-port)
                 (newline em-port)
                 (get-output-string em-port))))
      (close-output-port em-port)
      (guard (object
              ((and (error-object? object)
                    (string=? (error-object-message object) em))
               'pass)
              (else 'fail))
        (apply-env (empty-env) 'g)))
    1 (symbol?)
    (match eq? 'pass))

  (test apply-env:found
    (apply-env sample-env 'e)
    1 (expval?)
    (lambda (result)
      (cases expval result
        (num-val (num) (= num 4))
        (bool-val (bool) #f))))

  (test apply-env:not-found
    (let* ((em-port (open-output-string))
           (em (begin
                 (display "In apply-env: " em-port)
                 (display "No binding for i was found in environment " em-port)
                 (write sample-env em-port)
                 (display "." em-port)
                 (newline em-port)
                 (get-output-string em-port))))
      (close-output-port em-port)
      (guard (object
              ((and (error-object? object)
                    (string=? (error-object-message object) em))
               'pass)
              (else 'fail))
        (apply-env sample-env 'i)))
    1 (symbol?)
    (match eq? 'pass)))


(suite init-env ()

  (test init-env:check-result
    (init-env)
    1 (environment?)
    (lambda (result)
      (cases environment result
        (extend-env (var val saved)
          (and (eq? var 'i)
               (cases expval val
                 (num-val (num) (= num 1))
                 (else #f))
               (cases environment saved
                 (extend-env (var val saved)
                   (and (eq? var 'v)
                        (cases expval val
                          (num-val (num) (= num 5))
                          (else #f))
                        (cases environment saved
                          (extend-env (var val saved)
                            (and (eq? var 'x)
                                 (cases expval val
                                   (num-val (num) (= num 10))
                                   (else #f))
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

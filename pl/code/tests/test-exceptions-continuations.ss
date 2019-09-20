#!r7rs

;;; Tests for the (EXCEPTIONS continuations) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 19, 2019
;;; last revised July 19, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (EXCEPTIONS syntax-trees)
        (EXCEPTIONS expvals-and-environments)
        (EXCEPTIONS continuations))

(suite continuation? ()

  (test continuation?:end-cont
    (continuation? (end-cont))
    1 (true?))

  (test continuation?:diff1-cont
    (continuation? (diff1-cont (const-exp 0) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:diff2-cont
    (continuation? (diff2-cont (num-val 0) (end-cont)))
    1 (true?))

  (test continuation?:zero1-cont
    (continuation? (zero1-cont (end-cont)))
    1 (true?))

  (test continuation?:if-test-cont
    (continuation? (if-test-cont (const-exp 1) (const-exp 2) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:let-exp-cont
    (continuation? (let-exp-cont 'a (const-exp 3) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:rator-cont
    (continuation? (rator-cont (var-exp 'b) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:rand-cont
    (continuation? (rand-cont (proc-val (a-proc 'c (const-exp 4) (init-env)))
                              (end-cont)))
    1 (true?))

  (test continuation?:try-cont
    (continuation? (try-cont 'd (const-exp 5) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:raise-body-cont
    (continuation? (raise-body-cont (end-cont)))
    1 (true?))

  (test continuation?:cons1-cont
    (continuation? (cons1-cont (emptylist-exp) (init-env) (end-cont)))
    1 (true?))

  (test continuation?:cons2-cont
    (continuation? (cons2-cont (num-val 6) (end-cont)))
    1 (true?))

  (test continuation?:car1-cont
    (continuation? (car1-cont (end-cont)))
    1 (true?))

  (test continuation?:cdr1-cont
    (continuation? (cdr1-cont (end-cont)))
    1 (true?))

  (test continuation?:null1-cont
    (continuation? (null1-cont (end-cont)))
    1 (true?))

  (test continuation?list-element-cont
    (continuation? (list-element-cont (list (num-val 7) (num-val 8))
                                      (list (const-exp 9) (const-exp 10))
                                      (init-env)
                                      (end-cont)))
    1 (true?))

  (test continuation?:no
    (continuation? (num-val 11))
    1 (false?)))

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

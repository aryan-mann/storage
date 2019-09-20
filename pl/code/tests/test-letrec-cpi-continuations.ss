#!r7rs

;;; Tests for the (LETREC-CPI continuations) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 19, 2019
;;; last revised July 19, 2019

(import (scheme base)
        (utilities testing)
        (utilities eopl)
        (LETREC syntax-trees)
        (LETREC expvals-and-environments)
        (LETREC-CPI continuations))

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

  (test continuation?:no
    (continuation? (num-val 5))
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

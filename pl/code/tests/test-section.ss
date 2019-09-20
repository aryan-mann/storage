#!r7rs

;;; Tests for the (utilities section) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created August 23, 2011
;;; last revised June 26, 2019

(import (scheme base)
        (utilities section)
        (utilities testing))

(suite section ()

  (test typical
    (section expt <> 3)
    1 (procedure?)
    (lambda (cube)
      (suite section-typical-internal ()
        (test cube
          (cube 8)
          1 (number?)
          (lambda (result)
            (= result 512))))))

  (test no-slots
    (section expt 5 3)
    1 (procedure?)
    (lambda (thunk)
      (suite no-slots-internal ()
        (test thaw-thunk
          (thunk)
          1 (number?)
          (lambda (result)
            (= result 125))))))

  (test variable-arity
    (section - <> 259 <...>)
    1 (procedure?)
    (lambda (sectioned)
      (suite section-variable-arity ()
        (test no-extra-arguments
          (sectioned 821)
          1 (number?)
          (lambda (result)
            (= result (- 821 259))))
        (test some-extra-arguments
          (sectioned 723 482 805 263)
          1 (number?)
          (lambda (result)
            (= result (- 723 259 482 805 263)))))))

  (test no-fixed-args
    (section reverse <...>)
    1 (procedure?)
    (lambda (variated)
      (suite section-no-fixed-args ()
        (test no-arguments
          (variated)
          1 (null?))
        (test one-argument
          (variated 812)
          1 (list?)
          (lambda (result)
            (equal? result '(812))))
        (test many-arguments
          (variated 112 509 236 279)
          1 (list?)
          (lambda (result)
            (equal? result '(279 236 509 112)))))))

  (test all-slots
    (section <> <> <>)
    1 (procedure?)
    (lambda (binary-call)
      (suite all-slots-internal ()
        (test call
          (binary-call expt 291 880)
          1 (number?)
          (lambda (result)
            (= result (expt 291 880))))))))

;;; copyright © 2011, 2017, 2018, 2019 John David Stone

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

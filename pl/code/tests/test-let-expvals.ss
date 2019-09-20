#!r7rs

;;; Tests for the (LET expvals) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 9, 2019
;;; last revised July 9, 2019

(import (scheme base)
        (scheme write)
        (utilities testing)
        (utilities eopl)
        (LET expvals))

(suite expval? ()

  (test expval?:num-val
    (expval? (num-val 0))
    1 (true?))

  (test expval?:bool-val
    (expval? (bool-val #f))
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
                       (write (num-val 2) message)
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
      (expval->bool (num-val 2)))
    1 (symbol?)
    (match eq? 'pass)))


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

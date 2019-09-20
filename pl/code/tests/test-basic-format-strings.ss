#!r7rs

;;; Tests for the (srfi basic-format-strings) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 9, 2019
;;; last revised July 9, 2019

(import (scheme base)
        (utilities testing)
        (srfi basic-format-strings))

(suite format ()

  (test format:no-control-sequences
    (format "constant")
    1 (string?)
    (match string=? "constant"))

  (test format:tilde-at-end
    (format "tilde ~")
    1 (string?)
    (match string=? "tilde ~"))

  (test format:display-format
    (format "character ~s" #\a)
    1 (string?)
    (match string=? "character a"))

  (test format:write-format
    (format "character ~a" #\b)
    1 (string?)
    (match string=? "character #\\b"))

  (test format:newline
    (format "newline ~%")
    1 (string?)
    (match string=? "newline \n"))

  (test format:tilde
    (format "tilde ~~")
    1 (string?)
    (match string=? "tilde ~"))

  (test format:omnibus
    (format "~s ~a ~s ~a ~a ~~ ~%"
            'foo 42 "bar" #\? '())
    1 (string?)
    (match string=? "foo 42 bar #\\? () ~ \n")))
    
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

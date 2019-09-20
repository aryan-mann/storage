#!r7rs

;;; Tests for the (TYPED-OO type-checker) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created August 5, 2019
;;; last revised August 5, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (TYPED-OO syntax-trees)
        (TYPED-OO parser)
        (TYPED-OO type-checker))

(suite type-of-program ()

  (test type-of-program:cons
    (type-of-program (scan&parse "0"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:var
    (type-of-program (scan&parse "let i = 1 in i"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:diff
    (type-of-program (scan&parse "-(2,5)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:zero?
    (type-of-program (scan&parse "zero?(3)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:if
    (type-of-program (scan&parse "if zero?(4) then 5 else 6"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:let
    (type-of-program (scan&parse "let a = zero?(7) in a"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:proc
    (type-of-program (scan&parse "proc (c: bool) if c then 8 else 9"))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-types result-type)
          (and (pair? arg-types)
               (cases type (car arg-types)
                 (bool-type () #t)
                 (else #f))
               (null? (cdr arg-types))
               (cases type result-type
                 (int-type () #t)
                 (else #f))))
        (else #f))))

  (test type-of-program:call
    (type-of-program (scan&parse "(proc (e: int) 10 11)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:letrec
    (type-of-program (scan&parse "letrec int f (g: int)
                                   = if zero?(g) then 12 else (f -(g, 1))
                                  in (f 13)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:set
    (type-of-program (scan&parse "let h = 14 in set h = 15"))
    1 (type?)
    (lambda (result)
      (cases type result
        (void-type () #t)
        (else #f))))

  (test type-of-program:begin
    (type-of-program (scan&parse "begin 16; 17; 18 end"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:sum
    (type-of-program (scan&parse "+(19,20)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:product
    (type-of-program (scan&parse "*(21,22)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:cons
    (type-of-program (scan&parse "cons(23, emptylist_int)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (list-type (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f))))

  (test type-of-program:car
    (type-of-program (scan&parse "car(cons(24, emptylist_int))"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:cdr
    (type-of-program (scan&parse "cdr(cons(25, emptylist_int))"))
    1 (type?)
    (lambda (result)
      (cases type result
        (list-type (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f))))

  (test type-of-program:null?
    (type-of-program (scan&parse "null?(emptylist_listof int)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:emptylist
    (type-of-program (scan&parse "emptylist_int"))
    1 (type?)
    (lambda (result)
      (cases type result
        (list-type (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f))))

  (test type-of-program:list
    (type-of-program (scan&parse "list(25, 26, 27, 28)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (list-type (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f))))

  (test type-of-program:new-object
    (type-of-program (scan&parse "class i extends object
                                  field int j
                                  method void initialize(k: int)
                                   set j = +(k,1)
                                  new i(29)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (class-type (class-name) (eq? class-name 'i))
        (else #f))))

  (test type-of-program:method-call
    (type-of-program (scan&parse "class l extends object
                                  field int m
                                  method void initialize(n: int)
                                   set m = +(n,1)
                                  method int get-m()
                                   m
                                  send new l(30) get-m()"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:super-call
    (type-of-program (scan&parse "class o extends object
                                  field int p
                                  method void initialize(q: int)
                                   set p = +(q,1)
                                  method int get-p()
                                   p
                                  class r extends o
                                  method int get-super-p()
                                   super get-p()
                                  send new r(31) get-super-p()"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:self
    (type-of-program (scan&parse "class s extends object
                                  field int t
                                  method void initialize(u: int)
                                   set t = +(u,1)
                                  method s get-me()
                                   self
                                  send new s(32) get-me()"))
    1 (type?)
    (lambda (result)
      (cases type result
        (class-type (class-name) (eq? class-name 's))
        (else #f))))

  (test type-of-program:cast
    (type-of-program (scan&parse "interface v
                                   method int w(x: int)
                                  class y extends object implements v
                                   field int z
                                   method void initialize(a: int)
                                    set z = a
                                   method int w(x: int)
                                    +(z,x)
                                   let b = proc (c: v)
                                            if instanceof c y
                                            then send cast c y w(33)
                                            else 34
                                       d = new y(35)
                                   in (b d)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test type-of-program:instanceof
    (type-of-program (scan&parse "class e extends object
                                   field int f
                                   method void initialize(g: int)
                                    set f = g
                                  class h extends e
                                  let i = new h(36)
                                  in instanceof i e"))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test type-of-program:page-354
    (type-of-program
      (scan&parse "interface tree
                    method int sum()
                    method bool equal (t : tree)

                   class interior-node extends object implements tree
                    field tree left
                    field tree right
                    method void initialize(l : tree, r : tree)
                     begin
                      set left = l; set right = r
                     end
                    method tree getleft () left
                    method tree getright () right
                    method int sum () +(send left sum(), send right sum())
                    method bool equal (t : tree)
                     if instanceof t interior-node
                     then if send left equal(send
                                              cast t interior-node
                                              getleft())
                          then send right equal(send
                                                 cast t interior-node
                                                 getright())
                          else zero?(1)
                      else zero?(1)

                   class leaf-node extends object implements tree
                    field int value
                    method void initialize (v : int) set value = v
                    method int sum () value
                    method int getvalue () value
                    method bool equal (t : tree)
                     if instanceof t leaf-node
                     then zero?(-(value, send cast t leaf-node getvalue()))
                     else zero?(1)

                   let o1 = new interior-node(
                             new interior-node(
                              new leaf-node(3),
                              new leaf-node(4)),
                            new leaf-node(5))
                   in list(send o1 sum(),
                           if send o1 equal(o1) then 100 else 200)"))
    1 (type?)
    (lambda (result)
      (cases type result
        (list-type (base)
          (cases type base
            (int-type () #t)
            (else #f)))
        (else #f)))))


(suite type-to-external-form ()

  (test type-to-external-form:int
    (type-to-external-form (int-type))
    1 (string?)
    (match string=? "int"))

  (test type-to-external-form:bool
    (type-to-external-form (bool-type))
    1 (string?)
    (match string=? "bool"))

  (test type-to-external-form:proc
    (type-to-external-form (proc-type (list (proc-type (list (int-type)) (bool-type)))
                                      (int-type)))
    1 (string?)
    (match string=? "((int -> bool) -> int)"))

  (test type-to-external-form:void
    (type-to-external-form (void-type))
    1 (string?)
    (match string=? "void"))

  (test type-to-external-form:class
    (type-to-external-form (class-type 'j))
    1 (string?)
    (match string=? "j"))

  (test type-to-external-form:list
    (type-to-external-form (list-type (proc-type (list (int-type) (bool-type))
                                                 (int-type))))
    1 (string?)
    (match string=? "listof (int * bool -> int)")))


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

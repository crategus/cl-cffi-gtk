;;; ----------------------------------------------------------------------------
;;; rtest-glib.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(asdf:operate 'asdf:load-op :lisp-unit)
(asdf:operate 'asdf:load-op :bordeaux-threads)
(asdf:operate 'asdf:load-op :cl-cffi-gtk-glib)

(defpackage :glib-tests
  (:use :glib :cffi :common-lisp :lisp-unit))

(in-package :glib-tests)

;;; ----------------------------------------------------------------------------

(define-test glib-stable-pointer
  (let* ((func (lambda() 88))
         (ptr (allocate-stable-pointer func)))
    (assert-eql 88 (funcall (get-stable-pointer-value ptr)))
    (assert-false (free-stable-pointer ptr))
    (assert-false (get-stable-pointer-value ptr)))
  (let ((func (lambda () 99)))
    (with-stable-pointer (ptr func)
      (assert-eql 99 (funcall (get-stable-pointer-value ptr))))))

;;; ----------------------------------------------------------------------------

(define-test glib-misc
  (assert-eq :unsigned-long (cffi::canonicalize-foreign-type 'g-size))
  (assert-eq :long (cffi::canonicalize-foreign-type 'g-ssize))
  (assert-eq :UNSIGNED-LONG-LONG (cffi::canonicalize-foreign-type 'g-offset)))

;;; ----------------------------------------------------------------------------

(define-test glib-version
  (assert-equal    2 *glib-major-version*)
  (assert-equal   32 *glib-minor-version*)
  (assert-equal    3 *glib-micro-version*)
  (assert-equal 3203 *glib-binary-age*)
  (assert-equal    3 *glib-interface-age*)
  (assert-false (glib-check-version 2 24 0))
  (assert-equal "GLib version too old (micro mismatch)"
                (glib-check-version 2 34 0)))

;;; ----------------------------------------------------------------------------

(define-test glib-threads
  (let* ((func (lambda () (sleep 5)))
         (start (get-universal-time))
         (thread (g-thread-new "GTK+" func)))
  (assert-true (pointerp (g-thread-self)))
  (assert-true (pointerp (g-thread-join thread)))
  (assert-eql 5 (- (get-universal-time) start))))

;;; ----------------------------------------------------------------------------

(defvar *started* nil)

(define-test glib-utils
  (when (not *started*)
    (setq *started* t)
    (g-set-application-name "GTK Application")
    (g-set-prgname "GTK Program"))
  (assert-equal "GTK Application" (g-get-application-name))
  (assert-equal "GTK Program" (g-get-prgname)))

;;; ----------------------------------------------------------------------------

#+nil
(define-test glib-main-loop
  (let* ((loop (g-main-loop-new (null-pointer) nil))
         (func (lambda () (g-main-loop-run loop)))
         (context (g-main-context-default)))
    (g-thread-new "GTK+ thread" func)
    (sleep 1)
    (assert-true (g-main-loop-is-running loop))
    (assert-true (pointer-eq context (g-main-loop-get-context loop)))
    (assert-false (g-main-context-pending context))
    (assert-true (null-pointer-p (g-main-current-source)))
;    (let* ((source (g-timeout-source-new 1000)))
;      (g-source-attach source context)
;      (assert-true (pointer-eq context (g-source-get-context source))))
    (g-main-loop-quit loop)
    (sleep 1)
    (assert-false (g-main-loop-is-running loop))))

;;; --- End of file rtest-glib.lisp --------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.init.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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

(in-package :gtk)

(defun finalize-subclasses (class)
  (c2mop:ensure-finalized class)
  (iter (for subclass in (c2mop:class-direct-subclasses class))
        (finalize-subclasses subclass)))

(defun finalize-gtk-classes ()
  (finalize-subclasses (find-class 'gobject:g-object)))

(finalize-gtk-classes)

;;; ----------------------------------------------------------------------------

(at-init () (gtk-init))

#+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* nil)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

  (at-finalize ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread*
              (bt:make-thread (lambda () (gtk-main))
                              :name "cl-cffi-gtk main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values *main-thread* *main-thread-level*))

  (defun join-gtk-main ()
    (when *main-thread*
      (bt:join-thread *main-thread*)))

  (defun leave-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        (gtk-main-quit)))
    (values *main-thread* *main-thread-level*)))

#-thread-support
(progn
  (defun ensure-gtk-main ()
    (gtk-main)
    (values))

  (defun leave-gtk-main ()
    (gtk-main-quit))

  (defun join-gtk-main ()))

(export 'ensure-gtk-main)

(export 'leave-gtk-main)

(export 'join-gtk-main)

;;; --- End of file gtk.init.lisp ----------------------------------------------

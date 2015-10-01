;;; ----------------------------------------------------------------------------
;;; gtk.init.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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

#+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* 0)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

  (glib::at-finalize ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread*
              (bt:make-thread (lambda ()
                                ;; On Windows it is necessary to use the
                                ;; version gtk-main which puts the C function
                                ;; %gtk-main between gdk-thread-enter und
                                ;; gdk-thread-leave
                                (unless (or (find :os-windows *features*)
                                            (find :win32 *features*))
                                  (gdk-threads-init)   ;; Calling on win32 will deadlock
                                  (gdk-threads-enter)) ;; Calling on win32 will deadlock
                                (unwind-protect
                                    (progn
;                                      (%gtk-init)
                                      (%gtk-main))
                                  (unless (or (find :os-windows *features*)
                                              (find :win32 *features*))
                                    (gdk-threads-leave)) ;; Calling on win32 will deadlock
                                  ))
                              :name "cl-cffi-gtk main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values *main-thread* *main-thread-level*))

  (defun join-gtk-main ()
   #+cl-cffi-gtk-documentation
   "@version{2013-8-20}
    Wait until the GTK+ program terminates."
    (when *main-thread*
      (bt:join-thread *main-thread*)))

  (defun leave-gtk-main ()
   #+cl-cffi-gtk-documentation
   "@version{2013-7-30}
    @begin{short}
      Makes the innermost invocation of the main loop return when it regains
      control.
    @end{short}

    In the Lisp binding to GTK+ the function @fun{gtk-main-quit} is not called,
    but the function @sym{leave-gtk-main}. The function @sym{leave-gtk-main}
    does some additional bookkeeping, which is necessary to stop a Lisp program
    safely.
    @see-function{gtk-main-quit}"
    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (<= *main-thread-level* 0)
        (gtk-main-quit)
        (setf *main-thread-level* 0)))
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

;;; ----------------------------------------------------------------------------

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (glib::get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (func &key (priority +g-priority-default-idle+))
  (glib::%g-idle-add-full priority
                          (callback call-from-main-loop-callback)
                          (glib:allocate-stable-pointer func)
                          (callback glib:stable-pointer-destroy-notify-cb))
  (ensure-gtk-main))

(defmacro within-main-loop (&body body)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  The macro @sym{within-main-loop} is a wrapper around a GTK+ program. The
  functionality of the macro corresponds to the C functions @code{gtk_init()}
  and @code{gtk_main()} which initialize and start a GTK+ program. Both
  functions have corresponding Lisp functions. The function
  @code{gtk_main()} is exported as the Lisp function @fun{gtk-main}. The
  corresponding Lisp function to @code{gtk_init()} is called internally when
  loading the library, but is not exported.
  @see-function{gtk-main}"
  `(call-from-gtk-main-loop (lambda () ,@body)))

(export 'within-main-loop)

#+thread-support
(defmacro with-main-loop (&body body)
  `(progn
     (ensure-gtk-main)
     (within-main-loop ,@body)))

#-thread-support
(defmacro with-main-loop (&body body)
  `(progn
     ,@body
     (ensure-gtk-main)))

;;; ----------------------------------------------------------------------------

(glib::at-init () (%gtk-init))

;;; --- End of file gtk.init.lisp ----------------------------------------------

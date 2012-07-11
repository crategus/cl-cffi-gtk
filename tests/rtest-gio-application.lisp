;;; ----------------------------------------------------------------------------
;;; rtest-gio-application.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(defpackage :gio-tests
  (:use :gio :gobject :glib :cffi :common-lisp :lisp-unit))

(in-package :gio-tests)

;;; ----------------------------------------------------------------------------

(define-test gio-application
  (assert-true  (g-type-is-object "GApplication"))
  (assert-false (g-type-is-abstract "GApplication"))
  (assert-true  (g-type-is-derived "GApplication"))
  (assert-false (g-type-is-fundamental "GApplication"))
  (assert-true  (g-type-is-value-type "GApplication"))
  (assert-true  (g-type-has-value-table "GApplication"))
  (assert-true  (g-type-is-classed "GApplication"))
  (assert-true  (g-type-is-instantiatable "GApplication"))
  (assert-true  (g-type-is-derivable "GApplication"))
  (assert-true  (g-type-is-deep-derivable "GApplication"))
  (assert-false (g-type-is-interface "GApplication"))

  ;; Check the registered name
  (assert-eq 'g-application
             (registered-object-type-by-name "GApplication"))
  
  (let ((class (g-type-class-ref (gtype "GApplication"))))
    (assert-equal (gtype "GApplication")  (g-type-from-class class))
    (assert-equal (gtype "GApplication") (g-object-class-type class))
    (assert-equal "GApplication" (g-object-class-name class))
    (assert-equal (gtype "GApplication")
                  (g-type-from-class (g-type-class-peek "GApplication")))
    (assert-equal (gtype "GApplication")
                  (g-type-from-class (g-type-class-peek-static "GApplication")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'g-application)))
    ;; Check the class name and type of the class
    (assert-eq 'g-application (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GApplication" (gobject-class-g-type-name class))
    (assert-equal "GApplication" (gobject-class-direct-g-type-name class))
    (assert-equal "g_application_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GApplication"))
  (assert-eql 2 (g-type-depth "GApplication"))
  (assert-eql   (gtype "GApplication")
                (g-type-next-base "GApplication" "GObject"))
  (assert-true  (g-type-is-a "GApplication" "GObject"))
  (assert-false (g-type-is-a "GApplication" "gboolean"))
  (assert-false (g-type-is-a "GApplication" "GtkWindow"))
  (assert-equal '("GtkApplication")
                (mapcar #'gtype-name (g-type-children "GApplication")))
  (assert-equal '("GActionGroup" "GActionMap")
                (mapcar #'gtype-name (g-type-interfaces "GApplication")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GApplication" query)
    (assert-equal (gtype "GApplication")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GApplication"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 156 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
       '("application-id" "flags" "is-registered" "is-remote"
         "inactivity-timeout" "action-group")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GApplication"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GApplication" G-APPLICATION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GActionGroup" "GActionMap"))
                               ((ACTION-GROUP G-APPLICATION-ACTION-GROUP
                                 "action-group" "GActionGroup" NIL T)
                                (APPLICATION-ID G-APPLICATION-APPLICATION-ID
                                 "application-id" "gchararray" T T)
                                (FLAGS G-APPLICATION-FLAGS "flags"
                                 "GApplicationFlags" T T)
                                (INACTIVITY-TIMEOUT
                                 G-APPLICATION-INACTIVITY-TIMEOUT
                                 "inactivity-timeout" "guint" T T)
                                (IS-REGISTERED G-APPLICATION-IS-REGISTERED
                                 "is-registered" "gboolean" T NIL)
                                (IS-REMOTE G-APPLICATION-IS-REMOTE "is-remote"
                                 "gboolean" T NIL)))
     (get-g-type-definition (gtype "GApplication")))

  ;; Check functions
  (assert-true (g-application-id-is-valid "org.gtk.TestApplicaton"))
  (let ((app (make-instance 'g-application
                            :application-id "org.gtk.TestApplication"
                            :flags :none
                            :inactivity-timeout 10000)))
    (g-signal-connect app "activate"
       (lambda (application)
         (format t "~&Signal 'activate' called for ~A~%" application)))
    (g-signal-connect app "command-line"
       (lambda (application)
         (format t "~&Signal 'command-line' called for ~A~%" application)))
    (g-signal-connect app "open"
       (lambda (application)
         (format t "~&Signal 'open' called for ~A~%" application)))
    (g-signal-connect app "shutdown"
       (lambda (application)
         (format t "~&Signal 'shutdown' called for ~A~%" application)))
    (g-signal-connect app "startup"
       (lambda (application)
         (format t "~&Signal 'startup' called for ~A~%" application)))

    (g-application-hold app)
    (assert-equal "org.gtk.TestApplication"
                  (g-application-get-application-id app))
    (assert-eql 10000 (g-application-get-inactivity-timeout app))
    (assert-equal '(:none) (g-application-get-flags app))
    (assert-false (g-application-get-is-registered app))
    ;; Register the application
    (assert-true (g-application-register app (null-pointer)))
    ;; Check again if the application is registered
    (assert-true (g-application-get-is-registered app))
    ;; The application must be registered before is-remote can be checked
    (assert-false (g-application-get-is-remote app))
    (g-application-activate app)
    (assert-false (g-application-get-default))
    (g-application-release app)

    (assert-eql 0 (g-application-run app 0 (null-pointer)))
    (g-application-quit app)
    (g-object-unref (pointer app))
   )
)

;;; --- End of file rtest-gio-application.lisp ---------------------------------

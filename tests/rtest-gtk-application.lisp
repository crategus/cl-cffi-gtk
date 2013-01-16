;;; ----------------------------------------------------------------------------
;;; rtest-gtk-application.lisp
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

(in-package :gtk-tests)

;;; ----------------------------------------------------------------------------

(define-test gtk-application
  ;; Type checks
  (assert-true  (g-type-is-object "GtkApplication"))
  (assert-false (g-type-is-abstract "GtkApplication"))
  (assert-true  (g-type-is-derived "GtkApplication"))
  (assert-false (g-type-is-fundamental "GtkApplication"))
  (assert-true  (g-type-is-value-type "GtkApplication"))
  (assert-true  (g-type-has-value-table "GtkApplication"))
  (assert-true  (g-type-is-classed "GtkApplication"))
  (assert-true  (g-type-is-instantiatable "GtkApplication"))
  (assert-true  (g-type-is-derivable "GtkApplication"))
  (assert-true  (g-type-is-deep-derivable "GtkApplication"))
  (assert-false (g-type-is-interface "GtkApplication"))

  ;; Check the registered name
  (assert-eq 'gtk-application
             (registered-object-type-by-name "GtkApplication"))
  
  ;; Check infos about the class
  (let ((class (g-type-class-ref (gtype "GtkApplication"))))
    (assert-equal (gtype "GtkApplication")  (g-type-from-class class))
    (assert-equal (gtype "GtkApplication") (g-object-class-type class))
    (assert-equal "GtkApplication" (g-object-class-name class))
    (assert-equal (gtype "GtkApplication")
                  (g-type-from-class (g-type-class-peek "GtkApplication")))
    (assert-equal (gtype "GtkApplication")
                  (g-type-from-class (g-type-class-peek-static "GtkApplication")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-application)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-application (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkApplication" (gobject-class-g-type-name class))
    (assert-equal "GtkApplication" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_application_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GApplication") (g-type-parent "GtkApplication"))
  (assert-eql 3 (g-type-depth "GtkApplication"))
  (assert-eql   (gtype "GApplication")
                (g-type-next-base "GtkApplication" "GObject"))
  (assert-true  (g-type-is-a "GtkApplication" "GObject"))
  (assert-false (g-type-is-a "GtkApplication" "gboolean"))
  (assert-false (g-type-is-a "GtkApplication" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkApplication")))
  (assert-equal '("GActionGroup" "GActionMap")
                (mapcar #'gtype-name (g-type-interfaces "GtkApplication")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkApplication" query)
    (assert-equal (gtype "GtkApplication")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkApplication"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 212 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  20 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
       '("application-id" "flags" "is-registered" "is-remote"
         "inactivity-timeout" "action-group" "register-session" "app-menu"
         "menubar" "active-window")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkApplication"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkApplication" GTK-APPLICATION
                               (:SUPERCLASS G-APPLICATION :EXPORT T :INTERFACES
                                ("GActionGroup" "GActionMap") :TYPE-INITIALIZER
                                "gtk_application_get_type")
                               ((ACTIVE-WINDOW GTK-APPLICATION-ACTIVE-WINDOW
                                 "active-window" "GtkWindow" T NIL)
                                (APP-MENU GTK-APPLICATION-APP-MENU "app-menu"
                                 "GMenuModel" T T)
                                (MENUBAR GTK-APPLICATION-MENUBAR "menubar"
                                 "GMenuModel" T T)
                                (REGISTER-SESSION
                                 GTK-APPLICATION-REGISTER-SESSION
                                 "register-session" "gboolean" T T)))
     (get-g-type-definition (gtype "GtkApplication")))

  ;; Create an instance
  (assert-true (g-application-id-is-valid "org.gtk.TestApplicaton"))
  (let ((app (make-instance 'gtk-application
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


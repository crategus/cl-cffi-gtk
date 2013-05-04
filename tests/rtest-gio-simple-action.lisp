;;; ----------------------------------------------------------------------------
;;; rtest-gio-simple-action.lisp
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

(define-test gio-simple-action
  ;; Type checks
  (assert-true  (g-type-is-object "GSimpleAction"))
  (assert-false (g-type-is-abstract "GSimpleAction"))
  (assert-true  (g-type-is-derived "GSimpleAction"))
  (assert-false (g-type-is-fundamental "GSimpleAction"))
  (assert-true  (g-type-is-value-type "GSimpleAction"))
  (assert-true  (g-type-has-value-table "GSimpleAction"))
  (assert-true  (g-type-is-classed "GSimpleAction"))
  (assert-true  (g-type-is-instantiatable "GSimpleAction"))
  (assert-true  (g-type-is-derivable "GSimpleAction"))
  (assert-true  (g-type-is-deep-derivable "GSimpleAction"))
  (assert-false (g-type-is-interface "GSimpleAction"))

  ;; Check the registered name
  (assert-eq 'g-simple-action
             (registered-object-type-by-name "GSimpleAction"))
  
  ;; Check infos about the class
  (let ((class (g-type-class-ref (gtype "GSimpleAction"))))
    (assert-equal (gtype "GSimpleAction")  (g-type-from-class class))
    (assert-equal (gtype "GSimpleAction") (g-object-class-type class))
    (assert-equal "GSimpleAction" (g-object-class-name class))
    (assert-equal (gtype "GSimpleAction")
                  (g-type-from-class (g-type-class-peek "GSimpleAction")))
    (assert-equal (gtype "GSimpleAction")
                  (g-type-from-class (g-type-class-peek-static "GSimpleAction")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'g-simple-action)))
    ;; Check the class name and type of the class
    (assert-eq 'g-simple-action (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GSimpleAction" (gobject-class-g-type-name class))
    (assert-equal "GSimpleAction" (gobject-class-direct-g-type-name class))
    (assert-equal "g_simple_action_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GObject") (g-type-parent "GSimpleAction"))
  (assert-eql 2 (g-type-depth "GSimpleAction"))
  (assert-eql   (gtype "GSimpleAction")
                (g-type-next-base "GSimpleAction" "GObject"))
  (assert-true  (g-type-is-a "GSimpleAction" "GObject"))
  (assert-false (g-type-is-a "GSimpleAction" "gboolean"))
  (assert-false (g-type-is-a "GSimpleAction" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GSimpleAction")))
  (assert-equal '("GAction")
                (mapcar #'gtype-name (g-type-interfaces "GSimpleAction")))
  
  ;; Query infos about the class
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GSimpleAction" query)
    (assert-equal (gtype "GSimpleAction")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GSimpleAction"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql  68 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  28 (foreign-slot-value query 'g-type-query :instance-size)))
  
  ;; Get the class properties
  (assert-equal
       '("name" "parameter-type" "enabled" "state-type" "state")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GSimpleAction"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GSimpleAction" G-SIMPLE-ACTION
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GAction"))
                               ((ENABLED G-SIMPLE-ACTION-ENABLED "enabled"
                                 "gboolean" T T)
                                (NAME G-SIMPLE-ACTION-NAME "name" "gchararray"
                                 T NIL)
                                (PARAMETER-TYPE G-SIMPLE-ACTION-PARAMETER-TYPE
                                 "parameter-type" "GVariantType" T NIL)
                                (STATE G-SIMPLE-ACTION-STATE "state" "GVariant"
                                 T T)
                                (STATE-TYPE G-SIMPLE-ACTION-STATE-TYPE
                                 "state-type" "GVariantType" T NIL)))
     (get-g-type-definition (gtype "GSimpleAction")))
#|
  ;; Create an instance
  (let* ((action (make-instance 'g-simple-action
                                :name "simple"
                                :parameter-type (g-variant-type-new "b")
                                :state (g-variant-new-boolean t)))
         (action-ptr (pointer action)))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (format t "~&GSimpleAction: signal 'activate' occured.~%")
         (format t "~&    action    : ~A~%" action)
         (format t "~&    name      : ~A~%" (g-action-get-name action))
         (format t "~&    parameter : ~A~%" parameter)))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (g-simple-action-set-state action value)
         (format t "~&GSimpleAction: signal 'change-state' occured.~%")
         (format t "~&    action : ~A~%" action)
         (format t "~&    name   : ~A~%" (g-action-get-name action))
         (format t "~&    value  : ~A~%" value)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GSimpleAction") (g-object-type action))
    (assert-equal "GSimpleAction" (g-object-type-name action))
    (assert-true  (g-type-is-a "GSimpleAction" (g-type-from-instance action-ptr)))
    ;; Access the properties through the interface GAction
    (assert-true  (g-action-get-enabled action))
    (assert-equal "simple" (g-action-get-name action))
    (assert-eq    'g-variant-type (type-of (g-action-get-parameter-type action)))
    (assert-true  (g-variant-get-boolean (g-action-get-state action)))
    (assert-equal "b" (g-variant-get-type-string (g-action-get-state action)))
    (assert-eq    'g-variant-type (type-of (g-action-get-state-type action)))
    ;; Change the state
    (g-action-change-state action (g-variant-new-boolean nil))
    (assert-false (g-variant-get-boolean (g-action-get-state action)))
    ;; Activate the action
    (g-action-activate action (g-variant-new-boolean nil)))

  ;; Create a stateless action with g-simple-action-new
  (let* ((action (g-simple-action-new "second simple" (g-variant-type-new "b")))
         (action-ptr (pointer action)))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (format t "~&GSimpleAction: signal 'activate' occured.~%")
         (format t "~&    action    : ~A~%" action)
         (format t "~&    name      : ~A~%" (g-action-get-name action))
         (format t "~&    parameter : ~A~%" parameter)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GSimpleAction") (g-object-type action))
    (assert-equal "GSimpleAction" (g-object-type-name action))
    (assert-true  (g-type-is-a "GSimpleAction" (g-type-from-instance action-ptr)))
    ;; Access the properties through the interface GAction
    (assert-true  (g-action-get-enabled action))
    (assert-equal "second simple" (g-action-get-name action))
    (assert-eq    'g-variant-type (type-of (g-action-get-parameter-type action)))
    ;; Activate the action
    (g-action-activate action (g-variant-new-boolean nil)))

  ;; Create a stateful action with g-simple-action-new-stateful
  (let* ((action (g-simple-action-new-stateful "third simple"
                                               (g-variant-type-new "b")
                                               (g-variant-new-boolean t)))
         (action-ptr (pointer action)))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (format t "~&GSimpleAction: signal 'activate' occured.~%")
         (format t "~&    action    : ~A~%" action)
         (format t "~&    name      : ~A~%" (g-action-get-name action))
         (format t "~&    parameter : ~A~%" parameter)))
    (g-signal-connect action "change-state"
       (lambda (action value)
         (g-simple-action-set-state action value)
         (format t "~&GSimpleAction: signal 'change-state' occured.~%")
         (format t "~&    action : ~A~%" action)
         (format t "~&    name   : ~A~%" (g-action-get-name action))
         (format t "~&    value  : ~A~%" value)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GSimpleAction") (g-object-type action))
    (assert-equal "GSimpleAction" (g-object-type-name action))
    (assert-true  (g-type-is-a "GSimpleAction" (g-type-from-instance action-ptr)))
    ;; Access the properties through the interface GAction
    (assert-true  (g-action-get-enabled action))
    (assert-equal "third simple" (g-action-get-name action))
    (assert-eq    'g-variant-type (type-of (g-action-get-parameter-type action)))
    (assert-true  (g-variant-get-boolean (g-action-get-state action)))
    (assert-equal "b" (g-variant-get-type-string (g-action-get-state action)))
    (assert-eq    'g-variant-type (type-of (g-action-get-state-type action)))
    ;; Change the state
    (g-action-change-state action (g-variant-new-boolean nil))
    (assert-false (g-variant-get-boolean (g-action-get-state action)))
    ;; Activate the action
    (g-action-activate action (g-variant-new-boolean nil)))

  ;; Create a stateless action with g-simple-action-new and no parameter-type
  (let* ((action (g-simple-action-new "fourth simple" nil))
         (action-ptr (pointer action)))
    ;; Connect available signals
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (format t "~&GSimpleAction: signal 'activate' occured.~%")
         (format t "~&    action    : ~A~%" action)
         (format t "~&    name      : ~A~%" (g-action-get-name action))
         (format t "~&    parameter : ~A~%" parameter)))
    ;; Some general checks of the instance
    (assert-equal (gtype "GSimpleAction") (g-object-type action))
    (assert-equal "GSimpleAction" (g-object-type-name action))
    (assert-true  (g-type-is-a "GSimpleAction" (g-type-from-instance action-ptr)))
    ;; Access the properties through the interface GAction
    (assert-true  (g-action-get-enabled action))
    (assert-equal "fourth simple" (g-action-get-name action))
    ;; Activate the action
    (g-action-activate action (null-pointer)))
|#
)


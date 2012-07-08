;;; ----------------------------------------------------------------------------
;;; rtest-gio-action.lisp
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

(define-test gio-action
  ;; Type checks
  (assert-false (g-type-is-object "GAction"))
  (assert-false (g-type-is-abstract "GAction"))
  (assert-true  (g-type-is-derived "GAction"))
  (assert-false (g-type-is-fundamental "GAction"))
  (assert-true  (g-type-is-value-type "GAction"))
  (assert-true  (g-type-has-value-table "GAction"))
  (assert-false (g-type-is-classed "GAction"))
  (assert-false (g-type-is-instantiatable "GAction"))
  (assert-true  (g-type-is-derivable "GAction"))
  (assert-false (g-type-is-deep-derivable "GAction"))
  (assert-true  (g-type-is-interface "GAction"))

  ;; Check the registered name
  (assert-eq 'g-action
             (registered-object-type-by-name "GAction"))
  
  ;; Check infos about the interface
  (let ((class (find-class 'g-action)))
    ;; Check the interface name and type of the interface
    (assert-eq 'g-action (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GAction" (gobject-class-g-type-name class))
    (assert-equal "GAction" (gobject-class-direct-g-type-name class))
    (assert-equal "g_action_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-true  (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GInterface") (g-type-parent "GAction"))
  (assert-eql 2 (g-type-depth "GAction"))
  (assert-eql   nil
                (g-type-next-base "GAction" "GObject"))
  (assert-true  (g-type-is-a "GAction" "GObject"))
  (assert-false (g-type-is-a "GAction" "gboolean"))
  (assert-false (g-type-is-a "GAction" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GAction")))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-interfaces "GAction")))
  
  ;; Get the names of the interface properties
  (assert-equal
       '("enabled" "name" "parameter-type" "state" "state-type")
     (mapcar #'param-spec-name
             (g-object-interface-list-properties (gtype "GAction"))))

  ;; Get the interface definition
  (assert-equal
     '(DEFINE-G-INTERFACE "GAction"
            G-ACTION
            (:EXPORT T)
          (ENABLED G-ACTION-ENABLED "enabled" "gboolean" T NIL)
          (NAME G-ACTION-NAME "name" "gchararray" T NIL)
          (PARAMETER-TYPE G-ACTION-PARAMETER-TYPE "parameter-type"
           "GVariantType" T NIL)
          (STATE G-ACTION-STATE "state" "GVariant" T NIL)
          (STATE-TYPE G-ACTION-STATE-TYPE "state-type" "GVariantType" T NIL))
     (get-g-type-definition (gtype "GAction")))
)


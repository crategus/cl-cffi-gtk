;;; ----------------------------------------------------------------------------
;;; rtest-gdk-screen.lisp
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

(in-package :gdk-tests)

(define-test gdk-screen
  (let* ((screen (gdk-screen-get-default))
         (type (g-type-from-instance (pointer screen)))
         (n (gdk-screen-get-primary-monitor screen)))
    ;; The following tests are valid only for a specific display.
    ;; The tests should be cut out or the values must be replaced.
    (assert-equal "GdkX11Screen" (gtype-name type))
    (assert-eql 'gdk-screen (registered-object-type-by-name "GdkScreen"))
    (assert-equal "GdkScreen" (gtype-name (g-type-parent type)))
    (assert-equal '()
                  (mapcar #'gtype-name (g-type-children type)))
    (assert-true (gdk-screen-font-options screen))
    (assert-eql 96.0d0 (gdk-screen-resolution screen))
    (let ((rect (gdk-screen-get-monitor-geometry screen n)))
      (assert-eql 0 (gdk-rectangle-x rect))
      (assert-eql 0 (gdk-rectangle-y rect))
      (assert-eql (%gdk-screen-width)  (gdk-rectangle-width rect))
      (assert-eql (%gdk-screen-height) (gdk-rectangle-height rect)))
    (assert-eql 331 (gdk-screen-get-monitor-width-mm screen n))
    (assert-eql 207 (gdk-screen-get-monitor-height-mm screen n))
    (assert-equal "LVDS1" (gdk-screen-get-monitor-plug-name screen n))
    
    ;; These are general tests of the class GdkScreen.
    (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GdkScreen" GDK-SCREEN
        (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
          :TYPE-INITIALIZER "gdk_screen_get_type")
        ((FONT-OPTIONS GDK-SCREEN-FONT-OPTIONS "font-options" "gpointer" T T)
         (RESOLUTION GDK-SCREEN-RESOLUTION "resolution" "gdouble" T T)))
     (gobject::get-g-class-definition (gtype "GdkScreen")))
    
    (assert-equal
     '(PROGN
        (DEFCLASS GDK-SCREEN NIL
          ((FONT-OPTIONS :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gpointer" :ACCESSOR GDK-SCREEN-FONT-OPTIONS :INITARG
                         :FONT-OPTIONS :G-PROPERTY-NAME "font-options")
           (RESOLUTION :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
             "gdouble" :ACCESSOR GDK-SCREEN-RESOLUTION :INITARG :RESOLUTION
                       :G-PROPERTY-NAME "resolution"))
          (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GdkScreen")
          (:G-TYPE-INITIALIZER . "gdk_screen_get_type"))
        (EXPORT 'GDK-SCREEN (FIND-PACKAGE "GDK"))
        (EXPORT 'GDK-SCREEN-FONT-OPTIONS (FIND-PACKAGE "GDK"))
        (EXPORT 'GDK-SCREEN-RESOLUTION (FIND-PACKAGE "GDK")))
     (macroexpand-1 (gobject::get-g-class-definition (gtype "GdkScreen"))))))

;;; --- End of file rtest-gdk-screen.lisp --------------------------------------

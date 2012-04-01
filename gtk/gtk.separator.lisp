;;; ----------------------------------------------------------------------------
;;; gtk.separator.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;;
;;; GtkSeparator
;;; 
;;; A separator widget
;;; 
;;; Synopsis
;;; 
;;;     GtkSeparator
;;;
;;;     gtk_separator_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkSeparator
;;;                      +----GtkHSeparator
;;;                      +----GtkVSeparator
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSeparator implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;; 
;;; GtkSeparator is a horizontal or vertical separator widget, depending on the
;;; value of the "orientation" property, used to group the widgets within a
;;; window. It displays a line with a shadow to make it appear sunken into the
;;; interface.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparator
;;; 
;;; struct GtkSeparator;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkSeparator" 'gtk-separator))

(define-g-object-class "GtkSeparator" gtk-separator
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_separator_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_separator_new ()
;;; 
;;; GtkWidget * gtk_separator_new (GtkOrientation orientation);
;;; 
;;; Creates a new GtkSeparator with the given orientation.
;;; 
;;; orientation :
;;;     the separator's orientation.
;;; 
;;; Returns :
;;;     a new GtkSeparator
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; TODO: Implement gtk_separator_new

;;; ---------------------------------------------------------------------------- 
;;; GtkHSeparator
;;; 
;;; GtkHSeparator — A horizontal separator
;;;     
;;; Synopsis
;;; 
;;;     GtkHSeparator;
;;;     gtk_hseparator_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkSeparator
;;;                      +----GtkHSeparator
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHSeparator implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Description
;;; 
;;; The GtkHSeparator widget is a horizontal separator, used to group the
;;; widgets within a window. It displays a horizontal line with a shadow to
;;; make it appear sunken into the interface.
;;; 
;;; Note
;;;
;;; The GtkHSeparator widget is not used as a separator within menus. To create
;;; a separator in a menu create an empty GtkSeparatorMenuItem widget using
;;; gtk_separator_menu_item_new() and add it to the menu with
;;; gtk_menu_shell_append().
;;; 
;;; GtkHSeparator has been deprecated, use GtkSeparator instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHSeparator
;;; 
;;; struct GtkHSeparator;
;;; 
;;; Warning
;;; 
;;; GtkHSeparator is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHSeparator" 'gtk-hseparator)
  (setf *lisp-name-exceptions*
        (append '(("GtkVSeparator" GTK-HSEPARATOR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHSeparator" gtk-hseparator
  (:superclass gtk-separator
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hseparator_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hseparator_new ()
;;; 
;;; GtkWidget * gtk_hseparator_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_hseparator_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_separator_new() with
;;; GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new GtkHSeparator.
;;; 
;;; Returns :
;;;     a new GtkHSeparator.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hseparator-new))

(defun gtk-hseparator-new ()
  (make-instance 'gtk-hseprator))

(export 'gtk-hseparator)

;;; ----------------------------------------------------------------------------
;;; GtkVSeparator
;;; 
;;; A vertical separator
;;;     
;;; Synopsis
;;; 
;;;     GtkVSeparator;
;;;     gtk_vseparator_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkSeparator
;;;                      +----GtkVSeparator
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVSeparator implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;; Description
;;; 
;;; The GtkVSeparator widget is a vertical separator, used to group the widgets
;;; within a window. It displays a vertical line with a shadow to make it
;;; appear sunken into the interface.
;;; 
;;; GtkVSeparator has been deprecated, use GtkSeparator instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVSeparator
;;; 
;;; struct GtkVSeparator;
;;; 
;;; Warning
;;; 
;;; GtkVSeparator is deprecated and should not be used in newly-written code.
;;; 
;;; The GtkVSeparator struct contains private data only, and should be accessed
;;; using the functions below.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVSeparator" 'gtk-vseparator)
  (setf *lisp-name-exceptions*
        (append '(("GtkVSeparator" GTK-VSEPARATOR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVSeparator" gtk-vseparator
  (:superclass gtk-separator
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vseparator_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_vseparator_new ()
;;; 
;;; GtkWidget * gtk_vseparator_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_vseparator_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_separator_new() with
;;; GTK_ORIENTATION_VERTICAL instead
;;; 
;;; Creates a new GtkVSeparator.
;;; 
;;; Returns :
;;;     a new GtkVSeparator.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vseparator-new))

(defun gtk-vseparator-new ()
  (make-instance 'gtk-vseparator))

(export 'gtk-vseparator)

;;; --- End of file gtk.separator.lisp -----------------------------------------

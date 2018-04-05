;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser-widget.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; GtkRecentChooserWidget
;;;
;;; Displays recently used files
;;;
;;; Synopsis
;;;
;;;     GtkRecentChooserWidget
;;;
;;;     gtk_recent_chooser_widget_new
;;;     gtk_recent_chooser_widget_new_for_manager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserWidget
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkRecentChooserWidget" 'gtk-recent-chooser-widget))

(define-g-object-class "GtkRecentChooserWidget" gtk-recent-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_chooser_widget_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-chooser-widget 'type)
 "@version{2013-5-28}
  @begin{short}
    @sym{gtk-recent-chooser-widget} is a widget suitable for selecting recently
    used files. It is the main building block of a
    @class{gtk-recent-chooser-dialog}. Most applications will only need to use
    the latter; you can use @sym{gtk-recent-chooser-widget} as part of a larger
    window if you have special needs.
  @end{short}

  Note that @sym{gtk-recent-chooser-widget} does not have any methods of its
  own. Instead, you should use the functions that work on a
  @class{gtk-recent-chooser}.

  Recently used files are supported since GTK+ 2.10.")

;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation of the child properties.
;;       GtkRecentChooserWidget has no documented child properties.

#|
(define-child-property "GtkRecentChooserWidget"
                       gtk-recent-chooser-widget-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkRecentChooserWidget"
                       gtk-recent-chooser-widget-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkRecentChooserWidget"
                       gtk-recent-chooser-widget-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkRecentChooserWidget"
                       gtk-recent-chooser-widget-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkRecentChooserWidget"
                       gtk-recent-chooser-widget-child-position
                       "position" "gint" t t t)
|#

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-widget-new))

(defun gtk-recent-chooser-widget-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @return{A new @class{gtk-recent-chooser-widget}.}
  @begin{short}
    Creates a new @class{gtk-recent-chooser-widget} object.
  @end{short}
  This is an embeddable widget used to access the recently used resources list.

  Since 2.10
  @see-class{gtk-recent-chooser-widget}
  @see-function{gtk-recent-chooser-widget-new-for-manager}"
  (make-instance 'gtk-recent-chooser))

(export 'gtk-recent-chooser-widget-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_widget_new_for_manager ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-widget-new-for-manager))

(defun gtk-recent-chooser-widget-new-for-manager (manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @return{A new @class{gtk-recent-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk-recent-chooser-widget} widget with a specified
    recent manager.
  @end{short}

  This is useful if you have implemented your own recent manager, or if you
  have a customized instance of a @class{gtk-recent-manager} object.

  Since 2.10
  @see-class{gtk-recent-chooser-widget}
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-chooser-widget-new}"
  (make-instance 'gtk-recent-chooser-widget
                 :recent-manager manager))

(export 'gtk-recent-chooser-widget-new-for-manager)

;;; --- End of file gtk.recent-chooser-widget.lisp -----------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;; GtkFontChooserWidget
;;;
;;;     A widget for selecting fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_widget_new
;;;
;;; Properties
;;;
;;;     GAction*  tweak-action    Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFontChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFontChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkFontChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserWidget
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserWidget" gtk-font-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_widget_get_type")
  ((tweak-action
    gtk-font-chooser-widget-tweak-action
    "tweak-action" "GAction" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-chooser-widget 'type)
 "@version{2013-6-18}
  @begin{short}
    The @sym{gtk-font-chooser-widget} widget lists the available fonts, styles
    and sizes, allowing the user to select a font.
  @end{short}
  It is used in the @class{gtk-font-chooser-dialog} widget to provide a dialog
  box for selecting fonts.

  To set or to get the font which is initially selected, use the
  @fun{gtk-font-chooser-font} or @fun{gtk-font-chooser-font-desc}
  slot access functions.

  To change the text which is shown in the preview area, use the
  @fun{gtk-font-chooser-set-preview-text} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-font-chooser-widget} class has a single CSS node with name
    @code{fontchooser}.
  @end{dictionary}
  @see-class{gtk-font-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-chooser-widget-tweak-action -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tweak-action"
                                               'gtk-font-chooser-widget) 't)
 "The @code{tweak-action} property of type @class{g-action} (Read) @br{}
  A toggle action that can be used to switch to the tweak page of the font
  chooser widget, which lets the user tweak the OpenType features and variation
  axes of the selected font.
  The action will be enabled or disabled depending on whether the selected font
  has any features or axes.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-chooser-widget-tweak-action
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-chooser-widget-tweak-action 'function)
 "@version{2019-5-6}
  @begin{short}
    Accessor of the slot @slot[gtk-font-chooser-widget]{tweak-action} of the
    @class{gtk-font-chooser-widget} class.
  @end{short}
  @see-class{gtk-font-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-chooser-widget-new))

(defun gtk-font-chooser-widget-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @return{A new @class{gtk-font-chooser-widget} widget.}
  @short{Creates a new @class{gtk-font-chooser-widget} widget.}
  @see-class{gtk-font-chooser-widget}"
  (make-instance 'gtk-font-chooser-widget))

(export 'gtk-font-chooser-widget-new)

;;; --- End of file gtk.font-chooser-widget.lisp -------------------------------

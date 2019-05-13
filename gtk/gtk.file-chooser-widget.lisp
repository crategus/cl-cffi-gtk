;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkFileChooserWidget
;;;
;;;     File chooser widget that can be embedded in other widgets
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_widget_new
;;;
;;; Properties
;;;
;;;     gboolean   search-mode                Read / Write
;;;        gchar*  subtitle                   Read
;;;
;;; Signals
;;;
;;;         void   desktop-folder             Action
;;;         void   down-folder                Action
;;;         void   home-folder                Action
;;;         void   location-popup             Action
;;;         void   location-popup-on-paste    Action
;;;         void   location-toggle-popup      Action
;;;         void   places-shortcut            Action
;;;         void   quick-bookmark             Action
;;;         void   recent-shortcut            Action
;;;         void   search-shortcut            Action
;;;         void   show-hidden                Action
;;;         void   up-folder                  Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFileChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable, GtkFileChooser and GtkFileChooserEmbed.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserWidget
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFileChooserWidget" 'gtk-file-chooser-widget))

(define-g-object-class "GtkFileChooserWidget" gtk-file-chooser-widget
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFileChooser"
;                TODO: This interface is not documented.
;                We have no implementation.
;                "GtkFileChooserEmbed"
               )
   :type-initializer "gtk_file_chooser_widget_get_type")
  ((search-mode
    gtk-file-chooser-widget-search-mode
    "search-mode" "gboolean" t t)
   (subtitle
    gtk-file-chooser-widget-subtitle
    "subtitle" "gchararray" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-chooser-widget 'type)
 "@version{2019-5-13}
  @begin{short}
    @sym{gtk-file-chooser-widget} is a widget for choosing files.
  @end{short}
  It exposes the @class{gtk-file-chooser} interface, and you should use the
  methods of this interface to interact with the widget.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-file-chooser-widget} has a single CSS node with name
    @code{filechooser}.
  @end{dictionary}
  @see-class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-file-chooser-widget-search-mode ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "search-mode"
                                               'gtk-file-chooser-widget) 't)
 "The @code{search-mode} property of type @code{:boolean} (Read / Write) @br{}
  Search mode. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-search-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-search-mode 'function)
 "@version{2019-5-13}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-widget]{search-mode} slot of the
    @class{gtk-file-chooser-widget} class.
  @end{short}
  @see-class{gtk-file-chooser-widget}")

;;; --- gtk-file-chooser-widget-subtitle ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "subtitle"
                                               'gtk-file-chooser-widget) 't)
 "The @code{subtitle} property of type @code{:string} (Read) @br{}
  Subtitle. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-subtitle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-subtitle 'function)
 "@version{2019-5-13}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-widget]{subtitle} slot of the
    @class{gtk-file-chooser-widget} class.
  @end{short}
  @see-class{gtk-file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-file-chooser-widget-child-expand -----------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-expand 'function)
 "@version{2013-8-27}
  Accessor of the @code{expand} child property of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; --- gtk-file-chooser-widget-child-fill -------------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-fill 'function)
 "@version{2013-8-27}
  Accessor of the @code{fill} child property of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; --- gtk-file-chooser-widget-child-padding ----------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-padding
                       "padding" "guint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-padding
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-padding 'function)
 "@version{2013-8-27}
  Accessor of the @code{padding} child property of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; --- gtk-file-chooser-widget-child-pack-type --------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-pack-type
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-pack-type 'function)
 "@version{2013-8-27}
  Accessor of the @code{pack-type} child property of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; --- gtk-file-chooser-widget-child-position ---------------------------------

(define-child-property "GtkFileChooserWidget"
                       gtk-file-chooser-widget-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-widget-child-position
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-widget-child-position 'function)
 "@version{2013-8-27}
  Accessor of the @code{position} child property of the
  @class{gtk-file-chooser-widget} class.
  @see-class{gtk-file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-widget-new))

(defun gtk-file-chooser-widget-new (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-27}
  @argument[action]{open or save mode for the widget, a value of the
    @symbol{gtk-file-chooser-action} enumeration}
  @return{A new @class{gtk-file-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk-file-chooser-widget}.
  @end{short}
  This is a file chooser widget that can be embedded in custom windows, and it
  is the same widget that is used by @class{gtk-file-chooser-dialog}.
  @see-class{gtk-file-chooser-widget}
  @see-class{gtk-file-chooser-dialog}
  @see-symbol{gtk-file-chooser-action}"
  (make-instance 'gtk-file-chooser-widget
                 :action action))

(export 'gtk-file-chooser-widget-new)

;;; --- End of file gtk.file-chooser-widget.lisp -------------------------------

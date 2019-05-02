;;; ----------------------------------------------------------------------------
;;; gtk.shortcuts-group.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkShortcutsGroup
;;;
;;;     Represents a group of shortcuts in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsGroup
;;;
;;; Properties
;;;
;;;     GtkSizeGroup*  accel-size-group    Write
;;;            guint   height              Read
;;;            gchar*  title               Read / Write
;;;     GtkSizeGroup*  title-size-group    Write
;;;            gchar*  view                Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkShortcutsGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkShortcutsGroup implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutsGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcutsGroup" gtk-shortcuts-group
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_shortcuts_group_get_type")
   ((accel-size-group
    gtk-shortcuts-group-accel-size-group
    "accel-size-group" "GtkSizeGroup" nil t)
   (height
    gtk-shortcuts-group-height
    "height" "guint" t nil)
   (title
    gtk-shortcuts-group-title
    "title" "gchararray" t t)
   (title-size-group
    gtk-shortcuts-group-title-size-group
    "title-size-group" "GtkSizeGroup" nil t)
   (view
    gtk-shortcuts-group-view
    "view" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-shortcuts-group 'type)
 "@version{2019-4-12}
  @begin{short}
    A @sym{gtk-shortcuts-group} represents a group of related keyboard shortcuts
    or gestures.
  @end{short}
  The group has a title. It may optionally be associated with a view of the
  application, which can be used to show only relevant shortcuts depending on
  the application context.

  This widget is only meant to be used with @class{gtk-shortcuts-window}.
  @see-slot{gtk-shortcuts-group-accel-size-group}
  @see-slot{gtk-shortcuts-group-height}
  @see-slot{gtk-shortcuts-group-title}
  @see-slot{gtk-shortcuts-group-title-size-group}
  @see-slot{gtk-shortcuts-group-view}")

;;; --- gtk-shortcuts-group-accel-size-group -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-size-group"
                      'gtk-shortcuts-group) 't)
 "The @code{accel-size-group} property of type @symbol{gtk-size-group}
  (Write) @br{}
  The size group for the accelerator portion of shortcuts in this group.
  This is used internally by GTK+, and must not be modified by applications.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-group-accel-size-group
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-group-accel-size-group 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-group]{accel-size-group} of the
    @class{gtk-shortcuts-group} class.
  @end{short}
  @see-class{gtk-shortcuts-group}")

;;; --- gtk-shortcuts-group-height ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height"
                      'gtk-shortcuts-group) 't)
 "The @code{height} property of type @symbol{gtk-size-group} (Read) @br{}
  A rough measure for the number of lines in this group.
  This is used internally by GTK+, and is not useful for applications. @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-group-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-group-height 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-group]{height} of the
    @class{gtk-shortcuts-group} class.
  @end{short}
  @see-class{gtk-shortcuts-group}")

;;; --- gtk-shortcuts-group-title ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                      'gtk-shortcuts-group) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title for this group of shortcuts. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-group-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-group-title 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-group]{title} of the
    @class{gtk-shortcuts-group} class.
  @end{short}
  @see-class{gtk-shortcuts-group}")

;;; --- gtk-shortcuts-group-title-size-group -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title-size-group"
                      'gtk-shortcuts-group) 't)
 "The @code{title-size-group} property of type @code{:string} (Write) @br{}
  The size group for the textual portion of shortcuts in this group. This is
  used internally by GTK+, and must not be modified by applications.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-group-title-size-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-group-title-size-group 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-group]{title-size-group} of the
    @class{gtk-shortcuts-group} class.
  @end{short}
  @see-class{gtk-shortcuts-group}")

;;; --- gtk-shortcuts-group-view -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "view"
                      'gtk-shortcuts-group) 't)
 "The @code{view} property of type @code{:string} (Read / Write) @br{}
  An optional view that the shortcuts in this group are relevant for. The group
  will be hidden if the @code{view-name} property does not match the view of
  this group.
  Set this to NULL to make the group always visible. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-group-view atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-group-view 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-group]{view} of the
    @class{gtk-shortcuts-group} class.
  @end{short}
  @see-class{gtk-shortcuts-group}")

;;; --- End of file gtk.shortcuts-group.lisp -----------------------------------

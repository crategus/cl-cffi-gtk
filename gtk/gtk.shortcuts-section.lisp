;;; ----------------------------------------------------------------------------
;;; gtk.shortcuts-section.lisp
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
;;; GtkShortcutsSection
;;;
;;;     Represents an application mode in a GtkShortcutsWindow
;;;
;;; Types and Values
;;;
;;;     GtkShortcutsSection
;;;
;;; Properties
;;;
;;;        guint   max-height             Read / Write
;;;        gchar*  section-name           Read / Write
;;;        gchar*  title                  Read / Write
;;;        gchar*  view-name              Read / Write
;;;
;;; Signals
;;;
;;;     gboolean   change-current-page    Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkShortcutsSection
;;;
;;; Implemented Interfaces
;;;
;;;     GtkShortcutsSection implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkShortcutsSection
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkShortcutsSection" gtk-shortcuts-section
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_shortcuts_section_get_type")
   ((max-height
    gtk-shortcuts-section-max-height
    "max-height" "guint" t t)
   (section-name
    gtk-shortcuts-section-section-name
    "section-name" "gchar" t t)
   (title
    gtk-shortcuts-section-title
    "title" "gchar" t t)
   (view-name
    gtk-shortcus-section-view-name
    "view-name" "gchar" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-shortcuts-section 'type)
 "@version{2019-4-12}
  @begin{short}
    A @sym{gtk-shortcuts-section} collects all the keyboard shortcuts and
    gestures for a major application mode.
  @end{short}
  If your application needs multiple sections, you should give each section a
  unique @code{section-name} and a @code{title} that can be shown in the section
  selector of the @class{gtk-shortcuts-window}.

  The @code{max-height} property can be used to influence how the groups in the
  section are distributed over pages and columns.

  This widget is only meant to be used with @class{gtk-shortcuts-window}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
 lambda (shortcutsection arg1)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[shortcutswindow]{The @sym{gtk-shortcuts-window} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-shortcuts-section-max-height}
  @see-slot{gtk-shortcuts-section-section-name}
  @see-slot{gtk-shortcuts-section-title}
  @see-slot{gtk-shortcuts-section-view-name}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-shortcuts-section-max-height ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-height"
                      'gtk-shortcuts-section) 't)
 "The @code{max-heigth} property of type @code{:uint} (Read / Write) @br{}
  The maximum number of lines to allow per column. This property can be used to
  influence how the groups in this section are distributed across pages and
  columns. The default value of 15 should work in for most cases. @br{}
  Default value: 15")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-section-max-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-section-max-height 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-section]{max-height} of the
    @class{gtk-shortcuts-section} class.
  @end{short}
  @see-class{gtk-shortcuts-section}")

;;; --- gtk-shortcuts-section-section-name -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "section-name"
                      'gtk-shortcuts-section) 't)
 "The @code{section-name} property of type @code{:string} (Read / Write) @br{}
  A unique name to identify this section among the sections added to the
  @class{gtk-shortcuts-window}. Setting the @code{section-name} property to this
  string will make this section shown in the @class{gtk-shortcuts-window}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-section-section-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-section-section-name 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-section]{section-name} of the
    @class{gtk-shortcuts-section} class.
  @end{short}
  @see-class{gtk-shortcuts-section}")

;;; --- gtk-shortcuts-section-title --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                      'gtk-shortcuts-section) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The string to show in the section selector of the @class{gtk-shortcuts-window}
  for this section. If there is only one section, you don't need to set a title,
  since the section selector will not be shown in this case. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-section-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-section-title 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-section]{title} of the
    @class{gtk-shortcuts-section} class.
  @end{short}
  @see-class{gtk-shortcuts-section}")

;;; --- gtk-shortcuts-section-view-name ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "view-name"
                      'gtk-shortcuts-section) 't)
 "The @code{view-name} property of type @code{:string} (Read / Write) @br{}
  A view name to filter the groups in this section by. See \"view\".
  Applications are expected to use the @code{view-name} property for this
  purpose. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-shortcuts-section-view-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-shortcuts-section-view-name 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-shortcuts-section]{view-name} of the
    @class{gtk-shortcuts-section} class.
  @end{short}
  @see-class{gtk-shortcuts-section}")

;;; --- End of file gtk.shortcuts-section.lisp ---------------------------------

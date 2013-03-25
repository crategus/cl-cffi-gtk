;;; ----------------------------------------------------------------------------
;;; gtk.size-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkSizeGroup
;;;
;;; Grouping widgets so they request the same size
;;;
;;; Synopsis
;;;
;;;     GtkSizeGroup
;;;     GtkSizeGroupMode
;;;
;;;     gtk_size_group_new
;;;     gtk_size_group_set_mode
;;;     gtk_size_group_get_mode
;;;     gtk_size_group_set_ignore_hidden
;;;     gtk_size_group_get_ignore_hidden
;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSizeGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSizeGroup" gtk-size-group
  (:superclass g-object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_size_group_get_type")
  ((ignore-hidden
    gtk-size-group-ignore-hidden
    "ignore-hidden" "gboolean" t t)
   (mode
    gtk-size-group-mode
    "mode" "GtkSizeGroupMode" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-size-group 'type)
 "@version{2013-3-23}
  @begin{short}
    GtkSizeGroup provides a mechanism for grouping a number of widgets together
    so they all request the same amount of space. This is typically useful when
    you want a column of widgets to have the same size, but you can't use a
    GtkGrid widget.
  @end{short}

  In detail, the size requested for each widget in a GtkSizeGroup is the
  maximum of the sizes that would have been requested for each widget in the
  size group if they were not in the size group. The mode of the size group
  (see gtk_size_group_set_mode()) determines whether this applies to the
  horizontal size, the vertical size, or both sizes.

  Note that size groups only affect the amount of space requested, not the
  size that the widgets finally receive. If you want the widgets in a
  GtkSizeGroup to actually be the same size, you need to pack them in such a
  way that they get the size they request and not more. For example, if you
  are packing your widgets into a table, you would not include the GTK_FILL
  flag.

  GtkSizeGroup objects are referenced by each widget in the size group, so
  once you have added all widgets to a GtkSizeGroup, you can drop the initial
  reference to the size group with g_object_unref(). If the widgets in the
  size group are subsequently destroyed, then they will be removed from the
  size group and drop their references on the size group; when all widgets
  have been removed, the size group will be freed.

  Widgets can be part of multiple size groups; GTK+ will compute the
  horizontal size of a widget from the horizontal requisition of all widgets
  that can be reached from the widget by a chain of size groups of type
  GTK_SIZE_GROUP_HORIZONTAL or GTK_SIZE_GROUP_BOTH, and the vertical size from
  the vertical requisition of all widgets that can be reached from the widget
  by a chain of size groups of type GTK_SIZE_GROUP_VERTICAL or
  GTK_SIZE_GROUP_BOTH.

  Note that only non-contextual sizes of every widget are ever consulted by
  size groups (since size groups have no knowledge of what size a widget will
  be allocated in one dimension, it cannot derive how much height a widget
  will receive for a given width). When grouping widgets that trade height for
  width in mode GTK_SIZE_GROUP_VERTICAL or GTK_SIZE_GROUP_BOTH: the height for
  the minimum width will be the requested height for all widgets in the group.
  The same is of course true when horizontally grouping width for height
  widgets.

  Widgets that trade height-for-width should set a reasonably large minimum
  width by way of \"width-chars\" for instance. Widgets with static sizes as
  well as widgets that grow (such as ellipsizing text) need no such
  considerations.

  @subheading{GtkSizeGroup as GtkBuildable}
  Size groups can be specified in a UI definition by placing an <object>
  element with class=\"GtkSizeGroup\" somewhere in the UI definition. The
  widgets that belong to the size group are specified by a <widgets> element
  that may contain multiple <widget> elements, one for each member of the size
  group. The name attribute gives the id of the widget.
 
  Example 100. A UI definition fragment with GtkSizeGroup
  @begin{pre}
   <object class=\"GtkSizeGroup\">
     <property name=\"mode\">GTK_SIZE_GROUP_HORIZONTAL</property>
     <widgets>
       <widget name=\"radio1\"/>
       <widget name=\"radio2\"/>
     </widgets>
   </object>
  @end{pre}
  @see-slot{gtk-size-group-ignore-hidden}
  @see-slot{gtk-size-group-mode}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ignore-hidden"
                                               'gtk-size-group) 't)
 "The @code{\"ignore-hidden\"} property of type @code{:boolean}
  (Read / Write)@br{}
  If TRUE, unmapped widgets are ignored when determining the size of the
  group. @br{}
  Default value: FALSE @br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-size-group) 't)
 "The @code{\"mode\"} property of type @symbol{gtk-size-group-mode}
  (Read / Write)@br{}
  The directions in which the size group affects the requested sizes of its
  component widgets. @br{}
  Default value: GTK_SIZE_GROUP_HORIZONTAL")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-size-group-ignore-hidden atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-size-group-ignore-hidden 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"ignore-hidden\"} of the @class{gtk-size-group}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-size-group-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-size-group-mode 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"mode\"} of the @class{gtk-size-group}
  class.")

;;; ----------------------------------------------------------------------------
;;; enum GtkSizeGroupMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSizeGroupMode" gtk-size-group-mode
  (:export t
   :type-initializer "gtk_size_group_mode_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-size-group-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-size-group-mode atdoc:*external-symbols*)
 "@version{2013-23}
  @begin{short}
    The mode of the size group determines the directions in which the size group
    affects the requested sizes of its component widgets.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSizeGroupMode\" gtk-size-group-mode
  (:export t
   :type-initializer \"gtk_size_group_mode_get_type\")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{group has no effect}
    @entry[:horizontal]{group affects horizontal requisition}
    @entry[:vertical]{group affects vertical requisition}
    @entry[:both]{group affects both horizontal and vertical requisition}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-new))

(defun gtk-size-group-new (mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[mode]{the mode for the new size group.}
  @return{a newly created GtkSizeGroup}
  Create a new GtkSizeGroup."
  (make-instance 'gtk-size-group
                 :mode mode))

(export 'gtk-size-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-set-mode))

(defun gtk-size-group-set-mode (size-group mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @argument[mode]{the mode to set for the size group.}
  Sets the GtkSizeGroupMode of the size group. The mode of the size group
  determines whether the widgets in the size group should all have the same
  horizontal requisition (GTK_SIZE_GROUP_HORIZONTAL) all have the same
  vertical requisition (GTK_SIZE_GROUP_VERTICAL), or should all have the same
  requisition in both directions (GTK_SIZE_GROUP_BOTH)."
  (setf (gtk-size-group-mode size-group) mode))

(export 'gtk-size-group-set-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-get-mode))

(defun gtk-size-group-get-mode (size-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @return{The current mode of the size group.}
  Gets the current mode of the size group. See gtk_size_group_set_mode()."
  (gtk-size-group-mode size-group))

(export 'gtk-size-group-get-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_ignore_hidden ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-set-ignore-hidden))

(defun gtk-size-group-set-ignore-hidden (size-group ignore-hidden)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @argument[ignore_hidden]{whether unmapped widgets should be ignored when
    calculating the size}
  @begin{short}
    Sets whether unmapped widgets should be ignored when calculating the size.
  @end{short}

  Since 2.8"
  (setf (gtk-size-group-ignore-hidden size-group) ignore-hidden))

(export 'gtk-size-group-set-ignore-hidden)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_ignore_hidden ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-get-ignore-hidden))

(defun gtk-size-group-get-ignore-hidden (size-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @return{TRUE if invisible widgets are ignored.}
  @begin{short}
    Returns if invisible widgets are ignored when calculating the size.
  @end{short}

  Since 2.8"
  (gtk-size-group-ignore-hidden size-group))

(export 'gtk-size-group-get-ignore-hidden)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_add_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_add_widget" gtk-size-group-add-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @argument[widget]{the GtkWidget to add}
  @begin{short}
    Adds a widget to a GtkSizeGroup. In the future, the requisition of the
    widget will be determined as the maximum of its requisition and the
    requisition of the other widgets in the size group. Whether this applies
    horizontally, vertically, or in both directions depends on the mode of the
    size group. See gtk_size_group_set_mode().
  @end{short}

  When the widget is destroyed or no longer referenced elsewhere, it will be
  removed from the size group."
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-add-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_remove_widget" gtk-size-group-remove-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @argument[widget]{the GtkWidget to remove}
  Removes a widget from a GtkSizeGroup."
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-remove-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_widgets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_get_widgets" gtk-size-group-get-widgets)
    (g-slist g-object :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[size_group]{a GtkSizeGroup}
  @begin{return}
    A GSList of widgets. The list is owned by GTK+ and should not be modified.
  @end{return}
  @begin{short}
    Returns the list of widgets associated with size_group.
  @end{short}

  Since 2.10"
  (size-group g-object))

(export 'gtk-size-group-widgets)

;;; --- End of file gtk.size-group.lisp ---------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.size-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
 "@version{2013-5-23}
  @begin{short}
    @sym{gtk-size-group} provides a mechanism for grouping a number of widgets
    together so they all request the same amount of space. This is typically
    useful when you want a column of widgets to have the same size, but you
    cannot use a @class{gtk-grid} widget.
  @end{short}

  In detail, the size requested for each widget in a @sym{gtk-size-group} is the
  maximum of the sizes that would have been requested for each widget in the
  size group if they were not in the size group. The mode of the size group
  (see the function @fun{gtk-size-group-set-mode}) determines whether this
  applies to the horizontal size, the vertical size, or both sizes.

  Note that size groups only affect the amount of space requested, not the
  size that the widgets finally receive. If you want the widgets in a
  @sym{gtk-size-group} to actually be the same size, you need to pack them in
  such a way that they get the size they request and not more. For example, if
  you are packing your widgets into a table, you would not include the
  @code{:fill} flag.

  @sym{gtk-size-group} objects are referenced by each widget in the size group,
  so once you have added all widgets to a @sym{gtk-size-group}, you can drop the
  initial reference to the size group with the function @fun{g-object-unref}. If
  the widgets in the size group are subsequently destroyed, then they will be
  removed from the size group and drop their references on the size group; when
  all widgets have been removed, the size group will be freed.

  Widgets can be part of multiple size groups; GTK+ will compute the
  horizontal size of a widget from the horizontal requisition of all widgets
  that can be reached from the widget by a chain of size groups of type
  @code{:horizontal} or @code{:both}, and the vertical size from the vertical
  requisition of all widgets that can be reached from the widget by a chain of
  size groups of type @code{:vertical} or @code{:both}.

  Note that only non-contextual sizes of every widget are ever consulted by
  size groups (since size groups have no knowledge of what size a widget will
  be allocated in one dimension, it cannot derive how much height a widget
  will receive for a given width). When grouping widgets that trade height for
  width in mode @code{:vertical} or @code{:both}: the height for the minimum
  width will be the requested height for all widgets in the group. The same is
  of course true when horizontally grouping width for height widgets.

  Widgets that trade height-for-width should set a reasonably large minimum
  width by way of \"width-chars\" for instance. Widgets with static sizes as
  well as widgets that grow (such as ellipsizing text) need no such
  considerations.

  @subheading{@sym{gtk-size-group} as @class{gtk-buildable}}
    Size groups can be specified in a UI definition by placing an <object>
    element with class=\"GtkSizeGroup\" somewhere in the UI definition. The
    widgets that belong to the size group are specified by a <widgets> element
    that may contain multiple <widget> elements, one for each member of the size
    group. The name attribute gives the id of the widget.
 
    @b{Example:} A UI definition fragment with @sym{gtk-size-group}
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
  (Read / Write) @br{}
  If @em{true}, unmapped widgets are ignored when determining the size of the
  group. @br{}
  Default value: @code{nil} @br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-size-group) 't)
 "The @code{\"mode\"} property of type @symbol{gtk-size-group-mode}
  (Read / Write) @br{}
  The directions in which the size group affects the requested sizes of its
  component widgets. @br{}
  Default value: @code{:horizontal}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
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
 "@version{2013-5-23}
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
    @entry[:none]{Group has no effect.}
    @entry[:horizontal]{Group affects horizontal requisition.}
    @entry[:vertical]{Group affects vertical requisition.}
    @entry[:both]{Group affects both horizontal and vertical requisition.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-new))

(defun gtk-size-group-new (mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[mode]{the mode for the new size group}
  @return{A newly created @class{gtk-size-group} object.}
  Create a new @class{gtk-size-group} object."
  (make-instance 'gtk-size-group
                 :mode mode))

(export 'gtk-size-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-set-mode))

(defun gtk-size-group-set-mode (size-group mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @argument[mode]{the mode to set for the size group}
  Sets the @symbol{gtk-size-group-mode} of the size group. The mode of the size
  group determines whether the widgets in the size group should all have the
  same horizontal requisition (@code{:horizontal}) all have the same vertical
  requisition (@code{:vertical}), or should all have the same requisition in
  both directions (@code{:both})."
  (setf (gtk-size-group-mode size-group) mode))

(export 'gtk-size-group-set-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-get-mode))

(defun gtk-size-group-get-mode (size-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @return{The current mode of the size group.}
  Gets the current mode of the size group. See the function
  @fun{gtk-size-group-set-mode}.
  @see-function{gtk-size-group-set-mode}"
  (gtk-size-group-mode size-group))

(export 'gtk-size-group-get-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_ignore_hidden ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-size-group-set-ignore-hidden))

(defun gtk-size-group-set-ignore-hidden (size-group ignore-hidden)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @argument[ignore-hidden]{whether unmapped widgets should be ignored when
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
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @return{@em{True} if invisible widgets are ignored.}
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
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @argument[widget]{the @class{gtk-widget} to add}
  @begin{short}
    Adds a widget to a @class{gtk-size-group} object. In the future, the
    requisition of the widget will be determined as the maximum of its
    requisition and the requisition of the other widgets in the size group.
    Whether this applies horizontally, vertically, or in both directions
    depends on the mode of the size group. See the function
    @fun{gtk-size-group-set-mode}.
  @end{short}

  When the widget is destroyed or no longer referenced elsewhere, it will be
  removed from the size group.
  @see-function{gtk-size-group-set-mode}"
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-add-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_remove_widget" gtk-size-group-remove-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @argument[widget]{the @class{gtk-widget} to remove}
  Removes a widget from a @class{gtk-size-group} object."
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-remove-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_widgets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_get_widgets" gtk-size-group-get-widgets)
    (g-slist g-object :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[size-group]{a @class{gtk-size-group} object}
  @begin{return}
    A list of widgets. The list is owned by GTK+ and should not be modified.
  @end{return}
  @begin{short}
    Returns the list of widgets associated with @arg{size-group}.
  @end{short}

  Since 2.10"
  (size-group g-object))

(export 'gtk-size-group-widgets)

;;; --- End of file gtk.size-group.lisp ---------------------------------------

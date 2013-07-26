;;; ----------------------------------------------------------------------------
;;; gtk.tool-item-group.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; GtkToolItemGroup
;;;
;;; A sub container used in a tool palette
;;;
;;; Synopsis
;;;
;;;     GtkToolItemGroup
;;;
;;;     gtk_tool_item_group_get_collapsed
;;;     gtk_tool_item_group_get_drop_item
;;;     gtk_tool_item_group_get_ellipsize
;;;     gtk_tool_item_group_get_item_position
;;;     gtk_tool_item_group_get_n_items
;;;     gtk_tool_item_group_get_label
;;;     gtk_tool_item_group_get_label_widget
;;;     gtk_tool_item_group_get_nth_item
;;;     gtk_tool_item_group_get_header_relief
;;;     gtk_tool_item_group_insert
;;;     gtk_tool_item_group_new
;;;     gtk_tool_item_group_set_collapsed
;;;     gtk_tool_item_group_set_ellipsize
;;;     gtk_tool_item_group_set_item_position
;;;     gtk_tool_item_group_set_label
;;;     gtk_tool_item_group_set_label_widget
;;;     gtk_tool_item_group_set_header_relief
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItemGroup
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolItemGroup" 'gtk-tool-item-group))

(define-g-object-class "GtkToolItemGroup" gtk-tool-item-group
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkToolShell")
   :type-initializer "gtk_tool_item_group_get_type")
  ((collapsed
    gtk-tool-item-group-collapsed
    "collapsed" "gboolean" t t)
   (ellipsize
    gtk-tool-item-group-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (header-relief
    gtk-tool-item-group-header-relief
    "header-relief" "GtkReliefStyle" t t)
   (label
    gtk-tool-item-group-label
    "label" "gchar" t t)
   (label-widget
    gtk-tool-item-group-label-widget
    "label-widget" "GtkWidget" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-item-group 'type)
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-tool-item-group} is used together with @class{gtk-tool-palette}
    to add @class{gtk-tool-item}'s to a palette like container with different
    categories and drag and drop support.
  @end{short}
  @begin[Child Property Details]{dictionary}
    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item should receive extra space when the group grows. @br{}
      Default value: @code{nil}

    @subheading{The \"fill\" child property}
      @code{\"fill\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item should fill the available space. @br{}
      Default value: @em{true}

    @subheading{The \"homogeneous\" child property}
      @code{\"homogeneous\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item should be the same size as other homogeneous items. @br{}
      Default value: @em{true}

    @subheading{The \"new-row\" child property}
      @code{\"new-row\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item should start a new row. @br{}
      Default value: @code{nil}

    @subheading{The \"position\" child property}
      @code{\"position\"} of type @code{:int} (Read / Write) @br{}
      Position of the item within this group. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"expander-size\" style property}
      @code{\"expander-size\"} of type @code{:int} (Read) @br{}
      Size of the expander arrow. @br{}
      Allowed values: >= 0 @br{}
      Default value: 16

    @subheading{The \"header-spacing\" style property}
      @code{\"header-spacing\"} of type @code{:int} (Read) @br{}
      Spacing between expander arrow and caption. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2
  @end{dictionary}
  @see-slot{gtk-tool-item-group-collapsed}
  @see-slot{gtk-tool-item-group-ellipsize}
  @see-slot{gtk-tool-item-group-header-relief}
  @see-slot{gtk-tool-item-group-label}
  @see-slot{gtk-tool-item-group-label-widget}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "collapsed"
                                               'gtk-tool-item-group) 't)
 "The @code{\"collapsed\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the group has been collapsed and items are hidden. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize"
                                               'gtk-tool-item-group) 't)
 "The @code{\"ellipsize\"} property of type @symbol{pango-ellipsize-mode}
  (Read / Write)@br{}
  Ellipsize for item group headers. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "header-relief"
                                               'gtk-tool-item-group) 't)
 "The @code{\"header-relief\"} property of type @symbol{gtk-relief-style}
  (Read / Write)@br{}
  Relief of the group header button. @br{}
  Default value: @code{:normal}@br{}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label"
                                               'gtk-tool-item-group) 't)
 "The @code{\"label\"} property of type @code{:string} (Read / Write)@br{}
  The human-readable title of this item group. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget"
                                               'gtk-tool-item-group) 't)
 "The @code{\"label-widget\"} property of type @class{gtk-widget}
  (Read / Write)@br{}
  A widget to display in place of the usual label.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-collapsed atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-collapsed 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"collapsed\"} of the @class{gtk-tool-item-group}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-ellipsize 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"ellipsize\"} of the @class{gtk-tool-item-group}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-header-relief atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-header-relief 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"header-relief\"} of the
  @class{gtk-tool-item-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-label 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"label\"} of the @class{gtk-tool-item-group}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-label-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-label-widget 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"label-widget\"} of the
  @class{gtk-tool-item-group} class.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-homogeneous
                       "homogeneous" "gboolean" t t t)

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-new-row
                       "new-row" "gboolean" t t t)

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-expand 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"expand\"} of the
  @class{gtk-tool-item-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-fill 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"fill\"} of the
  @class{gtk-tool-item-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-homogeneous 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"homogeneous\"} of the
  @class{gtk-tool-item-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-new-row atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-new-row 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"new-row\"} of the
  @class{gtk-tool-item-group} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-position 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"position\"} of the
  @class{gtk-tool-item-group} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_collapsed ()
;;;
;;; gboolean gtk_tool_item_group_get_collapsed (GtkToolItemGroup *group);
;;;
;;; Gets whether group is collapsed or expanded.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     TRUE if group is collapsed, FALSE if it is expanded
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_drop_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_get_drop_item" gtk-tool-item-group-get-drop-item)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-15}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[x]{the x position}
  @argument[y]{the y position}
  @return{The @class{gtk-tool-item} at position (@arg{x}, @arg{y}).}
  @short{Gets the tool item at position (x, y).}

  Since 2.20"
  (group (g-object gtk-tool-item-group))
  (x :int)
  (y :int))

(export 'gtk-tool-item-group-get-drop-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_ellipsize ()
;;;
;;; PangoEllipsizeMode gtk_tool_item_group_get_ellipsize
;;;                                                   (GtkToolItemGroup *group);
;;;
;;; Gets the ellipsization mode of group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the PangoEllipsizeMode of group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_item_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_get_item_position"
           gtk-tool-item-group-get-item-position) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[group]{a @class{gtk-tool-item-group} object}
  @argument[item]{a @class{gtk-tool-item} object}
  @return{The index of @arg{item} in @arg{group} or -1 if @arg{item} is no
    child of @arg{group}}
  @begin{short}
    Gets the position of @arg{item} in @arg{group} as index.
  @end{short}

  Since 2.20
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-item-group}"
  (group (g-object gtk-tool-item-group))
  (item (g-object gtk-tool-item)))

(export 'gtk-tool-item-group-get-item-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_n_items ()
;;;
;;; guint gtk_tool_item_group_get_n_items (GtkToolItemGroup *group);
;;;
;;; Gets the number of tool items in group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the number of tool items in group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_label ()
;;;
;;; const gchar * gtk_tool_item_group_get_label (GtkToolItemGroup *group);
;;;
;;; Gets the label of group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the label of group. The label is an internal string of group and must
;;;     not be modified. Note that NULL is returned if a custom label has been
;;;     set with gtk_tool_item_group_set_label_widget()
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_label_widget ()
;;;
;;; GtkWidget * gtk_tool_item_group_get_label_widget (GtkToolItemGroup *group);
;;;
;;; Gets the label widget of group. See gtk_tool_item_group_set_label_widget().
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the label widget of group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_nth_item ()
;;;
;;; GtkToolItem * gtk_tool_item_group_get_nth_item (GtkToolItemGroup *group,
;;;                                                 guint index);
;;;
;;; Gets the tool item at index in group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; index :
;;;     the index
;;;
;;; Returns :
;;;     the GtkToolItem at index
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_header_relief ()
;;;
;;; GtkReliefStyle gtk_tool_item_group_get_header_relief
;;;                                                   (GtkToolItemGroup *group);
;;;
;;; Gets the relief mode of the header button of group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the GtkReliefStyle
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_insert" gtk-tool-item-group-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-17}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[item]{the @class{gtk-tool-item} to insert into group}
  @argument[position]{the position of @arg{item} in @arg{group}, starting with
    0. The position -1 means end of list.}
  @begin{short}
    Inserts @arg{item} at @arg{position} in the list of children of @arg{group}.
  @end{short}

  Since 2.20
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-item-group}"
  (group (g-object gtk-tool-item-group))
  (item (g-object gtk-tool-item))
  (position :int))

(export 'gtk-tool-item-group-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_new ()
;;;
;;; GtkWidget * gtk_tool_item_group_new (const gchar *label);
;;;
;;; Creates a new tool item group with label label.
;;;
;;; label :
;;;     the label of the new group
;;;
;;; Returns :
;;;     a new GtkToolItemGroup.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_collapsed ()
;;;
;;; void gtk_tool_item_group_set_collapsed (GtkToolItemGroup *group,
;;;                                         gboolean collapsed);
;;;
;;; Sets whether the group should be collapsed or expanded.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; collapsed :
;;;     whether the group should be collapsed or expanded
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_ellipsize ()
;;;
;;; void gtk_tool_item_group_set_ellipsize (GtkToolItemGroup *group,
;;;                                         PangoEllipsizeMode ellipsize);
;;;
;;; Sets the ellipsization mode which should be used by labels in group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; ellipsize :
;;;     the PangoEllipsizeMode labels in group should use
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_item_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_set_item_position"
           gtk-tool-item-group-set-item-position) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[group]{a @class{gtk-tool-item-group} object}
  @argument[item]{the @class{gtk-tool-item} to move to a new position, should
    be a child of @arg{group}}
  @argument[position]{the new position of @arg{item} in @arg{group}, starting
    with 0. The position -1 means end of list.}
  @begin{short}
    Sets the position of @arg{item} in the list of children of @arg{group}.
  @end{short}

  Since 2.20
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-item-group}"
  (group (g-object gtk-tool-item-group))
  (item (g-object gtk-tool-item))
  (position :int))

(export 'gtk-tool-item-group-set-item-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_label ()
;;;
;;; void gtk_tool_item_group_set_label (GtkToolItemGroup *group,
;;;                                     const gchar *label);
;;;
;;; Sets the label of the tool item group. The label is displayed in the header
;;; of the group.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; label :
;;;     the new human-readable label of of the group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_label_widget ()
;;;
;;; void gtk_tool_item_group_set_label_widget (GtkToolItemGroup *group,
;;;                                            GtkWidget *label_widget);
;;;
;;; Sets the label of the tool item group. The label widget is displayed in the
;;; header of the group, in place of the usual label.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; label_widget :
;;;     the widget to be displayed in place of the usual label
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_header_relief ()
;;;
;;; void gtk_tool_item_group_set_header_relief (GtkToolItemGroup *group,
;;;                                             GtkReliefStyle style);
;;;
;;; Set the button relief of the group header. See gtk_button_set_relief() for
;;; details.
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; style :
;;;     the GtkReliefStyle
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tool-item-group.lisp -----------------------------------

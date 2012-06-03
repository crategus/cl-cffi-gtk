;;; ----------------------------------------------------------------------------
;;; gtk.tool-item-group.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkToolItemGroup
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToolItemGroup implements AtkImplementorIface, GtkBuildable and
;;; GtkToolShell.
;;;
;;; Properties
;;; 
;;;   "collapsed"                gboolean              : Read / Write
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;;   "header-relief"            GtkReliefStyle        : Read / Write
;;;   "label"                    gchar*                : Read / Write
;;;   "label-widget"             GtkWidget*            : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;;   "fill"                     gboolean              : Read / Write
;;;   "homogeneous"              gboolean              : Read / Write
;;;   "new-row"                  gboolean              : Read / Write
;;;   "position"                 gint                  : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "expander-size"            gint                  : Read
;;;   "header-spacing"           gint                  : Read
;;; 
;;; Description
;;; 
;;; A GtkToolItemGroup is used together with GtkToolPalette to add GtkToolItems
;;; to a palette like container with different categories and drag and drop
;;; support.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "collapsed" property
;;; 
;;;   "collapsed"                gboolean              : Read / Write
;;; 
;;; Whether the group has been collapsed and items are hidden.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "ellipsize" property
;;; 
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;; 
;;; Ellipsize for item group headers.
;;; 
;;; Default value: PANGO_ELLIPSIZE_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "header-relief" property
;;; 
;;;   "header-relief"            GtkReliefStyle        : Read / Write
;;; 
;;; Relief of the group header button.
;;; 
;;; Default value: GTK_RELIEF_NORMAL
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;; 
;;;   "label"                    gchar*                : Read / Write
;;; 
;;; The human-readable title of this item group.
;;; 
;;; Default value: ""
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-widget" property
;;; 
;;;   "label-widget"             GtkWidget*            : Read / Write
;;; 
;;; A widget to display in place of the usual label.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "expand" child property
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;; 
;;; Whether the item should receive extra space when the group grows.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "fill" child property
;;; 
;;;   "fill"                     gboolean              : Read / Write
;;; 
;;; Whether the item should fill the available space.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "homogeneous" child property
;;; 
;;;   "homogeneous"              gboolean              : Read / Write
;;; 
;;; Whether the item should be the same size as other homogeneous items.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "new-row" child property
;;; 
;;;   "new-row"                  gboolean              : Read / Write
;;; 
;;; Whether the item should start a new row.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "position" child property
;;; 
;;;   "position"                 gint                  : Read / Write
;;; 
;;; Position of the item within this group.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "expander-size" style property
;;; 
;;;   "expander-size"            gint                  : Read
;;; 
;;; Size of the expander arrow.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;;
;;; ----------------------------------------------------------------------------
;;; The "header-spacing" style property
;;; 
;;;   "header-spacing"           gint                  : Read
;;; 
;;; Spacing between expander arrow and caption.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItemGroup
;;; 
;;; struct GtkToolItemGroup;
;;;
;;; This should not be accessed directly. Use the accessor functions below.
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
;;; 
;;; GtkToolItem * gtk_tool_item_group_get_drop_item (GtkToolItemGroup *group,
;;;                                                  gint x,
;;;                                                  gint y);
;;; 
;;; Gets the tool item at position (x, y).
;;; 
;;; group :
;;;     a GtkToolItemGroup
;;; 
;;; x :
;;;     the x position
;;; 
;;; y :
;;;     the y position
;;; 
;;; Returns :
;;;     the GtkToolItem at position (x, y). [transfer none]
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; gint gtk_tool_item_group_get_item_position (GtkToolItemGroup *group,
;;;                                             GtkToolItem *item);
;;; 
;;; Gets the position of item in group as index.
;;; 
;;; group :
;;;     a GtkToolItemGroup
;;; 
;;; item :
;;;     a GtkToolItem
;;; 
;;; Returns :
;;;     the index of item in group or -1 if item is no child of group
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; void gtk_tool_item_group_insert (GtkToolItemGroup *group,
;;;                                  GtkToolItem *item,
;;;                                  gint position);
;;; 
;;; Inserts item at position in the list of children of group.
;;; 
;;; group :
;;;     a GtkToolItemGroup
;;; 
;;; item :
;;;     the GtkToolItem to insert into group
;;; 
;;; position :
;;;     the position of item in group, starting with 0. The position -1 means
;;;     end of list.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; void gtk_tool_item_group_set_item_position (GtkToolItemGroup *group,
;;;                                             GtkToolItem *item,
;;;                                             gint position);
;;; 
;;; Sets the position of item in the list of children of group.
;;; 
;;; group :
;;;     a GtkToolItemGroup
;;; 
;;; item :
;;;     the GtkToolItem to move to a new position, should be a child of group.
;;; 
;;; position :
;;;     the new position of item in group, starting with 0. The position -1
;;;     means end of list.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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

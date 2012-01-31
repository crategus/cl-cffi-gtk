;;; ----------------------------------------------------------------------------
;;; gtk.size-group.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkSizeGroup
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSizeGroup implements GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "ignore-hidden"            gboolean              : Read / Write
;;;   "mode"                     GtkSizeGroupMode      : Read / Write
;;; 
;;; Description
;;; 
;;; GtkSizeGroup provides a mechanism for grouping a number of widgets together
;;; so they all request the same amount of space. This is typically useful when
;;; you want a column of widgets to have the same size, but you can't use a
;;; GtkTable widget.
;;; 
;;; In detail, the size requested for each widget in a GtkSizeGroup is the
;;; maximum of the sizes that would have been requested for each widget in the
;;; size group if they were not in the size group. The mode of the size group
;;; (see gtk_size_group_set_mode()) determines whether this applies to the
;;; horizontal size, the vertical size, or both sizes.
;;; 
;;; Note that size groups only affect the amount of space requested, not the
;;; size that the widgets finally receive. If you want the widgets in a
;;; GtkSizeGroup to actually be the same size, you need to pack them in such a
;;; way that they get the size they request and not more. For example, if you
;;; are packing your widgets into a table, you would not include the GTK_FILL
;;; flag.
;;; 
;;; GtkSizeGroup objects are referenced by each widget in the size group, so
;;; once you have added all widgets to a GtkSizeGroup, you can drop the initial
;;; reference to the size group with g_object_unref(). If the widgets in the
;;; size group are subsequently destroyed, then they will be removed from the
;;; size group and drop their references on the size group; when all widgets
;;; have been removed, the size group will be freed.
;;; 
;;; Widgets can be part of multiple size groups; GTK+ will compute the
;;; horizontal size of a widget from the horizontal requisition of all widgets
;;; that can be reached from the widget by a chain of size groups of type
;;; GTK_SIZE_GROUP_HORIZONTAL or GTK_SIZE_GROUP_BOTH, and the vertical size
;;; from the vertical requisition of all widgets that can be reached from the
;;; widget by a chain of size groups of type GTK_SIZE_GROUP_VERTICAL or
;;; GTK_SIZE_GROUP_BOTH.
;;; 
;;; Note that only non-contextual sizes of every widget are ever consulted by
;;; size groups (since size groups have no knowledge of what size a widget will
;;; be allocated in one dimension, it cannot derive how much height a widget
;;; will receive for a given width). When grouping widgets that trade height
;;; for width in mode GTK_SIZE_GROUP_VERTICAL or GTK_SIZE_GROUP_BOTH: the height
;;; for the minimum width will be the requested height for all widgets in the
;;; group. The same is of course true when horizontally grouping width for
;;; height widgets.
;;; 
;;; Widgets that trade height-for-width should set a reasonably large minimum
;;; width by way of "width-chars" for instance. Widgets with static sizes as
;;; well as widgets that grow (such as ellipsizing text) need no such
;;; considerations.
;;; 
;;; GtkSizeGroup as GtkBuildable
;;; 
;;; Size groups can be specified in a UI definition by placing an <object>
;;; element with class="GtkSizeGroup" somewhere in the UI definition. The
;;; widgets that belong to the size group are specified by a <widgets> element
;;; that may contain multiple <widget> elements, one for each member of the
;;; size group. The name attribute gives the id of the widget.
;;; 
;;; Example 100. A UI definition fragment with GtkSizeGroup
;;; 
;;; <object class="GtkSizeGroup">
;;;   <property name="mode">GTK_SIZE_GROUP_HORIZONTAL</property>
;;;   <widgets>
;;;     <widget name="radio1"/>
;;;     <widget name="radio2"/>
;;;   </widgets>
;;; </object>
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "ignore-hidden" property
;;; 
;;;   "ignore-hidden"            gboolean              : Read / Write
;;; 
;;; If TRUE, unmapped widgets are ignored when determining the size of the
;;; group.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "mode" property
;;; 
;;;   "mode"                     GtkSizeGroupMode      : Read / Write
;;; 
;;; The directions in which the size group affects the requested sizes of its
;;; component widgets.
;;; 
;;; Default value: GTK_SIZE_GROUP_HORIZONTAL
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSizeGroup
;;; 
;;; struct GtkSizeGroup;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSizeGroup" gtk-size-group
  (:superclass g-object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_size_group_get_type")
  ((ignore-hidden gtk-size-group-ignore-hidden
    "ignore-hidden" "gboolean" t t)
   (mode gtk-size-group-mode
    "mode" "GtkSizeGroupMode" t t)))

;;; ----------------------------------------------------------------------------
;;; enum GtkSizeGroupMode
;;; 
;;; typedef enum {
;;;   GTK_SIZE_GROUP_NONE,
;;;   GTK_SIZE_GROUP_HORIZONTAL,
;;;   GTK_SIZE_GROUP_VERTICAL,
;;;   GTK_SIZE_GROUP_BOTH
;;; } GtkSizeGroupMode;
;;; 
;;; The mode of the size group determines the directions in which the size group
;;; affects the requested sizes of its component widgets.
;;; 
;;; GTK_SIZE_GROUP_NONE
;;;     group has no effect
;;; 
;;; GTK_SIZE_GROUP_HORIZONTAL
;;;     group affects horizontal requisition
;;; 
;;; GTK_SIZE_GROUP_VERTICAL
;;;     group affects vertical requisition
;;; 
;;; GTK_SIZE_GROUP_BOTH
;;;     group affects both horizontal and vertical requisition
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSizeGroupMode" gtk-size-group-mode
  (:export t
   :type-initializer "gtk_size_group_mode_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_new ()
;;; 
;;; GtkSizeGroup * gtk_size_group_new (GtkSizeGroupMode mode);
;;; 
;;; Create a new GtkSizeGroup.
;;; 
;;; mode :
;;;     the mode for the new size group.
;;; 
;;; Returns :
;;;     a newly created GtkSizeGroup
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_mode ()
;;; 
;;; void gtk_size_group_set_mode (GtkSizeGroup *size_group,
;;;                               GtkSizeGroupMode mode);
;;; 
;;; Sets the GtkSizeGroupMode of the size group. The mode of the size group
;;; determines whether the widgets in the size group should all have the same
;;; horizontal requisition (GTK_SIZE_GROUP_HORIZONTAL) all have the same
;;; vertical requisition (GTK_SIZE_GROUP_VERTICAL), or should all have the same
;;; requisition in both directions (GTK_SIZE_GROUP_BOTH).
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; mode :
;;;     the mode to set for the size group.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_mode ()
;;; 
;;; GtkSizeGroupMode gtk_size_group_get_mode (GtkSizeGroup *size_group);
;;; 
;;; Gets the current mode of the size group. See gtk_size_group_set_mode().
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; Returns :
;;;     the current mode of the size group.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_set_ignore_hidden ()
;;; 
;;; void gtk_size_group_set_ignore_hidden (GtkSizeGroup *size_group,
;;;                                        gboolean ignore_hidden);
;;; 
;;; Sets whether unmapped widgets should be ignored when calculating the size.
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; ignore_hidden :
;;;     whether unmapped widgets should be ignored when calculating the size
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_ignore_hidden ()
;;; 
;;; gboolean gtk_size_group_get_ignore_hidden (GtkSizeGroup *size_group);
;;; 
;;; Returns if invisible widgets are ignored when calculating the size.
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; Returns :
;;;     TRUE if invisible widgets are ignored.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_add_widget ()
;;; 
;;; void gtk_size_group_add_widget (GtkSizeGroup *size_group, GtkWidget *widget)
;;; 
;;; Adds a widget to a GtkSizeGroup. In the future, the requisition of the
;;; widget will be determined as the maximum of its requisition and the
;;; requisition of the other widgets in the size group. Whether this applies
;;; horizontally, vertically, or in both directions depends on the mode of the
;;; size group. See gtk_size_group_set_mode().
;;; 
;;; When the widget is destroyed or no longer referenced elsewhere, it will be
;;; removed from the size group.
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; widget :
;;;     the GtkWidget to add
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_add_widget" gtk-size-group-add-widget) :void
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-add-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_remove_widget ()
;;; 
;;; void gtk_size_group_remove_widget (GtkSizeGroup *size_group,
;;;                                    GtkWidget *widget);
;;; 
;;; Removes a widget from a GtkSizeGroup.
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; widget :
;;;     the GtkWidget to remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_remove_widget" gtk-size-group-remove-widget) :void
  (size-group g-object)
  (widget g-object))

(export 'gtk-size-group-remove-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_widgets ()
;;; 
;;; GSList * gtk_size_group_get_widgets (GtkSizeGroup *size_group);
;;; 
;;; Returns the list of widgets associated with size_group.
;;; 
;;; size_group :
;;;     a GtkSizeGroup
;;; 
;;; Returns :
;;;     a GSList of widgets. The list is owned by GTK+ and should not be
;;;     modified.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_size_group_get_widgets" gtk-size-group-widgets)
    (g-slist g-object :free-from-foreign nil)
  (size-group g-object))

(export 'gtk-size-group-widgets)

;;; --- End of file gtk.size-group.lisp ----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.paned.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkPaned
;;;
;;; A widget with two adjustable panes
;;;
;;; Synopsis
;;;
;;;     GtkPaned
;;;
;;;     gtk_paned_new
;;;     gtk_paned_add1
;;;     gtk_paned_add2
;;;     gtk_paned_pack1
;;;     gtk_paned_pack2
;;;     gtk_paned_get_child1
;;;     gtk_paned_get_child2
;;;     gtk_paned_set_position
;;;     gtk_paned_get_position
;;;     gtk_paned_get_handle_window
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkPaned
;;;                            +----GtkHPaned
;;;                            +----GtkVPaned
;;;
;;; Implemented Interfaces
;;;
;;; GtkPaned implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Properties
;;;
;;;   "max-position"             gint                  : Read
;;;   "min-position"             gint                  : Read
;;;   "position"                 gint                  : Read / Write
;;;   "position-set"             gboolean              : Read / Write
;;;
;;; Child Properties
;;;
;;;   "resize"                   gboolean              : Read / Write
;;;   "shrink"                   gboolean              : Read / Write
;;;
;;; Style Properties
;;;
;;;   "handle-size"              gint                  : Read
;;;
;;; Signals
;;;
;;;   "accept-position"                                : Action
;;;   "cancel-position"                                : Action
;;;   "cycle-child-focus"                              : Action
;;;   "cycle-handle-focus"                             : Action
;;;   "move-handle"                                    : Action
;;;   "toggle-handle-focus"                            : Action
;;;
;;; Description
;;;
;;; GtkPaned has two panes, arranged either horizontally or vertically. The
;;; division between the two panes is adjustable by the user by dragging a
;;; handle.
;;;
;;; Child widgets are added to the panes of the widget with gtk_paned_pack1()
;;; and gtk_paned_pack2(). The division between the two children is set by
;;; default from the size requests of the children, but it can be adjusted by
;;; the user.
;;;
;;; A paned widget draws a separator between the two child widgets and a small
;;; handle that the user can drag to adjust the division. It does not draw any
;;; relief around the children or around the separator. (The space in which the
;;; separator is called the gutter.) Often, it is useful to put each child
;;; inside a GtkFrame with the shadow type set to GTK_SHADOW_IN so that the
;;; gutter appears as a ridge. No separator is drawn if one of the children is
;;; missing.
;;;
;;; Each child has two options that can be set, resize and shrink. If resize is
;;; true, then when the GtkPaned is resized, that child will expand or shrink
;;; along with the paned widget. If shrink is true, then that child can be made
;;; smaller than its requisition by the user. Setting shrink to FALSE allows the
;;; application to set a minimum size. If resize is false for both children,
;;; then this is treated as if resize is true for both children.
;;;
;;; The application can set the position of the slider as if it were set by the
;;; user, by calling gtk_paned_set_position().
;;;
;;; Example 91. Creating a paned widget with minimum sizes.
;;;
;;; GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
;;; GtkWidget *frame1 = gtk_frame_new (NULL);
;;; GtkWidget *frame2 = gtk_frame_new (NULL);
;;; gtk_frame_set_shadow_type (GTK_FRAME (frame1), GTK_SHADOW_IN);
;;; gtk_frame_set_shadow_type (GTK_FRAME (frame2), GTK_SHADOW_IN);
;;;
;;; gtk_widget_set_size_request (hpaned, 200, -1);
;;;
;;; gtk_paned_pack1 (GTK_PANED (hpaned), frame1, TRUE, FALSE);
;;; gtk_widget_set_size_request (frame1, 50, -1);
;;;
;;; gtk_paned_pack2 (GTK_PANED (hpaned), frame2, FALSE, FALSE);
;;; gtk_widget_set_size_request (frame2, 50, -1);
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-position" property
;;;
;;;   "max-position"             gint                  : Read
;;;
;;; The largest possible value for the position property. This property is
;;; derived from the size and shrinkability of the widget's children.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 2147483647
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-position" property
;;;
;;;   "min-position"             gint                  : Read
;;;
;;; The smallest possible value for the position property. This property is
;;; derived from the size and shrinkability of the widget's children.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "position" property
;;;
;;;   "position"                 gint                  : Read / Write
;;;
;;; Position of paned separator in pixels (0 means all the way to the left/top).
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "position-set" property
;;;
;;;   "position-set"             gboolean              : Read / Write
;;;
;;; TRUE if the Position property should be used.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "resize" child property
;;;
;;;   "resize"                   gboolean              : Read / Write
;;;
;;; The "resize" child property determines whether the child expands and shrinks
;;; along with the paned widget.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "shrink" child property
;;;
;;;   "shrink"                   gboolean              : Read / Write
;;;
;;; The "shrink" child property determines whether the child can be made smaller
;;; than its requisition.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "handle-size" style property
;;;
;;;   "handle-size"              gint                  : Read
;;;
;;; Width of handle.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 5
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accept-position" signal
;;;
;;; gboolean user_function (GtkPaned *widget,
;;;                         gpointer  user_data)      : Action
;;;
;;; The ::accept-position signal is a keybinding signal which gets emitted to
;;; accept the current position of the handle when moving it using key bindings.
;;;
;;; The default binding for this signal is Return or Space.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cancel-position" signal
;;;
;;; gboolean user_function (GtkPaned *widget,
;;;                         gpointer  user_data)      : Action
;;;
;;; The ::cancel-position signal is a keybinding signal which gets emitted to
;;; cancel moving the position of the handle using key bindings. The position of
;;; the handle will be reset to the value prior to moving it.
;;;
;;; The default binding for this signal is Escape.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cycle-child-focus" signal
;;;
;;; gboolean user_function (GtkPaned *widget,
;;;                         gboolean  reversed,
;;;                         gpointer  user_data)      : Action
;;;
;;; The ::cycle-child-focus signal is a keybinding signal which gets emitted to
;;; cycle the focus between the children of the paned.
;;;
;;; The default binding is f6.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; reversed :
;;;     whether cycling backward or forward
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cycle-handle-focus" signal
;;;
;;; gboolean user_function (GtkPaned *widget,
;;;                         gboolean  reversed,
;;;                         gpointer  user_data)      : Action
;;;
;;; The ::cycle-handle-focus signal is a keybinding signal which gets emitted to
;;; cycle whether the paned should grab focus to allow the user to change
;;; position of the handle by using key bindings.
;;;
;;; The default binding for this signal is f8.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; reversed :
;;;     whether cycling backward or forward
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-handle" signal
;;;
;;; gboolean user_function (GtkPaned     *widget,
;;;                         GtkScrollType scroll_type,
;;;                         gpointer      user_data)        : Action
;;;
;;; The ::move-handle signal is a keybinding signal which gets emitted to move
;;; the handle when the user is using key bindings to move it.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; scroll_type :
;;;     a GtkScrollType
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-handle-focus" signal
;;;
;;; gboolean user_function (GtkPaned *widget,
;;;                         gpointer  user_data)      : Action
;;;
;;; The ::toggle-handle-focus is a keybinding signal which gets emitted to
;;; accept the current position of the handle and then move focus to the next
;;; widget in the focus chain.
;;;
;;; The default binding is Tab.
;;;
;;; widget :
;;;     the object that received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPaned
;;;
;;; struct GtkPaned;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPaned" 'gtk-paned))

(define-g-object-class "GtkPaned" gtk-paned
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_paned_get_type")
  ((max-position
    gtk-paned-max-position
    "max-position" "gint" t nil)
   (min-position
    gtk-paned-min-position
    "min-position" "gint" t nil)
   (position
    gtk-paned-position
    "position" "gint" t t)
   (position-set
    gtk-paned-position-set
    "position-set" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkPaned"
                       gtk-paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkPaned"
                       gtk-paned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_new ()
;;;
;;; GtkWidget * gtk_paned_new (GtkOrientation orientation);
;;;
;;; Creates a new GtkPaned widget.
;;;
;;; orientation :
;;;     the paned's orientation.
;;;
;;; Returns :
;;;     a new GtkPaned.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-new))

(defun gtk-paned-new (orientation)
  (make-instance 'gtk-paned
                 :orientation orientation))

(export 'gtk-panded-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add1 ()
;;;
;;; void gtk_paned_add1 (GtkPaned *paned, GtkWidget *child);
;;;
;;; Adds a child to the top or left pane with default parameters. This is
;;; equivalent to gtk_paned_pack1 (paned, child, FALSE, TRUE).
;;;
;;; paned :
;;;     a paned widget
;;;
;;; child :
;;;     the child to add
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add1 (paned child)
  (gtk-paned-pack1 paned child :resize nil :shrink t))

(export 'gtk-paned-add1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add2 ()
;;;
;;; void gtk_paned_add2 (GtkPaned *paned, GtkWidget *child);
;;;
;;; Adds a child to the bottom or right pane with default parameters. This is
;;; equivalent to gtk_paned_pack2 (paned, child, TRUE, TRUE).
;;;
;;; paned :
;;;     a paned widget
;;;
;;; child :
;;;     the child to add
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add2 (paned child)
  (gtk-paned-pack2 paned child :resize t :shrink t))

(export 'gtk-paned-add2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack1 ()
;;;
;;; void gtk_paned_pack1 (GtkPaned *paned,
;;;                       GtkWidget *child,
;;;                       gboolean resize,
;;;                       gboolean shrink);
;;;
;;; Adds a child to the top or left pane.
;;;
;;; paned :
;;;     a paned widget
;;;
;;; child :
;;;     the child to add
;;;
;;; resize :
;;;     should this child expand when the paned widget is resized.
;;;
;;; shrink :
;;;     can this child be made smaller than its requisition.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack1" %gtk-paned-pack1) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack1 (paned child &key (resize nil) (shrink t))
  (%gtk-paned-pack1 paned child resize shrink))

(export 'gtk-paned-pack1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack2 ()
;;;
;;; void gtk_paned_pack2 (GtkPaned *paned,
;;;                       GtkWidget *child,
;;;                       gboolean resize,
;;;                       gboolean shrink);
;;;
;;; Adds a child to the bottom or right pane.
;;;
;;; paned :
;;;     a paned widget
;;;
;;; child :
;;;     the child to add
;;;
;;; resize :
;;;     should this child expand when the paned widget is resized.
;;;
;;; shrink :
;;;     can this child be made smaller than its requisition.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack2" %gtk-paned-pack2) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack2 (paned child &key (resize t) (shrink t))
  (%gtk-paned-pack2 paned child resize shrink))

(export 'gtk-paned-pack2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child1 ()
;;;
;;; GtkWidget * gtk_paned_get_child1 (GtkPaned *paned);
;;;
;;; Obtains the first child of the paned widget.
;;;
;;; paned :
;;;     a GtkPaned widget
;;;
;;; Returns :
;;;     first child, or NULL if it is not set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child1" gtk-paned-child1) g-object
  (paned g-object))

(export 'gtk-paned-child1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child2 ()
;;;
;;; GtkWidget * gtk_paned_get_child2 (GtkPaned *paned);
;;;
;;; Obtains the second child of the paned widget.
;;;
;;; paned :
;;;     a GtkPaned widget
;;;
;;; Returns :
;;;     second child, or NULL if it is not set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child2" gtk-paned-child2) g-object
  (paned g-object))

(export 'gtk-paned-child2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_set_position ()
;;;
;;; void gtk_paned_set_position (GtkPaned *paned, gint position);
;;;
;;; Sets the position of the divider between the two panes.
;;;
;;; paned :
;;;     a GtkPaned widget
;;;
;;; position :
;;;     pixel position of divider, a negative value means that the position is
;;;     unset.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-set-position))

(defun gtk-paned-set-position (paned position)
  (setf (gtk-paned-position paned) position))

(export 'gtk-paned-set-position)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_position ()
;;;
;;; gint gtk_paned_get_position (GtkPaned *paned);
;;;
;;; Obtains the position of the divider between the two panes.
;;;
;;; paned :
;;;     a GtkPaned widget
;;;
;;; Returns :
;;;     position of the divider
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-get-position))

(defun gtk-paned-get-position (paned)
  (gtk-paned-position paned))

(export 'gkt-paned-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_handle_window ()
;;;
;;; GdkWindow * gtk_paned_get_handle_window (GtkPaned *paned);
;;;
;;; Returns the GdkWindow of the handle. This function is useful when handling
;;; button or motion events because it enables the callback to distinguish
;;; between the window of the paned, a child and the handle.
;;;
;;; paned :
;;;     a GtkPaned
;;;
;;; Returns :
;;;     the paned's handle window
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_handle_window" gtk-paned-get-handle-window)
    (g-object gdk-window)
  (paned (g-object gtk-paned)))

(export 'gtk-paned-get-handle-window)

;;; ----------------------------------------------------------------------------
;;; GtkHPaned
;;;
;;; A container with two panes arranged horizontally
;;;
;;; Synopsis
;;;
;;;     GtkHPaned
;;;
;;;     gtk_hpaned_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkPaned
;;;                            +----GtkHPaned
;;;
;;; Implemented Interfaces
;;;
;;; GtkHPaned implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;;
;;; The HPaned widget is a container widget with two children arranged
;;; horizontally. The division between the two panes is adjustable by the user
;;; by dragging a handle. See GtkPaned for details.
;;;
;;; GtkHPaned has been deprecated, use GtkPaned instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHPaned
;;;
;;; struct GtkHPaned;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHPaned" 'gtk-h-paned))

(define-g-object-class "GtkHPaned" gtk-h-paned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hpaned_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkHPaned"
                       gtk-h-paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkHPaned"
                       gtk-h-paned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_hpaned_new ()
;;;
;;; GtkWidget * gtk_hpaned_new (void);
;;;
;;; Warning
;;;
;;; gtk_hpaned_new has been deprecated since version 3.2 and should not be used
;;; in newly-written code. Use gtk_paned_new() with GTK_ORIENTATION_HORIZONTAL
;;; instead
;;;
;;; Create a new GtkHPaned
;;;
;;; Returns :
;;;     the new GtkHPaned
;;; ----------------------------------------------------------------------------

(defun gtk-hpaned-new ()
  (make-instance 'gtk-h-paned))

(export 'gtk-hpaned-new)

;;; ----------------------------------------------------------------------------
;;; GtkVPaned
;;;
;;; A container with two panes arranged vertically
;;;
;;; Synopsis
;;;
;;;     GtkVPaned
;;;
;;;     gtk_vpaned_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkPaned
;;;                            +----GtkVPaned
;;;
;;; Implemented Interfaces
;;;
;;; GtkVPaned implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; Description
;;;
;;; The VPaned widget is a container widget with two children arranged
;;; vertically. The division between the two panes is adjustable by the user by
;;; dragging a handle. See GtkPaned for details.
;;;
;;; GtkVPaned has been deprecated, use GtkPaned instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVPaned
;;;
;;; struct GtkVPaned;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVPaned" 'gtk-v-paned))

(define-g-object-class "GtkVPaned" gtk-v-paned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vpaned_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkVPaned"
                       gtk-v-paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkVPaned"
                       gtk-v-paned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_vpaned_new ()
;;;
;;; GtkWidget * gtk_vpaned_new (void);
;;;
;;; Warning
;;;
;;; gtk_vpaned_new has been deprecated since version 3.2 and should not be used
;;; in newly-written code. Use gtk_paned_new() with GTK_ORIENTATION_VERTICAL
;;; instead
;;;
;;; Create a new GtkVPaned
;;;
;;; Returns :
;;;     the new GtkVPaned
;;; ----------------------------------------------------------------------------

(defun gtk-vpaned-new ()
  (make-instance 'gtk-v-paned))

(export 'gtk-vpaned-new)

;;; --- End of file gtk.paned.lisp ---------------------------------------------

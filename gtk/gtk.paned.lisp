;;; ----------------------------------------------------------------------------
;;; gtk.paned.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See >http://common-lisp.net/project/cl-gtk2/>.
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
;;;
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-paned 'type)
 "@version{2013-3-17}
  @begin{short}
    GtkPaned has two panes, arranged either horizontally or vertically. The
    division between the two panes is adjustable by the user by dragging a
    handle.
  @end{short}

  Child widgets are added to the panes of the widget with gtk_paned_pack1()
  and gtk_paned_pack2(). The division between the two children is set by
  default from the size requests of the children, but it can be adjusted by
  the user.

  A paned widget draws a separator between the two child widgets and a small
  handle that the user can drag to adjust the division. It does not draw any
  relief around the children or around the separator. (The space in which the
  separator is called the gutter.) Often, it is useful to put each child
  inside a GtkFrame with the shadow type set to GTK_SHADOW_IN so that the
  gutter appears as a ridge. No separator is drawn if one of the children is
  missing.

  Each child has two options that can be set, resize and shrink. If resize is
  true, then when the GtkPaned is resized, that child will expand or shrink
  along with the paned widget. If shrink is true, then that child can be made
  smaller than its requisition by the user. Setting shrink to FALSE allows the
  application to set a minimum size. If resize is false for both children,
  then this is treated as if resize is true for both children.

  The application can set the position of the slider as if it were set by the
  user, by calling gtk_paned_set_position().

  @b{Example.} Creating a paned widget with minimum sizes.
  @begin{pre}
 GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
 GtkWidget *frame1 = gtk_frame_new (NULL);
 GtkWidget *frame2 = gtk_frame_new (NULL);
 gtk_frame_set_shadow_type (GTK_FRAME (frame1), GTK_SHADOW_IN);
 gtk_frame_set_shadow_type (GTK_FRAME (frame2), GTK_SHADOW_IN);

 gtk_widget_set_size_request (hpaned, 200, -1);

 gtk_paned_pack1 (GTK_PANED (hpaned), frame1, TRUE, FALSE);
 gtk_widget_set_size_request (frame1, 50, -1);

 gtk_paned_pack2 (GTK_PANED (hpaned), frame2, FALSE, FALSE);
 gtk_widget_set_size_request (frame2, 50, -1);
  @end{pre}

  @see-slot{gtk-paned-max-position}
  @see-slot{gtk-paned-min-position}
  @see-slot{gtk-paned-position}
  @see-slot{gtk-paned-position-set}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-position" 'gtk-paned) 't)
 "The @code{\"max-position\"} property of type @code{:int} (Read)@br{}
  The largest possible value for the position property. This property is
  derived from the size and shrinkability of the widget's children. @br{}
  Allowed values: >= 0@br{}
  Default value: 2147483647@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-position" 'gtk-paned) 't)
 "The @code{\"min-position\"} property of type @code{:int} (Read)@br{}
  The smallest possible value for the position property. This property is
  derived from the size and shrinkability of the widget's children. @br{}
  Allowed values: >= 0@br{}
  Default value: 0@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "position" 'gtk-paned) 't)
 "The @code{\"position\"} property of type @code{:int} (Read / Write)@br{}
  Position of paned separator in pixels (0 means all the way to the
  left/top). @br{}
  Allowed values: >= 0@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "position-set" 'gtk-paned) 't)
 "The @code{\"position-set\"} property of type @code{:boolean}
  (Read / Write)@br{}
  TRUE if the Position property should be used. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-max-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-max-position 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"max-position\"} of the @class{gtk-paned} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-min-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-min-position 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"min-position\"} of the @class{gtk-paned} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-position 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"position\"} of the @class{gtk-paned} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-position-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-position-set 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"position-set\"} of the @class{gtk-paned} class.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkPaned"
                       gtk-paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkPaned"
                       gtk-paned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-child-resize 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"resize\"} of the @class{gtk-paned}
    class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-child-shrink 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"shrink\"} of the @class{gtk-paned}
    class.")

;;; ----------------------------------------------------------------------------
;;; gtk_paned_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-new))

(defun gtk-paned-new (orientation)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[orientation]{the paned's orientation.}
  @return{A new GtkPaned.}
  @short{Creates a new GtkPaned widget.}

  Since 3.0"
  (make-instance 'gtk-paned
                 :orientation orientation))

(export 'gtk-paned-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add1 ()
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add1 (paned child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a paned widget}
  @argument[child]{the child to add}
  Adds a child to the top or left pane with default parameters. This is
  equivalent to gtk_paned_pack1 (paned, child, FALSE, TRUE)."
  (gtk-paned-pack1 paned child :resize nil :shrink t))

(export 'gtk-paned-add1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add2 ()
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add2 (paned child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a paned widget}
  @argument[child]{the child to add}
  Adds a child to the bottom or right pane with default parameters. This is
  equivalent to gtk_paned_pack2 (paned, child, TRUE, TRUE)."
  (gtk-paned-pack2 paned child :resize t :shrink t))

(export 'gtk-paned-add2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack1 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack1" %gtk-paned-pack1) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack1 (paned child &key (resize nil) (shrink t))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a paned widget}
  @argument[child]{the child to add}
  @argument[resize]{should this child expand when the paned widget is resized.}
  @argument[shrink]{can this child be made smaller than its requisition.}
  Adds a child to the top or left pane."
  (%gtk-paned-pack1 paned child resize shrink))

(export 'gtk-paned-pack1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack2 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack2" %gtk-paned-pack2) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack2 (paned child &key (resize t) (shrink t))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a paned widget}
  @argument[child]{the child to add}
  @argument[resize]{should this child expand when the paned widget is resized.}
  @argument[shrink]{can this child be made smaller than its requisition.}
  Adds a child to the bottom or right pane."
  (%gtk-paned-pack2 paned child resize shrink))

(export 'gtk-paned-pack2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child1 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child1" gtk-paned-child1) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a GtkPaned widget}
  @return{First child, or NULL if it is not set.}
  @short{Obtains the first child of the paned widget.}

  Since 2.4"
  (paned g-object))

(export 'gtk-paned-child1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child2 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child2" gtk-paned-child2) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a GtkPaned widget}
  @return{Second child, or NULL if it is not set.}
  @short{Obtains the second child of the paned widget.}

  Since 2.4"
  (paned g-object))

(export 'gtk-paned-child2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_set_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-set-position))

(defun gtk-paned-set-position (paned position)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a GtkPaned widget}
  @argument[position]{pixel position of divider, a negative value means that
    the position is unset.}
  Sets the position of the divider between the two panes."
  (setf (gtk-paned-position paned) position))

(export 'gtk-paned-set-position)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_position ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-get-position))

(defun gtk-paned-get-position (paned)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a GtkPaned widget}
  @return{position of the divider}
  Obtains the position of the divider between the two panes."
  (gtk-paned-position paned))

(export 'gtk-paned-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_handle_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_handle_window" gtk-paned-get-handle-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[paned]{a GtkPaned}
  @return{the paned's handle window}
  @begin{short}
    Returns the GdkWindow of the handle. This function is useful when handling
    button or motion events because it enables the callback to distinguish
    between the window of the paned, a child and the handle.
  @end{short}

  Since 2.20"
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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHPaned
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHPaned" 'gtk-hpaned))

(define-g-object-class "GtkHPaned" gtk-hpaned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hpaned_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hpaned 'type)
 "@version{2013-3-7}
  @begin{short}
    The @sym{gtk-hpaned} widget is a container widget with two children arranged
    horizontally. The division between the two panes is adjustable by the user
    by dragging a handle. See @class{gtk-paned} for details.
  @end{short}

  @sym{gtk-hpaned} has been deprecated, use @class{gtk-paned} instead.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkHPaned"
                       gtk-hpaned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkHPaned"
                       gtk-hpaned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hpaned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hpaned-child-resize 'function)
 "@version{2013-3-7}
  @begin{short}
    Accessor of the child property @code{\"resize\"} of the @class{gtk-hpaned}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hpaned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hpaned-child-shrink 'function)
 "@version{2013-3-7}
  @begin{short}
    Accessor of the child property @code{\"shrink\"} of the @class{gtk-hpaned}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_hpaned_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-hpaned-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-7}
  @return{The new @class{gtk-hpaned} widget.}
  @b{Warning}

  @sym{gtk-hpaned-new} has been deprecated since version 3.2 and should not be
  used in newly-written code. Use @fun{gtk-paned-new} with @code{:horizontal}
  instead.

  @short{Create a new @class{gtk-hpaned} widget.}"
  (make-instance 'gtk-paned
                 :orientation :horizontal))

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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVPaned
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVPaned" 'gtk-vpaned))

(define-g-object-class "GtkVPaned" gtk-vpaned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vpaned_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vpaned 'type)
 "@version{2013-3-7}
  @begin{short}
    The @sym{gtk-vpaned} widget is a container widget with two children arranged
    vertically. The division between the two panes is adjustable by the user by
    dragging a handle. See @class{gtk-paned} for details.
  @end{short}

  @sym{gtk-vpaned} has been deprecated, use @class{gtk-paned} instead.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkVPaned"
                       gtk-vpaned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkVPaned"
                       gtk-vpaned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vpaned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vpaned-child-resize 'function)
 "@version{2013-3-7}
  @begin{short}
    Accessor of the child property @code{\"resize\"} of the @class{gtk-vpaned}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vpaned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vpaned-child-shrink 'function)
 "@version{2013-3-7}
  @begin{short}
    Accessor of the child property @code{\"shrink\"} of the @class{gtk-vpaned}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_vpaned_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-vpaned-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-7}
  @return{The new @class{gtk-vpaned} widget.}
  @b{Warning}

  @sym{gtk-vpaned-new} has been deprecated since version 3.2 and should not be
  used in newly-written code. Use @fun{gtk-paned-new} with @code{:vertival}
  instead.

  @short{Create a new @class{gtk-vpaned} widget.}"
  (make-instance 'gtk-paned
                 :orientation :vertical))

(export 'gtk-vpaned-new)

;;; --- End of file gtk.paned.lisp ---------------------------------------------

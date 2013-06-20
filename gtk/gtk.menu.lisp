;;; ----------------------------------------------------------------------------
;;; gtk.menu.lisp
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
;;; GtkMenu
;;;
;;; A menu widget
;;;
;;; Synopsis
;;;
;;;     GtkMenu
;;;
;;;     gtk_menu_new
;;;     gtk_menu_new_from_model
;;;     gtk_menu_set_screen
;;;     gtk_menu_reorder_child
;;;     gtk_menu_attach
;;;     gtk_menu_popup_for_device
;;;     gtk_menu_popup
;;;     gtk_menu_set_accel_group
;;;     gtk_menu_get_accel_group
;;;     gtk_menu_set_accel_path
;;;     gtk_menu_get_accel_path
;;;     gtk_menu_set_title
;;;     gtk_menu_get_title
;;;     gtk_menu_set_monitor
;;;     gtk_menu_get_monitor
;;;     gtk_menu_get_tearoff_state
;;;     gtk_menu_set_reserve_toggle_size
;;;     gtk_menu_get_reserve_toggle_size
;;;
;;;     gtk_menu_popdown
;;;     gtk_menu_reposition
;;;     gtk_menu_get_active
;;;     gtk_menu_set_active
;;;     gtk_menu_set_tearoff_state
;;;     gtk_menu_attach_to_widget
;;;     gtk_menu_detach
;;;     gtk_menu_get_attach_widget
;;;     gtk_menu_get_for_attach_widget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenu
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkMenu" 'gtk-menu))

(define-g-object-class "GtkMenu" gtk-menu
  (:superclass gtk-menu-shell
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_menu_get_type")
  ((accel-group
    gtk-menu-accel-group
    "accel-group" "GtkAccelGroup" t t)
   (accel-path
    gtk-menu-accel-path
    "accel-path" "gchararray" t t)
   (active
    gtk-menu-active
    "active" "gint" t t)
   (attach-widget
    gtk-menu-attach-widget
    "attach-widget" "GtkWidget" t t)
   (monitor
    gtk-menu-monitor
    "monitor" "gint" t t)
   (reserve-toggle-size
    gtk-menu-reserve-toggle-size
    "reserve-toggle-size" "gboolean" t t)
   (tearoff-state
    gtk-menu-tearoff-state
    "tearoff-state" "gboolean" t t)
   (tearoff-title
    gtk-menu-tearoff-title
    "tearoff-title" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu 'type)
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-menu} is a @class{gtk-menu-shell} that implements a drop down
    menu consisting of a list of @class{gtk-menu-item} objects which can be
    navigated and activated by the user to perform application functions.
  @end{short}

  A @sym{gtk-menu} is most commonly dropped down by activating a
  @class{gtk-menu-item} in a @class{gtk-menu-bar} or popped up by activating a
  @class{gtk-menu-item} in another @sym{gtk-menu}.

  A @sym{gtk-menu} can also be popped up by activating a
  @class{gtk-combo-box} widget. Other composite widgets such as the
  @class{gtk-notebook} can pop up a @sym{gtk-menu} as well.

  Applications can display a @sym{gtk-menu} as a popup menu by calling the
  @fun{gtk-menu-popup} function. The example below shows how an application can
  pop up a menu when the 3rd mouse button is pressed.

  @b{Example:} Connecting the popup signal handler
  @begin{pre}
 /* connect our handler which will popup the menu */
 g_signal_connect_swapped (window, \"button_press_event\",
 G_CALLBACK (my_popup_handler), menu);
  @end{pre}

  @b{Example:} Signal handler which displays a popup menu
  @begin{pre}
 static gint
 my_popup_handler (GtkWidget *widget, GdkEvent *event)
 {
   GtkMenu *menu;
   GdkEventButton *event_button;

   g_return_val_if_fail (widget != NULL, FALSE);
   g_return_val_if_fail (GTK_IS_MENU (widget), FALSE);
   g_return_val_if_fail (event != NULL, FALSE);

   /* The \"widget\" is the menu that was supplied when
    * g_signal_connect_swapped() was called.
    */
   menu = GTK_MENU (widget);

   if (event->type == GDK_BUTTON_PRESS)
     {
       event_button = (GdkEventButton *) event;
       if (event_button->button == GDK_BUTTON_SECONDARY)
         {
           gtk_menu_popup (menu, NULL, NULL, NULL, NULL,
                           event_button->button, event_button->time);
           return TRUE;
         @}
     @}

   return FALSE;
 @}
  @end{pre}
  @begin[Child Property Details]{dictionary}
    @subheading{The \"bottom-attach\" child property}
      @code{\"bottom-attach\"} of type @code{:int} (Read / Write) @br{}
      The row number to attach the bottom of the child to. @br{}
      Allowed values: >= @code{G_MAXULONG} @br{}
      Default value: @code{-1}

    @subheading{The \"left-attach\" child property}
      @code{\"left-attach\"} of type @code{:int} (Read / Write) @br{}
      The column number to attach the left side of the child to. @br{}
      Allowed values: >= @code{G_MAXULONG} @br{}
      Default value: @code{-1}

    @subheading{The \"right-attach\" child property}
      @code{\"right-attach\"} of type @code{:int} (Read / Write) @br{}
      The column number to attach the right side of the child to. @br{}
      Allowed values: >= @code{G_MAXULONG} @br{}
      Default value: @code{-1}

    @subheading{The \"top-attach\" child property}
      @code{\"top-attach\"} of type @code{:int} (Read / Write) @br{}
      The row number to attach the top of the child to. @br{}
      Allowed values: >= @code{G_MAXULONG} @br{}
      Default value: @code{-1}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"arrow-placement\" style property}
      @code{\"arrow-placement\"} of type @symbol{gtk-arrow-placement}
      (Read) @br{}
      Indicates where scroll arrows should be placed. @br{}
      Default value: @code{:boths} @br{}
      Since 2.16

    @subheading{The \"arrow-scaling\" style property}
      @code{\"arrow-scaling\"} of type @code{:float} (Read) @br{}
      Arbitrary constant to scale down the size of the scroll arrow. @br{}
      Allowed values: [0,1] @br{}
      Default value: 0.7 @br{}
      Since 2.16

    @subheading{The \"double-arrows\" style property}
      @code{\"double-arrows\"} of type @code{:boolean} (Read) @br{}
      When scrolling, always show both arrows. @br{}
      Default value: @em{true}

    @subheading{The \"horizontal-offset\" style property}
      @code{\"horizontal-offset\"} of type @code{:int} (Read) @br{}
      When the menu is a submenu, position it this number of pixels offset
      horizontally. @br{}
      Default value: -2

    @subheading{The \"horizontal-padding\" style property}
      @code{\"horizontal-padding\"} of type @code{:int} (Read) @br{}
      Extra space at the left and right edges of the menu. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0

    @subheading{The \"vertical-offset\" style property}
      @code{\"vertical-offset\"} of type @code{:int} (Read) @br{}
      When the menu is a submenu, position it this number of pixels offset
      vertically. @br{}
      Default value: 0

    @subheading{The \"vertical-padding\" style property}
      @code{\"vertical-padding\"} of type @code{:int} (Read) @br{}
      Extra space at the top and bottom of the menu. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"move-scroll\" signal}
      @begin{pre}
 lambda (menu scroll-type)   : Action
      @end{pre}
      @begin[code]{table}
        @entry[menu]{a @class{gtk-menu} widget}
        @entry[scroll-type]{a @symbol{gtk-scroll-type}}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-menu-accel-group}
  @see-slot{gtk-menu-accel-path}
  @see-slot{gtk-menu-active}
  @see-slot{gtk-menu-attach-widget}
  @see-slot{gtk-menu-monitor}
  @see-slot{gtk-menu-reserve-toggle-size}
  @see-slot{gtk-menu-tearoff-state}
  @see-slot{gtk-menu-tearoff-title}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group" 'gtk-menu) 't)
 "The @code{\"accel-group\"} property of type @class{gtk-accel-group}
  (Read / Write) @br{}
  The accel group holding accelerators for the menu. @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-path" 'gtk-menu) 't)
 "The @code{\"accel-path\"} property of type @code{:string} (Read / Write) @br{}
  An accel path used to conveniently construct accel paths of child items. @br{}
  Default value: @code{nil} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-menu) 't)
 "The @code{\"active\"} property of type @code{:int} (Read / Write) @br{}
  The index of the currently selected menu item, or -1 if no menu item is
  selected. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attach-widget" 'gtk-menu) 't)
 "The @code{\"attach-widget\"} property of type @class{gtk-widget}
  (Read / Write) @br{}
  The widget the menu is attached to. Setting this property attaches the menu
  without a @code{GtkMenuDetachFunc}. If you need to use a detacher, use
  @fun{gtk-menu-attach-to-widget} directly. @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "monitor" 'gtk-menu) 't)
 "The @code{\"monitor\"} property of type @code{:int} (Read / Write) @br{}
  The monitor the menu will be popped up on. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reserve-toggle-size"
                                               'gtk-menu) 't)
 "The @code{\"reserve-toggle-size\"} property of type @code{:boolean}
  (Read / Write) @br{}
  A boolean that indicates whether the menu reserves space for toggles and
  icons, regardless of their actual presence.
  This property should only be changed from its default value for
  special-purposes such as tabular menus. Regular menus that are connected to
  a menu bar or context menus should reserve toggle space for consistency. @br{}
  Default value: @em{true} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-state" 'gtk-menu) 't)
 "The @code{\"tearoff-state\"} property of type @code{:boolean}
  (Read / Write) @br{}
  A boolean that indicates whether the menu is torn-off. @br{}
  Default value: @code{nil} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-title" 'gtk-menu) 't)
 "The @code{\"tearoff-title\"} property of type @code{:string}
  (Read / Write) @br{}
  A title that may be displayed by the window manager when this menu is
  torn-off. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-accel-group 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"accel-group\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-accel-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-accel-path 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"accel-path\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-active 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"active\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-attach-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-attach-widget 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"attach-widget\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-monitor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-monitor 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"monitor\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-reserve-toggle-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-reserve-toggle-size 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"reserve-toggle-size\"} of the @class{gtk-menu}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-tearoff-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-tearoff-state 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"tearoff-state\"} of the @class{gtk-menu} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-tearoff-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-tearoff-title 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"tearoff-title\"} of the @class{gtk-menu} class.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkMenu"
                       gtk-menu-child-left-attach
                       "left-attach" "gint" t t t)

(define-child-property "GtkMenu"
                       gtk-menu-child-right-attach
                       "right-attach" "gint" t t t)

(define-child-property "GtkMenu"
                       gtk-menu-child-top-attach
                       "top-attach" "gint" t t t)

(define-child-property "GtkMenu"
                       gtk-menu-child-bottom-attach
                       "bottom-attach" "gint" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-left-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-left-attach 'function)
 "@version{2013-3-16}
  Accessor of the child property @code{\"left-attach\"} of the @class{gtk-menu}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-right-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-right-attach 'function)
 "@version{2013-3-16}
  Accessor of the child property @code{\"right-attach\"} of the @class{gtk-menu}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-top-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-top-attach 'function)
 "@version{2013-3-16}
  Accessor of the child property @code{\"top-attach\"} of the @class{gtk-menu}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-bottom-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-bottom-attach 'function)
 "@version{2013-3-16}
  Accessor of the child property @code{\"bottom-attach\"} of the
  @class{gtk-menu} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-new))

(defun gtk-menu-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @return{A new @class{gtk-menu} widget.}
  Creates a new @class{gtk-menu} widget."
  (make-instance 'gtk-menu))

(export 'gtk-menu-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_new_from_model ()
;;;
;;; GtkWidget * gtk_menu_new_from_model (GMenuModel *model);
;;;
;;; Creates a GtkMenu and populates it with menu items and submenus according to
;;; model.
;;;
;;; The created menu items are connected to actions found in the
;;; GtkApplicationWindow to which the menu belongs - typically by means of being
;;; attached to a widget (see gtk_menu_attach_to_widget()) that is contained
;;; within the GtkApplicationWindows widget hierarchy.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; Returns :
;;;     a new GtkMenu
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_set_screen" gtk-menu-set-screen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[screen]{a @class{gdk-screen}, or @code{nil} if the screen should be
    determined by the widget the menu is attached to}
  @short{Sets the @class{gdk-screen} on which the menu will be displayed.}

  Since 2.2"
  (menu (g-object gtk-menu))
  (screen (g-object gdk-screen)))

(export 'gtk-menu-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reorder_child" gtk-menu-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[child]{the @class{gtk-menu-item} to move}
  @argument[position]{the new position to place child. Positions are numbered
    from 0 to n - 1.}
  Moves @arg{child} to a new position in the list of menu children."
  (menu g-object)
  (child g-object)
  (position :int))

(export 'gtk-menu-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_attach" gtk-menu-attach) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-menu-item} widget}
  @argument[left-attach]{the column number to attach the left side of the item
    to}
  @argument[right-attach]{the column number to attach the right side of the
    item to}
  @argument[top-attach]{the row number to attach the top of the item to}
  @argument[bottom-attach]{the row number to attach the bottom of the item to}
  @begin{short}
    Adds a new @class{gtk-menu-item} to a (table) menu. The number of cells that
    an item will occupy is specified by @arg{left-attach}, @arg{right-attach},
    @arg{top-attach} and @arg{bottom-attach}. These each represent the leftmost,
    rightmost, uppermost and lower column and row numbers of the table.
   (Columns and rows are indexed from zero).
  @end{short}

  Note that this function is not related to the @fun{gtk-menu-detach} function.

  Since 2.4
  @see-function{gtk-menu-detach}"
  (menu g-object)
  (child g-object)
  (left-attach :uint)
  (right-attach :uint)
  (top-attach :uint)
  (bottom-attach :uint))

(export 'gtk-menu-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_for_device ()
;;;
;;; void gtk_menu_popup_for_device (GtkMenu *menu,
;;;                                 GdkDevice *device,
;;;                                 GtkWidget *parent_menu_shell,
;;;                                 GtkWidget *parent_menu_item,
;;;                                 GtkMenuPositionFunc func,
;;;                                 gpointer data,
;;;                                 GDestroyNotify destroy,
;;;                                 guint button,
;;;                                 guint32 activate_time);
;;;
;;; Displays a menu and makes it available for selection.
;;;
;;; Applications can use this function to display context-sensitive menus, and
;;; will typically supply NULL for the parent_menu_shell, parent_menu_item,
;;; func, data and destroy parameters. The default menu positioning function
;;; will position the menu at the current position of device (or its
;;; corresponding pointer).
;;;
;;; The button parameter should be the mouse button pressed to initiate the menu
;;; popup. If the menu popup was initiated by something other than a mouse
;;; button press, such as a mouse button release or a keypress, button should be
;;; 0.
;;;
;;; The activate_time parameter is used to conflict-resolve initiation of
;;; concurrent requests for mouse/keyboard grab requests. To function properly,
;;; this needs to be the time stamp of the user event (such as a mouse click or
;;; key press) that caused the initiation of the popup. Only if no such event is
;;; available, gtk_get_current_event_time() can be used instead.
;;;
;;; menu :
;;;     a GtkMenu
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; parent_menu_shell :
;;;     the menu shell containing the triggering menu item, or NULL
;;;
;;; parent_menu_item :
;;;     the menu item whose activation triggered the popup, or NULL
;;;
;;; func :
;;;     a user supplied function used to position the menu, or NULL
;;;
;;; data :
;;;     user supplied data to be passed to func
;;;
;;; destroy :
;;;     destroy notify for data
;;;
;;; button :
;;;     the mouse button which was pressed to initiate the event
;;;
;;; activate_time :
;;;     the time at which the activation event occurred
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popup" %gtk-menu-popup) :void
  (menu g-object)
  (parent-menu-shell g-object)
  (parent-menu-item g-object)
  (menu-position-func :pointer)
  (data :pointer)
  (button :uint)
  (activate-time :uint32))

(defcallback gtk-menu-position-func-callback :void
    ((menu g-object)
     (x :pointer)
     (y :pointer)
     (push-in :pointer)
     (data :pointer))
  (restart-case
    (multiple-value-bind (rx ry rpush-in)
        (funcall (glib::get-stable-pointer-value data) menu)
      (setf (mem-ref x :int) rx
            (mem-ref y :int) ry
            (mem-ref push-in :boolean) rpush-in))
    (return-zero () (setf (mem-ref x :int) 0
                          (mem-ref y :int) 0
                          (mem-ref push-in :boolean) nil))))

(defun gtk-menu-popup (menu &key parent-menu-shell
                             parent-menu-item
                             position-func
                             (button 0)
                             (activate-time (gtk-get-current-event-time)))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-16}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[parent-menu-shell]{the menu shell containing the triggering menu
    item, or @code{nil}}
  @argument[parent-menu-item]{the menu item whose activation triggered the
    popup, or @code{nil}}
  @argument[position-func]{a user supplied function used to position the menu,
    or @code{nil}}
  @argument[button]{the mouse button which was pressed to initiate the event.}
  @argument[activate-time]{the time at which the activation event occurred.}
  @begin{short}
    Displays a menu and makes it available for selection.
  @end{short}

  Applications can use this function to display context-sensitive menus, and
  will typically supply @code{nil} for the @arg{parent-menu-shell},
  @arg{parent-menu-item}, and @arg{position-func} parameter. The default menu
  positioning function will position the menu at the current mouse cursor
  position.

  The @arg{button} parameter should be the mouse button pressed to initiate the
  menu popup. If the menu popup was initiated by something other than a mouse
  button press, such as a mouse button release or a keypress, @arg{button}
  should be 0.

  The @arg{activate-time} parameter is used to conflict-resolve initiation of
  concurrent requests for mouse/keyboard grab requests. To function properly,
  this needs to be the timestamp of the user event (such as a mouse click or
  key press) that caused the initiation of the popup. Only if no such event is
  available, the @fun{gtk-get-current-event-time} function can be used instead.
  @see-function{gtk-get-current-event-time}"
  (if position-func
      (with-stable-pointer (ptr position-func)
        (%gtk-menu-popup menu
                         parent-menu-shell
                         parent-menu-item
                         (callback gtk-menu-position-func-callback)
                         ptr
                         button
                         activate-time))
      (%gtk-menu-popup menu
                       parent-menu-shell
                       parent-menu-item
                       (null-pointer)
                       (null-pointer)
                       button
                       activate-time)))

(export 'gtk-menu-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_accel_group ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-accel-group))

(defun gtk-menu-set-accel-group (menu accel-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[accel-group]{the @class{gtk-accel-group} to be associated with the
    menu}
  Set the @class{gtk-accel-group} which holds global accelerators for the menu.
  This accelerator group needs to also be added to all windows that this menu is
  being used in with the @fun{gtk-window-add-accel-group} function, in order for
  those windows to support all the accelerators contained in this group.
  @see-function{gtk-window-add-accel-group}"
  (setf (gtk-menu-accel-group menu) accel-group))

(export 'gtk-menu-set-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_accel_group ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-accel-group))

(defun gtk-menu-get-accel-group (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @return{The @class{gtk-accel-group} associated with the menu.}
  Gets the @class{gtk-accel-group} which holds global accelerators for the menu.
  See the @fun{gtk-menu-set-accel-group} function.
  @see-function{gtk-menu-set-accel-group}"
  (gtk-menu-accel-group menu))

(export 'gtk-menu-get-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_accel_path ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-accel-path))

(defun gtk-menu-set-accel-path (menu accel-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a valid @class{gtk-menu} widget}
  @argument[accel-path]{a valid accelerator path}
  @begin{short}
    Sets an accelerator path for this menu from which accelerator paths for its
    immediate children, its menu items, can be constructed.
  @end{short}
  The main purpose of this function is to spare the programmer the inconvenience
  of having to call the @fun{gtk-menu-item-set-accel-path} function on each menu
  item that should support runtime user changable accelerators. Instead, by just
  calling the @fun{gtk-menu-set-accel-path} on their parent, each menu item of
  this menu, that contains a label describing its purpose, automatically gets an
  accel path assigned.

  For example, a menu containing menu items \"New\" and \"Exit\", will, after
  @code{gtk_menu_set_accel_path (menu, \"<Gnumeric-Sheet>/File\");} has been
  called, assign its items the accel paths: \"<Gnumeric-Sheet>/File/New\" and
  \"<Gnumeric-Sheet>/File/Exit\".

  Assigning accel paths to menu items then enables the user to change their
  accelerators at runtime. More details about accelerator paths and their
  default setups can be found at @fun{gtk-accel-map-add-entry}.

  Note that @arg{accel-path} string will be stored in a @type{g-quark}.
  Therefore, if you pass a static string, you can save some memory by interning
  it first with the @fun{g-intern-static-string} function.
  @see-function{gtk-menu-item-set-accel-path}
  @see-function{gtk-accel-map-add-entry}
  @see-function{g-intern-static-string}"
  (setf (gtk-menu-accel-path menu) accel-path))

(export 'gtk-menu-set-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_accel_path ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-accel-path))

(defun gtk-menu-get-accel-path (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a valid @class{gtk-menu} widget}
  @return{The accelerator path set on the menu.}
  @short{Retrieves the accelerator path set on the menu.}

  Since 2.14"
  (gtk-menu-accel-path menu))

(export 'gtk-menu-get-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_set_title" gtk-menu-set-title) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[title]{a string containing the title for the menu}
  @short{Sets the title string for the menu.}

  The title is displayed when the menu is shown as a tearoff menu. If title is
  @code{nil}, the menu will see if it is attached to a parent menu item, and if
  so it will try to use the same text as that menu item's label."
  (menu (g-object gtk-menu))
  (title :string))

(export 'gtk-menu-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_get_title" gtk-menu-get-title) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @begin{return}
    The title of the menu, or @code{nil} if the menu has no title set on it.
    This string is owned by GTK+ and should not be modified or freed.
  @end{return}
  Returns the title of the menu. See the @fun{gtk-menu-set-title} function."
  (menu (g-object gtk-menu)))

(export 'gtk-menu-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_monitor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-monitor))

(defun gtk-menu-set-monitor (menu monitor-num)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @code{gtk-menu} widget}
  @argument[monitor-num]{the number of the monitor on which the menu should be
    popped up}
  @begin{short}
    Informs GTK+ on which monitor a menu should be popped up. See the
    @fun{gdk-screen-get-monitor-geometry} function.
  @end{short}

  This function should be called from a @code{GtkMenuPositionFunc} if the menu
  should not appear on the same monitor as the pointer. This information cannot
  be reliably inferred from the coordinates returned by a
  @code{GtkMenuPositionFunc}, since, for very long menus, these coordinates may
  extend beyond the monitor boundaries or even the screen boundaries.

  Since 2.4
  @see-function{gdk-screen-get-monitor-geometry}"
  (setf (gtk-menu-monitor menu) monitor-num))

(export 'gtk-menu-set-monitor)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_monitor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-monitor))

(defun gtk-menu-get-monitor (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @begin{return}
    The number of the monitor on which the menu should be popped up or -1,
    if no monitor has been set.
  @end{return}
  @short{Retrieves the number of the monitor on which to show the menu.}

  Since 2.14"
  (gtk-menu-monitor menu))

(export 'gtk-menu-get-monitor)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_tearoff_state ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-tearoff-state))

(defun gtk-menu-get-tearoff-state (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @return{@em{True} if the @arg{menu} is currently torn off.}
  @begin{short}
    Returns whether the @arg{menu} is torn off.
  @end{short}
  See the @fun{gtk-menu-set-tearoff-state} function."
  (gtk-menu-tearoff-state menu))

(export 'gtk-menu-get-tearoff-state)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_reserve_toggle_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-reserve-toggle-size))

(defun gtk-menu-set-reserve-toggle-size (menu reserve-toggle-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[reserve-toggle-size]{whether to reserve size for toggles}
  @begin{short}
    Sets whether the @arg{menu} should reserve space for drawing toggles or
    icons, regardless of their actual presence.
  @end{short}

  Since 2.18"
  (setf (gtk-menu-reserve-toggle-size menu) reserve-toggle-size))

(export 'gtk-menu-set-reserve-toggle-size)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_reserve_toggle_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-reserve-toggle-size))

(defun gtk-menu-get-reserve-toggle-size (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @return{Whether the menu reserves toggle space.}
  @begin{short}
    Returns whether the @arg{menu} reserves space for toggles and icons,
    regardless of their actual presence.
  @end{short}

  Since 2.18"
  (gtk-menu-reserve-toggle-size menu))

(export 'gtk-menu-get-reserve-toggle-size)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popdown ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popdown" gtk-menu-popdown) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  Removes the @arg{menu} from the screen."
  (menu g-object))

(export 'gtk-menu-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reposition ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reposition" gtk-menu-reposition) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  Repositions the @arg{menu} according to its position function."
  (menu g-object))

(export 'gtk-menu-reposition)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-get-active))

(defun gtk-menu-get-active (menu)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @begin{return}
    The @class{gtk-menu-item} that was last selected in the @arg{menu}. If a
    selection has not yet been made, the first menu item is selected.
  @end{return}
  Returns the selected menu item from the @arg{menu}. This is used by the
  @class{gtk-combo-box} widget."
  (gtk-menu-active menu))

(export 'gtk-menu-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-active))

(defun gtk-menu-set-active (menu index)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[index]{the index of the @arg{menu} item to select. Index values are
    from 0 to n-1.}
  Selects the specified menu item within the @arg{menu}. This is used by the
  @class{gtk-combo-box} widget and should not be used by anyone else."
  (setf (gtk-menu-active menu) index))

(export 'gtk-menu-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_tearoff_state ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-set-tearoff-state))

(defun gtk-menu-set-tearoff-state (menu torn-off)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[torn-off]{if @em{true}, @arg{menu} is displayed as a tearoff menu}
  Changes the tearoff state of the @arg{menu}. A menu is normally displayed as
  drop down menu which persists as long as the @arg{menu} is active. It can also
  be displayed as a tearoff menu which persists until it is closed or
  reattached."
  (setf (gtk-menu-tearoff-state menu) torn-off))

(export 'gtk-menu-set-tearoff-state)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach_to_widget ()
;;;
;;; void gtk_menu_attach_to_widget (GtkMenu *menu,
;;;                                 GtkWidget *attach_widget,
;;;                                 GtkMenuDetachFunc detacher);
;;;
;;; Attaches the menu to the widget and provides a callback function that will
;;; be invoked when the menu calls gtk_menu_detach() during its destruction.
;;;
;;; menu :
;;;     a GtkMenu
;;;
;;; attach_widget :
;;;     the GtkWidget that the menu will be attached to
;;;
;;; detacher :
;;;     the user supplied callback function that will be called when the menu
;;;     calls gtk_menu_detach()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_detach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_detach" gtk-menu-detach) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menu]{a @class{gtk-menu} widget}
  Detaches the menu from the widget to which it had been attached. This
  function will call the callback function, detacher, provided when the
  @fun{gtk-menu-attach-to-widget} function was called.
  @see-function{gtk-menu-attach-to-widget}"
  (menu (g-object menu)))

(export 'gtk-menu-detach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_attach_widget ()
;;;
;;; GtkWidget * gtk_menu_get_attach_widget (GtkMenu *menu);
;;;
;;; Returns the GtkWidget that the menu is attached to.
;;;
;;; menu :
;;;     a GtkMenu
;;;
;;; Returns :
;;;     the GtkWidget that the menu is attached to
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_for_attach_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_get_for_attach_widget" gtk-menu-get-for-attach-widget)
    (g-list (g-object gtk-menu) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[widget]{a @class{gtk-widget} widget}
  @return{The list of menus attached to this @arg{widget}.}
  @begin{short}
    Returns a list of the menus which are attached to this @arg{widget}. This
    list is owned by GTK+ and must not be modified.
  @end{short}

  Since 2.6"
  (width (g-object gtk-widget)))

(export 'gtk-menu-get-for-attach-widget)

;;; ----------------------------------------------------------------------------
;;; GtkMenuPositionFunc ()
;;;
;;; void (*GtkMenuPositionFunc) (GtkMenu *menu,
;;;                              gint *x,
;;;                              gint *y,
;;;                              gboolean *push_in,
;;;                              gpointer user_data);
;;;
;;; A user function supplied when calling gtk_menu_popup() which controls the
;;; positioning of the menu when it is displayed. The function sets the x and y
;;; parameters to the coordinates where the menu is to be drawn. To make the
;;; menu appear on a different monitor than the mouse pointer,
;;; gtk_menu_set_monitor() must be called.
;;;
;;; menu :
;;;     a GtkMenu.
;;;
;;; x :
;;;     address of the gint representing the horizontal position where the menu
;;;     shall be drawn
;;;
;;; y :
;;;     address of the gint representing the vertical position where the menu
;;;     shall be drawn. This is an output parameter
;;;
;;; push_in :
;;;     This parameter controls how menus placed outside the monitor are
;;;     handled. If this is set to TRUE and part of the menu is outside the
;;;     monitor then GTK+ pushes the window into the visible area, effectively
;;;     modifying the popup position. Note that moving and possibly resizing the
;;;     menu around will alter the scroll position to keep the menu items "in
;;;     place", i.e. at the same monitor position they would have been without
;;;     resizing. In practice, this behavior is only useful for combobox popups
;;;     or option menus and cannot be used to simply confine a menu to monitor
;;;     boundaries. In that case, changing the scroll offset is not desirable.
;;;
;;; user_data :
;;;     the data supplied by the user in the gtk_menu_popup() data parameter.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkMenuDetachFunc ()
;;;
;;; void (*GtkMenuDetachFunc) (GtkWidget *attach_widget, GtkMenu *menu);
;;;
;;; A user function supplied when calling gtk_menu_attach_to_widget() which will
;;; be called when the menu is later detached from the widget.
;;;
;;; attach_widget :
;;;     the GtkWidget that the menu is being detached from.
;;;
;;; menu :
;;;     the GtkMenu being detached.
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.menu.lisp ----------------------------------------------

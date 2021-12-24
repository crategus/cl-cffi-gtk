;;; ----------------------------------------------------------------------------
;;; gtk.menu.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     A menu widget
;;;
;;; Types and Values
;;;
;;;     GtkArrowPlacement
;;;     GtkMenu
;;;
;;; Functions
;;;
;;;     GtkMenuPositionFunc
;;;     GtkMenuDetachFunc
;;;
;;;     gtk_menu_new
;;;     gtk_menu_new_from_model
;;;     gtk_menu_set_screen
;;;     gtk_menu_reorder_child
;;;     gtk_menu_attach
;;;     gtk_menu_popup_at_rect
;;;     gtk_menu_popup_at_widget
;;;     gtk_menu_popup_at_pointer
;;;     gtk_menu_popup_for_device
;;;     gtk_menu_popup
;;;     gtk_menu_set_accel_group                           Accessor
;;;     gtk_menu_get_accel_group                           Accessor
;;;     gtk_menu_set_accel_path                            Accessor
;;;     gtk_menu_get_accel_path                            Accessor
;;;     gtk_menu_set_title                                 Accessor
;;;     gtk_menu_get_title                                 Accessor
;;;     gtk_menu_set_monitor                               Accessor
;;;     gtk_menu_get_monitor                               Accessor
;;;     gtk_menu_place_on_monitor
;;;     gtk_menu_get_tearoff_state                         Accessor
;;;     gtk_menu_set_reserve_toggle_size                   Accessor
;;;     gtk_menu_get_reserve_toggle_size                   Accessor
;;;     gtk_menu_popdown
;;;     gtk_menu_reposition
;;;     gtk_menu_get_active                                Accessor
;;;     gtk_menu_set_active                                Accessor
;;;     gtk_menu_set_tearoff_state                         Accessor
;;;     gtk_menu_attach_to_widget
;;;     gtk_menu_detach
;;;     gtk_menu_get_attach_widget                         Accessor
;;;     gtk_menu_get_for_attach_widget
;;;
;;; Properties
;;;
;;;         GtkAccelGroup*   accel-group            Read / Write
;;;                 gchar*   accel-path             Read / Write
;;;                  gint    active                 Read / Write
;;;        GdkAnchorHints    anchor-hints           Read / Write / Construct
;;;             GtkWidget*   attach-widget          Read / Write
;;;     GdkWindowTypeHint    menu-type-hint         Read / Write / Construct
;;;                  gint    monitor                Read / Write
;;;                  gint    rect-anchor-dx         Read / Write / Construct
;;;                  gint    rect-anchor-dy         Read / Write / Construct
;;;              gboolean    reserve-toggle-size    Read / Write
;;;              gboolean    tearoff-state          Read / Write
;;;                 gchar*   tearoff-title          Read / Write
;;;
;;; Child Properties
;;;
;;;                  gint    bottom-attach          Read / Write
;;;                  gint    left-attach            Read / Write
;;;                  gint    right-attach           Read / Write
;;;                  gint    top-attach             Read / Write
;;;
;;; Style Properties
;;;
;;;     GtkArrowPlacement    arrow-placement        Read
;;;                gfloat    arrow-scaling          Read
;;;              gboolean    double-arrows          Read
;;;                  gint    horizontal-offset      Read
;;;                  gint    horizontal-padding     Read
;;;                  gint    vertical-offset        Read
;;;                  gint    vertical-padding       Read
;;;
;;; Signals
;;;
;;;                  void    move-scroll            Action
;;;                  void    popped-up              Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkMenuShell
;;;                     ╰── GtkMenu
;;;                         ╰── GtkRecentChooserMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenu implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkArrowPlacement
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkArrowPlacement" gtk-arrow-placement
  (:export t
   :type-initializer "gtk_arrow_placement_get_type")
  (:both 0)
  (:start 1)
  (:end 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-placement atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-arrow-placement atdoc:*external-symbols*)
 "@version{2021-11-14}
  @begin{short}
    Used to specify the placement of scroll arrows in scrolling menus.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkArrowPlacement\" gtk-arrow-placement
  (:export t
   :type-initializer \"gtk_arrow_placement_get_type\")
  (:both 0)
  (:start 1)
  (:end 2))
  @end{pre}
  @begin[code]{table}
    @entry[:both]{Place one arrow on each end of the menu.}
    @entry[:start]{Place both arrows at the top of the menu.}
    @entry[:end]{Place both arrows at the bottom of the menu.}
  @end{table}
  @see-class{gtk-menu}")

;;; ----------------------------------------------------------------------------
;;; struct GtkMenu
;;; ----------------------------------------------------------------------------

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
   #+gtk-3-22
   (anchor-hints
    gtk-menu-anchor-hints
    "anchor-hints" "GdkAnchorHints" t t)
   (attach-widget
    gtk-menu-attach-widget
    "attach-widget" "GtkWidget" t t)
   #+gtk-3-22
   (menu-type-hint
    gtk-menu-menu-type-hint
    "menu-type-hint" "GdkWindowTypeHint" t t)
   (monitor
    gtk-menu-monitor
    "monitor" "gint" t t)
   #+gtk-3-22
   (rect-anchor-dx
    gtk-menu-rect-anchor-dx
    "rect-anchor-dx" "gint" t t)
   #+gtk-3-22
   (rect-anchor-dy
    gtk-menu-rect-anchor-dy
    "rect-anchor-dy" "gint" t t)
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
 "@version{*2021-11-14}
  @begin{short}
    A @sym{gtk-menu} widget is a @class{gtk-menu-shell} widget that implements
    a drop down menu consisting of a list of @class{gtk-menu-item} widgets
    which can be navigated and activated by the user to perform application
    functions.
  @end{short}

  A @sym{gtk-menu} widget is most commonly dropped down by activating a
  @class{gtk-menu-item} widget in a @class{gtk-menu-bar} widget or popped up by
  activating a @class{gtk-menu-item} widget in another @sym{gtk-menu} widget.

  A @sym{gtk-menu} widget can also be popped up by activating a
  @class{gtk-combo-box} widget. Other composite widgets such as the
  @class{gtk-notebook} widget can pop up a @sym{gtk-menu} widget as well.

  Applications can display a @sym{gtk-menu} widget as a popup menu by calling
  the @fun{gtk-menu-popup-at-pointer} function. The example below shows how an
  application can pop up a menu when a mouse button is pressed.
  @begin[Example]{dictionary}
    Example with a signal handler which displays a popup menu.
    @begin{pre}
(defun example-menu-popup (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :default-width 300
                                 :default-height 180
                                 :title \"Example Popup Menu\"))
          (button (gtk-button-new-with-label \"Click me\")))
      ;; Create pop-up menu for button
      (let ((popup (make-instance 'gtk-menu))
            (bigitem (gtk-menu-item-new-with-label \"Larger\"))
            (smallitem (gtk-menu-item-new-with-label \"Smaller\")))
        (gtk-menu-shell-append popup bigitem)
        (gtk-menu-shell-append popup smallitem)
        (gtk-widget-show-all popup)
        ;; Signal handler to pop up the menu
        (g-signal-connect button \"button-press-event\"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk-menu-popup-at-pointer popup event)
             t)))
      (g-signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
menu
├── arrow.top
├── <child>
┊
├── <child>
╰── arrow.bottom
    @end{pre}
    The main CSS node of the @sym{gtk-menu} implemenation has name @code{menu},
    and there are two subnodes with name @code{arrow}, for scrolling menu
    arrows. These subnodes get the @code{.top} and @code{.bottom} style classes.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[bottom-attach]{entry}
        The @code{bottom-attach} child property of type @code{:int}
        (Read / Write) @br{}
        The row number to attach the bottom of the child to. @br{}
        Allowed values: >= -1 @br{}
        Default value: -1
      @end{entry}
      @begin[left-attach]{entry}
        The @code{left-attach} child property of type @code{:int}
        (Read / Write) @br{}
        The column number to attach the left side of the child to. @br{}
        Allowed values: >= -1 @br{}
        Default value: -1
      @end{entry}
      @begin[right-attach]{entry}
        The @code{right-attach} child property of type @code{:int}
        (Read / Write) @br{}
        The column number to attach the right side of the child to. @br{}
        Allowed values: >= -1 @br{}
        Default value: -1
      @end{entry}
      @begin[top-attach]{entry}
        The @code{top-attach} child property of type @code{:int}
        (Read / Write) @br{}
        The row number to attach the top of the child to. @br{}
        Allowed values: >= -1 @br{}
        Default value: -1
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-placement]{entry}
        The @code{arrow-placement} style property of type
        @symbol{gtk-arrow-placement} (Read) @br{}
        Indicates where scroll arrows should be placed. @br{}
        @em{Warning:} The @code{arrow-placement} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Default value: @code{:boths}
      @end{entry}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of type @code{:float}
        (Read) @br{}
        Arbitrary constant to scale down the size of the scroll arrow. @br{}
        @em{Warning:} The @code{arrow-scaling} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard min-width/min-height CSS properties on the arrow
        node. The value of this style property is ignored. @br{}
        Allowed values: [0,1] @br{}
        Default value: 0.7
      @end{entry}
      @begin[double-arrows]{entry}
        The @code{double-arrows} style property of type @code{:boolean}
        (Read) @br{}
        When scrolling, always show both arrows. @br{}
        @em{Warning:} The @code{double-arrows} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[horizontal-offset]{entry}
        The @code{horizontal-offset} style property of type @code{:int}
        (Read) @br{}
        When the menu is a submenu, position it this number of pixels offset
        horizontally. @br{}
        Default value: -2
      @end{entry}
      @begin[horizontal-padding]{entry}
        The @code{horizontal-padding} style property of type @code{:int}
        (Read) @br{}
        Extra space at the left and right edges of the menu. @br{}
        @em{Warning:} The @code{horizontal-padding} style property has been
        deprecated since version 3.8 and should not be used in newly written
        code. Use the standard padding CSS property, through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}. The value of
        this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0
      @end{entry}
      @begin[vertical-offset]{entry}
        The @code{vertical-offset} style property of type @code{:int}
        (Read) @br{}
        When the menu is a submenu, position it this number of pixels offset
        vertically. @br{}
        Default value: 0
      @end{entry}
      @begin[vertical-padding]{entry}
        The @code{vertical-padding} style property of type @code{:int}
        (Read) @br{}
        Extra space at the top and bottom of the menu. @br{}
        @em{Warning:} The @code{vertical-padding} style property has been
        deprecated since version 3.8 and should not be used in newly written
        code. Use the standard padding CSS property, through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}. The value of
        this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"move-scroll\" signal}
      @begin{pre}
 lambda (menu scrolltype)    :action
      @end{pre}
      @begin[code]{table}
        @entry[menu]{A @sym{gtk-menu} widget.}
        @entry[scrolltype]{A value of the @symbol{gtk-scroll-type} enumeration.}
      @end{table}
    @subheading{The \"popped-up\" signal}
      @begin{pre}
 lambda (menu flipped final xflipped yflipped)    :run-first
      @end{pre}
      Emitted when the position of the menu is finalized after being popped up
      using the @fun{gtk-menu-popup-at-rect}, @fun{gtk-menu-popup-at-widget},
      or @fun{gtk-menu-popup-at-pointer} functions.

      The menu might be flipped over the anchor rectangle in order to keep it
      on-screen, in which case the @arg{xflipped} and @arg{yflipped} arguments
      will be set to @em{true} accordingly.

      The @arg{flipped} argument is the ideal position of the menu after any
      possible flipping, but before any possible sliding. The @arg{final}
      argument is @arg{flipped}, but possibly translated in the case that
      flipping is still ineffective in keeping menu on-screen.

      @image[popup-slide]{}

      The blue menu is the ideal position of the menu, the green menu is
      @arg{flipped}, and the red menu is @arg{final}.

      See the @fun{gtk-menu-popup-at-rect}, @fun{gtk-menu-popup-at-widget},
      @fun{gtk-menu-popup-at-pointer} functions, and the
      @slot[gtk-menu]{anchor-hints}, @slot[gtk-menu]{rect-anchor-dx},
      @slot[gtk-menu]{rect-anchor-dy}, and @slot[gtk-menu]{menu-type-hint}
      properties.

      Since 3.22
      @begin[code]{table}
        @entry[menu]{A @sym{gtk-menu} widget that popped up.}
        @entry[flipped]{The position of @arg{menu} after any possible flipping
          or @code{nil} if the backend can not obtain it.}
        @entry[final]{The final position of @arg{menu} or @code{nil} if the
          backend can not obtain it.}
        @entry[xflipped]{@em{True} if the anchors were flipped horizontally.}
        @entry[yflipped]{@em{True} if the anchors were flipped vertically.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-menu-accel-group}
  @see-slot{gtk-menu-accel-path}
  @see-slot{gtk-menu-active}
  @see-slot{gtk-menu-anchor-hints}
  @see-slot{gtk-menu-attach-widget}
  @see-slot{gtk-menu-menu-type-hint}
  @see-slot{gtk-menu-monitor}
  @see-slot{gtk-menu-rect-anchor-dx}
  @see-slot{gtk-menu-rect-anchor-dy}
  @see-slot{gtk-menu-reserve-toggle-size}
  @see-slot{gtk-menu-tearoff-state}
  @see-slot{gtk-menu-tearoff-title}
  @see-class{gtk-menu-shell}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-menu-accel-group ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group" 'gtk-menu) 't)
 "The @code{accel-group} property of type @class{gtk-accel-group} (Read / Write)
  @br{}
  The accelerator group holding accelerators for the menu.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-accel-group 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-accel-group object) => group}
  @syntax[]{(setf (gtk-menu-accel-group object) group)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[group]{the @class{gtk-accel-group} object to be associated with the
    menu}
  @begin{short}
    Accessor of the @slot[gtk-menu]{accel-group} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @sym{gtk-menu-accel-group} slot access function gets the accelerator group
  which holds global accelerators for the menu. The
  @sym{(setf gtk-menu-accel-group)} slot access function sets the accelerator
  group.

  This accelerator group needs to also be added to all windows that this menu
  is being used in with the @fun{gtk-window-add-accel-group} function, in order
  for those windows to support all the accelerators contained in this group.
  @see-class{gtk-menu}
  @see-class{gtk-accel-group}
  @see-function{gtk-window-add-accel-group}")

;;; --- gtk-menu-accel-path ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-path" 'gtk-menu) 't)
 "The @code{accel-path} property of type @code{:string} (Read / Write) @br{}
  An accel path used to conveniently construct accel paths of child items. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-accel-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-accel-path 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-accel-path object) => path}
  @syntax[]{(setf (gtk-menu-accel-path object) path)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[path]{a string with a valid accelerator path}
  @begin{short}
    Accessor of the @slot[gtk-menu]{accel-path} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @sym{gtk-menu-accel-path} slot access function retrieves the accelerator
  path for this menu from which accelerator paths for its immediate children,
  its menu items, can be constructed. The @sym{(setf gtk-menu-accel-path)} slot
  access function sets an accelerator path.

  The main purpose of this function is to spare the programmer the inconvenience
  of having to call the @fun{gtk-menu-item-accel-path} function on each menu
  item that should support runtime user changable accelerators. Instead, by just
  calling the @sym{gtk-menu-accel-path} function on their parent, each menu item
  of this menu, that contains a label describing its purpose, automatically
  gets an accel path assigned.

  For example, a menu containing menu items \"New\" and \"Exit\", will, after
  @begin{pre}
(setf (gtk-menu-accel-path menu) \"<Gnumeric-Sheet>/File\")
  @end{pre}
  has been called, assign its items the accel paths:
  \"<Gnumeric-Sheet>/File/New\" and \"<Gnumeric-Sheet>/File/Exit\".

  Assigning accel paths to menu items then enables the user to change their
  accelerators at runtime. More details about accelerator paths and their
  default setups can be found at the documentation for the
  @fun{gtk-accel-map-add-entry} function.
  @see-class{gtk-menu}
  @see-function{gtk-menu-item-accel-path}
  @see-function{gtk-accel-map-add-entry}")

;;; --- gtk-menu-active --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-menu) 't)
 "The @code{active} property of type @code{:int} (Read / Write) @br{}
  The index of the currently selected menu item, or -1 if no menu item is
  selected. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-active 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-active object) => active}
  @syntax[]{(setf (gtk-menu-active object) active)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[active]{an integer with the index of the currently selected menu
    item}
  @begin{short}
    Accessor of the @slot[gtk-menu]{active} slot of the @class{gtk-menu} class.
  @end{short}

  The @sym{gtk-menu-active} slot access function returns the currently selected
  menu item from the menu, or -1 if no menu item is selected. The
  @sym{(setf gtk-menu-active)} slot access function selects the specified menu
  item. This is used by the @class{gtk-combo-box} widget and should not be used
  by anyone else.
  @see-class{gtk-menu}
  @see-class{gtk-combo-box}")

;;; --- gtk-menu-anchor-hints --------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "anchor-hints" 'gtk-menu) 't)
 "The @code{anchor-hints} property of type @symbol{gdk-anchor-hints}
  (Read / Write / Construct) @br{}
  Positioning hints for aligning the menu relative to a rectangle. These hints
  determine how the menu should be positioned in the case that the menu would
  fall off-screen if placed in its ideal position. Since 3.22 @br{}
  Default value: @code{'(:flip-x :flip-y :slide-x :slide-y :resize-x
                         :resize-y)}")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-menu-anchor-hints atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-anchor-hints 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-anchor-hints object) => hints}
  @syntax[]{(setf (gtk-menu-anchor-hints object) hints)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[hints]{a value of the @symbol{gdk-anchor-hints} flags}
  @begin{short}
    Accessor of the @slot[gtk-menu]{anchor-hints} slot of the @class{gtk-menu}
    class.
  @end{short}

  Positioning hints for aligning the menu relative to a rectangle. These hints
  determine how the menu should be positioned in the case that the menu would
  fall off-screen if placed in its ideal position.

  @image[popup-flip]{}

  For example, @code{:flip-y} will replace @code{:north-west} with
  @code{:south-west} and vice versa if the menu extends beyond the bottom edge
  of the monitor. See the @fun{gtk-menu-popup-at-rect},
  @fun{gtk-menu-popup-at-widget}, @fun{gtk-menu-popup-at-pointer} functions,
  the @slot[gtk-menu]{rect-anchor-dx}, @slot[gtk-menu]{rect-anchor-dy},
  @slot[gtk-menu]{menu-type-hint} properties, and the \"popped-up\" signal.

  Since 3.22
  @see-class{gtk-menu}
  @see-symbol{gdk-anchor-hints}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-rect-anchor-dx}
  @see-function{gtk-menu-rect-anchor-dy}
  @see-function{gtk-menu-menu-type-hint}")

;;; --- gtk-menu-attach-widget -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attach-widget" 'gtk-menu) 't)
 "The @code{attach-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  The widget the menu is attached to. Setting this property attaches the menu
  without a @code{GtkMenuDetachFunc} callback function. If you need to use a
  detacher, use the @fun{gtk-menu-attach-to-widget} function directly.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-attach-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-attach-widget 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-attach-widget object) => widget}
  @syntax[]{(setf (gtk-menu-attach-widget object) widget)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[widget]{a @class{gtk-widget} object}
  @begin{short}
    Accessor of the @slot[gtk-menu]{attach-widget} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @sym{gtk-menu-attach-widget} slot access function returns the widget that
  the menu is attached to. The @sym{(setf gtk-menu-attach-widget)} slot access
  function sets the widget.

  Setting this property attaches the menu without a call of the
  @code{GtkMenuDetachFunc} callback function. If you need to use a detacher, use
  the @fun{gtk-menu-attach-to-widget} function directly.
  @see-class{gtk-menu}
  @see-class{gtk-widget}
  @see-function{gtk-menu-attach-to-widget}")

;;; --- gtk-menu-menu-type-hint ------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "menu-type-hint" 'gtk-menu) 't)
 "The @code{menu-type-hint} property of type @symbol{gdk-window-type-hint}
  (Read / Write / Construct) @br{}
  The @symbol{gdk-window-type-hint} value to use for the @class{gdk-window}
  object of the menu. Since 3.22 @br{}
  Default value: @code{:popup-menu}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-menu-type-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-menu-type-hint 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-menu-type-hint object) => hint}
  @syntax[]{(setf (gtk-menu-menu-type-hint object) hint)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[hint]{a value of the @symbol{gdk-window-type-hint} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-menu]{menu-type-hint} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @symbol{gdk-window-type-hint} value to use for the @class{gdk-window}
  object of the menu. See the @fun{gtk-menu-popup-at-rect},
  @fun{gtk-menu-popup-at-widget}, @fun{gtk-menu-popup-at-pointer} functions,
  the @slot[gtk-menu]{anchor-hints}, @slot[gtk-menu]{rect-anchor-dx},
  @slot[gtk-menu]{rect-anchor-dy} properties, and the \"popped-up\" signal.

  Since 3.22
  @see-class{gtk-menu}
  @see-class{gdk-window}
  @see-symbol{gdk-window-type-hint}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-anchor-hints}
  @see-function{gtk-menu-rect-anchor-dx}
  @see-function{gtk-menu-rect-anchor-dy}")

;;; --- gtk-menu-monitor -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "monitor" 'gtk-menu) 't)
 "The @code{monitor} property of type @code{:int} (Read / Write) @br{}
  The monitor the menu will be popped up on. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-monitor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-monitor 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-monitor object) => num}
  @syntax[]{(setf (gtk-menu-monitor object) num)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[num]{an integer with the number of the monitor on which
    the menu should be popped up}
  @begin{short}
    Accessor of the @slot[gtk-menu]{monitor} slot of the @class{gtk-menu} class.
  @end{short}

  The @sym{gtk-menu-monitor} slot access function retrieves the number of
  the monitor on which to show the menu. The @sym{(setf gtk-menu-monitor)} slot
  access function sets the monitor.

  This function should be called from a @code{GtkMenuPositionFunc} callback
  function if the menu should not appear on the same monitor as the pointer.
  This information cannot be reliably inferred from the coordinates returned by
  a @code{GtkMenuPositionFunc} callback function, since, for very long menus,
  these coordinates may extend beyond the monitor boundaries or even the screen
  boundaries.
  @see-class{gtk-menu}")

;;; --- gtk-menu-rect-anchor-dx ------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "rect-anchor-dx" 'gtk-menu) 't)
 "The @code{rect-anchor-dx} property of type @code{:int}
  (Read / Write / Construct) @br{}
  Horizontal offset to apply to the menu, i.e. the rectangle or widget anchor.
  Since 3.22 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-rect-anchor-dx atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-rect-anchor-dx 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-rect-anchor-dx object) => anchor}
  @syntax[]{(setf (gtk-menu-rect-anchor-dx object) anchor)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[anchor]{an integer with the horizontal offset to apply to the menu}
  @begin{short}
    Accessor of the @slot[gtk-menu]{rect-anchor-dx} slot of the
    @class{gtk-menu} class.
  @end{short}

  Horizontal offset to apply to the menu, i.e. the rectangle or widget anchor.
  See the @fun{gtk-menu-popup-at-rect}, @fun{gtk-menu-popup-at-widget},
  @fun{gtk-menu-popup-at-pointer} functions, the @slot[gtk-menu]{anchor-hints},
  @slot[gtk-menu]{rect-anchor-dx}, @slot[gtk-menu]{rect-anchor-dy} properties,
  and the \"popped-up\" signal.

  Since 3.22
  @see-class{gtk-menu}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-anchor-hints}
  @see-function{gtk-menu-rect-anchor-dx}
  @see-function{gtk-menu-rect-anchor-dy}")

;;; --- gtk-menu-rect-anchor-dy ------------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "rect-anchor-dy" 'gtk-menu) 't)
 "The @code{rect-anchor-dy} property of type @code{:int}
  (Read / Write / Construct) @br{}
  Vertical offset to apply to the menu, i.e. the rectangle or widget anchor.
  Since 3.22 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-rect-anchor-dy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-rect-anchor-dy 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-rect-anchor-dy object) => anchor}
  @syntax[]{(setf (gtk-menu-rect-anchor-dy object) anchor)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[anchor]{an integer with the vertical offset to apply to the menu}
  @begin{short}
    Accessor of the @slot[gtk-menu]{rect-anchor-dy} slot of the
    @class{gtk-menu} class.
  @end{short}

  Vertical offset to apply to the menu, i.e. the rectangle or widget anchor.

  See the @fun{gtk-menu-popup-at-rect}, @fun{gtk-menu-popup-at-widget},
  @fun{gtk-menu-popup-at-pointer} functions, the @slot[gtk-menu]{anchor-hints},
  @slot[gtk-menu]{rect-anchor-dx}, @slot[gtk-menu]{rect-anchor-dy} properties,
  and the \"popped-up\" signal.

  Since 3.22
  @see-class{gtk-menu}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-anchor-hints}
  @see-function{gtk-menu-rect-anchor-dx}
  @see-function{gtk-menu-rect-anchor-dy}")

;;; --- gtk-menu-reserve-toggle-size -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reserve-toggle-size"
                                               'gtk-menu) 't)
 "The @code{reserve-toggle-size} property of type @code{:boolean} (Read / Write)
  @br{}
  A boolean that indicates whether the menu reserves space for toggles and
  icons, regardless of their actual presence. This property should only be
  changed from its default value for special purposes such as tabular menus.
  Regular menus that are connected to a menu bar or context menus should
  reserve toggle space for consistency. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-reserve-toggle-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-reserve-toggle-size 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-reserve-toggle-size object) => reserve}
  @syntax[]{(setf (gtk-menu-reserve-toggle-size object) reserve)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[reserve]{a boolean whether to reserve size for toggles}
  @begin{short}
    Accessor of the @slot[gtk-menu]{reserve-toggle-size} slot of the
    @class{gtk-menu} class.
  @end{short}

  The @sym{gtk-menu-reserve-toggle-size} slot access function returns whether
  the menu reserves space for toggles and icons, regardless of their actual
  presence. The @sym{(setf gtk-menu-reserve-toggle-size)} slot access function
  sets whether the menu should reserve space.

  This property should only be changed from its default value for
  special purposes such as tabular menus. Regular menus that are connected to
  a menu bar or context menus should reserve toggle space for consistency.
  @see-class{gtk-menu}")

;;; --- gtk-menu-tearoff-state -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-state" 'gtk-menu) 't)
 "The @code{tearoff-state} property of type @code{:boolean} (Read / Write) @br{}
  A boolean that indicates whether the menu is torn-off. @br{}
  @em{Warning:} The @code{tearoff-state} property has been deprecated since
  version 3.10 and should not be used in newly written code. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-tearoff-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-tearoff-state 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-tearoff-state object) => state}
  @syntax[]{(setf (gtk-menu-tearoff-state object) state)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[state]{if @em{true}, @arg{menu} is displayed as a tearoff menu}
  @begin{short}
    Accessor of the @slot[gtk-menu]{tearoff-state} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @sym{gtk-menu-tearoff-state} slot access function returns whether the menu
  is torn off. The @sym{(setf gtk-menu-tearoff-state)} slot access function
  changes the tearoff state of the menu.

  A menu is normally displayed as drop down menu which persists as long as the
  menu is active. It can also be displayed as a tearoff menu which persists
  until it is closed or reattached.
  @begin[Warning]{dictionary}
    The @sym{gtk-menu tearoff-state} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-menu}")

;;; --- gtk-menu-tearoff-title -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-title" 'gtk-menu) 't)
 "The @code{tearoff-title} property of type @code{:string} (Read / Write) @br{}
  A title that may be displayed by the window manager when this menu is
  torn-off. @br{}
  @em{Warning:} The @code{tearoff-title} property has been deprecated since
  version 3.10 and should not be used in newly written code. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-tearoff-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-tearoff-title 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-tearoff-title object) => title}
  @syntax[]{(setf (gtk-menu-tearoff-title object) title)}
  @argument[object]{a @class{gtk-menu} widget}
  @argument[title]{a string containing the title for the menu}
  @begin{short}
    Accessor of the @slot[gtk-menu]{tearoff-title} slot of the @class{gtk-menu}
    class.
  @end{short}

  The @sym{gtk-menu-tearoff-title} slot access function returns the title of
  the menu. The @sym{(setf gtk-menu-tearoff-title)} slot access function sets
  the title.

  The title is displayed when the menu is shown as a tearoff menu. If the
  @arg{title} argument is @code{nil}, the menu will see if it is attached to a
  parent menu item, and if so it will try to use the same text as that menu
  label of the item.
  @begin[Warning]{dictionary}
    The @sym{gtk-menu-tearoff-title} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-menu}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkMenu" 'gtk-menu))

;;; --- gtk-menu-child-bottom-attach -------------------------------------------

(define-child-property "GtkMenu"
                       gtk-menu-child-bottom-attach
                       "bottom-attach" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-bottom-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-bottom-attach 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-child-bottom-attach container child) => attach}
  @syntax[]{(setf (gtk-menu-child-bottom-attach container child) attach)}
  @argument[container]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[attach]{an integer with the row number}
  @begin{short}
    Accessor of the @code{bottom-attach} child property of the
    @class{gtk-menu} class.
  @end{short}

  The row number to attach the bottom of the child to.
  @see-class{gtk-menu}
  @see-class{gtk-widget}")

;;; --- gtk-menu-child-left-attach ---------------------------------------------

(define-child-property "GtkMenu"
                       gtk-menu-child-left-attach
                       "left-attach" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-left-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-left-attach 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-child-left-attach container child) => attach}
  @syntax[]{(setf (gtk-menu-child-left-attach container child) attach)}
  @argument[container]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[attach]{an integer with the column number}
  @begin{short}
    Accessor of the @code{left-attach} child property of the @class{gtk-menu}
    class.
  @end{short}

  The column number to attach the left side of the child to.
  @see-class{gtk-menu}
  @see-class{gtk-widget}")

;;; --- gtk-menu-child-right-attach --------------------------------------------

(define-child-property "GtkMenu"
                       gtk-menu-child-right-attach
                       "right-attach" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-right-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-right-attach 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-child-right-attach container child) => attach}
  @syntax[]{(setf (gtk-menu-child-right-attach container child) attach)}
  @argument[container]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[attach]{an integer with the column number}
  @begin{short}
    Accessor of the @code{right-attach} child property of the @class{gtk-menu}
    class.
  @end{short}

  The column number to attach the right side of the child to.
  @see-class{gtk-menu}
  @see-class{gtk-widget}")

;;; --- gtk-menu-child-top-attach ----------------------------------------------

(define-child-property "GtkMenu"
                       gtk-menu-child-top-attach
                       "top-attach" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-child-top-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-child-top-attach 'function)
 "@version{2021-11-14}
  @syntax[]{(gtk-menu-child-top-attach container child) => attach}
  @syntax[]{(setf (gtk-menu-child-top-attach container child) attach)}
  @argument[container]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[attach]{an integer with the row number}
  @begin{short}
    Accessor of the @code{top-attach} child property of the @class{gtk-menu}
    class.
  @end{short}

  The row number to attach the top of the child to.
  @see-class{gtk-menu}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-new))

(defun gtk-menu-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @return{A new @class{gtk-menu} widget.}
  @short{Creates a new menu.}
  @see-class{gtk-menu}"
  (make-instance 'gtk-menu))

(export 'gtk-menu-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_new_from_model ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_new_from_model" gtk-menu-new-from-model)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[model]{a @class{g-menu-model} object}
  @return{A new @class{gtk-menu} widget.}
  @begin{short}
    Creates a menu and populates it with menu items and submenus
    according to @arg{model}.
  @end{short}
  The created menu items are connected to actions found in the
  @class{gtk-application-window} widget to which the menu belongs - typically
  by means of being attached to a widget, see the
  @fun{gtk-menu-attach-to-widget} function, that is contained within the
  @class{gtk-application-window} widgets hierarchy.
  @see-class{gtk-menu}
  @see-class{g-menu-model}
  @see-class{gtk-application-window}
  @see-function{gtk-menu-attach-to-widget}"
  (model (g-object g-menu-model)))

(export 'gtk-menu-new-from-model)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_set_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_set_screen" gtk-menu-set-screen) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[screen]{a @class{gdk-screen} object, or @code{nil} if the screen
    should be determined by the widget the menu is attached to}
  @short{Sets the screen on which the menu will be displayed.}
  @see-class{gtk-menu}
  @see-class{gdk-screen}"
  (menu (g-object gtk-menu))
  (screen (g-object gdk-screen)))

(export 'gtk-menu-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reorder_child" gtk-menu-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[child]{the @class{gtk-menu-item} widget to move}
  @argument[position]{the new position to place child, positions are numbered
    from 0 to n - 1}
  @begin{short}
    Moves @arg{child} to a new position in the list of menu children.
  @end{short}
  @see-class{gtk-menu}
  @see-class{gtk-menu-item}"
  (menu (g-object gtk-menu))
  (child (g-object gtk-menu-item))
  (position :int))

(export 'gtk-menu-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_attach" gtk-menu-attach) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[child]{a @class{gtk-menu-item} widget}
  @argument[left]{an unsigned integer with the column number to attach the left
    side of the item to}
  @argument[right]{an unsigned integer with the column number to attach the
    right side of the item to}
  @argument[top]{an unsigned integer with the row number to attach the top of
    the item to}
  @argument[bottom]{an unsigned integer with the row number to attach the
    bottom of the item to}
  @begin{short}
    Adds a menu item to a (table) menu.
  @end{short}
  The number of cells that an item will occupy is specified by the
  @arg{left}, @arg{right}, @arg{top} and @arg{bottom} arguments. These each
  represent the leftmost, rightmost, uppermost and lower column and row numbers
  of the table. Columns and rows are indexed from zero.

  Note that this function is not related to the @fun{gtk-menu-detach} function.
  @see-class{gtk-menu}
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-detach}"
  (menu (g-object gtk-menu))
  (child (g-object gtk-menu-item))
  (left :uint)
  (right :uint)
  (top :uint)
  (bottom :uint))

(export 'gtk-menu-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_at_rect ()
;;;
;;; void
;;; gtk_menu_popup_at_rect (GtkMenu *menu,
;;;                         GdkWindow *rect_window,
;;;                         const GdkRectangle *rect,
;;;                         GdkGravity rect_anchor,
;;;                         GdkGravity menu_anchor,
;;;                         const GdkEvent *trigger_event);
;;;
;;; Displays menu and makes it available for selection.
;;;
;;; See gtk_menu_popup_at_widget() and gtk_menu_popup_at_pointer(), which handle
;;; more common cases for popping up menus.
;;;
;;; menu will be positioned at rect , aligning their anchor points. rect is
;;; relative to the top-left corner of rect_window . rect_anchor and menu_anchor
;;; determine anchor points on rect and menu to pin together. menu can
;;; optionally be offset by "rect-anchor-dx" and "rect-anchor-dy".
;;;
;;; Anchors should be specified under the assumption that the text direction is
;;; left-to-right; they will be flipped horizontally automatically if the text
;;; direction is right-to-left.
;;;
;;; Other properties that influence the behaviour of this function are
;;; "anchor-hints" and "menu-type-hint". Connect to the "popped-up" signal to
;;; find out how it was actually positioned.
;;;
;;; menu :
;;;     the GtkMenu to pop up
;;;
;;; rect_window :
;;;     the GdkWindow rect is relative to.
;;;
;;; rect :
;;;     the GdkRectangle to align menu with.
;;;
;;; rect_anchor :
;;;     the point on rect to align with menu 's anchor point
;;;
;;; menu_anchor :
;;;     the point on menu to align with rect 's anchor point
;;;
;;; trigger_event :
;;;     the GdkEvent that initiated this request or NULL if it's the current
;;;     event.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_at_widget ()
;;;
;;; void
;;; gtk_menu_popup_at_widget (GtkMenu *menu,
;;;                           GtkWidget *widget,
;;;                           GdkGravity widget_anchor,
;;;                           GdkGravity menu_anchor,
;;;                           const GdkEvent *trigger_event);
;;;
;;; Displays menu and makes it available for selection.
;;;
;;; See gtk_menu_popup_at_pointer() to pop up a menu at the master pointer.
;;; gtk_menu_popup_at_rect() also allows you to position a menu at an arbitrary
;;; rectangle.
;;;
;;; menu will be positioned at widget , aligning their anchor points.
;;; widget_anchor and menu_anchor determine anchor points on widget and menu to
;;; pin together. menu can optionally be offset by "rect-anchor-dx" and
;;; "rect-anchor-dy".
;;;
;;; Anchors should be specified under the assumption that the text direction is
;;; left-to-right; they will be flipped horizontally automatically if the text
;;; direction is right-to-left.
;;;
;;; Other properties that influence the behaviour of this function are
;;; "anchor-hints" and "menu-type-hint". Connect to the "popped-up" signal to
;;; find out how it was actually positioned.
;;;
;;; menu :
;;;     the GtkMenu to pop up
;;;
;;; widget :
;;;     the GtkWidget to align menu with.
;;;
;;; widget_anchor :
;;;     the point on widget to align with menu 's anchor point
;;;
;;; menu_anchor :
;;;     the point on menu to align with widget 's anchor point
;;;
;;; trigger_event :
;;;     the GdkEvent that initiated this request or NULL if it's the current
;;;     event.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_at_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popup_at_pointer" gtk-menu-popup-at-pointer) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[event]{a @class{gdk-event} event that initiated this request of
    @code{nil} if it is the current event}
  @begin{short}
    Displays menu and makes it available for selection.
  @end{short}
  See the @fun{gtk-menu-popup-at-widget} function to pop up a menu at a widget.
  The @fun{gtk-menu-popup-at-rect} function also allows you to position a menu
  at an arbitrary rectangle.

  The menu will be positioned at the pointer associated with @arg{event}.

  Properties that influence the behaviour of this function are
  @slot[gtk-menu]{anchor-hints}, @slot[gtk-menu]{rect-anchor-dx},
  @slot[gtk-menu]{rect-anchor-dy}, and @slot[gtk-menu]{menu-type-hint}. Connect
  to the \"popped-up\" signal to find out how it was actually positioned.

  Since 3.22
  @see-class{gtk-menu}
  @see-class{gdk-event}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-menu-anchor-hints}
  @see-function{gtk-menu-rect-anchor-dx}
  @see-function{gtk-menu-rect-anchor-dy}
  @see-function{gtk-menu-menu-type-hint}"
  (menu (g-object gtk-menu))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-menu-popup-at-pointer)

;;; ----------------------------------------------------------------------------
;;; GtkMenuPositionFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-menu-position-func :void
    ((menu (g-object gtk-menu))
     (x :pointer)
     (y :pointer)
     (push-in :pointer)
     (data :pointer))
  (restart-case
    (multiple-value-bind (rx ry rpush-in)
        (funcall (get-stable-pointer-value data) menu)
      (setf (mem-ref x :int) rx
            (mem-ref y :int) ry
            (mem-ref push-in :boolean) rpush-in))
    (return-zero () (setf (mem-ref x :int) 0
                          (mem-ref y :int) 0
                          (mem-ref push-in :boolean) nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-position-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-menu-position-func atdoc:*external-symbols*)
 "@version{2021-11-14}
  @begin{short}
    A user function supplied when calling the @fun{gtk-menu-popup} function
    which controls the positioning of the menu when it is displayed.
  @end{short}
  The function sets the @arg{x} and @arg{y} parameters to the coordinates where
  the menu is to be drawn. To make the menu appear on a different monitor than
  the mouse pointer, the @fun{gtk-menu-monitor} function must be called.
  @begin{pre}
 lambda (menu x y push)
  @end{pre}
  @begin[code]{table}
    @entry[menu]{A @class{gtk-menu} widget.}
    @entry[x]{A pointer to the integer representing the horizontal position
      where the menu shall be drawn.}
    @entry[y]{A pointer to the integer representing the vertical position where
      the menu shall be drawn.}
    @entry[push]{This parameter controls how menus placed outside the monitor
      are handled. If this is set to @em{true} and part of the menu is outside
      the monitor then GTK pushes the window into the visible area, effectively
      modifying the popup position. Note that moving and possibly resizing the
      menu around will alter the scroll position to keep the menu items \"in
      place\", i.e. at the same monitor position they would have been without
      resizing. In practice, this behavior is only useful for combobox popups
      or option menus and cannot be used to simply confine a menu to monitor
      boundaries. In that case, changing the scroll offset is not desirable.}
  @end{table}
  @see-class{gtk-menu}
  @see-function{gtk-menu-popup}
  @see-function{gtk-menu-monitor}")

(export 'gtk-menu-position-func)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popup_for_device" %gtk-menu-popup-for-device) :void
  (menu (g-object gtk-menu))
  (device (g-object gdk-device))
  (shell (g-object gtk-menu-shell))
  (item (g-object gtk-menu-item))
  (func :pointer)
  (data :pointer)
  (destroy :pointer)
  (button :uint)
  (time :uint32))

(defun gtk-menu-popup-for-device (menu device shell item func button time)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[device]{a @class{gdk-device} object}
  @argument[shell]{a @class{gtk-menu-shell} widget containing the triggering
    menu item, or @code{nil}}
  @argument[item]{a @class{gtk-menu-item} widget whose activation triggered the
    popup, or @code{nil}}
  @argument[func]{a user supplied @symbol{gtk-menu-position-func} callback
    function used to position the menu, or @code{nil}}
  @argument[button]{an unsigned integer with the mouse button which was pressed
    to initiate the event}
  @argument[time]{an unsigned integer with the time at which the activation
    event occurred}
  @begin{short}
    Displays a menu and makes it available for selection.
  @end{short}
  Applications can use this function to display context-sensitive menus, and
  will typically supply @code{nil} for the @arg{shell}, @arg{item}, @arg{func}
  parameters. The default menu positioning function will position the menu at
  the current position of device, or its corresponding pointer.

  The button parameter should be the mouse button pressed to initiate the menu
  popup. If the menu popup was initiated by something other than a mouse button
  press, such as a mouse button release or a keypress, button should be 0.

  The @arg{time} parameter is used to conflict-resolve initiation of
  concurrent requests for mouse/keyboard grab requests. To function properly,
  this needs to be the time stamp of the user event, such as a mouse click or
  key press, that caused the initiation of the popup. Only if no such event is
  available, the @fun{gtk-current-event-time} function can be used instead.
  @begin[Warning]{dictionary}
    The @sym{gtk-menu-popup-for-device} function has been deprecated since
    version 3.22 and should not be used in newly written code. Please use the
    @fun{gtk-menu-popup-at-widget}, @fun{gtk-menu-popup-at-pointer},
    or @fun{gtk-menu-popup-at-rect} functions instead.
  @end{dictionary}
  @see-class{gtk-menu}
  @see-class{gdk-device}
  @see-class{gtk-widget}
  @see-symbol{gtk-menu-position-func}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-popup-at-rect}
  @see-function{gtk-current-event-time}"
  (%gtk-menu-popup-for-device menu
                              device
                              shell
                              item
                              (callback gtk-menu-position-func)
                              (allocate-stable-pointer func)
                              (callback stable-pointer-destroy-notify)
                              button
                              time))

(export 'gtk-menu-popup-for-device)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popup" %gtk-menu-popup) :void
  (menu (g-object gtk-menu))
  (shell (g-object gtk-menu-shell))
  (item (g-object gtk-menu-item))
  (func :pointer)
  (data :pointer)
  (button :uint)
  (time :uint32))

(defun gtk-menu-popup (menu &key shell
                                 item
                                 func
                                 (button 0)
                                 (time (gtk-current-event-time)))
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[shell]{a @class{gtk-menu-shell} widget containing the triggering
    menu item, or @code{nil}}
  @argument[item]{a @class{gtk-menu-item} widget whose activation triggered the
    popup, or @code{nil}}
  @argument[func]{a user supplied @symbol{gtk-menu-position-func} callback
    function used to position the menu, or @code{nil}}
  @argument[button]{an unsigned integer with the mouse button which was pressed
    to initiate the event}
  @argument[time]{an unsigned integer with the time at which the activation
    event occurred}
  @begin{short}
    Displays a menu and makes it available for selection.
  @end{short}
  Applications can use this function to display context-sensitive menus, and
  will typically supply @code{nil} for the @arg{shell}, @arg{item}, and
  @arg{func} parameter. The default menu positioning function will position the
  menu at the current mouse cursor position.

  The @arg{button} parameter should be the mouse button pressed to initiate the
  menu popup. If the menu popup was initiated by something other than a mouse
  button press, such as a mouse button release or a keypress, @arg{button}
  should be 0.

  The @arg{time} parameter is used to conflict-resolve initiation of concurrent
  requests for mouse/keyboard grab requests. To function properly, this needs to
  be the timestamp of the user event, such as a mouse click or key press, that
  caused the initiation of the popup. Only if no such event is available, the
  @fun{gtk-current-event-time} function can be used instead.
  @begin[Warning]{dictionary}
    The @sym{gtk-menu-popup} function has been deprecated since version 3.22
    and should not be used in newly written code. Please use the
    @fun{gtk-menu-popup-at-widget}, @fun{gtk-menu-popup-at-pointer}, or
    @fun{gtk-menu-popup-at-rect} functions instead.
  @end{dictionary}
  @see-class{gtk-menu}
  @see-class{gtk-widget}
  @see-symbol{gtk-menu-position-func}
  @see-function{gtk-current-event-time}
  @see-function{gtk-menu-popup-at-widget}
  @see-function{gtk-menu-popup-at-pointer}
  @see-function{gtk-menu-popup-at-rect}"
  (if func
      (with-stable-pointer (ptr func)
        (%gtk-menu-popup menu
                         shell
                         item
                         (callback gtk-menu-position-func)
                         ptr
                         button
                         time))
      (%gtk-menu-popup menu
                       shell
                       item
                       (null-pointer)
                       (null-pointer)
                       button
                       time)))

(export 'gtk-menu-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_place_on_monitor ()
;;;
;;; void
;;; gtk_menu_place_on_monitor (GtkMenu *menu, GdkMonitor *monitor);
;;;
;;; Places menu on the given monitor.
;;;
;;; menu :
;;;     a GtkMenu
;;;
;;; monitor :
;;;     the monitor to place the menu on
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_popdown ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_popdown" gtk-menu-popdown) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @short{Removes the menu from the screen.}
  @see-class{gtk-menu}"
  (menu (g-object gtk-menu)))

(export 'gtk-menu-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_reposition ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_reposition" gtk-menu-reposition) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @short{Repositions the menu according to its position function.}
  @see-class{gtk-menu}"
  (menu (g-object gtk-menu)))

(export 'gtk-menu-reposition)

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

;;; ----------------------------------------------------------------------------
;;; gtk_menu_attach_to_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_attach_to_widget" gtk-menu-attach-to-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @argument[widget]{a @class{gtk-widget} object that the menu will be attached
    to}
  @argument[detacher]{a user supplied @code{GtkMenuDetachFunc} callback function
    that will be called when the menu calls the @fun{gtk-menu-detach} function}
  @begin{short}
    Attaches the menu to the widget and provides a callback function that will
    be invoked when the menu calls the @fun{gtk-menu-detach} function during
    its destruction.
  @end{short}
  @see-class{gtk-menu}
  @see-class{gtk-widget}
  @see-fun{gtk-menu-detach}"
  (menu (g-object gtk-menu))
  (widget (g-object gtk-widget))
  (detacher :pointer))

(export 'gtk-menu-attach-to-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_detach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_detach" gtk-menu-detach) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[menu]{a @class{gtk-menu} widget}
  @begin{short}
    Detaches the menu from the widget to which it had been attached.
  @end{short}
  This function will call the callback function, detacher, provided when the
  @fun{gtk-menu-attach-to-widget} function was called.
  @see-class{gtk-menu}
  @see-function{gtk-menu-attach-to-widget}"
  (menu (g-object gtk-menu)))

(export 'gtk-menu-detach)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_get_for_attach_widget () -> gtk-menu-for-attach-widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_get_for_attach_widget" gtk-menu-for-attach-widget)
    (g-list (g-object gtk-menu) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-14}
  @argument[widget]{a @class{gtk-widget} widget}
  @return{The list of @class{gtk-menu} widgets attached to @arg{widget}.}
  @begin{short}
    Returns a list of the menus which are attached to a widget.
  @end{short}
  @see-class{gtk-menu}
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget)))

(export 'gtk-menu-for-attach-widget)

;;; --- End of file gtk.menu.lisp ----------------------------------------------

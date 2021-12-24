;;; ----------------------------------------------------------------------------
;;; gtk.container.lisp
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
;;; GtkContainer
;;;
;;;     Base class for widgets which contain other widgets
;;;
;;; Types and Values
;;;
;;;     GtkResizeMode
;;;     GtkContainer
;;;
;;; Functions
;;;
;;;     GTK_IS_RESIZE_CONTAINER
;;;     GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID
;;;
;;;     gtk_container_add
;;;     gtk_container_remove
;;;     gtk_container_add_with_properties
;;;     gtk_container_get_resize_mode                      Accessor / deprecated
;;;     gtk_container_set_resize_mode                      Accessor / deprecated
;;;     gtk_container_check_resize
;;;     gtk_container_foreach
;;;     gtk_container_get_children
;;;     gtk_container_get_path_for_child
;;;     gtk_container_set_reallocate_redraws               deprecated
;;;     gtk_container_get_focus_child
;;;     gtk_container_set_focus_child
;;;     gtk_container_get_focus_vadjustment
;;;     gtk_container_set_focus_vadjustment
;;;     gtk_container_get_focus_hadjustment
;;;     gtk_container_set_focus_hadjustment
;;;     gtk_container_resize_children                      deprecated
;;;     gtk_container_child_type
;;;     gtk_container_child_get
;;;     gtk_container_child_set
;;;     gtk_container_child_get_property
;;;     gtk_container_child_set_property
;;;     gtk_container_child_get_valist
;;;     gtk_container_child_set_valist
;;;     gtk_container_child_notify
;;;     gtk_container_child_notify_by_pspec
;;;     gtk_container_forall
;;;     gtk_container_get_border_width                     Accessor
;;;     gtk_container_set_border_width                     Accessor
;;;     gtk_container_propagate_draw
;;;     gtk_container_get_focus_chain                      deprecated
;;;     gtk_container_set_focus_chain                      deprecated
;;;     gtk_container_unset_focus_chain                    deprecated
;;;     gtk_container_class_find_child_property
;;;     gtk_container_class_install_child_property
;;;     gtk_container_class_install_child_properties
;;;     gtk_container_class_list_child_properties
;;;     gtk_container_class_handle_border_width
;;;
;;; Properties
;;;
;;;             guint    border-width       Read / Write
;;;         GtkWidget*   child              Write
;;;     GtkResizeMode    resize-mode        Read / Write
;;;
;;; Signals
;;;
;;;              void    add                Run First
;;;              void    check-resize       Run Last
;;;              void    remove             Run First
;;;              void    set-focus-child    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 ├── GtkBox
;;;                 ├── GtkFixed
;;;                 ├── GtkFlowBox
;;;                 ├── GtkGrid
;;;                 ├── GtkHeaderBar
;;;                 ├── GtkPaned
;;;                 ├── GtkIconView
;;;                 ├── GtkLayout
;;;                 ├── GtkListBox
;;;                 ├── GtkMenuShell
;;;                 ├── GtkNotebook
;;;                 ├── GtkSocket
;;;                 ├── GtkStack
;;;                 ├── GtkTable
;;;                 ├── GtkTextView
;;;                 ├── GtkToolbar
;;;                 ├── GtkToolItemGroup
;;;                 ├── GtkToolPalette
;;;                 ╰── GtkTreeView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkContainer implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkResizeMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkResizeMode" gtk-resize-mode
  (:export t
   :type-initializer "gtk_resize_mode_get_type")
  (:parent 0)
  (:queue 1)
  (:immediate 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-resize-mode atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-resize-mode atdoc:*external-symbols*)
 "@version{2021-9-12}
  @begin{short}
    An enumeration representing the values of the
    @slot[gtk-container]{resize-mode} property.
  @end{short}
  @begin[Warning]{dictionary}
    Resize modes are deprecated since version 3.12 and should not be used in
    newly written code. They are not necessary anymore since frame clocks and
    might introduce obscure bugs if used.
  @end{dictionary}
  @begin{pre}
(define-g-enum \"GtkResizeMode\" gtk-resize-mode
  (:export t
   :type-initializer \"gtk_resize_mode_get_type\")
  (:parent 0)
  (:queue 1)
  (:immediate 2))
  @end{pre}
  @begin[code]{table}
    @entry[:parent]{Pass resize request to the parent.}
    @entry[:queue]{Queue resizes on this widget.}
    @entry[:immediate]{Resize immediately.}
  @end{table}
  @see-class{gtk-container}
  @see-function{gtk-container-resize-mode}")

;;; ----------------------------------------------------------------------------
;;; struct GtkContainer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkContainer" gtk-container
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_container_get_type")
  ((border-width
    gtk-container-border-width
    "border-width" "guint" t t)
   (child
    gtk-container-child
    "child" "GtkWidget" nil t)
   (resize-mode
    gtk-container-resize-mode
    "resize-mode" "GtkResizeMode" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-container 'type)
 "@version{2021-9-12}
  @begin{short}
    Base class for widgets which contain other widgets.
  @end{short}

  A GTK user interface is constructed by nesting widgets inside widgets.
  Container widgets are the inner nodes in the resulting tree of widgets: they
  contain other widgets. So, for example, you might have a @class{gtk-window}
  widget containing a @class{gtk-frame} widget containing a @class{gtk-label}
  widget. If you wanted an image instead of a textual label inside the frame,
  you might replace the @class{gtk-label} widget with a @class{gtk-image}
  widget.

  There are two major kinds of container widgets in GTK. Both are subclasses
  of the abstract @sym{gtk-container} base class.

  The first type of container widget has a single child widget and derives
  from the @class{gtk-bin} class. These containers are decorators, which add
  some kind of functionality to the child. For example, a @class{gtk-button}
  widget makes its child into a clickable button. A @class{gtk-frame} widget
  draws a frame around its child and a @class{gtk-window} widget places its
  child widget inside a toplevel window.

  The second type of container can have more than one child. Its purpose is to
  manage layout. This means that these containers assign sizes and positions
  to their children. For example, a @class{gtk-grid} widget arranges the widgets
  it contains in a two-dimensional grid.

  @subheading{Height for width geometry management}
  GTK uses a height-for-width and width-for-height geometry management system.
  Height-for-width means that a widget can change how much vertical space it
  needs, depending on the amount of horizontal space that it is given and
  similar for width-for-height.

  There are some things to keep in mind when implementing container widgets
  that make use of the height for width geometry management system. First,
  it is important to note that a container must prioritize one of its
  dimensions, that is to say that a widget or container can only have a
  @symbol{gtk-size-request-mode} mode that is @code{:height-for-width} or
  @code{:width-for-height}. However, every widget and container must
  be able to respond to the APIs for both dimensions, i.e. even if a widget
  has a request mode that is height-for-width, it is possible that its parent
  will request its sizes using the width-for-height APIs.

  @subheading{Child properties}
  The @sym{gtk-container} widget introduces child properties. These are object
  properties that are not specific to either the container or the contained
  widget, but rather to their relation. Typical examples of child properties
  are the position or pack-type of a widget which is contained in a
  @class{gtk-box} widget.

  Use the @fun{gtk-container-class-find-child-property} or
  @fun{gtk-container-class-list-child-properties} functions to get information
  about existing child properties.

  To obtain or to set the value of a child property, use the
  @fun{gtk-container-child-property}, @fun{gtk-container-child-get},
  or @fun{gtk-container-child-set} functions. To emit notification about child
  property changes, use the @fun{gtk-widget-child-notify} function.
  @begin[GtkContainer as GtkBuildable]{dictionary}
    The @sym{gtk-container} implementation of the @class{gtk-buildable}
    interface supports a @code{<packing>} element for children, which can
    contain multiple @code{<property>} elements that specify child properties
    for the child.

    Child properties can also be marked as translatable using the same
    \"translatable\", \"comments\" and \"context\" attributes that are used for
    regular properties.

    Containers can have a @code{<focus-chain>} element containing multiple
    @code{<widget>} elements, one for each child that should be added to the
    focus chain. The \"name\" attribute gives the ID of the widget.

    @b{Example:} Child properties in UI definitions
    @begin{pre}
<object class=\"GtkBox\">
  <child>
    <object class=\"GtkEntry\" id=\"entry1\"/>
    <packing>
      <property name=\"pack-type\">start</property>
    </packing>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"entry2\"/>
  </child>
  <focus-chain>
    <widget name=\"entry1\"/>
    <widget name=\"entry2\"/>
  </focus-chain>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"add\" signal}
      @begin{pre}
 lambda (container widget)    :run-first
      @end{pre}
    @subheading{The \"check-resize\" signal}
      @begin{pre}
 lambda (container)    :run-last
      @end{pre}
    @subheading{The \"remove\" signal}
      @begin{pre}
 lambda (container widget)    :run-first
      @end{pre}
    @subheading{The \"set-focus-child\" signal}
      @begin{pre}
 lambda (container widget)    :run-first
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-container-border-width}
  @see-slot{gtk-container-child}
  @see-slot{gtk-container-resize-mode}
  @see-class{gtk-bin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-container-border-width ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "border-width"
                                               'gtk-container) 't)
 "The @code{border-width} property of type @code{:uint} (Read / Write) @br{}
  The width of the empty border outside the containers children. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-container-border-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-container-border-width 'function)
 "@version{*2021-12-3}
  @syntax[]{(gtk-container-border-width object) => width}
  @syntax[]{(setf gtk-container-border-width object) width)}
  @argument[object]{a @class{gtk-container} widget}
  @argument[width]{an unsigned integer with the border width}
  @begin{short}
    Accessor of the @slot[gtk-container]{border-width} slot of the
    @class{gtk-container} class.
  @end{short}

  The @sym{gtk-container-border-width} slot access function retrieves the
  border width of the container. The @sym{(setf gtk-container-border-width)}
  slot acces function sets the border width.

  The border width of a container is the amount of space to leave around the
  outside of the container. Valid values are in the range [0, 65535] pixels.
  The only exception to this is the @class{gtk-window} widget. Because toplevel
  windows cannot leave space outside, they leave the space inside. The border
  is added on all sides of the container. To add space to only one side, one
  approach is to create a @class{gtk-alignment} widget, call the
  @fun{gtk-widget-size-request} function to give it a size, and place it on the
  side of the container as a spacer.
  @see-class{gtk-container}
  @see-class{gtk-window}
  @see-class{gtk-alignment}
  @see-function{gtk-widget-size-request}")

;;; --- gtk-container-child ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child" 'gtk-container) 't)
 "The @code{child} property of type @class{gtk-widget} (Write) @br{}
  Can be used to add a new child to the container.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-container-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-container-child 'function)
 "@version{2021-9-12}
  @syntax[]{(gtk-container-child object) => child}
  @syntax[]{(setf gtk-container-child object) child)}
  @argument[object]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk-container]{child} slot of the
    @class{gtk-container} class.
  @end{short}

  Can be used to add a new child to the container.
  @see-class{gtk-container}
  @see-class{gtk-widget}")

;;; --- gtk-container-resize-mode ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resize-mode" 'gtk-container) 't)
 "The @code{resize-mode} property of type @symbol{gtk-resize-mode}
  (Read / Write) @br{}
  Specify how resize events are handled. @br{}
  @em{Warning:} Resize modes are deprecated since version 3.12 and should not
  be used in newly written code. They are not necessary anymore since frame
  clocks and might introduce obscure bugs if used. @br{}
  Default value: @code{:parent}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-container-resize-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-container-resize-mode 'function)
 "@version{2021-9-18}
  @syntax[]{(gtk-container-resize-mode object) => mode}
  @syntax[]{(setf gtk-container-resize-mode object) mode)}
  @argument[object]{a @class{gtk-container} widget}
  @argument[mode]{a value of the @symbol{gtk-resize-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-container]{resize-mode} slot of the
    @class{gtk-container} class.
  @end{short}

  The @sym{gtk-container-resize-mode} slot access function returns the current
  resize mode of the container. The @sym{(setf gtk-container-resize-mode)}
  slot access function sets the resize mode.

  The resize mode of a container determines whether a resize request will be
  passed to the parent of the container, queued for later execution or executed
  immediately.
  @begin[Warning]{dictionary}
    The @sym{gtk-container-resize-mode} function has been deprecated since
    version 3.12 and should not be used in newly written code. Resize modes are
    deprecated. They are not necessary anymore since frame clocks and might
    introduce obscure bugs if used.
  @end{dictionary}
  @see-class{gtk-container}
  @see-symbol{gtk-resize-mode}")

;;; ----------------------------------------------------------------------------
;;; GTK_IS_RESIZE_CONTAINER()
;;;
;;; #define GTK_IS_RESIZE_CONTAINER(widget)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID()
;;;
;;; #define GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID (object,
;;;                                                       property_id, pspec)
;;;
;;; This macro should be used to emit a standard warning about unexpected
;;; properties in set_child_property() and get_child_property() implementations.
;;;
;;; object :
;;;     the GObject on which set_child_property() or get_child_property() was
;;;     called
;;;
;;; property_id :
;;;     the numeric id of the property
;;;
;;; pspec :
;;;     the GParamSpec of the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_add" gtk-container-add) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-11}
  @argument[container]{a @class{gtk-container} widget}
  @argument[widget]{a @class{gtk-widget} child widget to be placed inside
    @arg{container}}
  @begin{short}
    Adds a child widget to the container.
  @end{short}
  Typically used for simple containers such as @class{gtk-window},
  @class{gtk-frame}, or @class{gtk-button} widgets.

  For more complicated layout containers such as @class{gtk-box} or
  @class{gtk-grid} widgets, this function will pick default packing parameters
  that may not be correct. So consider functions such as the
  @fun{gtk-box-pack-start} and @fun{gtk-grid-attach} functions as an alternative
  to the @sym{gtk-container-add} function in those cases.

  A widget may be added to only one container at a time. You cannot place the
  same widget inside two different containers.
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-class{gtk-window}
  @see-class{gtk-frame}
  @see-class{gtk-button}
  @see-function{gtk-box-pack-start}
  @see-function{gtk-grid-attach}"
  (container (g-object gtk-container))
  (widget (g-object gtk-widget)))

(export 'gtk-container-add)

;;; ----------------------------------------------------------------------------
;;; gtk_container_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_remove" gtk-container-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[widget]{a current @class{gtk-widget} child widget of
    @arg{container}}
  @begin{short}
    Removes a child widget from the container.
  @end{short}
  The child widget must be inside the container.
  @see-class{gtk-container}
  @see-class{gtk-widget}"
  (container (g-object gtk-container))
  (widget (g-object gtk-widget)))

(export 'gtk-container-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_container_add_with_properties ()
;;;
;;; void gtk_container_add_with_properties (GtkContainer *container,
;;;                                         GtkWidget *widget,
;;;                                         const gchar *first_prop_name,
;;;                                         ...);
;;;
;;; Adds widget to container, setting child properties at the same time.
;;; See gtk_container_add() and gtk_container_child_set() for more details.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; widget :
;;;     a widget to be placed inside container
;;;
;;; first_prop_name :
;;;     the name of the first child property to set
;;;
;;; ... :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_check_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_check_resize" gtk-container-check-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @begin{short}
    Emits the \"check-resize\" signal on the container.
  @end{short}
  @see-class{gtk-container}"
  (container (g-object gtk-container)))

(export 'gtk-container-check-resize)

;;; ----------------------------------------------------------------------------
;;; GtkCallback ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-callback :void
    ((widget (g-object gtk-widget))
     (data :pointer))
  (restart-case
    (funcall (get-stable-pointer-value data) widget)
    (return () :report "Error in GtkCallback function." nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-callback atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-callback atdoc:*external-symbols*)
 "@version{2021-9-20}
  @begin{short}
    The type of the callback functions used for e.g. iterating over the
    children of a container, see the @fun{gtk-container-foreach} function.
  @end{short}
  @begin{pre}
 lambda (widget)
  @end{pre}
  @begin[code]{table}
    @entry[widget]{A @class{gtk-widget} object to operate on.}
  @end{table}
  @see-class{gtk-container}
  @see-function{gtk-container-foreach}
  @see-function{gtk-container-forall}")

(export 'gtk-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_container_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_foreach" %gtk-container-foreach) :void
  (container (g-object gtk-container))
  (func :pointer)
  (data :pointer))

(defun gtk-container-foreach (container func)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[func]{a @symbol{gtk-callback} callback function}
  @begin{short}
    Invokes a function on each non-internal child of the container.
  @end{short}
  See the @fun{gtk-container-forall} function for details on what constitutes
  an \"internal\" child. Most applications should use the
  @sym{gtk-container-foreach} function, rather than the
  @fun{gtk-container-forall} function.
  @see-class{gtk-container}
  @see-symbol{gtk-callback}
  @see-function{gtk-container-forall}"
  (with-stable-pointer (ptr func)
    (%gtk-container-foreach container
                            (callback gtk-callback)
                            ptr)))

(export 'gtk-container-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_children () -> gtk-container-children
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_get_children" gtk-container-children)
    (g-list g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @return{A list of the containers non-internal children.}
  @begin{short}
    Returns a list with the non-internal children of the container.
  @end{short}
  See the @fun{gtk-container-forall} function for details on what constitutes
  an \"internal\" child.
  @begin[Example]{dictionary}
    @begin{pre}
(setq box (make-instance 'gtk-box :orientation :vertical))
=> #<GTK-BOX {1001E2A183@}>
(gtk-container-add box (make-instance 'gtk-button))
(gtk-container-add box (make-instance 'gtk-label))
(gtk-container-children box)
=> (#<GTK-BUTTON {1001E2B0A3@}> #<GTK-LABEL {1001E2BFD3@}>)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-container}
  @see-function{gtk-container-forall}"
  (container (g-object gtk-container)))

(export 'gtk-container-children)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_path_for_child () -> gtk-container-path-for-child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_get_path_for_child" gtk-container-path-for-child)
    (g-boxed-foreign gtk-widget-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} child widget of @arg{container}}
  @return{A newly created @class{gtk-widget-path} instance.}
  @begin{short}
    Returns a newly created widget path representing all the widget hierarchy
    from the toplevel down to and including child.
  @end{short}
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-class{gtk-widget-path}"
  (container (g-object gtk-container))
  (child (g-object gtk-widget)))

(export 'gtk-container-path-for-child)

;;; ----------------------------------------------------------------------------
;;; gtk_container_set_reallocate_redraws ()                not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_set_reallocate_redraws"
           gtk-container-set-reallocate-redraws) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[redraw]{a boolean with the value for the @code{reallocate-redraws}
    flag of the container}
  @begin{short}
    Sets the @code{reallocate_redraws} flag of the container to the given value.
  @end{short}
  Containers requesting reallocation redraws get automatically redrawn if any
  of their children changed allocation.
  @begin[Warning]{dictionary}
    The @sym{gtk-container-reallocate-redraws} function has been deprecated
    since version 3.14 and should not be used in newly written code. Call
    the @fun{gtk-widget-queue-draw} function.
  @end{dictionary}
  @see-class{gtk-container}
  @see-function{gtk-widget-queue-draw}"
  (container (g-object gtk-container))
  (redraw :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_child ()
;;; gtk_container_set_focus_child () -> gtk-container-focus-child
;;; ----------------------------------------------------------------------------

(defun (setf gtk-container-focus-child) (child container)
  (foreign-funcall "gtk_container_set_focus_child"
                   (g-object gtk-container) container
                   (g-object gtk-widget) child
                   :void)
  child)

(defcfun ("gtk_container_get_focus_child" gtk-container-focus-child)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @syntax[]{(gtk-container-focus-child container) => child}
  @syntax[]{(setf (gtk-container-focus-child container) child)}
  @argument[container]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @begin{short}
    Accessor of the current focused child widget in the container.
  @end{short}

  The @sym{gtk-container-focus-child} function returns the current focus child
  widget which will receive the focus inside the container when the container
  is focussed. This is not the currently focused widget. That can be obtained
  by calling the @fun{gtk-window-focus} function. The
  @sym{(setf gtk-container-focus-child)} function sets, or unsets, if the
  @arg{child} argument is @code{nil}, the focused child of the container.

  This function emits the \"set-focus-child\" signal of the container.
  Implementations of the @class{gtk-container} class can override the default
  behaviour by overriding the handler of this signal.

  This function is mostly meant to be used by widgets. Applications can use the
  @fun{gtk-widget-grab-focus} function to manualy set the focus to a specific
  widget.
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{gtk-window-focus}
  @see-function{gtk-widget-grab-focus}"
  (container (g-object gtk-container)))

(export 'gtk-container-focus-child)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_vadjustment ()
;;; gtk_container_set_focus_vadjustment () -> gtk-container-focus-vadjustment
;;; ----------------------------------------------------------------------------

(defun (setf gtk-container-focus-vadjustment) (adjustment container)
  (foreign-funcall "gtk_container_set_focus_vadjustment"
                   (g-object gtk-container) container
                   (g-object gtk-adjustment) adjustment
                   :void)
  adjustment)

(defcfun ("gtk_container_get_focus_vadjustment" gtk-container-focus-vadjustment)
    (g-object gtk-adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @syntax[]{(gtk-container-focus-vadjustment container) => adjustment}
  @syntax[]{(setf (gtk-container-focus-vadjustment container) adjustment)}
  @argument[container]{a @class{gtk-container} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be
    adjusted when the focus is moved among the descendents of the container}
  @begin{short}
    Accessor of the vertical focus adjustment of the container.
  @end{short}

  The @sym{gtk-container-focus-vadjustment} function retrieves the vertical
  focus adjustment for the container. The
  @sym{(setf gtk-container-focus-vadjustment)} function sets the vertical
  adjustment.

  Hooks up an adjustment to focus handling in a container, so when a child of
  the container is focused, the adjustment is scrolled to show that widget. The
  adjustments have to be in pixel units and in the same coordinate system as
  the allocation for immediate children of the container.
  @see-class{gtk-container}
  @see-class{gtk-adjustment}
  @see-function{gtk-container-focus-hadjustment}"
  (container (g-object gtk-container)))

(export 'gtk-container-focus-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_hadjustment ()
;;; gtk_container_set_focus_hadjustment () -> gtk-container-focus-hadjustment
;;; ----------------------------------------------------------------------------

(defun (setf gtk-container-focus-hadjustment) (adjustment container)
  (foreign-funcall "gtk_container_set_focus_hadjustment"
                   (g-object gtk-container) container
                   (g-object gtk-adjustment) adjustment
                   :void)
  adjustment)

(defcfun ("gtk_container_get_focus_hadjustment" gtk-container-focus-hadjustment)
    (g-object gtk-adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @syntax[]{(gtk-container-focus-hadjustment container) => adjustment}
  @syntax[]{(setf (gtk-container-focus-hadjustment container) adjustment)}
  @argument[container]{a @class{gtk-container} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be
    adjusted when the focus is moved among the descendents of the container}
  @begin{short}
    Accessor of the horizontal focus adjustment of the container.
  @end{short}

  The @sym{gtk-container-focus-hadjustment} function retrieves the horizontal
  focus adjustment for the container. The
  @sym{(setf gtk-container-focus-hadjustment)} function sets the horizontal
  adjustment.

  Hooks up an adjustment to focus handling in a container, so when a child of
  the container is focused, the adjustment is scrolled to show that widget. The
  adjustments have to be in pixel units and in the same coordinate system as
  the allocation for immediate children of the container.
  @see-class{gtk-container}
  @see-class{gtk-adjustment}
  @see-function{gtk-container-focus-vadjustment}"
  (container (g-object gtk-container)))

(export 'gtk-container-focus-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_container_resize_children ()                       not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_resize_children" gtk-container-resize-children) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @short{undocumented}
  @begin[Warning]{dictionary}
    The @sym{gtk-container-resize-children} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-container}"
  (container (g-object gtk-container)))

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_child_type" gtk-container-child-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @return{A @class{g-type} type ID.}
  @begin{short}
    Returns the type of the children supported by the container.
  @end{short}

  Note that this may return @var{+g-type-none+} to indicate that no more
  children can be added, e.g. for a @class{gtk-paned} widget which already
  has two children.
  @see-class{gtk-container}
  @see-class{g-type}
  @see-variable{+g-type-none+}"
  (container (g-object gtk-container)))

(export 'gtk-container-child-type)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get ()
;;; ----------------------------------------------------------------------------

(defun gtk-container-child-get (container child &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} child widget which is a child of
    @arg{container}}
  @argument[args]{a list of strings with the child property names to get the
    values for}
  @return{A list with the values of the properties.}
  @begin{short}
    Gets the values of one or more child properties for a child widget of the
    container.
  @end{short}
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{gtk-container-child-set}
  @see-function{gtk-container-child-property}"
  (loop for arg in args
        collect (gtk-container-child-property container child arg)))

(export 'gtk-container-child-get)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set ()
;;; ----------------------------------------------------------------------------

(defun gtk-container-child-set (container child &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} child widget which is a child of
    @arg{container}}
  @argument[args]{a list of property names and values}
  @begin{short}
    Sets one or more child properties for a child widget of the container.
  @end{short}
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{gtk-container-child-get}
  @see-function{gtk-container-child-property}"
  (loop for (name value) on args by #'cddr
        do (setf (gtk-container-child-property container child name) value)))

(export 'gtk-container-child-set)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get_property ()
;;; gtk_container_child_set_property () -> gtk-container-child-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_child_set_property" %gtk-container-child-set-property)
    :void
  (container (g-object gtk-container))
  (child (g-object gtk-widget))
  (property :string)
  (value (:pointer (:struct g-value))))

(defun (setf gtk-container-child-property) (value container child property)
  (let ((gtype (g-param-spec-value-type
                   (gtk-container-class-find-child-property
                       (g-type-from-instance container) property))))
    (with-foreign-object (new-value '(:struct g-value))
      (set-g-value new-value value gtype :zero-g-value t)
      (%gtk-container-child-set-property container child property new-value)
      (g-value-unset new-value)
      (values value))))

(defcfun ("gtk_container_child_get_property" %gtk-container-child-property)
    :void
  (container (g-object gtk-container))
  (child (g-object gtk-widget))
  (property :string)
  (value (:pointer (:struct g-value))))

(defun gtk-container-child-property (container child property)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-16}
  @syntax[]{(gtk-container-child-property container child property) => value}
  @syntax[]{(setf (gtk-container-child-property container child property) value)}
  @argument[container]{a @class{gtk-container} widget}
  @argument[child]{a @class{gtk-widget} object which is a child of
    @arg{container}}
  @argument[property]{a string with the name of the property to get}
  @argument[value]{the value of the property}
  @begin{short}
    Gets or sets the value of a child property for the child widget of the
    container.
  @end{short}
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{gtk-container-child-get}
  @see-function{gtk-container-child-set}"
  (let ((gtype (g-param-spec-value-type
                   (gtk-container-class-find-child-property
                       (g-type-from-instance container)
                       property))))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value gtype)
      (%gtk-container-child-property container child property value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gtk-container-child-property)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get_valist ()
;;;
;;; void gtk_container_child_get_valist (GtkContainer *container,
;;;                                      GtkWidget *child,
;;;                                      const gchar *first_property_name,
;;;                                      va_list var_args);
;;;
;;; Gets the values of one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; var_args :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set_valist ()
;;;
;;; void gtk_container_child_set_valist (GtkContainer *container,
;;;                                      GtkWidget *child,
;;;                                      const gchar *first_property_name,
;;;                                      va_list var_args);
;;;
;;; Sets one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_property_name :
;;;     the name of the first property to set
;;;
;;; var_args :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_child_notify" gtk-container-child-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{the @class{gtk-container} widget}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[property]{a string with the name of a child property installed on
    the class of @arg{container}}
  @begin{short}
    Emits a \"child-notify\" signal for the @arg{property} child property on
    @arg{widget}.
  @end{short}

  This is an analogue of the @fun{g-object-notify} function for child
  properties. Also see the @fun{gtk-widget-child-notify} function.
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{g-object-notify}
  @see-function{gtk-widget-child-notify}"
  (container (g-object gtk-container))
  (child (g-object gtk-widget))
  (property :string))

(export 'gtk-container-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_notify_by_pspec ()
;;;
;;; void gtk_container_child_notify_by_pspec (GtkContainer *container,
;;;                                           GtkWidget *child,
;;;                                           GParamSpec *pspec);
;;;
;;; Emits a "child-notify" signal for the child property specified by pspec on
;;; the child.
;;;
;;; This is an analogue of g_object_notify_by_pspec() for child properties.
;;;
;;; container :
;;;     the GtkContainer
;;;
;;; child :
;;;     the child widget
;;;
;;; pspec :
;;;     the GParamSpec of a child property instealled on the class of container
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_forall ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_forall" %gtk-container-forall) :void
  (container (g-object gtk-container))
  (callback :pointer)
  (data :pointer))

(defun gtk-container-forall (container func)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[container]{a @class{gtk-container} widget}
  @argument[func]{a @symbol{gtk-callback} callback function which is passed as
    a callback}
  @begin{short}
    Invokes a function on each child of the container, including children that
    are considered \"internal\", implementation details of the container.
  @end{short}
  \"Internal\" children generally were not added by the user of the container,
  but were added by the container implementation itself. Most applications
  should use the @fun{gtk-container-foreach} function, rather than the
  @sym{gtk-container-forall} function.
  @see-class{gtk-container}
  @see-symbol{gtk-callback}
  @see-function{gtk-container-foreach}"
  (with-stable-pointer (ptr func)
    (%gtk-container-forall container
                           (callback gtk-callback)
                           ptr)))

(export 'gtk-container-forall)

;;; ----------------------------------------------------------------------------
;;; gtk_container_propagate_draw ()
;;;
;;; void gtk_container_propagate_draw (GtkContainer *container,
;;;                                    GtkWidget *child,
;;;                                    cairo_t *cr);
;;;
;;; When a container receives a call to the draw function, it must send
;;; synthetic "draw" calls to all children that don't have their own GdkWindows.
;;; This function provides a convenient way of doing this. A container, when it
;;; receives a call to its "draw" function, calls gtk_container_propagate_draw()
;;; once for each child, passing in the cr the container received.
;;;
;;; gtk_container_propagate_draw() takes care of translating the origin of cr,
;;; and deciding whether the draw needs to be sent to the child. It is a
;;; convenient and optimized way of getting the same effect as calling
;;; gtk_widget_draw() on the child directly.
;;;
;;; In most cases, a container can simply either inherit the "draw"
;;; implementation from GtkContainer, or do some drawing and then chain to the
;;; ::draw implementation from GtkContainer.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a child of container
;;;
;;; cr :
;;;     Cairo context as passed to the container. If you want to use cr in
;;;     container's draw function, consider using cairo_save() and
;;;     cairo_restore() before calling this function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_chain ()                       deprecated
;;; gtk_container_set_focus_chain () -> gtk-container-focus-chain
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_set_focus_chain" %gtk-container-set-focus-chain) :void
  (container (g-object gtk-container))
  (focusable (g-list (g-object gtk-widget))))

(defun (setf gtk-container-focus-chain) (focusable container)
  (%gtk-container-set-focus-chain container
                                  (mapcar #'pointer focusable))
  focusable)

(defcfun ("gtk_container_get_focus_chain" %gtk-container-get-focus-chain)
    :boolean
  (container (g-object gtk-container))
  (focusable (:pointer (g-list (g-object gtk-widget)))))

(defun gtk-container-focus-chain (container)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @syntax[]{(gtk-container-focus-chain container) => focusable}
  @syntax[]{(setf (gtk-container-focus-chain container) focusable)}
  @argument[container]{a @class{gtk-container} widget}
  @argument[focusable]{a list of @class{gtk-widget} objects representing the
    focus chain}
  @begin{short}
    Accessor of the focus chain widgets of the container.
  @end{short}

  The @sym{gtk-container-focus-chain} function retrieves the focus chain of the
  container, if one has been set explicitly. If no focus chain has been
  explicitly set, GTK computes the focus chain based on the positions of the
  children. In that case, GTK returns @em{false}. The
  @sym{(setf gtk-container-focus-chain)} function sets a focus chain,
  overriding the one computed automatically by GTK.

  In principle each widget in the chain should be a descendant of the container,
  but this is not enforced by this method, since it is allowed to set the focus
  chain before you pack the widgets, or have a widget in the chain that is not
  always packed. The necessary checks are done when the focus chain is actually
  traversed.
  @begin[Warning]{dictionary}
    The @sym{gtk-container-focus-chain} function has been deprecated since
    version 3.24 and should not be used in newly written code. For overriding
    focus behavior, use the \"focus\" signal.
  @end{dictionary}
  @see-class{gtk-container}
  @see-class{gtk-widget}
  @see-function{gtk-container-unset-focus-chain}"
  (with-foreign-object (focusable '(g-list (g-object gtk-widget)))
    (when (%gtk-container-get-focus-chain container focusable)
      (mem-ref focusable '(g-list (g-object gtk-widget))))))

(export 'gtk-container-focus-chain)

;;; ----------------------------------------------------------------------------
;;; gtk_container_unset_focus_chain ()                     deprecated
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_unset_focus_chain" gtk-container-unset-focus-chain)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[container]{a @class{gtk-container} widget}
  @begin{short}
    Removes a focus chain explicitly set with the
    @fun{gtk-container-focus-chain} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-container-unset-focus-chain} function has been deprecated since
    version 3.24 and should not be used in newly written code. For overriding
    focus behavior, use the \"focus\" signal.
  @end{dictionary}
  @see-class{gtk-container}
  @see-function{gtk-container-focus-chain}"
  (container (g-object gtk-container)))

(export 'gtk-container-unset-focus-chain)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_find_child_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_class_find_child_property"
          %gtk-container-class-find-child-property)
    (:pointer (:struct g-param-spec))
  (class (:pointer (:struct g-type-class)))
  (property :string))

(defun gtk-container-class-find-child-property (gtype property)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[gtype]{a @class{g-type} type ID}
  @argument[property]{a string with the name of the child property to find}
  @begin{return}
    The @symbol{g-param-spec} instance of the child property or a
    @code{null-pointer} if the @arg{gtype} type has no child property with that
    name.
  @end{return}
  @begin{short}
    Finds a child property of a container type by name.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gtk-container-class-find-child-property \"GtkBox\" \"expand\")
=> #.(SB-SYS:INT-SAP #X00A7DA60)
(g-param-spec-type *)
=> #<GTYPE :name \"GParamBoolean\" :id 24606448>
(g-param-spec-value-type **)
=> #<GTYPE :name \"gboolean\" :id 20>
(gtk-container-class-find-child-property \"GtkBox\" \"unknown\")
=> #.(SB-SYS:INT-SAP #X00000000)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-container}
  @see-symbol{g-type}
  @see-symbol{g-param-spec}"
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%gtk-container-class-find-child-property class property)))
        (unless (null-pointer-p pspec) pspec))
      (g-type-class-unref class))))

(export 'gtk-container-class-find-child-property)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_install_child_property ()
;;;
;;; void gtk_container_class_install_child_property (GtkContainerClass *cclass,
;;;                                                  guint property_id,
;;;                                                  GParamSpec *pspec);
;;;
;;; Installs a child property on a container class.
;;;
;;; cclass :
;;;     a GtkContainerClass
;;;
;;; property_id :
;;;     the id for the property
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_install_child_properties ()
;;;
;;; void
;;; gtk_container_class_install_child_properties (GtkContainerClass *cclass,
;;;                                               guint n_pspecs,
;;;                                               GParamSpec **pspecs);
;;;
;;; Installs child properties on a container class.
;;;
;;; cclass :
;;;     a GtkContainerClass

;;; n_pspecs :
;;;     the length of the GParamSpec array
;;;
;;; pspecs :
;;;     the GParamSpec array defining the new child properties.
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_list_child_properties ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_container_class_list_child_properties"
          %gtk-container-class-list-child-properties)
    (:pointer (:pointer (:struct g-param-spec)))
  (class (:pointer (:struct gobject::g-object-class)))
  (n-props (:pointer :uint)))

(defun gtk-container-class-list-child-properties (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-12}
  @argument[gtype]{a @class{g-type} type ID}
  @return{A list of @symbol{g-param-spec} instances.}
  @short{Returns the child properties of a container type.}
  @begin[Note]{dictionary}
    In the Lisp binding we pass the type of a container class and not
    a pointer to the container class as argument to the function.
  @end{dictionary}
  @see-class{gtk-container}
  @see-class{g-type}
  @see-class{g-param-spec}"
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (with-foreign-object (n-props :uint)
        (let ((pspecs (%gtk-container-class-list-child-properties class
                                                                  n-props)))
          (unwind-protect
            (loop for count from 0 below (mem-ref n-props :uint)
                  for pspec = (mem-aref pspecs :pointer count)
                  collect pspec)
            (g-free pspecs))))
      (g-type-class-unref class))))

(export 'gtk-container-class-list-child-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_handle_border_width ()
;;;
;;; void gtk_container_class_handle_border_width (GtkContainerClass *klass);
;;;
;;; Modifies a subclass of GtkContainerClass to automatically add and remove the
;;; border-width setting on GtkContainer. This allows the subclass to ignore the
;;; border width in its size request and allocate methods. The intent is for a
;;; subclass to invoke this in its class_init function.
;;;
;;; gtk_container_class_handle_border_width() is necessary because it would
;;; break API too badly to make this behavior the default. So subclasses must
;;; "opt in" to the parent class handling border_width for them.
;;;
;;; klass :
;;;     the class struct of a GtkContainer subclass
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.container.lisp -----------------------------------------

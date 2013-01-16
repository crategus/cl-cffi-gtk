;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.container.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "border-width" 'gtk-container) 't)
 "@version{2013-1-4}
  The \"border-width\" property of type guint (Read / Write).@br{}
  The width of the empty border outside the containers children.@br{}
  Allowed values: <= 65535@br{}
  Default value: 0")

(setf (documentation (atdoc:get-slot-from-name "child" 'gtk-container) 't)
 "@version{2013-1-4}
  The \"child\" property of type GtkWidget* (Write).@br{}
  Can be used to add a new child to the container.")

(setf (documentation (atdoc:get-slot-from-name "resize-mode" 'gtk-container) 't)
 "@version{2013-1-4}
  The \"resize-mode\" property of type GtkResizeMode (Read / Write).@br{}
  Specify how resize events are handled.@br{}
  Default value: GTK_RESIZE_PARENT")

;;; --- gtk-container ----------------------------------------------------------

(setf (documentation 'gtk-container 'type)
 "@version{2013-1-4}
  @short{Base class for widgets which contain other widgets.}

  A GTK+ user interface is constructed by nesting widgets inside widgets.
  Container widgets are the inner nodes in the resulting tree of widgets: they
  contain other widgets. So, for example, you might have a GtkWindow
  containing a GtkFrame containing a GtkLabel. If you wanted an image instead
  of a textual label inside the frame, you might replace the GtkLabel widget
  with a GtkImage widget.

  There are two major kinds of container widgets in GTK+. Both are subclasses
  of the abstract GtkContainer base class.

  The first type of container widget has a single child widget and derives
  from GtkBin. These containers are decorators, which add some kind of
  functionality to the child. For example, a GtkButton makes its child into a
  clickable button; a GtkFrame draws a frame around its child and a GtkWindow
  places its child widget inside a top-level window.

  The second type of container can have more than one child; its purpose is to
  manage layout. This means that these containers assign sizes and positions
  to their children. For example, a GtkHBox arranges its children in a
  horizontal row, and a GtkGrid arranges the widgets it contains in a
  two-dimensional grid.

  @b{Height for width geometry management}

  GTK+ uses a height-for-width (and width-for-height) geometry management
  system. Height-for-width means that a widget can change how much vertical
  space it needs, depending on the amount of horizontal space that it is given
  (and similar for width-for-height).

  There are some things to keep in mind when implementing container widgets
  that make use of GTK+'s height for width geometry management system. First,
  it's important to note that a container must prioritize one of its
  dimensions, that is to say that a widget or container can only have a
  GtkSizeRequestMode that is GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH or
  GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT. However, every widget and container must
  be able to respond to the APIs for both dimensions, i.e. even if a widget
  has a request mode that is height-for-width, it is possible that its parent
  will request its sizes using the width-for-height APIs.

  To ensure that everything works properly, here are some guidelines to follow
  when implementing height-for-width (or width-for-height) containers.

  Each request mode involves 2 virtual methods. Height-for-width apis run
  through gtk_widget_get_preferred_width() and then through
  gtk_widget_get_preferred_height_for_width(). When handling requests in the
  opposite GtkSizeRequestMode it is important that every widget request at
  least enough space to display all of its content at all times.

  When gtk_widget_get_preferred_height() is called on a container that is
  height-for-width, the container must return the height for its minimum
  width. This is easily achieved by simply calling the reverse apis
  implemented for itself as follows:
  @begin{pre}
  static void
  foo_container_get_preferred_height (GtkWidget *widget,
                                      gint *min_height, gint *nat_height)
  {
     if (i_am_in_height_for_width_mode)
       {
         gint min_width;
 
         GTK_WIDGET_GET_CLASS (widget)->
                    get_preferred_width (widget, &min_width, NULL);
         GTK_WIDGET_GET_CLASS (widget)->
                    get_preferred_height_for_width (widget,
                                                    min_width,
                                                    min_height, nat_height);
       @}
     else
       {
         ... many containers support both request modes, execute the real
         width-for-height request here by returning the collective heights of
         all widgets that are stacked vertically (or whatever is appropriate
         for this container) ...
       @}
  @}
  @end{pre}
  Similarly, when gtk_widget_get_preferred_width_for_height() is called for a
  container or widget that is height-for-width, it then only needs to return
  the base minimum width like so:
  @begin{pre}
  static void
  foo_container_get_preferred_width_for_height (GtkWidget *widget,
                                                gint for_height,
                                                gint *min_width,
                                                gint *nat_width)
  {
     if (i_am_in_height_for_width_mode)
       {
         GTK_WIDGET_GET_CLASS (widget)->
                    get_preferred_width (widget, min_width, nat_width);
       @}
     else
       {
         ... execute the real width-for-height request here based on the
         required width of the children collectively if the container were to
         be allocated the said height ...
       @}
  @}
  @end{pre}
  Height for width requests are generally implemented in terms of a virtual
  allocation of widgets in the input orientation. Assuming an height-for-width
  request mode, a container would implement the
  get_preferred_height_for_width() virtual function by first calling
  gtk_widget_get_preferred_width() for each of its children.

  For each potential group of children that are lined up horizontally, the
  values returned by gtk_widget_get_preferred_width() should be collected in
  an array of GtkRequestedSize structures. Any child spacing should be removed
  from the input for_width and then the collective size should be allocated
  using the gtk_distribute_natural_allocation() convenience function.

  The container will then move on to request the preferred height for each
  child by using gtk_widget_get_preferred_height_for_width() and using the
  sizes stored in the GtkRequestedSize array.

  To allocate a height-for-width container, it's again important to consider
  that a container must prioritize one dimension over the other. So if a
  container is a height-for-width container it must first allocate all widgets
  horizontally using a GtkRequestedSize array and
  gtk_distribute_natural_allocation() and then add any extra space (if and
  where appropriate) for the widget to expand.

  After adding all the expand space, the container assumes it was allocated
  sufficient height to fit all of its content. At this time, the container
  must use the total horizontal sizes of each widget to request the
  height-for-width of each of its children and store the requests in a
  GtkRequestedSize array for any widgets that stack vertically (for tabular
  containers this can be generalized into the heights and widths of rows and
  columns). The vertical space must then again be distributed using
  gtk_distribute_natural_allocation() while this time considering the
  allocated height of the widget minus any vertical spacing that the container
  adds. Then vertical expand space should be added where appropriate and
  available and the container should go on to actually allocating the child
  widgets.

  See GtkWidget's geometry management section to learn more about implementing
  height-for-width geometry management for widgets.

  @b{Child properties}

  GtkContainer introduces child properties. These are object properties that
  are not specific to either the container or the contained widget, but rather
  to their relation. Typical examples of child properties are the position or
  pack-type of a widget which is contained in a GtkBox.

  Use gtk_container_class_install_child_property() to install child properties
  for a container class and gtk_container_class_find_child_property() or
  gtk_container_class_list_child_properties() to get information about
  existing child properties.

  To set the value of a child property, use
  gtk_container_child_set_property(), gtk_container_child_set() or
  gtk_container_child_set_valist(). To obtain the value of a child property,
  use gtk_container_child_get_property(), gtk_container_child_get() or
  gtk_container_child_get_valist(). To emit notification about child property
  changes, use gtk_widget_child_notify().

  @b{GtkContainer as GtkBuildable}

  The GtkContainer implementation of the GtkBuildable interface supports a
  <packing> element for children, which can contain multiple <property>
  elements that specify child properties for the child.

  Example 105. Child properties in UI definitions
  @begin{pre}
  <object class=\"GtkVBox\">
    <child>
      <object class=\"GtkLabel\"/>
      <packing>
        <property name=\"pack-type\">start</property>
      </packing>
    </child>
  </object>
  @end{pre}
  Since 2.16, child properties can also be marked as translatable using the
  same \"translatable\", \"comments\" and \"context\" attributes that are used
  for regular properties.
  @begin[Signal Details]{dictionary}
    @b{The \"add\" signal}
    @begin{pre}
 void user_function (GtkContainer *container,
                    GtkWidget    *widget,
                    gpointer      user_data)      : Run First
    @end{pre}
    @b{The \"check-resize\" signal}
    @begin{pre}
 void user_function (GtkContainer *container,
                     gpointer      user_data)      : Run Last
    @end{pre}
    @b{The \"remove\" signal}
    @begin{pre}
 void user_function (GtkContainer *container,
                     GtkWidget    *widget,
                     gpointer      user_data)      : Run First
    @end{pre}
    @b{The \"set-focus-child\" signal}
    @begin{pre}
 void user_function (GtkContainer *container,
                     GtkWidget    *widget,
                     gpointer      user_data)      : Run First
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-container-border-width}
  @see-slot{gtk-container-child}
  @see-slot{gtk-container-resize-mode}
")

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

;;; --- gtk-container-add ------------------------------------------------------

(setf (documentation 'gtk-container-add 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[widget]{a widget to be placed inside container}
  @short{Adds widget to container.}
  Typically used for simple containers such as GtkWindow, GtkFrame, or
  GtkButton; for more complicated layout containers such as GtkBox or GtkGrid,
  this function will pick default packing parameters that may not be correct. So
  consider functions such as gtk_box_pack_start() and gtk_grid_attach() as an
  alternative to gtk_container_add() in those cases. A widget may be added to
  only one container at a time; you can't place the same widget inside two
  different containers.")

;;; --- gtk-container-remove ---------------------------------------------------

(setf (documentation 'gtk-container-remove 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[widget]{a current child of container}
  @short{Removes widget from container.}
  widget must be inside container. Note that container will own a reference to
  widget, and that this may be the last reference held; so removing a widget
  from its container can destroy that widget. If you want to use widget again,
  you need to add a reference to it while it's not inside a container, using
  g_object_ref(). If you don't want to use widget again it's usually more
  efficient to simply destroy it directly using gtk_widget_destroy() since this
  will remove it from the container and help break any circular reference count
  cycles.")

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

;;; --- gtk-container-check-resize ---------------------------------------------

(setf (documentation 'gtk-container-check-resize 'function)
 "@version{2013-1-4}
  @short{undocumented}")

;;; --- gtk-container-foreach --------------------------------------------------

(setf (documentation 'gtk-container-foreach 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[function]{a Lisp function which is passed as a callback to the C
    function @code{gtk_container_foreach()}}
  @begin{short}
    Invokes callback on each non-internal child of container.
  @end{short}
  See gtk_container_forall() for details on what constitutes an \"internal\"
  child. Most applications should use @sym{gtk-container-foreach}, rather than
  gtk_container_forall().")

;;; --- gtk-container-get-children ---------------------------------------------

(setf (documentation 'gtk-container-get-children 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{a newly-allocated list of the container's non-internal children}
  @short{Returns the container's non-internal children.}
  See gtk_container_forall() for details on what constitutes an \"internal\"
  child.")

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_path_for_child ()
;;;
;;; GtkWidgetPath * gtk_container_get_path_for_child (GtkContainer *container,
;;;                                                   GtkWidget *child);
;;;
;;; Returns a newly created widget path representing all the widget hierarchy
;;; from the toplevel down to and including child.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a child of container
;;;
;;; Returns :
;;;     A newly created GtkWidgetPath
;;; ----------------------------------------------------------------------------

;;; --- gtk-container-set-reallocate-redraws -----------------------------------

(setf (documentation 'gtk-container-set-reallocate-redraws 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[needs-redraws]{the new value for the container's reallocate_redraws
    flag}
  @begin{short}
    Sets the reallocate_redraws flag of the container to the given value.
  @end{short}

  Containers requesting reallocation redraws get automatically redrawn if any
  of their children changed allocation.")

;;; --- gtk-container-get-focus-child ------------------------------------------

(setf (documentation 'gtk-container-get-focus-child 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{The child widget which will receive the focus inside container when
    the conatiner is focussed, or NULL if none is set.}
  @short{Returns the current focus child widget inside container.}
  This is not the currently focused widget. That can be obtained by calling
  @fun{gtk-window-get-focus}.

  Since 2.14
  @see-function{gtk-window-get-focus}")

;;; --- gtk-container-set-focus-child ------------------------------------------

(setf (documentation 'gtk-container-set-focus-child 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[child]{a GtkWidget, or NULL}
  @short{Sets, or unsets if child is NULL, the focused child of container.}

  This function emits the GtkContainer::set_focus_child signal of container.
  Implementations of GtkContainer can override the default behaviour by
  overriding the class closure of this signal.

  This is function is mostly meant to be used by widgets. Applications can use
  gtk_widget_grab_focus() to manualy set the focus to a specific widget.")

;;; --- gtk-container-get-focus-vadjustment ------------------------------------

(setf (documentation 'gtk-container-get-focus-vadjustment 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{the vertical focus adjustment, or NULL if none has been set}
  @short{Retrieves the vertical focus adjustment for the container.}
  See @fun{gtk-container-set-focus-vadjustment}.
  @see-function{gtk-container-set-focus-vadjustment}")

;;; --- gtk-container-set-focus-vadjustment ------------------------------------

(setf (documentation 'gtk-container-set-focus-vadjustment 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[adjustment]{an adjustment which should be adjusted when the focus is
    moved among the descendents of container}
  @begin{short}
    Hooks up an adjustment to focus handling in a container, so when a child of
    the container is focused, the adjustment is scrolled to show that widget.
  @end{short}
  This function sets the vertical alignment. See
  @fun{gtk-scrolled-window-get-vadjustment} for a typical way of obtaining the
  adjustment and @fun{gtk-container-set-focus-hadjustment} for setting the
  horizontal adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the container.")

;;; --- gtk-container-get-focus-hadjustment ------------------------------------

(setf (documentation 'gtk-container-get-focus-hadjustment 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{the horizontal focus adjustment, or NULL if none has been set}
  @short{Retrieves the horizontal focus adjustment for the container.}
  See @fun{gtk-container-set-focus-hadjustment}.")

;;; --- gtk-container-set-focus-hadjustment ------------------------------------

(setf (documentation 'gtk-container-set-focus-hadjustment 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[adjustment]{an adjustment which should be adjusted when the focus is
    moved among the descendents of container}
  @begin{short}
    Hooks up an adjustment to focus handling in a container, so when a child of
    the container is focused, the adjustment is scrolled to show that widget.
  @end{short}
  This function sets the horizontal alignment. See
  gtk_scrolled_window_get_hadjustment() for a typical way of obtaining the
  adjustment and gtk_container_set_focus_vadjustment() for setting the
  vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the container.")

;;; --- gtk-container-resize-children ------------------------------------------

(setf (documentation 'gtk-container-resize-children 'function)
 "@version{2013-1-4}
  @short{undocumented}")

;;; --- gtk-container-child-type -----------------------------------------------

(setf (documentation 'gtk-container-child-type 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{a GType}
  @short{Returns the type of the children supported by the container.}

  Note that this may return G_TYPE_NONE to indicate that no more children can
  be added, e.g. for a GtkPaned which already has two children.")

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get ()
;;;
;;; void gtk_container_child_get (GtkContainer *container,
;;;                               GtkWidget *child,
;;;                               const gchar *first_prop_name,
;;;                               ...);
;;;
;;; Gets the values of one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_prop_name :
;;;     the name of the first property to get
;;;
;;; ... :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set ()
;;;
;;; void gtk_container_child_set (GtkContainer *container,
;;;                               GtkWidget *child,
;;;                               const gchar *first_prop_name,
;;;                               ...);
;;;
;;; Sets one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_prop_name :
;;;     the name of the first property to set
;;;
;;; ... :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get_property ()
;;;
;;; void gtk_container_child_get_property (GtkContainer *container,
;;;                                        GtkWidget *child,
;;;                                        const gchar *property_name,
;;;                                        GValue *value);
;;;
;;; Gets the value of a child property for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; property_name :
;;;     the name of the property to get
;;;
;;; value :
;;;     a location to return the value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set_property ()
;;;
;;; void gtk_container_child_set_property (GtkContainer *container,
;;;                                        GtkWidget *child,
;;;                                        const gchar *property_name,
;;;                                        const GValue *value);
;;;
;;; Sets a child property for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; property_name :
;;;     the name of the property to set
;;;
;;; value :
;;;     the value to set the property to
;;; ----------------------------------------------------------------------------

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
;;;
;;; void gtk_container_child_notify (GtkContainer *container,
;;;                                  GtkWidget *child,
;;;                                  const gchar *child_property);
;;;
;;; Emits a "child-notify" signal for the child property child_property on
;;; widget.
;;;
;;; This is an analogue of g_object_notify() for child properties.
;;;
;;; Also see gtk_widget_child_notify().
;;;
;;; container :
;;;     the GtkContainer
;;;
;;; child :
;;;     the child widget
;;;
;;; child_property :
;;;     the name of a child property installed on the class of container
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; --- gtk-container-forall ---------------------------------------------------

(setf (documentation 'gtk-container-forall 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[callback]{a callback}
  @arguemnt[callback_data]{callback user data}
  @begin{short}
    Invokes callback on each child of container, including children that are
    considered \"internal\" (implementation details of the container).
  @end{short}
  \"Internal\" children generally weren't added by the user of the container,
  but were added by the container implementation itself. Most applications
  should use gtk_container_foreach(), rather than gtk_container_forall().")

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
;;; gtk_container_get_focus_chain ()
;;;
;;; gboolean gtk_container_get_focus_chain (GtkContainer *container,
;;;                                         GList **focusable_widgets);
;;;
;;; Retrieves the focus chain of the container, if one has been set explicitly.
;;; If no focus chain has been explicitly set, GTK+ computes the focus chain
;;; based on the positions of the children. In that case, GTK+ stores NULL in
;;; focusable_widgets and returns FALSE.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; focusable_widgets :
;;;     location to store the focus chain of the container, or NULL. You should
;;;     free this list using g_list_free() when you are done with it, however no
;;;     additional reference count is added to the individual widgets in the
;;;     focus chain.
;;;
;;; Returns :
;;;     TRUE if the focus chain of the container has been set explicitly.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_set_focus_chain ()
;;;
;;; void gtk_container_set_focus_chain (GtkContainer *container,
;;;                                     GList *focusable_widgets);
;;;
;;; Sets a focus chain, overriding the one computed automatically by GTK+.
;;;
;;; In principle each widget in the chain should be a descendant of the
;;; container, but this is not enforced by this method, since it's allowed to
;;; set the focus chain before you pack the widgets, or have a widget in the
;;; chain that isn't always packed. The necessary checks are done when the focus
;;; chain is actually traversed.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; focusable_widgets :
;;;     the new focus chain
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_unset_focus_chain ()
;;;
;;; void gtk_container_unset_focus_chain (GtkContainer *container);
;;;
;;; Removes a focus chain explicitly set with gtk_container_set_focus_chain().
;;;
;;; container :
;;;     a GtkContainer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_find_child_property ()
;;;
;;; GParamSpec * gtk_container_class_find_child_property
;;;                                                 (GObjectClass *cclass,
;;;                                                  const gchar *property_name)
;;;
;;; Finds a child property of a container class by name.
;;;
;;; cclass :
;;;     a GtkContainerClass
;;;
;;; property_name :
;;;     the name of the child property to find
;;;
;;; Returns :
;;;     the GParamSpec of the child property or NULL if class has no child
;;;     property with that name
;;; ----------------------------------------------------------------------------

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
;;; gtk_container_class_list_child_properties ()
;;;
;;; GParamSpec ** gtk_container_class_list_child_properties
;;;                                                       (GObjectClass *cclass,
;;;                                                        guint *n_properties);
;;;
;;; Returns all child properties of a container class.
;;;
;;; cclass :
;;;     a GtkContainerClass
;;;
;;; n_properties :
;;;     location to return the number of child properties found
;;;
;;; Returns :
;;;     a newly allocated NULL-terminated array of GParamSpec*. The array must
;;;     be freed with g_free()
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;;
;;;     Accessors
;;;
;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-container-border-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-container-border-width 'function)
 "@version{2013-1-4}
  @begin{short}
    Accessor of the slot @arg{border-width} of the @class{gtk-container} class.
  @end{short}
  See the function @fun{gtk-container-set-border-width} for details.
  @see-function{gtk-container-set-border-width}")

;;; --- gtk-container-get-border-width -----------------------------------------

(setf (documentation 'gtk-container-get-border-width 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{the current border width}
  @short{Retrieves the border width of the container.}
  See @fun{gtk-container-set-border-width}.
  @see-function{gtk-container-set-border-width}")

;;; --- gtk-container-set-border-width -----------------------------------------

(setf (documentation 'gtk-container-set-border-width 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[border_width]{amount of blank space to leave outside the container.
    Valid values are in the range 0-65535 pixels.}
  @short{Sets the border width of the container.}

  The border width of a container is the amount of space to leave around the
  outside of the container. The only exception to this is GtkWindow; because
  toplevel windows can't leave space outside, they leave the space inside. The
  border is added on all sides of the container. To add space to only one
  side, one approach is to create a GtkAlignment widget, call
  gtk_widget_set_size_request() to give it a size, and place it on the side of
  the container as a spacer.")

;;; --- gtk-container-resize-mode ----------------------------------------------

(setf (gethash 'gtk-container-resize-mode atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-container-resize-mode 'function)
 "@version{2013-1-4}
  @begin{short}
    Accessor of the slot @arg{resize-mode} of the @class{gtk-container} class.
  @end{short}
  See the function @fun{gtk-container-set-resize-mode} for details.
  @see-function{gtk-container-set-resize-mode}")

;;; --- gtk-container-get-resize-mode ------------------------------------------

(setf (documentation 'gtk-container-get-resize-mode 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @return{the current resize mode}
  @short{Returns the resize mode for the container.}
  See @fun{gtk-container-set-resize-mode}.
  @see-function{gtk-container-set-resize-mode}")

;;; --- gtk-container-set-resize-mode ------------------------------------------

(setf (documentation 'gtk-container-set-resize-mode 'function)
 "@version{2013-1-4}
  @argument[container]{a GtkContainer}
  @argument[resize-mode]{the new resize mode}
  @short{Sets the resize mode for the container.}

  The resize mode of a container determines whether a resize request will be
  passed to the container's parent, queued for later execution or executed
  immediately.")

;;; --- End of file gtk.container.lisp -----------------------------------------

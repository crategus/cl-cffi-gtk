;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See http://www.gtk.org.
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

(in-package :gtk)

;;; --- gtk-widget -------------------------------------------------------------

(setf (documentation 'gtk-widget 'type)
 "@version{2012-12-29}
  @short{Base class for all widgets.}

    @sym{gtk-widget} is the base class all widgets in GTK+ derive from. It
    manages the widget lifecycle, states and style.

    @heading{Height-for-width Geometry Management}
    GTK+ uses a height-for-width (and width-for-height) geometry management
    system. Height-for-width means that a widget can change how much vertical
    space it needs, depending on the amount of horizontal space that it is given
    (and similar for width-for-height). The most common example is a label that
    reflows to fill up the available width, wraps to fewer lines, and therefore
    needs less height.

    Height-for-width geometry management is implemented in GTK+ by way of five
    virtual methods:
    @begin{pre}
 GtkWidgetClass.get_request_mode()
 GtkWidgetClass.get_preferred_width()
 GtkWidgetClass.get_preferred_height()
 GtkWidgetClass.get_preferred_height_for_width()
 GtkWidgetClass.get_preferred_width_for_height()
    @end{pre}
    There are some important things to keep in mind when implementing
    height-for-width and when using it in container implementations.

    The geometry management system will query a widget hierarchy in only one
    orientation at a time. When widgets are initially queried for their minimum
    sizes it is generally done in two initial passes in the
    @symbol{gtk-size-request-mode} chosen by the toplevel.

    For example, when queried in the normal @code{:height-for-width}
    mode: First, the default minimum and natural width for each widget in the
    interface will be computed using @fun{gtk-widget-get-preferred-width}.
    Because the preferred widths for each container depend on the preferred
    widths of their children, this information propagates up the hierarchy, and
    finally a minimum and natural width is determined for the entire toplevel.
    Next, the toplevel will use the minimum width to query for the minimum
    height contextual to that width using
    @fun{gtk-widget-get-preferred-height-for-width}, which will also be a highly
    recursive operation. The minimum height for the minimum width is normally
    used to set the minimum size constraint on the toplevel (unless
    @fun{gtk-window-set-geometry-hints} is explicitly used instead).

    After the toplevel window has initially requested its size in both
    dimensions it can go on to allocate itself a reasonable size (or a size
    previously specified with @fun{gtk-window-set-default-size}). During the
    recursive allocation process it's important to note that request cycles will
    be recursively executed while container widgets allocate their children.
    Each container widget, once allocated a size, will go on to first share the
    space in one orientation among its children and then request each child's
    height for its target allocated width or its width for allocated height,
    depending. In this way a @sym{gtk-widget} will typically be requested its
    size a number of times before actually being allocated a size. The size a
    widget is finally allocated can of course differ from the size it has
    requested. For this reason, @sym{gtk-widget} caches a small number of
    results to avoid re-querying for the same sizes in one allocation cycle.

    See @class{gtk-container}'s geometry management section to learn more about
    how height-for-width allocations are performed by container widgets.

    If a widget does move content around to intelligently use up the allocated
    size then it must support the request in both
    @symbol{gtk-size-request-mode}'s even if the widget in question only trades
    sizes in a single orientation.

    For instance, a @class{gtk-label} that does height-for-width word wrapping
    will not expect to have @code{GtkWidgetClass.get_preferred_height()} called
    because that call is specific to a width-for-height request. In this case
    the label must return the height required for its own minimum possible
    width. By following this rule any widget that handles height-for-width or
    width-for-height requests will always be allocated at least enough space to
    fit its own content.

    Here are some examples of how a @code{:height-for-width} widget generally
    deals with width-for-height requests, for
    @code{GtkWidgetClass.get_preferred_height()} it will do:
    @begin{pre}
 static void
 foo_widget_get_preferred_height (GtkWidget *widget,
                                  gint *min_height, gint *nat_height)
 {
    if (i_am_in_height_for_width_mode)
      {
        gint min_width;

        GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
                                                            &min_width,
                                                            NULL);
        GTK_WIDGET_GET_CLASS (widget)->
                 get_preferred_height_for_width (widget,
                                                 min_width,
                                                 min_height,
                                                 nat_height);
      @}
    else
      {
         ... some widgets do both. For instance, if a GtkLabel is rotated to
         90 degrees it will return the minimum and natural height for the
         rotated label here.
      @}
 @}
    @end{pre}
    And in @code{GtkWidgetClass.get_preferred_width_for_height()} it will simply
    return the minimum and natural width:
    @begin{pre}
 static void
 foo_widget_get_preferred_width_for_height (GtkWidget *widget,
                                            gint for_height,
                                            gint *min_width,
                                            gint *nat_width)
 {
    if (i_am_in_height_for_width_mode)
      {
        GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
                                                            min_width,
                                                            nat_width);
      @}
    else
      {
         ... again if a widget is sometimes operating in width-for-height
         mode (like a rotated GtkLabel) it can go ahead and do its real width
         for height calculation here.
      @}
 @}
    @end{pre}
    Often a widget needs to get its own request during size request or
    allocation. For example, when computing height it may need to also compute
    width. Or when deciding how to use an allocation, the widget may need to
    know its natural size. In these cases, the widget should be careful to call
    its virtual methods directly, like this:

    Example 101. Widget calling its own size request method.
    @begin{pre}
 GTK_WIDGET_GET_CLASS(widget)->get_preferred_width (widget),
                               &min, &natural);
    @end{pre}
    It will not work to use the wrapper functions, such as
    @fun{gtk-widget-get-preferred-width} inside your own size request
    implementation. These return a request adjusted by @class{gtk-size-group}
    and by the @code{GtkWidgetClass.adjust_size_request()} virtual method. If a
    widget used the wrappers inside its virtual method implementations, then the
    adjustments (such as widget margins) would be applied twice. GTK+ therefore
    does not allow this and will warn if you try to do it.

    Of course if you are getting the size request for another widget, such as a
    child of a container, you must use the wrapper APIs. Otherwise, you would
    not properly consider widget margins, @class{gtk-size-group}, and so forth.

   @heading{Style Properties}
   @sym{gtk-widget} introduces style properties - these are basically object
   properties that are stored not on the object, but in the style object
   associated to the widget. Style properties are set in resource files. This
   mechanism is used for configuring such things as the location of the
   scrollbar arrows through the theme, giving theme authors more control over
   the look of applications without the need to write a theme engine in C.

   Use @fun{gtk-widget-class-install-style-property} to install style properties
   for a widget class, @fun{gtk-widget-class-find-style-property} or
   @fun{gtk-widget-class-list-style-properties} to get information about
   existing style properties and @fun{gtk-widget-style-get-property},
   @fun{gtk-widget-style-get} or @fun{gtk-widget-style-get-valist} to obtain the
   value of a style property.

    @heading{gtk-widget as gtk-buildable}
    The @sym{gtk-widget} implementation of the @class{gtk-buildable} interface
    supports a custom @code{<accelerator>} element, which has attributes named
    key, modifiers and signal and allows to specify accelerators.

    Example 102. A UI definition fragment specifying an accelerator
    @begin{pre}
 <object class=\"GtkButton\">
   <accelerator key=\"q\" modifiers=\"GDK_CONTROL_MASK\" signal=\"clicked\"/>
 </object>
    @end{pre}
    In addition to accelerators, @sym{gtk-widget} also support a custom
    @code{<accessible>} element, which supports actions and relations.
    Properties on the accessible implementation of an object can be set by
    accessing the internal child @arg{\"accessible\"} of a @sym{gtk-widget}.

    Example 103. A UI definition fragment specifying an accessible
    @begin{pre}
 <object class=\"GtkButton\" id=\"label1\"/>
   <property name=\"label\">I am a Label for a Button</property>
  </object>
 <object class=\"GtkButton\" id=\"button1\">
   <accessibility>
     <action action_name=\"click\"
             translatable=\"yes\">Click the button.</action>
     <relation target=\"label1\" type=\"labelled-by\"/>
   </accessibility>
   <child internal-child=\"accessible\">
     <object class=\"AtkObject\" id=\"a11y-button1\">
       <property name=\"AtkObject::name\">Clickable Button</property>
     </object>
   </child>
 </object>
    @end{pre}
    Finally, @sym{gtk-widget} allows style information such as style classes to
    be associated with widgets, using the custom @code{<style>} element:

    Example 104. A UI definition fragment specifying an style class
    @begin{pre}
 <object class=\"GtkButton\" id=\"button1\">
   <style>
     <class name=\"my-special-button-class\"/>
     <class name=\"dark-button\"/>
   </style>
 </object>
    @end{pre}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"cursor-aspect-ratio\" style property}
    @code{\"cursor-aspect-ratio\"} @code{gfloat} (Read)@br{}
    Aspect ratio with which to draw insertion cursor.@br{}
    Allowed values: @code{[0,1]}@br{}
    Default value: @code{0.04}

    @subheading{The \"cursor-color\" style property}
    @code{\"cursor-color\"} @class{gdk-color} (Read).@br{}
    Color with which to draw insertion cursor.

    @subheading{The \"focus-line-pattern\" style property}
    @code{\"focus-line-pattern\"} @code{gchar*} (Read)@br{}
    Dash pattern used to draw the focus indicator.@br{}
    Default value: @code{\"\001\001\"}

    @subheading{The \"focus-line-width\" style property}
    @code{\"focus-line-width\"} @code{gint} (Read)@br{}
    Width, in pixels, of the focus indicator line.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{1}

    @subheading{The \"focus-padding\" style property}
    @code{\"focus-padding\"} @code{gint} (Read)@br{}
    Width, in pixels, between focus indicator and the widget 'box'.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{1}

    @subheading{The \"interior-focus\" style property}
    @code{\"interior-focus\"} @code{gboolean} (Read)@br{}
    Whether to draw the focus indicator inside widgets.@br{}
    Default value: @arg{true}

    @subheading{The \"link-color\" style property}
    @code{\"link-color\"} @class{gdk-color} (Read)@br{}
    The @code{\"link-color\"} style property defines the color of unvisited
    links.@br{}
    Since 2.10

    @subheading{The \"scroll-arrow-hlength\" style property}
    @code{\"scroll-arrow-hlength\"} @code{gint} (Read)@br{}
    The @code{\"scroll-arrow-hlength\"} style property defines the length of
    horizontal scroll arrows.@br{}
    Allowed values: @code{>= 1}
    Default value: @code{16}@br{}
    Since 2.10

    @subheading{The \"scroll-arrow-vlength\" style property}
    @code{\"scroll-arrow-vlength\"} @code{gint} (Read)@br{}
    The @code{\"scroll-arrow-vlength\"} style property defines the length of
    vertical scroll arrows.@br{}
    Allowed values: @code{>= 1}
    Default value: @code{16}
    Since 2.10

    @subheading{The \"secondary-cursor-color\" style property}
    @code{\"secondary-cursor-color\"} @class{gdk-color} (Read)@br{}
    Color with which to draw the secondary insertion cursor when editing mixed
    right-to-left and left-to-right text.

    @subheading{The \"separator-height\" style property}
    @code{\"separator-height\"} @code{gint} (Read)@br{}
    The @code{\"separator-height\"} style property defines the height of
    separators. This property only takes effect if @code{\"wide-separators\"} is
    @arg{true}.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{0}@br{}
    Since 2.10

    @subheading{The \"separator-width\" style property}
    @code{\"separator-width\"} @code{gint} (Read)@br{}
    The @code{\"separator-width\"} style property defines the width of
    separators. This property only takes effect if @code{\"wide-separators\"} is
    @arg{true}.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{0}@br{}
    Since 2.10

    @subheading{The \"visited-link-color\" style property}
    @code{\"visited-link-color\"} @class{gdk-color} (Read)@br{}
    The @code{\"visited-link-color\"} style property defines the color of
    visited links.@br{}
    Since 2.10

    @subheading{The \"wide-separators\" style property}
    @code{\"wide-separators\"} @code{gboolean} (Read)@br{}
    The @code{\"wide-separators\"} style property defines whether separators
    have configurable width and should be drawn using a box instead of a
    line.@br{}
    Default value: @code{nil}@br{}
    Since 2.10

    @subheading{The \"window-dragging\" style property}
    @code{\"window-dragging\"} @code{gboolean} (Read)@br{}
    Whether windows can be dragged by clicking on empty areas.@br{}
    Default value: @code{nil}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @b{The \"accel-closures-changed\" signal}@br{}
    @begin{pre}
 void user_function (GtkWidget *widget, gpointer user_data)
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[user-data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"button-press-event\" signal}@br{}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
   The @code{::button-press-event} signal will be emitted when a button
   (typically from a mouse) is pressed.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_BUTTON_PRESS_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the GdkEventButton which triggered this signal}
      @entry[user-data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"button-release-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The @code{::button-release-event} signal will be emitted when a button
    (typically from a mouse) is released.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_BUTTON_RELEASE_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the GdkEventButton which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"can-activate-accel\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         guint      signal_id,
                         gpointer   user_data)      : Run Last
    @end{pre}
    Determines whether an accelerator that activates the signal identified by
    signal_id can currently be activated. This signal is present to allow
    applications and derived widgets to override the default GtkWidget handling
    for determining whether an accelerator can be activated.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[signal_id]{the ID of a signal installed on widget}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE if the signal can be activated.}
    @end{table}

    @b{The \"child-notify\" signal}
    @begin{pre}
 void user_function (GtkWidget  *widget,
                     GParamSpec *pspec,
                     gpointer    user_data)      : No Hooks
    @end{pre}
    The @code{::child-notify} signal is emitted for each child property that has
    changed on an object. The signal's detail holds the property name.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[pspec]{the GParamSpec of the changed child property}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"composited-changed\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     gpointer   user_data)      : Action
    @end{pre}
    The ::composited-changed signal is emitted when the composited status of
    widgets screen changes. See gdk_screen_is_composited().
    @begin[code]{table}
      @entry[widget]{the object on which the signal is emitted}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"configure-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The @code{::configure-event} signal will be emitted when the size, position
    or stacking of the widget's window has changed.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
    for all new windows.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventConfigure which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"damage-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    Emitted when a redirected window belonging to widget gets drawn into. The
    region/area members of the event shows what area of the redirected drawable
    was drawn into.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventExpose event}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}
    Since 2.14

    @b{The \"delete-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The @code{::delete-event} signal is emitted if a user requests that a
    toplevel window is closed. The default handler for this signal destroys the
    window. Connecting gtk_widget_hide_on_delete() to this signal will cause the
    window to be hidden instead, so that it can later be shown again without
    reconstructing it.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the event which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"destroy\" signal}
    @begin{pre}
 void user_function (GtkWidget *object,
                     gpointer   user_data)      : No Hooks
    @end{pre}
    Signals that all holders of a reference to the widget should release the
    reference that they hold. May result in finalization of the widget if all
    references are released.
    @begin[code]{table}
      @entry[object]{the object which received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"destroy-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::destroy-event signal is emitted when a GdkWindow is destroyed. You
    rarely get this signal, because most widgets disconnect themselves from
    their window before they destroy it, so no widget owns the window at destroy
    time.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
    for all new windows.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the event which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"direction-changed\" signal}
    @begin{pre}
 void user_function (GtkWidget       *widget,
                     GtkTextDirection previous_direction,
                     gpointer         user_data)               : Run First
    @end{pre}
    The ::direction-changed signal is emitted when the text direction of a
    widget changes.
    @begin[code]{table}
      @entry[widget]{the object on which the signal is emitted}
      @entry[previous_direction]{the previous text direction of widget}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-begin\" signal}
    @begin{pre}
 void user_function (GtkWidget      *widget,
                     GdkDragContext *drag_context,
                     gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-begin signal is emitted on the drag source when a drag is
    started. A typical reason to connect to this signal is to set up a custom
    drag icon with gtk_drag_source_set_icon().

    Note that some widgets set up a drag icon in the default handler of this
    signal, so you may have to use g_signal_connect_after() to override what the
    default handler did.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-data-delete\" signal}
    @begin{pre}
 void user_function (GtkWidget      *widget,
                     GdkDragContext *drag_context,
                     gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-data-delete signal is emitted on the drag source when a drag with
    the action GDK_ACTION_MOVE is successfully completed. The signal handler is
    responsible for deleting the data that has been dropped. What \"delete\"
    means depends on the context of the drag operation.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-data-get\" signal}
    @begin{pre}
 void user_function (GtkWidget        *widget,
                     GdkDragContext   *drag_context,
                     GtkSelectionData *data,
                     guint             info,
                     guint             time,
                     gpointer          user_data)         : Run Last
    @end{pre}
    The ::drag-data-get signal is emitted on the drag source when the drop site
    requests the data which is dragged. It is the responsibility of the signal
    handler to fill data with the data in the format which is indicated by info.
    See gtk_selection_data_set() and gtk_selection_data_set_text().
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[data]{the GtkSelectionData to be filled with the dragged data}
      @entry[info]{the info that has been registered with the target in the
        GtkTargetList}
      @entry[time]{the timestamp at which the data was requested}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-data-received\" signal}
    @begin{pre}
 void user_function (GtkWidget        *widget,
                     GdkDragContext   *drag_context,
                     gint              x,
                     gint              y,
                     GtkSelectionData *data,
                     guint             info,
                     guint             time,
                     gpointer          user_data)         : Run Last
    @end{pre}
    The ::drag-data-received signal is emitted on the drop site when the dragged
    data has been received. If the data was received in order to determine
    whether the drop will be accepted, the handler is expected to call
    gdk_drag_status() and not finish the drag. If the data was received in
    response to a \"drag-drop\" signal (and this is the last target to be
    received), the handler for this signal is expected to process the received
    data and then call gtk_drag_finish(), setting the success parameter
    depending on whether the data was processed successfully.

    The handler may inspect and modify drag_context->action before calling
    gtk_drag_finish(), e.g. to implement GDK_ACTION_ASK as shown in the
    following example:
    @begin{pre}
 void
 drag_data_received (GtkWidget          *widget,
                     GdkDragContext     *drag_context,
                     gint                x,
                     gint                y,
                     GtkSelectionData   *data,
                     guint               info,
                     guint               time)
 {
   if ((data->length >= 0) && (data->format == 8))
     {
       if (drag_context->action == GDK_ACTION_ASK)
         {
           GtkWidget *dialog;
           gint response;
           dialog = gtk_message_dialog_new (NULL,
                                            GTK_DIALOG_MODAL |
                                            GTK_DIALOG_DESTROY_WITH_PARENT,
                                            GTK_MESSAGE_INFO,
                                            GTK_BUTTONS_YES_NO,
                                            \"Move the data ?\n\");
           response = gtk_dialog_run (GTK_DIALOG (dialog));
           gtk_widget_destroy (dialog);

           if (response == GTK_RESPONSE_YES)
             drag_context->action = GDK_ACTION_MOVE;
           else
             drag_context->action = GDK_ACTION_COPY;
          @}

       gtk_drag_finish (drag_context, TRUE, FALSE, time);
       return;
     @}

    gtk_drag_finish (drag_context, FALSE, FALSE, time);
  @}
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[x]{where the drop happened}
      @entry[y]{where the drop happened}
      @entry[data]{the received data}
      @entry[info]{the info that has been registered with the target in the
        GtkTargetList}
      @entry[time]{the timestamp at which the data was received}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-drop\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget      *widget,
                         GdkDragContext *drag_context,
                         gint            x,
                         gint            y,
                         guint           time,
                         gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-drop signal is emitted on the drop site when the user drops the
    data onto the widget. The signal handler must determine whether the cursor
    position is in a drop zone or not. If it is not in a drop zone, it returns
    FALSE and no further processing is necessary. Otherwise, the handler returns
    TRUE. In this case, the handler must ensure that gtk_drag_finish() is called
    to let the source know that the drop is done. The call to gtk_drag_finish()
    can be done either directly or in a \"drag-data-received\" handler which
    gets triggered by calling gtk_drag_get_data() to receive the data for one or
    more of the supported targets.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[x]{the x coordinate of the current cursor position}
      @entry[y]{the y coordinate of the current cursor position}
      @entry[time]{the timestamp of the motion event}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{whether the cursor position is in a drop zone}
    @end{table}

    @b{The \"drag-end\" signal}
    @begin{pre}
 void user_function (GtkWidget      *widget,
                     GdkDragContext *drag_context,
                     gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-end signal is emitted on the drag source when a drag is finished.
    A typical reason to connect to this signal is to undo things done in
    \"drag-begin\".
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-failed\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget      *widget,
                         GdkDragContext *drag_context,
                         GtkDragResult   result,
                         gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-failed signal is emitted on the drag source when a drag has
    failed. The signal handler may hook custom code to handle a failed DND
    operation based on the type of error, it returns TRUE is the failure has
    been already handled (not showing the default \"drag operation failed\"
    animation), otherwise it returns FALSE.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[result]{the result of the drag operation}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE if the failed drag operation has been already
        handled.}
    @end{table}
    Since 2.12

    @b{The \"drag-leave\" signal}
    @begin{pre}
 void user_function (GtkWidget      *widget,
                     GdkDragContext *drag_context,
                     guint           time,
                     gpointer        user_data)         : Run Last
    @end{pre}
    The ::drag-leave signal is emitted on the drop site when the cursor leaves
    the widget. A typical reason to connect to this signal is to undo things
    done in \"drag-motion\", e.g. undo highlighting with gtk_drag_unhighlight()
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[drag_context]{the drag context}
      @entry[time]{the timestamp of the motion event}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"drag-motion\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget      *widget,
                         GdkDragContext *drag_context,
                         gint            x,
                         gint            y,
                         guint           time,
                         gpointer        user_data)         : Run Last
    @end{pre}
    The drag-motion signal is emitted on the drop site when the user moves the
    cursor over the widget during a drag. The signal handler must determine
    whether the cursor position is in a drop zone or not. If it is not in a drop
    zone, it returns FALSE and no further processing is necessary. Otherwise,
    the handler returns TRUE. In this case, the handler is responsible for
    providing the necessary information for displaying feedback to the user, by
    calling gdk_drag_status().

    If the decision whether the drop will be accepted or rejected can't be made
    based solely on the cursor position and the type of the data, the handler
    may inspect the dragged data by calling gtk_drag_get_data() and defer the
    gdk_drag_status() call to the \"drag-data-received\" handler. Note that you
    cannot not pass GTK_DEST_DEFAULT_DROP, GTK_DEST_DEFAULT_MOTION or
    GTK_DEST_DEFAULT_ALL to gtk_drag_dest_set() when using the drag-motion
    signal that way.

    Also note that there is no drag-enter signal. The drag receiver has to keep
    track of whether he has received any drag-motion signals since the last
    \"drag-leave\" and if not, treat the drag-motion signal as an \"enter\"
    signal. Upon an \"enter\", the handler will typically highlight the drop
    site with gtk_drag_highlight().
    @begin{pre}
 static void
 drag_motion (GtkWidget *widget,
              GdkDragContext *context,
              gint x,
              gint y,
              guint time)
 {
   GdkAtom target;

   PrivateData *private_data = GET_PRIVATE_DATA (widget);

   if (!private_data->drag_highlight)
    {
      private_data->drag_highlight = 1;
      gtk_drag_highlight (widget);
    @}

   target = gtk_drag_dest_find_target (widget, context, NULL);
   if (target == GDK_NONE)
     gdk_drag_status (context, 0, time);
   else
    {
      private_data->pending_status = context->suggested_action;
      gtk_drag_get_data (widget, context, target, time);
    @}

   return TRUE;
 @}

 static void
 drag_data_received (GtkWidget        *widget,
                     GdkDragContext   *context,
                     gint              x,
                     gint              y,
                     GtkSelectionData *selection_data,
                     guint             info,
                     guint             time)
 {
   PrivateData *private_data = GET_PRIVATE_DATA (widget);

   if (private_data->suggested_action)
    {
      private_data->suggested_action = 0;

     /* We are getting this data due to a request in drag_motion,
      * rather than due to a request in drag_drop, so we are just
      * supposed to call gdk_drag_status (), not actually paste in
      * the data.
      */
      str = gtk_selection_data_get_text (selection_data);
      if (!data_is_acceptable (str))
        gdk_drag_status (context, 0, time);
      else
        gdk_drag_status (context, private_data->suggested_action, time);
    @}
   else
    {
      /* accept the drop */
    @}
 @}
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[drag_context]{the drag context}
      @entry[x]{the x coordinate of the current cursor position}
      @entry[y]{the y coordinate of the current cursor position}
      @entry[time]{the timestamp of the motion event}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{whether the cursor position is in a drop zone}
    @end{table}

    @b{The \"draw\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget    *widget,
                         CairoContext *cr,
                         gpointer      user_data)      : Run Last
    @end{pre}
    This signal is emitted when a widget is supposed to render itself.
    The widget's top left corner must be painted at the origin of the passed in
    context and be sized to the values returned by
    gtk_widget_get_allocated_width() and gtk_widget_get_allocated_height().

    Signal handlers connected to this signal can modify the cairo context passed
    as cr in any way they like and don't need to restore it. The signal emission
    takes care of calling cairo_save() before and cairo_restore() after invoking
    the handler.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[cr]{the cairo context to draw to}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 3.0

    @b{The \"enter-notify-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::enter-notify-event will be emitted when the pointer enters the
    widget's window.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_ENTER_NOTIFY_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventCrossing which triggered this signal}
      @entry[user-data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The GTK+ main loop will emit three signals for each GDK event delivered to a
    widget: one generic ::event signal, another, more specific, signal that
    matches the type of event delivered (e.g. \"key-press-event\") and finally a
    generic \"event-after\" signal.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the GdkEvent which triggered this signal}
      @entry[user-data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event and to cancel the emission of the second specific ::event signal.
        FALSE to propagate the event further and to allow the emission of the
        second signal. The ::event-after signal is emitted regardless of the
        return value.}
    @end{table}

    @b{The \"event-after\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     GdkEvent  *event,
                     gpointer   user_data)
    @end{pre}
    After the emission of the \"event\" signal and (optionally) the second more
    specific signal, ::event-after will be emitted regardless of the previous
    two signals handlers return values.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the GdkEvent which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"focus\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget       *widget,
                         GtkDirectionType direction,
                         gpointer         user_data)      : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"focus-in-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::focus-in-event signal will be emitted when the keyboard focus enters
    the widget's window.@br{}
    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_FOCUS_CHANGE_MASK mask.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventFocus which triggered this signal}
      @entry[user-data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"focus-out-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::focus-out-event signal will be emitted when the keyboard focus leaves
    the widget's window.@br{}
    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_FOCUS_CHANGE_MASK mask.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventFocus which triggered this signal}
      @entry[user-data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE topropagate the event further.}
    @end{table}

    @b{The \"grab-broken-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    Emitted when a pointer or keyboard grab on a window belonging to widget gets
    broken.

    On X11, this happens when the grab window becomes unviewable (i.e. it or one
    of its ancestors is unmapped), or if the same application grabs the pointer
    or keyboard again.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventGrabBroken event}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}
    Since 2.8

    @b{The \"grab-focus\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     gpointer   user_data)      : Action
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"grab-notify\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     gboolean   was_grabbed,
                     gpointer   user_data)        : Run First
    @end{pre}
    The ::grab-notify signal is emitted when a widget becomes shadowed by a GTK+
    grab (not a pointer or keyboard grab) on another widget, or when it becomes
    unshadowed due to a grab being removed.

    A widget is shadowed by a gtk_grab_add() when the topmost grab widget in the
    grab stack of its window group is not its ancestor.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[was_grabbed]{FALSE if the widget becomes shadowed, TRUE if it
        becomes unshadowed}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"hide\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     gpointer   user_data)      : Run First
    @end{pre}
    @begin[code]{table}
      @enty[widget]{the object which received the signal.}
      @entry[user-data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"hierarchy-changed\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     GtkWidget *previous_toplevel,
                     gpointer   user_data)              : Run Last
    @end{pre}
    The ::hierarchy-changed signal is emitted when the anchored state of a
    widget changes. A widget is anchored when its toplevel ancestor is a
    GtkWindow. This signal is emitted when a widget changes from un-anchored to
    anchored or vice-versa.
    @begin[code]{table}
      @entry[widget]{the object on which the signal is emitted}
      @entry[previous_toplevel]{the previous toplevel ancestor, or NULL if the
        widget was previously unanchored}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"key-press-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::key-press-event signal is emitted when a key is pressed. The signal
    emission will reoccur at the key-repeat rate when the key is kept pressed.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_KEY_PRESS_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @endtry[widget]{the object which received the signal}
      @entry[event]{the GdkEventKey which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"key-release-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::key-release-event signal is emitted when a key is released.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_KEY_RELEASE_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventKey which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"keynav-failed\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget       *widget,
                         GtkDirectionType direction,
                         gpointer         user_data)      : Run Last
    @end{pre}
    Gets emitted if keyboard navigation fails. See gtk_widget_keynav_failed()
    for details.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[direction]{the direction of movement}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE if stopping keyboard navigation is fine, FALSE if the
        emitting widget should try to handle the keyboard navigation attempt in
        its parent container(s).}
    @end{table}
    Since 2.12

    @b{The \"leave-notify-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::leave-notify-event will be emitted when the pointer leaves the
    widget's window.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_LEAVE_NOTIFY_MASK mask.

    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventCrossing which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"map\" signal}
    @begin{pre}
 void user_function (GtkWidget *widget,
                     gpointer   user_data)      : Run First
    @end{pre}
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"map-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::map-event signal will be emitted when the widget's window is mapped.
    A window is mapped when it becomes visible on the screen.
 
    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
    for all new windows.
    @begin[code]{table}
      @entry[widget]{the object which received the signal}
      @entry[event]{the GdkEventAny which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

    @b{The \"mnemonic-activate\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         gboolean   arg1,
                         gpointer   user_data)      : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[widget}{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"motion-notify-event\" signal}
    @begin{pre}
 gboolean user_function (GtkWidget *widget,
                         GdkEvent  *event,
                         gpointer   user_data)      : Run Last
    @end{pre}
    The ::motion-notify-event signal is emitted when the pointer moves over the
    widget's GdkWindow.

    To receive this signal, the GdkWindow associated to the widget needs to
    enable the GDK_POINTER_MOTION_MASK mask.
 
    This signal will be sent to the grab widget if there is one.
    @begin[code]{table}
      @entry[widget]{the object which received the signal.}
      @entry[event]{the GdkEventMotion which triggered this signal}
      @entry[user_data]{user data set when the signal handler was connected.}
      @entry[Returns]{TRUE to stop other handlers from being invoked for the
        event. FALSE to propagate the event further.}
    @end{table}

  @end{dictionary}
  @see-slot{gtk-widget-app-paintable}
  @see-slot{gtk-widget-can-default}
  @see-slot{gtk-widget-can-focus}
  @see-slot{gtk-widget-composite-child}
  @see-slot{gtk-widget-double-buffered}
  @see-slot{gtk-widget-events}
  @see-slot{gtk-widget-expand}
  @see-slot{gtk-widget-halign}
  @see-slot{gtk-widget-has-default}
  @see-slot{gtk-widget-has-focus}
  @see-slot{gtk-widget-has-tooltip}
  @see-slot{gtk-widget-height-request}
  @see-slot{gtk-widget-hexpand}
  @see-slot{gtk-widget-hexpand-set}
  @see-slot{gtk-widget-is-focus}
  @see-slot{gtk-widget-margin}
  @see-slot{gtk-widget-margin-bottom}
  @see-slot{gtk-widget-margin-left}
  @see-slot{gtk-widget-margin-right}
  @see-slot{gtk-widget-margin-top}
  @see-slot{gtk-widget-name}
  @see-slot{gtk-widget-no-show-all}
  @see-slot{gtk-widget-parent}
  @see-slot{gtk-widget-receives-default}
  @see-slot{gtk-widget-sensitive}
  @see-slot{gtk-widget-style}
  @see-slot{gtk-widget-tooltip-markup}
  @see-slot{gtk-widget-tooltip-text}
  @see-slot{gtk-widget-valign}
  @see-slot{gtk-widget-vexpand}
  @see-slot{gtk-widget-vexpand-set}
  @see-slot{gtk-widget-visible}
  @see-slot{gtk-widget-width-request}
  @see-slot{gtk-widget-window}")


;;; ----------------------------------------------------------------------------
;;; The "move-focus" signal
;;;
;;; void user_function (GtkWidget       *widget,
;;;                     GtkDirectionType direction,
;;;                     gpointer         user_data)      : Action
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "parent-set" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     GtkWidget *old_parent,
;;;                     gpointer   user_data)       : Run First
;;;
;;; The ::parent-set signal is emitted when a new parent has been set on a
;;; widget.
;;;
;;; widget :
;;;     the object on which the signal is emitted
;;;
;;; old_parent :
;;;     the previous parent, or NULL if the widget just got its initial parent.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-menu" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         gpointer   user_data)      : Action
;;;
;;; This signal gets emitted whenever a widget should pop up a context menu.
;;; This usually happens through the standard key binding mechanism; by pressing
;;; a certain key while a widget is focused, the user can cause the widget to
;;; pop up a menu. For example, the GtkEntry widget creates a menu with
;;; clipboard commands. See the section called
;;; \"Implement GtkWidget::popup_menu\" for an example of how to use this
;;; signal.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE if a menu was activated
;;;
;;; ----------------------------------------------------------------------------
;;; The "property-notify-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::property-notify-event signal will be emitted when a property on the
;;; widget's window has been changed or deleted.
;;;
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_PROPERTY_CHANGE_MASK mask.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventProperty which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "proximity-in-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_PROXIMITY_IN_MASK mask.
;;;
;;; This signal will be sent to the grab widget if there is one.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventProximity which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "proximity-out-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_PROXIMITY_OUT_MASK mask.
;;;
;;; This signal will be sent to the grab widget if there is one.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventProximity which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "query-tooltip" signal
;;;
;;; gboolean user_function (GtkWidget  *widget,
;;;                         gint        x,
;;;                         gint        y,
;;;                         gboolean    keyboard_mode,
;;;                         GtkTooltip *tooltip,
;;;                         gpointer    user_data)          : Run Last
;;;
;;; Emitted when "has-tooltip" is TRUE and the "gtk-tooltip-timeout" has expired
;;; with the cursor hovering "above" widget; or emitted when widget got focus in
;;; keyboard mode.
;;;
;;; Using the given coordinates, the signal handler should determine whether a
;;; tooltip should be shown for widget. If this is the case TRUE should be
;;; returned, FALSE otherwise. Note that if keyboard_mode is TRUE, the values of
;;; x and y are undefined and should not be used.
;;;
;;; The signal handler is free to manipulate tooltip with the therefore destined
;;; function calls.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; x :
;;;     the x coordinate of the cursor position where the request has been
;;;     emitted, relative to widget's left side
;;;
;;; y :
;;;     the y coordinate of the cursor position where the request has been
;;;     emitted, relative to widget's top
;;;
;;; keyboard_mode :
;;;     TRUE if the tooltip was trigged using the keyboard
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE if tooltip should be shown right now, FALSE otherwise.
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "realize" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     gpointer   user_data)      : Run First
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen-changed" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     GdkScreen *previous_screen,
;;;                     gpointer   user_data)            : Run Last
;;;
;;; The ::screen-changed signal gets emitted when the screen of a widget has
;;; changed.
;;;
;;; widget :
;;;     the object on which the signal is emitted
;;;
;;; previous_screen :
;;;     the previous screen, or NULL if the widget was not associated with a
;;;     screen before
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::scroll-event signal is emitted when a button in the 4 to 7 range is
;;; pressed. Wheel mice are usually configured to generate button press events
;;; for buttons 4 and 5 when the wheel is turned.
;;;
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_BUTTON_PRESS_MASK mask.
;;;
;;; This signal will be sent to the grab widget if there is one.
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; event :
;;;     the GdkEventScroll which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-clear-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::selection-clear-event signal will be emitted when the the widget's
;;; window has lost ownership of a selection.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventSelection which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-get" signal
;;;
;;; void user_function (GtkWidget        *widget,
;;;                     GtkSelectionData *data,
;;;                     guint             info,
;;;                     guint             time,
;;;                     gpointer          user_data)      : Run Last
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-notify-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; event :
;;;     .
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-received" signal
;;;
;;; void user_function (GtkWidget        *widget,
;;;                     GtkSelectionData *data,
;;;                     guint             time,
;;;                     gpointer          user_data)      : Run Last
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-request-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::selection-request-event signal will be emitted when another client
;;; requests ownership of the selection owned by the widget's window.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventSelection which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "show" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     gpointer   user_data)      : Run First
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-help" signal
;;;
;;; gboolean user_function (GtkWidget        *widget,
;;;                         GtkWidgetHelpType help_type,
;;;                         gpointer          user_data)      : Action
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-allocate" signal
;;;
;;; void user_function (GtkWidget    *widget,
;;;                     GdkRectangle *allocation,
;;;                     gpointer      user_data)       : Run First
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-changed" signal
;;;
;;; void user_function (GtkWidget   *widget,
;;;                     GtkStateType state,
;;;                     gpointer     user_data)      : Run First
;;;
;;; Warning
;;;
;;; GtkWidget::state-changed is deprecated and should not be used in
;;; newly-written code. 3.0. Use "state-flags-changed" instead.
;;;
;;; The ::state-changed signal is emitted when the widget state changes.
;;; See gtk_widget_get_state().
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; state :
;;;     the previous state
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-flags-changed" signal
;;;
;;; void user_function (GtkWidget    *widget,
;;;                     GtkStateFlags flags,
;;;                     gpointer      user_data)      : Run First
;;;
;;; The ::state-flags-changed signal is emitted when the widget state changes,
;;; see gtk_widget_get_state_flags().
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; flags :
;;;     The previous state flags.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-set" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     GtkStyle  *previous_style,
;;;                     gpointer   user_data)           : Run First
;;;
;;; Warning
;;;
;;; GtkWidget::style-set has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use the "style-updated" signal
;;;
;;; The ::style-set signal is emitted when a new style has been set on a widget.
;;; Note that style-modifying functions like gtk_widget_modify_base() also cause
;;; this signal to be emitted.
;;;
;;; Note that this signal is emitted for changes to the deprecated GtkStyle. To
;;; track changes to the GtkStyleContext associated with a widget, use the
;;; "style-updated" signal.
;;;
;;; widget :
;;;     the object on which the signal is emitted
;;;
;;; previous_style :
;;;     the previous style, or NULL if the widget just got its initial style
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-updated" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     gpointer   user_data)      : Run First
;;;
;;; The ::style-updated signal is emitted when the GtkStyleContext of a widget
;;; is changed. Note that style-modifying functions like
;;; gtk_widget_override_color() also cause this signal to be emitted.
;;;
;;; widget :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "touch-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *arg1,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; ----------------------------------------------------------------------------
;;; The "unmap" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     gpointer   user_data)      : Run First
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "unmap-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::unmap-event signal will be emitted when the widget's window is
;;; unmapped. A window is unmapped when it becomes invisible on the screen.
;;;
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventAny which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "unrealize" signal
;;;
;;; void user_function (GtkWidget *widget,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "visibility-notify-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::visibility-notify-event will be emitted when the widget's window is
;;; obscured or unobscured.
;;;
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_VISIBILITY_NOTIFY_MASK mask.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventVisibility which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-state-event" signal
;;;
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; The ::window-state-event will be emitted when the state of the toplevel
;;; window associated to the widget changes.
;;;
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;;
;;; widget :
;;;     the object which received the signal
;;;
;;; event :
;;;     the GdkEventWindowState which triggered this signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "app-paintable" 'gtk-widget) 't)
 "The @arg{\"app-paintable\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the application will paint directly on the widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "can-default" 'gtk-widget) 't)
 "The @arg{\"can-default\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the widget can be the default widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "can-focus" 'gtk-widget) 't)
 "The @arg{\"can-focus\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the widget can accept the input focus.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "composite-child" 'gtk-widget) 't)
 "The @arg{\"composite-child\"} property of type @code{gboolean} (Read).@br{}
  Whether the widget is part of a composite widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "double-buffered" 'gtk-widget) 't)
 "The @arg{\"double-buffered\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the widget is double buffered.@br{}
  Default value: @code{t}.@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "events" 'gtk-widget) 't)
 "The @arg{\"events\"} property of type @symbol{gdk-event-mask}
  (Read / Write).@br{}
  The event mask that decides what kind of @class{gdk-event} this widget
  gets.@br{}
  Default value: @code{:structure-mask}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "expand" 'gtk-widget) 't)
 "The @arg{\"expand\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether to expand in both directions. Setting this sets both properties
  @arg{hexpand} and @arg{vexpand}.@br{}
  Default value: @code{nil}.@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "halign" 'gtk-widget) 't)
 "The @arg{\"halign\"} property of type @symbol{gtk-align} (Read / Write).@br{}
  How to distribute horizontal space if widget gets extra space.@br{}
  Default value: @code{:fill}.@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-default" 'gtk-widget) 't)
 "The @arg{\"has-default\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether the widget is the default widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-focus" 'gtk-widget) 't)
 "The @arg{\"has-focus\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether the widget has the input focus.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-tooltip" 'gtk-widget) 't)
 "The @arg{\"has-tooltip\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Enables or disables the emission of @code{\"query-tooltip\"} on widget. A
  value of @arg{true} indicates that the widget can have a tooltip, in this
  case the widget will be queried using @code{\"query-tooltip\"} to
  determine whether it will provide a tooltip or not.@br{}
  Note that setting this property to @arg{true} for the first time will change
  the event masks of the @class{gdk-window} instances of this widget to include
  @code{\"leave-notify\"} and @code{\"motion-notify\"} events. This cannot and
  will not be undone when the property is set to @code{nil} again.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "height-request" 'gtk-widget) 't)
 "The @arg{\"height-request\"} property of type @code{gint} (Read / Write).@br{}
  Override for height request of the widget, or @code{-1} if natural request
  should be used.@br{}
  Allowed values: @code{>= G_MAXULONG}.@br{}
  Default value: @code{-1}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "hexpand" 'gtk-widget) 't)
 "The @arg{\"hexpand\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether to expand horizontally. See @fun{gtk-widget-set-hexpand}.@br{}
  Default value: @code{nil}.@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "hexpand-set" 'gtk-widget) 't)
 "The @arg{\"hexpand-set\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether to use the @arg{\"hexpand\"} property. See
  @fun{gtk-widget-get-hexpand-set}.@br{}
  Default value: @code{nil}.@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "is-focus" 'gtk-widget) 't)
 "The @arg{\"is-focus\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether the widget is the focus widget within the toplevel.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin" 'gtk-widget) 't)
 "The @arg{\"margin\"} property of type @code{gint} (Read / Write).@br{}
  Sets all four sides' margin at once. If read, returns max margin on any
  side.@br{}
  Allowed values: @code{[0,32767]}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-bottom" 'gtk-widget) 't)
 "The @arg{\"margin-bottom\"} property of type @code{gint} (Read / Write).@br{}
  Margin on bottom side of widget.@br{}
  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  @fun{gtk-widget-set-size-request} for example.@br{}
  Allowed values: @code{[0,32767]}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-left" 'gtk-widget) 't)
 "The @arg{\"margin-left\"} property of type @code{gint} (Read / Write).@br{}
  Margin on left side of widget.@br{}
  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  @fun{gtk-widget-set-size-request} for example.@br{}
  Allowed values: @code{[0,32767]}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-right" 'gtk-widget) 't)
 "The @arg{\"margin-right\"} property of type @code{gint} (Read / Write).@br{}
  Margin on right side of widget.@br{}
  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  @fun{gtk-widget-set-size-request} for example.@br{}
  Allowed values: @code{[0,32767]}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-top" 'gtk-widget) 't)
 "The @arg{\"margin-top\"} property of type @code{gint} (Read / Write).@br{}
  Margin on top side of widget.@br{}
  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  @fun{gtk-widget-set-size-request} for example.@br{}
  Allowed values: @code{[0,32767]}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-widget) 't)
 "The @arg{\"name\"} property of type @code{gchar*} (Read / Write).@br{}
  The name of the widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "no-show-all" 'gtk-widget) 't)
 "The @arg{\"no-show-all\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether @fun{gtk-widget-show-all} should not affect this widget.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "parent" 'gtk-widget) 't)
 "The @arg{\"parent\"} property of type @class{gtk-container}
  (Read / Write).@br{}
  The parent widget of this widget. Must be a @class{gtk-container} widget.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "receives-default" 'gtk-widget) 't)
 "The @arg{\"receives-default\"} property of type @code{gboolean}
  (Read / Write).@br{}
  If @arg{true}, the widget will receive the default action when it is
  focused.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "sensitive" 'gtk-widget) 't)
 "The @arg{\"sensitive\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether the widget responds to input.@br{}
  Default value: @code{t}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "style" 'gtk-widget) 't)
 "The @arg{\"style\"} property of type @class{gtk-style} (Read / Write).@br{}
  The style of the widget, which contains information about how it will look
  (colors etc).")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-markup" 'gtk-widget) 't)
 "The @arg{\"tooltip-markup\"} property of type @code{gchar*}
  (Read / Write)@br{}
  Sets the text of tooltip to be the given string, which is marked up with the
  Pango text markup language. Also see @fun{gtk-tooltip-set-markup}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}: @arg{\"has-tooltip\"} will
  automatically be set to @arg{true} and there will be taken care of
  @code{\"query-tboltip\"} in the default signal handler.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-text" 'gtk-widget) 't)
 "The @arg{\"tooltip-text\"} property of type @code{gchar*} (Read / Write).@br{}
  Sets the text of tooltip to be the given string.
  Also see @fun{gtk-tooltip-set-text}.
  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not @code{nil}: @arg{\"has-tooltip\"} will
  automatically be set to @arg{true} and there will be taken care of
  @code{\"query-tooltip\"} in the default signal handler.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "valign" 'gtk-widget) 't)
 "The @arg{\"valign\"} property of type @symbol{gtk-align} (Read / Write).@br{}
  How to distribute vertical space if widget gets extra space.@br{}
  Default value: @code{:fill}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "vexpand" 'gtk-widget) 't)
 "The @arg{\"vexpand\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether to expand vertically. See @fun{gtk-widget-set-vexpand}.@br{}
  Default value: @code{nil}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "vexpand-set" 'gtk-widget) 't)
 "The @arg{\"vexpand-set\"} property of type @code{gboolean}
  (Read / Write).@br{}
  Whether to use the @arg{\"vexpand\"} property.
  See @fun{gtk-widget-get-vexpand-set}.@br{}
  Default value: @code{nil}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-widget) 't)
 "The @arg{\"visible\"} property of type @code{gboolean} (Read / Write).@br{}
  Whether the widget is visible.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "width-request" 'gtk-widget) 't)
 "The @arg{\"width-request\"} property of type @code{gint} (Read / Write).@br{}
  Override for width request of the widget, or @code{-1} if natural request
  should be used.@br{}
  Allowed values: @code{>= G_MAXULONG}@br{}
  Default value: @code{-1}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "window" 'gtk-widget) 't)
 "The @arg{\"window\"} property of type @class{gdk-window} (Read).@br{}
  The widget's window if it is realized, @code{nil} otherwise.@br{}
  Since 2.14")

;;; --- gtk-requisiton ---------------------------------------------------------

(setf (gethash 'gtk-requisition atdoc:*class-name-alias*) "CStruct")
(setf (documentation 'gtk-requisition 'type)
 "@version{2012-12-22}
  @begin{short}
    A @sym{gtk-requisition} represents the desired size of a widget.
  @end{short}
  See the section called \"Height-for-width Geometry Management\" for more
  information.
  @begin{pre}
(define-g-boxed-cstruct gtk-requisition \"GtkRequisition\"
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[width]{the widget's desired width}
    @entry[height]{the widget's desired height}
  @end{table}
  @see-slot{gtk-requisition-width}
  @see-slot{gtk-requisition-height}
  @see-constructor{make-gtk-requisition}
  @see-constructor{copy-gtk-requisition}")

(setf (gethash 'gtk-requisition-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-requisition-width 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @arg{width} of the @class{gtk-requisition} structure.
  @end{short}
  @see-class{gtk-requisition}")

(setf (gethash 'gtk-requisition-height atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-requisition-height 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @arg{height} of the @class{gtk-requisition}
    structure.
  @end{short}
  @see-class{gtk-requisition}")

;;; --- gtk-allocation ---------------------------------------------------------

(setf (documentation 'gtk-allocation 'type)
 "@version{2012-12-22}
  @begin{short}
    A @sym{gtk-allocation} of a widget represents a region which has been
    allocated to the widget by its parent.
  @end{short}
  It is a subregion of its parents allocation. See the section called
  \"Height-for-width Geometry Management\" for more information.
  @begin{pre}
(define-g-boxed-cstruct gtk-allocation \"GtkAllocation\"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @see-slot{gtk-allocation-x}
  @see-slot{gtk-allocation-y}
  @see-slot{gtk-allocation-width}
  @see-slot{gtk-allocation-height}
  @see-constructor{make-gtk-allocation}
  @see-constructor{copy-gtk-allociation}")

(setf (gethash 'gtk-allocation-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-x 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @code{x} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-y 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @code{y} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-width 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @code{width} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-heigth atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-heigth 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor of the slot @code{heigth} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

;;; ----------------------------------------------------------------------------
;;; struct GtkWidgetAuxInfo
;;;
;;; struct GtkWidgetAuxInfo {
;;;   gint width;
;;;   gint height;
;;;
;;;   guint   halign : 4;
;;;   guint   valign : 4;
;;;
;;;   GtkBorder margin;
;;; };
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-help-type ---------------------------------------------------

(setf (gethash 'gtk-widget-help-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-widget-help-type atdoc:*external-symbols*)
 "@short{}
  @begin{pre}
(define-g-enum \"GtkWidgetHelpType\" gtk-widget-help-type
  (:export t
   :type-initializer \"gtk_widget_help_type_get_type\")
  (:tooltip 0)
  (:whats-this 1))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_new ()
;;;
;;; GtkWidget * gtk_widget_new (GType type,
;;;                             const gchar *first_property_name,
;;;                             ...);
;;;
;;; This is a convenience function for creating a widget and setting its
;;; properties in one go. For example you might write:
;;; gtk_widget_new (GTK_TYPE_LABEL, "label", "Hello World", "xalign", 0.0, NULL)
;;; to create a left-aligned label. Equivalent to g_object_new(), but returns a
;;; widget so you don't have to cast the object yourself.
;;;
;;; type :
;;;     type ID of the widget to create
;;;
;;; first_property_name :
;;;     name of first property to set
;;;
;;; ... :
;;;     value of first property, followed by more properties, NULL-terminated
;;;
;;; Returns :
;;;     a new GtkWidget of type widget_type
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-destroy -----------------------------------------------------

(setf (documentation 'gtk-widget-destroy 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{Destroys a widget.}

  When a widget is destroyed, it will break any references it holds to other
  objects. If the widget is inside a container, the widget will be removed
  from the container. If the widget is a toplevel (derived from
  @class{gtk-window}), it will be removed from the list of toplevels, and the
  reference GTK+ holds to it will be removed. Removing a widget from its
  container or the list of toplevels results in the widget being finalized,
  unless you've added additional references to the widget with
  @fun{g-object-ref}.

  In most cases, only toplevel widgets (windows) require explicit destruction,
  because when you destroy a toplevel its children will be destroyed as well.
  @see-class{gtk-widget}
  @see-class{gtk-window}
  @see-function{g-object-ref}")

;;; --- gtk-widget-in-destruction ----------------------------------------------

(setf (documentation 'gtk-widget-in-destruction 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} is being destroyed.}
  @short{Returns whether the @arg{widget} is currently being destroyed.}
  This information can sometimes be used to avoid doing unnecessary work.
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroyed ()
;;;
;;; void gtk_widget_destroyed (GtkWidget *widget, GtkWidget **widget_pointer);
;;;
;;; This function sets *widget_pointer to NULL if widget_pointer != NULL. It's
;;; intended to be used as a callback connected to the "destroy" signal of a
;;; widget. You connect gtk_widget_destroyed() as a signal handler, and pass the
;;; address of your widget variable as user data. Then when the widget is
;;; destroyed, the variable will be set to NULL. Useful for example to avoid
;;; multiple copies of the same dialog.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; widget_pointer :
;;;     address of a variable that contains widget
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-unparent ----------------------------------------------------

(setf (documentation 'gtk-widget-unparent 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{This function is only for use in widget implementations.}
  Should be called by implementations of the remove method on
  @class{gtk-container}, to dissociate a child from the container.
  @see-class{gtk-widget}
  @see-class{gtk-container}")

;;; --- gtk-widget-show --------------------------------------------------------

(setf (documentation 'gtk-widget-show 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Flags a @arg{widget} to be displayed.
  @end{short}
  Any @arg{widget} that isn't shown will not appear on the screen. If you want
  to show all the widgets in a container, it's easier to call
  @fun{gtk-widget-show-all} on the container, instead of individually showing
  the widgets.

  Remember that you have to show the containers containing a widget, in
  addition to the @arg{widget} itself, before it will appear onscreen.

  When a toplevel container is shown, it is immediately realized and mapped;
  other shown widgets are realized and mapped when their toplevel container is
  realized and mapped.
  @see-class{gtk-widget}
  @see-function{gtk-widget-show-all}")

;;; --- gtk-widget-show-now ----------------------------------------------------

(setf (documentation 'gtk-widget-show-now 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{Shows a widget.}
  If @arg{widget} is an unmapped toplevel widget (i. e. a @class{gtk-window}
  that has not yet been shown), enter the main loop and wait for the window to
  actually be mapped. Be careful; because the main loop is running, anything can
  happen during this function.
  @see-class{gtk-widget}
  @see-class{gtk-window}")

;;; --- gtk-widget-hide --------------------------------------------------------

(setf (documentation 'gtk-widget-hide 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Reverses the effects of @fun{gtk-widget-show}, causing the @arg{widget} to
    be hidden (invisible to the user).
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-widget-show}")

;;; --- gtk-widget-show-all ----------------------------------------------------

(setf (documentation 'gtk-widget-show-all 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Recursively shows a @arg{widget}, and any child widgets (if the @arg{widget}
    is a container).
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-widget-show}")

;;; --- gtk-widget-map ---------------------------------------------------------

(setf (documentation 'gtk-widget-map 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    This function is only for use in widget implementations.
  @end{short}
  Causes a widget to be mapped if it isn't already.
  @see-class{gtk-widget}")

;;; --- gtk-widget-unmap -------------------------------------------------------

(setf (documentation 'gtk-widget-unmap 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{This function is only for use in widget implementations.}
  Causes a widget to be unmapped if it's currently mapped.
  @see-class{gtk-widget}")

;;; --- gtk-widget-realize -----------------------------------------------------

(setf (documentation 'gtk-widget-realize 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{Creates the GDK (windowing system) resources associated with a widget.}
  For example, @code{widget->window} will be created when a widget is realized.
  Normally realization happens implicitly; if you show a widget and all its
  parent containers, then the widget will be realized and mapped automatically.

  Realizing a widget requires all the widget's parent widgets to be realized;
  calling @sym{gtk-widget-realize} realizes the widget's parents in addition to
  widget itself. If a widget is not yet inside a toplevel window when you
  realize it, bad things will happen.

  This function is primarily used in widget implementations, and isn't very
  useful otherwise. Many times when you think you might need it, a better
  approach is to connect to a signal that will be called after the widget is
  realized automatically, such as \"draw\". Or simply @fun{g-signal-connect} to
  the \"realize\" signal.
  @see-class{gtk-widget}
  @see-function{g-signal-connect}")

;;; --- gtk-widget-unrealize ---------------------------------------------------

(setf (documentation 'gtk-widget-unrealize 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    This function is only useful in widget implementations.
  @end{short}
  Causes a @arg{widget} to be unrealized (frees all GDK resources associated
  with the @arg{widget}, such as @code{widget->window}).
  @see-class{gtk-widget}")

;;; --- gtk-widget-draw --------------------------------------------------------

(setf (documentation 'gtk-widget-queue-draw 'function)
 "@version{2012-12-23}
  @argument[widget]{the @arg{widget} to draw. It must be drawable (see
    @fun{gtk-widget-is-drawable}) and a size must have been allocated.}
  @argument[cr]{a cairo context to draw to}
  @begin{short}
    Draws @arg{widget} to a cairo context @arg{cr}.
  @end{short}
  The top left corner of the @arg{widget} will be drawn to the currently set
  origin point of the cairo context @arg{cr}.

  You should pass a cairo context as @arg{cr} argument that is in an original
  state. Otherwise the resulting drawing is undefined. For example changing the
  operator using cairo_set_operator() or the line width using
  @code{cairo_set_line_width()} might have unwanted side effects. You may
  however change the context's transform matrix - like with
  @code{cairo_scale()}, @code{cairo_translate()} or @code{cairo_set_matrix()}
  and clip region with @code{cairo_clip()} prior to calling this function. Also,
  it is fine to modify the context with @code{cairo_save()} and
  @code{cairo_push_group()} prior to calling this function.

  @begin[Note]{dictionary}
    Special purpose widgets may contain special code for rendering to the screen
    and might appear differently on screen and when rendered using
    @fun{gtk-widget-draw}.
  @end{dictionary}
  Since 3.0")

;;; --- gtk-widget-queue-draw --------------------------------------------------

(setf (documentation 'gtk-widget-queue-draw 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Equivalent to calling @fun{gtk-widget-queue-draw-area} for the entire area
    of a @arg{widget}.
  @end{short}")

;;; --- gtk-widget-queue-resize ------------------------------------------------

(setf (documentation 'gtk-widget-queue-resize 'function)
 "@argument[widget]{a @class{GtkWidget} instance}
  @begin{short}
    This function is only for use in widget implementations.
  @end{short}
  Flags a @arg{widget} to have its size renegotiated; should be called when a
  widget for some reason has a new size request. For example, when you change
  the text in a @class{gtk-label}, @class{gtk-label} queues a resize to ensure
  there's enough space for the new text.
  @begin[Note]{dictionary}
    You cannot call @sym{gtk-widget-queue-resize} on a widget from inside its
    implementation of the @code{GtkWidgetClass::size_allocate} virtual method.
    Calls to @sym{gtk-widget-queue-resize} from inside
    @code{GtkWidgetClass::size_allocate} will be silently ignored.
  @end{dictionary}")

;;; --- gtk-widget-queue-resize-no-redraw --------------------------------------

(setf (documentation 'gtk-widget-queue-resize-no-redraw 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    This function works like @fun{gtk-widget-queue-resize}, except that the
    widget is not invalidated.
  @end{short}
  Since 2.4.
  @see-function{gtk-widget-queue-resize}")

;;; --- gtk-widget-size-request ------------------------------------------------

(setf (documentation 'gtk-widget-size-request 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[requisition]{a @class{gtk-requisition} to be filled in}
  @heading{Warning}
  @begin{short}
    @sym{gtk-widget-size-request} has been deprecated since version 3.0 and
    should not be used in newly-written code.
  @end{short}
  Use @fun{gtk-widget-get-preferred-size} instead.

  This function is typically used when implementing a @class{gtk-container}
  subclass. Obtains the preferred size of a @arg{widget}. The container uses
  this information to arrange its child widgets and decide what size allocations
  to give them with @fun{gtk-widget-size-allocate}.

  You can also call this function from an application, with some caveats. Most
  notably, getting a size request requires the widget to be associated with a
  screen, because font information may be needed. Multihead-aware applications
  should keep this in mind.

  Also remember that the size request is not necessarily the size a widget
  will actually be allocated.
  @see-function{gtk-widget-get-preferred-size}
  @see-function{gtk-widget-size-allocate}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_requisition ()
;;;
;;; void gtk_widget_get_child_requisition (GtkWidget *widget,
;;;                                        GtkRequisition *requisition);
;;;
;;; Warning
;;;
;;; gtk_widget_get_child_requisition has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_widget_get_preferred_size() instead.
;;;
;;; This function is only for use in widget implementations. Obtains
;;; widget->requisition, unless someone has forced a particular geometry on the
;;; widget (e.g. with gtk_widget_set_size_request()), in which case it returns
;;; that geometry instead of the widget's requisition.
;;;
;;; This function differs from gtk_widget_size_request() in that it retrieves
;;; the last size request value from widget->requisition, while
;;; gtk_widget_size_request() actually calls the "size_request" method on widget
;;; to compute the size request and fill in widget->requisition, and only then
;;; returns widget->requisition.
;;;
;;; Because this function does not call the "size_request" method, it can only
;;; be used when you know that widget->requisition is up-to-date, that is,
;;; gtk_widget_size_request() has been called since the last time a resize was
;;; queued. In general, only container implementations have this information;
;;; applications should use gtk_widget_size_request().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; requisition :
;;;     a GtkRequisition to be filled in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_allocate ()
;;;
;;; void gtk_widget_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
;;;
;;; This function is only used by GtkContainer subclasses, to assign a size and
;;; position to their child widgets.
;;;
;;; In this function, the allocation may be adjusted. It will be forced to a 1x1
;;; minimum size, and the adjust_size_allocation virtual method on the child
;;; will be used to adjust the allocation. Standard adjustments include removing
;;; the widget's margins, and applying the widget's "halign" and "valign"
;;; properties.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; allocation :
;;;     position and size to be allocated to widget
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-add-accelerator ---------------------------------------------

(setf (documentation 'gtk-widget-add-accelerator 'function)
 "@argument[widget]{widget to install an accelerator on}
  @argument[accel-signal]{widget signal to emit on accelerator activation}
  @argument[accel-group]{accel group for this @arg{widget}, added to its
    toplevel}
  @argument[accel-key]{GDK keyval of the accelerator}
  @argument[accel-mods]{modifier key combination of the accelerator}
  @argument[accel-flags]{flag accelerators, e.g. @code{:visible}}
  @begin{short}
    Installs an accelerator for this widget in @arg{accel-group} that causes
    @arg{accel-signal} to be emitted if the accelerator is activated.
  @end{short}
  The @arg{accel-group} needs to be added to the widget's toplevel via
  @fun{gtk-window-add-accel-group}, and the signal must be of type
  @code{G_RUN_ACTION}. Accelerators added through this function are not user
  changeable during runtime. If you want to support accelerators that can be
  changed by the user, use @fun{gtk-accel-map-add-entry} and
  @fun{gtk-widget-set-accel-path} or @fun{gtk-menu-item-set-accel-path}
  instead.
  @see-function{gtk-window-add-accel-group}
  @see-function{gtk-accel-map-add-entry}
  @see-function{gtk-widget-set-accel-path}
  @see-function{gtk-menu-item-set-accel-path}")

;;; --- gtk-widget-remove-accelerator ------------------------------------------

(setf (documentation 'gtk-widget-remove-accelerator 'function)
 "@argument[widget]{@arg{widget} to remove an accelerator from}
  @argument[accel-group]{accel group for this @arg{widget}}
  @argument[accel-key]{GDK keyval of the accelerator}
  @argument[accel-mods]{modifier key combination of the accelerator}
  @return{Whether an accelerator was installed and could be removed.}
  @begin{short}
    Removes an accelerator from @arg{widget}, previously installed with
    @fun{gtk-widget-add-accelerator}.
  @end{short}
  @see-function{gtk-widget-add-accelerator}")

;;; --- gtk-widget-set-accel-path ----------------------------------------------

(setf (documentation 'gtk-widget-set-accel-path 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[accel-path]{path used to look up the accelerator}
  @argument[accel-group]{a @class{gtk-accel-group}}
  @begin{short}
    Given an accelerator group, @arg{accel-group}, and an accelerator path,
    @arg{accel-path}, sets up an accelerator in @arg{accel-group} so whenever
    the key binding that is defined for @arg{accel-path} is pressed, widget will
    be activated.
  @end{short}
  This removes any accelerators (for any accelerator group) installed by
  previous calls to @sym{gtk-widget-set-accel-path}. Associating accelerators
  with paths allows them to be modified by the user and the modifications to be
  saved for future use. (See @fun{gtk-accel-map-save}.)

  This function is a low level function that would most likely be used by a
  menu creation system like @class{gtk-ui-manager}. If you use
  @class{gtk-ui-manager}, setting up accelerator paths will be done
  automatically.

  Even when you you aren't using @class{gtk-ui-manager}, if you only want to set
  up accelerators on menu items @fun{gtk-menu-item-set-accel-path} provides a
  somewhat more convenient interface.

  Note that @arg{accel-path} string will be stored in a @type{g-quark}.
  Therefore, if you pass a static string, you can save some memory by interning
  it first with @code{g_intern_static_string()}.
  @see-function{gtk-accel-map-save}
  @see-class{gtk-ui-manager}
  @see-function{gtk-menu-item-set-accel-path}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_accel_closures ()
;;;
;;; GList * gtk_widget_list_accel_closures (GtkWidget *widget);
;;;
;;; Lists the closures used by widget for accelerator group connections with
;;; gtk_accel_group_connect_by_path() or gtk_accel_group_connect(). The closures
;;; can be used to monitor accelerator changes on widget, by connecting to the
;;; GtkAccelGroup::accel-changed signal of the GtkAccelGroup of a closure which
;;; can be found out with gtk_accel_group_from_accel_closure().
;;;
;;; widget :
;;;     widget to list accelerator closures for
;;;
;;; Returns :
;;;     a newly allocated GList of closures
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-can-activate-accel ------------------------------------------

(setf (documentation 'gtk-widget-can-activate-accel 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[signal-id]{the ID of a signal installed on @arg{widget}}
  @return{@arg{true} if the accelerator can be activated.}
  @begin{short}
    Determines whether an accelerator that activates the signal identified by
    @arg{signal-id} can currently be activated.
  @end{short}
  This is done by emitting the \"can-activate-accel\" signal on widget; if the
  signal isn't overridden by a handler or in a derived widget, then the default
  check is that the widget must be sensitive, and the widget and all its
  ancestors mapped.

  Since 2.4")

;;; --- gtk-widget-event -------------------------------------------------------

(setf (documentation 'gtk-widget-event 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[event]{a @class{gdk-event}}
  @return{Return from the event signal emission (@arg{true} if the event was
    handled).}
  @begin{short}
    Rarely used function. This function is used to emit the event signals on a
    widget (those signals should never be emitted without using this function to
    do so).
  @end{short}
  If you want to synthesize an event though, don't use this function;
  instead, use @fun{gtk-main-do-event} so the event will behave as if it were in
  the event queue. Don't synthesize expose events; instead, use
  @fun{gdk-window-invalidate-rect} to invalidate a region of the window.
  @see-function{gtk-main-do-event}
  @see-function{gdk-window-invalidate-rect}")

;;; --- gtk-widget-activate ----------------------------------------------------

(setf (documentation 'gtk-widget-activate 'function)
 "@argument[widget]{a @class{gtk-widget} instance that's activatable}
  @return{@arg{true} if the @arg{widget} was activatable}
  @begin{short}
    For widgets that can be \"activated\" (buttons, menu items, etc.) this
    function activates them.
  @end{short}
  Activation is what happens when you press Enter on a widget during key
  navigation. If @arg{widget} isn't activatable, the function returns
  @code{nil}.")

;;; --- gtk-widget-reparent ----------------------------------------------------

(setf (documentation 'gtk-widget-reparent 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[new-parent]{a @class{gtk-container} to move the widget into}
  @begin{short}
    Moves a widget from one @class{gtk-container} to another, handling reference
    count issues to avoid destroying the widget.
  @end{short}")

;;; --- gtk-widget-intersect ---------------------------------------------------

(setf (documentation 'gtk-widget-intersect 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @argument[area]{a rectangle}
  @return{Returns the intersection as a rectangle, if there was an intersection
    or @code{nil}.}
  @begin{short}
    Computes the intersection of a @arg{widget}'s area and @arg{area}, and
    returns the intersection as a rectangle of type @class{gdk-rectangle} if
    there was an intersection.
  @end{short}")

;;; gtk-widget-is-focus is documented as the accessor of the slot "is-fous"

;;; ---- gtk-widget-grab-focus -------------------------------------------------

(setf (documentation 'gtk-widget-grab-focus 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Causes @arg{widget} to have the keyboard focus for the @class{gtk-window}
    instance it's inside. @arg{widget} must be a focusable widget, such as a
    @class{gtk-entry}; something like @class{gtk-frame} won't work.
  @end{short}

  More precisely, it must have the @code{:can-focus} flag set. Use
  @fun{gtk-widget-set-can-focus} to modify that flag.

  The @arg{widget} also needs to be realized and mapped. This is indicated by
  the related signals. Grabbing the focus immediately after creating the
  @arg{widget} will likely fail and cause critical warnings.
  @see-function{gtk-widget-set-can-focus}")

;;; --- gtk-widget-grab-default ------------------------------------------------

(setf (documentation 'gtk-widget-grab-default 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Causes @arg{widget} to become the default widget. @arg{widget} must have the
    @code{:can-default} flag set; typically you have to set this flag yourself
    by calling @fun{gtk-widget-set-can-default}.
  @end{short}
  The default widget is activated when the user presses Enter in a window.
  Default widgets must be activatable, that is, @fun{gtk-widget-activate} should
  affect them. Note that @class{gtk-entry} widgets require the
  \"activates-default\" property set to @arg{true} before they activate the
  default widget when Enter is pressed and the @class{gtk-entry} is focused.
  @see-function{gtk-widget-set-can-default}
  @see-function{gtk-widget-activate}")

;;; --- gtk-widget-set-name ----------------------------------------------------

(setf (documentation 'gtk-widget-set-name 'function)
 "@version{2013-1-26}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[name]{name for the @arg{widget}}
  @begin{short}
    Widgets can be named, which allows you to refer to them from a CSS file. You
    can apply a style to widgets with a particular name in the CSS file. See the
    documentation for the CSS syntax (on the same page as the docs for
    @class{gtk-style-context}).
  @end{short}

  Note that the CSS syntax has certain special characters to delimit and
  represent elements in a selector (period, #, >, *...), so using these will
  make your widget impossible to match by name. Any combination of
  alphanumeric symbols, dashes and underscores will suffice.")

;;; --- gtk-widget-get-name ----------------------------------------------------

(setf (documentation 'gtk-widget-get-name 'function)
 "@version{2013-1-26}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{Name of the @arg{widget}.}
  @begin{short}
    Retrieves the name of a @arg{widget}.
  @end{short}
  See @fun{gtk-widget-set-name} for the significance of widget names.
  @see-function{gtk-widget-set-name}")

;;; --- gtk-widget-set-state ---------------------------------------------------

(setf (documentation 'gtk-widget-set-state 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget}}
  @argument[state]{new state for @arg{widget}}
  @subheading{Warning}
  @begin{short}
    @sym{gtk-widget-set-state} is deprecated and should not be used in
    newly-written code. Use @fun{gtk-widget-set-state-flags} instead.
  @end{short}

  This function is for use in widget implementations. Sets the state of a
  widget (insensitive, prelighted, etc.) Usually you should set the state
  using wrapper functions such as @fun{gtk-widget-set-sensitive}.
  @see-function{gtk-widget-set-state-flags}
  @see-function{gtk-widget-set-sensitive}")

;;; --- gtk-widget-set-sensitive -----------------------------------------------

(setf (documentation 'gtk-widget-set-sensitive 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[sensitive]{@arg{true} to make the @arg{widget} sensitive}
  @begin{short}
    Sets the sensitivity of a widget.
  @end{short}
  A widget is sensitive if the user can interact with it. Insensitive widgets
  are \"grayed out\" and the user can't interact with them. Insensitive widgets
  are known as \"inactive\", \"disabled\", or \"ghosted\" in some other
  toolkits.")

;;; --- gtk-widget-set-parent --------------------------------------------------

(setf (documentation 'gtk-widget-set-parent 'function)
 "@version{2012-12-29}
  @argumen[twidget]{a @class{gtk-widget} instance}
  @argument[parent]{parent container}
  @begin{short}
    This function is useful only when implementing subclasses of
    @class{gtk-container}.
  @end{short}
  Sets the container as the parent of @arg{widget}, and takes care of some
  details such as updating the state and style of the child to reflect its new
  location. The opposite function is @fun{gtk-widget-unparent}.
  @see-function{gtk-widget-unparent}")

;;; --- gtk-widget-set-parent-window -------------------------------------------

(setf (documentation 'gtk-widget-set-parent-window 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance.}
  @argument[parent-window]{the new parent window}
  @short{Sets a non default parent window for @arg{widget}.}

  For @class{gtk-window} classes, setting a @arg{parent-window} effects whether
  the window is a toplevel window or can be embedded into other widgets.
  @begin[Note]{dictionary}
    For @class{gtk-window} classes, this needs to be called before the window is
    realized.
  @end{dictionary}")

;;; --- gtk-widget-get-parent-window -------------------------------------------

(setf (documentation 'gtk-widget-get-parent-window 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget}.}
  @return{the parent window of @arg{widget}}
  @short{Gets widget's parent window.}")

;;; --- gtk-widget-set-events --------------------------------------------------

(setf (documentation 'gtk-widget-set-events 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[events]{event mask}
  @short{Sets the event mask (see @symbol{gdk-event-mask}) for a @arg{widget}.}
  The event mask determines which events a @arg{widget} will receive. Keep in
  mind that different widgets have different default event masks, and by
  changing the event mask you may disrupt a widget's functionality, so be
  careful. This function must be called while a widget is unrealized. Consider
  @fun{gtk-widget-add-events} for widgets that are already realized, or if you
  want to preserve the existing event mask. This function can't be used with
  @code{:no-window} widgets; to get events on those widgets, place them inside a
  @class{gtk-event-box} and receive events on the event box.
  @see-function{gtk-widget-add-events}")

;;; --- gtk-widget-get-events --------------------------------------------------

(setf (documentation 'gtk-widget-get-events 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{event mask for @arg{widget}}
  @begin{short}
    Returns the event mask for the widget (a bitfield containing flags from the
    @symbol{gdk-event-mask enumeration}).
  @end{short}
  These are the events that the widget will receive.")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_events ()
;;;
;;; void gtk_widget_add_events (GtkWidget *widget, gint events);
;;;
;;; Adds the events in the bitfield events to the event mask for widget.
;;; See gtk_widget_set_events() for details.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; events :
;;;     an event mask, see GdkEventMask
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_device_events ()
;;;
;;; void gtk_widget_set_device_events (GtkWidget *widget,
;;;                                    GdkDevice *device,
;;;                                    GdkEventMask events);
;;;
;;; Sets the device event mask (see GdkEventMask) for a widget. The event mask
;;; determines which events a widget will receive from device. Keep in mind that
;;; different widgets have different default event masks, and by changing the
;;; event mask you may disrupt a widget's functionality, so be careful. This
;;; function must be called while a widget is unrealized. Consider
;;; gtk_widget_add_device_events() for widgets that are already realized, or if
;;; you want to preserve the existing event mask. This function can't be used
;;; with GTK_NO_WINDOW widgets; to get events on those widgets, place them
;;; inside a GtkEventBox and receive events on the event box.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; events :
;;;     event mask
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_events ()
;;;
;;; GdkEventMask gtk_widget_get_device_events (GtkWidget *widget,
;;;                                            GdkDevice *device);
;;;
;;; Returns the events mask for the widget corresponding to an specific device.
;;; These are the events that the widget will receive when device operates on
;;; it.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     device event mask for widget
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_device_events ()
;;;
;;; void gtk_widget_add_device_events (GtkWidget *widget,
;;;                                    GdkDevice *device,
;;;                                    GdkEventMask events);
;;;
;;; Adds the device events in the bitfield events to the event mask for widget.
;;; See gtk_widget_set_device_events() for details.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; events :
;;;     an event mask, see GdkEventMask
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_device_enabled ()
;;;
;;; void gtk_widget_set_device_enabled (GtkWidget *widget,
;;;                                     GdkDevice *device,
;;;                                     gboolean enabled);
;;;
;;; Enables or disables a GdkDevice to interact with widget and all its
;;; children.
;;;
;;; It does so by descending through the GdkWindow hierarchy and enabling the
;;; same mask that is has for core events (i.e. the one that
;;; gdk_window_get_events() returns).
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; enabled :
;;;     whether to enable the device
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_enabled ()
;;;
;;; gboolean gtk_widget_get_device_enabled (GtkWidget *widget,
;;;                                         GdkDevice *device);
;;;
;;; Returns whether device can interact with widget and its children.
;;; See gtk_widget_set_device_enabled().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     TRUE is device is enabled for widget
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-get-toplevel ------------------------------------------------

(setf (documentation 'gtk-widget-get-toplevel 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{the topmost ancestor of @arg{widget}, or @arg{widget} itself if
    there's no ancestor}
  @begin{short}
    This function returns the topmost widget in the container hierarchy
    @arg{widget} is a part of.
  @end{short}
  If @arg{widget} has no parent widgets, it will be returned as the topmost
  widget. No reference will be added to the returned widget; it should not be
  unreferenced.

  Note the difference in behavior vs. @fun{gtk-widget-get-ancestor};
  @code{(gtk-widget-get-ancestor widget :window)} would return @code{nil} if
  @arg{widget} wasn't inside a toplevel window, and if the window was inside a
  @sym{gtk-window} derived widget which was in turn inside the toplevel
  @class{gtk-window}. While the second case may seem unlikely, it actually
  happens when a @class{gtk-plug} is embedded inside a @class{gtk-socket} within
  the same application.

  To reliably find the toplevel @class{gtk-window}, use
  @sym{gtk-widget-get-toplevel} and check if the @code{:toplevel} flags is set
  on the result.
  @begin{pre}
 GtkWidget *toplevel = gtk_widget_get_toplevel (widget);
 if (gtk_widget_is_toplevel (toplevel))
   {
     /* Perform action on toplevel. */
   @}
  @end{pre}
  @see-function{gtk-widget-get-ancestor}")

;;; --- gtk-widget-get-ancestor ------------------------------------------------

(setf (documentation 'gtk-widget-get-ancestor 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[widget-type]{ancestor type}
  @return{the ancestor widget, or @arg{nil} if not found}
  @begin{short}
    Gets the first ancestor of @arg{widget} with type @arg{widget-type}.
  @end{short}
  For example, @code{(gtk-widget-get-ancestor widget :box)} gets the first
  @class{gtk-box} that's an ancestor of @arg{widget}. No reference will be added
  to the returned widget; it should not be unreferenced. See note about checking
  for a toplevel @class{gtk-window} in the docs for
  @fun{gtk-widget-get-toplevel}.

  Note that unlike @fun{gtk-widget-is-ancestor}, @sym{gtk-widget-get-ancestor}
  considers @arg{widget} to be an ancestor of itself.
  @see-function{gtk-widget-get-toplevel}
  @see-function{gtk-widget-is-ancestor}")

;;; --- gtk-widget-get-visual --------------------------------------------------

(setf (documentation 'gtk-widget-get-visual 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The visual for @arg{widget}.}
  @short{Gets the visual that will be used to render @arg{widget}.}")


;;; --- gtk-widget-set-visual --------------------------------------------------

(setf (documentation 'gtk-widget-set-visual 'function)
 "@version{2012-12-29}
  @argument[widget]{a @arg{gtk-widget} instance}
  @argument[visual]{visual to be used or @code{nil} to unset a previous one}
  @begin{short}
    Sets the @arg{visual} that should be used for by @arg{widget} and its
    children for creating @class{gdk-window}'s.
  @end{short}
  The @arg{visual} must be on the same @class{gdk-screen} as returned by
  @fun{gtk-widget-get-screen}, so handling the \"screen-changed\" signal is
  necessary.

  Setting a new @arg{visual} will not cause widget to recreate its windows, so
  you should call this function before widget is realized.
  @see-function{gtk-widget-get-screen}")

;;; --- gtk-widget-get-pointer -------------------------------------------------

(setf (documentation 'gtk-widget-get-pointer 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[x]{return location for the X coordinate, or @code{nil}}
  @argument[y]{return location for the Y coordinate, or @code{nil}}
  @begin{short}
    Obtains the location of the mouse pointer in widget coordinates.
  @end{short}
  Widget coordinates are a bit odd; for historical reasons, they are defined as
  widget->window coordinates for widgets that are not @code{:no-window} widgets,
  and are relative to widget->allocation.x, widget->allocation.y for widgets
  that are @code{:no-window} widgets.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-get-pointer} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use
    @fun{gdk-window-get-device-position} instead.
  @end{dictionary}
  @see-function{gdk-window-get-device-position}")

;;; --- gtk-widget-is-ancestor -------------------------------------------------

(setf (documentation 'gtk-widget-is-ancestor 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[ancestor]{another @class{gtk-widget} instance}
  @return{@arg{true} if @arg{ancestor} contains @arg{widget} as a child,
    grandchild, great grandchild, etc.}
  @begin{short}
    Determines whether @arg{widget} is somewhere inside @arg{ancestor}, possibly
    with intermediate containers.
  @end{short}")

;;; --- gtk-widget-translate-coordinates ---------------------------------------

(setf (documentation 'gtk-widget-translate-coordinates 'function)
 "@version{2012-12-29}
  @argument[src-widget]{a @class{gtk-widget} instance}
  @argument[dest-widget]{a @class{gtk-widget} instance}
  @argument[src-x]{X position relative to @arg{src-widget}}
  @argument[src-y]{Y position relative to @arg{src-widget}}
  @return{@code{nil} if either widget was not realized, or there was no common
    ancestor. Otherwise the X poistion and the Y position relative to
    @arg{dest-widget}}
  @begin{short}
    Translate coordinates relative to @arg{src-widget}'s allocation to
    coordinates relative to @arg{dest-widget}'s allocations.
  @end{short}
  In order to perform this operation, both widgets must be realized, and must
  share a common toplevel.")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide_on_delete ()
;;;
;;; gboolean gtk_widget_hide_on_delete (GtkWidget *widget);
;;;
;;; Utility function; intended to be connected to the "delete-event" signal on
;;; a GtkWindow. The function calls gtk_widget_hide() on its argument, then
;;; returns TRUE. If connected to ::delete-event, the result is that clicking
;;; the close button for a window (on the window frame, top right corner
;;; usually) will hide but not destroy the window. By default, GTK+ destroys
;;; windows when ::delete-event is received.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-set-sytle ---------------------------------------------------

(setf (documentation 'gtk-widget-set-style 'function)
 "@version{2013-1-27}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[style]{a @class{gtk-style}, or @code{nil} to remove the effect of a
    previous call to @sym{gtk-widget-set-style} and go back to the default
    style}
  @begin{short}
    Used to set the @class{gtk-style} for a @arg{widget} (widget->style).
  @end{short}
  Since GTK 3, this function does nothing, the passed in style is ignored.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-set-style} has been deprecated since version 3.0 and should
    not be used in newly-written code. Use @class{gtk-style-context} instead.
  @end{dictionary}
  @see-class{gtk-style-context}")

;;; --- gtk-widget-ensure-style ------------------------------------------------

(setf (documentation 'gtk-widget-ensure-style 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @short{Ensures that @arg{widget} has a style.}

  Not a very useful function; most of the time, if you want the style, the
  widget is realized, and realized widgets are guaranteed to have a style
  already.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-ensure-style} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use @class{gtk-style-context}
    instead.
  @end{dictionary}
  @see-class{gtk-style-context}")

;;; --- gtk-widget-get-style ---------------------------------------------------

(setf (documentation 'gtk-widget-get-style 'function)
 "@version{2013-1-27}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @arg{widget}'s @class{gtk-style}}
  @short{Simply an accessor function that returns widget->style.}
  @begin[Warning]{dictionary}
    @sym{gtk-widget-get-style} has been deprecated since version 3.0 and should
    not be used in newly-written code. Use @class{gtk-style-context} instead.
  @end{dicitonary}
  @see-class{gtk-style-context}")

;;; --- gtk-widget-reset-rc-styles ---------------------------------------------

(setf (documentation 'gtk-widget-reset-rc-styles 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Reset the styles of @arg{widget} and all descendents, so when they are
    looked up again, they get the correct values for the currently loaded RC
    file settings.
  @end{short}

  This function is not useful for applications.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-reset-rc-styles} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use @class{gtk-style-context}
    instead, and @fun{gtk-widget-reset-style}.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-function{gtk-widget-reset-style}")

;;; --- gtk-widget-get-default-style -------------------------------------------

(setf (documentation 'gtk-widget-get-default-style 'function)
 "@version{2013-1-6}
  @return{The default style. This @class{gtk-style} object is owned by GTK+ and
    should not be modified or freed.}
  @short{Returns the default style used by all widgets initially.}
  @begin[Warning]{dictionary}
    @sym{gtk-widget-get-default-style} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use @class{gtk-style-context}
    instead, and @fun{gtk-css-provider-get-default} to obtain a
    @class{gtk-style-provider} with the default widget style information.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-style-provider}
  @see-function{gtk-css-provider-get-default}")

;;; --- gtk-widget-set-direction -----------------------------------------------

(setf (documentation 'gtk-widget-set-direction 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[dir]{the new direction}
  @begin{short}
    Sets the reading direction on a particular @arg{widget}.
  @end{short}
  This direction controls the primary direction for widgets containing text, and
  also the direction in which the children of a container are packed. The
  ability to set the direction is present in order so that correct localization
  into languages with right-to-left reading directions can be done. Generally,
  applications will let the default reading direction present, except for
  containers where the containers are arranged in an order that is explicitely
  visual rather than logical (such as buttons for text justification).

  If the direction is set @code{:none}, then the value set by
  @fun{gtk-widget-set-default-direction} will be used.
  @see-function{gtk-widget-set-default-direction}")

;;; --- gtk-text-direction -----------------------------------------------------

(setf (gethash 'gtk-text-direction atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-text-direction atdoc:*external-symbols*)
 "@version{2012-12-23}
  @short{}
  @begin{pre}
(define-g-enum \"GtkTextDirection\" gtk-text-direction
  (:export t
   :type-initializer \"gtk_text_direction_get_type\")
  (:none 0)
  (:ltr 1)
  (:rtl 2))
  @end{pre}")

;;; --- gtk-widget-get-direction -----------------------------------------------

(setf (documentation 'gtk-widget-get-direction 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The reading direction for the @arg{widget}.}
  @short{Gets the reading direction for a particular @arg{widget}.}
  See @fun{gtk-widget-set-direction}.
  @see-function{gtk-widget-set-direction}")

;;; --- gtk-widget-set-default-direction ---------------------------------------

(setf (documentation 'gtk-widgt-set-default-direction 'function)
 "@version{2013-1-6}
  @argument[dir]{the new default direction. This cannot be @code{:none}.}
  @begin{short}
    Sets the default reading direction for widgets where the direction has not
    been explicitly set by @fun{gtk-widget-set-direction}.
  @end{short}
  @see-function{gtk-widget-set-direction}")

;;; --- gtk-widget-get-default-direction ---------------------------------------

(setf (documentation 'gtk-widget-get-default-direction 'function)
 "@version{2013-1-6}
  @return{The current default direction.}
  @begin{short}
    Obtains the current default reading direction.
  @end{short}
  See @fun{gtk-widget-set-default-direction}.
  @see-function{gtk-widget-set-default-direction}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_shape_combine_region ()
;;;
;;; void gtk_widget_shape_combine_region (GtkWidget *widget,
;;;                                       cairo_region_t *region);
;;;
;;; Sets a shape for this widget's GDK window. This allows for transparent
;;; windows etc., see gdk_window_shape_combine_region() for more information.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; region :
;;;     shape to be added, or NULL to remove an existing shape
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_input_shape_combine_region ()
;;;
;;; void gtk_widget_input_shape_combine_region (GtkWidget *widget,
;;;                                             cairo_region_t *region);
;;;
;;; Sets an input shape for this widget's GDK window. This allows for windows
;;; which react to mouse click in a nonrectangular region, see
;;; gdk_window_input_shape_combine_region() for more information.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; region :
;;;     shape to be added, or NULL to remove an existing shape
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-path --------------------------------------------------------

(setf (documentation 'gtk-widget-path 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[path-type]{@code{:name} or @code{:class}, the default value is
    @code{:name}}
  @return{Returns the path string, or @code{nil}}
  @begin{short}
    Obtains the full path to @arg{widget}.
  @end{short}
  The path is simply the name of a widget and all its parents in the container
  hierarchy, separated by periods. The name of a widget comes from
  @fun{gtk-widget-get-name}. Paths are used to apply styles to a widget in gtkrc
  configuration files. Widget names are the type of the widget by default (e.g.
  \"GtkButton\") or can be set to an application-specific value with
  @fun{gtk-widget-set-name}. By setting the name of a widget, you allow users or
  theme authors to apply styles to that specific widget in their gtkrc file.
  @code{path_reversed_p} fills in the path in reverse order, i.e. starting with
  widget's name instead of starting with the name of widget's outermost
  ancestor.

  With the a value of @code{:class} for the argument @arg{path-type} always
  uses the name of a widget's type, never uses a custom name set with
  @fun{gtk-widget-set-name}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-path} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use @fun{gtk-widget-get-path} instead
  @end{dictionary}
  @see-function{gtk-widget-get-path}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_path ()
;;;
;;; void gtk_widget_class_path (GtkWidget *widget,
;;;                             guint *path_length,
;;;                             gchar **path,
;;;                             gchar **path_reversed);
;;;
;;; Warning
;;;
;;; gtk_widget_class_path has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_get_path() instead
;;;
;;; Same as gtk_widget_path(), but always uses the name of a widget's type,
;;; never uses a custom name set with gtk_widget_set_name().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; path_length :
;;;     location to store the length of the class path, or NULL
;;;
;;; path :
;;;     location to store the class path as an allocated string, or NULL
;;;
;;; path_reversed :
;;;     location to store the reverse class path as an allocated string,
;;;     or NULL
;;; ----------------------------------------------------------------------------

;;; not exported, is implemented as gtk-widget-path


;;; --- gtk-widget-get-composite-name ------------------------------------------

(setf (documentation 'gtk-widget-get-composite-name 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The composite name of @arg{widget}, or @code{nil} if @arg{widget} is
    not a composite child.}
  @short{Obtains the composite name of a @arg{widget}.}")

;;; --- gtk-widget-override-background-color -----------------------------------

(setf (documentation 'gtk-widget-override-background-color 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the background color}
  @argument[color]{the color to assign, or @code{nil} to undo the effect of
    previous calls to @sym{gtk-widget-override-background-color}}
  @short{Sets the background color to use for a @arg{widget}.}

  All other style values are left untouched. See
  @fun{gtk-widget-override-color}.

  Since 3.0
  @see-function{gtk-widget-override-color}")

;;; --- gtk-widget-override-color ----------------------------------------------

(setf (documentation 'gtk-widget-override-color 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the color}
  @argument[color]{the color to assign, or @code{nil} to undo the effect of
    previous calls to @sym{gtk-widget-override-color}}
  @short{Sets the color to use for a @arg{widget}.}
  All other style values are left untouched.

  Since 3.0
  @begin[Notes]{dictionary}
    @begin{itemize}
      @begin{item}
        This API is mostly meant as a quick way for applications to change a
        widget appearance. If you are developing a widgets library and intend
        this change to be themeable, it is better done by setting meaningful CSS
        classes and regions in your widget/container implementation through
        @fun{gtk-style-context-add-class} and
        @fun{gtk-style-context-add-region}.@br{}
        This way, your widget library can install a @class{gtk-css-provider}
        with the @code{GTK_STYLE_PROVIDER_PRIORITY_FALLBACK} priority in order
        to provide a default styling for those widgets that need so, and this
        theming may fully overridden by the user's theme.
      @end{item}
      @begin{item}
        Note that for complex widgets this may bring in undesired results (such
        as uniform background color everywhere), in these cases it is better to
        fully style such widgets through a @class{gtk-css-provider}
        with the @code{GTK_STYLE_PROVIDER_PRIORITY_APPLICATION} priority.
      @end{item}
    @end{itemize}
  @end{dictionary}
  @see-function{gtk-style-context-add-class}
  @see-function{gtk-style-context-add-region}")

;;; --- gtk-widget-override-font -----------------------------------------------

(setf (documentation 'gtk-widget-override-font 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[font-desc]{the font description to use, or @code{nil} to undo the
    effect of previous calls to @sym{gtk-widget-override-font}}
  @short{Sets the font to use for a @arg{widget}.}
  All other style values are left untouched. See
  @fun{gtk-widget-override-color}.

  Since 3.0
  @see-function{gtk-widget-override-color}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_symbolic_color ()
;;;
;;; void gtk_widget_override_symbolic_color (GtkWidget *widget,
;;;                                          const gchar *name,
;;;                                          const GdkRGBA *color);
;;;
;;; Sets a symbolic color for a widget.
;;;
;;; All other style values are left untouched. See gtk_widget_override_color()
;;; for overriding the foreground or background color.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; name :
;;;     the name of the symbolic color to modify
;;;
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo the
;;;     effect of previous calls to gtk_widget_override_symbolic_color()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_cursor ()
;;;
;;; void gtk_widget_override_cursor (GtkWidget *widget,
;;;                                  const GdkRGBA *cursor,
;;;                                  const GdkRGBA *secondary_cursor);
;;;
;;; Sets the cursor color to use in a widget, overriding the "cursor-color" and
;;; "secondary-cursor-color" style properties. All other style values are left
;;; untouched. See also gtk_widget_modify_style().
;;;
;;; Note that the underlying properties have the GdkColor type, so the alpha
;;; value in primary and secondary will be ignored.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; cursor :
;;;     the color to use for primary cursor (does not need to be allocated), or
;;;     NULL to undo the effect of previous calls to of
;;;     gtk_widget_override_cursor()
;;;
;;; secondary_cursor :
;;;     the color to use for secondary cursor (does not need to be allocated),
;;;     or NULL to undo the effect of previous calls to of
;;;     gtk_widget_override_cursor()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_style ()
;;;
;;; void gtk_widget_modify_style (GtkWidget *widget, GtkRcStyle *style);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_style has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use GtkStyleContext with a custom
;;; GtkStyleProvider instead
;;;
;;; Modifies style values on the widget.
;;;
;;; Modifications made using this technique take precedence over style values
;;; set via an RC file, however, they will be overridden if a style is
;;; explicitely set on the widget using gtk_widget_set_style(). The GtkRcStyle
;;; structure is designed so each field can either be set or unset, so it is
;;; possible, using this function, to modify some style values and leave the
;;; others unchanged.
;;;
;;; Note that modifications made with this function are not cumulative with
;;; previous calls to gtk_widget_modify_style() or with such functions as
;;; gtk_widget_modify_fg(). If you wish to retain previous values, you must
;;; first call gtk_widget_get_modifier_style(), make your modifications to the
;;; returned style, then call gtk_widget_modify_style() with that style. On the
;;; other hand, if you first call gtk_widget_modify_style(), subsequent calls to
;;; such functions gtk_widget_modify_fg() will have a cumulative effect with the
;;; initial modifications.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; style :
;;;     the GtkRcStyle holding the style modifications
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_modifier_style ()
;;;
;;; GtkRcStyle * gtk_widget_get_modifier_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_get_modifier_style has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use GtkStyleContext with a custom
;;; GtkStyleProvider instead.
;;;
;;; Returns the current modifier style for the widget. (As set by
;;; gtk_widget_modify_style().) If no style has previously set, a new GtkRcStyle
;;; will be created with all values unset, and set as the modifier style for the
;;; widget. If you make changes to this rc style, you must call
;;; gtk_widget_modify_style(), passing in the returned rc style, to make sure
;;; that your changes take effect.
;;;
;;; Caution: passing the style back to gtk_widget_modify_style() will normally
;;; end up destroying it, because gtk_widget_modify_style() copies the passed-in
;;; style and sets the copy as the new modifier style, thus dropping any
;;; reference to the old modifier style. Add a reference to the modifier style
;;; if you want to keep it alive.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the modifier style for the widget. This rc style is owned by the widget.
;;;     If you want to keep a pointer to value this around, you must add a
;;;     refcount using g_object_ref()
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-modify-fg ---------------------------------------------------

(setf (documentation 'gtk-widget-modify-fg 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the foreground color}
  @argument[color]{the color to assign (does not need to be allocated), or
    @code{nil} to undo the effect of previous calls to of
    @sym{gtk-widget-modify-fg}}
  @short{Sets the foreground color for a @arg{widget} in a particular state.}

  All other style values are left untouched.
  See also @fun{gtk-widget-modify-style}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-modify-fg} has been deprecated since version 3.0 and should
    not be used in newly-written code. Use @fun{gtk-widget-override-color}
    instead.
  @end{dictionary}
  @see-function{gtk-widget-override-color}")

;;; --- gtk-widget-modify-bg ---------------------------------------------------

(setf (documentation 'gtk-widget-modify-bg 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the background color}
  @argument[color]{the color to assign (does not need to be allocated), or
    @code{nil} to undo the effect of previous calls to of
    @sym{gtk-widget-modify-bg}}
  @short{Sets the background color for a @arg{widget} in a particular state.}

  All other style values are left untouched.
  See also @fun{gtk-widget-modify-style}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-modify-bg} has been deprecated since version 3.0 and should
    not be used in newly-written code. Use
    @fun{gtk-widget-override-background-color} instead.
  @end{dictionary}
  @begin[Note]{dictionary}
    Note that \"no window\" widgets (which have the @code{:no-window} flag set)
    draw on their parent container's window and thus may not draw any background
    themselves. This is the case for e.g. @class{gtk-label}.

    To modify the background of such widgets, you have to set the background
    color on their parent; if you want to set the background of a rectangular
    area around a label, try placing the label in a @class{gtk-event-box} widget
    and setting the background color on that.
  @end{dictionary}
  @see-function{gtk-widget-modify-style}
  @see-function{gtk-widget-override-background-color}")

;;; --- gtk-widget-modify-text -------------------------------------------------

(setf (documentation 'gtk-widget-modify-text 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the text color}
  @argument[color]{the color to assign (does not need to be allocated), or
    @code{nil} to undo the effect of previous calls to of
    @sym{gtk-widget-modify-text}}
  @short{Sets the text color for a @arg{widget} in a particular state.}

  All other style values are left untouched. The text color is the foreground
  color used along with the base color (see @fun{gtk-widget-modify-base}) for
  widgets such as @class{gtk-entry} and @class{gtk-text-view}.
  See also @fun{gtk-widget-modify-style}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-modify-text} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use
    @fun{gtk-widget-override-color} instead
  @end{dictionary}
  @see-function{gtk-widget-override-color}")

;;; --- gtk-widget-modify-base -------------------------------------------------

(setf (documentation 'gtk-widget-modify-base 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[state]{the state for which to set the base color}
  @argument[color]{the color to assign (does not need to be allocated), or
    @code{nil} to undo the effect of previous calls to of
    @ym{gtk-widget-modify-base}}
  @begin{short}
    Sets the base color for a @arg{widget} in a particular state.
  @end{short}
  All other style values are left untouched. The base color is the background
  color used along with the text color (see @fun{gtk-widget-modify-text}) for
  widgets such as @class{gtk-entry} and @class{gtk-text-view}. See
  also @fun{gtk-widget-modify-style}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-modify-base} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use
    @fun{gtk-widget-override-background-color} instead.
  @end{dictionary}
  @begin[Note]{dictionary}
    Note that \"no window\" widgets (which have the @code{:no-window} flag set)
    draw on their parent container's window and thus may not draw any background
    themselves. This is the case for e.g. @class{gtk-label}.

    To modify the background of such widgets, you have to set the base color on
    their parent; if you want to set the background of a rectangular area around
    a label, try placing the label in a @class{gtk-event-box} widget and setting
    the base color on that.
  @end{dictionary}
  @see-function{gtk-widget-modify-text}
  @see-function{gtk-widget-modify-style}
  @see-function{gtk-widget-override-background-color}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_font ()
;;;
;;; void gtk_widget_modify_font (GtkWidget *widget,
;;;                              PangoFontDescription *font_desc);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_font has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_font() instead.
;;;
;;; Sets the font to use for a widget.
;;;
;;; All other style values are left untouched.
;;; See also gtk_widget_modify_style().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; font_desc :
;;;     the font description to use, or NULL to undo the effect of previous
;;;     calls to gtk_widget_modify_font()
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-modify-cursor -----------------------------------------------

(setf (documentation 'gtk-widget-modify-cursor 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[primary]{the color to use for primary cursor (does not need to be
    allocated), or @code{nil} to undo the effect of previous calls to of
    @sym{gtk-widget-modify-cursor}.}
  @argument[secondary]{the color to use for secondary cursor (does not need to
    be allocated), or @code{nil} to undo the effect of previous calls to of
    @sym{gtk-widget-modify-cursor}.}
  @begin{short}
    Sets the cursor color to use in a widget, overriding the \"cursor-color\"
    and \"secondary-cursor-color\" style properties.
  @end{short}

  All other style values are left untouched.
  See also @fun{gtk-widget-modify-style}.

  Since 2.12
  @begin[Warning]{dictionary}
    @sym{gtk-widget-modify-cursor} is deprecated since version 3.0 and should
    not be used in newly-written code. Use @fun{gtk-widget-override-cursor}
    instead.
  @end{dictionary}
  @see-function{gtk-widget-modify-style}
  @see-function{gtk-widget-override-cursor}")

;;; --- gtk-widget-create-pango-context ----------------------------------------

(setf (documentation 'gtk-widget-create-pango-context 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The new @class{pango-context}.}
  @begin{short}
    Creates a new @class{pango-context} with the appropriate font map, font
    description, and base direction for drawing text for this @arg{widget}.
  @end{short}
  See also @fun{gtk-widget-get-pango-context}.
  @see-function{gtk-widget-get-pango-context}")

;;; --- gtk-widget-get-pango-context -------------------------------------------

(setf (documentation 'gtk-widget-get-pango-context 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @class{pango-context} for the @arg{widget}.}
  @begin{short}
    Gets a @class{pango-context} with the appropriate font map, font
    description, and base direction for this @arg{widget}.
  @end{short}
  Unlike the context returned by @fun{gtk-widget-create-pango-context}, this
  context is owned by the widget (it can be used until the screen for the widget
  changes or the widget is removed from its toplevel), and will be updated to
  match any changes to the widget's attributes.

  If you create and keep a @class{pango-layout} using this context, you must
  deal with changes to the context by calling
  @fun{pango-layout-context-changed} on the layout in response to the
  \"style-updated\" and \"direction-changed\" signals for the widget.
  @see-function{gtk-widget-create-pango-context}
  @see-function{pango-layout-context-changed}")

;;; --- gtk-widget-create-pango-layout -----------------------------------------

(setf (documentation 'gtk-widget-create-pango-layout 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[text]{text to set on the layout (can be @code{nil})}
  @return{the new @class{pango-layout}}
  @begin{short}
    Creates a new @class{pango-layout} with the appropriate font map, font
    description, and base direction for drawing text for this @arg{widget}.
  @end{short}

  If you keep a @class{pango-layout} created in this way around, in order to
  notify the layout of changes to the base direction or font of this
  @arg{widget}, you must call @fun{pango-layout-context-changed} in response to
  the \"style-updated\" and \"direction-changed\" signals for the widget.
  @see-function{pango-layout-context-changed}")

;;; --- gtk-widget-render-icon -------------------------------------------------

(setf (documentation 'gtk-widget-render-icon 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[stock-id]{a stock ID}
  @argument[size]{a stock size. A size of @code{(GtkIconSize)-1} means render at
    the size of the source and don't scale (if there are multiple source sizes,
    GTK+ picks one of the available sizes)}
  @argument[detail]{render detail to pass to theme engine}
  @return{A new pixbuf, or @code{nil} if the stock ID wasn't known}
  @begin{short}
    A convenience function that uses the theme settings for widget to look up
    @arg{stock-id} and render it to a pixbuf.
  @end{short}
  @arg{stock-id} should be a stock icon ID such as @code{GTK_STOCK_OPEN} or
  @code{GTK_STOCK_OK}. @arg{size} should be a size such as
  @code{GTK_ICON_SIZE_MENU}. @arg{detail} should be a string that identifies the
  widget or code doing the rendering, so that theme engines can special-case
  rendering for that widget or code.

  The pixels in the returned @class{gdk-pixbuf} are shared with the rest of the
  application and should not be modified. The pixbuf should be freed after use
  with @fun{g-object-unref}.
  @begin[Warning]{dictionary}
    @sym{gtk-widget-render-icon} has been deprecated since version 3.0 and
    should not be used in newly-written code. Use
    @fun{gtk-widget-render-icon-pixbuf} instead.
  @end{dictionary}
  @see-function{gtk-widget-render-icon-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_render_icon_pixbuf ()
;;;
;;; GdkPixbuf * gtk_widget_render_icon_pixbuf (GtkWidget *widget,
;;;                                            const gchar *stock_id,
;;;                                            GtkIconSize size);
;;;
;;; A convenience function that uses the theme engine and style settings for
;;; widget to look up stock_id and render it to a pixbuf. stock_id should be a
;;; stock icon ID such as GTK_STOCK_OPEN or GTK_STOCK_OK. size should be a size
;;; such as GTK_ICON_SIZE_MENU.
;;;
;;; The pixels in the returned GdkPixbuf are shared with the rest of the
;;; application and should not be modified. The pixbuf should be freed after use
;;; with g_object_unref().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; stock_id :
;;;     a stock ID
;;;
;;; size :
;;;     a stock size. A size of (GtkIconSize)-1 means render at the size of the
;;;     source and don't scale (if there are multiple source sizes, GTK+ picks
;;;     one of the available sizes)
;;;
;;; Returns :
;;;     a new pixbuf, or NULL if the stock ID wasn't known
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-pop-composite-child -----------------------------------------

(setf (documentation 'gtk-widget-pop-composite-child 'function)
 "@version{2013-1-6}
  @begin{short}
    Cancels the effect of a previous call to
    @fun{gtk-widget-push-composite-child}.
  @end{short}
  @see-function{gtk-widget-push-composite-child}")

;;; --- gtk-widget-push-composite-child ----------------------------------------

(setf (documentation 'gtk-widget-push-composite-child 'function)
 "@version{2013-1-6}
  @begin{short}
    Makes all newly-created widgets as composite children until the
    corresponding @fun{gtk-widget-pop-composite-child} call.
  @end{short}

  A composite child is a child that's an implementation detail of the
  container it's inside and should not be visible to people using the
  container. Composite children aren't treated differently by GTK (but
  see @fun{gtk-container-foreach} vs. @fun{gtk-container-forall}), but e.g. GUI
  builders might want to treat them in a different way.

  Here is a simple example:
  @begin{pre}
 gtk_widget_push_composite_child ();
 scrolled_window->hscrollbar = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL,
                                                  hadjustment);
 gtk_widget_set_composite_name (scrolled_window->hscrollbar, \"hscrollbar\");
 gtk_widget_pop_composite_child ();
 gtk_widget_set_parent (scrolled_window->hscrollbar,
                        GTK_WIDGET (scrolled_window));
 g_object_ref (scrolled_window->hscrollbar);
  @end{pre}
  @see-function{gtk-widget-pop-composite-child}")

;;; --- gtk-widget-queue-draw-area ---------------------------------------------

(setf (documentation 'gtk-widget-queue-draw-area 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[x]{x coordinate of upper-left corner of rectangle to redraw}
  @argument[y]{y coordinate of upper-left corner of rectangle to redraw}
  @argument[width]{width of region to draw}
  @argument[height]{height of region to draw}
  @begin{short}
    Convenience function that calls @fun{gtk-widget-queue-draw-region} on the
    region created from the given coordinates.
  @end{short}

  The region here is specified in widget coordinates. Widget coordinates are a
  bit odd; for historical reasons, they are defined as widget->window
  coordinates for widgets that are not @code{:no-window} widgets, and are
  relative to widget->allocation.x, widget->allocation.y for widgets that are
  @code{:no-window} widgets.")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_region ()
;;;
;;; void gtk_widget_queue_draw_region (GtkWidget *widget,
;;;                                    const cairo_region_t *region);
;;;
;;; Invalidates the rectangular area of widget defined by region by calling
;;; gdk_window_invalidate_region() on the widget's window and all its child
;;; windows. Once the main loop becomes idle (after the current batch of events
;;; has been processed, roughly), the window will receive expose events for the
;;; union of all regions that have been invalidated.
;;;
;;; Normally you would only use this function in widget implementations. You
;;; might also use it to schedule a redraw of a GtkDrawingArea or some portion
;;; thereof.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; region :
;;;     region to draw
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-set-app-paintable -------------------------------------------

(setf (documentation 'gtk-widget-set-app-paintable 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[app-paintable]{@arg{true} if the application will paint on the
    @arg{widget}}
  @begin{short}
    Sets whether the application intends to draw on the widget in an \"draw\"
    handler.
  @end{short}

  This is a hint to the widget and does not affect the behavior of the GTK+
  core; many widgets ignore this flag entirely. For widgets that do pay
  attention to the flag, such as @class{gtk-event-box} and @class{gtk-window},
  the effect is to suppress default themed drawing of the widget's background.
  (Children of the widget will still be drawn.) The application is then entirely
  responsible for drawing the widget background.

  Note that the background is still drawn when the widget is mapped.")

;;; --- gtk-widget-set-double-buffered -----------------------------------------

(setf (documentation 'gtk-widget-set-double-buffered 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[double-buffered]{@arg{true} to double-buffer a @arg{widget}}
  @begin{short}
    Widgets are double buffered by default; you can use this function to turn
    off the buffering.
  @end{short}
  \"Double buffered\" simply means that @fun{gdk-window-begin-paint-region} and
  @fun{gdk-window-end-paint} are called automatically around expose events sent
  to the widget. @fun{gdk-window-begin-paint} diverts all drawing to a widget's
  window to an offscreen buffer, and @fun{gdk-window-end-paint} draws the buffer
  to the screen. The result is that users see the window update in one smooth
  step, and don't see individual graphics primitives being rendered.

  In very simple terms, double buffered widgets don't flicker, so you would
  only use this function to turn off double buffering if you had special needs
  and really knew what you were doing.

  Note: if you turn off double-buffering, you have to handle expose events,
  since even the clearing to the background color or pixmap will not happen
  automatically (as it is done in @fun{gdk-window-begin-paint}).
  @see-function{gdk-window-begin-paint-region}
  @see-function{gdk-window-end-paint}
  @see-function{gdk-window-begin-paint}")

;;; --- gtk-widget-set-redraw-on-allocate --------------------------------------

(setf (documentation 'gtk-widget-set-redraw-on-allocate 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[redraw-on-allocate]{if @arg{true}, the entire widget will be redrawn
    when it is allocated to a new size. Otherwise, only the new portion of the
    widget will be redrawn.}
  @begin{short}
    Sets whether the entire widget is queued for drawing when its size
    allocation changes.
  @end{short}
  By default, this setting is @arg{true} and the entire widget is redrawn on
  every size change. If your widget leaves the upper left unchanged when made
  bigger, turning this setting off will improve performance.

  Note that for @code{:no-window} widgets setting this flag to @code{nil} turns
  off all allocation on resizing: the widget will not even redraw if its
  position changes; this is to allow containers that don't draw anything to
  avoid excess invalidations. If you set this flag on a @code{:no-window}
  widget that does draw on widget->window, you are responsible for invalidating
  both the old and new allocation of the widget when the widget is moved and
  responsible for invalidating regions newly when the widget increases size.")

;;; --- gtk-widget-set-composite-name ------------------------------------------

(setf (documentation 'gtk-widget-set-composite-name 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[name]{the name to set}
  @begin{short}
    Sets a widgets composite name.
  @end{short}
  The widget must be a composite child of its parent; see
  @fun{gtk-widget-push-composite-child}.
  @see-function{gtk-widget-push-composite-child}")

;;; --- gtk-widget-mnemonic-activate -------------------------------------------

(setf (documentation 'gtk-widget-mnemonic-activate 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @arguemnt[group-cycling]{@arg{true} if there are other widgets with the same
    mnemonic}
  @return{@arg{true} if the signal has been handled}
  @short{Emits the \"mnemonic-activate\" signal.}

  The default handler for this signal activates the widget if group_cycling is
  @code{nil}, and just grabs the focus if @arg{group-cycling} is @arg{true}.")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property ()
;;;
;;; void gtk_widget_class_install_style_property (GtkWidgetClass *klass,
;;;                                               GParamSpec *pspec);
;;;
;;; Installs a style property on a widget class. The parser for the style
;;; property is determined by the value type of pspec.
;;;
;;; klass :
;;;     a GtkWidgetClass
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property_parser ()
;;;
;;; void gtk_widget_class_install_style_property_parser
;;;                                                 (GtkWidgetClass *klass,
;;;                                                  GParamSpec *pspec,
;;;                                                  GtkRcPropertyParser parser)
;;;
;;; Installs a style property on a widget class.
;;;
;;; klass :
;;;     a GtkWidgetClass
;;;
;;; pspec :
;;;     the GParamSpec for the style property
;;;
;;; parser :
;;;     the parser for the style property
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-class-find-style-property -----------------------------------

(setf (documentation 'gtk-widget-class-find-style-property 'function)
 "@version{2013-1-6}
  @argument[class]{a pointer to a C widget class structure}
  @argument[property-name]{the name of the style property to find}
  @return{The @class{g-param-spec} of the style property or @code{nil} if
    @arg{class} has no style property with that name.}
  @short{Finds a style property of a widget class by name.}

  Since 2.2")

;;; --- gtk-widget-class-list-style-properties ---------------------------------

(setf (documentation 'gtk-widget-class-list-style-properties 'function)
 "@version{2013-1-6}
  @argument[type]{a widget class name}
  @return{A list of @class{g-param-spec}.}
  @short{Returns all style properties of a widget class.}

 Since 2.2")

;;; --- gtk-widget-region-intersect --------------------------------------------

(setf (documentation 'gtk-widget-region-intersect 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument{region]{a @symbol{cairo-region-t}, in the same coordinate system as
    widget->allocation. That is, relative to widget->window for
    @code{:no-window} widgets; relative to the parent window of widget->window
    for widgets with their own window.}
  @return{A newly allocated region holding the intersection of @arg{widget} and
    @arg{region}. The coordinates of the return value are relative to
    widget->window for @code{:no-window} widgets, and relative to the parent
    window of widget->window for widgets with their own window.}
  @begin{short}
    Computes the intersection of a @arg{widget}'s area and @arg{region},
    returning the intersection.
  @end{short}
  The result may be empty, use @fun{cairo-region-is-empty} to check.
  @see-function{cairo-region-is-empty}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_expose ()
;;;
;;; gint gtk_widget_send_expose (GtkWidget *widget, GdkEvent *event);
;;;
;;; Very rarely-used function. This function is used to emit an expose event on
;;; a widget. This function is not normally used directly. The only time it is
;;; used is when propagating an expose event to a child NO_WINDOW widget, and
;;; that is normally done using gtk_container_propagate_draw().
;;;
;;; If you want to force an area of a window to be redrawn, use
;;; gdk_window_invalidate_rect() or gdk_window_invalidate_region(). To cause
;;; the redraw to be done immediately, follow that call with a call to
;;; gdk_window_process_updates().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; event :
;;;     a expose GdkEvent
;;;
;;; Returns :
;;;     return from the event signal emission (TRUE if the event was handled)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_focus_change ()
;;;
;;; gboolean gtk_widget_send_focus_change (GtkWidget *widget, GdkEvent *event);
;;;
;;; Sends the focus change event to widget
;;;
;;; This function is not meant to be used by applications. The only time it
;;; should be used is when it is necessary for a GtkWidget to assign focus to a
;;; widget that is semantically owned by the first widget even though it's not a
;;; direct child - for instance, a search entry in a floating window similar to
;;; the quick search in GtkTreeView.
;;;
;;; An example of its usage is:
;;;
;;; GdkEvent *fevent = gdk_event_new (GDK_FOCUS_CHANGE);
;;;
;;; fevent->focus_change.type = GDK_FOCUS_CHANGE;
;;; fevent->focus_change.in = TRUE;
;;; fevent->focus_change.window = gtk_widget_get_window (widget);
;;; if (fevent->focus_change.window != NULL)
;;;   g_object_ref (fevent->focus_change.window);
;;;
;;; gtk_widget_send_focus_change (widget, fevent);
;;;
;;; gdk_event_free (event);
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; event :
;;;     a GdkEvent of type GDK_FOCUS_CHANGE
;;;
;;; Returns :
;;;     the return value from the event signal emission: TRUE if the event was
;;;     handled, and FALSE otherwise
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get ()
;;;
;;; void gtk_widget_style_get (GtkWidget *widget,
;;;                            const gchar *first_property_name,
;;;                            ...);
;;;
;;; Gets the values of a multiple style properties of widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; ... :
;;;     pairs of property names and locations to return the property values,
;;;     starting with the location for first_property_name, terminated by NULL.
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-style-get-property ------------------------------------------

(setf (documentation 'gtk-widget-style-get-property 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[property-name]{the name of a style property}
  @return{The style property value.}
  @short{Gets the value of a style property of @arg{widget}.}")

;;; ----------------------------------------------------------------------------

;;; TODO: Extension of the Lisp binding. Add the documentation.

#|
(defun gtk-widget-style-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
      (let ((g-param-spec (gtk-widget-class-find-style-property class
                                                                property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'gtk-widget-style-property-info)

;;; ----------------------------------------------------------------------------

(defun gtk-widget-style-property-type (widget property-name)
  (let ((property-info (gtk-widget-style-property-info
                                                   (g-type-from-instance widget)
                                                   property-name)))
    (param-spec-type property-info)))

(export 'gtk-widget-style-property-type)

;;; ----------------------------------------------------------------------------

;; This implementation is wrong.

(defun gtk-widget-style-property-value (widget property-name
                                               &optional property-type)
  (unless property-type
    (setf property-type
          (gtk-widget-style-property-type widget property-name)))
  (setf property-type (gtype property-type))
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue property-type)
    (prog1 (%gtk-widget-style-get-property widget property-name gvalue)
      (g-value-unset gvalue))))

(export 'gtk-widget-style-property-value)
|#

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_valist ()
;;;
;;; void gtk_widget_style_get_valist (GtkWidget *widget,
;;;                                   const gchar *first_property_name,
;;;                                   va_list var_args);
;;;
;;; Non-vararg variant of gtk_widget_style_get(). Used primarily by language
;;; bindings.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; var_args :
;;;     a va_list of pairs of property names and locations to return the
;;;     property values, starting with the location for first_property_name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_attach ()
;;;
;;; void gtk_widget_style_attach (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_style_attach is deprecated and should not be used in
;;; newly-written code. 3.0. This step is unnecessary with GtkStyleContext.
;;;
;;; This function attaches the widget's GtkStyle to the widget's GdkWindow. It
;;; is a replacement for
;;;
;;; widget->style = gtk_style_attach (widget->style, widget->window);
;;;
;;; and should only ever be called in a derived widget's "realize"
;;; implementation which does not chain up to its parent class' "realize"
;;; implementation, because one of the parent classes (finally GtkWidget) would
;;; attach the style itself.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_type ()
;;;
;;; void gtk_widget_class_set_accessible_type (GtkWidgetClass *widget_class,
;;;                                            GType type);
;;;
;;; Sets the type to be used for creating accessibles for widgets of
;;; widget_class. The given type must be a subtype of the type used for
;;; accessibles of the parent class.
;;;
;;; This function should only be called from class init functions of widgets.
;;;
;;; widget_class :
;;;     class to set the accessible type for
;;;
;;; type :
;;;     The object type that implements the accessible for widget_class
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_role ()
;;;
;;; void gtk_widget_class_set_accessible_role (GtkWidgetClass *widget_class,
;;;                                            AtkRole role);
;;;
;;; Sets the default AtkRole to be set on accessibles created for widgets of
;;; widget_class. Accessibles may decide to not honor this setting if their role
;;; reporting is more refined. Calls to gtk_widget_class_set_accessible_type()
;;; will reset this value.
;;;
;;; In cases where you want more fine-grained control over the role of
;;; accessibles created for widget_class, you should provide your own accessible
;;; type and use gtk_widget_class_set_accessible_type() instead.
;;;
;;; If role is ATK_ROLE_INVALID, the default role will not be changed and the
;;; accessible's default role will be used instead.
;;;
;;; This function should only be called from class init functions of widgets.
;;;
;;; widget_class :
;;;     class to set the accessible role for
;;;
;;; role :
;;;     The role to use for accessibles created for widget_class
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-get-accessible ----------------------------------------------

(setf (documentation 'gtk-widget-get-accessible 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{the @code{AtkObject} associated with widget}
  @begin{short}
    Returns the accessible object that describes the widget to an assistive
    technology.
  @end{short}

  If accessibility support is not available, this @code{AtkObject} instance may
  be a no-op. Likewise, if no class-specific @code{AtkObject} implementation is
  available for the widget instance in question, it will inherit an
  @code{AtkObject} implementation from the first ancestor class for which such
  an implementation is defined.

  The documentation of the ATK library contains more information about
  accessible objects and their uses.")

;;; --- gtk-widget-child-focus -------------------------------------------------

(setf (documentation 'gtk-widget-child-focus 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[direction]{direction of focus movement}
  @return{@arg{true} if focus ended up inside widget}
  @begin{short}
    This function is used by custom widget implementations; if you're writing an
    app, you'd use @fun{gtk-widget-grab-focus} to move the focus to a particular
    widget, and @fun{gtk-container-set-focus-chain} to change the focus tab
    order.
  @end{short}
  So you may want to investigate those functions instead.

  @sym{gtk-widget-child-focus} is called by containers as the user moves around
  the window using keyboard shortcuts. @arg{direction} indicates what kind of
  motion is taking place (up, down, left, right, tab forward, tab backward).
  @sym{gtk-widget-child-focus} emits the \"focus\" signal; widgets override the
  default handler for this signal in order to implement appropriate focus
  behavior.

  The default ::focus handler for a widget should return @arg{true} if moving in
  direction left the focus on a focusable location inside that widget, and
  @code{nil} if moving in direction moved the focus outside the widget. If
  returning @arg{true}, widgets normally call @fun{gtk-widget-grab-focus} to
  place the focus accordingly; if returning @code{nil}, they don't modify the
  current focus location.
  @see-function{gtk-widget-grab-focus}
  @see-function{gtk-container-set-focus-chain}")

;;; --- gtk-widget-child-notify ------------------------------------------------

(setf (documentation 'gtk-widget-child-notify 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[child-property]{the name of a child property installed on the class
    of widget's parent}
  @begin{short}
    Emits a \"child-notify\" signal for the child property child_property on
    widget.
  @end{short}

  This is the analogue of @fun{g-object-notify} for child properties.
  Also see @fun{gtk-container-child-notify}.
  @see-function{g-object-notify}
  @see-function{gtk-container-child-notify}")

;;; --- gtk-widget-freeze-child-notify -----------------------------------------

(setf (documentation 'gtk-widget-freeze-child-notify 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Stops emission of \"child-notify\" signals on widget.
  @end{short}
  The signals are queued until @fun{gtk-widget-thaw-child-notify} is called on
  widget.

  This is the analogue of @fun{g-object-freeze-notify} for child properties.
  @see-function{gtk-widget-thaw-child-notify}
  @see-function{g-object-freeze-notify}")

;;; --- gtk-widget-get-child-visible -------------------------------------------

(setf (documentation 'gtk-widget-get-child-visible 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the widget is mapped with the parent.}
  @begin{short}
    Gets the value set with @fun{gtk-widget-set-child-visible}.
  @end{short}
  If you feel a need to use this function, your code probably needs
  reorganization.

  This function is only useful for container implementations and never should
  be called by an application.
  @see-function{gtk-widget-set-child-visible}")

;;; --- gtk-widget-get-parent --------------------------------------------------

(setf (documentation 'gtk-widget-get-parent 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The parent container of @arg{widget}, or @code{nil}.}
  @short{Returns the parent container of @arg{widget}.}")

;;; --- gtk-widget-get-settings ------------------------------------------------

(setf (documentation 'gtk-widget-get-settings 'documentation)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The relevant @class{gtk-setting}'s object.}
  @short{Gets the settings object holding the settings used for this
    @arg{widget}.}

  Note that this function can only be called when the @class{gtk-widget} is
  attached to a toplevel, since the settings object is specific to a particular
  @class{gdk-screen}.")

;;; --- gtk-widget-get-clipboard -----------------------------------------------

(setf (documentation 'gtk-widget-get-clipboard 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[selection]{a @symbol{gdk-atom} which identifies the clipboard to
    use. @code{GDK_SELECTION_CLIPBOARD} gives the default clipboard. Another
    common value is @code{GDK_SELECTION_PRIMARY}, which gives the primary X
    selection.}
  @return{The appropriate clipboard object. If no clipboard already exists, a
    new one will be created. Once a clipboard object has been created, it is
    persistent for all time.}
  @begin{short}
    Returns the clipboard object for the given selection to be used with widget.
  @end{short}
  @arg{widget} must have a @class{gdk-display} associated with it, so must be
  attached to a toplevel window.

  Since 2.2")

;;; --- gtk-widget-get-display -------------------------------------------------

(setf (documentation 'gtk-widget-get-display 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @class{gdk-display} for the toplevel for this @arg{widget}.}
  @begin{short}
    Get the @class{gdk-display} for the toplevel window associated with this
    @arg{widget}.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk-window} at the top.

  In general, you should only create display specific resources when a widget
  has been realized, and you should free those resources when the widget is
  unrealized.

  Since 2.2")

;;; --- gtk-widget-get-root-window ---------------------------------------------

(setf (documentation 'gtk-widget-get-root-window 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @class{gdk-window} root window for the toplevel for this widget}
  @begin{short}
    Get the root window where this widget is located.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with @class{gtk-window} at the top.

  The root window is useful for such purposes as creating a popup
  @class{gdk-window} associated with the window. In general, you should only
  create display specific resources when a widget has been realized, and you
  should free those resources when the widget is unrealized.

  Since 2.2")

;;; --- gtk-widget-get-screen --------------------------------------------------

(setf (documentation 'gtk-widget-get-screen 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @class{gdk-screen} for the toplevel for this @arg{widget}.}
  @begin{short}
    Get the @class{gdk-screen} from the toplevel window associated with this
    widget.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk-window} at the top.

  In general, you should only create screen specific resources when a widget
  has been realized, and you should free those resources when the widget is
  unrealized.

  Since 2.2")

;;; --- gtk-widget-has-screen --------------------------------------------------

(setf (documentation 'gtk-widget-has-screen 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if there is a @class{gdk-screen} associcated with the
    widget.}
  @begin{short}
    Checks whether there is a @class{gdk-screen} is associated with this
    @arg{widget}.
  @end{short}
  All toplevel widgets have an associated screen, and all widgets added into a
  hierarchy with a toplevel window at the top.

  Since 2.2")

;;; --- gtk-widget-get-size-request --------------------------------------------

(setf (documentation 'gtk-widget-get-size-request 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@code{width} -- width, or @code{nil}@br{}
          @code{height} -- height, or @code{nil}}
  @begin{short}
    Gets the size request that was explicitly set for the widget using
    @fun{gtk-widget-set-size-request}.
  @end{short}
  A value of -1 stored in width or height indicates that that dimension has not
  been set explicitly and the natural requisition of the widget will be used
  intead. See @fun{gtk-widget-set-size-request}. To get the size a widget will
  actually request, call @fun{gtk-widget-get-preferred-size} instead of this
  function.
  @see-function{gtk-widget-set-size-request}
  @see-function{gtk-widget-get-preferred-size}")

;;; --- gtk-widget-set-child-visible -------------------------------------------

(setf (documentation 'gtk-widget-set-child-visible 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[is-visible]{if @arg{true}, @arg{widget} should be mapped along with
    its parent.}
  @begin{short}
    Sets whether @arg{widget} should be mapped along with its when its parent is
    mapped and widget has been shown with @fun{gtk-widget-show}.
  @end{short}

  The child visibility can be set for widget before it is added to a container
  with @fun{gtk-widget-set-parent}, to avoid mapping children unnecessary before
  immediately unmapping them. However it will be reset to its default state of
  @arg{true} when the widget is removed from a container.

  Note that changing the child visibility of a widget does not queue a resize
  on the widget. Most of the time, the size of a widget is computed from all
  visible children, whether or not they are mapped. If this is not the case,
  the container can queue a resize itself.

  This function is only useful for container implementations and never should
  be called by an application.
  @see-function{gtk-widget-show}
  @see-function{gtk-widget-set-parent}")

;;; --- gtk-widget-set-size-request --------------------------------------------

(setf (documentation 'gtk-widget-set-size-request 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[width]{width @arg{widget} should request, or -1 to unset}
  @argument[height]{height @arg{widget} should request, or -1 to unset}
  @begin{short}
    Sets the minimum size of a @arg{widget}.
  @end{short}
  That is, the @arg{widget}'s size request will be @arg{width} by @arg{height}.
  You can use this function to force a @arg{widget} to be either larger or
  smaller than it normally would be.

  In most cases, @fun{gtk-window-set-default-size} is a better choice for
  toplevel windows than this function; setting the default size will still allow
  users to shrink the window. Setting the size request will force them to leave
  the window at least as large as the size request. When dealing with window
  sizes, @fun{gtk-window-set-geometry-hints} can be a useful function as well.

  Note the inherent danger of setting any fixed size - themes, translations
  into other languages, different fonts, and user action can all change the
  appropriate size for a given @arg{widget}. So, it's basically impossible to
  hardcode a size that will always be correct.

  The size request of a @arg{widget} is the smallest size a @arg{widget} can
  accept while still functioning well and drawing itself correctly. However in
  some strange cases a @arg{widget} may be allocated less than its requested
  size, and in many cases a @arg{widget} may be allocated more space than it
  requested.

  If the size request in a given direction is -1 (unset), then the \"natural\"
  size request of the @arg{widget} will be used instead.

  Widgets can't actually be allocated a size less than 1 by 1, but you can
  pass 0,0 to this function to mean \"as small as possible\".

  The size request set here does not include any margin from the
  @class{gtk-widget} properties @code{margin-left}, @code{margin-right},
  @code{margin-top}, and @code{margin-bottom}, but it does include pretty much
  all other padding or border properties set by any subclass of
  @class{gtk-widget}.
  @see-function{gtk-window-set-default-size}
  @see-function{gtk-window-set-geometry-hints}")

;;; --- gtk-widget-thaw-child-notify -------------------------------------------

(setf (documentation 'gtk-widget-thaw-child-notify 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Reverts the effect of a previous call to
    @fun{gtk-widget-freeze-child-notify}.
  @end{short}
  This causes all queued \"child-notify\" signals on @arg{widget} to be
  emitted.
  @see-function{gtk-widget-freeze-child-notify}")

;;; --- gtk-widget-set-no-show-all----------------------------------------------

(setf (documentation 'gtk-widget-set-no-show-all 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[no-show-all]{the new value for the \"no-show-all\" property}
  @begin{short}
    Sets the \"no-show-all\" property, which determines whether calls to
    @fun{gtk-widget-show-all} will affect this widget.
  @end{short}

  This is mostly for use in constructing widget hierarchies with externally
  controlled visibility, see @class{gtk-ui-manager}.

  Since 2.4
  @see-function{gtk-widget-show-all}")

;;; --- gtk-widget-get-now-show-all --------------------------------------------

(setf (documentation 'gtk-widget-get-now-show-all 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{the current value of the \"no-show-all\" property.}
  @begin{short}
    Returns the current value of the \"no-show-all\" property, which determines
    whether calls to @fun{gtk-widget-show-all} will affect this widget.
  @end{short}

  Since 2.4
  @see-function{gtk-widget-show-all}")

;;; --- gtk-widget-list-mnemonic-labels ----------------------------------------

(setf (documentation 'gtk-widget-list-mnemonic-labels 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The list of mnemonic labels.}
  @begin{short}
    Returns a list of the widgets, normally labels, for which this @arg{widget}
    is the target of a mnemonic.
  @end{short}
  (See for example, @fun{gtk-label-set-mnemonic-widget}.)

  The widgets in the list are not individually referenced. If you want to
  iterate through the list and perform actions involving callbacks that might
  destroy the widgets, you must call
  @code{g_list_foreach (result, (GFunc)g_object_ref, NULL)} first, and then
  unref all the widgets afterwards.

  Since 2.4
  @begin[Example]{dictionary}
    @begin{pre}
 (setq button (gtk-button-new-with-mnemonic \"_Hello\"))
=> #<GTK-BUTTON {C2794C9@}>
 (gtk-widget-list-mnemonic-labels button)
=> (#<GTK-LABEL {C292FE1@}>)
    @end{pre}
  @end{dictionary}
  @see-function{gtk-label-set-mnemonic-widget}")

;;; --- gtk-widget-add-mnemonic-label ------------------------------------------

(setf (documentation 'gtk-widget-add-mnemonic-label 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[label]{a @class{gtk-widget} instance that acts as a mnemonic label
    for @arg{widget}}
  @begin{short}
    Adds a widget to the list of mnemonic labels for this widget.
  @end{short}
  (See @fun{gtk-widget-list-mnemonic-labels}). Note the list of mnemonic labels
  for the widget is cleared when the widget is destroyed, so the caller must
  make sure to update its internal state at this point as well, by using a
  connection to the \"destroy\" signal or a weak notifier.

  Since 2.4")

;;; --- gtk-widget-remove-mnemonic-label ---------------------------------------

(setf (documentation 'gtk-widget-remove-mnemonic-label 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[label]{a @class{gtk-widget} instance that was previously set as a
    mnemnic label for widget with @fun{gtk-widget-add-mnemonic-label}.}
  @begin{short}
    Removes a widget from the list of mnemonic labels for this @arg{widget}.
  @end{short}
  (See @fun{gtk-widget-list-mnemonic-labels}). The widget must have previously
  been added to the list with @fun{gtk-widget-add-mnemonic-label}.

  Since 2.4
  @see-function{gtk-widget-list-mnemonic-labels}
  @see-function{gtk-widget-add-mnemoic-label}")

;;; --- gtk-widget-is-composited -----------------------------------------------

(setf (documentation 'gtk-widget-is-composited 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the @arg{widget} can rely on its alpha channel being
    drawn correctly.}
  @begin{short}
    Whether @arg{widget} can rely on having its alpha channel drawn correctly.
  @end{short}
  On X11 this function returns whether a compositing manager is running for
  widget's screen.

  Please note that the semantics of this call will change in the future if
  used on a @arg{widget} that has a composited window in its hierarchy (as set
  by @fun{gdk-window-set-composited}).

  Since 2.10
  @see-function{gdk-window-set-composited}")

;;; --- gtk-widget-error-bell --------------------------------------------------

(setf (documentation 'gtk-widget-error-bell 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Notifies the user about an input-related error on this widget.
  @end{short}
  If the \"gtk-error-bell\" setting is @arg{true}, it calls
  @fun{gdk-window-beep}, otherwise it does nothing.

  Note that the effect of @fun{gdk-window-beep} can be configured in many ways,
  depending on the windowing backend and the desktop environment or window
  manager that is used.

  Since 2.12
  @see-function{gdk-window-beep}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_keynav_failed ()
;;;
;;; gboolean gtk_widget_keynav_failed (GtkWidget *widget,
;;;                                    GtkDirectionType direction);
;;;
;;; This function should be called whenever keyboard navigation within a single
;;; widget hits a boundary. The function emits the "keynav-failed" signal on the
;;; widget and its return value should be interpreted in a way similar to the
;;; return value of gtk_widget_child_focus():
;;;
;;; When TRUE is returned, stay in the widget, the failed keyboard navigation is
;;; Ok and/or there is nowhere we can/should move the focus to.
;;;
;;; When FALSE is returned, the caller should continue with keyboard navigation
;;; outside the widget, e.g. by calling gtk_widget_child_focus() on the widget's
;;; toplevel.
;;;
;;; The default ::keynav-failed handler returns TRUE for GTK_DIR_TAB_FORWARD and
;;; GTK_DIR_TAB_BACKWARD. For the other values of GtkDirectionType, it looks at
;;; the "gtk-keynav-cursor-only" setting and returns FALSE if the setting is
;;; TRUE. This way the entire user interface becomes cursor-navigatable on input
;;; devices such as mobile phones which only have cursor keys but no tab key.
;;;
;;; Whenever the default handler returns TRUE, it also calls
;;; gtk_widget_error_bell() to notify the user of the failed keyboard
;;; navigation.
;;;
;;; A use case for providing an own implementation of ::keynav-failed (either
;;; by connecting to it or by overriding it) would be a row of GtkEntry widgets
;;; where the user should be able to navigate the entire row with the cursor
;;; keys, as e.g. known from user interfaces that require entering license keys.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; direction :
;;;     direction of focus movement
;;;
;;; Returns :
;;;     TRUE if stopping keyboard navigation is fine, FALSE if the emitting
;;;     widget should try to handle the keyboard navigation attempt in its
;;;     parent container(s).
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-get-tooltip-markup ------------------------------------------

(setf (documentation 'gtk-widget-get-tooltip-markup 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The tooltip text, or @code{nil}.}
  @short{Gets the contents of the tooltip for @arg{widget}.}

  Since 2.12")

;;; --- gtk-widget-set-tooltip-markup ------------------------------------------

(setf (documentation 'gtk-widget-set-tooltip-markup 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[markup]{the contents of the tooltip for widget, or @code{nil}}
  @begin{short}
    Sets @arg{markup} as the contents of the tooltip, which is marked up with
    the Pango text markup language.
  @end{short}

  This function will take care of setting \"has-tooltip\" to @arg{true} and of
  the default handler for the \"query-tooltip\" signal.

  See also the \"tooltip-markup\" property and @fun{gtk-tooltip-set-markup}.

  Since 2.12
  @see-function{gtk-tooltip-set-markup}")

;;; --- gtk-widget-set-tooltip-text --------------------------------------------

(setf (documentation 'gtk-widget-set-tooltip-text 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[text]{the contents of the tooltip for @arg{widget}}
  @begin{short}
    Sets text as the contents of the tooltip.
  @end{short}
  This function will take care of setting \"has-tooltip\" to TRUE and of the
  default handler for the \"query-tooltip\" signal.

  See also the \"tooltip-text\" property and @fun{gtk-tooltip-set-text}.

  Since 2.12
  @see-function{gtk-tooltip-set-text}")

;;; --- gtk-widget-get-tooltip-text --------------------------------------------

(setf (documentation 'gtk-widget-get-tooltip-text 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The tooltip text, or @code{nil}.}
  @begin{short}
    Gets the contents of the tooltip for @arg{widget}.
  @end{short}

  Since 2.12")

;;; --- gtk-widget-get-tooltip-window ------------------------------------------

(setf (documentation 'gtk-widget-get-tooltip-window 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @class{gtk-window} instance of the current tooltip}
  @begin{short}
    Returns the @class{gtk-window} instance of the current tooltip.
  @end{short}
  This can be the @class{gtk-window} instance created by default, or the custom
  tooltip window set using @fun{gtk-widget-set-tooltip-window}.

  Since 2.12
  @see-function{gtk-widget-set-tooltip-window}")

;;; --- gtk-widget-set-tooltip-window ------------------------------------------

(setf (documentation 'gtk-widget-set-tooltip-window 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[custom-window]{a @class{gtk-window} instance, or @arg{nil}}
  @begin{short}
    Replaces the default, usually yellow, window used for displaying tooltips
    with @arg{custom-window}.
  @end{short}
  GTK+ will take care of showing and hiding @arg{custom-window} at the right
  moment, to behave likewise as the default tooltip window. If
  @arg{custom-window} is @arg{nil}, the default tooltip window will be used.

  If the custom window should have the default theming it needs to have the
  name \"gtk-tooltip\", see @fun{gtk-widget-set-name}.

  Since 2.12
  @see-function{gtk-widget-set-name}")

;;; --- gtk-widget-get-has-tooltip ---------------------------------------------

(setf (documentation 'gtk-widget-get-has-tooltip 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{Current value of the \"has-tooltip\" property on @arg{widget}.}
  @begin{short}
    Returns the current value of the \"has-tooltip\" property.
  @end{short}
  See \"has-tooltip\" for more information.

  Since 2.12")

;;; --- gtk-widget-set-has-tooltip ---------------------------------------------

(setf (documentation 'gtk-widget-set-has-tooltip 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[has-tooltip]{whether or not @arg{widget} has a tooltip.}
  @begin{short}
    Sets the \"has-tooltip\" property on widget to @arg{has-tooltip}.
  @end{short}
  See \"has-tooltip\" for more information.

  Since 2.12")

;;; --- gtk-widget-trigger-tooltip-query ---------------------------------------

(setf (documentation 'gtk-widget-trigger-tooltip-query 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @begin{short}
    Triggers a tooltip query on the display where the toplevel of @arg{widget}
    is located.
  @end{short}
  See @fun{gtk-tooltip-trigger-tooltip-query} for more information.

  Since 2.12
  @see-function{gtk-tooltip-trigger-tooltip-query}")

;;; --- gtk-widget-get-window --------------------------------------------------

(setf (documentation 'gtk-widget-get-window 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{widget}'s window}
  @begin{short}
    Returns the @arg{widget}'s window if it is realized, @code{nil} otherwise
  @end{short}

  Since 2.14")

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_should_draw_window ()
;;;
;;; gboolean gtk_cairo_should_draw_window (cairo_t *cr, GdkWindow *window);
;;;
;;; This function is supposed to be called in "draw" implementations for widgets
;;; that support multiple windows. cr must be untransformed from invoking of the
;;; draw function. This function will return TRUE if the contents of the given
;;; window are supposed to be drawn and FALSE otherwise. Note that when the
;;; drawing was not initiated by the windowing system this function will return
;;; TRUE for all windows, so you need to draw the bottommost window first. Also,
;;; do not use "else if" statements to check which window should be drawn.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; window :
;;;     the window to check. window may not be an input-only window.
;;;
;;; Returns :
;;;     TRUE if window should be drawn
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_transform_to_window ()
;;;
;;; void gtk_cairo_transform_to_window (cairo_t *cr,
;;;                                     GtkWidget *widget,
;;;                                     GdkWindow *window);
;;;
;;; Transforms the given cairo context cr that from widget-relative coordinates
;;; to window-relative coordinates. If the widget's window is not an ancestor of
;;; window, no modification will be applied.
;;;
;;; This is the inverse to the transformation GTK applies when preparing an
;;; expose event to be emitted with the "draw" signal. It is intended to help
;;; porting multiwindow widgets from GTK+ 2 to the rendering architecture of
;;; GTK+ 3.
;;;
;;; cr :
;;;     the cairo context to transform
;;;
;;; widget :
;;;     the widget the context is currently centered for
;;;
;;; window :
;;;     the window to transform the context to
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-get-allocated-width -----------------------------------------

(setf (documentation 'gtk-widget-get-allocated-width 'function)
 "@version{2013-1-6}
  @argument[widget]{the @arg{widget} to query}
  @return{The width of the @arg{widget}.}
  @begin{short}
    Returns the width that has currently been allocated to @arg{widget}.
  @end{short}
  This function is intended to be used when implementing handlers for the
  \"draw\" function.")

;;; --- gtk-widget-get-allocated-height ----------------------------------------

(setf (documentation 'gtk-widget-get-allocated-height 'function)
 "@version{2013-1-6}
  @argument[widget]{the @arg{widget} to query}
  @return{The height of the @arg{widget}.}
  @begin{short}
    Returns the height that has currently been allocated to @arg{widget}.
  @end{short}
  This function is intended to be used when implementing handlers for the
  \"draw\" function.")

;;; --- gtk-widget-get-allocation ----------------------------------------------

(setf (documentation 'gtk-widget-get-allocation 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[allocation]{a pointer to a @class{gtk-allocation} structure to copy
    to}
  @begin{short}
    Retrieves the widget's allocation.
  @end{short}

  Note, when implementing a @class{gtk-container}: a widget's allocation will be
  its \"adjusted\" allocation, that is, the widget's parent container typically
  calls @fun{gtk-widget-size-allocate} with an allocation, and that
  allocation is then adjusted (to handle margin and alignment for example)
  before assignment to the widget. @sym{gtk-widget-get-allocation} returns the
  adjusted allocation that was actually assigned to the widget. The adjusted
  allocation is guaranteed to be completely contained within the
  @fun{gtk-widget-size-allocate} allocation, however. So a @class{gtk-container}
  is guaranteed that its children stay inside the assigned bounds, but not that
  they have exactly the bounds the container assigned. There is no way to get
  the original allocation assigned by @fun{gtk-widget-size-allocate}, since
  it isn't stored; if a container implementation needs that information it will
  have to track it itself.

  Since 2.18")

;;; --- gtk-widget-set-allocation ----------------------------------------------

(setf (documentation 'gtk-widget-set-allocation 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[allocation]{a pointer to a @class{gtk-allocation} structure to copy
    from}
  @begin{short}
    Sets the widget's allocation. This should not be used directly, but from
    within a widget's size_allocate method.
  @end{short}

  The allocation set should be the \"adjusted\" or actual allocation. If you're
  implementing a @class{gtk-container}, you want to use
  @fun{gtk-widget-size-allocate} instead of @sym{gtk-widget-set-allocation}. The
  @code{GtkWidgetClass::adjust_size_allocation} virtual method adjusts the
  allocation inside @fun{gtk-widget-size-allocate} to create an adjusted
  allocation.

  Since 2.18")

;;; --- gtk-widget-get-app-paintable -------------------------------------------

(setf (documentation 'gtk-widget-get-app-paintable 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the @arg{widget} is app paintable}
  @begin{short}
    Determines whether the application intends to draw on the widget in an
    \"draw\" handler.
  @end{short}
  See @fun{gtk-widget-set-app-paintable}

  Since 2.18
  @see-function{gtk-widget-set-app-paintable}")

;;; --- gtk-widget-get-can-default ---------------------------------------------

(setf (documentation 'gtk-widget-get-can-default 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{ture} if @arg{widget} can be a default widget, @code{nil}
    otherwise}
  @short{Determines whether @arg{widget} can be a default widget.}
  See @fun{gtk-widget-set-can-default}.

  Since 2.18
  @see-function{gtk-widget-set-can-default}")

;;; --- gtk-widget-set-can-default ---------------------------------------------

(setf (documentation 'gtk-widget-set-can-default 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[can-default]{whether or not @arg{widget} can be a default widget.}
  @begin{short}
    Specifies whether @arg{widget} can be a default widget.
  @end{short}
  See @fun{gtk-widget-grab-default} for details about the meaning of
  \"default\".

  Since 2.18
  @see-function{gtk-widget-grab-default}")

;;; --- gtk-widget-get-can-focus -----------------------------------------------

(setf (documentation 'gtk-widget-get-can-focus 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} can own the input focus, @code{nil}
    otherwise}
  @short{Determines whether @arg{widget} can own the input focus.}
  See @fun{gtk-widget-set-can-focus}.

  Since 2.18")

;;; --- gtk-widget-set-can-focus -----------------------------------------------

(setf (documentation 'gtk-widget-set-can-focus 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[can-focus]{whether or not @arg{widget} can own the input focus.}
  @short{Specifies whether @arg{widget} can own the input focus.}
  See @fun{gtk-widget-grab-focus} for actually setting the input focus on a
  widget.

  Since 2.18
  @see-function{gtk-widget-grab-focus}")

;;; --- gtk-widget-get-double-buffered -----------------------------------------

(setf (documentation 'gtk-widget-get-double-buffered 'function)
 "@version{2012-12-29}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the @arg{widget} is double buffered}
  @short{Determines whether the @arg{widget} is double buffered.}
  See @fun{gtk-widget-set-double-buffered}.

  Since 2.18")

;;; --- gtk-widget-get-has-window ----------------------------------------------

(setf (documentation 'gtk-widget-get-has-window 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} has a window, @arg{false} otherwise}
  @begin{short}
    Determines whether @arg{widget} has a @class{gdk-window} of its own.
  @end{short}
  See @fun{gtk-widget-set-has-window}.

  Since 2.18")

;;; --- gtk-widget-set-has-window ----------------------------------------------

(setf (documentation 'gtk-widget-set-has-window 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[has-window]{whether or not @arg{widget} has a window.}
  @begin{short}
    Specifies whether @arg{widget} has a @class{gdk-window} of its own.
  @end{short}
  Note that all realized widgets have a non-NULL \"window\" pointer
  (@fun{gtk-widget-get-window} never returns a @code{NULL} window when a widget
  is realized), but for many of them it's actually the @class{gdk-window} of one
  of its parent widgets. Widgets that do not create a window for themselves in
  \"realize\" must announce this by calling this function with
  @code{has-window = nil}.

  This function should only be called by widget implementations, and they
  should call it in their @code{init()} function.

  Since 2.18")

;;; --- gtk-widget-get-sensitive -----------------------------------------------

(setf (documentation 'gtk-widget-get-sensitive 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the @arg{widget} is sensitive}
  @begin{short}
    Returns the @arg{widget}'s sensitivity (in the sense of returning the value
    that has been set using @fun{gtk-widget-set-sensitive}).
  @end{short}

  The effective sensitivity of a widget is however determined by both its own
  and its parent widget's sensitivity. See @fun{gtk-widget-is-sensitive}.

  Since 2.18")

;;; --- gtk-widget-is-sensitive ------------------------------------------------

(setf (documentation 'gtk-widget-is-sensitive 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the widget is effectively sensitive}
  @begin{short}
    Returns the widget's effective sensitivity, which means it is sensitive
   itself and also its parent widget is sensitive.
  @end{short}

  Since 2.18")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state ()
;;;
;;; GtkStateType gtk_widget_get_state (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_get_state is deprecated and should not be used in newly-written
;;; code. 3.0. Use gtk_widget_get_state_flags() instead.
;;;
;;; Returns the widget's state. See gtk_widget_set_state().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the state of widget.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-get-visible -------------------------------------------------

(setf (documentation 'gtk-widget-get-visible 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if the @arg{widget} is visible}
  @begin{short}
    Determines whether the @arg{widget} is visible.
  @end{short}
  Note that this doesn't take into account whether the widget's parent is also
  visible or the widget is obscured in any way.

  See @fun{gtk-widget-set-visible}.

  Since 2.18")

;;; --- gtk-widget-set-visible -------------------------------------------------

(setf (documentation 'gtk-widget-set-visible 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[visible]{whether the @arg{widget} should be shown or not}
  @begin{short}
    Sets the visibility state of @arg{widget}.
  @end{short}
  Note that setting this to @arg{true} doesn't mean the widget is actually
  viewable, see @fun{gtk-widget-get-visible}.

  This function simply calls @fun{gtk-widget-show} or @fun{gtk-widget-hide} but
  is nicer to use when the visibility of the widget depends on some condition.

  Since 2.18")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_state_flags ()
;;;
;;; void gtk_widget_set_state_flags (GtkWidget *widget,
;;;                                  GtkStateFlags flags,
;;;                                  gboolean clear);
;;;
;;; This function is for use in widget implementations. Turns on flag values in
;;; the current widget state (insensitive, prelighted, etc.).
;;;
;;; It is worth mentioning that any other state than GTK_STATE_FLAG_INSENSITIVE,
;;; will be propagated down to all non-internal children if widget is a
;;; GtkContainer, while GTK_STATE_FLAG_INSENSITIVE itself will be propagated
;;; down to all GtkContainer children by different means than turning on the
;;; state flag down the hierarchy, both gtk_widget_get_state_flags() and
;;; gtk_widget_is_sensitive() will make use of these.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; flags :
;;;     State flags to turn on
;;;
;;; clear :
;;;     Whether to clear state before turning on flags
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unset_state_flags ()
;;;
;;; void gtk_widget_unset_state_flags (GtkWidget *widget, GtkStateFlags flags);
;;;
;;; This function is for use in widget implementations. Turns off flag values
;;; for the current widget state (insensitive, prelighted, etc.). See
;;; gtk_widget_set_state_flags().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; flags :
;;;     State flags to turn off
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state_flags ()
;;;
;;; GtkStateFlags gtk_widget_get_state_flags (GtkWidget *widget);
;;;
;;; Returns the widget state as a flag set. It is worth mentioning that the
;;; effective GTK_STATE_FLAG_INSENSITIVE state will be returned, that is, also
;;; based on parent insensitivity, even if widget itself is sensitive.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     The state flags for widget
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; gtk-widget-has-default is implemented as accessor
;;; gtk-widget-has-focus is implemented as accessor


;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_visible_focus ()
;;;
;;; gboolean gtk_widget_has_visible_focus (GtkWidget *widget);
;;;
;;; Determines if the widget should show a visible indication that it has the
;;; global input focus. This is a convenience function for use in ::draw
;;; handlers that takes into account whether focus indication should currently
;;; be shown in the toplevel window of widget. See
;;; gtk_window_get_focus_visible() for more information about focus indication.
;;;
;;; To find out if the widget has the global input focus, use
;;; gtk_widget_has_focus().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget should display a 'focus rectangle'
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_grab ()
;;;
;;; gboolean gtk_widget_has_grab (GtkWidget *widget);
;;;
;;; Determines whether the widget is currently grabbing events, so it is the
;;; only widget receiving input events (keyboard and mouse).
;;;
;;; See also gtk_grab_add().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget is in the grab_widgets stack
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_rc_style ()
;;;
;;; gboolean gtk_widget_has_rc_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_has_rc_style has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use GtkStyleContext instead.
;;;
;;; Determines if the widget style has been looked up through the rc mechanism.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget has been looked up through the rc mechanism, FALSE
;;;     otherwise.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-is-drawable -------------------------------------------------

(setf (documentation 'gtk-widget-is-drawable 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} is drawable, @code{nil} otherwise}
  @short{Determines whether @arg{widget} can be drawn to.}
  A widget can be drawn to if it is mapped and visible.

  Since 2.18")

;;; --- gtk-widget-is-toplevel -------------------------------------------------

(setf (documentation 'gtk-widget-is-toplevel 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} is a toplevel, @code{nil} otherwise}
  @short{Determines whether @arg{widget} is a toplevel widget.}

  Currently only @class{gtk-window} and @class{gtk-invisible} (and
  out-of-process @class{gtk-plug}'s) are toplevel widgets. Toplevel widgets have
  no parent widget.

  Since 2.18")

;;; --- gtk-widget-set-window --------------------------------------------------

(setf (documentation 'gtk-widget-set-window 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[window]{a @class{gdk-window} instance}
  @begin{short}
    Sets a @arg{widget}'s @arg{window}.
  @end{short}
  This function should only be used in a widget's \"realize\" implementation.
  The window passed is usually either new window created with
  @fun{gdk-window-new}, or the window of its parent widget as returned by
  @fun{gtk-widget-get-parent-window}.

  Widgets must indicate whether they will create their own @class{gdk-window} by
  calling @fun{gtk-widget-set-has-window}. This is usually done in the widget's
  @code{init()} function.

  @begin[Note]{dictionary}
    This function does not add any reference to window.
  @end{dictionary}
  Since 2.18
  @see-function{gtk-widget-get-parent-window}
  @see-function{gtk-widget-set-has-window}")

;;; --- gtk-widget-set-receives-default ----------------------------------------

(setf (documentation 'gtk-widget-set-receives-default 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[receives-default]{whether or not @arg{widget} can be a default
    widget.}
  @begin{short}
    Specifies whether @arg{widget} will be treated as the default widget within
    its toplevel when it has the focus, even if another widget is the default.
  @end{short}

  See @fun{gtk-widget-grab-default} for details about the meaning of
  \"default\".

  Since 2.18
  @see-function{gtk-widget-grab-default}")

;;; --- gtk-widget-get-receives-default ----------------------------------------

(setf (documentation 'gtk-widget-get-receives-default 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{true} if @arg{widget} acts as default widget when focussed,
    @code{nil} otherwise}
  @begin{short}
    Determines whether @arg{widget} is alyways treated as default widget withing
    its toplevel when it has the focus, even if another widget is the default.
  @end{short}

  See @fun{gtk-widget-set-receives-default}.

  Since 2.18
  @see-function{gtk-widget-set-receives-default}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_support_multidevice ()
;;;
;;; void gtk_widget_set_support_multidevice (GtkWidget *widget,
;;;                                          gboolean support_multidevice);
;;;
;;; Enables or disables multiple pointer awareness. If this setting is TRUE,
;;; widget will start receiving multiple, per device enter/leave events. Note
;;; that if custom GdkWindows are created in "realize",
;;; gdk_window_set_support_multidevice() will have to be called manually on
;;; them.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; support_multidevice :
;;;     TRUE to support input from multiple devices.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_support_multidevice ()
;;;
;;; gboolean gtk_widget_get_support_multidevice (GtkWidget *widget);
;;;
;;; Returns TRUE if widget is multiple pointer aware. See
;;; gtk_widget_set_support_multidevice() for more information.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget is multidevice aware.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_realized ()
;;;
;;; void gtk_widget_set_realized (GtkWidget *widget, gboolean realized);
;;;
;;; Marks the widget as being realized.
;;;
;;; This function should only ever be called in a derived widget's "realize" or
;;; "unrealize" implementation.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; realized :
;;;     TRUE to mark the widget as realized
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_realized ()
;;;
;;; gboolean gtk_widget_get_realized (GtkWidget *widget);
;;;
;;; Determines whether widget is realized.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget is realized, FALSE otherwise
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_mapped ()
;;;
;;; void gtk_widget_set_mapped (GtkWidget *widget, gboolean mapped);
;;;
;;; Marks the widget as being realized.
;;;
;;; This function should only ever be called in a derived widget's "map" or
;;; "unmap" implementation.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; mapped :
;;;     TRUE to mark the widget as mapped
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_mapped ()
;;;
;;; gboolean gtk_widget_get_mapped (GtkWidget *widget);
;;;
;;; Whether the widget is mapped.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget is mapped, FALSE otherwise.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_requisition ()
;;;
;;; void gtk_widget_get_requisition (GtkWidget *widget,
;;;                                  GtkRequisition *requisition);
;;;
;;; Warning
;;;
;;; gtk_widget_get_requisition has been deprecated since version 3.0 and should
;;; not be used in newly-written code. The GtkRequisition cache on the widget
;;; was removed, If you need to cache sizes across requests and allocations, add
;;; an explicit cache to the widget in question instead.
;;;
;;; Retrieves the widget's requisition.
;;;
;;; This function should only be used by widget implementations in order to
;;; figure whether the widget's requisition has actually changed after some
;;; internal state change (so that they can call gtk_widget_queue_resize()
;;; instead of gtk_widget_queue_draw()).
;;;
;;; Normally, gtk_widget_size_request() should be used.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; requisition :
;;;     a pointer to a GtkRequisition to copy to
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_device_is_shadowed ()
;;;
;;; gboolean gtk_widget_device_is_shadowed (GtkWidget *widget,
;;;                                         GdkDevice *device);
;;;
;;; Returns TRUE if device has been shadowed by a GTK+ device grab on another
;;; widget, so it would stop sending events to widget. This may be used in the
;;; "grab-notify" signal to check for specific devices.
;;; See gtk_device_grab_add().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     TRUE if there is an ongoing grab on device by another GtkWidget than
;;;     widget.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_modifier_mask ()
;;;
;;; GdkModifierType gtk_widget_get_modifier_mask (GtkWidget *widget,
;;;                                               GdkModifierIntent intent);
;;;
;;; Returns the modifier mask the widget's windowing system backend uses for a
;;; particular purpose.
;;;
;;; See gdk_keymap_get_modifier_mask().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; intent :
;;;     the use case for the modifier mask
;;;
;;; Returns :
;;;     the modifier mask used for intent.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_insert_action_group ()
;;;
;;; void gtk_widget_insert_action_group (GtkWidget *widget,
;;;                                      const gchar *name,
;;;                                      GActionGroup *group);
;;;
;;; Inserts group into widget. Children of widget that implement GtkActionable
;;; can then be associated with actions in group by setting their 'action-name'
;;; to prefix.action-name.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; name :
;;;     the prefix for actions in group
;;;
;;; group :
;;;     a GActionGroup
;;;
;;; Since 3.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_path ()
;;;
;;; GtkWidgetPath * gtk_widget_get_path (GtkWidget *widget);
;;;
;;; Returns the GtkWidgetPath representing widget, if the widget is not
;;; connected to a toplevel widget, a partial path will be created.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     The GtkWidgetPath representing widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style_context ()
;;;
;;; GtkStyleContext * gtk_widget_get_style_context (GtkWidget *widget);
;;;
;;; Returns the style context associated to widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     a GtkStyleContext. This memory is owned by widget and must not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_style ()
;;;
;;; void gtk_widget_reset_style (GtkWidget *widget);
;;;
;;; Updates the style context of widget and all descendents by updating its
;;; widget path. GtkContainers may want to use this on a child when reordering
;;; it in a way that a different style might apply to it. See also
;;; gtk_container_get_path_for_child().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_new ()
;;;
;;; GtkRequisition * gtk_requisition_new (void);
;;;
;;; Allocates a new GtkRequisition structure and initializes its elements to
;;; zero.
;;;
;;; Returns :
;;;     a new empty GtkRequisition. The newly allocated GtkRequisition should be
;;;     freed with gtk_requisition_free().
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_copy ()
;;;
;;; GtkRequisition * gtk_requisition_copy (const GtkRequisition *requisition);
;;;
;;; Copies a GtkRequisition.
;;;
;;; requisition :
;;;     a GtkRequisition
;;;
;;; Returns :
;;;     a copy of requisition
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_free ()
;;;
;;; void gtk_requisition_free (GtkRequisition *requisition);
;;;
;;; Frees a GtkRequisition.
;;;
;;; requisition :
;;;     a GtkRequisition
;;; ----------------------------------------------------------------------------

;;; --- gtk-size-request-mode --------------------------------------------------

(setf (gethash 'gtk-size-request-mode atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-size-request-mode atdoc:*external-symbols*)
 "@version{2013-1-6}
  @begin{short}
    Specifies a preference for height-for-width or width-for-height geometry
    management.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSizeRequestMode\" gtk-size-request-mode
  (:export t
   :type-initializer \"gtk_size_request_mode_type_get_type\")
  (:height-for-width 0)
  (:width-for-height 1)
  (:constant-size 2))
  @end{pre}
  @begin{table}
    @entry[:height-for-width]{Prefer height-for-width geometry management}
    @entry[:width-for-height]{Prefer width-for-height geometry management}
    @entry[:constant-size]{Dont trade height-for-width or width-for-height}
  @end{table}")

;;; --- gtk-requested-size -----------------------------------------------------

(setf (gethash 'gtk-requested-size atdoc:*class-name-alias*) "CStruct")
(setf (documentation 'gtk-requested-size 'type)
 "@version{2013-1-6}
  @begin{short}
    Represents a request of a screen object in a given orientation.
  @end{short}
  These are primarily used in container implementations when allocating a
  natural size for children calling. See gtk_distribute_natural_allocation().
  @begin{pre}
(define-g-boxed-cstruct gtk-requested-size \"GtkRequestedSize\"
  (data :pointer)
  (minimum-size :int)
  (natural-size :int))
  @end{pre}
  @begin{table}
    @entry[gpointer data]{A client pointer}
    @entry[gint minimum_size]{The minimum size needed for allocation in a given
      orientation}
    @entry[gint natural_size]{The natural size for allocation in a given
      orientation}
  @end{table}")

;;; --- gtk-widget-get-preferred-height ----------------------------------------

(setf (documentation 'gtk-widget-get-preferred-height 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{minimum-height} -- the minimum height, or @code{nil}@br{}
          @arg{natural-height} -- the natural height, or @code{nil}}
  @short{Retrieves a @arg{widget}'s initial minimum and natural height.}

  Since 3.0
  @begin[note]{dictionary}
    This call is specific to width-for-height requests.

    The returned request will be modified by the
    @code{GtkWidgetClass::adjust_size_request} virtual method and by any
    @class{gtk-size-group}'s that have been applied. That is, the returned
    request is the one that should be used for layout, not necessarily the one
    returned by the @arg{widget} itself.
  @end{dictionary}
  @see-function{gtk-widget-get-preferred-width}
  @see-class{gtk-size-group}")

;;; --- gtk-widget-get-preferred-width -----------------------------------------

(setf (documentation 'gtk-widget-get-preferred-width 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{@arg{minimum-width} -- the minimum width, or @code{nil}@br{}
          @arg{natural-width} -- the natural width, or @code{nil}}
  @short{Retrieves a @arg{widget}'s initial minimum and natural width.}

  Since 3.0
  @begin[Example]{dictionary}
    @begin{pre}
 (setq widget (make-instance 'gtk-button :label \"Hello\"))
=> #<GTK-BUTTON {B1D0079@}>
 (gtk-widget-get-preferred-width widget)
=> 49
=> 49

 (setq widget (make-instance 'gtk-button :label \"Hello, more text\"))
=> #<GTK-BUTTON {B1D60E9@}>
 (gtk-widget-get-preferred-width widget)
=> 123
=> 123
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    This call is specific to height-for-width requests.

    The returned request will be modified by the
    @code{GtkWidgetClass::adjust_size_request} virtual method and by any
    @class{gtk-size-group}'s that have been applied. That is, the returned
    request is the one that should be used for layout, not necessarily the one
    returned by the @arg{widget} itself.
  @end{dictionary}
  @see-function{gtk-widget-get-preferred-height}
  @see-class{gtk-size-group}")

;;; --- gtk-widget-get-preferred-height-for-width ------------------------------

(setf (documentation 'gtk-widget-get-preferred-for-width 'function)
 "@version{2013-2-2}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[width]{the width which is available for allocation}
  @return{@code{minimum-height} -- the minimum height, or @code{nil}@br{}
          @code{natural-height} -- the natural height, or @code{nil}@br{}}
  @begin{short}
    Retrieves a @arg{widget}'s minimum and natural height if it would be given
    the specified @arg{width}.
  @end{short}

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk-size-group}'s that have been applied. That is, the returned request
  is the one that should be used for layout, not necessarily the one returned by
  the @arg{widget} itself.

  Since 3.0")

;;; --- gtk-widget-get-preferred-width-for-height ------------------------------

(setf (documentation 'gtk-widget-get-preferred-width-for-height 'function)
 "@version{2013-2-2}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[height]{the height which is available for allocation}
  @return{@code{minimum-width} -- the minimum width, or @code{nil}@br{}
          @code{natural-width} -- the natural width, or @code{nil}@br{}}
  @begin{short}
    Retrieves a @arg{widget}'s minimum and natural width if it would be given
    the specified @arg{height}.
  @end{short}

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk-size-group}'s that have been applied. That is, the returned request
  is the one that should be used for layout, not necessarily the one returned by
  the @arg{widget} itself.

  Since 3.0")

;;; --- gtk-widget-get-request-mode --------------------------------------------

(setf (documentation 'gtk-widget-get-request-mode 'function)
 "@version{2013-1-6}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The @symbol{gtk-size-request-mode} preferred by widget.}
  @begin{short}
    Gets whether the @arg{widget} prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @begin[Note]{dictionary}
    @class{gtk-bin} widgets generally propagate the preference of their child,
    container widgets need to request something either in context of their
    children or in context of their allocation capabilities.
  @end{dictionary}

  Since 3.0")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_size ()
;;;
;;; void gtk_widget_get_preferred_size (GtkWidget *widget,
;;;                                     GtkRequisition *minimum_size,
;;;                                     GtkRequisition *natural_size);
;;;
;;; Retrieves the minimum and natural size of a widget, taking into account the
;;; widget's preference for height-for-width management.
;;;
;;; This is used to retrieve a suitable size by container widgets which do not
;;; impose any restrictions on the child placement. It can be used to deduce
;;; toplevel window and menu sizes as well as child widgets in free-form
;;; containers such as GtkLayout.
;;;
;;; Note
;;;
;;; Handle with care. Note that the natural height of a height-for-width widget
;;; will generally be a smaller size than the minimum height, since the required
;;; height for the natural width is generally smaller than the required height
;;; for the minimum width.
;;;
;;; widget :
;;;     a GtkWidget instance
;;;
;;; minimum_size :
;;;     location for storing the minimum size, or NULL
;;;
;;; natural_size :
;;;     location for storing the natural size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_distribute_natural_allocation ()
;;;
;;; gint gtk_distribute_natural_allocation (gint extra_space,
;;;                                         guint n_requested_sizes,
;;;                                         GtkRequestedSize *sizes);
;;;
;;; Distributes extra_space to child sizes by bringing smaller children up to
;;; natural size first.
;;;
;;; The remaining space will be added to the minimum_size member of the
;;; GtkRequestedSize struct. If all sizes reach their natural size then the
;;; remaining space is returned.
;;;
;;; extra_space :
;;;     Extra space to redistribute among children after subtracting minimum
;;;     sizes and any child padding from the overall allocation
;;;
;;; n_requested_sizes :
;;;     Number of requests to fit into the allocation
;;;
;;; sizes :
;;;     An array of structs with a client pointer and a minimum/natural size in
;;;     the orientation of the allocation.
;;;
;;; Returns :
;;;     The remainder of extra_space after redistributing space to sizes.
;;; ----------------------------------------------------------------------------

;;; --- gtk-align --------------------------------------------------------------

(setf (gethash 'gtk-align atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-align atdoc:*external-symbols*)
 "@version{2012-12-29}
  @begin{short}
    Controls how a widget deals with extra space in a single (x or y) dimension.
  @end{short}

  Alignment only matters if the widget receives a \"too large\" allocation, for
  example if you packed the widget with the \"expand\" flag inside a
  @class{gtk-box}, then the widget might get extra space. If you have for
  example a 16x16 icon inside a 32x32 space, the icon could be scaled and
  stretched, it could be centered, or it could be positioned to one side of the
  space.

  Note that in horizontal context @code{:start} and @code{:align} are
  interpreted relative to text direction.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GtkAlign\" gtk-align
  (:export t
   :type-initializer \"gtk_align_get_type\")
  (:fill 0)
  (:start 1)
  (:end 2)
  (:center 3))
    @end{pre}
  @end{dictionary}
  @begin{table}
    @entry[:fill]{stretch to fill all space if possible, center if no meaningful
      way to stretch}
    @entry[:start]{snap to left or top side, leaving space on right or bottom}
    @entry[:end]{snap to right or bottom side, leaving space on left or top}
    @entry[:center]{center natural width of widget inside the allocation}
  @end{table}")

;;; --- gtk-widget-get-halign --------------------------------------------------

(setf (documentation 'gtk-widget-get-halign 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{the horizontal alignment of widget}
  @short{Gets the value of the \"halign\" property.}")

;;; --- gtk-widget-set-halign --------------------------------------------------

(setf (documentation 'gtk-widget-set-halign 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[align]{the horizontal alignment}
  @begin{short}
    Sets the horizontal alignment of widget. See the \"halign\" property.
  @end{short}")

;;; --- gtk-widget-get-valign --------------------------------------------------

(setf (documentation 'gtk-widget-get-valign 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{the vertical alignment of widget}
  @begin{short}
    Gets the value of the \"valign\" property.
  @end{short}")

;;; --- gtk-widget-set-valign --------------------------------------------------

(setf (documentation 'gtk-widget-set-valign 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[align]{the vertical alignment}
  @begin{short}
    Sets the vertical alignment of widget. See the \"valign\" property.
  @end{short}")

;;; --- gtk-widget-get-margin-left ---------------------------------------------

(setf (documentation 'gtk-widget-get-margin-left 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The left margin of widget}
  @short{Gets the value of the \"margin-left\" property.}

  Since 3.0")

;;; --- gtk-widget-set-margin-left ---------------------------------------------

(setf (documentation 'gtk-widget-set-margin-left 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[margin]{the left margin}
  @short{Sets the left margin of widget. See the \"margin-left\" property.}

  Since 3.0")

;;; --- gtk-widget-get-margin-right --------------------------------------------

(setf (documentation 'gtk-widget-get-margin-right 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The right margin of widget}
  @short{Gets the value of the \"margin-right\" property.}

  Since 3.0")

;;; --- gtk-widget-set-margin-right --------------------------------------------

(setf (documentation 'gtk-widget-set-margin-right 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[margin]{the right margin}
  @short{Sets the right margin of widget. See the \"margin-right\" property.}

  Since 3.0")

;;; --- gtk-widget-get-margin-top ----------------------------------------------

(setf (documentation 'gtk-widget-get-margin-top 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The top margin of widget}
  @short{Gets the value of the \"margin-top\" property.}

  Since 3.0")

;;; --- gtk-widget-set-margin-top ----------------------------------------------

(setf (documentation 'gtk-widget-set-margin-top 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[margin]{the top margin}
  @short{Sets the top margin of widget. See the \"margin-top\" property.}

  Since 3.0")

;;; --- gtk-widget-get-margin-bottom -------------------------------------------

(setf (documentation 'gtk-widget-get-margin-bottom 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{The bottom margin of widget}
  @short{Gets the value of the \"margin-bottom\" property.}

  Since 3.0")

;;; --- gtk-widget-set-margin-bottom -------------------------------------------

(setf (documentation 'gtk-widget-set-margin-bottom 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[margin]{the bottom margin}
  @short{Sets the bottom margin of widget. See the \"margin-bottom\" property.}

  Since 3.0")

;;; --- gtk-widget-get-hexpand -------------------------------------------------

(setf (documentation 'gtk-widget-get-hexpand 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{whether hexpand flag is set}
  @begin{short}
    Gets whether the widget would like any available extra horizontal space.
  @end{short}
  When a user resizes a GtkWindow, widgets with expand=TRUE generally receive
  the extra space. For example, a list or scrollable area or document in your
  window would often be set to expand.

  Containers should use gtk_widget_compute_expand() rather than this function,
  to see whether a widget, or any of its children, has the expand flag set. If
  any child of a widget wants to expand, the parent may ask to expand also.

  This function only looks at the widget's own hexpand flag, rather than
  computing whether the entire widget tree rooted at this widget wants to
  expand.")

;;; --- gtk-widget-set-hexpand -------------------------------------------------

(setf (documentation 'gtk-widget-set-hexpand 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[expand]{whether to expand}
  @begin{short}
    Sets whether the widget would like any available extra horizontal space.
  @end{short}
  When a user resizes a GtkWindow, widgets with expand=TRUE generally receive
  the extra space. For example, a list or scrollable area or document in your
  window would often be set to expand.

  Call this function to set the expand flag if you would like your widget to
  become larger horizontally when the window has extra room.

  By default, widgets automatically expand if any of their children want to
  expand. (To see if a widget will automatically expand given its current
  children and state, call gtk_widget_compute_expand(). A container can decide
  how the expandability of children affects the expansion of the container by
  overriding the compute_expand virtual method on GtkWidget.).

  Setting hexpand explicitly with this function will override the automatic
  expand behavior.

  This function forces the widget to expand or not to expand, regardless of
  children. The override occurs because gtk_widget_set_hexpand() sets the
  hexpand-set property (see gtk_widget_set_hexpand_set()) which causes the
  widget's hexpand value to be used, rather than looking at children and
  widget state.")

;;; --- gtk-widget-get-hexpand-set ---------------------------------------------

(setf (documentation 'gtk-widget-get-hexpand-set 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{whether hexpand has been explicitly set}
  @begin{short}
    Gets whether gtk_widget_set_hexpand() has been used to explicitly set the
    expand flag on this widget.
  @end{short}

  If hexpand is set, then it overrides any computed expand value based on
  child widgets. If hexpand is not set, then the expand value depends on
  whether any children of the widget would like to expand.

  There are few reasons to use this function, but it's here for completeness
  and consistency.")

;;; --- gtk-widget-set-hexpand-set ---------------------------------------------

(setf (documentation 'gtk-widget-set-hexpand-set 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[set]{value for hexpand-set property}
  @begin{short}
    Sets whether the hexpand flag (see gtk_widget_get_hexpand()) will be used.
  @end{short}

  The hexpand-set property will be set automatically when you call
  gtk_widget_set_hexpand() to set hexpand, so the most likely reason to use
  this function would be to unset an explicit expand flag.

  If hexpand is set, then it overrides any computed expand value based on
  child widgets. If hexpand is not set, then the expand value depends on
  whether any children of the widget would like to expand.

  There are few reasons to use this function, but it's here for completeness
  and consistency.")

;;; --- gtk-widget-get-vexpand -------------------------------------------------

(setf (documentation 'gtk-widget-get-vexpand 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{whether vexpand flag is set}
  @begin{short}
    Gets whether the widget would like any available extra vertical space.
  @end{short}
  See gtk_widget_get_hexpand() for more detail.")

;;; --- gtk-widget-set-vexpand -------------------------------------------------

(setf (documentation 'gtk-widget-set-vexpand 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[expand]{whether to expand}
  @begin{short}
    Sets whether the widget would like any available extra vertical space.
  @end{short}
  See gtk_widget_set_hexpand() for more detail.")

;;; --- gtk-widget-get-vexpand-set ---------------------------------------------

(setf (documentation 'gtk-widget-get-vexpand-set 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @return{whether vexpand has been explicitly set}
  @begin{short}
    Gets whether gtk_widget_set_vexpand() has been used to explicitly set the
    expand flag on this widget.
  @end{short}

  See gtk_widget_get_hexpand_set() for more detail.")

;;; --- gtk-widget-set-vexpand-set ---------------------------------------------

(setf (documentation 'gtk-widget-set-vexpand-set 'function)
 "@version{2013-1-5}
  @argument[widget]{a @class{gtk-widget} instance}
  @argument[set]{value for vexpand-set property}
  @begin{short}
    Sets whether the vexpand flag (see gtk_widget_get_vexpand()) will be used.
  @end{short}

  See gtk_widget_set_hexpand_set() for more detail.")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_compute_expand ()
;;;
;;; void gtk_widget_queue_compute_expand (GtkWidget *widget);
;;;
;;; Mark widget as needing to recompute its expand flags. Call this function
;;; when setting legacy expand child properties on the child of a container.
;;;
;;; See gtk_widget_compute_expand().
;;;
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_expand ()
;;;
;;; gboolean gtk_widget_compute_expand (GtkWidget *widget,
;;;                                     GtkOrientation orientation);
;;;
;;; Computes whether a container should give this widget extra space when
;;; possible. Containers should check this, rather than looking at
;;; gtk_widget_get_hexpand() or gtk_widget_get_vexpand().
;;;
;;; This function already checks whether the widget is visible, so visibility
;;; does not need to be checked separately. Non-visible widgets are not
;;; expanded.
;;;
;;; The computed expand value uses either the expand setting explicitly set on
;;; the widget itself, or, if none has been explicitly set, the widget may
;;; expand if some of its children do.
;;;
;;; widget :
;;;     the widget
;;;
;;; orientation :
;;;     expand direction
;;;
;;; Returns :
;;;     whether widget tree rooted here should be expanded
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-widget-app-paintable -----------------------------------------------

(setf (gethash 'gtk-widget-app-paintable atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-app-paintable 'function)
 "@version{2012-12-29}
  @begin{short}
    Accessor of the slot \"app-paintable\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-app-paintable} for more information.
  @see-function{gtk-widget-set-app-paintable}")

;;; --- gtk-widget-can-default -------------------------------------------------

(setf (gethash 'gtk-widget-can-default atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-can-default 'function)
 "@version{2012-12-29}
  @begin{short}
    Accessor of the slot \"can-default\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-can-default} for more information.
  @see-function{gtk-widget-set-can-default}")

;;; --- gtk-widget-can-focus ---------------------------------------------------

(setf (gethash 'gtk-widget-can-focus atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-can-focus 'function)
 "@version{2012-12-29}
  @begin{short}
    Accessor of the slot \"can-focus\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-can-focus} for more information.
  @see-function{gtk-widget-set-can-focus}")

;;; --- gtk-widget-composite-child ---------------------------------------------

(setf (gethash 'gtk-widget-composite-child atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-composite-child 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"composite-child\" of the @class{gtk-widget} class.
  @end{short}

  See the slot description for \"composite-child\" for more information.")

;;; --- gtk-widget-double-buffered ---------------------------------------------

(setf (gethash 'gtk-widget-double-buffered atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-double-buffered 'function)
 "@version{2012-12-29}
  @begin{short}
    Accessor of the slot \"double-buffered\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-double-buffered} for details.
  @see-function{gtk-widget-set-double-buffered}")

;;; --- gtk-widget-events ------------------------------------------------------

(setf (gethash 'gtk-widget-events atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-events 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"events\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-events} for details.
  @see-function{gtk-widget-set-events}")

;;; --- gtk-widget-expand ------------------------------------------------------

(setf (gethash 'gtk-widget-expand atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-expand 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"expand\" of the @class{gtk-widget} class.
  @end{short}

  See the slot description for \"expand\" for more information.")

;;; --- gtk-widget-halign ------------------------------------------------------

(setf (gethash 'gtk-widget-halign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-halign 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"halign\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-get-halign} for details.
  @see-function{gtk-widget-set-halign}")

;;; --- gtk-widget-has-default -------------------------------------------------

(setf (gethash 'gtk-widget-has-default atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-has-default 'function)
 "@version{2013-1-5}
  @argument[widget]{a GtkWidget}
  @return{TRUE if widget is the current default widget within its toplevel,
    FALSE otherwise}
  @begin{short}
    Determines whether widget is the current default widget within its toplevel.
  @end{short}
  See @fun{gtk-widget-set-can-default}.

  Since 2.18
  @see-function{gtk-widget-set-can-default}")

;;; --- gtk-widget-has-focus ---------------------------------------------------

(setf (gethash 'gtk-widget-has-focus atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-has-focus 'function)
 "@version{2013-1-5}
  @argument[widget]{a GtkWidget}
  @return{TRUE if the widget has the global input focus.}
  @begin{short}
    Determines if the widget has the global input focus.
  @end{short}
  See gtk_widget_is_focus() for the difference between having the global input
  focus, and only having the focus within a toplevel.

  Since 2.18")

;;; --- gtk-widget-has-tooltip -------------------------------------------------

(setf (gethash 'gtk-widget-has-tooltip atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-has-tooltip 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"has-tooltip\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-has-tooltip} for details.
  @see-function{gtk-widget-set-has-tooltip}")

;;; --- gtk-widget-height-request ----------------------------------------------

(setf (gethash 'gtk-widget-height-request atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-height-request 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"height-request\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-size-request} for details.
  @see-function{gtk-widget-set-size-request}")

;;; --- gtk-widget-hexpand -----------------------------------------------------

(setf (gethash 'gtk-widget-hexpand atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-hexpand 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"hexpand\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-widget-hexpand-set -------------------------------------------------

(setf (gethash 'gtk-widget-hexpand-set atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-hexpand-set 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"hexpand-set\" of the @class{gtk-widget} class.
  @end{short}
  @see-function{gtk-widget-get-hexpand-set}
  @see-function{gtk-widget-set-hexpand-set}")

;;; --- gtk-widget-is-focus ----------------------------------------------------

(setf (gethash 'gtk-widget-is-focus atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-is-focus 'function)
 "@argument[widget]{a @class{gtk-widget} instance}
  @return{true if the @arg{widget} is the focus widget}
  @begin{short}
    Determines if the @arg{widget} is the focus widget within its toplevel.
  @end{short}
  (This does not mean that the @code{HAS_FOCUS} flag is necessarily set;
  @code{HAS_FOCUS} will only be set if the toplevel widget additionally has the
  global input focus.)")

;;; --- gtk-widget-margin ------------------------------------------------------

(setf (gethash 'gtk-widget-margin atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-margin 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"margin\" of the @class{gtk-widget} class.
  @end{short}")

;;; --- gtk-widget-margin-left -------------------------------------------------

(setf (gethash 'gtk-widget-margin-left atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-margin-left 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"margin-left\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-margin-left} for details.
  @see-function{gtk-widget-set-margin-left}")

;;; --- gtk-widget-margin-right -------------------------------------------------

(setf (gethash 'gtk-widget-margin-right atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-margin-right 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"margin-right\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-margin-right} for details.
  @see-function{gtk-widget-set-margin-right}")

;;; --- gtk-widget-margin-top -------------------------------------------------

(setf (gethash 'gtk-widget-margin-top atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-margin-top 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"margin-top\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-margin-top} for details.
  @see-function{gtk-widget-set-margin-top}")

;;; --- gtk-widget-margin-bottom -----------------------------------------------

(setf (gethash 'gtk-widget-margin-bottom atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-margin-bottom 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"margin-bottom\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-margin-bottom} for details.
  @see-function{gtk-widget-set-margin-bottom}")

;;; --- gtk-widget-name --------------------------------------------------------

(setf (gethash 'gtk-widget-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-name 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"name\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-name} for details.
  @see-function{gtk-widget-set-name}")

;;; --- gtk-widget-no-show-all --------------------------------------------------------

(setf (gethash 'gtk-widget-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-name 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"no-show-all\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-no-show-all} for details.
  @see-function{gtk-widget-set-no-show-all}")

;;; --- gtk-widget-parent ------------------------------------------------------

(setf (gethash 'gtk-widget-parent atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-parent 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"parent\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-parent} for details.
  @see-function{gtk-widget-set-parent}")

;;; --- gtk-widget-receives-default --------------------------------------------

(setf (gethash 'gtk-widget-receives-default atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-receives-default 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"receives-default\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-receives-default} for details.
  @see-function{gtk-widget-set-receives-default}")

;;; --- gtk-widget-sensitive ---------------------------------------------------

(setf (gethash 'gtk-widget-sensitive atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-sensitive 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"sensitive\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-sensitive} for details.
  @see-function{gtk-widget-set-sensitive}")

;;; --- gtk-widget-style -------------------------------------------------------

(setf (gethash 'gtk-widget-style atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-style 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"style\" of the @class{gtk-widget} class.
  @end{short}

  @b{Warning}

  gtk_widget_get_style has been deprecated since version 3.0 and should not
  be used in newly-written code. Use GtkStyleContext instead.")

;;; --- gtk-widget-tooltip-markup ----------------------------------------------

(setf (gethash 'gtk-widget-tooltip-markup atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-tooltip-markup 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"tooltip-markup\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-tooltip-markup} for details.
  @see-function{gtk-widget-set-tooltip-markup}")

;;; --- gtk-widget-tooltip-text ------------------------------------------------

(setf (gethash 'gtk-widget-tooltip-text atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-tooltip-text 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"tooltip-text\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-tooltip-text} for details.
  @see-function{gtk-widget-set-tooltip-text}")

;;; --- gtk-widget-valign ------------------------------------------------------

(setf (gethash 'gtk-widget-valign atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-valign 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"valign\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-valign} for details.
  @see-function{gtk-widget-set-valign}")

;;; --- gtk-widget-vexpand -----------------------------------------------------

(setf (gethash 'gtk-widget-vexpand atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-vexpand 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"vexpand\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-vexpand} for details.
  @see-function{gtk-widget-set-vexpand}")

;;; --- gtk-widget-vexpand-set -------------------------------------------------

(setf (gethash 'gtk-widget-vexpand-set atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-vexpand-set 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"vexpand-set\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-vexpand-set} for details.
  @see-function{gtk-widget-set-vexpand-set}")

;;; --- gtk-widget-visible -----------------------------------------------------

(setf (gethash 'gtk-widget-visible atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-visible 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"visible\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-visible} for details.
  @see-function{gtk-widget-set-visible}")

;;; --- gtk-widget-width-request ----------------------------------------------

(setf (gethash 'gtk-widget-width-request atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-width-request 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"width-request\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-set-size-request} for details.
  @see-function{gtk-widget-set-size-request}
  @see-function{gtk-widget-get-size-request}")

;;; --- gtk-widget-window ---------------------------------------------------

(setf (gethash 'gtk-widget-window atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-window 'function)
 "@version{2013-1-5}
  @begin{short}
    Accessor of the slot \"window\" of the @class{gtk-widget} class.
  @end{short}

  See the function @fun{gtk-widget-get-window} for details.
  @see-function{gtk-widget-get-window}")

;;; --- End of file atdoc-gtk.widget.lisp --------------------------------------

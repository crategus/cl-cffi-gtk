;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.package.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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

(in-package :gtk)

(setf (documentation (find-package :gtk) t)
 "GTK+ is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK+ is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK+ has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  
  This is the documention of a Lisp binding to GTK+.
  @begin[Main loop and Events]{section}
    Library initialization, main event loop, and events.

    Before using GTK+, you need to initialize it; initialization connects to the
    window system display, and parses some standard command line arguments. The
    gtk_init() macro initializes GTK+. gtk_init() exits the application if
    errors occur; to avoid this, use gtk_init_check(). gtk_init_check() allows
    you to recover from a failed GTK+ initialization - you might start up your
    application in text mode instead.

    Like all GUI toolkits, GTK+ uses an event-driven programming model. When the
    user is doing nothing, GTK+ sits in the main loop and waits for input. If
    the user performs some action - say, a mouse click - then the main loop
    \"wakes up\" and delivers an event to GTK+. GTK+ forwards the event to one
    or more widgets.

    When widgets receive an event, they frequently emit one or more signals.
    Signals notify your program that \"something interesting happened\" by
    invoking functions you've connected to the signal with g_signal_connect().
    Functions connected to a signal are often termed callbacks.

    When your callbacks are invoked, you would typically take some action - for
    example, when an Open button is clicked you might display a
    GtkFileChooserDialog. After a callback finishes, GTK+ will return to the
    main loop and await more user input.

    Example 6. Typical main() function for a GTK+ application
    @begin{pre}
 int
 main (int argc, char **argv)
 {
   /* Initialize i18n support */
   gtk_set_locale ();

   /* Initialize the widget set */
   gtk_init (&argc, &argv);

   /* Create the main window */
   mainwin = gtk_window_new (GTK_WINDOW_TOPLEVEL);

   /* Set up our GUI elements */
   ...

   /* Show the application window */
   gtk_widget_show_all (mainwin);

   /* Enter the main event loop, and wait for user interaction */
   gtk_main ();

   /* The user lost interest */
  return 0;
 @}
    @end{pre}
    It's OK to use the GLib main loop directly instead of gtk_main(), though it
    involves slightly more typing. See GMainLoop in the GLib documentation.

    @about-function{gtk-disable-setlocale}
    @about-function{gtk-get-default-language}
    @about-function{gtk-parse-args}
    @about-function{gtk-init}
    @about-function{gtk-init-check}
    @about-function{gtk-init-with-args}
    @about-function{gtk-get-option-group}
    @about-function{gtk-events-pending}
    @about-function{gtk-main}
    @about-function{gtk-main-level}
    @about-function{gtk-main-quit}
    @about-function{gtk-main-iteration}
    @about-function{gtk-main-iteration-do}
    @about-function{gtk-main-do-event}
    @about-function{gtk-true}
    @about-function{gtk-false}
    @about-function{gtk-grab-add}
    @about-function{gtk-grab-get-current}
    @about-function{gtk-grab-remove}
    @about-function{gtk-device-grab-add}
    @about-function{gtk-device-grab-remove}
    @about-function{gtk-priority-resize}
    @about-function{gtk-key-snooper-install}
    @about-function{gtk-key-snooper-remove}
    @about-function{gtk-get-current-event}
    @about-function{gtk-get-current-event-time}
    @about-function{gtk-get-current-event-state}
    @about-function{gtk-get-current-event-device}
    @about-function{gtk-get-event-widget}
    @about-function{gtk-propagate-event}
  @end{section}
  @begin[GtkDialog]{section}
    Create popup windows

    Dialog boxes are a convenient way to prompt the user for a small amount of
    input, e. g. to display a message, ask a question, or anything else that
    does not require extensive effort on the user's part.

    GTK+ treats a dialog as a window split vertically. The top section is a
    @class{gtk-vbox}, and is where widgets such as a @class{gtk-label} or a
    @class{gtk-entry} should be packed. The bottom area is known as the
    @code{action_area}. This is generally used for packing buttons into the
    dialog which may perform functions such as @code{cancel}, @code{ok}, or
    @code{apply}.

    @class{gtk-dialog} boxes are created with a call to @fun{gtk-dialog-new} or
    @code{gtk_dialog_new_with_buttons()}. @code{gtk_dialog_new_with_buttons()}
    is recommended; it allows you to set the dialog title, some convenient
    flags, and add simple buttons.

    If 'dialog' is a newly created dialog, the two primary areas of the window
    can be accessed through @fun{gtk-dialog-get-content-area} and
    @fun{gtk-dialog-get-action-area}, as can be seen from the example below.

    A 'modal' dialog (that is, one which freezes the rest of the application
    from user input), can be created by calling @fun{gtk-window-set-modal} on
    the dialog. Use the @code{GTK_WINDOW()} macro to cast the widget returned
    from @fun{gtk-dialog-new} into a @class{gtk-window}. When using
    @code{gtk_dialog_new_with_buttons()} you can also pass the
    @code{GTK_DIALOG_MODAL} flag to make a dialog modal.

    If you add buttons to @class{gtk-dialog} using
    @code{gtk_dialog_new_with_buttons()}, @fun{gtk-dialog-add-button},
    @fun{gtk-dialog-add-buttons}, or @fun{gtk-dialog-add-action-widget},
    clicking the button will emit a signal called \"response\" with a response
    ID that you specified. GTK+ will never assign a meaning to positive response
    IDs; these are entirely user-defined. But for convenience, you can use the
    response IDs in the @symbol{gtk-response-type} enumeration (these all have
    values less than zero). If a dialog receives a delete event, the
    \"response\" signal will be emitted with a response ID of
    @code{:DELETE_EVENT}.

    If you want to block waiting for a dialog to return before returning
    control flow to your code, you can call @fun{gtk-dialog-run}. This function
    enters a recursive main loop and waits for the user to respond to the
    dialog, returning the response ID corresponding to the button the user
    clicked.

    For the simple dialog in the following example, in reality you'd probably
    use @class{gtk-message-dialog} to save yourself some effort. But you'd need
    to create the dialog contents manually if you had more than a simple message
    in the dialog.

    Example 44. Simple GtkDialog usage
    @begin{pre}
 /* Function to open a dialog box displaying the message provided. */
 void
 quick_message (gchar *message)
 {
    GtkWidget *dialog, *label, *content_area;

    /* Create the widgets */
    dialog = gtk_dialog_new_with_buttons (\"Message\",
                                          main_application_window,
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_NONE,
                                          NULL);
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    label = gtk_label_new (message);

    /* Ensure that the dialog box is destroyed when the user responds */
    g_signal_connect_swapped (dialog,
                              \"response\",
                              G_CALLBACK (gtk_widget_destroy),
                              dialog);

    /* Add the label, and show everything we've added to the dialog */

    gtk_container_add (GTK_CONTAINER (content_area), label);
    gtk_widget_show_all (dialog);
 @}
    @end{pre}

    @b{GtkDialog as GtkBuildable}

    The @class{gtk-dialog} implementation of the @class{gtk-buildable} interface
    exposes the @code{vbox} and @code{action_area} as internal children with
    the names \"vbox\" and \"action_area\".

    @class{gtk-dialog} supports a custom @code{<action-widgets>} element, which
    can contain multiple @code{<action-widget>} elements. The \"response\"
    attribute specifies a numeric response, and the content of the element is
    the id of widget (which should be a child of the dialogs
    @code{action_area}).

    Example 45. A @class{gtk-dialog} UI definition fragment.
    @begin{pre}
 <object class=\"GtkDialog\" id=\"dialog1\">
   <child internal-child=\"vbox\">
     <object class=\"GtkVBox\" id=\"vbox\">
       <child internal-child=\"action_area\">
         <object class=\"GtkHButtonBox\" id=\"button_box\">
           <child>
            <object class=\"GtkButton\" id=\"button_cancel\"/>
           </child>
           <child>
             <object class=\"GtkButton\" id=\"button_ok\"/>
           </child>
         </object>
       </child>
     </object>
   </child>
   <action-widgets>
     <action-widget response=\"3\">button_ok</action-widget>
     <action-widget response=\"-5\">button_cancel</action-widget>
   </action-widgets>
 </object>
    @end{pre}
    @about-class{gtk-dialog}
    @about-symbol{gtk-dialog-flags}
    @about-symbol{gtk-response-type}
    @about-function{gtk-dialog-new}
    @about-function{gtk-dialog-new-with-buttons}
    @about-function{gtk-dialog-run}
    @about-function{gtk-dialog-response}
    @about-function{gtk-dialog-add-button}
    @about-function{gtk-dialog-add-buttons}
    @about-function{gtk-dialog-add-action-widget}
    @about-function{gtk-dialog-set-default-response}
    @about-function{gtk-dialog-set-response-sensitive}
    @about-function{gtk-dialog-get-response-for-widget}
    @about-function{gtk-dialog-get-widget-for-response}
    @about-function{gtk-dialog-get-action-area}
    @about-function{gtk-dialog-get-content-area}
    @about-function{gtk-alternative-dialog-button-order}
    @about-function{gtk-dialog-set-alternative-button-order}
    @about-function{gtk-dialog-set-alternative-button-order-from-array}
  @end{section}
  @begin[GtkAboutDialog]{section}
    Display information about an application.

    The GtkAboutDialog offers a simple way to display information about a
    program like its logo, name, copyright, website and license. It is also
    possible to give credits to the authors, documenters, translators and
    artists who have worked on the program. An about dialog is typically opened
    when the user selects the About option from the Help menu. All parts of the
    dialog are optional.

    About dialog often contain links and email addresses. GtkAboutDialog
    displays these as clickable links. By default, it calls gtk_show_uri() when
    a user clicks one. The behaviour can be overridden with the
    \"activate-link\" signal.

    To make constructing a GtkAboutDialog as convenient as possible, you can
    use the function gtk_show_about_dialog() which constructs and shows a dialog
    and keeps it around so that it can be shown again.

    Note that GTK+ sets a default title of _(\"About %s\") on the dialog window
    (where %s is replaced by the name of the application, but in order to ensure
    proper translation of the title, applications should set the title property
    explicitly when constructing a GtkAboutDialog, as shown in the following
    example:
    @begin{pre}
 gtk_show_about_dialog (NULL,
                        \"program-name\", \"ExampleCode\",
                        \"logo\", example_logo,
                        \"title\" _(\"About ExampleCode\"),
                        NULL);
    @end{pre}
    It is also possible to show a GtkAboutDialog like any other GtkDialog, e. g.
    using gtk_dialog_run(). In this case, you might need to know that the
    'Close' button returns the GTK_RESPONSE_CANCEL response id.

    @about-class{gtk-about-dialog}
    @about-symbol{gtk-license}
    @about-function{gtk-about-dialog-new}
    @about-function{gtk-about-dialog-get-program-name}
    @about-function{gtk-about-dialog-set-program-name}
    @about-function{gtk-about-dialog-get-version}
    @about-function{gtk-about-dialog-set-version}
    @about-function{gtk-about-dialog-get-copyright}
    @about-function{gtk-about-dialog-set-copyright}
    @about-function{gtk-about-dialog-get-comments}
    @about-function{gtk-about-dialog-set-comments}
    @about-function{gtk-about-dialog-get-license}
    @about-function{gtk-about-dialog-set-license}
    @about-function{gtk-about-dialog-get-wrap-license}
    @about-function{gtk-about-dialog-set-wrap-license}
    @about-function{gtk-about-dialog-get-license-type}
    @about-function{gtk-about-dialog-set-license-type}
    @about-function{gtk-about-dialog-get-website}
    @about-function{gtk-about-dialog-set-website}
    @about-function{gtk-about-dialog-get-website-label}
    @about-function{gtk-about-dialog-set-website-label}
    @about-function{gtk-about-dialog-get-authors}
    @about-function{gtk-about-dialog-set-authors}
    @about-function{gtk-about-dialog-get-artists}
    @about-function{gtk-about-dialog-set-artists}
    @about-function{gtk-about-dialog-get-documenters}
    @about-function{gtk-about-dialog-set-documenters}
    @about-function{gtk-about-dialog-get-translator-credits}
    @about-function{gtk-about-dialog-set-translator-credits}
    @about-function{gtk-about-dialog-get-logo}
    @about-function{gtk-about-dialog-set-logo}
    @about-function{gtk-about-dialog-get-logo-icon-name}
    @about-function{gtk-about-dialog-set-logo-icon-name}
    @about-function{gtk-about-dialog-add-credit-section}
    @about-function{gtk-show-about-dialog}
  @end{section}
  @begin[GtkWidget]{section}
    Base class for all widgets

    GtkWidget is the base class all widgets in GTK+ derive from. It manages the
    widget lifecycle, states and style.

    @b{Height-for-width Geometry Management}

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
    sizes it is generally done in two initial passes in the GtkSizeRequestMode
    chosen by the toplevel.

    For example, when queried in the normal GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH
    mode: First, the default minimum and natural width for each widget in the
    interface will be computed using gtk_widget_get_preferred_width(). Because
    the preferred widths for each container depend on the preferred widths of
    their children, this information propagates up the hierarchy, and finally a
    minimum and natural width is determined for the entire toplevel. Next, the
    toplevel will use the minimum width to query for the minimum height
    contextual to that width using gtk_widget_get_preferred_height_for_width(),
    which will also be a highly recursive operation. The minimum height for the
    minimum width is normally used to set the minimum size constraint on the
    toplevel (unless gtk_window_set_geometry_hints() is explicitly used
    instead).

    After the toplevel window has initially requested its size in both
    dimensions it can go on to allocate itself a reasonable size (or a size
    previously specified with gtk_window_set_default_size()). During the
    recursive allocation process it's important to note that request cycles will
    be recursively executed while container widgets allocate their children.
    Each container widget, once allocated a size, will go on to first share the
    space in one orientation among its children and then request each child's
    height for its target allocated width or its width for allocated height,
    depending. In this way a GtkWidget will typically be requested its size a
    number of times before actually being allocated a size. The size a widget
    is finally allocated can of course differ from the size it has requested.
    For this reason, GtkWidget caches a small number of results to avoid
    re-querying for the same sizes in one allocation cycle.

    See GtkContainer's geometry management section to learn more about how
    height-for-width allocations are performed by container widgets.

    If a widget does move content around to intelligently use up the allocated
    size then it must support the request in both GtkSizeRequestModes even if
    the widget in question only trades sizes in a single orientation.

    For instance, a GtkLabel that does height-for-width word wrapping will not
    expect to have GtkWidgetClass.get_preferred_height() called because that
    call is specific to a width-for-height request. In this case the label must
    return the height required for its own minimum possible width. By following
    this rule any widget that handles height-for-width or width-for-height
    requests will always be allocated at least enough space to fit its own
    content.

    Here are some examples of how a GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH widget
    generally deals with width-for-height requests, for
    GtkWidgetClass.get_preferred_height() it will do:
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
    And in GtkWidgetClass.get_preferred_width_for_height() it will simply return
    the minimum and natural width:
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
    gtk_widget_get_preferred_width() inside your own size request
    implementation. These return a request adjusted by GtkSizeGroup and by the
    GtkWidgetClass.adjust_size_request() virtual method. If a widget used the
    wrappers inside its virtual method implementations, then the adjustments
    (such as widget margins) would be applied twice. GTK+ therefore does not
    allow this and will warn if you try to do it.

    Of course if you are getting the size request for another widget, such as a
    child of a container, you must use the wrapper APIs. Otherwise, you would
    not properly consider widget margins, GtkSizeGroup, and so forth.

   @b{Style Properties}

   GtkWidget introduces style properties - these are basically object
   properties that are stored not on the object, but in the style object
   associated to the widget. Style properties are set in resource files. This
   mechanism is used for configuring such things as the location of the
   scrollbar arrows through the theme, giving theme authors more control over
   the look of applications without the need to write a theme engine in C.

   Use gtk_widget_class_install_style_property() to install style properties
   for a widget class, gtk_widget_class_find_style_property() or
   gtk_widget_class_list_style_properties() to get information about existing
   style properties and gtk_widget_style_get_property(), gtk_widget_style_get()
   or gtk_widget_style_get_valist() to obtain the value of a style property.

    @b{GtkWidget as GtkBuildable}

    The GtkWidget implementation of the GtkBuildable interface supports a custom
    <accelerator> element, which has attributes named key, modifiers and signal
    and allows to specify accelerators.

    Example 102. A UI definition fragment specifying an accelerator
    @begin{pre}
 <object class=\"GtkButton\">
   <accelerator key=\"q\" modifiers=\"GDK_CONTROL_MASK\" signal=\"clicked\"/>
 </object>
    @end{pre}
    In addition to accelerators, GtkWidget also support a custom <accessible>
    element, which supports actions and relations. Properties on the accessible
    implementation of an object can be set by accessing the internal child
    \"accessible\" of a GtkWidget.

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
    Finally, GtkWidget allows style information such as style classes to be
    associated with widgets, using the custom <style> element:

    Example 104. A UI definition fragment specifying an style class
    @begin{pre}
 <object class=\"GtkButton\" id=\"button1\">
   <style>
     <class name=\"my-special-button-class\"/>
     <class name=\"dark-button\"/>
   </style>
 </object>
    @end{pre}
    @about-struct{gtk-requisition}
    @about-struct{gtk-allocation}
    @about-class{gtk-widget}
    @about-class{gtk-widget-class}
    @about-symbol{gtk-widget-aux-info}
    @about-symbol{gtk-widget-help-type}
    @about-function{gtk-widget-new}
    @about-function{gtk-widget-destroy}
    @about-function{gtk-widget-in-destruction}
    @about-function{gtk-widget-destroyed}
    @about-function{gtk-widget-unparent}
    @about-function{gtk-widget-show}
    @about-function{gtk-widget-show-now}
    @about-function{gtk-widget-hide}
    @about-function{gtk-widget-show-all}
    @about-function{gtk-widget-map}
    @about-function{gtk-widget-unmap}
    @about-function{gtk-widget-realize}
    @about-function{gtk-widget-unrealize}
    @about-function{gtk-widget-draw}
    @about-function{gtk-widget-queue-draw}
    @about-function{gtk-widget-queue-resize}
    @about-function{gtk-widget-queue-resize-no-redraw}
    @about-function{gtk-widget-size-request}
    @about-function{gtk-widget-get-child-requisition}
    @about-function{gtk-widget-size-allocate}
    @about-function{gtk-widget-add-accelerator}
    @about-function{gtk-widget-remove-accelerator}
    @about-function{gtk-widget-set-accel-path}
    @about-function{gtk-widget-list-accel-closures}
    @about-function{gtk-widget-can-activate-accel}
    @about-function{gtk-widget-event}
    @about-function{gtk-widget-activate}
    @about-function{gtk-widget-reparent}
    @about-function{gtk-widget-intersect}
    @about-function{gtk-widget-is-focus}
    @about-function{gtk-widget-grab-focus}
    @about-function{gtk-widget-grab-default}
    @about-function{gtk-widget-set-name}
    @about-function{gtk-widget-get-name}
    @about-function{gtk-widget-set-state}
    @about-function{gtk-widget-set-sensitive}
    @about-function{gtk-widget-set-parent}
    @about-function{gtk-widget-set-parent-window}
    @about-function{gtk-widget-get-parent-window}
    @about-function{gtk-widget-set-events}
    @about-function{gtk-widget-get-events}
    @about-function{gtk-widget-add-events}
    @about-function{gtk-widget-set-device-events}
    @about-function{gtk-widget-get-device-events}
    @about-function{gtk-widget-add-device-events}
    @about-function{gtk-widget-set-device-enabled}
    @about-function{gtk-widget-get-device-enabled}
    @about-function{gtk-widget-get-toplevel}
    @about-function{gtk-widget-get-ancestor}
    @about-function{gtk-widget-get-visual}
    @about-function{gtk-widget-set-visual}
    @about-function{gtk-widget-get-pointer}
    @about-function{gtk-widget-is-ancestor}
    @about-function{gtk-widget-translate-coordinates}
    @about-function{gtk-widget-hide-on-delete}
    @about-function{gtk-widget-set-style}
    @about-function{gtk-widget-ensure-style}
    @about-function{gtk-widget-get-style}
    @about-function{gtk-widget-reset-rc-styles}
    @about-function{gtk-widget-get-default-style}
    @about-function{gtk-widget-set-direction}
    @about-symbol{gtk-text-direction}
    @about-function{gtk-widget-get-direction}
    @about-function{gtk-widget-set-default-direction}
    @about-function{gtk-widget-get-default-direction}
    @about-function{gtk-widget-shape-combine-region}
    @about-function{gtk-widget-input-shape-combine-region}
    @about-function{gtk-widget-path}
    @about-function{gtk-widget-class-path}
    @about-function{gtk-widget-get-composite-name}
    @about-function{gtk-widget-override-background-color}
    @about-function{gtk-widget-override-color}
    @about-function{gtk-widget-override-font}
    @about-function{gtk-widget-override-symbolic-color}
    @about-function{gtk-widget-override-cursor}
    @about-function{gtk-widget-modify-style}
    @about-function{gtk-widget-get-modifier-style}
    @about-function{gtk-widget-modify-fg}
    @about-function{gtk-widget-modify-bg}
    @about-function{gtk-widget-modify-text}
    @about-function{gtk-widget-modify-base}
    @about-function{gtk-widget-modify-font}
    @about-function{gtk-widget-modify-cursor}
    @about-function{gtk-widget-create-pango-context}
    @about-function{gtk-widget-get-pango-context}
    @about-function{gtk-widget-create-pango-layout}
    @about-function{gtk-widget-render-icon}
    @about-function{gtk-widget-render-icon-pixbuf}
    @about-function{gtk-widget-pop-composite-child}
    @about-function{gtk-widget-push-composite-child}
    @about-function{gtk-widget-queue-draw-area}
    @about-function{gtk-widget-queue-draw-region}
    @about-function{gtk-widget-set-app-paintable}
    @about-function{gtk-widget-set-double-buffered}
    @about-function{gtk-widget-set-redraw-on-allocate}
    @about-function{gtk-widget-set-composite-name}
    @about-function{gtk-widget-mnemonic-activate}
    @about-function{gtk-widget-class-install-style-property}
    @about-function{gtk-widget-class-install-style-property-parser}
    @about-function{gtk-widget-class-find-style-property}
    @about-function{gtk-widget-class-list-style-properties}
    @about-function{gtk-widget-region-intersect}
    @about-function{gtk-widget-send-expose}
    @about-function{gtk-widget-send-focus-change}
    @about-function{gtk-widget-style-get}
    @about-function{gtk-widget-style-get-property}
    @about-function{gtk-widget-style-get-valist}
    @about-function{gtk-widget-style-attach}
    @about-function{gtk-widget-class-set-accessible-type}
    @about-function{gtk-widget-class-set-accessible-role}
    @about-function{gtk-widget-get-accessible}
    @about-function{gtk-widget-child-focus}
    @about-function{gtk-widget-child-notify}
    @about-function{gtk-widget-freeze-child-notify}
    @about-function{gtk-widget-get-child-visible}
    @about-function{gtk-widget-get-parent}
    @about-function{gtk-widget-get-settings}
    @about-function{gtk-widget-get-clipboard}
    @about-function{gtk-widget-get-display}
    @about-function{gtk-widget-get-root-window}
    @about-function{gtk-widget-get-screen}
    @about-function{gtk-widget-has-screen}
    @about-function{gtk-widget-get-size-request}
    @about-function{gtk-widget-set-child-visible}
    @about-function{gtk-widget-set-size-request}
    @about-function{gtk-widget-thaw-child-notify}
    @about-function{gtk-widget-set-no-show-all}
    @about-function{gtk-widget-get-no-show-all}
    @about-function{gtk-widget-list-mnemonic-labels}
    @about-function{gtk-widget-add-mnemonic-label}
    @about-function{gtk-widget-remove-mnemonic-label}
    @about-function{gtk-widget-is-composited}
    @about-function{gtk-widget-error-bell}
    @about-function{gtk-widget-keynav-failed}
    @about-function{gtk-widget-get-tooltip-markup}
    @about-function{gtk-widget-set-tooltip-markup}
    @about-function{gtk-widget-get-tooltip-text}
    @about-function{gtk-widget-set-tooltip-text}
    @about-function{gtk-widget-get-tooltip-window}
    @about-function{gtk-widget-set-tooltip-window}
    @about-function{gtk-widget-get-has-tooltip}
    @about-function{gtk-widget-set-has-tooltip}
    @about-function{gtk-widget-trigger-tooltip-query}
    @about-function{gtk-widget-get-window}
    @about-function{gtk-cairo-should-draw-window}
    @about-function{gtk-cairo-transform-to-window}
    @about-function{gtk-widget-get-allocated-width}
    @about-function{gtk-widget-get-allocated-height}
    @about-function{gtk-widget-get-allocation}
    @about-function{gtk-widget-set-allocation}
    @about-function{gtk-widget-get-app-paintable}
    @about-function{gtk-widget-get-can-default}
    @about-function{gtk-widget-set-can-default}
    @about-function{gtk-widget-get-can-focus}
    @about-function{gtk-widget-set-can-focus}
    @about-function{gtk-widget-get-double-buffered}
    @about-function{gtk-widget-get-has-window}
    @about-function{gtk-widget-set-has-window}
    @about-function{gtk-widget-get-sensitive}
    @about-function{gtk-widget-is-sensitive}
    @about-function{gtk-widget-get-state}
    @about-function{gtk-widget-get-visible}
    @about-function{gtk-widget-set-visible}
    @about-function{gtk-widget-set-state-flags}
    @about-function{gtk-widget-unset-state-flags}
    @about-function{gtk-widget-get-state-flags}
    @about-function{gtk-widget-has-default}
    @about-function{gtk-widget-has-focus}
    @about-function{gtk-widget-has-visible-focus}
    @about-function{gtk-widget-has-grab}
    @about-function{gtk-widget-has-rc-style}
    @about-function{gtk-widget-is-drawable}
    @about-function{gtk-widget-is-toplevel}
    @about-function{gtk-widget-set-window}
    @about-function{gtk-widget-set-receives-default}
    @about-function{gtk-widget-get-receives-default}
    @about-function{gtk-widget-set-support-multidevice}
    @about-function{gtk-widget-get-support-multidevice}
    @about-function{gtk-widget-set-realized}
    @about-function{gtk-widget-get-realized}
    @about-function{gtk-widget-set-mapped}
    @about-function{gtk-widget-get-mapped}
    @about-function{gtk-widget-get-requisition}
    @about-function{gtk-widget-device-is-shadowed}
    @about-function{gtk-widget-get-modifier-mask}
    @about-function{gtk-widget-get-path}
    @about-function{gtk-widget-get-style-context}
    @about-function{gtk-widget-reset-style}
    @about-function{gtk-requisition-new}
    @about-function{gtk-requisition-copy}
    @about-function{gtk-requisition-free}
    @about-symbol{gtk-size-request-mode}
    @about-symbol{gtk-requested-size}
    @about-function{gtk-widget-get-preferred-height}
    @about-function{gtk-widget-get-preferred-width}
    @about-function{gtk-widget-get-preferred-height-for-width}
    @about-function{gtk-widget-get-preferred-width-for-height}
    @about-function{gtk-widget-get-request-mode}
    @about-function{gtk-widget-get-preferred-size}
    @about-function{gtk-distribute-natural-allocation}
    @about-symbol{gtk-align}
    @about-function{gtk-widget-get-halign}
    @about-function{gtk-widget-set-halign}
    @about-function{gtk-widget-get-valign}
    @about-function{gtk-widget-set-valign}
    @about-function{gtk-widget-get-margin-left}
    @about-function{gtk-widget-set-margin-left}
    @about-function{gtk-widget-get-margin-right}
    @about-function{gtk-widget-set-margin-right}
    @about-function{gtk-widget-get-margin-top}
    @about-function{gtk-widget-set-margin-top}
    @about-function{gtk-widget-get-margin-bottom}
    @about-function{gtk-widget-set-margin-bottom}
    @about-function{gtk-widget-get-hexpand}
    @about-function{gtk-widget-set-hexpand}
    @about-function{gtk-widget-get-hexpand-set}
    @about-function{gtk-widget-set-hexpand-set}
    @about-function{gtk-widget-get-vexpand}
    @about-function{gtk-widget-set-vexpand}
    @about-function{gtk-widget-get-vexpand-set}
    @about-function{gtk-widget-set-vexpand-set}
    @about-function{gtk-widget-queue-compute-expand}
    @about-function{gtk-widget-compute-expand}
  @end{section}
")

;;; --- atdoc-gtk.package.lisp -------------------------------------------------

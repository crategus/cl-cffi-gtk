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

;;; --- gtk-widget-error-bell --------------------------------------------------

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





;;; --- gtk-size-request-mode --------------------------------------------------


;;; --- gtk-requested-size -----------------------------------------------------


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

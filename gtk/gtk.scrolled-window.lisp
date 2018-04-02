;;; ----------------------------------------------------------------------------
;;; gtk.scrolled-window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkScrolledWindow
;;;
;;; Adds scrollbars to its child widget
;;;
;;; Synopsis
;;;
;;;     GtkScrolledWindow
;;;
;;;     gtk_scrolled_window_new
;;;     gtk_scrolled_window_get_hadjustment
;;;     gtk_scrolled_window_get_vadjustment
;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar
;;;     gtk_scrolled_window_set_policy
;;;     gtk_scrolled_window_add_with_viewport
;;;     gtk_scrolled_window_set_placement
;;;     gtk_scrolled_window_unset_placement
;;;     gtk_scrolled_window_set_shadow_type
;;;     gtk_scrolled_window_set_hadjustment
;;;     gtk_scrolled_window_set_vadjustment
;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_get_shadow_type
;;;     gtk_scrolled_window_get_min_content_width
;;;     gtk_scrolled_window_set_min_content_width
;;;     gtk_scrolled_window_get_min_content_height
;;;     gtk_scrolled_window_set_min_content_height
;;;     gtk_scrolled_window_set_kinetic_scrolling
;;;     gtk_scrolled_window_get_kinetic_scrolling
;;;     gtk_scrolled_window_set_capture_button_press
;;;     gtk_scrolled_window_get_capture_button_press
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkScrolledWindow
;;;
;;; Implemented Interfaces
;;;
;;; GtkScrolledWindow implements AtkImplementorIface and GtkBuildable.
;;;
;;;
;;; Signals
;;;
;;;   "move-focus-out"                                : Action
;;;   "scroll-child"                                  : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScrolledWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScrolledWindow" gtk-scrolled-window
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_scrolled_window_get_type")
  ((hadjustment
    gtk-scrolled-window-hadjustment
    "hadjustment" "GtkAdjustment" t t)
   (hscrollbar-policy
    gtk-scrolled-window-hscrollbar-policy
    "hscrollbar-policy" "GtkPolicyType" t t)
   (kinetic-scrolling
    gtk-scrolled-window-kinetic-scrolling
    "kinetic-scrolling" "gboolean" t t)
   (min-content-height
    gtk-scrolled-window-min-content-height
    "min-content-height" "gint" t t)
   (min-content-width
    gtk-scrolled-window-min-content-width
    "min-content-width" "gint" t t)
   (shadow-type
    gtk-scrolled-window-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (vadjustment
    gtk-scrolled-window-vadjustment
    "vadjustment" "GtkAdjustment" t t)
   (vscrollbar-policy
    gtk-scrolled-window-vscrollbar-policy
    "vscrollbar-policy" "GtkPolicyType" t t)
   (window-placement
    gtk-scrolled-window-window-placement
    "window-placement" "GtkCornerType" t t)
   (window-placement-set
    gtk-scrolled-window-window-placement-set
    "window-placement-set" "gboolean" t t)
   #+gtk-3-22
   (propagate-natural-height
    gtk-scrolled-window-propagate-natural-height
    "propagate-natural-height" "gboolean" t t)
   #+gtk-3-22
   (propagate-natural-width
    gtk-scrolled-window-propagate-natural-width
    "propagate-natural-width" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scrolled-window 'type)
 "@version{2013-6-20}
  @begin{short}
    @sym{gtk-scrolled-window} is a @class{gtk-bin} subclass: it is a container
    the accepts a single child widget. @sym{gtk-scrolled-window} adds scrollbars
    to the child widget and optionally draws a beveled frame around the child
    widget.
  @end{short}

  The scrolled window can work in two ways. Some widgets have native scrolling
  support; these widgets implement the @class{gtk-scrollable} interface. Widgets
  with native scroll support include @class{gtk-tree-view},
  @class{gtk-text-view}, and @class{gtk-layout}.

  For widgets that lack native scrolling support, the @class{gtk-viewport}
  widget acts as an adaptor class, implementing scrollability for child widgets
  that lack their own scrolling capabilities. Use @class{gtk-viewport} to scroll
  child widgets such as @class{gtk-grid}, @class{gtk-box}, and so on.

  If a widget has native scrolling abilities, it can be added to the
  @sym{gtk-scrolled-window} with the function @fun{gtk-container-add}. If a
  widget does not, you must first add the widget to a @class{gtk-viewport}, then
  add the @class{gtk-viewport} to the scrolled window. The convenience function
  @fun{gtk-scrolled-window-add-with-viewport} does exactly this, so you can
  ignore the presence of the viewport.

  The position of the scrollbars is controlled by the scroll adjustments. See
  the function @class{gtk-adjustment} for the fields in an adjustment - for
  @class{gtk-scrollbar}, used by @sym{gtk-scrolled-window}, the @code{value}
  slot represents the position of the scrollbar, which must be between the
  @code{lower} slot and @code{upper} - @code{page-size}. The @code{page-size}
  slot represents the size of the visible scrollable area. The
  @code{step-increment} and @code{page-increment} slot are used when the user
  asks to step down (using the small stepper arrows) or page down (using for
  example the PageDown key).

  If a @sym{gtk-scrolled-window} does not behave quite as you would like, or
  does not have exactly the right layout, it is very possible to set up your own
  scrolling with @class{gtk-scrollbar} and for example a @class{gtk-grid}.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"scrollbar-spacing\" style property}
      @code{\"scrollbar-spacing\"} of type @code{:int} (Read) @br{}
      Number of pixels between the scrollbars and the scrolled window. @br{}
      Allowed values: >= 0 @br{}
      Default value: 3

    @subheading{The \"scrollbars-within-bevel\" style property}
      @code{\"scrollbars-within-bevel\"} of type @code{:boolean} (Read) @br{}
      Whether to place scrollbars within the scrolled window's bevel. @br{}
      Default value: @code{nil} @br{}
      Since 2.12
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
 lambda (scrolled-window direction-type)   : Action
      @end{pre}
      The \"move-focus-out\" signal is a keybinding signal which gets emitted
      when focus is moved away from the scrolled window by a keybinding. The
      \"move-focus\" signal is emitted with @arg{direction-type} on this
      scrolled windows toplevel parent in the container hierarchy. The default
      bindings for this signal are Tab+Ctrl and Tab+Ctrl+Shift.
      @begin[code]{table}
        @entry[scrolled-window]{A @sym{gtk-scrolled-window}.}
        @entry[direction-type]{Either @code{:forward} or @code{:backward}.}
      @end{table}
      @subheading{The \"scroll-child\" signal}
        @begin{pre}
 lambda (scrolled-window scroll horizontal)   : Action
        @end{pre}
        The \"scroll-child\" signal is a keybinding signal which gets emitted
        when a keybinding that scrolls is pressed. The horizontal or vertical
        adjustment is updated which triggers a signal that the scrolled windows
        child may listen to and scroll itself.
        @begin[code]{table}
          @entry[scrolled-window]{a @sym{gtk-scrolled-window}.}
          @entry[scroll]{A @symbol{gtk-scroll-type} describing how much to
            scroll.}
          @entry[horizontal]{Whether the keybinding scrolls the child
            horizontally or not.}
        @end{table}
  @end{dictionary}
  @see-slot{gtk-scrolled-window-hadjustment}
  @see-slot{gtk-scrolled-window-hscrollbar-policy}
  @see-slot{gtk-scrolled-window-kinetic-scrolling}
  @see-slot{gtk-scrolled-window-min-content-height}
  @see-slot{gtk-scrolled-window-min-content-width}
  @see-slot{gtk-scrolled-window-shadow-type}
  @see-slot{gtk-scrolled-window-vadjustment}
  @see-slot{gtk-scrolled-window-vscrollbar-policy}
  @see-slot{gtk-scrolled-window-window-placement}
  @see-slot{gtk-scrolled-window-window-placement-set}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hadjustment"
                                               'gtk-scrolled-window) 't)
 "The @code{\"hadjustment\"} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The @class{gtk-adjustment} for the horizontal position.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hscrollbar-policy"
                                               'gtk-scrolled-window) 't)
 "The @code{\"hscrollbar-policy\"} property of type @symbol{gtk-policy-type}
  (Read / Write) @br{}
  When the horizontal scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "kinetic-scrolling"
                                               'gtk-scrolled-window) 't)
 "The @code{\"kinetic-scrolling\"} property of type @code{:boolean}
  (Read / Write] @br{}
  The kinetic scrolling behavior flags. Kinetic scrolling only applies to
  devices with source @code{:touchscreen} @br{}
  Default value: @em{true} @br{}
  Since 3.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-height"
                                               'gtk-scrolled-window) 't)
 "The @code{\"min-content-height\"} property of type @code{:int}
  (Read / Write) @br{}
  The minimum content height of @arg{scrolled-window}, or -1 if not set. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-width"
                                               'gtk-scrolled-window) 't)
 "The @code{\"min-content-width\"} property of type @code{:int}
  (Read / Write) @br{}
  The minimum content width of @arg{scrolled-window}, or -1 if not set. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type"
                                               'gtk-scrolled-window) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Style of bevel around the contents. @br{}
  Default value: @code{:none}")

;;; ----------------------------------------------------------------------------
#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vadjustment"
                                               'gtk-scrolled-window) 't)
 "The @code{\"vadjustment\"} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The @class{gtk-adjustment} for the vertical position.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vscrollbar-policy"
                                               'gtk-scrolled-window) 't)
 "The @code{\"vscrollbar-policy\"} property of type @symbol{gtk-policy-type}
  (Read / Write) @br{}
  When the vertical scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement"
                                               'gtk-scrolled-window) 't)
 "The @code{\"window-placement\"} property of type @symbol{gtk-corner-type}
  (Read / Write) @br{}
  Where the contents are located with respect to the scrollbars. This property
  only takes effect if @code{\"window-placement-set\"} is @em{true}. @br{}
  Default value: @code{:left}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement-set"
                                               'gtk-scrolled-window) 't)
 "The @code{\"window-placement-set\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether @code{\"window-placement\"} should be used to determine the location
  of the contents with respect to the scrollbars. Otherwise, the
  @code{\"gtk-scrolled-window-placement\"} setting is used. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-hadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-hadjustment 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"hadjustment\"} of the @class{gtk-scrolled-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-hscrollbar-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-hscrollbar-policy 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"hscrollbar-policy\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-kinetic-scrolling atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-kinetic-scrolling 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"kinetic-scrolling\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-height 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"min-content-height\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-width 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"min-content-width\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-shadow-type 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"shadow-type\"} of the @class{gtk-scrolled-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-vadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-vadjustment 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"vadjustment\"} of the @class{gtk-scrolled-window}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-vscrollbar-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-vscrollbar-policy 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"vscrollbar-policy\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"window-placement\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement-set 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"window-placement-set\"} of the
  @class{gtk-scrolled-window} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-new))

(defun gtk-scrolled-window-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[hadjustment]{horizontal adjustment}
  @argument[vadjustment]{vertical adjustment}
  @return{A new scrolled window.}
  @short{Creates a new scrolled window.}

  The two arguments are the scrolled window's adjustments; these will be
  shared with the scrollbars and the child widget to keep the bars in sync
  with the child. Usually you want to pass @code{nil} for the adjustments, which
  will cause the scrolled window to create them for you."
  (make-instance 'gtk-scrolled-window
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-scrolled-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_hadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-hadjustment))

(defun gtk-scrolled-window-get-hadjustment (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The horizontal @class{gtk-adjustment}.}
  Returns the horizontal scrollbar's adjustment, used to connect the
  horizontal scrollbar to the child widget's horizontal scroll functionality."
  (gtk-scrolled-window-hadjustment scrolled-window))

(export 'gtk-scrolled-window-get-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_vadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-vadjustment))

(defun gtk-scrolled-window-get-vadjustment (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-4}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The vertical @class{gtk-djustment} object.}
  Returns the vertical scrollbar's adjustment, used to connect the vertical
  scrollbar to the child widget's vertical scroll functionality."
  (gtk-scrolled-window-vadjustment scrolled-window))

(export 'gtk-scrolled-window-get-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_hscrollbar ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_get_hscrollbar"
           gtk-scrolled-window-get-hscrollbar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @begin{return}
    The horizontal scrollbar of the scrolled window, or @code{nil} if it does
    not have one.
  @end{return}
  @short{Returns the horizontal scrollbar of @arg{scrolled-window}.}

  Since 2.8"
  (scrolled-window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-get-hscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_vscrollbar ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_get_vscrollbar"
           gtk-scrolled-window-get-vscrollbar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @begin{return}
    The vertical scrollbar of the scrolled window, or @code{nil} if it does not
    have one.
  @end{return}
  @short{Returns the vertical scrollbar of @arg{scrolled-window}.}

  Since 2.8"
  (scrolled-window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-get-vscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-policy))

(defun gtk-scrolled-window-set-policy (scrolled-window hscrollbar-policy
                                                       vscrollbar-policy)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[hscrollbar-policy]{policy for horizontal bar}
  @argument[vscrollbar-policy]{policy for vertical bar}
  @begin{short}
    Sets the scrollbar policy for the horizontal and vertical scrollbars.
  @end{short}

  The policy determines when the scrollbar should appear; it is a value from
  the @symbol{gtk-policy-type} enumeration. If @code{:always}, the scrollbar is
  always present; if @code{:never}, the scrollbar is never present; if
  @code{:automatic}, the scrollbar is present only if needed (that is, if
  the slider part of the bar would be smaller than the trough - the display is
  larger than the page size)."
  (setf (gtk-scrolled-window-hscrollbar-policy scrolled-window)
        hscrollbar-policy
        (gtk-scrolled-window-vscrollbar-policy scrolled-window)
        vscrollbar-policy))

(export 'gtk-scrolled-window-set-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_add_with_viewport ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_add_with_viewport"
           gtk-scrolled-window-add-with-viewport) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[child]{the widget you want to scroll}
  @begin{short}
    Used to add children without native scrolling capabilities. This is simply a
    convenience function; it is equivalent to adding the unscrollable child to a
    viewport, then adding the viewport to the scrolled window. If a child has
    native scrolling, use the function @fun{gtk-container-add} instead of this
    function.
  @end{short}

  The viewport scrolls the child by moving its @class{gdk-window}, and takes the
  size of the child to be the size of its toplevel @class{gdk-window}. This will
  be very wrong for most widgets that support native scrolling; for example, if
  you add a widget such as @class{gtk-tree-view} with a viewport, the whole
  widget will scroll, including the column headings. Thus, widgets with native
  scrolling support should not be used with the @class{gtk-viewport} proxy.

  A widget supports scrolling natively if it implements the
  @class{gtk-scrollable} interface."
  (scrolled-window (g-object gtk-scrolled-window))
  (child (g-object gtk-widget)))

(export 'gtk-scrolled-window-add-with-viewport)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_placement ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-placement))

(defun gtk-scrolled-window-set-placement (scrolled-window window-placement)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[window-placement]{position of the child window}
  @begin{short}
    Sets the placement of the contents with respect to the scrollbars for the
    scrolled window.
  @end{short}

  The default is @code{:left}, meaning the child is in the top left, with the
  scrollbars underneath and to the right. Other values in
  @symbol{gtk-corner-type} are @code{:top-right}, @code{:bottom-left}, and
  @code{:bottom-right}.

  See also the functions @fun{gtk-scrolled-window-get-placement} and
  @fun{gtk-scrolled-window-unset-placement}.
  @see-function{gtk-scrolled-window-get-placement}
  @see-function{gtk-scrolled-window-unset-placement}"
  (setf (gtk-scrolled-window-window-placement-set scrolled-window)
        t
        (gtk-scrolled-window-window-placement scrolled-window)
        window-placement))

(export 'gtk-scrolled-window-set-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_unset_placement ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-unset-placement))

(defun gtk-scrolled-window-unset-placement (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @begin{short}
    Unsets the placement of the contents with respect to the scrollbars for the
    scrolled window. If no window placement is set for a scrolled window, it
    obeys the @code{\"gtk-scrolled-window-placement\"} XSETTING.
  @end{short}

  See also the functions @fun{gtk-scrolled-window-set-placement} and
  @fun{gtk-scrolled-window-get-placement}.

  Since 2.10
  @see-function{gtk-scrolled-window-set-placement}
  @see-function{gtk-scrolled-window-get-placement}"
  (setf (gtk-scrolled-window-window-placement-set scrolled-window) nil))

(export 'gtk-scrolled-window-unset-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-shadow-type))

(defun gtk-scrolled-window-set-shadow-type (scrolled-window shadow-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[shadow-type]{kind of shadow to draw around scrolled window contents}
  Changes the type of shadow drawn around the contents of
  @arg{scrolled-window}."
  (setf (gtk-scrolled-window-shadow-type scrolled-window) shadow-type))

(export 'gtk-scrolled-window-set-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_hadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-hadjustment))

(defun gtk-scrolled-window-set-hadjustment (scrolled-window hadjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[hadjustment]{horizontal scroll adjustment}
  Sets the @class{gtk-adjustment} for the horizontal scrollbar."
  (setf (gtk-scrolled-window-hadjustment scrolled-window) hadjustment))

(export 'gtk-scrolled-window-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_vadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-vadjustment))

(defun gtk-scrolled-window-set-vadjustment (scrolled-window vadjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[vadjustment]{vertical scroll adjustment}
  Sets the @class{gtk-adjustment} for the vertical scrollbar."
  (setf (gtk-scrolled-window-vadjustment scrolled-window) vadjustment))

(export 'gtk-scrolled-window-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_placement ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-placement))

(defun gtk-scrolled-window-get-placement (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @begin{return}
    The current placement value. See also the functions
    @fun{gtk-scrolled-window-set-placement} and
    @fun{gtk-scrolled-window-unset-placement}.
  @end{return}
  Gets the placement of the contents with respect to the scrollbars for the
  scrolled window. See the function @fun{gtk-scrolled-window-set-placement}.
  @see-function{gtk-scrolled-window-set-placement}
  @see-function{gtk-scrolled-window-unset-placement}"
  (gtk-scrolled-window-window-placement scrolled-window))

(export 'gtk-scrolled-window-get-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-policy))

(defun gtk-scrolled-window-get-policy (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @begin{return}
    @code{hscrollbar-policy} -- the policy for the horizontal scrollbar,
                                or @code{nil} @br{}
    @code{vscrollbar-policy} -- the policy for the vertical scrollbar,
                                or @code{nil}
  @end{return}
  Retrieves the current policy values for the horizontal and vertical
  scrollbars. See the function @fun{gtk-scrolled-window-set-policy}.
  @see-function{gtk-scrolled-window-set-policy}"
  (values (gtk-scrolled-window-hscrollbar-policy scrolled-window)
          (gtk-scrolled-window-vscrollbar-policy scrolled-window)))

(export 'gtk-scrolled-window-get-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_shadow_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-shadow-type))

(defun gtk-scrolled-window-get-shadow-type (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The current shadow type.}
  Gets the shadow type of the scrolled window. See the function
  @fun{gtk-scrolled-window-set-shadow-type}.
  @see-function{gtk-scrolled-window-set-shadow-type}"
  (gtk-scrolled-window-shadow-type scrolled-window))

(export 'gtk-scrolled-window-get-shadow-type)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_min_content_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-min-content-width))

(defun gtk-scrolled-window-get-min-content-width (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The minimum content width.}
  @begin{short}
    Gets the minimum content width of @arg{scrolled-window}, or -1 if not set.
  @end{short}

  Since 3.0"
  (gtk-scrolled-window-min-content-width scrolled-window))

(export 'gtk-scrolled-window-get-min-content-width)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_min_content_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-min-content-width))

(defun gtk-scrolled-window-set-min-content-width (scrolled-window width)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[width]{the minimal content width}
  @begin{short}
    Sets the minimum width that @arg{scrolled-window} should keep visible. Note
    that this can and (usually will) be smaller than the minimum size of the
    content.
  @end{short}

  Since 3.0"
  (setf (gtk-scrolled-window-min-content-width scrolled-window) width))

(export 'gtk-scrolled-window-set-min-content-width)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_min_content_height ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-min-content-height))

(defun gtk-scrolled-window-get-min-content-height (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The minimal content height.}
  @begin{short}
    Gets the minimal content height of @arg{scrolled-window}, or -1 if not set.
  @end{short}

  Since 3.0"
  (gtk-scrolled-window-min-content-height scrolled-window))

(export 'gtk-scrolled-window-get-min-content-height)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_min_content_height ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-min-content-height))

(defun gtk-scrolled-window-set-min-content-height (scrolled-window height)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[height]{the minimal content height}
  @begin{short}
    Sets the minimum height that @arg{scrolled-window} should keep visible. Note
    that this can and (usually will) be smaller than the minimum size of the
    content.
  @end{short}

  Since 3.0"
  (setf (gtk-scrolled-window-min-content-height scrolled-window) height))

(export 'gtk-scrolled-window-set-min-content-height)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_kinetic_scrolling ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-set-kinetic-scrolling))

(defun gtk-scrolled-window-set-kinetic-scrolling (scrolled-window
                                                  kinetic-scrolling)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[kinetic-scrolling]{@em{true} to enable kinetic scrolling}
  @begin{short}
    Turns kinetic scrolling on or off. Kinetic scrolling only applies to devices
    with source @code{:touchscreen}.
  @end{short}

  Since 3.4"
  (setf (gtk-scrolled-window-kinetic-scrolling scrolled-window)
        kinetic-scrolling))

(export 'gtk-scrolled-window-set-kinetic-scrolling)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_kinetic_scrolling ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-kinetic-scrolling))

(defun gtk-scrolled-window-get-kinetic-scrolling (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The scrolling behavior flags.}
  @short{Returns the specified kinetic scrolling behavior.}

  Since 3.4"
  (gtk-scrolled-window-kinetic-scrolling scrolled-window))

(export 'gtk-scrolled-window-get-kinetic-scrolling)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_capture_button_press ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_set_capture_button_press"
           gtk-scrolled-window-set-capture-button-press) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[capture-button-press]{@em{true} to capture button presses}
  @begin{short}
    Changes the behaviour of @arg{scrolled-window} wrt. to the initial event
    that possibly starts kinetic scrolling. When @arg{capture-button-press} is
    set to @em{true}, the event is captured by the scrolled window, and then
    later replayed if it is meant to go to the child widget.
  @end{short}

  This should be enabled if any child widgets perform non-reversible actions
  on \"button-press-event\" signals. If they do not, and handle additionally
  handle \"grab-broken-event\", it might be better to set
  @arg{capture-button-press} to @code{nil}.

  This setting only has an effect if kinetic scrolling is enabled.

  Since 3.4"
  (scrolled-window (g-object gtk-scrolled-window)
  (capture-button-press :boolean)))

(export 'gtk-scrolled-window-set-capture-button-press)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_capture_button_press ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_get_capture_button_press"
           gtk-scrolled-window-get-capture-button-press) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{@em{True} if button presses are captured during kinetic scrolling.}
  @begin{short}
    Return whether button presses are captured during kinetic scrolling. See
    the function @fun{gtk-scrolled-window-set-capture-button-press}.
  @end{short}

  Since 3.4
  @see-function{gtk-scrolled-window-set-capture-button-press}"
  (scrolled-window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-get-capture-button-press)

;;; --- End of file gtk.scrolled-window.lisp -----------------------------------

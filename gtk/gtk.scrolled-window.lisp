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
;;;     Adds scrollbars to its child widget
;;;
;;; Types and Values
;;;
;;;     GtkScrolledWindow
;;;     GtkPolicyType
;;;     GtkCornerType
;;;
;;; Functions
;;;
;;;     gtk_scrolled_window_new
;;;     gtk_scrolled_window_get_hadjustment                Accessor
;;;     gtk_scrolled_window_set_hadjustment                Accessor
;;;     gtk_scrolled_window_get_vadjustment                Accessor
;;;     gtk_scrolled_window_set_vadjustment                Accessor
;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar
;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy
;;;     gtk_scrolled_window_add_with_viewport
;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement
;;;     gtk_scrolled_window_unset_placement
;;;     gtk_scrolled_window_get_shadow_type                Accessor
;;;     gtk_scrolled_window_set_shadow_type                Accessor
;;;     gtk_scrolled_window_get_kinetic_scrolling          Accessor
;;;     gtk_scrolled_window_set_kinetic_scrolling          Accessor
;;;     gtk_scrolled_window_get_capture_button_press
;;;     gtk_scrolled_window_set_capture_button_press
;;;     gtk_scrolled_window_get_overlay_scrolling          Accessor
;;;     gtk_scrolled_window_set_overlay_scrolling          Accessor
;;;     gtk_scrolled_window_get_min_content_width          Accessor
;;;     gtk_scrolled_window_set_min_content_width          Accessor
;;;     gtk_scrolled_window_get_min_content_height         Accessor
;;;     gtk_scrolled_window_set_min_content_height         Accessor
;;;     gtk_scrolled_window_get_max_content_width          Accessor
;;;     gtk_scrolled_window_set_max_content_width          Accessor
;;;     gtk_scrolled_window_get_max_content_height         Accessor
;;;     gtk_scrolled_window_set_max_content_height         Accessor
;;;     gtk_scrolled_window_get_propagate_natural_width    Accessor
;;;     gtk_scrolled_window_set_propagate_natural_width    Accessor
;;;     gtk_scrolled_window_get_propagate_natural_height   Accessor
;;;     gtk_scrolled_window_set_propagate_natural_height   Accessor
;;;
;;; Properties
;;;
;;;     GtkAdjustment*  hadjustment                 Read / Write / Construct
;;;     GtkPolicyType   hscrollbar-policy           Read / Write
;;;          gboolean   kinetic-scrolling           Read / Write
;;;              gint   max-content-height          Read / Write
;;;              gint   max-content-width           Read / Write
;;;              gint   min-content-height          Read / Write
;;;              gint   min-content-width           Read / Write
;;;          gboolean   overlay-scrolling           Read / Write
;;;          gboolean   propagate-natural-height    Read / Write
;;;          gboolean   propagate-natural-width     Read / Write
;;;     GtkShadowType   shadow-type                 Read / Write
;;;     GtkAdjustment*  vadjustment                 Read / Write / Construct
;;;     GtkPolicyType   vscrollbar-policy           Read / Write
;;;     GtkCornerType   window-placement            Read / Write
;;;          gboolean   window-placement-set        Read / Write
;;;
;;; Style Properties
;;;
;;;              gint   scrollbar-spacing           Read
;;;          gboolean   scrollbars-within-bevel     Read
;;;
;;; Signals
;;;
;;;              void   edge-overshot               Run Last
;;;              void   edge-reached                Run Last
;;;              void   move-focus-out              Action
;;;          gboolean   scroll-child                Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkScrolledWindow
;;;                         ╰── GtkPlacesSidebar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScrolledWindow implements AtkImplementorIface and GtkBuildable.
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
   #+gtk-3-22
   (max-content-height
    gtk-scrolled-window-max-content-height
    "max-content-height" "gint" t t)
   #+gtk-3-22
   (max-content-width
    gtk-scrolled-window-max-content-width
    "max-content-width" "gint" t t)
   (min-content-height
    gtk-scrolled-window-min-content-height
    "min-content-height" "gint" t t)
   (min-content-width
    gtk-scrolled-window-min-content-width
    "min-content-width" "gint" t t)
   #+gtk-3-16
   (overlay-scrolling
    gtk-scrolled-window-overlay-scrolling
    "overlay-scrolling" "gboolean" t t)
   #+gtk-3-22
   (propagate-natural-height
    gtk-scrolled-window-propagate-natural-height
    "propagate-natural-height" "gboolean" t t)
   #+gtk-3-22
   (propagate-natural-width
    gtk-scrolled-window-propagate-natural-width
    "propagate-natural-width" "gboolean" t t)
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
    "window-placement-set" "gboolean" t t)))

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

  @bsubheading{Touch support}
  @sym{gtk-scrolled-window} has built-in support for touch devices. When a
  touchscreen is used, swiping will move the scrolled window, and will expose
  'kinetic' behavior. This can be turned off with the \"kinetic-scrolling\"
  property if it is undesired.

  @sym{gtk-scrolled-window} also displays visual 'overshoot' indication when the
  content is pulled beyond the end, and this situation can be captured with the
  \"edge-overshot\" signal.

  If no mouse device is present, the scrollbars will overlayed as narrow,
  auto-hiding indicators over the content. If traditional scrollbars are desired
  although no mouse is present, this behaviour can be turned off with the
  \"overlay-scrolling\" property.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-scrolled-window} has a main CSS node with name
    @code{scrolledwindow}.

    It uses subnodes with names @code{overshoot} and @code{undershoot} to draw
    the overflow and underflow indications. These nodes get the @code{.left},
    @code{.right}, @code{.top} or @code{.bottom} style class added depending on
    where the indication is drawn.

    @sym{gtk-scrolled-window} also sets the positional style classes
    @code{.left}, @code{.right}, @code{.top}, @code{.bottom} and style classes
    related to overlay scrolling @code{.overlay-indicator}, @code{.dragging},
    @code{.hovering} on its scrollbars.

    If both scrollbars are visible, the area where they meet is drawn with a
    subnode named @code{junction}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[scrollbar-spacing]{entry}
        The @code{scrollbar-spacing} style property of type @code{:int}
        (Read) @br{}
        Number of pixels between the scrollbars and the scrolled window. @br{}
        Allowed values: >= 0 @br{}
        Default value: 3
      @end{entry}
      @begin[scrollbars-within-bevel]{entry}
        The @code{scrollbars-within-bevel} style property of type
        @code{:boolean} (Read) @br{}
        Whether to place scrollbars within the scrolled window's bevel. @br{}
        @b{Warning:} @code{scrollbars-within-bevel} has been deprecated since
        version 3.20 and should not be used in newly-written code. The value of
        this style property is ignored. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"edge-overshot\" signal}
      @begin{pre}
 lambda (scolled-window pos)    : Run Last
      @end{pre}
      The \"edge-overshot\" signal is emitted whenever user initiated scrolling
      makes the scrolled window firmly surpass, i.e. with some edge resistance,
      the lower or upper limits defined by the adjustment in that orientation.
      A similar behavior without edge resistance is provided by the
      \"edge-reached\" signal.
      Note: The @arg{pos} argument is LTR/RTL aware, so callers should be aware
      too if intending to provide behavior on horizontal edges.
      @begin[code]{table}
        @entry[scrolled-window]{A @sym{gtk-scrolled-window} object.} 
        @entry[pos]{Edge side of type @symbol{gtk-position-type} that was hit.}
      @end{table}
      Since 3.16

    @subheading{The \"edge-reached\" signal}
      @begin{short}
 lambda (scrolled-window pos)    : Run Last
      @end{short}
      The \"edge-reached\" signal is emitted whenever user-initiated scrolling
      makes the scrolled window exactly reach the lower or upper limits defined
      by the adjustment in that orientation.
      A similar behavior with edge resistance is provided by the
      \"edge-overshot\" signal.
      Note: The @arg{pos} argument is LTR/RTL aware, so callers should be aware
      too if intending to provide behavior on horizontal edges.
      @begin[code]{table}
        @entry[scrolled-window]{A @sym{gtk-scrolled-window} object.} 
        @entry[pos]{Edge side of type @symbol{gtk-position-type} that was hit.}
      @end{table}
      Since 3.16

    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
 lambda (scrolled-window direction-type)    : Action
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
 lambda (scrolled-window scroll horizontal)    : Action
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
  @see-slot{gtk-scrolled-window-max-content-height}
  @see-slot{gtk-scrolled-window-max-content-width}
  @see-slot{gtk-scrolled-window-min-content-height}
  @see-slot{gtk-scrolled-window-min-content-width}
  @see-slot{gtk-scrolled-window-overlay-scrolling}
  @see-slot{gtk-scrolled-window-propagate-natural-height}
  @see-slot{gtk-scrolled-window-propagate-natural-width}
  @see-slot{gtk-scrolled-window-shadow-type}
  @see-slot{gtk-scrolled-window-vadjustment}
  @see-slot{gtk-scrolled-window-vscrollbar-policy}
  @see-slot{gtk-scrolled-window-window-placement}
  @see-slot{gtk-scrolled-window-window-placement-set}
  @see-class{gtk-scrollable}
  @see-class{gtk-viewport}
  @see-class{gtk-adjustment}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-scrolled-window-hadjustment ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hadjustment"
                                               'gtk-scrolled-window) 't)
 "The @code{hadjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The @class{gtk-adjustment} for the horizontal position.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-hadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-hadjustment 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{hadjustment} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-hscrollbar-policy ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hscrollbar-policy"
                                               'gtk-scrolled-window) 't)
 "The @code{hscrollbar-policy} property of type @symbol{gtk-policy-type}
  (Read / Write) @br{}
  When the horizontal scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-hscrollbar-policy
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-hscrollbar-policy 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{hscrollbar-policy} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-kinetic-scrolling ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "kinetic-scrolling"
                                               'gtk-scrolled-window) 't)
 "The @code{kinetic-scrolling} property of type @code{:boolean}
  (Read / Write] @br{}
  The kinetic scrolling behavior flags. Kinetic scrolling only applies to
  devices with source @code{:touchscreen} @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-kinetic-scrolling
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-kinetic-scrolling 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{kinetic-scrolling} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-max-content-height ---------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-content-height"
                                               'gtk-scrolled-window) 't)
 "The @code{max-content-height} property of type @code{:int}
  (Read / Write] @br{}
  The maximum content height of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-max-content-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-max-content-height 'function)
 "@version{2019-4-10}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{max-content-height} of the
    @class{gtk-scrolled-window} class.
  @end{short}

  Since 3.22
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-max-content-width ----------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-content-width"
                                               'gtk-scrolled-window) 't)
 "The @code{max-content-width} property of type @code{:int}
  (Read / Write] @br{}
  The maximum content width of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-max-content-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-max-content-width 'function)
 "@version{2019-4-10}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{max-content-width} of the
    @class{gtk-scrolled-window} class.
  @end{short}

  Since 3.22
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-min-content-height ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-height"
                                               'gtk-scrolled-window) 't)
 "The @code{min-content-height} property of type @code{:int}
  (Read / Write) @br{}
  The minimum content height of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-height 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{min-content-height} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-min-content-width ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-width"
                                               'gtk-scrolled-window) 't)
 "The @code{min-content-width} property of type @code{:int}
  (Read / Write) @br{}
  The minimum content width of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-width 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{min-content-width} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-overlay-scrolling ----------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "overlay-scrolling"
                                               'gtk-scrolled-window) 't)
 "The @code{overlay-scrolling} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether overlay scrolling is enabled or not. If it is, the scrollbars are only
  added as traditional widgets when a mouse is present. Otherwise, they are
  overlayed on top of the content, as narrow indicators. @br{}
  Default value: @em{true} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-overlay-scrolling
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-overlay-scrolling 'function)
 "@version{2019-4-10}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{overlay-scrolling} of the
    @class{gtk-scrolled-window} class.
  @end{short}

  Since 3.16
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-propagate-natural-height ---------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "propagate-natural-height"
                                               'gtk-scrolled-window) 't)
 "The @code{propagate-natural-height} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural height of the child should be calculated and propagated
  through the scrolled window’s requested natural height.
  This is useful in cases where an attempt should be made to allocate exactly
  enough space for the natural size of the child. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-propagate-natural-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-propagate-natural-height 'function)
 "@version{2019-4-10}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{propagate-natural-height}
    of the @class{gtk-scrolled-window} class.
  @end{short}

  Since 3.22
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-propagate-natural-width ----------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "propagate-natural-width"
                                               'gtk-scrolled-window) 't)
 "The @code{propagate-natural-width} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural width of the child should be calculated and propagated
  through the scrolled window’s requested natural width.
  This is useful in cases where an attempt should be made to allocate exactly
  enough space for the natural size of the child. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-propagate-natural-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-propagate-natural-width 'function)
 "@version{2019-4-10}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{propagate-natural-width}
    of the @class{gtk-scrolled-window} class.
  @end{short}

  Since 3.22
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-shadow-type ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type"
                                               'gtk-scrolled-window) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Style of bevel around the contents. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-shadow-type 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{shadow-type} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-vadjustment ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vadjustment"
                                               'gtk-scrolled-window) 't)
 "The @code{vadjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The @class{gtk-adjustment} for the vertical position.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-vadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-vadjustment 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{vadjustment} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-vscrollbar-policy ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vscrollbar-policy"
                                               'gtk-scrolled-window) 't)
 "The @code{vscrollbar-policy} property of type @symbol{gtk-policy-type}
  (Read / Write) @br{}
  When the vertical scrollbar is displayed. @br{}
  Default value: @code{:automatic}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-vscrollbar-policy
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-vscrollbar-policy 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{vscrollbar-policy} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-window-placement -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement"
                                               'gtk-scrolled-window) 't)
 "The @code{window-placement} property of type @symbol{gtk-corner-type}
  (Read / Write) @br{}
  Where the contents are located with respect to the scrollbars. This property
  only takes effect if @code{window-placement-set} is @em{true}. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{window-placement} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-window-placement-set -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement-set"
                                               'gtk-scrolled-window) 't)
 "The @code{window-placement-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether @code{window-placement} should be used to determine the location
  of the contents with respect to the scrollbars. @br{}
  @b{Warning:} @code{window-placement-set} has been deprecated since version
  3.10 and should not be used in newly-written code. This value is ignored and
  @code{window-placement} value is always honored. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement-set 'function)
 "@version{2013-3-23}
  @begin{short}
    Accessor of the slot @slot[gtk-scrolled-window]{window-placement-set} of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}")

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
  @see-class{gtk-scrolled-window}"
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
  @see-class{gtk-scrolled-window}"
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
;;; gtk_scrolled_window_add_with_viewport ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_add_with_viewport"
           gtk-scrolled-window-add-with-viewport) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-4-10}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @argument[child]{the @class{gtk-widget} object you want to scroll}
  @begin{short}
    Used to add children without native scrolling capabilities.
  @end{short}
  This is simply a convenience function; it is equivalent to adding the
  unscrollable child to a viewport, then adding the viewport to the scrolled
  window. If a child has native scrolling, use the @fun{gtk-container-add}
  function instead of this function.

  The viewport scrolls the child by moving its @class{gdk-window}, and takes the
  size of the child to be the size of its toplevel @class{gdk-window}. This will
  be very wrong for most widgets that support native scrolling; for example, if
  you add a widget such as @class{gtk-tree-view} with a viewport, the whole
  widget will scroll, including the column headings. Thus, widgets with native
  scrolling support should not be used with the @class{gtk-viewport} proxy.

  A widget supports scrolling natively if it implements the
  @class{gtk-scrollable} interface.
  @begin[Warning]{dictionary}
    The @sym{gtk-scrolled-window-add-with-viewport} function has been deprecated
    since version 3.8 and should not be used in newly-written code.
    The @fun{gtk-container-add} function will automatically add a 
    @class{gtk-viewport} if the child doesn’t implement @class{gtk-scrollable}.
  @end{dictionary}
  @see-class{gtk-scrolled-window}"
  (scrolled-window (g-object gtk-scrolled-window))
  (child (g-object gtk-widget)))

(export 'gtk-scrolled-window-add-with-viewport)

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
  @see-function{gtk-scrolled-window-set-placement}
  @see-function{gtk-scrolled-window-get-placement}"
  (setf (gtk-scrolled-window-window-placement-set scrolled-window) nil))

(export 'gtk-scrolled-window-unset-placement)

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
;;; gtk_scrolled_window_get_kinetic_scrolling ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-get-kinetic-scrolling))

(defun gtk-scrolled-window-get-kinetic-scrolling (scrolled-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrolled-window]{a @class{gtk-scrolled-window} container}
  @return{The scrolling behavior flags.}
  @short{Returns the specified kinetic scrolling behavior.}
  @see-class{gtk-scrolled-window}"
  (gtk-scrolled-window-kinetic-scrolling scrolled-window))

(export 'gtk-scrolled-window-get-kinetic-scrolling)

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
  @see-class{gtk-scrolled-window}"
  (setf (gtk-scrolled-window-kinetic-scrolling scrolled-window)
        kinetic-scrolling))

(export 'gtk-scrolled-window-set-kinetic-scrolling)

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
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-set-capture-button-press}"
  (scrolled-window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-get-capture-button-press)

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
  @see-class{gtk-scrolled-window}"
  (scrolled-window (g-object gtk-scrolled-window)
  (capture-button-press :boolean)))

(export 'gtk-scrolled-window-set-capture-button-press)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_overlay_scrolling ()
;;;
;;; gboolean
;;; gtk_scrolled_window_get_overlay_scrolling
;;;                               (GtkScrolledWindow *scrolled_window);
;;;
;;; Returns whether overlay scrolling is enabled for this scrolled window.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; Returns :
;;;     TRUE if overlay scrolling is enabled
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_overlay_scrolling ()
;;;
;;; void
;;; gtk_scrolled_window_set_overlay_scrolling
;;;                                (GtkScrolledWindow *scrolled_window,
;;;                                 gboolean overlay_scrolling);
;;;
;;; Enables or disables overlay scrolling for this scrolled window.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; overlay_scrolling :
;;;     whether to enable overlay scrolling
;;; 
;;; Since 3.16
;;; ----------------------------------------------------------------------------

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
  @see-class{gtk-scrolled-window}"
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
  @see-class{gtk-scrolled-window}"
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
  @see-class{gtk-scrolled-window}"
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
  @see-class{gtk-scrolled-window}"
  (setf (gtk-scrolled-window-min-content-height scrolled-window) height))

(export 'gtk-scrolled-window-set-min-content-height)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_max_content_width ()
;;;
;;; gint
;;; gtk_scrolled_window_get_max_content_width
;;;                               (GtkScrolledWindow *scrolled_window);
;;;
;;; Returns the maximum content width set.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;; 
;;; Returns :
;;;     the maximum content width, or -1
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_max_content_width ()
;;;
;;; void
;;; gtk_scrolled_window_set_max_content_width
;;;                                (GtkScrolledWindow *scrolled_window,
;;;                                 gint width);
;;;
;;; Sets the maximum width that scrolled_window should keep visible. The
;;; scrolled_window will grow up to this width before it starts scrolling the
;;; content.
;;;
;;; It is a programming error to set the maximum content width to a value
;;; smaller than “min-content-width”.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; width :
;;;     the maximum content width
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_max_content_height ()
;;;
;;; gint
;;; gtk_scrolled_window_get_max_content_height
;;;                               (GtkScrolledWindow *scrolled_window);
;;;
;;; Returns the maximum content height set.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; Returns :
;;;     the maximum content height, or -1
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_max_content_height ()
;;;
;;; void
;;; gtk_scrolled_window_set_max_content_height
;;;                                (GtkScrolledWindow *scrolled_window,
;;;                                 gint height);
;;;
;;; Sets the maximum height that scrolled_window should keep visible. The
;;; scrolled_window will grow up to this height before it starts scrolling the
;;; content.
;;;
;;; It is a programming error to set the maximum content height to a value
;;; smaller than “min-content-height”.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; height :
;;;     the maximum content height
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_propagate_natural_width ()
;;;
;;; gboolean
;;; gtk_scrolled_window_get_propagate_natural_width
;;;                                (GtkScrolledWindow *scrolled_window);
;;;
;;; Reports whether the natural width of the child will be calculated and
;;; propagated through the scrolled window’s requested natural width.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; Returns :
;;;     whether natural width propagation is enabled.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_propagate_natural_width ()
;;;
;;; void
;;; gtk_scrolled_window_set_propagate_natural_width
;;;                                (GtkScrolledWindow *scrolled_window,
;;;                                 gboolean propagate);
;;;
;;; Sets whether the natural width of the child should be calculated and
;;; propagated through the scrolled window’s requested natural width.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; propagate :
;;;     whether to propagate natural width
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_propagate_natural_height ()
;;;
;;; gboolean
;;; gtk_scrolled_window_get_propagate_natural_height
;;;                                (GtkScrolledWindow *scrolled_window);
;;;
;;; Reports whether the natural height of the child will be calculated and
;;; propagated through the scrolled window’s requested natural height.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; Returns :
;;;     whether natural height propagation is enabled.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_propagate_natural_height ()
;;;
;;; void
;;; gtk_scrolled_window_set_propagate_natural_height
;;;                                (GtkScrolledWindow *scrolled_window,
;;;                                 gboolean propagate);
;;;
;;; Sets whether the natural height of the child should be calculated and
;;; propagated through the scrolled window’s requested natural height.
;;;
;;; scrolled_window :
;;;     a GtkScrolledWindow
;;;
;;; propagate :
;;;     whether to propagate natural height
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.scrolled-window.lisp -----------------------------------

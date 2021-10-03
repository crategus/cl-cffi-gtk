;;; ----------------------------------------------------------------------------
;;; gtk.scrolled-window.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;;     GtkAdjustment*   hadjustment                 Read / Write / Construct
;;;     GtkPolicyType    hscrollbar-policy           Read / Write
;;;          gboolean    kinetic-scrolling           Read / Write
;;;              gint    max-content-height          Read / Write
;;;              gint    max-content-width           Read / Write
;;;              gint    min-content-height          Read / Write
;;;              gint    min-content-width           Read / Write
;;;          gboolean    overlay-scrolling           Read / Write
;;;          gboolean    propagate-natural-height    Read / Write
;;;          gboolean    propagate-natural-width     Read / Write
;;;     GtkShadowType    shadow-type                 Read / Write
;;;     GtkAdjustment*   vadjustment                 Read / Write / Construct
;;;     GtkPolicyType    vscrollbar-policy           Read / Write
;;;     GtkCornerType    window-placement            Read / Write
;;;          gboolean    window-placement-set        Read / Write
;;;
;;; Style Properties
;;;
;;;              gint    scrollbar-spacing           Read
;;;          gboolean    scrollbars-within-bevel     Read
;;;
;;; Signals
;;;
;;;              void    edge-overshot               Run Last
;;;              void    edge-reached                Run Last
;;;              void    move-focus-out              Action
;;;          gboolean    scroll-child                Action
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
;;; enum GtkPolicyType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPolicyType" gtk-policy-type
  (:export t
   :type-initializer "gtk_policy_type_get_type")
  (:always 0)
  (:automatic 1)
  (:never 2)
  (:external))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-policy-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-policy-type atdoc:*external-symbols*)
 "@version{*2021-7-24}
  @begin{short}
    Determines how the size should be computed to achieve one of the visibility
    mode for the scrollbars.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPolicyType\" gtk-policy-type
  (:export t
   :type-initializer \"gtk_policy_type_get_type\")
  (:always 0)
  (:automatic 1)
  (:never 2)
  (:external 3))
  @end{pre}
  @begin[code]{table}
    @entry[:always]{The scrollbar is always visible. The view size is
      independent of the content.}
    @entry[:automatic]{The scrollbar will appear and disappear as necessary.
      For example, when all of a @class{gtk-tree-view} widget cannot be seen.}
    @entry[:never]{The scrollbar should never appear. In this mode the content
      determines the size.}
    @entry[:external]{Do not show a scrollbar, but do not force the size to
      follow the content. This can be used e.g. to make multiple scrolled
      windows share a scrollbar.}
  @end{table}
  @see-class{gtk-scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; enum GtkCornerType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCornerType" gtk-corner-type
  (:export t
   :type-initializer "gtk_corner_type_get_type")
  (:top-left 0)
  (:bottom-left 1)
  (:top-right 2)
  (:bottom-right 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-corner-type atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-corner-type atdoc:*external-symbols*)
 "@version{2021-3-19}
  @begin{short}
    Specifies which corner a child widget should be placed in when packed into
    a @class{gtk-scrolled-window} widget.
  @end{short}
  This is effectively the opposite of where the scroll bars are placed.
  @begin{pre}
(define-g-enum \"GtkCornerType\" gtk-corner-type
  (:export t
   :type-initializer \"gtk_corner_type_get_type\")
  (:top-left 0)
  (:bottom-left 1)
  (:top-right 2)
  (:bottom-right 3))
  @end{pre}
  @begin[code]{table}
    @entry[:top-left]{Place the scrollbars on the right and bottom of the
      widget (default behaviour).}
    @entry[:bottom-left]{Place the scrollbars on the top and right of the
      widget.}
    @entry[:top-right]{Place the scrollbars on the left and bottom of the
      widget.}
    @entry[:bottom-right]{Place the scrollbars on the top and left of the
      widget.}
  @end{table}
  @see-class{gtk-scrolled-window}")

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
 "@version{*2021-3-14}
  @begin{short}
    The @sym{gtk-scrolled-window} widget is a container that accepts a single
    child widget, makes that child scrollable using either internally added
    scrollbars or externally associated adjustments, and optionally draws a
    frame around the child.
  @end{short}

  @image[scrolledwindow]{}

  Widgets with native scrolling support, i.e. those whose classes implement the
  @class{gtk-scrollable} interface, are added directly. For other types of
  widgets, the @class{gtk-viewport} class acts as an adaptor, giving
  scrollability to other widgets. The @sym{gtk-scrolled-window} widgets
  implementation of the function @fun{gtk-container-add} intelligently accounts
  for whether or not the added child is a @class{gtk-scrollable} widget. If it
  is not, the @sym{gtk-scrolled-window} widget wraps the child in a
  @class{gtk-viewport} widget and adds that for you. Therefore, you can just
  add any child widget and not worry about the details.

  If the function @fun{gtk-container-add} has added a @class{gtk-viewport}
  widget for you, you can remove both your added child widget from the
  @class{gtk-viewport} widget, and the @class{gtk-viewport} widget from the
  @sym{gtk-scrolled-window} widget, like this:
  @begin{pre}
(let ((scrolled-window (make-instance 'gtk-scrolled-window))
      (child-widget (make-instance 'gtk-button)))

  ;; GtkButton is not a GtkScrollable, so GtkScrolledWindow will automatically
  ;; add a GtkViewport.
  (gtk-container-add scrolled-window child-widget)

  ;; Either of these will result in child-widget being unparented:
  (gtk-container-remove scrolled-window child-widget)
  ;; or
  (gtk-container-remove scrolled-window (gtk-bin-child scrolled-window))
  ... )
  @end{pre}
  Unless the @code{hscrollbar-policy} and @code{vscrollbar-policy} properties
  are @code{:never} or @code{:external}, the @class{gtk-scrolled-window} widget
  adds internal @class{gtk-scrollbar} widgets around its child. The scroll
  position of the child, and if applicable the scrollbars, is controlled by the
  @code{hadjustment} and @code{vadjustment} properties that are associated with
  the @sym{gtk-scrolled-window} widget. See the docs on the
  @class{gtk-scrollbar} widget for the details, but note that the
  @code{step-increment} and @code{page-increment} properties are only effective
  if the policy causes scrollbars to be present.

  If a @sym{gtk-scrolled-window} widget does not behave quite as you would like,
  or does not have exactly the right layout, it is very possible to set up your
  own scrolling with the @class{gtk-scrollbar} widget and for example a
  @class{gtk-grid} widget.

  @subheading{Touch support}
  The @sym{gtk-scrolled-window} widget has built-in support for touch devices.
  When a touchscreen is used, swiping will move the scrolled window, and will
  expose 'kinetic' behavior. This can be turned off with the
  @code{kinetic-scrolling} property if it is undesired.

  The @sym{gtk-scrolled-window} widget also displays visual 'overshoot'
  indication when the content is pulled beyond the end, and this situation can
  be captured with the @code{edge-overshot} signal.

  If no mouse device is present, the scrollbars will overlayed as narrow,
  auto-hiding indicators over the content. If traditional scrollbars are
  desired although no mouse is present, this behaviour can be turned off with
  the @code{overlay-scrolling} property.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-scrolled-window} widget has a main CSS node with name
    @code{scrolledwindow}. It uses subnodes with names @code{overshoot} and
    @code{undershoot} to draw the overflow and underflow indications. These
    nodes get the @code{.left}, @code{.right}, @code{.top} or @code{.bottom}
    style class added depending on where the indication is drawn.

    The @sym{gtk-scrolled-window} widget also sets the positional style classes
    @code{.left}, @code{.right}, @code{.top}, @code{.bottom} and style classes
    related to overlay scrolling @code{.overlay-indicator}, @code{.dragging},
    @code{.hovering} on its scrollbars.

    If both scrollbars are visible, the area where they meet is drawn with a
    subnode named @code{junction}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[scrollbar-spacing]{entry}
        The @code{scrollbar-spacing} style property of type @code{:int} (Read)
        @br{}
        Number of pixels between the scrollbars and the scrolled window. @br{}
        Allowed values: >= 0 @br{}
        Default value: 3
      @end{entry}
      @begin[scrollbars-within-bevel]{entry}
        The @code{scrollbars-within-bevel} style property of type
        @code{:boolean} (Read) @br{}
        Whether to place scrollbars within the scrolled window's bevel. @br{}
        @em{Warning:} The @code{scrollbars-within-bevel} style property has
        been deprecated since version 3.20 and should not be used in
        newly written code. The value of this style property is ignored. @br{}
        Default value: @em{false}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"edge-overshot\" signal}
      @begin{pre}
 lambda (window pos)    : Run Last
      @end{pre}
      The signal is emitted whenever user initiated scrolling makes the scrolled
      window firmly surpass, i.e. with some edge resistance, the lower or upper
      limits defined by the adjustment in that orientation. A similar behavior
      without edge resistance is provided by the \"edge-reached\" signal.
      Note: The @arg{pos} argument is LTR/RTL aware, so callers should be aware
      too if intending to provide behavior on horizontal edges.
      @begin[code]{table}
        @entry[window]{The @sym{gtk-scrolled-window} widget which received the
          signal.}
        @entry[pos]{Edge side as a value of the @symbol{gtk-position-type}
          enumeration that was hit.}
      @end{table}
    @subheading{The \"edge-reached\" signal}
      @begin{short}
 lambda (window pos)    : Run Last
      @end{short}
      The signal is emitted whenever user-initiated scrolling makes the scrolled
      window exactly reach the lower or upper limits defined by the adjustment
      in that orientation. A similar behavior with edge resistance is provided
      by the \"edge-overshot\" signal.
      Note: The @arg{pos} argument is LTR/RTL aware, so callers should be aware
      too if intending to provide behavior on horizontal edges.
      @begin[code]{table}
        @entry[window]{The @sym{gtk-scrolled-window} widget which received the
          signal.}
        @entry[pos]{Edge side as a value of the @symbol{gtk-position-type}
          enumeration that was hit.}
      @end{table}
    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
 lambda (window direction)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when focus is moved
      away from the scrolled window by a keybinding. The \"move-focus\" signal
      is emitted with the @arg{direction} value on this scrolled windows
      toplevel parent in the container hierarchy. The default bindings for this
      signal are the @kbd{Tab+Ctrl} and @kbd{Tab+Ctrl+Shift} keys.
      @begin[code]{table}
        @entry[window]{The @sym{gtk-scrolled-window} widget which received the
          signal.}
        @entry[direction]{Either the @code{:tab-forward} or @code{:tab-backward}
          value of the @symbol{gtk-direction-type} enumeration.}
      @end{table}
      @subheading{The \"scroll-child\" signal}
        @begin{pre}
 lambda (window scroll horizontal)    : Action
        @end{pre}
        The signal is a keybinding signal which gets emitted when a keybinding
        that scrolls is pressed. The horizontal or vertical adjustment is
        updated which triggers a signal that the scrolled windows child may
        listen to and scroll itself.
        @begin[code]{table}
          @entry[window]{The @sym{gtk-scrolled-window} widget which received
            the signal.}
          @entry[scroll]{A value of the @symbol{gtk-scroll-type} enumeration
            describing how much to scroll.}
          @entry[horizontal]{A boolean whether the keybinding scrolls the child
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
  The adjustment for the horizontal position.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-hadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-hadjustment 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-hadjustment object) => hadjustment}
  @syntax[]{(setf (gtk-scrolled-window-hadjustment object) hadjustment)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[hadjustment]{a @class{gtk-adjustment} object with the horizontal
    scroll adjustment}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{hadjustment} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-hadjustment} returns the
  horizontal scrollbar's adjustment, used to connect the horizontal scrollbar
  to the child widget's horizontal scroll functionality. The slot access
  function @sym{(setf gtk-scrolled-window-hadjustment)} sets the adjustment for
  the horizontal scrollbar.
  @see-class{gtk-scrolled-window}
  @see-class{gtk-adjustment}
  @see-function{gtk-scrolled-window-vadjustment}")

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
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-hscrollbar-policy object) => policy}
  @syntax[]{(setf (gtk-scrolled-window-hscrollbar-policy object) policy)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[policy]{a value of the @symbol{gtk-policy-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{hscrollbar-policy} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-policy-type}
  @see-function{gtk-scrolled-window-vscrollbar-policy}")

;;; --- gtk-scrolled-window-kinetic-scrolling ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "kinetic-scrolling"
                                               'gtk-scrolled-window) 't)
 "The @code{kinetic-scrolling} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether kinetic scrolling is enabled or not. Kinetic scrolling only applies
  to input devices of type @code{:touchscreen}. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-kinetic-scrolling
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-kinetic-scrolling 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-kinetic-scrolling object) => scrolling}
  @syntax[]{(setf (gtk-scrolled-window-kinetic-scrolling object) scrolling)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[scrolling]{@em{true} to enable kinetic scrolling}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{kinetic-scrolling} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-kinetic-scrolling} returns
  the specified kinetic scrolling behavior. The slot access function
  @sym{(setf gtk-scrolled-window-kinetic-scrolling)} turns kinetic scrolling on
  or off.

  Kinetic scrolling only applies to input devices of type @code{:touchscreen}.
  @see-class{gtk-scrolled-window}
  @see-symbol{gdk-input-source}")

;;; --- gtk-scrolled-window-max-content-height ---------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-content-height"
                                               'gtk-scrolled-window) 't)
 "The @code{max-content-height} property of type @code{:int} (Read / Write)
  @br{}
  The maximum content height of the scrolled window, or -1 if not set.
  Since 3.22 @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-max-content-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-max-content-height 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-max-content-height object) => height}
  @syntax[]{(setf (gtk-scrolled-window-max-content-height object) height)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[height]{an integer with the maximum content height}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{max-content-height} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-max-content-height} returns
  the maximum content height that the scrolled window should keep visible. The
  scrolled window will grow up to this height before it starts scrolling the
  content. The slot access function
  @sym{(setf gtk-scrolled-window-max-content-height)} sets the maximum height.

  It is a programming error to set the maximum content height to a value
  smaller than the @slot[gtk-scrolled-window]{min-content-height} value.

  Since 3.22
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-min-content-height}")

;;; --- gtk-scrolled-window-max-content-width ----------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-content-width"
                                               'gtk-scrolled-window) 't)
 "The @code{max-content-width} property of type @code{:int}
  (Read / Write] @br{}
  The maximum content width of the scrolled window, or -1 if not set.
  Since 3.22 @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-max-content-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-max-content-width 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-max-content-width object) => width}
  @syntax[]{(setf (gtk-scrolled-window-max-content-width object) width)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[width]{an integer with the maximum content width}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{max-content-width} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-max-content-width} returns
  the maximum content width that the scrolled window should keep visible. The
  scrolled window will grow up to this width before it starts scrolling the
  content. The slot access function
  @sym{(setf gtk-scrolled-window-max-content-width)} sets the maximum width.

  It is a programming error to set the maximum content width to a value
  smaller than the @slot[gtk-scrolled-window]{min-content-width} value.

  Since 3.22
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-min-content-width}")

;;; --- gtk-scrolled-window-min-content-height ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-height"
                                               'gtk-scrolled-window) 't)
 "The @code{min-content-height} property of type @code{:int} (Read / Write)
  @br{}
  The minimum content height of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-height 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-min-content-height object) => height}
  @syntax[]{(setf (gtk-scrolled-window-min-content-height object) height)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[height]{an integer with the minimal content height}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{min-content-height} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-min-content-height} gets
  the minimal content height of the scrolled window that the scrolled window
  should keep visible. Note that this can and, usually will, be smaller than
  the minimum size of the content. The slot access function
  @sym{(setf gtk-scrolled-window-min-content-heigth)} sets the minimum height.
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-max-content-height}")

;;; --- gtk-scrolled-window-min-content-width ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-content-width"
                                               'gtk-scrolled-window) 't)
 "The @code{min-content-width} property of type @code{:int} (Read / Write) @br{}
  The minimum content width of the scrolled window, or -1 if not set. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-min-content-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-min-content-width 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-min-content-width object) => width}
  @syntax[]{(setf (gtk-scrolled-window-min-content-width object) width)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[width]{an integer with the minimal content width}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{min-content-width} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-min-content-width} gets the
  minimum content width of the scrolled window that the scrolled window should
  keep visible. Note that this can and, usually will, be smaller than the
  minimum size of the content. The slot access function
  @sym{(setf gtk-scrolled-window-min-content-width)} sets the minimum width.
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-max-content-width}")

;;; --- gtk-scrolled-window-overlay-scrolling ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "overlay-scrolling"
                                               'gtk-scrolled-window) 't)
 "The @code{overlay-scrolling} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether overlay scrolling is enabled or not. If it is, the scrollbars are only
  added as traditional widgets when a mouse is present. Otherwise, they are
  overlayed on top of the content, as narrow indicators. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-overlay-scrolling
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-overlay-scrolling 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-overlay-scrolling object) => scrolling}
  @syntax[]{(setf (gtk-scrolled-window-overlay-scrolling object) scrolling)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[scrolling]{a boolean whether to enable overly scrolling}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{overlay-scrolling} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-overlay-scrolling} returns
  whether overlay scrolling is enabled for this scrolled window. The slot
  access function @sym{(setf gtk-scrolled-window-overlay-scrolling)} enables or
  disables overlay scrolling.
  @see-class{gtk-scrolled-window}")

;;; --- gtk-scrolled-window-propagate-natural-height ---------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "propagate-natural-height"
                                               'gtk-scrolled-window) 't)
 "The @code{propagate-natural-height} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural height of the child should be calculated and propagated
  through the scrolled window’s requested natural height. This is useful in
  cases where an attempt should be made to allocate exactly enough space for
  the natural size of the child. Since 3.22 @br{}
  Default value: @em{false}")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-propagate-natural-height
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-propagate-natural-height 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-propagate-natural-height object) => propagate}
  @syntax[]{(setf (gtk-scrolled-window-propagate-natural-height object) propagate)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[propagate]{a boolean whether to propagate natural height}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{propagate-natural-height} slot
    of the @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-propagate-natural-height}
  reports whether the natural height of the child will be calculated and
  propagated through the scrolled window’s requested natural height. The slot
  access function @sym{(setf gtk-scrolled-window-propagate-natural-height)}
  sets the property.

  Since 3.22
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-propagate-natural-width}")

;;; --- gtk-scrolled-window-propagate-natural-width ----------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "propagate-natural-width"
                                               'gtk-scrolled-window) 't)
 "The @code{propagate-natural-width} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the natural width of the child should be calculated and propagated
  through the scrolled window’s requested natural width. This is useful in cases
  where an attempt should be made to allocate exactly enough space for the
  natural size of the child. Since 3.22 @br{}
  Default value: @em{false}")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-scrolled-window-propagate-natural-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-propagate-natural-width 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-propagate-natural-width object) => propagate}
  @syntax[]{(setf (gtk-scrolled-window-propagate-natural-width object) propagate)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[propagate]{a boolean whether to propagate natural width}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{propagate-natural-width} slot
    of the @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-propagate-natural-width}
  reports whether the natural width of the child will be calculated and
  propagated through the scrolled window’s requested natural width. The slot
  access function @sym{(setf gtk-scrolled-window-propagate-natural-width)} sets
  the property.

  Since 3.22
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-propagate-natural-height}")

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
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-shadow-type object) => shadow-type}
  @syntax[]{(setf (gtk-scrolled-window-shadow-type object) shadow-type)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[shadow-type]{a value of the @class{gtk-shadow-type} enumeration
    with the kind of shadow to draw around scrolled window contents}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{shadow-type} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-shadow-type} gets the
  shadow type of the scrolled window. The slot access function
  @sym{(setf gtk-scrolled-window-shadow-type)} sets the shadow type.
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-shadow-type}")

;;; --- gtk-scrolled-window-vadjustment ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vadjustment"
                                               'gtk-scrolled-window) 't)
 "The @code{vadjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The adjustment for the vertical position.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-vadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-vadjustment 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-vadjustment object) => vadjustment}
  @syntax[]{(setf (gtk-scrolled-window-vadjustment object) vadjustment)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[vadjustment]{a @class{gtk-adjustment} object with the vertical
    scroll adjustment}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{vadjustment} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  The slot access function @sym{gtk-scrolled-window-vadjustment} returns the
  vertical scrollbar's adjustment, used to connect the vertical scrollbar to
  the child widget's vertical scroll functionality. The slot access function
  @sym{(setf gtk-scrolled-window-vadjustment)} sets the adjusment for the
  vertical scrollbar.
  @see-class{gtk-scrolled-window}
  @see-class{gtk-adjustment}
  @see-function{gtk-scrolled-window-hadjustment}")

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
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-vscrollbar-policy object) => policy}
  @syntax[]{(setf (gtk-scrolled-window-vscrollbar-policy object) policy)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[policy]{a value of the @symbol{gtk-policy-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{vscrollbar-policy} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-policy-type}
  @see-function{gtk-scrolled-window-hscrollbar-policy}")

;;; --- gtk-scrolled-window-window-placement -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement"
                                               'gtk-scrolled-window) 't)
 "The @code{window-placement} property of type @symbol{gtk-corner-type}
  (Read / Write) @br{}
  Where the contents are located with respect to the scrollbars. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-window-placement object) => placement}
  @syntax[]{(setf (gtk-scrolled-window-window-placement object) placement)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[placement]{a value of the @symbol{gtk-corner-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{window-placement} slot of the
    @class{gtk-scrolled-window} class.
  @end{short}

  Where the contents are located with respect to the scrollbars.
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-corner-type}
  @see-function{gtk-scrolled-window-placement}")

;;; --- gtk-scrolled-window-window-placement-set -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window-placement-set"
                                               'gtk-scrolled-window) 't)
 "The @code{window-placement-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether @code{window-placement} should be used to determine the location
  of the contents with respect to the scrollbars. @br{}
  @em{Warning:} The @code{window-placement-set} property has been deprecated
  since version 3.10 and should not be used in newly written code. This value
  is ignored and the value of the @code{window-placement} property is always
  honored. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrolled-window-window-placement-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrolled-window-window-placement-set 'function)
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-window-placement-set object) => placement-set}
  @syntax[]{(setf (gtk-scrolled-window-window-placement-set object) placement-set)}
  @argument[object]{a @class{gtk-scrolled-window} widget}
  @argument[placement]{a boolean whether @code{window-placement} should be used}
  @begin{short}
    Accessor of the @slot[gtk-scrolled-window]{window-placement-set} slot of
    the @class{gtk-scrolled-window} class.
  @end{short}

  Whether the @slot[gtk-scrolled-window]{window-placement} property should be
  used to determine the location of the contents with respect to the scrollbars.
  @begin[Warning]{dictionary}
    The @code{window-placement-set} property has been deprecated since version
    3.10 and should not be used in newly written code. This value is ignored
    and the @slot[gtk-scrolled-window]{window-placement} property is always
    honored.
  @end{dictionary}
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-window-placement}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrolled-window-new))

(defun gtk-scrolled-window-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @argument[hadjustment]{a @class{gtk-adjustment} object with the horizontal
    adjustment}
  @argument[vadjustment]{a @class{gtk-adjustment} object with the vertical
    adjustment}
  @return{A new @class{gtk-scrolled-window} widget.}
  @short{Creates a new scrolled window.}

  The two optional arguments are the scrolled window's adjustments. These will
  be shared with the scrollbars and the child widget to keep the bars in sync
  with the child. Usually you want to use the default values for the
  adjustments, which will cause the scrolled window to create them for you.
  @see-class{gtk-scrolled-window}
  @see-class{gtk-adjustment}"
  (make-instance 'gtk-scrolled-window
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-scrolled-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_hscrollbar () -> gtk-srolled-window-hscrollbar
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_get_hscrollbar" gtk-scrolled-window-hscrollbar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @begin{return}
    The horizontal @class{gtk-scrollbar} widget of the scrolled window, or
    @code{nil} if it does not have one.
  @end{return}
  @short{Returns the horizontal scrollbar of the scrolled window.}
  @see-class{gtk-scrolled-window}
  @see-class{gtk-scrollbar}
  @see-function{gtk-scrolled-window-vscrollbar}"
  (window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-hscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_vscrollbar () -> gtk-scrolled-window-vscrollbar
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_get_vscrollbar" gtk-scrolled-window-vscrollbar)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @begin{return}
    The vertical @class{gtk-scrollbar} widget of the scrolled window, or
    @code{nil} if it does not have one.
  @end{return}
  @short{Returns the vertical scrollbar of the scrolled window.}
  @see-class{gtk-scrolled-window}
  @see-class{gtk-scrollbar}
  @see-function{gtk-scrolled-window-hscrollbar}"
  (window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-vscrollbar)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_policy ()
;;; gtk_scrolled_window_get_policy () -> gtk-scrolled-window-policy
;;; ----------------------------------------------------------------------------

(defun (setf gtk-scrolled-window-policy) (policy window)
  (destructuring-bind (hscroll-policy vscroll-policy) policy
    (foreign-funcall "gtk_scrolled_window_set_policy"
                     (g-object gtk-scrolled-window) window
                     gtk-policy-type hscroll-policy
                     gtk-policy-type vscroll-policy
                     :void)
    (values hscroll-policy vscroll-policy)))

(defun gtk-scrolled-window-policy (window)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-14}
  @syntax[]{(gtk-scrolled-window-policy window) => hscroll-policy, vscroll-policy}
  @syntax[]{(setf (gtk-scrolled-window-policy window) (list hscroll-policy vscroll-policy))}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @argument[hscroll-policy]{a value of the @symbol{gtk-policy-type}
    enumeration for the policy for horizontal bar}
  @argument[vscroll-policy]{a value of the @symbol{gtk-policy-type}
    enumeration for the policy for vertical bar}
  @begin{short}
    Accessor of the policy values of the srolled window.
  @end{short}

  The function @sym{gtk-scrolled-window-policy} retrieves the current policy
  values for the horizontal and vertical scrollbars. The function
  @sym{(setf gtk-scrolled-window-policy)} sets the scrollbar policy.

  The policy determines when the scrollbar should appear. It is a value from
  the @symbol{gtk-policy-type} enumeration. If @code{:always}, the scrollbar is
  always present; if @code{:never}, the scrollbar is never present; if
  @code{:automatic}, the scrollbar is present only if needed, that is, if
  the slider part of the bar would be smaller than the trough - the display is
  larger than the page size.
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-policy-type}"
  (values (gtk-scrolled-window-hscrollbar-policy window)
          (gtk-scrolled-window-vscrollbar-policy window)))

(export 'gtk-scrolled-window-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_add_with_viewport ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_add_with_viewport"
           gtk-scrolled-window-add-with-viewport) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @argument[child]{the @class{gtk-widget} object you want to scroll}
  @begin{short}
    Used to add children without native scrolling capabilities.
  @end{short}
  This is simply a convenience function. It is equivalent to adding the
  unscrollable child to a viewport, then adding the viewport to the scrolled
  window. If a child has native scrolling, use the function
  @fun{gtk-container-add} instead of this function.

  The viewport scrolls the child by moving its @class{gdk-window} object, and
  takes the size of the child to be the size of its toplevel @class{gdk-window}
  object. This will be very wrong for most widgets that support native
  scrolling. For example, if you add a widget such as @class{gtk-tree-view}
  widget with a viewport, the whole widget will scroll, including the column
  headings. Thus, widgets with native scrolling support should not be used with
  the @class{gtk-viewport} proxy.

  A widget supports scrolling natively if it implements the
  @class{gtk-scrollable} interface.
  @begin[Warning]{dictionary}
    The function @sym{gtk-scrolled-window-add-with-viewport} has been deprecated
    since version 3.8 and should not be used in newly written code. The function
    @fun{gtk-container-add} will automatically add a @class{gtk-viewport} widget
    if the child does not implement the @class{gtk-scrollable} interface.
  @end{dictionary}
  @see-class{gtk-scrolled-window}
  @see-class{gtk-widget}
  @see-class{gtk-scrollable}
  @see-class{gdk-window}
  @see-class{gtk-viewport}
  @see-function{gtk-container-add}"
  (window (g-object gtk-scrolled-window))
  (child (g-object gtk-widget)))

(export 'gtk-scrolled-window-add-with-viewport)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_set_placement ()
;;; gtk_scrolled_window_get_placement () -> gtk-scrolled-window-placement
;;; ----------------------------------------------------------------------------

;; gtk_scrolled_window_set_placement sets the new value for the placement
;; and than updates the scrolled window

(defun (setf gtk-scrolled-window-placement) (placement scrolled-window)
  (foreign-funcall "gtk_scrolled_window_set_placement"
                   (g-object gtk-scrolled-window) scrolled-window
                   gtk-corner-type placement
                   :void)
  placement)

(defun gtk-scrolled-window-placement (window)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-placement window) => placement}
  @syntax[]{(setf (gtk-scrolled-window-placement window) placement)}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @argument[placement]{a value of the @symbol{gtk-corner-type} enumeration with
    the position of the child window}
  @begin{short}
    Accessor of the placement of the contents with respect to the scrollbars
    for the scrolled window.
  @end{short}

  The function @sym{gtk-scrolled-window-placement} gets the placement of the
  contents with respect to the scrollbars for the scrolled window. The function
  @sym{(setf gtk-scrolled-window-placement)} sets the placement of the contents.

  The default is @code{:left}, meaning the child is in the top left, with the
  scrollbars underneath and to the right. Other values in the
  @symbol{gtk-corner-type} enumeration are @code{:top-right},
  @code{:bottom-left}, and @code{:bottom-right}.

  See also the function @fun{gtk-scrolled-window-unset-placement}.
  @begin[Note]{dictionary}
    In contrast to the slot access function
    @fun{gtk-scrolled-window-window-placement} the function
    @sym{gtk-scrolled-window-placement} updates the scrolled window after
    setting the new value for the window placement.
  @end{dictionary}
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-unset-placement}
  @see-function{gtk-scrolled-window-window-placement}"
  (gtk-scrolled-window-window-placement window))

(export 'gtk-scrolled-window-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_unset_placement ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scrolled_window_unset_placement"
           gtk-scrolled-window-unset-placement) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @begin{short}
    Unsets the placement of the contents with respect to the scrollbars for the
    scrolled window.
  @end{short}
  If no window placement is set for a scrolled window, it defaults to
  the value @code{:left} of the @symbol{gtk-corner-type} enumeration.

  See also the function @fun{gtk-scrolled-window-placement}.
  @see-class{gtk-scrolled-window}
  @see-symbol{gtk-corner-type}
  @see-function{gtk-scrolled-window-placement}"
  (window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-unset-placement)

;;; ----------------------------------------------------------------------------
;;; gtk_scrolled_window_get_capture_button_press ()
;;; gtk_scrolled_window_set_capture_button_press ()
;;; -> gtk-scrolled-window-capture-button-press
;;; ----------------------------------------------------------------------------

(defun (setf gtk-scrolled-window-capture-button-press) (capture window)
  (foreign-funcall "gtk_scrolled_window_set_capture_button_press"
                   (g-object gtk-scrolled-window) window
                   :boolean capture
                   :void)
  capture)

(defcfun ("gtk_scrolled_window_get_capture_button_press"
           gtk-scrolled-window-capture-button-press) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-19}
  @syntax[]{(gtk-scrolled-window-capture-button-press window) => capture}
  @syntax[]{(setf (gtk-scrolled-window-capture-button-press window) capture)}
  @argument[window]{a @class{gtk-scrolled-window} widget}
  @argument[capture]{@em{true} to capture button presses}
  @begin{short}
    Whether button presses are captured during kinetic scrolling.
  @end{short}

  The function @sym{gtk-scrolled-window-capture-button-press} returns whether
  button presses are captured during kinetic scrolling. The function
  @sym{(setf gtk-scrolled-window-capture-button-press)} changes the behaviour
  of the scrolled window with respect to the initial event that possibly starts
  kinetic scrolling.

  When @arg{capture} is set to @em{true}, the event is captured by the scrolled
  window, and then later replayed if it is meant to go to the child widget.

  This should be enabled if any child widgets perform non-reversible actions
  on \"button-press-event\" signals. If they do not, and handle additionally
  handle \"grab-broken-event\", it might be better to set
  @arg{capture-button-press} to @em{false}.

  This setting only has an effect if kinetic scrolling is enabled.
  @see-class{gtk-scrolled-window}
  @see-function{gtk-scrolled-window-kinetic-scrolling}"
  (window (g-object gtk-scrolled-window)))

(export 'gtk-scrolled-window-capture-button-press)

;;; --- End of file gtk.scrolled-window.lisp -----------------------------------

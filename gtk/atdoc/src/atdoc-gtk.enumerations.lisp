;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.enumerations.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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

;;; --- gtk-accel-flags --------------------------------------------------------

(setf (gethash 'gtk-accel-flags atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-accel-flags atdoc:*external-symbols*)
 "@version{2013-1-13}
  @short{}
  @begin{pre}
(define-g-flags \"GtkAccelFlags\" gtk-accel-flags
  (:export t
   :type-initializer \"gtk_accel_flags_get_type\")
  (:visible 1)
  (:locked 2)
  (:mask 7))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; enum GtkArrowPlacement
;;;
;;; typedef enum {
;;;   GTK_ARROWS_BOTH,
;;;   GTK_ARROWS_START,
;;;   GTK_ARROWS_END
;;; } GtkArrowPlacement;
;;;
;;; Used to specify the placement of scroll arrows in scrolling menus.
;;;
;;; GTK_ARROWS_BOTH
;;;     Place one arrow on each end of the menu.
;;;
;;; GTK_ARROWS_START
;;;     Place both arrows at the top of the menu.
;;;
;;; GTK_ARROWS_END
;;;     Place both arrows at the bottom of the menu.
;;; ----------------------------------------------------------------------------

;;; --- gtk-arrow-type ---------------------------------------------------------

(setf (gethash 'gtk-arrow-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-arrow-type atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Used to indicate the direction in which a @class{gtk-arrow} should point.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkArrowType\" gtk-arrow-type
  (:export t
   :type-initializer \"gtk_arrow_type_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{Represents an upward pointing arrow.}
    @entry[:down]{Represents a downward pointing arrow.}
    @entry[:left]{Represents a left pointing arrow.}
    @entry[:right]{Represents a right pointing arrow.}
    @entry[:none]{No arrow. Since 2.10.}
  @end{table}")

;;; --- gtk-attach-options -----------------------------------------------------

(setf (gethash 'gtk-attach-options atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-attach-options atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Denotes the expansion properties that a widget will have when it (or its
    parent) is resized.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkAttachOptions\" gtk-attach-options
  (:export t
   :type-initializer \"gtk_attach_options_get_type\")
  (:expand 1)
  (:shrink 2)
  (:fill 4))
  @end{pre}
  @begin[code]{table}
    @entry[:expand]{the widget should expand to take up any extra space in its
      container that has been allocated.}
    @entry[:shrink]{the widget should shrink as and when possible.}
    @entry[:fill]{the widget should fill the space allocated to it.}
  @end{table}")

;;; --- gtk-button-box-style ---------------------------------------------------

(setf (gethash 'gtk-button-box-style atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-button-box-style atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Used to dictate the style that a @class{gtk-button-box} uses to layout the
    buttons it contains. (See also: @class{gtk-vbutton-box} and
    @class{gtk-hbutton-box}).
  @end{short}
  @begin{pre}
(define-g-enum \"GtkButtonBoxStyle\" gtk-button-box-style
  (:export t
   :type-initializer \"gtk_button_box_style_get_type\")
  (:default-style 0)
  (:spread 1)
  (:edge 2)
  (:start 3)
  (:end 4)
  (:center 5))
  @end{pre}
  @begin[code]{table}
    @entry[:spread]{Buttons are evenly spread across the box.}
    @entry[:edge]{Buttons are placed at the edges of the box.}
    @entry[:start]{Buttons are grouped towards the start of the box, (on the
      left for a HBox, or the top for a VBox).}
    @entry[:end]{Buttons are grouped towards the end of the box, (on the right
      for a HBox, or the bottom for a VBox).}
    @entry[:center]{Buttons are centered in the box. Since 2.12.}
  @end{table}")

;;; --- gtk-corner-type --------------------------------------------------------

(setf (gethash 'gtk-corner-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-corner-type atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Specifies which corner a child widget should be placed in when packed into a
    @class{gtk-scrolled-window}. This is effectively the opposite of where the
    scroll bars are placed.
  @end{short}
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
    @entry[:top-left]{Place the scrollbars on the right and bottom of the widget
      (default behaviour).}
    @entry[:bottom-left]{Place the scrollbars on the top and right of the
      widget.}
    @entry[:top-right]{Place the scrollbars on the left and bottom of the
      widget.}
    @entry[:bottom-right]{Place the scrollbars on the top and left of the
      widget.}
  @end{table}")

;;; --- gtk-delete-type --------------------------------------------------------

(setf (gethash 'gtk-delete-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-delete-type atdoc:*external-symbols*)
 "@version{2013-1-18}
  @short{}
  @begin{pre}
(define-g-enum \"GtkDeleteType\" gtk-delete-type
  (:export t
   :type-initializer \"gtk_delete_type_get_type\")
  (:chars 0)
  (:word-ends 1)
  (:words 2)
  (:display-lines 3)
  (:display-line-ends 4)
  (:paragraph-ends 5)
  (:paragraphs 6)
  (:whitespace 7))
  @end{pre}")

;;; --- gtk-direction-type -----------------------------------------------------

(setf (gethash 'gtk-direction-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-direction-type atdoc:*external-symbols*)
 "@version{2013-1-18}
  @short{}
  @begin{pre}
(define-g-enum \"GtkDirectionType\" gtk-direction-type
  (:export t
   :type-initializer \"gtk_direction_type_get_type\")
  (:tab-forward 0)
  (:tab-backward 1)
  (:up 2)
  (:down 3)
  (:left 4)
  (:right 5))
  @end{pre}")

;;; --- gtk-expander-style -----------------------------------------------------

(setf (gethash 'gtk-expander-style atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-expander-style atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Used to specify the style of the expanders drawn by a @class{gtk-tree-view}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkExpanderStyle\" gtk-expander-style
  (:export t
   :type-initializer \"gtk_expander_style_get_type\")
  (:collapsed 0)
  (:semi-collapsed 1)
  (:semi-expanded 2)
  (:expanded 3))
  @end{pre}
  @begin[code]{table}
    @entry[:collapsed]{The style used for a collapsed subtree.}
    @entry[:semi-collapsed]{Intermediate style used during animation.}
    @entry[:semi-expanded]{Intermediate style used during animation.}
    @entry[:expanded]{The style used for an expanded subtree.}
  @end{table}")

;;; --- gtk-im-preedit-style ---------------------------------------------------

(setf (gethash 'gtk-im-preedit-style atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-im-preedit-style atdoc:*external-symbols*)
 "@version{2013-1-18}
  @short{}
  @begin{pre}
(define-g-enum \"GtkIMPreeditStyle\" gtk-im-preedit-style
  (:export t
   :type-initializer \"gtk_im_preedit_style_get_type\")
  (:nothing 0)
  (:callback 1)
  (:none 2))
  @end{pre}")

;;; --- gtk-im-status-style ----------------------------------------------------

(setf (gethash 'gtk-im-status-style atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-im-status-style atdoc:*external-symbols*)
 "@version{2013-1-18}
  @short{}
  @begin{pre}
(define-g-enum \"GtkIMStatusStyle\" gtk-im-status-style
  (:export t
   :type-initializer \"gtk_im_status_style_get_type\")
  (:nothing 0)
  (:callback 1)
  (:none 2))
  @end{pre}")

;;; --- gtk-justification ------------------------------------------------------

(setf (gethash 'gtk-justification atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-justification atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Used for justifying the text inside a GtkLabel widget. (See also
    @class{gtk-alignment}).
  @end{short}
  @begin{pre}
(define-g-enum \"GtkJustification\" gtk-justification
  (:export t
   :type-initializer \"gtk_justification_get_type\")
  (:left 0)
  (:right 1)
  (:center 2)
  (:fill 3))
  @end{pre}
  @begin[code]{table}
    @entry[:left]{The text is placed at the left edge of the label.}
    @entry[:right]{The text is placed at the right edge of the label.}
    @entry[:center]{The text is placed in the center of the label.}
    @entry[:fill]{The text is placed is distributed across the label.}
  @end{table}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GtkMovementStep
;;;
;;; typedef enum {
;;;   GTK_MOVEMENT_LOGICAL_POSITIONS,
;;;   GTK_MOVEMENT_VISUAL_POSITIONS,
;;;   GTK_MOVEMENT_WORDS,
;;;   GTK_MOVEMENT_DISPLAY_LINES,
;;;   GTK_MOVEMENT_DISPLAY_LINE_ENDS,
;;;   GTK_MOVEMENT_PARAGRAPHS,
;;;   GTK_MOVEMENT_PARAGRAPH_ENDS,
;;;   GTK_MOVEMENT_PAGES,
;;;   GTK_MOVEMENT_BUFFER_ENDS,
;;;   GTK_MOVEMENT_HORIZONTAL_PAGES
;;; } GtkMovementStep;
;;;
;;; GTK_MOVEMENT_LOGICAL_POSITIONS
;;;     Move forward or back by graphemes
;;;
;;; GTK_MOVEMENT_VISUAL_POSITIONS
;;;     Move left or right by graphemes
;;;
;;; GTK_MOVEMENT_WORDS
;;;     Move forward or back by words
;;;
;;; GTK_MOVEMENT_DISPLAY_LINES
;;;     Move up or down lines (wrapped lines)
;;;
;;; GTK_MOVEMENT_DISPLAY_LINE_ENDS
;;;     Move to either end of a line
;;;
;;; GTK_MOVEMENT_PARAGRAPHS
;;;     Move up or down paragraphs (newline-ended lines)
;;;
;;; GTK_MOVEMENT_PARAGRAPH_ENDS
;;;     Move to either end of a paragraph
;;;
;;; GTK_MOVEMENT_PAGES
;;;     Move by pages
;;;
;;; GTK_MOVEMENT_BUFFER_ENDS
;;;     Move to ends of the buffer
;;;
;;; GTK_MOVEMENT_HORIZONTAL_PAGES
;;;     Move horizontally by pages
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMovementStep" gtk-movement-step
  (:export t
   :type-initializer "gtk_movement_step_get_type")
  (:logical-positions 0)
  (:visual-positions 1)
  (:words 2)
  (:display-lines 3)
  (:display-line-ends 4)
  (:paragraphs 5)
  (:paragraph-ends 6)
  (:pages 7)
  (:buffer-ends 8)
  (:horizontal-pages 9))
|#

;;; --- gtk-orientation --------------------------------------------------------

(setf (gethash 'gtk-orientation atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-orientation atdoc:*external-symbols*)
 "@version{2013-1-18}
  @begin{short}
    Represents the orientation of widgets which can be switched between
    horizontal and vertical orientation on the fly, like @class{gtk-toolbar}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkOrientation\" gtk-orientation
  (:export t
   :type-initializer \"gtk_orientation_get_type\")
  (:horizontal 0)
  (:vertical 1))
  @end{pre}
  @begin[code]{table}
    @entry[:horizontal]{The widget is in horizontal orientation.}
    @entry[:vertical]{The widget is in vertical orientation.}
  @end{table}")

;;; --- gtk-pack-type ----------------------------------------------------------

(setf (gethash 'gtk-pack-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-pack-type atdoc:*external-symbols*)
 "@version{2013-1-20}
  @begin{short}
    Represents the packing location of a @class{gtk-box} children.
  @end{short}
  See also @class{gtk-vbox}, @class{gtk-hbox}, and @class{gtk-button-box}.
  @begin{pre}
(define-g-enum \"GtkPackType\" gtk-pack-type
  (:export t
   :type-initializer \"gtk_pack_type_get_type\")
  (:start 0)
  (:end 1))
  @end{pre}
  @begin[code]{table}
    @entry[:start]{The child is packed into the start of the box.}
    @entry[:end]{The child is packed into the end of the box.}
  @end{table}
  @see-class{gtk-box}
  @see-class{gtk-hbox}
  @see-class{gtk-vbox}
  @see-class{gtk-button-box}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GtkPathPriorityType
;;;
;;; typedef enum {
;;;   GTK_PATH_PRIO_LOWEST      = 0,
;;;   GTK_PATH_PRIO_GTK        = 4,
;;;   GTK_PATH_PRIO_APPLICATION = 8,
;;;   GTK_PATH_PRIO_THEME       = 10,
;;;   GTK_PATH_PRIO_RC          = 12,
;;;   GTK_PATH_PRIO_HIGHEST     = 15
;;; } GtkPathPriorityType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPathPriorityType" gtk-path-priority-type
  (:export t
   :type-initializer "gtk_path_priority_type_get_type")
  (:lowest 0)
  (:gtk 4)
  (:application 8)
  (:theme 10)
  (:rc 12)
  (:highest 15))

;;; ----------------------------------------------------------------------------
;;; enum GtkPathType
;;;
;;; typedef enum {
;;;   GTK_PATH_WIDGET,
;;;   GTK_PATH_WIDGET_CLASS,
;;;   GTK_PATH_CLASS
;;; } GtkPathType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPathType" gtk-path-type
  (:export t
   :type-initializer "gtk_path_type_get_type")
  (:widget 0)
  (:widget-class 1)
  (:class 2))

;;; ----------------------------------------------------------------------------
;;; enum GtkPolicyType
;;;
;;; typedef enum {
;;;   GTK_POLICY_ALWAYS,
;;;   GTK_POLICY_AUTOMATIC,
;;;   GTK_POLICY_NEVER
;;; } GtkPolicyType;
;;;
;;; Determines when a scroll bar will be visible.
;;;
;;; GTK_POLICY_ALWAYS
;;;     The scrollbar is always visible.
;;;
;;; GTK_POLICY_AUTOMATIC
;;;     The scrollbar will appear and disappear as necessary. For example, when
;;;     all of a GtkCList can not be seen.
;;;
;;; GTK_POLICY_NEVER
;;;     The scrollbar will never appear.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPolicyType" gtk-policy-type
  (:export t
   :type-initializer "gtk_policy_type_get_type")
  (:always 0)
  (:automatic 1)
  (:never 2))
|#

;;; --- gtk-position-type ------------------------------------------------------

(setf (gethash 'gtk-position-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-position-type atdoc:*external-symbols*)
 "@version{2013-1-9}
  @begin{short}
    Describes which edge of a widget a certain feature is positioned at, e. g.
    the tabs of a @class{gtk-notebook}, the handle of a @class{gtk-handle-box}
    or the label of a @class{gtk-scale}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPositionType\" gtk-position-type
  (:export t
   :type-initializer \"gtk_position_type_get_type\")
  (:left 0)
  (:right 1)
  (:top 2)
  (:bottom 3))
  @end{pre}
  @begin[code]{table}
    @entry[:left]{The feature is at the left edge.}
    @entry[:right]{The feature is at the right edge.}
    @entry[:top]{The feature is at the top edge.}
    @entry[:bottom]{The feature is at the bottom edge.}
  @end{table}")

;;; --- gtk-relief-style -------------------------------------------------------

(setf (gethash 'gtk-relief-style atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-relief-style atdoc:*external-symbols*)
 "@version{2013-1-9}
  @short{Indicated the relief to be drawn around a GtkButton.}
  @begin{pre}
(define-g-enum \"GtkReliefStyle\" gtk-relief-style
  (:export t
   :type-initializer \"gtk_relief_style_get_type\")
  (:normal 0)
  (:half 1)
  (:none 2))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Draw a normal relief.}
    @entry[:half]{A half relief.}
    @entry[:none]{No relief.}
  @end{table}")

;;; --- gtk-resize-mode --------------------------------------------------------

(setf (gethash 'gtk-resize-mode atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-resize-mode atdoc:*external-symbols*)
 "@version{2013-1-20}
  @short{}
  @begin{pre}
(define-g-enum \"GtkResizeMode\" gtk-resize-mode
  (:export t
   :type-initializer \"gtk_resize_mode_get_type\")
  (:parent 0)
  (:queue 1)
  (:immediate 2))
  @end{pre}
  @begin[code]{table}
    @entry[:parent]{Pass resize request to the parent}
    @entry[:queue]{Queue resizes on this widget}
    @entry[:immediate]{Resize immediately. Deprecated.}
  @end{table}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GtkScrollStep
;;;
;;; typedef enum {
;;;   GTK_SCROLL_STEPS,
;;;   GTK_SCROLL_PAGES,
;;;   GTK_SCROLL_ENDS,
;;;   GTK_SCROLL_HORIZONTAL_STEPS,
;;;   GTK_SCROLL_HORIZONTAL_PAGES,
;;;   GTK_SCROLL_HORIZONTAL_ENDS
;;; } GtkScrollStep;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkScrollStep" gtk-scroll-step
  (:export t
   :type-initializer "gtk_scroll_step_get_type")
  (:steps 0)
  (:pages 1)
  (:ends 2)
  (:horizontal-steps 3)
  (:horizontal-pages 4)
  (:horizontal-ends 5))

;;; ----------------------------------------------------------------------------
;;; enum GtkScrollType
;;;
;;; typedef enum {
;;;   GTK_SCROLL_NONE,
;;;   GTK_SCROLL_JUMP,
;;;   GTK_SCROLL_STEP_BACKWARD,
;;;   GTK_SCROLL_STEP_FORWARD,
;;;   GTK_SCROLL_PAGE_BACKWARD,
;;;   GTK_SCROLL_PAGE_FORWARD,
;;;   GTK_SCROLL_STEP_UP,
;;;   GTK_SCROLL_STEP_DOWN,
;;;   GTK_SCROLL_PAGE_UP,
;;;   GTK_SCROLL_PAGE_DOWN,
;;;   GTK_SCROLL_STEP_LEFT,
;;;   GTK_SCROLL_STEP_RIGHT,
;;;   GTK_SCROLL_PAGE_LEFT,
;;;   GTK_SCROLL_PAGE_RIGHT,
;;;   GTK_SCROLL_START,
;;;   GTK_SCROLL_END
;;; } GtkScrollType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkScrollType" gtk-scroll-type
  (:export t
   :type-initializer "gtk_scroll_type_get_type")
  (:none 0)
  (:jump 1)
  (:step-backward 2)
  (:step-forward 3)
  (:page-backward 4)
  (:page-forward 5)
  (:step-up 6)
  (:step-down 7)
  (:page-up 8)
  (:page-down 9)
  (:step-left 10)
  (:step-right 11)
  (:page-left 12)
  (:page-right 13)
  (:start 14)
  (:end 15))

;;; ----------------------------------------------------------------------------
;;; enum GtkSelectionMode
;;;
;;; typedef enum {
;;;   GTK_SELECTION_NONE,
;;;   GTK_SELECTION_SINGLE,
;;;   GTK_SELECTION_BROWSE,
;;;   GTK_SELECTION_MULTIPLE
;;; } GtkSelectionMode;
;;;
;;; Used to control what selections users are allowed to make.
;;;
;;; GTK_SELECTION_NONE
;;;     No selection is possible.
;;;
;;; GTK_SELECTION_SINGLE
;;;     Zero or one element may be selected.
;;;
;;; GTK_SELECTION_BROWSE
;;;     Exactly one element is selected. In some circumstances, such as
;;;     initially or during a search operation, it's possible for no element to
;;;     be selected with GTK_SELECTION_BROWSE. What is really enforced is that
;;;     the user can't deselect a currently selected element except by selecting
;;;     another element.
;;;
;;; GTK_SELECTION_MULTIPLE
;;;     Any number of elements may be selected. The Ctrl key may be used to
;;;     enlarge the selection, and Shift key to select between the focus and the
;;;     child pointed to. Some widgets may also allow Click-drag to select a
;;;     range of elements.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSelectionMode" gtk-selection-mode
  (:export t
   :type-initializer "gtk_selection_mode_get_type")
  (:none 0)
  (:single 1)
  (:browse 2)
  (:multiple 3)
  (:extended 3))
|#

;;; --- gtk-shadow-type --------------------------------------------------------

(setf (gethash 'gtk-shadow-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-shadow-type atdoc:*external-symbols*)
 "@version{2013-2-3}
  @begin{short}
    Used to change the appearance of an outline typically provided by a
    @class{gtk-frame} widget.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkShadowType\" gtk-shadow-type
  (:export t
   :type-initializer \"gtk_shadow_type_get_type\")
  (:none 0)
  (:in 1)
  (:out 2)
  (:etched-in 3)
  (:etched-out 4))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No outline.}
    @entry[:in]{The outline is bevelled inwards.}
    @entry[:out]{The outline is bevelled outwards like a button.}
    @entry[:etched-in]{The outline has a sunken 3d appearance.}
    @entry[:etched-out]{The outline has a raised 3d appearance.}
  @end{table}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GtkStateType
;;;
;;; typedef enum {
;;;   GTK_STATE_NORMAL,
;;;   GTK_STATE_ACTIVE,
;;;   GTK_STATE_PRELIGHT,
;;;   GTK_STATE_SELECTED,
;;;   GTK_STATE_INSENSITIVE,
;;;   GTK_STATE_INCONSISTENT,
;;;   GTK_STATE_FOCUSED
;;; } GtkStateType;
;;;
;;; This type indicates the current state of a widget; the state determines how
;;; the widget is drawn. The GtkStateType enumeration is also used to identify
;;; different colors in a GtkStyle for drawing, so states can be used for
;;; subparts of a widget as well as entire widgets.
;;;
;;; GTK_STATE_NORMAL
;;;     State during normal operation.
;;;
;;; GTK_STATE_ACTIVE
;;;     State of a currently active widget, such as a depressed button.
;;;
;;; GTK_STATE_PRELIGHT
;;;     State indicating that the mouse pointer is over the widget and the
;;;     widget will respond to mouse clicks.
;;;
;;; GTK_STATE_SELECTED
;;;     State of a selected item, such the selected row in a list.
;;;
;;; GTK_STATE_INSENSITIVE
;;;     State indicating that the widget is unresponsive to user actions.
;;;
;;; GTK_STATE_INCONSISTENT
;;;     The widget is inconsistent, such as checkbuttons or radiobuttons that
;;;     aren't either set to TRUE nor FALSE, or buttons requiring the user
;;;     attention.
;;;
;;; GTK_STATE_FOCUSED
;;;     The widget has the keyboard focus.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkStateType" gtk-state-type
  (:export t
   :type-initializer "gtk_state_type_get_type")
  (:normal 0)
  (:active 1)
  (:prelight 2)
  (:selected 3)
  (:insensitive 4)
  (:inconsistent 5)
  (:focused 6))

;;; ----------------------------------------------------------------------------
;;; enum GtkStateFlags
;;;
;;; typedef enum {
;;;   GTK_STATE_FLAG_NORMAL       = 0,
;;;   GTK_STATE_FLAG_ACTIVE       = 1 << 0,
;;;   GTK_STATE_FLAG_PRELIGHT     = 1 << 1,
;;;   GTK_STATE_FLAG_SELECTED     = 1 << 2,
;;;   GTK_STATE_FLAG_INSENSITIVE  = 1 << 3,
;;;   GTK_STATE_FLAG_INCONSISTENT = 1 << 4,
;;;   GTK_STATE_FLAG_FOCUSED      = 1 << 5,
;;;   GTK_STATE_FLAG_BACKDROP     = 1 << 6
;;; } GtkStateFlags;
;;;
;;; Describes a widget state.
;;;
;;; GTK_STATE_FLAG_NORMAL
;;;     State during normal operation.
;;;
;;; GTK_STATE_FLAG_ACTIVE
;;;     Widget is active.
;;;
;;; GTK_STATE_FLAG_PRELIGHT
;;;     Widget has a mouse pointer over it.
;;;
;;; GTK_STATE_FLAG_SELECTED
;;;     Widget is selected.
;;;
;;; GTK_STATE_FLAG_INSENSITIVE
;;;     Widget is insensitive.
;;;
;;; GTK_STATE_FLAG_INCONSISTENT
;;;     Widget is inconsistent.
;;;
;;; GTK_STATE_FLAG_FOCUSED
;;;     Widget has the keyboard focus.
;;;
;;; GTK_STATE_FLAG_BACKDROP
;;;     Widget is in a background toplevel window.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkStateFlags" gtk-state-flags
  (:export t
   :type-initializer "gtk_state_flags_get_type")
  (:normal 0)
  (:active 1)
  (:prelight 2)
  (:selected 3)
  (:insensitive 4)
  (:inconsistent 5)
  (:focused 6))

;;; ----------------------------------------------------------------------------
;;; enum GtkToolbarStyle
;;;
;;; typedef enum {
;;;   GTK_TOOLBAR_ICONS,
;;;   GTK_TOOLBAR_TEXT,
;;;   GTK_TOOLBAR_BOTH,
;;;   GTK_TOOLBAR_BOTH_HORIZ
;;; } GtkToolbarStyle;
;;;
;;; Used to customize the appearance of a GtkToolbar. Note that setting the
;;; toolbar style overrides the user's preferences for the default toolbar
;;; style. Note that if the button has only a label set and GTK_TOOLBAR_ICONS is
;;; used, the label will be visible, and vice versa.
;;;
;;; GTK_TOOLBAR_ICONS
;;;     Buttons display only icons in the toolbar.
;;;
;;; GTK_TOOLBAR_TEXT
;;;     Buttons display only text labels in the toolbar.
;;;
;;; GTK_TOOLBAR_BOTH
;;;     Buttons display text and icons in the toolbar.
;;;
;;; GTK_TOOLBAR_BOTH_HORIZ
;;;     Buttons display icons and text alongside each other, rather than
;;;     vertically stacked
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkToolbarStyle" gtk-toolbar-style
  (:export t
   :type-initializer "gtk_toolbar_style_get_type")
  (:icons 0)
  (:text 1)
  (:both 2)
  (:both-horiz 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkWindowPosition
;;;
;;; typedef enum {
;;;   GTK_WIN_POS_NONE,
;;;   GTK_WIN_POS_CENTER,
;;;   GTK_WIN_POS_MOUSE,
;;;   GTK_WIN_POS_CENTER_ALWAYS,
;;;   GTK_WIN_POS_CENTER_ON_PARENT
;;; } GtkWindowPosition;
;;;
;;; Window placement can be influenced using this enumeration. Note that using
;;; GTK_WIN_POS_CENTER_ALWAYS is almost always a bad idea. It won't necessarily
;;; work well with all window managers or on all windowing systems.
;;;
;;; GTK_WIN_POS_NONE
;;;     No influence is made on placement.
;;;
;;; GTK_WIN_POS_CENTER
;;;     Windows should be placed in the center of the screen.
;;;
;;; GTK_WIN_POS_MOUSE
;;;     Windows should be placed at the current mouse position.
;;;
;;; GTK_WIN_POS_CENTER_ALWAYS
;;;     Keep window centered as it changes size, etc.
;;;
;;; GTK_WIN_POS_CENTER_ON_PARENT
;;;     Center the window on its transient parent (see
;;;     gtk_window_set_transient_for()).
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWindowPosition" gtk-window-position
  (:export t
   :type-initializer "gtk_window_position_get_type")
  (:none 0)
  (:center 1)
  (:mouse 2)
  (:center-always 3)
  (:center-on-parent 4))
|#

;;; --- gtk-window-type --------------------------------------------------------

(setf (gethash 'gtk-window-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-window-type atdoc:*external-symbols*)
 "@version{2012-12-30}
  @begin{short}
    An enumeration for the possible types of a @code{GtkWindow}.
  @end{short}
  A GtkWindow can be one of the types @code{:toplevel} or @code{:popup}. Most
  things you'd consider a \"window\" should have type @code{:toplevel}; windows
  with this type are managed by the window manager and have a frame by default
  (call @fun{gtk-window-set-decorated} to toggle the frame). Windows with type
  @code{:popup} are ignored by the window manager; window manager keybindings
  won't work on them, the window manager won't decorate the window with a frame,
  many GTK+ features that rely on the window manager will not work (e. g. resize
  grips and maximization/minimization). @code{:popup} is used to implement
  widgets such as @code{GtkMenu} or tooltips that you normally don't think of as
  windows per se. Nearly all windows should be @code{:toplevel}. In particular,
  do not use @code{:popup} just to turn off the window borders; use
  @fun{gtk-window-set-decorated} for that.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GtkWindowType\" gtk-window-type
  (:export t
   :type-initializer \"gtk_window_type_get_type\")
  (:toplevel 0)
  (:popup 1))
    @end{pre}
    @begin{table}
      @entry[:toplevel]{A regular window, such as a dialog.}
      @entry[:popup]{A special window such as a tooltip.}
    @end{table}
  @end{dictionary}
  @see-function{gtk-window-set-decorated}")

#|
;;; ----------------------------------------------------------------------------
;;; enum GtkSortType
;;;
;;; typedef enum {
;;;   GTK_SORT_ASCENDING,
;;;   GTK_SORT_DESCENDING
;;; } GtkSortType;
;;;
;;; Determines the direction of a sort.
;;;
;;; GTK_SORT_ASCENDING
;;;     Sorting is in ascending order.
;;;
;;; GTK_SORT_DESCENDING
;;;     Sorting is in descending order.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSortType" gtk-sort-type
  (:export t
   :type-initializer "gtk_sort_type_get_type")
  (:ascending 0)
  (:descending 1))

;;; ----------------------------------------------------------------------------
;;; enum GtkDragResult
;;;
;;; typedef enum {
;;;   GTK_DRAG_RESULT_SUCCESS,
;;;   GTK_DRAG_RESULT_NO_TARGET,
;;;   GTK_DRAG_RESULT_USER_CANCELLED,
;;;   GTK_DRAG_RESULT_TIMEOUT_EXPIRED,
;;;   GTK_DRAG_RESULT_GRAB_BROKEN,
;;;   GTK_DRAG_RESULT_ERROR
;;; } GtkDragResult;
;;;
;;; Gives an indication why a drag operation failed. The value can by obtained
;;; by connecting to the "drag-failed" signal.
;;;
;;; GTK_DRAG_RESULT_SUCCESS
;;;     The drag operation was successful.
;;;
;;; GTK_DRAG_RESULT_NO_TARGET
;;;     No suitable drag target.
;;;
;;; GTK_DRAG_RESULT_USER_CANCELLED
;;;     The user cancelled the drag operation.
;;;
;;; GTK_DRAG_RESULT_TIMEOUT_EXPIRED
;;;     The drag operation timed out.
;;;
;;; GTK_DRAG_RESULT_GRAB_BROKEN
;;;     The pointer or keyboard grab used for the drag operation was broken.
;;;
;;; GTK_DRAG_RESULT_ERROR
;;;     The drag operation failed due to some unspecified error.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkDragResult" gtk-drag-result
  (:export t
   :type-initializer "gtk_drag_result_get_type")
  (:success 0)
  (:no-target 1)
  (:user-cancelled 2)
  (:timeout-expired 3)
  (:grab-broken 4)
  (:error 5))

;;; ----------------------------------------------------------------------------
;;; enum GtkJunctionSides
;;;
;;; typedef enum {
;;;   GTK_JUNCTION_NONE   = 0,
;;;   GTK_JUNCTION_CORNER_TOPLEFT = 1 << 0,
;;;   GTK_JUNCTION_CORNER_TOPRIGHT = 1 << 1,
;;;   GTK_JUNCTION_CORNER_BOTTOMLEFT = 1 << 2,
;;;   GTK_JUNCTION_CORNER_BOTTOMRIGHT = 1 << 3,
;;;   GTK_JUNCTION_TOP    = (GTK_JUNCTION_CORNER_TOPLEFT |
;;;                          GTK_JUNCTION_CORNER_TOPRIGHT),
;;;   GTK_JUNCTION_BOTTOM = (GTK_JUNCTION_CORNER_BOTTOMLEFT |
;;;                          GTK_JUNCTION_CORNER_BOTTOMRIGHT),
;;;   GTK_JUNCTION_LEFT   = (GTK_JUNCTION_CORNER_TOPLEFT |
;;;                          GTK_JUNCTION_CORNER_BOTTOMLEFT),
;;;   GTK_JUNCTION_RIGHT  = (GTK_JUNCTION_CORNER_TOPRIGHT |
;;;                          GTK_JUNCTION_CORNER_BOTTOMRIGHT)
;;; } GtkJunctionSides;
;;;
;;; Describes how a rendered element connects to adjacent elements.
;;;
;;; GTK_JUNCTION_NONE
;;;     No junctions.
;;;
;;; GTK_JUNCTION_CORNER_TOPLEFT
;;;     Element connects on the top-left corner.
;;;
;;; GTK_JUNCTION_CORNER_TOPRIGHT
;;;     Element connects on the top-right corner.
;;;
;;; GTK_JUNCTION_CORNER_BOTTOMLEFT
;;;     Element connects on the bottom-left corner.
;;;
;;; GTK_JUNCTION_CORNER_BOTTOMRIGHT
;;;     Element connects on the bottom-right corner.
;;;
;;; GTK_JUNCTION_TOP
;;;     Element connects on the top side.
;;;
;;; GTK_JUNCTION_BOTTOM
;;;     Element connects on the bottom side.
;;;
;;; GTK_JUNCTION_LEFT
;;;     Element connects on the left side.
;;;
;;; GTK_JUNCTION_RIGHT
;;;     Element connects on the right side.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkBorderStyle
;;;
;;; typedef enum {
;;;   GTK_BORDER_STYLE_NONE,
;;;   GTK_BORDER_STYLE_SOLID,
;;;   GTK_BORDER_STYLE_INSET,
;;;   GTK_BORDER_STYLE_OUTSET,
;;;   GTK_BORDER_STYLE_HIDDEN,
;;;   GTK_BORDER_STYLE_DOTTED,
;;;   GTK_BORDER_STYLE_DASHED,
;;;   GTK_BORDER_STYLE_DOUBLE,
;;;   GTK_BORDER_STYLE_GROOVE,
;;;   GTK_BORDER_STYLE_RIDGE
;;; } GtkBorderStyle;
;;;
;;; Describes how the border of a UI element should be rendered.
;;;
;;; GTK_BORDER_STYLE_NONE
;;;     No visible border
;;;
;;; GTK_BORDER_STYLE_SOLID
;;;     A single line segment
;;;
;;; GTK_BORDER_STYLE_INSET
;;;     Looks as if the content is sunken into the canvas
;;;
;;; GTK_BORDER_STYLE_OUTSET
;;;     Looks as if the content is coming out of the canvas
;;;
;;; GTK_BORDER_STYLE_HIDDEN
;;;     Same as GTK_BORDER_STYLE_NONE
;;;
;;; GTK_BORDER_STYLE_DOTTED
;;;     A series of round dots
;;;
;;; GTK_BORDER_STYLE_DASHED
;;;     A series of square-ended dashes
;;;
;;; GTK_BORDER_STYLE_DOUBLE
;;;     Two parrallel lines with some space between them
;;;
;;; GTK_BORDER_STYLE_GROOVE
;;;     Looks as if it were carved in the canvas
;;;
;;; GTK_BORDER_STYLE_RIDGE
;;;     Looks as if it were coming out of the canvas
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRegionFlags
;;;
;;; typedef enum {
;;;   GTK_REGION_EVEN    = 1 << 0,
;;;   GTK_REGION_ODD     = 1 << 1,
;;;   GTK_REGION_FIRST   = 1 << 2,
;;;   GTK_REGION_LAST    = 1 << 3,
;;;   GTK_REGION_ONLY    = 1 << 4,
;;;   GTK_REGION_SORTED  = 1 << 5
;;; } GtkRegionFlags;
;;;
;;; Describes a region within a widget.
;;;
;;; GTK_REGION_EVEN
;;;     Region has an even number within a set.
;;;
;;; GTK_REGION_ODD
;;;     Region has an odd number within a set.
;;;
;;; GTK_REGION_FIRST
;;;     Region is the first one within a set.
;;;
;;; GTK_REGION_LAST
;;;     Region is the last one within a set.
;;;
;;; GTK_REGION_ONLY
;;;     Region is the only one within a set.
;;;
;;; GTK_REGION_SORTED
;;;     Region is part of a sorted area.
;;; ----------------------------------------------------------------------------
|#

;;; --- End of file atdoc-gtk.enumerations.lisp --------------------------------

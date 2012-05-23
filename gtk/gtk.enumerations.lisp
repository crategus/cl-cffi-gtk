;;; ----------------------------------------------------------------------------
;;; gtk.enumerations.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Standard Enumerations
;;; 
;;; Synopsis
;;; 
;;;     GtkAccelFlags
;;;     GtkArrowPlacement
;;;     GtkArrowType
;;;     GtkAttachOptions
;;;     GtkButtonBoxStyle
;;;     GtkCornerType
;;;     GtkDeleteType
;;;     GtkDirectionTye
;;;     GtkExpanderStyle
;;;     GtkIMPreeditStyle
;;;     GtkIMStatusStyle
;;;     GtkJustification
;;;     GtkMovementStep
;;;     GtkOrientation
;;;     GtkPackType
;;;     GtkPathPriorityType
;;;     GtkPathType
;;;     GtkPolicyType
;;;     GtkPositionType
;;;     GtkReliefStyle
;;;     GtkResizeMode
;;;     GtkScrollStep
;;;     GtkScrollType
;;;     GtkSelectionMode
;;;     GtkShadowType
;;;     GtkStateType
;;;     GtkStateFlags
;;;     GtkToolbarStyle
;;;     GtkWindowPosition
;;;     GtkWindowType
;;;     GtkSortType
;;;     GtkDragResult
;;;     GtkJunctionSides
;;;     GtkBorderStyle
;;;     GtkRegionFlags
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkAccelFlags
;;; 
;;; typedef enum {
;;;   GTK_ACCEL_VISIBLE        = 1 << 0, /* display in GtkAccelLabel? */
;;;   GTK_ACCEL_LOCKED         = 1 << 1, /* is it removable? */
;;;   GTK_ACCEL_MASK           = 0x07
;;; } GtkAccelFlags;
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkAccelFlags" gtk-accel-flags
  (:export t
   :type-initializer "gtk_accel_flags_get_type")
  (:visible 1)
  (:locked 2)
  (:mask 7))

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

;;; ----------------------------------------------------------------------------
;;; enum GtkArrowType
;;; 
;;; typedef enum {
;;;   GTK_ARROW_UP,
;;;   GTK_ARROW_DOWN,
;;;   GTK_ARROW_LEFT,
;;;   GTK_ARROW_RIGHT,
;;;   GTK_ARROW_NONE
;;; } GtkArrowType;
;;; 
;;; Used to indicate the direction in which a GtkArrow should point.
;;; 
;;; GTK_ARROW_UP
;;;     Represents an upward pointing arrow.
;;; 
;;; GTK_ARROW_DOWN
;;;     Represents a downward pointing arrow.
;;; 
;;; GTK_ARROW_LEFT
;;;     Represents a left pointing arrow.
;;; 
;;; GTK_ARROW_RIGHT
;;;     Represents a right pointing arrow.
;;; 
;;; GTK_ARROW_NONE
;;;     No arrow. Since 2.10.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkArrowType" gtk-arrow-type
  (:export t
   :type-initializer "gtk_arrow_type_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))

;;; ----------------------------------------------------------------------------
;;; enum GtkAttachOptions
;;; 
;;; typedef enum {
;;;   GTK_EXPAND = 1 << 0,
;;;   GTK_SHRINK = 1 << 1,
;;;   GTK_FILL   = 1 << 2
;;; } GtkAttachOptions;
;;; 
;;; Denotes the expansion properties that a widget will have when it (or its
;;; parent) is resized.
;;; 
;;; GTK_EXPAND
;;;     the widget should expand to take up any extra space in its container
;;;     that has been allocated.
;;; 
;;; GTK_SHRINK
;;;     the widget should shrink as and when possible.
;;; 
;;; GTK_FILL
;;;     the widget should fill the space allocated to it.
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkAttachOptions" gtk-attach-options
  (:export t
   :type-initializer "gtk_attach_options_get_type")
  (:expand 1)
  (:shrink 2)
  (:fill 4))

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonBoxStyle
;;; 
;;; typedef enum {
;;;   GTK_BUTTONBOX_SPREAD = 1,
;;;   GTK_BUTTONBOX_EDGE,
;;;   GTK_BUTTONBOX_START,
;;;   GTK_BUTTONBOX_END,
;;;   GTK_BUTTONBOX_CENTER
;;; } GtkButtonBoxStyle;
;;; 
;;; Used to dictate the style that a GtkButtonBox uses to layout the buttons
;;; it contains. (See also: GtkVButtonBox and GtkHButtonBox).
;;; 
;;; GTK_BUTTONBOX_SPREAD
;;;     Buttons are evenly spread across the box.
;;; 
;;; GTK_BUTTONBOX_EDGE
;;;     Buttons are placed at the edges of the box.
;;; 
;;; GTK_BUTTONBOX_START
;;;     Buttons are grouped towards the start of the box, (on the left for a
;;;     HBox, or the top for a VBox).
;;; 
;;; GTK_BUTTONBOX_END
;;;     Buttons are grouped towards the end of the box, (on the right for a
;;;     HBox, or the bottom for a VBox).
;;; 
;;; GTK_BUTTONBOX_CENTER
;;;     Buttons are centered in the box. Since 2.12.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkButtonBoxStyle" gtk-button-box-style
  (:export t
   :type-initializer "gtk_button_box_style_get_type")
  (:default-style 0)
  (:spread 1)
  (:edge 2)
  (:start 3)
  (:end 4)
  (:center 5))

;;; ----------------------------------------------------------------------------
;;; enum GtkCornerType
;;; 
;;; typedef enum {
;;;   GTK_CORNER_TOP_LEFT,
;;;   GTK_CORNER_BOTTOM_LEFT,
;;;   GTK_CORNER_TOP_RIGHT,
;;;   GTK_CORNER_BOTTOM_RIGHT
;;; } GtkCornerType;
;;; 
;;; Specifies which corner a child widget should be placed in when packed into
;;; a GtkScrolledWindow. This is effectively the opposite of where the scroll
;;; bars are placed.
;;; 
;;; GTK_CORNER_TOP_LEFT
;;;     Place the scrollbars on the right and bottom of the widget
;;;     (default behaviour).
;;; 
;;; GTK_CORNER_BOTTOM_LEFT
;;;     Place the scrollbars on the top and right of the widget.
;;; 
;;; GTK_CORNER_TOP_RIGHT
;;;     Place the scrollbars on the left and bottom of the widget.
;;; 
;;; GTK_CORNER_BOTTOM_RIGHT
;;;     Place the scrollbars on the top and left of the widget.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCornerType" gtk-corner-type
  (:export t
   :type-initializer "gtk_corner_type_get_type")
  (:top-left 0)
  (:bottom-left 1)
  (:top-right 2)
  (:bottom-right 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkDeleteType
;;; 
;;; typedef enum {
;;;   GTK_DELETE_CHARS,
;;;   GTK_DELETE_WORD_ENDS,       /* delete only the portion of the word to the
;;;                                * left/right of cursor if we're in the
;;;                                *  middle of a word */
;;;   GTK_DELETE_WORDS,
;;;   GTK_DELETE_DISPLAY_LINES,
;;;   GTK_DELETE_DISPLAY_LINE_ENDS,
;;;   GTK_DELETE_PARAGRAPH_ENDS,      /* like C-k in Emacs (or its reverse) */
;;;   GTK_DELETE_PARAGRAPHS,          /* C-k in pico, kill whole line */
;;;   GTK_DELETE_WHITESPACE           /* M-\ in Emacs */
;;; } GtkDeleteType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkDeleteType" gtk-delete-type
  (:export t
   :type-initializer "gtk_delete_type_get_type")
  (:chars 0)
  (:word-ends 1)
  (:words 2)
  (:display-lines 3)
  (:display-line-ends 4)
  (:paragraph-ends 5)
  (:paragraphs 6)
  (:whitespace 7))

;;; ----------------------------------------------------------------------------
;;; enum GtkDirectionType
;;; 
;;; typedef enum {
;;;   GTK_DIR_TAB_FORWARD,
;;;   GTK_DIR_TAB_BACKWARD,
;;;   GTK_DIR_UP,
;;;   GTK_DIR_DOWN,
;;;   GTK_DIR_LEFT,
;;;   GTK_DIR_RIGHT
;;; } GtkDirectionType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkDirectionType" gtk-direction-type
  (:export t
   :type-initializer "gtk_direction_type_get_type")
  (:tab-forward 0)
  (:tab-backward 1)
  (:up 2)
  (:down 3)
  (:left 4)
  (:right 5))

;;; ----------------------------------------------------------------------------
;;; enum GtkExpanderStyle
;;; 
;;; typedef enum {
;;;   GTK_EXPANDER_COLLAPSED,
;;;   GTK_EXPANDER_SEMI_COLLAPSED,
;;;   GTK_EXPANDER_SEMI_EXPANDED,
;;;   GTK_EXPANDER_EXPANDED
;;; } GtkExpanderStyle;
;;; 
;;; Used to specify the style of the expanders drawn by a GtkTreeView.
;;; 
;;; GTK_EXPANDER_COLLAPSED
;;;     The style used for a collapsed subtree.
;;; 
;;; GTK_EXPANDER_SEMI_COLLAPSED
;;;     Intermediate style used during animation.
;;; 
;;; GTK_EXPANDER_SEMI_EXPANDED
;;;     Intermediate style used during animation.
;;; 
;;; GTK_EXPANDER_EXPANDED
;;;     The style used for an expanded subtree.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkExpanderStyle" gtk-expander-style
  (:export t
   :type-initializer "gtk_expander_style_get_type")
  (:collapsed 0)
  (:semi-collapsed 1)
  (:semi-expanded 2)
  (:expanded 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkIMPreeditStyle
;;; 
;;; typedef enum {
;;;   GTK_IM_PREEDIT_NOTHING,
;;;   GTK_IM_PREEDIT_CALLBACK,
;;;   GTK_IM_PREEDIT_NONE
;;; } GtkIMPreeditStyle;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIMPreeditStyle" gtk-im-preedit-style
  (:export t)
  (:nothing 0)
  (:callback 1)
  (:none 2))

;;; ----------------------------------------------------------------------------
;;; enum GtkIMStatusStyle
;;; 
;;; typedef enum {
;;;   GTK_IM_STATUS_NOTHING,
;;;   GTK_IM_STATUS_CALLBACK,
;;;   GTK_IM_STATUS_NONE
;;; } GtkIMStatusStyle;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIMStatusStyle" gtk-im-status-style
  (:export t)
  (:nothing 0)
  (:callback 1)
  (:none 2))

;;; ----------------------------------------------------------------------------
;;; enum GtkJustification
;;; 
;;; typedef enum {
;;;   GTK_JUSTIFY_LEFT,
;;;   GTK_JUSTIFY_RIGHT,
;;;   GTK_JUSTIFY_CENTER,
;;;   GTK_JUSTIFY_FILL
;;; } GtkJustification;
;;; 
;;; Used for justifying the text inside a GtkLabel widget.
;;; (See also GtkAlignment).
;;; 
;;; GTK_JUSTIFY_LEFT
;;;     The text is placed at the left edge of the label.
;;; 
;;; GTK_JUSTIFY_RIGHT
;;;     The text is placed at the right edge of the label.
;;; 
;;; GTK_JUSTIFY_CENTER
;;;     The text is placed in the center of the label.
;;; 
;;; GTK_JUSTIFY_FILL
;;;     The text is placed is distributed across the label.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkJustification" gtk-justification
  (:export t
   :type-initializer "gtk_justification_get_type")
  (:left 0)
  (:right 1)
  (:center 2)
  (:fill 3))

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

;;; ----------------------------------------------------------------------------
;;; enum GtkOrientation
;;; 
;;; typedef enum {
;;;   GTK_ORIENTATION_HORIZONTAL,
;;;   GTK_ORIENTATION_VERTICAL
;;; } GtkOrientation;
;;; 
;;; Represents the orientation of widgets which can be switched between
;;; horizontal and vertical orientation on the fly, like GtkToolbar.
;;; 
;;; GTK_ORIENTATION_HORIZONTAL
;;;     The widget is in horizontal orientation.
;;; 
;;; GTK_ORIENTATION_VERTICAL
;;;     The widget is in vertical orientation.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkOrientation" gtk-orientation
  (:export t
   :type-initializer "gtk_orientation_get_type")
  (:horizontal 0)
  (:vertical 1))

;;; ----------------------------------------------------------------------------
;;; enum GtkPackType
;;; 
;;; typedef enum {
;;;   GTK_PACK_START,
;;;   GTK_PACK_END
;;; } GtkPackType;
;;; 
;;; Represents the packing location GtkBox children. (See: GtkVBox, GtkHBox,
;;; and GtkButtonBox).
;;; 
;;; GTK_PACK_START
;;;     The child is packed into the start of the box
;;; 
;;; GTK_PACK_END
;;;     The child is packed into the end of the box
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPackType" gtk-pack-type
  (:export t
   :type-initializer "gtk_pack_type_get_type")
  (:start 0)
  (:end 1))

;;; ----------------------------------------------------------------------------
;;; enum GtkPathPriorityType
;;; 
;;; typedef enum {
;;;   GTK_PATH_PRIO_LOWEST      = 0,
;;;   GTK_PATH_PRIO_GTK         = 4,
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
;;;     The scrollbar will appear and disappear as necessary. For example,
;;;     when all of a GtkCList can not be seen.
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

;;; ----------------------------------------------------------------------------
;;; enum GtkPositionType
;;; 
;;; typedef enum {
;;;   GTK_POS_LEFT,
;;;   GTK_POS_RIGHT,
;;;   GTK_POS_TOP,
;;;   GTK_POS_BOTTOM
;;; } GtkPositionType;
;;; 
;;; Describes which edge of a widget a certain feature is positioned at, e.g.
;;; the tabs of a GtkNotebook, the handle of a GtkHandleBox or the label of a
;;; GtkScale.
;;; 
;;; GTK_POS_LEFT
;;;     The feature is at the left edge.
;;; 
;;; GTK_POS_RIGHT
;;;     The feature is at the right edge.
;;; 
;;; GTK_POS_TOP
;;;     The feature is at the top edge.
;;; 
;;; GTK_POS_BOTTOM
;;;     The feature is at the bottom edge.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPositionType" gtk-position-type
  (:export t
   :type-initializer "gtk_position_type_get_type")
  (:left 0)
  (:right 1)
  (:top 2)
  (:bottom 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkReliefStyle
;;; 
;;; typedef enum {
;;;   GTK_RELIEF_NORMAL,
;;;   GTK_RELIEF_HALF,
;;;   GTK_RELIEF_NONE
;;; } GtkReliefStyle;
;;; 
;;; Indicated the relief to be drawn around a GtkButton.
;;; 
;;; GTK_RELIEF_NORMAL
;;;     Draw a normal relief.
;;; 
;;; GTK_RELIEF_HALF
;;;     A half relief.
;;; 
;;; GTK_RELIEF_NONE
;;;     No relief.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkReliefStyle" gtk-relief-style
  (:export t
   :type-initializer "gtk_relief_style_get_type")
  (:normal 0)
  (:half 1)
  (:none 2))

;;; ----------------------------------------------------------------------------
;;; enum GtkResizeMode
;;; 
;;; typedef enum {
;;;   GTK_RESIZE_PARENT,
;;;   GTK_RESIZE_QUEUE,
;;;   GTK_RESIZE_IMMEDIATE
;;; } GtkResizeMode;
;;; 
;;; GTK_RESIZE_PARENT
;;;     Pass resize request to the parent
;;; 
;;; GTK_RESIZE_QUEUE
;;;     Queue resizes on this widget
;;; 
;;; GTK_RESIZE_IMMEDIATE
;;;     Resize immediately. Deprecated.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkResizeMode" gtk-resize-mode
  (:export t
   :type-initializer "gtk_resize_mode_get_type")
  (:parent 0)
  (:queue 1)
  (:immediate 2))

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
;;;     initially or during a search operation, it's possible for no element
;;;     to be selected with GTK_SELECTION_BROWSE. What is really enforced is
;;;     that the user can't deselect a currently selected element except by
;;;     selecting another element.
;;; 
;;; GTK_SELECTION_MULTIPLE
;;;     Any number of elements may be selected. The Ctrl key may be used to
;;;     enlarge the selection, and Shift key to select between the focus and
;;;     the child pointed to. Some widgets may also allow Click-drag to select
;;;     a range of elements.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSelectionMode" gtk-selection-mode
  (:export t
   :type-initializer "gtk_selection_mode_get_type")
  (:none 0)
  (:single 1)
  (:browse 2)
  (:multiple 3)
  (:extended 3))

;;; ----------------------------------------------------------------------------
;;; enum GtkShadowType
;;; 
;;; typedef enum {
;;;   GTK_SHADOW_NONE,
;;;   GTK_SHADOW_IN,
;;;   GTK_SHADOW_OUT,
;;;   GTK_SHADOW_ETCHED_IN,
;;;   GTK_SHADOW_ETCHED_OUT
;;; } GtkShadowType;
;;; 
;;; Used to change the appearance of an outline typically provided by a
;;; GtkFrame.
;;; 
;;; GTK_SHADOW_NONE
;;;     No outline.
;;; 
;;; GTK_SHADOW_IN
;;;     The outline is bevelled inwards.
;;; 
;;; GTK_SHADOW_OUT
;;;     The outline is bevelled outwards like a button.
;;; 
;;; GTK_SHADOW_ETCHED_IN
;;;     The outline has a sunken 3d appearance.
;;; 
;;; GTK_SHADOW_ETCHED_OUT
;;;     The outline has a raised 3d appearance.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkShadowType" gtk-shadow-type
  (:export t
   :type-initializer "gtk_shadow_type_get_type")
  (:none 0)
  (:in 1)
  (:out 2)
  (:etched-in 3)
  (:etched-out 4))

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
;;;   GTK_STATE_FLAG_FOCUSED      = 1 << 5
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
;;; style. Note that if the button has only a label set and GTK_TOOLBAR_ICONS
;;; is used, the label will be visible, and vice versa.
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
;;;     Center the window on its transient parent
;;;     (see gtk_window_set_transient_for()).
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWindowPosition" gtk-window-position
  (:export t
   :type-initializer "gtk_window_position_get_type")
  (:none 0)
  (:center 1)
  (:mouse 2)
  (:center-always 3)
  (:center-on-parent 4))

;;; ----------------------------------------------------------------------------
;;; enum GtkWindowType
;;; 
;;; typedef enum {
;;;   GTK_WINDOW_TOPLEVEL,
;;;   GTK_WINDOW_POPUP
;;; } GtkWindowType;
;;; 
;;; A GtkWindow can be one of these types. Most things you'd consider a
;;; "window" should have type GTK_WINDOW_TOPLEVEL; windows with this type are
;;; managed by the window manager and have a frame by default (call
;;; gtk_window_set_decorated() to toggle the frame). Windows with type
;;; GTK_WINDOW_POPUP are ignored by the window manager; window manager
;;; keybindings won't work on them, the window manager won't decorate the
;;; window with a frame, many GTK+ features that rely on the window manager
;;; will not work (e.g. resize grips and maximization/minimization).
;;; GTK_WINDOW_POPUP is used to implement widgets such as GtkMenu or tooltips
;;; that you normally don't think of as windows per se. Nearly all windows
;;; should be GTK_WINDOW_TOPLEVEL. In particular, do not use GTK_WINDOW_POPUP
;;; just to turn off the window borders; use gtk_window_set_decorated() for
;;; that.
;;; 
;;; GTK_WINDOW_TOPLEVEL
;;;     A regular window, such as a dialog.
;;; 
;;; GTK_WINDOW_POPUP
;;;     A special window such as a tooltip.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWindowType" gtk-window-type
  (:export t
   :type-initializer "gtk_window_type_get_type")
  (:toplevel 0)
  (:popup 1))

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
;;;   GTK_BORDER_STYLE_OUTSET
;;; } GtkBorderStyle;
;;; 
;;; Describes how the border of a UI element should be rendered.
;;; 
;;; GTK_BORDER_STYLE_NONE
;;;     No visible border
;;; 
;;; GTK_BORDER_STYLE_SOLID
;;;     A solid border
;;; 
;;; GTK_BORDER_STYLE_INSET
;;;     An inset border
;;; 
;;; GTK_BORDER_STYLE_OUTSET
;;;     An outset border
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRegionFlags
;;; 
;;; typedef enum {
;;;   GTK_REGION_EVEN    = 1 << 0,
;;;   GTK_REGION_ODD     = 1 << 1,
;;;   GTK_REGION_FIRST   = 1 << 2,
;;;   GTK_REGION_LAST    = 1 << 3,
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
;;; GTK_REGION_SORTED
;;;     Region is part of a sorted area.
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.enumerations.lisp --------------------------------------

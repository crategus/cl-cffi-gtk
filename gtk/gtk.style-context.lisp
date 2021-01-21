;;; ----------------------------------------------------------------------------
;;; gtk.style-context.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GtkStyleContext
;;;
;;;     Rendering UI elements
;;;
;;; Types and Values
;;;
;;;     GtkStyleContext
;;;
;;;     GtkJunctionSides
;;;     GtkRegionFlags                                   to gtk-widget-path.lisp
;;;     GtkStyleContextPrintFlags
;;;     GtkBorder
;;;     GtkBorderStyle
;;;
;;;     GTK_STYLE_PROPERTY_BACKGROUND_COLOR
;;;     GTK_STYLE_PROPERTY_COLOR
;;;     GTK_STYLE_PROPERTY_FONT
;;;     GTK_STYLE_PROPERTY_MARGIN
;;;     GTK_STYLE_PROPERTY_PADDING
;;;     GTK_STYLE_PROPERTY_BORDER_WIDTH
;;;     GTK_STYLE_PROPERTY_BORDER_RADIUS
;;;     GTK_STYLE_PROPERTY_BORDER_STYLE
;;;     GTK_STYLE_PROPERTY_BORDER_COLOR
;;;     GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
;;;
;;;     GTK_STYLE_CLASS_ACCELERATOR
;;;     GTK_STYLE_CLASS_ARROW
;;;     GTK_STYLE_CLASS_BACKGROUND
;;;     GTK_STYLE_CLASS_BOTTOM
;;;     GTK_STYLE_CLASS_BUTTON
;;;     GTK_STYLE_CLASS_CALENDAR
;;;     GTK_STYLE_CLASS_CELL
;;;     GTK_STYLE_CLASS_COMBOBOX_ENTRY
;;;     GTK_STYLE_CLASS_CONTEXT_MENU
;;;     GTK_STYLE_CLASS_CHECK
;;;     GTK_STYLE_CLASS_CSD
;;;     GTK_STYLE_CLASS_CURSOR_HANDLE
;;;     GTK_STYLE_CLASS_DEFAULT
;;;     GTK_STYLE_CLASS_DESTRUCTIVE_ACTION
;;;     GTK_STYLE_CLASS_DIM_LABEL
;;;     GTK_STYLE_CLASS_DND
;;;     GTK_STYLE_CLASS_DOCK
;;;     GTK_STYLE_CLASS_ENTRY
;;;     GTK_STYLE_CLASS_ERROR
;;;     GTK_STYLE_CLASS_EXPANDER
;;;     GTK_STYLE_CLASS_FRAME
;;;     GTK_STYLE_CLASS_FLAT
;;;     GTK_STYLE_CLASS_GRIP
;;;     GTK_STYLE_CLASS_HEADER
;;;     GTK_STYLE_CLASS_HIGHLIGHT
;;;     GTK_STYLE_CLASS_HORIZONTAL
;;;     GTK_STYLE_CLASS_IMAGE
;;;     GTK_STYLE_CLASS_INFO
;;;     GTK_STYLE_CLASS_INLINE_TOOLBAR
;;;     GTK_STYLE_CLASS_INSERTION_CURSOR
;;;     GTK_STYLE_CLASS_LABEL
;;;     GTK_STYLE_CLASS_LEFT
;;;     GTK_STYLE_CLASS_LEVEL_BAR
;;;     GTK_STYLE_CLASS_LINKED
;;;     GTK_STYLE_CLASS_LIST
;;;     GTK_STYLE_CLASS_LIST_ROW
;;;     GTK_STYLE_CLASS_MARK
;;;     GTK_STYLE_CLASS_MENU
;;;     GTK_STYLE_CLASS_MENUBAR
;;;     GTK_STYLE_CLASS_MENUITEM
;;;     GTK_STYLE_CLASS_MESSAGE_DIALOG
;;;     GTK_STYLE_CLASS_MONOSPACE
;;;     GTK_STYLE_CLASS_NEEDS_ATTENTION
;;;     GTK_STYLE_CLASS_NOTEBOOK
;;;     GTK_STYLE_CLASS_OSD
;;;     GTK_STYLE_CLASS_OVERSHOOT
;;;     GTK_STYLE_CLASS_PANE_SEPARATOR
;;;     GTK_STYLE_CLASS_PAPER
;;;     GTK_STYLE_CLASS_POPUP
;;;     GTK_STYLE_CLASS_POPOVER
;;;     GTK_STYLE_CLASS_PRIMARY_TOOLBAR
;;;     GTK_STYLE_CLASS_PROGRESSBAR
;;;     GTK_STYLE_CLASS_PULSE
;;;     GTK_STYLE_CLASS_QUESTION
;;;     GTK_STYLE_CLASS_RADIO
;;;     GTK_STYLE_CLASS_RAISED
;;;     GTK_STYLE_CLASS_READ_ONLY
;;;     GTK_STYLE_CLASS_RIGHT
;;;     GTK_STYLE_CLASS_RUBBERBAND
;;;     GTK_STYLE_CLASS_SCALE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
;;;     GTK_STYLE_CLASS_SCROLLBAR
;;;     GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
;;;     GTK_STYLE_CLASS_SEPARATOR
;;;     GTK_STYLE_CLASS_SIDEBAR
;;;     GTK_STYLE_CLASS_SLIDER
;;;     GTK_STYLE_CLASS_SPINBUTTON
;;;     GTK_STYLE_CLASS_SPINNER
;;;     GTK_STYLE_CLASS_STATUSBAR
;;;     GTK_STYLE_CLASS_SUBTITLE
;;;     GTK_STYLE_CLASS_SUGGESTED_ACTION
;;;     GTK_STYLE_CLASS_TITLE
;;;     GTK_STYLE_CLASS_TITLEBAR
;;;     GTK_STYLE_CLASS_TOOLBAR
;;;     GTK_STYLE_CLASS_TOOLTIP
;;;     GTK_STYLE_CLASS_TOUCH_SELECTION
;;;     GTK_STYLE_CLASS_TOP
;;;     GTK_STYLE_CLASS_TROUGH
;;;     GTK_STYLE_CLASS_UNDERSHOOT
;;;     GTK_STYLE_CLASS_VERTICAL
;;;     GTK_STYLE_CLASS_VIEW
;;;     GTK_STYLE_CLASS_WARNING
;;;     GTK_STYLE_CLASS_WIDE
;;;
;;;     GTK_STYLE_REGION_COLUMN
;;;     GTK_STYLE_REGION_COLUMN_HEADER
;;;     GTK_STYLE_REGION_ROW
;;;     GTK_STYLE_REGION_TAB
;;;
;;; Functions
;;;
;;;     gtk_style_context_new
;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_get                              missing
;;;     gtk_style_context_get_direction                    Accessor
;;;     gtk_style_context_get_junction_sides
;;;     gtk_style_context_get_parent                       Accessor
;;;     gtk_style_context_get_path
;;;     gtk_style_context_get_property
;;;     gtk_style_context_get_screen                       Accessor
;;;     gtk_style_context_get_frame_clock
;;;     gtk_style_context_get_state
;;;     gtk_style_context_get_style                        missing
;;;     gtk_style_context_get_style_property
;;;     gtk_style_context_get_style_valist                 missing
;;;     gtk_style_context_get_valist                       missing
;;;     gtk_style_context_get_section
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_background_color
;;;     gtk_style_context_get_border_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_get_font
;;;     gtk_style_context_invalidate
;;;     gtk_style_context_state_is_running                 missing / deprecated
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_lookup_icon_set
;;;     gtk_style_context_notify_state_change
;;;     gtk_style_context_pop_animatable_region                      deprecated
;;;     gtk_style_context_push_animatable_region                     deprecated
;;;     gtk_style_context_cancel_animations                missing / deprecated
;;;     gtk_style_context_scroll_animations                missing / deprecated
;;;     gtk_style_context_remove_provider
;;;     gtk_style_context_remove_provider_for_screen
;;;     gtk_style_context_reset_widgets
;;;     gtk_style_context_set_background
;;;     gtk_style_context_restore
;;;     gtk_style_context_save
;;;     gtk_style_context_set_direction                    Accessor
;;;     gtk_style_context_set_junction_sides
;;;     gtk_style_context_set_parent                       Accessor
;;;     gtk_style_context_set_path
;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_list_classes
;;;     gtk_style_context_add_region
;;;     gtk_style_context_remove_region
;;;     gtk_style_context_has_region
;;;     gtk_style_context_list_regions
;;;     gtk_style_context_set_screen                       Accessor
;;;     gtk_style_context_set_frame_clock
;;;     gtk_style_context_set_state
;;;     gtk_style_context_set_scale
;;;     gtk_style_context_get_scale
;;;     gtk_style_context_to_string
;;;
;;;     GtkBorder
;;;
;;;     gtk_border_new
;;;     gtk_border_copy
;;;     gtk_border_free
;;;
;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_background_get_clip
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_extension
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_frame_gap
;;;     gtk_render_handle
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option
;;;     gtk_render_slider
;;;     gtk_render_activity
;;;     gtk_render_icon_pixbuf
;;;     gtk_render_icon_surface
;;;     gtk_render_icon
;;;     gtk_render_insertion_cursor
;;;
;;; Properties
;;;
;;;  GtkTextDirection    direction      Read / Write
;;;     GdkFrameClock*   paint-clock    Read / Write
;;;   GtkStyleContext*   parent         Read / Write
;;;         GdkScreen*   screen         Read / Write
;;;
;;; Signals
;;;
;;;              void    changed        Run First
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkBorder
;;;
;;;     GObject
;;;     ╰── GtkStyleContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; Search a better place
(glib-init::at-init () (foreign-funcall "gtk_ui_manager_get_type" g-size))

;;; ----------------------------------------------------------------------------
;;; enum GtkJunctionSides
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkJunctionSides" gtk-junction-sides
  (:export t
   :type-initializer "gtk_junction_sides_get_type")
  (:none 0)
  (:corner-topleft #.(ash 1 0))
  (:corner-topright #.(ash 1 1))
  (:corner-bottomleft #.(ash 1 2))
  (:corner-bottomright #.(ash 1 3))
  (:top 3)
  (:left 5)
  (:bottom 6)
  (:right 10))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-junction-sides atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-junction-sides atdoc:*external-symbols*)
 "@version{2020-2-28}
  @begin{short}
    Describes how a rendered element connects to adjacent elements.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkJunctionSides\" gtk-junction-sides
  (:export t
   :type-initializer \"gtk_junction_sides_get_type\")
  (:none 0)
  (:corner-topleft #.(ash 1 0))
  (:corner-topright #.(ash 1 1))
  (:corner-bottomleft #.(ash 1 2))
  (:corner-bottomright #.(ash 1 3))
  (:top 3)
  (:left 5)
  (:bottom 6)
  (:right 10))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No junctions.}
    @entry[:corner-topleft]{Element connects on the top-left corner.}
    @entry[:corner-topright]{Element connects on the top-right corner.}
    @entry[:corner-bottomleft]{Element connects on the bottom-left corner.}
    @entry[:corner-bottomright]{Element connects on the bottom-right corner.}
    @entry[:top]{Element connects on the top side.}
    @entry[:bottom]{Element connects on the bottom side.}
    @entry[:left]{Element connects on the left side.}
    @entry[:right]{Element connects on the right side.}
  @end{table}
  @see-class{gtk-style-context}")

;;; ----------------------------------------------------------------------------
;;; enum GtkStyleContextPrintFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkStyleContextPrintFlags" gtk-style-context-print-flags
  (:export t
   :type-initializer "gtk_style_context_print_flags_get_type")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-print-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-style-context-print-flags atdoc:*external-symbols*)
 "@version{2020-2-28}
  @begin{short}
    Flags that modify the behavior of the @fun{gtk-style-context-to-string}
    function.
  @end{short}
  New values may be added to this enumeration.
  @begin{pre}
(define-g-flags \"GtkStyleContextPrintFlags\" gtk-style-context-print-flags
  (:export t
   :type-initializer \"gtk_style_context_print_flags_get_type\")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{}
    @entry[:recurse]{Print the entire tree of CSS nodes starting at the style
      context's node.}
    @entry[:show-style]{Show the values of the CSS properties for each node.}
  @end{table}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-to-string}")

;;; ----------------------------------------------------------------------------
;;; enum GtkBorderStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkBorderStyle" gtk-border-style
  (:export t
   :type-initializer "gtk_border_style_get_type")
  :none
  :solid
  :inset
  :outset
  :hidden
  :dotted
  :dashed
  :double
  :groove
  :ridge)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-style atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-border-style atdoc:*external-symbols*)
 "@version{2020-2-28}
  @begin{short}
    Describes how the border of a UI element should be rendered.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkBorderStyle\" gtk-border-style
  (:export t
   :type-initializer \"gtk_border_style_get_type\")
  :none
  :solid
  :inset
  :outset
  :hidden
  :dotted
  :dashed
  :double
  :groove
  :ridge)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No visible border.}
    @entry[:solid]{A single line segment.}
    @entry[:inset]{Looks as if the content is sunken into the canvas.}
    @entry[:outset]{Looks as if the content is coming out of the canvas.}
    @entry[:hidden]{Same as @code{:none}.}
    @entry[:dotted]{A series of round dots.}
    @entry[:dashed]{A series of square-ended dashes.}
    @entry[:double]{Two parrallel lines with some space between them.}
    @entry[:groove]{Looks as if it were carved in the canvas.}
    @entry[:ridge]{Looks as if it were coming out of the canvas.}
  @end{table}
  @see-class{gtk-style-context}")

;;; ----------------------------------------------------------------------------
;;; struct GtkBorder
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_border_get_type" g-size))

(define-g-boxed-cstruct gtk-border "GtkBorder"
  (left   :int16 :initform 0)
  (right  :int16 :initform 0)
  (top    :int16 :initform 0)
  (bottom :int16 :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-border 'type)
 "@version{2020-9-13}
  @begin{short}
    A structure that specifies a border around a rectangular area that can be
    of different width on each side.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gtk-border \"GtkBorder\"
  (left   :int16 :initform 0)
  (right  :int16 :initform 0)
  (top    :int16 :initform 0)
  (bottom :int16 :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[left]{The width of the left border.}
    @entry[right]{The width of the right border.}
    @entry[top]{The width of the top border.}
    @entry[bottom]{The width of the bottom border.}
  @end{table}
  @see-constructor{make-gtk-border}
  @see-constructor{copy-gtk-border}
  @see-slot{gtk-border-left}
  @see-slot{gtk-border-right}
  @see-slot{gtk-border-top}
  @see-slot{gtk-border-bottom}")

(export (boxed-related-symbols 'gtk-border))

;;; ----------------------------------------------------------------------------
;;; Constructors for GtkBorder
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gtk-border 'function)
 "@version{2013-8-27}
  Creates and returns an structure of type @class{gtk-border}.
  @see-class{gtk-border}
  @see-function{copy-gtk-border}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gtk-border 'function)
 "@version{2013-8-27}
  Copies and returns an structure of type @class{gtk-border}.
  @see-class{gtk-border}")

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkBorder
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-left atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-left 'function)
 "@version{2013-8-27}
  Accessor of the @code{left} slot of the @class{gtk-border} structure.
  @see-class{gtk-border}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-right atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-right 'function)
 "@version{2013-8-27}
  Accessor of the @code{right} slot of the @class{gtk-border} structure.
  @see-class{gtk-border}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-top atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-top 'function)
 "@version{2013-8-27}
  Accessor of the @code{top} slot of the @class{gtk-border} structure.
  @see-class{gtk-border}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-bottom atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-bottom 'function)
 "@version{2013-8-27}
  Accessor of the @code{bottom} slot of the @class{gtk-border} structure.
  @see-class{gtk-border}")

;;; ----------------------------------------------------------------------------
;;; GtkStyleContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStyleContext" gtk-style-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_style_context_get_type")
  ((direction
    gtk-style-context-direction
    "direction" "GtkTextDirection" t t)
   (paint-clock
    gtk-style-context-paint-clock
    "paint-clock" "GdkFrameClock" t t)
   (parent
    gtk-style-context-parent
    "parent" "GtkStyleContext" t t)
   (screen
    gtk-style-context-screen
    "screen" "GdkScreen" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-style-context 'type)
 "@version{2020-3-2}
  @begin{short}
    @sym{gtk-style-context} is an object that stores styling information
    affecting a widget defined by @class{gtk-widget-path}.
  @end{short}

  In order to construct the final style information, @sym{gtk-style-context}
  queries information from all attached style providers of type
  @class{gtk-style-provider}. Style providers can be either attached explicitly
  to the context through the @fun{gtk-style-context-add-provider} function, or
  to the screen through the @fun{gtk-style-context-add-provider-for-screen}
  function. The resulting style is a combination of all providers' information
  in priority order.

  For GTK+ widgets, any @sym{gtk-style-context} returned by the
  @fun{gtk-widget-style-context} funcion will already have a
  @class{gtk-widget-path}, a @class{gdk-screen} object and a text direction
  @code{RTL/LTR} information set. The style context will be also updated
  automatically if any of these settings change on the widget.

  If you are using the theming layer standalone, you will need to set a widget
  path and a screen yourself to the created style context through the
  @fun{gtk-style-context-path} and @fun{gtk-style-context-screen} functions.
  See the \"Foreign drawing\" example in gtk3-demo.

  @subheading{Style Classes}
  Widgets can add style classes to their style context, which can be used to
  associate different styles by class. The documentation for individual widgets
  lists which style classes it uses itself, and which style classes may be added
  by applications to affect their appearance. GTK+ defines macros for a number
  of style classes.

  @subheading{Style Regions}
  Widgets can also add regions with flags to their style context. This feature
  is deprecated and will be removed in a future GTK+ update. Please use style
  classes instead. GTK+ defines macros for a number of style regions.

  @subheading{Custom styling in UI libraries and applications}
  If you are developing a library with custom GtkWidgets that render differently
  than standard components, you may need to add a @class{gtk-style-provider}
  object yourself with the priority
  @var{+gtk-style-provider-priority-fallback+}, either a
  @class{gtk-css-provider} object or a custom object implementing the
  @class{gtk-style-provider} interface. This way themes may still attempt to
  style your UI elements in a different way if needed so.

  If you are using custom styling on an applications, you probably want then to
  make your style information prevail to the theme’s, so you must use a
  @class{gtk-style-provider} with the priority
  @var{+gtk-style-provider-priority-application+}, keep in mind that the user
  settings in @file{XDG_CONFIG_HOME/gtk-3.0/gtk.css} will still take precedence
  over your changes, as it uses the priority
  @var{+gtk-style-provider-priority-user+}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (stylecontext)    : Run First
      @end{pre}
      The \"changed signal\" is emitted when there is a change in the style
      context. For a style context returned by the function
      @fun{gtk-widget-style-context}, the \"style-updated\" signal might be
      more convenient to use. This signal is useful when using the theming layer
      standalone.
      @begin[code]{table}
        @entry[stylecontext]{The @sym{gtk-style-context} object which received
          the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-style-context-direction}
  @see-slot{gtk-style-context-paint-clock}
  @see-slot{gtk-style-context-parent}
  @see-slot{gtk-style-context-screen}
  @see-class{gtk-style-provider}
  @see-class{gtk-css-provider}
  @see-class{gtk-widget-path}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-style-context-direction --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "direction"
                                               'gtk-style-context) 't)
 "The @code{direction} property of type @symbol{gtk-text-direction}
  (Read / Write) @br{}
  Text direction. @br{}
  Default value: @code{:ltr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-direction 'function)
 "@version{2020-3-2}
  @syntax[]{(gtk-style-context-direction object) => direction}
  @syntax[]{(setf (gtk-style-context-direction object) direction)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[direction]{a direction of type @symbol{gtk-text-direction}}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{direction} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The slot access function @sym{gtk-style-context-direction} returns the widget
  direction used for rendering. The slot access function
  @sym{(setf gtk-style-context-direction)} sets the reading direction for
  rendering purposes.

  If you are using a style context returned from the function
  @fun{gtk-widget-style-context}, you do not need to call this yourself.
  @begin[Warning]{dictionary}
    The slot access function @sym{gtk-style-context-direction} has been
    deprecated since version 3.8 and should not be used in newly-written code.
    Use the function @fun{gtk-style-context-state} and check for @code{:dir-ltr}
    and @code{:dir-rtl} instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-text-direction}
  @see-function{gtk-style-context-state}
  @see-function{gtk-widget-style-context}")

;;; --- gtk-style-context-paint-clock ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paint-clock"
                                               'gtk-style-context) 't)
 "The @code{paint-clock} property of type @class{gdk-frame-clock} (Read / Write)
  @br{}
  The associated frame clock object.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-paint-clock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-paint-clock 'function)
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-paint-clock object) => paint-clock}
  @syntax[]{(setf (gtk-style-context-paint-clock object) paint-clock)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[paint-clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{paint-clock} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The associated frame clock object.
  @see-class{gtk-style-context}
  @see-class{gdk-frame-clock}")

;;; --- gtk-style-context-parent -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parent" 'gtk-style-context) 't)
 "The @code{parent} property of type @class{gtk-style-context}
  (Read / Write) @br{}
  Sets or gets the style context's parent. See the slot access function
  @fun{gtk-style-context-parent} for details.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-parent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-parent 'function)
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-parent object) => parent}
  @syntax[]{(setf (gtk-style-context-direction object) parent)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[parent]{a @class{gtk-style-context} parent or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{parent} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The slot access function @sym{gtk-style-context-parent} gets the parent style
  context. The slot access function @sym{(setf gtk-style-context-parent)} sets
  the parent style context.

  The parent style context is used to implement inheritance of properties.
  If you are using a style context returned from the function
  @fun{gtk-widget-style-context}, the parent will be set for you.
  @see-class{gtk-style-context}
  @see-function{gtk-widget-style-context}")

;;; --- gtk-style-context-screen -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-style-context) 't)
 "The @code{screen} property of type @class{gdk-screen} (Read / Write) @br{}
  The associated screen object.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-screen 'function)
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-screen object) => screen}
  @syntax[]{(setf (gtk-style-context-direction object) screen)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{screen} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The slot access function @sym{gtk-style-context-screen} returns the screen to
  which the style context is attached. The slot access function
  @sym{(setf gtk-style-context-screen)} attaches the style context to the given
  screen.

  The screen is used to add style information from 'global' style providers,
  such as the screens @class{gtk-settings} instance.

  If you are using a style context returned from the function
  @fun{gtk-widget-style-context}, you do not need to call this yourself.
  @see-class{gtk-style-context}
  @see-class{gdk-screen}
  @see-class{gtk-settings}
  @see-function{gtk-widget-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-style-context-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @return{A newly created @class{gtk-style-context} object.}
  @begin{short}
    Creates a standalone style context object.
  @end{short}
  This style context won't be attached to any widget, so you may want to call
  the function @fun{gtk-style-context-path} yourself.
  @begin[Note]{dictionary}
    This function is only useful when using the theming layer separated from
    GTK+, if you are using a style context to theme GtkWidgets, use
    the function @fun{gtk-widget-style-context} in order to get a style
    context ready to theme the widget.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-path}
  @see-function{gtk-widget-style-context}"
  (make-instance 'gtk-style-context))

(export 'gtk-style-context-new)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_provider" gtk-style-context-add-provider) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @argument[priority]{the priority of type @code{:uint} of the style provider}
  @begin{short}
    Adds a style provider to the style context, to be used in style
    construction.
  @end{short}

  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  priorities @var{+gtk-style-provider-priority-fallback+} and
  @var{+gtk-style-provider-priority-user+}.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through this
    function takes precedence over another added through the function
    @fun{gtk-style-context-add-provider-for-screen}.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-style-provider}
  @see-function{gtk-style-context-add-provider-for-screen}"
  (context (g-object gtk-style-context))
  (provider (g-object gtk-style-provider))
  (priority :uint))

(export 'gtk-style-context-add-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_provider_for_screen"
           gtk-style-context-add-provider-for-screen) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @argument[priority]{the priority of type @code{:uint} of the style provider}
  @begin{short}
    Adds a global style provider to the screen, which will be used in style
    construction for all style contexts under the screen.
  @end{short}

  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  priorities @var{+gtk-style-provider-priority-fallback+} and
  @var{+gtk-style-provider-priority-user+}.

  GTK+ uses this to make styling information from @class{gtk-settings}
  available.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through the
    function @fun{gtk-style-context-add-provider} takes precedence over another
    added through this function.
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gtk-style-context}
  @see-class{gtk-style-provider}
  @see-class{gtk-settings}
  @see-function{gtk-style-context-add-provider}"
  (screen (g-object gdk-screen))
  (provider (g-object gtk-style-provider))
  (priority :uint))

(export 'gtk-style-context-add-provider-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get ()
;;;
;;; void gtk_style_context_get (GtkStyleContext *context,
;;;                             GtkStateFlags state,
;;;                             ...);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the property values for
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_junction_sides ()
;;; gtk_style_context_set_junction_sides () -> gtk-style-context-junction-sides
;;; ----------------------------------------------------------------------------

(defun (setf gtk-style-context-junction-sides) (sides context)
  (foreign-funcall "gtk_style_context_set_junction_sides"
                   (g-object gtk-style-context) context
                   gtk-junction-sides sides
                   :void)
  sides)

(defcfun ("gtk_style_context_get_junction_sides"
           gtk-style-context-junction-sides) gtk-junction-sides
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-junction-sides context) => sides}
  @syntax[]{(setf (gtk-style-context-junction-sides context) sides)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[sides]{sides of type @symbol{g-junction-sides}}
  @begin{short}
    Accessor of the junction sides of a style context.
  @end{short}

  The slot access function @sym{gtk-style-context-junction-sides} returns the
  sides where rendered elements connect visually with others. The slot access
  function @sym{(setf gtk-style-context-function-sides)} sets the sides where
  rendered elements, mostly through the function @fun{gtk-render-frame}, will
  visually connect with other visual elements.

  This is merely a hint that may or may not be honored by theming engines.

  Container widgets are expected to set junction hints as appropriate for their
  children, so it should not normally be necessary to call this function
  manually.
  @see-class{gtk-style-context}
  @see-symbol{gtk-junction-sides}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-junction-sides)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_path ()
;;; gtk_style_context_set_path () -> gtk-style-context-path
;;; ----------------------------------------------------------------------------

(defun (setf gtk-style-context-path) (path context)
  (foreign-funcall "gtk_style_context_set_path"
                   (g-object gtk-style-context) context
                   (g-boxed-foreign gtk-widget-path) path
                   :void)
  path)

(defcfun ("gtk_style_context_get_path" gtk-style-context-path)
    (g-boxed-foreign gtk-widget-path)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-path context) => path}
  @syntax[]{(setf (gtk-style-context-path context) path)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[path]{a @class{gtk-widget-path} structure}
  @begin{short}
    Accessor of the widget path of the context.
  @end{short}

  The slot access function @sym{gtk-style-context-path} returns the widget
  path used for style matching. The slot access function
  @sym{(setf gtk-style-context-path)} sets the widget path used for style
  matching. As a consequence, the style will be regenerated to match the new
  given path.

  If you are using a style context returned from the function
  @fun{gtk-widget-style-context}, you do not need to call this yourself.
  @see-class{gtk-style-context}
  @see-class{gtk-widget-path}
  @see-function{gtk-widget-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-path)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_property () -> gtk-style-context-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_property" %gtk-style-context-property)
    :void
  (context (g-object gtk-style-context))
  (property :string)
  (state gtk-state-flags)
  (value (:pointer (:struct g-value))))

(defun gtk-style-context-property (context property state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[property]{a @code{:string} with a style property name}
  @argument[state]{a state of type @symbol{gtk-state-flags} to retrieve the
    property value for}
  @return{The value for the style property.}
  @begin{short}
    Gets a style property from the context for the given state.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (setq context (gtk-style-context-new))
=> #<GTK-STYLE-CONTEXT {100687D223@}>
 (gtk-style-context-property context \"color\" :normal)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 1.0d0 :BLUE 1.0d0 :ALPHA 1.0d0)
 (gtk-style-context-property context \"opacity\" :normal)
=> 1.0d0
 (gtk-style-context-property context \"font\" :normal)
=> #<PANGO-FONT-DESCRIPTION {100687E0B3@}>
 (pango-font-description-to-string *)
=> \"Ubuntu 11\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value)
    (prog2
      (%gtk-style-context-property context property state value)
      ;; TODO: Handle the case für an invalid property
      (when value
        (parse-g-value value))
      (g-value-unset value))))

(export 'gtk-style-context-property)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_frame_clock ()
;;; gtk_style_context_set_frame_clock () -> gtk-style-context-frame-clock
;;; ----------------------------------------------------------------------------

(defun (setf gtk-style-context-frame-clock) (frame-clock context)
  (foreign-funcall "gtk_style_context_set_frame_clock"
                   (g-object gtk-style-context) context
                   (g-object gdk-frame-clock) frame-clock
                   :void)
  frame-clock)

(defcfun ("gtk_style_context_get_frame_clock" gtk-style-context-frame-clock)
    (g-object gdk-frame-clock)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-frame-clock context) => frame-clock}
  @syntax[]{(setf (gtk-style-context-frame-clock context) frame-clock)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Accessor of the @class{gdk-frame-clock} object of the style context.
  @end{short}

  The slot access function @sym{gtk-style-context-frame-clock} returns the
  @class{gdk-frame-clock} to which the style context is attached. The slot
  access function @sym{(setf gtk-style-context-frame-clock)} attaches the style
  context to the given frame clock.

  The frame clock is used for the timing of animations. If you are using a style
  context returned from the function @fun{gtk-widget-style-context}, you do
  not need to call this yourself.
  @see-class{gtk-style-context}
  @see-class{gdk-frame-clock}
  @see-function{gtk-widget-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-frame-clock)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_state ()
;;; gtk_style_context_set_state () -> gtk-style-context-state
;;; ----------------------------------------------------------------------------

(defun (setf gtk-style-context-state) (state context)
  (foreign-funcall "gtk_style_context_set_state"
                   (g-object gtk-style-context) context
                   gtk-state-flags state
                   :void)
  state)

(defcfun ("gtk_style_context_get_state" gtk-style-context-state) gtk-state-flags
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-state context) => state}
  @syntax[]{(setf (gtk-style-context-state context) state)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to represent}
  @begin{short}
    Accessor of the state used when rendering.
  @end{short}

  The function @sym{gtk-style-context-state} returns the state used when
  rendering. The function @sym{(setf gtk-style-context-state)} sets the state
  to be used when rendering with any of the @sym{gtk-render-*} functions.
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-state)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style ()
;;;
;;; void gtk_style_context_get_style (GtkStyleContext *context, ...);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_property () -> gtk-style-context-style-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_style_property"
          %gtk-style-context-style-property) :void
  (context (g-object gtk-style-context))
  (property-name :string)
  (value (:pointer (:struct g-value))))

(defun gtk-style-context-style-property (context widget property-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[widget]{a @class{gtk-widget} the style property is looked up for}
  @argument[property-name]{a @code{:string} with the name of the widget style
    property}
  @return{Returns the value of the style property.}
  @begin{short}
    Gets the value for a widget style property.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (setq message (make-instance 'gtk-message-dialog))
=> #<GTK-MESSAGE-DIALOG {100577F4A3@}>
 (setq context (gtk-widget-style-context message))
=> #<GTK-STYLE-CONTEXT {10057DE323@}>
 (gtk-style-context-style-property context message \"message-border\")
=> 12
    @end{pre}
  @end{dictionary}
  @see-class{gtk-style-context}"
  (let ((gtype (g-param-spec-value-type
                   (gtk-widget-class-find-style-property
                       (g-type-from-instance widget)
                       property-name))))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value gtype)
      (prog2
        (%gtk-style-context-style-property context property-name value)
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gtk-style-context-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_valist ()
;;;
;;; void gtk_style_context_get_style_valist (GtkStyleContext *context,
;;;                                          va_list args);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_valist ()
;;;
;;; void gtk_style_context_get_valist (GtkStyleContext *context,
;;;                                    GtkStateFlags state,
;;;                                    va_list args);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the property values for
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_section () -> gtk-style-context-section
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_section" gtk-style-context-section)
    (g-boxed-foreign gtk-css-section)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[property]{a string with the name of the style property}
  @return{Returns @code{nil} or the section where the property was defined.}
  @begin{short}
    Queries the location in the CSS where @arg{property} was defined for the
    current style context.
  @end{short}
  Note that the state to be queried is taken from the function
  @fun{gtk-style-context-state}.

  If the location is not available, @code{nil} will be returned. The location
  might not be available for various reasons, such as the property being
  overridden, property not naming a supported CSS property or tracking of
  definitions being disabled for performance reasons.

  Shorthand CSS properties cannot be queried for a location and will always
  return @code{nil}.
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (property g-string))

(export 'gtk-style-context-section)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_color () -> gtk-style-context-color
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_color" %gtk-style-context-color) :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (color (g-boxed-foreign gdk-rgba)))

(defun gtk-style-context-color (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the color
    for}
  @return{The @class{gdk-rgba} foreground color.}
  @begin{short}
    Gets the foreground color for a given state.
  @end{short}
  See the function @fun{gtk-style-context-property} for details.
  @begin[Example]{dictionary}
    @begin{pre}
 (setq context (gtk-style-context-new))
=> #<GTK-STYLE-CONTEXT {10058ED093@}>
 (gtk-style-context-color context :normal)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 1.0d0 :BLUE 1.0d0 :ALPHA 1.0d0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}
  @see-class{gdk-rgba}
  @see-function{gtk-style-context-property}"
  (let ((color (make-gdk-rgba)))
    (%gtk-style-context-color context state color)
    color))

(export 'gtk-style-context-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_background_color ()
;;; -> gtk-style-context-background-color
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_background_color"
          %gtk-style-context-background-color) :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (color (g-boxed-foreign gdk-rgba)))

(defun gtk-style-context-background-color (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the color
    for}
  @return{Returns the @class{gdk-rgba} background color.}
  @begin{short}
    Gets the background color for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-background-color} has been deprecated
    since version 3.16 and should not be used in newly-written code. Use the
    function @fun{gtk-render-background} instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}
  @see-class{gdk-rgba}
  @see-function{gtk-render-background}"
  (let ((color (make-gdk-rgba)))
    (%gtk-style-context-background-color context state color)
    color))

(export 'gtk-style-context-background-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border_color () -> gtk-style-context-border-color
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_border_color" %gtk-style-context-border-color)
    :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (color (g-boxed-foreign gdk-rgba)))

(defun gtk-style-context-border-color (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the color
    for}
  @return{Returns the @class{gdk-rgba} border color.}
  @begin{short}
    Gets the border color for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-border-color} has been deprecated since
    version 3.16 and should not be used in newly-written code. Use the function
    @fun{gtk-render-frame} instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}
  @see-class{gdk-rgba}
  @see-function{gtk-render-frame}"
  (let ((color (make-gdk-rgba)))
    (%gtk-style-context-border-color context state color)
    color))

(export 'gtk-style-context-border-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border () -> gtk-style-context-border
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_border" %gtk-style-context-border) :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (border (g-boxed-foreign gtk-border)))

(defun gtk-style-context-border (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the
    border for}
  @return{Returns border settings as a @class{gtk-border} structure.}
  @begin{short}
    Gets the value for the border settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((border (make-gtk-border)))
    (%gtk-style-context-border context state border)
    border))

(export 'gtk-style-context-border)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_padding () -> gtk-style-context-padding
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_padding" %gtk-style-context-padding) :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (padding (g-boxed-foreign gtk-border)))

(defun gtk-style-context-padding (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the
    padding for}
  @return{Returns padding settings as a @class{gtk-border} structure.}
  @begin{short}
    Gets the value for the padding settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((padding (make-gtk-border)))
    (%gtk-style-context-padding context state padding)
    padding))

(export 'gtk-style-context-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_margin () -> gtk-style-context-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_margin" %gtk-style-context-margin) :void
  (context (g-object gtk-style-context))
  (state gtk-state-flags)
  (margin (g-boxed-foreign gtk-border)))

(defun gtk-style-context-margin (context state)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the
    margin for}
  @return{Returns margin settings as a @class{gtk-border} structure.}
  @begin{short}
    Gets the value for the margin settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((margin (make-gtk-border)))
    (%gtk-style-context-margin context state margin)
    margin))

(export 'gtk-style-context-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_font () -> gtk-style-context-font
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_font" gtk-style-context-font)
    (g-boxed-foreign pango-font-description)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{state of type @symbol{gtk-state-flags} to retrieve the
    font for}
  @return{Returns a @class{pango-font-description} structure for the given
    state.}
  @begin{short}
    Returns the font description for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-font} has been deprecated since version
    3.8 and should not be used in newly-written code. Use the function
    @fun{gtk-style-context-property} for \"font\" or subproperties instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-state-flags}
  @see-function{gtk-style-context-property}
  @see-class{pango-font-description}"
  (context (g-object gtk-style-context))
  (state gtk-state-flags))

(export 'gtk-style-context-font)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_invalidate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_invalidate" gtk-style-context-invalidate) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Invalidates style context information, so it will be reconstructed again.
  @end{short}

  If you are using a style context returned from the function
  @fun{gtk-widget-style-context}, you do not need to call this yourself.
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-invalidate} has been deprecated since
    version 3.12 and should not be used in newly-written code. Style contexts
    are invalidated automatically.
  @end{dictionary}
  @see-class{gtk-widget}
  @see-function{gtk-widget-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-invalidate)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_state_is_running ()
;;;
;;; gboolean gtk_style_context_state_is_running (GtkStyleContext *context,
;;;                                              GtkStateType state,
;;;                                              gdouble *progress);
;;;
;;; Returns TRUE if there is a transition animation running for the current
;;; region (see gtk_style_context_push_animatable_region()).
;;;
;;; If progress is not NULL, the animation progress will be returned there, 0.0
;;; means the state is closest to being unset, while 1.0 means it's closest to
;;; being set. This means transition animation will run from 0 to 1 when state
;;; is being set and from 1 to 0 when it's being unset.
;;;
;;; Warning
;;;
;;; gtk_style_context_state_is_running has been deprecated since version 3.6 and
;;; should not be used in newly-written code.
;;;
;;; This function always returns FALSE
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     a widget state
;;;
;;; progress :
;;;     return location for the transition progress
;;;
;;; Returns :
;;;     TRUE if there is a running transition animation for state.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_lookup_color" %gtk-style-context-lookup-color)
    :boolean
  (context (g-object gtk-style-context))
  (color-name :string)
  (color (g-boxed-foreign gdk-rgba)))

(defun gtk-style-context-lookup-color (context color-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-3}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[color-name]{a string with a color name to lookup}
  @return{The looked up @class{gdk-rgba} color, or @code{nil}.}
  @begin{short}
    Looks up and resolves a color name in the style context color map.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gdk-rgba}"
  (let ((color (make-gdk-rgba)))
    (when (%gtk-style-context-lookup-color context color-name color)
      color)))

(export 'gtk-style-context-lookup-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_lookup_icon_set" gtk-style-context-lookup-icon-set)
    (g-boxed-foreign gtk-icon-set)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[stock-id]{a string with an icon name}
  @return{The looked up @class{gtk-icon-set} object, or @code{nil}.}
  @begin{short}
    Looks up @arg{stock-id} in the icon factories associated to the style
    context and the default icon factory, returning an icon set if found,
    otherwise @code{nil}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-lookup-icon-set} has been deprecated
    since version 3.10 and should not be used in newly-written code. Use the
    function @fun{gtk-icon-theme-lookup-icon} instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-icon-set}
  @see-function{gtk-icon-theme-lookup-icon}"
  (context (g-object gtk-style-context))
  (stock-id :string))

(export 'gtk-style-context-lookup-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_notify_state_change ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_notify_state_change"
           gtk-style-context-notify-state-change) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[window]{a @class{gdk-window} object}
  @argument[region-id]{a pointer to the animatable region to notify on, or
    @code{NULL}}
  @argument[state]{state of type @symbol{gtk-state-type} to trigger transition
    for}
  @argument[state-value]{@em{true} if @arg{state} is the state we are changing
    to, @em{false} if we are changing away from it}
  @begin{short}
    Notifies a state change on the style context.
  @end{short}
  So if the current style makes use of transition animations, one will be
  started so all rendered elements under @arg{region-id} are animated for the
  state being set to value @arg{state-value}.

  The window parameter is used in order to invalidate the rendered area as the
  animation runs, so make sure it is the same window that is being rendered on
  by the @sym{gtk-render-*} functions.

  If @arg{region-id} is @code{NULL}, all rendered elements using the style
  context will be affected by this state transition.
  @begin[Example]{dictionary}
    As a practical example, a button notifying a state transition on the
    prelight state:
    @begin{pre}
 gtk_style_context_notify_state_change (context,
                                        gtk_widget_get_window (widget),
                                        NULL,
                                        GTK_STATE_PRELIGHT,
                                        button->in_button);
    @end{pre}
    Can be handled in the CSS file like this:
    @begin{pre}
   GtkButton {
       background-color: #f00
   @}

   GtkButton:hover {
       background-color: #fff;
       transition: 200ms linear
   @}
    @end{pre}
    This combination will animate the button background from red to white if a
    pointer enters the button, and back to red if the pointer leaves the button.

    Note that state is used when finding the transition parameters, which is why
    the style places the transition under the @code{:hover} pseudo-class.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-notify-state-change} has been deprecated
    since version 3.6 and should not be used in newly-written code. This
    function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gdk-window}
  @see-symbol{gtk-state-type}
  @see-function{gtk-style-context-push-animatable-region}"
  (context (g-object gtk-style-context))
  (window (g-object gdk-window))
  (region-id :pointer)
  (state gtk-state-type)
  (state-value :boolean))

(export 'gtk-style-context-notify-state-change)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_pop_animatable_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_pop_animatable_region"
           gtk-style-context-pop-animatable-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Pops an animatable region from context.
  @end{short}
  See the functoin @fun{gtk-style-context-push-animatable-region}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-pop-animatable-region} has been
    deprecated since version 3.6 and should not be used in newly-written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-push-animatable-region}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-pop-animatable-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_push_animatable_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_push_animatable_region"
           gtk-style-context-push-animatable-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[region-id]{a pointer which is an unique identifier for the
    animatable region}
  @begin{short}
    Pushes an animatable region, so all further @sym{gtk-render-*} calls between
    this call and the following @fun{gtk-style-context-pop-animatable-region}
    will potentially show transition animations for this region.
  @end{short}
  If the function @fun{gtk-style-context-notify-state-change} is called for a
  given state, and the current theme/style defines transition animations for
  state changes.

  The @arg{region-id} used must be unique in the style context so the theming
  engine can uniquely identify rendered elements subject to a state transition.
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-push-animatable-region} has been
    deprecated since version 3.6 and should not be used in newly-written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-pop-animatable-region}
  @see-function{gtk-style-context-notify-state-change}"
  (context (g-object gtk-style-context))
  (region-id :pointer))

(export 'gtk-style-context-push-animatable-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_cancel_animations ()
;;;
;;; void gtk_style_context_cancel_animations (GtkStyleContext *context,
;;;                                           gpointer region_id);
;;;
;;; Stops all running animations for region_id and all animatable regions
;;; underneath.
;;;
;;; A NULL region_id will stop all ongoing animations in context, when dealing
;;; with a GtkStyleContext obtained through gtk_widget_get_style_context(), this
;;; is normally done for you in all circumstances you would expect all widget to
;;; be stopped, so this should be only used in complex widgets with different
;;; animatable regions.
;;;
;;; Warning
;;;
;;; gtk_style_context_cancel_animations has been deprecated since version 3.6
;;; and should not be used in newly-written code.
;;;
;;; This function does nothing.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_id :
;;;     animatable region to stop, or NULL. See
;;;     gtk_style_context_push_animatable_region().
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_scroll_animations ()
;;;
;;; void gtk_style_context_scroll_animations (GtkStyleContext *context,
;;;                                           GdkWindow *window,
;;;                                           gint dx,
;;;                                           gint dy);
;;;
;;; This function is analogous to gdk_window_scroll(), and should be called
;;; together with it so the invalidation areas for any ongoing animation are
;;; scrolled together with it.
;;;
;;; Warning
;;;
;;; gtk_style_context_scroll_animations has been deprecated since version 3.6
;;; and should not be used in newly-written code.
;;;
;;; This function does nothing.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; window :
;;;     a GdkWindow used previously in gtk_style_context_notify_state_change()
;;;
;;; dx :
;;;     Amount to scroll in the X axis
;;;
;;; dy :
;;;     Amount to scroll in the Y axis
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_provider" gtk-style-context-remove-provider)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-1}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @begin{short}
    Removes the provider from the style providers list in the style context.
  @end{short}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-add-provider}"
  (context (g-object gtk-style-context))
  (provider (g-object gtk-style-provider)))

(export 'gtk-style-context-remove-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_provider_for_screen"
           gtk-style-context-remove-provider-for-screen) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-1}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @begin{short}
    Removes the provider from the global style providers list in the screen.
  @end{short}
  @see-class{gdk-screen}
  @see-function{gtk-style-context-add-provider-for-screen}"
  (screen (g-object gdk-screen))
  (provider (g-object gtk-style-provider)))

(export 'gtk-style-context-remove-provider-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_reset_widgets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_reset_widgets" gtk-style-context-reset-widgets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-27}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    This function recomputes the styles for all widgets under a particular
    screen.
  @end{short}
  This is useful when some global parameter has changed that affects the
  appearance of all widgets, because when a widget gets a new style, it will
  both redraw and recompute any cached information about its appearance. As an
  example, it is used when the color scheme changes in the related
  @class{gtk-settings} object.
  @see-class{gtk-style-context}
  @see-class{gtk-settings}
  @see-class{gdk-screen}"
  (screen (g-object gdk-screen)))

(export 'gtk-style-context-reset-widgets)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_set_background" gtk-style-context-set-background)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the background of the window to the background pattern or color
    specified in the style context for its current state.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-set-background} has been deprecated
    since version 3.18 and should not be used in newly-written code. Use the
    function @fun{gtk-render-background} instead. Note that clients still using
    this function are now responsible for calling this function again whenever
    the style context is invalidated.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gdk-window}
  @see-function{gtk-render-background}"
  (context (g-object gtk-style-context))
  (window (g-object gdk-window)))

(export 'gtk-style-context-set-background)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_restore ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_restore" gtk-style-context-restore) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Restores the style context state to a previous stage.
  @end{short}
  See the function @fun{gtk-style-context-save}.
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-save}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_save" gtk-style-context-save) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Saves the style context state.
  @end{short}
  So all modifications done through the functions
  @fun{gtk-style-context-add-class}, @fun{gtk-style-context-remove-class} or
  @fun{gtk-style-context-junction-sides} can be reverted in one go through
  the function @fun{gtk-style-context-restore}.
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-add-class}
  @see-function{gtk-style-context-remove-class}
  @see-function{gtk-style-context-junction-sides}
  @see-function{gtk-style-context-restore}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-save)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_class" gtk-style-context-add-class) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[class-name]{a string with a class name to use in styling}
  @begin{short}
    Adds a style class to the context, so posterior calls to the function
    @fun{gtk-style-context-property} or any of the @sym{gtk-render-*} functions
    will make use of this new class for styling.
  @end{short}
  @begin[Example]{dictionary}
    In the CSS file format, a GtkEntry defining an \"entry\" class, would be
    matched by:
    @begin{pre}
 GtkEntry.entry { ... @}
  @end{pre}
  While any widget defining an \"entry\" class would be matched by:
  @begin{pre}
 .entry { ... @}
    @end{pre}
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-property}"
  (context (g-object gtk-style-context))
  (class-name g-string))

(export 'gtk-style-context-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_class" gtk-style-context-remove-class) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[class-name]{a string with a class name to remove}
  @begin{short}
    Removes a class name from the style context.
  @end{short}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (class-name g-string))

(export 'gtk-style-context-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_has_class" gtk-style-context-has-class) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[class-name]{a string with a class name}
  @return{@arg{True} if the style context has @arg{class-name} defined.}
  @begin{short}
    Returns @arg{true} if the style context currently has defined the given
    class name.
  @end{short}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (class-name g-string))

(export 'gtk-style-context-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_classes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_list_classes" gtk-style-context-list-classes)
    (g-list g-string)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @return{A list of strings with the currently defined classes.}
  @begin{short}
    Returns the list of classes currently defined in the style context.
  @end{short}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-list-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_region" gtk-style-context-add-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[name]{a string with a region name to use in styling}
  @argument[flags]{flags of type @symbol{gtk-region-flags} that apply to the
    region}
  @begin{short}
    Adds a region to the context, so posterior calls to the function
    @fun{gtk-style-context-property} or any of the @sym{gtk-render-*}
    functions will make use of this new region for styling.
  @end{short}
  @begin[Example]{dictionary}
    In the CSS file format, a GtkTreeView defining a \"row\" region, would be
    matched by:
    @begin{pre}
 GtkTreeView row { ... @}
    @end{pre}
    Pseudo-classes are used for matching flags, so the two following rules would
    apply to even and odd rows, respectively.
    @begin{pre}
 GtkTreeView row:nth-child(even) { ... @}
 GtkTreeView row:nth-child(odd) { ... @}
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    Region names must only contain lowercase letters and '-', starting always
    with a lowercase letter.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-add-region} has been deprecated since
    version 3.14 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (name :string)
  (flags gtk-region-flags))

(export 'gtk-style-context-add-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_region" gtk-style-context-remove-region)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[region-name]{a string with a region name to unset}
  @begin{short}
    Removes a region from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-remove-region} has been deprecated since
    version 3.14 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (region-name g-string))

(export 'gtk-style-context-remove-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_has_region" %gtk-style-context-has-region) :boolean
  (context (g-object gtk-style-context))
  (region-name g-string)
  (flags (:pointer gtk-region-flags)))

(defun gtk-style-context-has-region (context region-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[region-name]{a string with a region name}
  @return{Returns the region flags of type @symbol{gtk-region-flags}.}
  @begin{short}
    Returns the region flags if the style context has the region defined.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-has-region} has been deprecated since
    version 3.14 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (with-foreign-object (flags 'gtk-region-flags)
    (when (%gtk-style-context-has-region context region-name flags)
      (mem-ref flags 'gtk-region-flags))))

(export 'gtk-style-context-has-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_regions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_list_regions" gtk-style-context-list-regions)
    (g-list g-string)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @return{Returns a list of strings with the currently defined regions.}
  @begin{short}
    Returns the list of regions currently defined in the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-list-regions} has been deprecated since
    version 3.14 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-list-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_scale ()
;;; gtk_style_context_set_scale () -> gtk-style-context-scale
;;; ----------------------------------------------------------------------------

(defun (setf gtk-style-context-scale) (scale context)
  (foreign-funcall "gtk_style_context_set_scale"
                   (g-object gtk-style-context) context
                   :int scale
                   :void)
  scale)

(defcfun ("gtk_style_context_get_scale" gtk-style-context-scale) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @syntax[]{(gtk-style-context-scale context) => scale}
  @syntax[]{(setf (gtk-style-context-scale context) scale)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[scale]{an integer with a scale}
  @begin{short}
    Accessor of the scale used for image assets for the style context.
  @end{short}

  The function @sym{gtk-style-context-scale} returns the scale used for image
  assets for the style context. The function
  @sym{(setf gtk-style-context-scale)} sets the scale to use when getting image
  assets for the style context.
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_to_string ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_style_context_to_string" gtk-style-context-to-string) g-string
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[flags]{flags of type @symbol{gtk-style-context-print-flags} that
    determine what to print}
  @return{A string representing the style context.}
  @begin{short}
    Converts the style context into a string representation.
  @end{short}

  The string representation always includes information about the name, state,
  ID, visibility and style classes of the CSS node that is backing the style
  context. Depending on the flags, more information may be included.

  This function is intended for testing and debugging of the CSS implementation
  in GTK+. There are no guarantees about the format of the returned string, it
  may change.

  Since 3.20
  @begin[Example]{dictionary}
    @begin{pre}
  (setq context
        (gtk-widget-style-context (make-instance 'gtk-message-dialog)))
=> #<GTK-STYLE-CONTEXT {1001C70663@}>
  (gtk-style-context-to-string context :recurse)
=>
\"[messagedialog.background.csd:dir(ltr)@]
  decoration:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    box.horizontal:dir(ltr)
      image:dir(ltr)
      box.vertical:dir(ltr)
        label:dir(ltr)
        [label:dir(ltr)@]
    box.horizontal.dialog-action-box:dir(ltr)
      buttonbox.linked.horizontal.dialog-action-area:dir(ltr)
  box.titlebar.horizontal:dir(ltr)
    [label.title:dir(ltr)@]
\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-style-context-print-flags}"
  (context (g-object gtk-style-context))
  (flags gtk-style-context-print-flags))

#+gtk-3-20
(export 'gtk-style-context-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_border_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-border-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @return{A newly allocated @class{gtk-border} structure.}
  @begin{short}
    Allocates a new @class{gtk-border} structure and initializes its elements
    to zero.
  @end{short}
  @see-class{gtk-border}"
  (make-gtk-border :left 0 :right 0 :top 0 :bottom 0))

(export 'gtk-border-new)

;;; ----------------------------------------------------------------------------
;;; gtk_border_copy ()
;;; ----------------------------------------------------------------------------

(defun gtk-border-copy (border)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[border]{a @class{gtk-border} structure}
  @return{A copy of @arg{border}.}
  @short{Copies a @class{gtk-border} structure.}
  @see-class{gtk-border}"
  (copy-gtk-border border))

(export 'gtk-border-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_border_free ()
;;;
;;; void gtk_border_free (GtkBorder *border_);
;;;
;;; Frees a GtkBorder structure.
;;;
;;; border_ :
;;;     a GtkBorder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_arrow ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_arrow" gtk-render-arrow) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[angle]{a @code{:double} with an arrow angle from 0 to 2 * pi,
    being 0 the arrow pointing to the north}
  @argument[x]{a @code{:double} with the x origin of the render area}
  @argument[y]{a @code{:double} with the y origin of the render area}
  @argument[size]{a @code{:double} with the square side for render area}
  @begin{short}
    Renders an arrow pointing to angle.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (angle :double)
  (x :double)
  (y :double)
  (size :double))

(export 'gtk-render-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_background" gtk-render-background) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with the x origin of the render area}
  @argument[y]{a @code{:double} with the y origin of the render area}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rechtanlge height}
  @begin{short}
    Renders the background of an element.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-background)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background_get_clip ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_render_background_get_clip" %gtk-render-background-get-clip)
    :void
  (context (g-object gtk-style-context))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (out-clip (g-boxed-foreign gdk-rectangle)))

#+gtk-3-20
(defun gtk-render-background-get-clip (context x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[x]{a @code{:double} with the x origin of the render area}
  @argument[y]{a @code{:double} with the y origin of the render area}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @return{A @class{gdk-rectangle}.}
  @begin{short}
    Returns the area that will be affected (i.e. drawn to) when calling the
    function @fun{gtk-render-background} for the given context and rectangle.
  @end{short}

  Since 3.20
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (let ((out-clip (make-gdk-rectangle)))
    (%gtk-render-background-get-clip context x y width height out-clip)
    out-clip))

#+gtk-3-20
(export 'gtk-render-background-get-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_render_check ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_check" gtk-render-check) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with the x origin of the rectangle}
  @argument[y]{a @code{:double} with the y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders a checkmark as in a @class{gtk-check-button}.
  @end{short}

  The @code{:active} state of type @symbol{gtk-state-flags} determines whether
  the check is on or off, and @code{:inconsistent} determines whether it should
  be marked as undefined.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-check)

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_expander" gtk-render-expander) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with the x origin of the rectangle}
  @argument[y]{a @code{:double} with the y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders an expander as used in @class{gtk-tree-view} and
    @class{gtk-expander} in the area defined by x, y, width, height.
  @end{short}
  The state @code{:active} of type @symbol{gtk-state-flags} determines whether
  the expander is collapsed or expanded.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-expander)

;;; ----------------------------------------------------------------------------
;;; gtk_render_extension ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_extension" gtk-render-extension) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with the x origin of the rectangle}
  @argument[y]{a @code{:double} with the y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @argument[gap-side]{side of type @symbol{gtk-position-type} where the gap is}
  @begin{short}
    Renders a extension as in a @class{gtk-notebook} tab in the rectangle
    defined by x, y, width, height.
  @end{short}
  The side where the extension connects to is defined by @arg{gap-side}.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-smbol{gtk-position-type}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (gap-side gtk-position-type))

(export 'gtk-render-extension)

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_focus" gtk-render-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders a focus indicator on the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_frame" gtk-render-frame) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders a frame around the rectangle defined by x, y, width, height.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame_gap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_frame_gap" gtk-render-frame-gap) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @argument[gap-side]{a side of type @symbol{gtk-position-type} where the gap
    is}
  @argument[xy0-gap]{initial coordinate of type @code{:double} for the gap}
  @argument[xy1-gap]{end coordinate of type @code{:double} for the gap}
  @begin{short}
    Renders a frame around the rectangle defined by (x, y, width, height)
    leaving a gap on one side.
  @end{short}
  xy0_gap and xy1_gap will mean X coordinates for @code{GTK_POS_TOP} and
  @code{GTK_POS_BOTTOM} gap sides, and Y coordinates for @code{GTK_POS_LEFT}
  and @code{GTK_POS_RIGHT}.
  @begin[Warnin]{dictionary}
    The function @sym{gtk-render-frame-gap} has been deprecated since version
    3.24 and should not be used in newly-written code. Use the function
    @fun{gtk-render-frame} instead. Themes can create gaps by omitting borders
    via CSS.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (gap-side gtk-position-type)
  (xy0-gap :double)
  (xy1-gap :double))

(export 'gtk-render-frame-gap)

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_handle" gtk-render-handle) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders a handle as in @class{gtk-handle-box}, @class{gtk-paned} and
    @class{gtk-window}'s resize grip, in the rectangle determined by x, y,
    width, height.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-handle)

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_layout" gtk-render-layout) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[layout]{a @symbol{pango-layout} to render}
  @begin{short}
    Renders a pango layout on the coordinates x, y
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{pango-layout}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (layout (g-object pango-layout)))

(export 'gtk-render-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_render_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_line" gtk-render-line) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x0]{a @code{:double} with the x coordinate for the origin of the
    line}
  @argument[y0]{a @code{:double} with the y coordinate for the origin of the
    line}
  @argument[x1]{a @code{:double} with the x coordinate for the end of the line}
  @argument[y1]{a @code{:double} with the y coordinate for the end of the line}
  @begin{short}
    Renders a line from (x0, y0) to (x1, y1).
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(export 'gtk-render-line)

;;; ----------------------------------------------------------------------------
;;; gtk_render_option ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_option" gtk-render-option) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders an option mark as in a @class{gtk-radio-button}
  @end{short}
  The @code{GTK_STATE_FLAG_ACTIVE} state will determine whether the option is
  on or off, and @code{GTK_STATE_FLAG_INCONSISTENT} whether it should be marked
  as undefined.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-option)

;;; ----------------------------------------------------------------------------
;;; gtk_render_slider ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_slider" gtk-render-slider) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @argument[orientation]{the @symbol{gtk-orientation} of the slider}
  @begin{short}
    Renders a slider as in @class{gtk-scale} in the rectangle defined by x, y,
    width, height.
  @end{short}
  The orientation defines whether the slider is vertical or horizontal.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (orientation gtk-orientation))

(export 'gtk-render-slider)

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_activity" gtk-render-activity) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with a x origin of the rectangle}
  @argument[y]{a @code{:double} with a y origin of the rectangle}
  @argument[width]{a @code{:double} with a rectangle width}
  @argument[height]{a @code{:double} with a rectangle height}
  @begin{short}
    Renders an activity area such as in @class{gtk-spinner} or the fill line in
    @class{gtk-range}.
  @end{short}
  The state @code{GTK_STATE_FLAG_ACTIVE} determines whether there is activity
  going on.
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(export 'gtk-render-activity)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon_pixbuf" gtk-render-icon-pixbuf) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-13}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[source]{the @class{gtk-icon-source} object specifying the icon to
    render}
  @argument[size]{the size of type @symbol{gtk-icon-size} to render the icon at}
  @return{A newly-created @class{gdk-pixbuf} containing the rendered icon.}
  @begin{short}
    Renders the icon specified by @arg{source} at the given @arg{size},
    returning the result in a pixbuf.
  @end{short}

  A size of @code{(GtkIconSize) - 1} means render at the size of the source and
  do not scale.
  @begin[Warning]{dictionary}
    The function @sym{gtk-render-icon-pixbuf} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-icon-theme-load-icon} instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-icon-source}
  @see-class{gdk-pixbuf}"
  (context (g-object gtk-style-context))
  (source (g-boxed-foreign gtk-icon-source))
  (size gtk-icon-size))

(export 'gtk-render-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon_surface" gtk-render-icon-surface) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argumuent[surface]{a @symbol{cairo-surface-t} instance containing the icon
    to draw}
  @argument[x]{a @code{:double} with a x position for the icon}
  @argument[y]{a @code{:double} with a y position for the icon}
  @begin{short}
    Renders the icon in surface at the specified x and y coordinates.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (surface (:pointer (:struct cairo-surface-t)))
  (x :double)
  (y :double))

(export 'gtk-render-icon-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon" gtk-render-icon) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[pixbuf]{a @class{gdk-pixbuf} containing the icon to draw}
  @argument[x]{a @code{:double} with the x position for the pixbuf}
  @argument[y]{a @code{:double} with the y position for the pixbuf}
  @begin{short}
    Renders the icon in pixbuf at the specified x and y coordinates.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-class{gdk-pixbuf}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (pixbuf (g-object gdk-pixbuf))
  (x :double)
  (y :double))

(export 'gtk-render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_render_insertion_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_insertion_cursor" gtk-render-insertion-cursor) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-8}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a @code{:double} with the x origin}
  @argument[y]{a @code{:double} with the y origin}
  @argument[layout]{the @class{pango-layout} of the text}
  @argument[index]{the index of type @code{:int} in the pango layout}
  @argument[direction]{the @symbol{pango-direction} of the text}
  @begin{short}
    Draws a text caret on the Cairo context at the specified index of the
    pango layout.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-class{pango-layout}"
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (layout (g-object pango-layout))
  (index :int)
  (direction pango-direction))

(export 'gtk-render-insertion-cursor)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BACKGROUND_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_BACKGROUND_COLOR "background-color"
;;;
;;; A property holding the background color of rendered elements as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_COLOR "color"
;;;
;;; A property holding the foreground color of rendered elements as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_FONT
;;;
;;; #define GTK_STYLE_PROPERTY_FONT "font"
;;;
;;; A property holding the font properties used when rendering text as a
;;; PangoFontDescription.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_MARGIN
;;;
;;; #define GTK_STYLE_PROPERTY_MARGIN "margin"
;;;
;;; A property holding the rendered element's margin as a GtkBorder. The margin
;;; is defined as the spacing between the border of the element and its
;;; surrounding elements. It is external to GtkWidgets's size allocations, and
;;; the most external spacing property of the padding/border/margin series.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_PADDING
;;;
;;; #define GTK_STYLE_PROPERTY_PADDING "padding"
;;;
;;; A property holding the rendered element's padding as a GtkBorder. The
;;; padding is defined as the spacing between the inner part of the element
;;; border and its child. It's the innermost spacing property of the
;;; padding/border/margin series.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_WIDTH
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_WIDTH "border-width"
;;;
;;; A property holding the rendered element's border width in pixels as a
;;; GtkBorder. The border is the intermediary spacing property of the
;;; padding/border/margin series.
;;;
;;; gtk_render_frame() uses this property to find out the frame line width, so
;;; GtkWidgets rendering frames may need to add up this padding when requesting
;;; size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_RADIUS
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_RADIUS "border-radius"
;;;
;;; A property holding the rendered element's border radius in pixels as a gint.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_STYLE
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_STYLE "border-style"
;;;
;;; A property holding the element's border style as a GtkBorderStyle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_COLOR "border-color"
;;;
;;; A property holding the element's border color as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
;;;
;;; #define GTK_STYLE_PROPERTY_BACKGROUND_IMAGE "background-image"
;;;
;;; A property holding the element's background as a cairo_pattern_t.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ACCELERATOR
;;;
;;; #define GTK_STYLE_CLASS_ACCELERATOR "accelerator"
;;;
;;; A CSS class to match an accelerator.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ARROW
;;;
;;; #define GTK_STYLE_CLASS_ARROW "arrow"
;;;
;;; A CSS class used when rendering an arrow element.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BACKGROUND
;;;
;;; #define GTK_STYLE_CLASS_BACKGROUND "background"
;;;
;;; A CSS class to match the window background.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BOTTOM
;;;
;;; #define GTK_STYLE_CLASS_BOTTOM "bottom"
;;;
;;; A CSS class to indicate an area at the bottom of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook. Refer to individual widget documentation for used
;;; style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BUTTON
;;;
;;; #define GTK_STYLE_CLASS_BUTTON "button"
;;;
;;; A CSS class to match buttons.
;;;
;;; This is used by GtkButton and its subclasses, as well as various other
;;; widget pieces that appear like buttons, e.g. the arrows in a GtkCalendar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CALENDAR
;;;
;;; #define GTK_STYLE_CLASS_CALENDAR "calendar"
;;;
;;; A CSS class to match calendars.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CELL
;;;
;;; #define GTK_STYLE_CLASS_CELL "cell"
;;;
;;; A CSS class to match content rendered in cell views.
;;;
;;; This is used by cell renderers, e.g. in GtkIconView and GtkTreeView.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_COMBOBOX_ENTRY
;;;
;;; #define GTK_STYLE_CLASS_COMBOBOX_ENTRY "combobox-entry"
;;;
;;; A CSS class to match combobox entries.
;;;
;;; This is used by GtkComboBox. Refer to individual widget documentation for
;;  used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CONTEXT_MENU
;;;
;;; #define GTK_STYLE_CLASS_CONTEXT_MENU "context-menu"
;;;
;;; A CSS class to match context menus.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CHECK
;;;
;;; #define GTK_STYLE_CLASS_CHECK "check"
;;;
;;; A CSS class to match check boxes.
;;;
;;; This is used in GtkCheckButton, GtkCheckMenuItem and GtkCellRendererToggle.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CSD
;;;
;;; #define GTK_STYLE_CLASS_CSD "csd"
;;;
;;; A CSS class that gets added to windows which have client-side decorations.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CURSOR_HANDLE
;;;
;;; #define GTK_STYLE_CLASS_CURSOR_HANDLE "cursor-handle"
;;;
;;; A CSS class used when rendering a drag handle for text selection.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DEFAULT
;;;
;;; #define GTK_STYLE_CLASS_DEFAULT "default"
;;;
;;; A CSS class to match the default widget.
;;;
;;; This is used by GtkButton. Refer to individual widget documentation for used
;;; style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DESTRUCTIVE_ACTION
;;;
;;; #define GTK_STYLE_CLASS_DESTRUCTIVE_ACTION "destructive-action"
;;;
;;; A CSS class used when an action (usually a button) is one that is expected
;;; to remove or destroy something visible to the user.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DIM_LABEL
;;;
;;; #define GTK_STYLE_CLASS_DIM_LABEL "dim-label"
;;;
;;; A CSS class to match dimmed labels.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DND
;;;
;;; #define GTK_STYLE_CLASS_DND "dnd"
;;;
;;; A CSS class for a drag-and-drop indicator.
;;;
;;; This is used when drawing an outline around a potential drop target during
;;; DND. Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DOCK
;;;
;;; #define GTK_STYLE_CLASS_DOCK "dock"
;;;
;;; A CSS class defining a dock area.
;;;
;;; This is used by GtkHandleBox. Refer to individual widget documentation for
;;; used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ENTRY
;;;
;;; #define GTK_STYLE_CLASS_ENTRY "entry"
;;;
;;; A CSS class to match text entries.
;;;
;;; This is used by GtkEntry. Refer to individual widget documentation for used
;;; style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ERROR
;;;
;;; #define GTK_STYLE_CLASS_ERROR "error"
;;;
;;; A CSS class for an area displaying an error message, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar. Refer to individual widget documentation for
;;; used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_EXPANDER
;;;
;;; #define GTK_STYLE_CLASS_EXPANDER "expander"
;;;
;;; A CSS class defining an expander, such as those in treeviews.
;;;
;;; Used for drawing expanders in GtkTreeView, GtkExpander and GtkToolItemGroup.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_FRAME
;;;
;;; #define GTK_STYLE_CLASS_FRAME "frame"
;;;
;;; A CSS class defining a frame delimiting content, such as GtkFrame or the
;;; scrolled window frame around the scrollable area.
;;;
;;; This is used in GtkFrame and GtkScrollbar. Refer to individual widget
;;; documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_FLAT
;;;
;;; #define GTK_STYLE_CLASS_FLAT "flat"
;;;
;;; A CSS class that is added when widgets that usually have a frame or border
;;; (like buttons or entries) should appear without it.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_GRIP
;;;
;;; #define GTK_STYLE_CLASS_GRIP "grip"
;;;
;;; A CSS class defining a resize grip.
;;;
;;; This is used for the resize grip in GtkWindow. Refer to individual widget
;;; documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HEADER
;;;
;;; #define GTK_STYLE_CLASS_HEADER "header"
;;;
;;; A CSS class to match a header element.
;;;
;;; This is used for the header in GtkCalendar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HIGHLIGHT
;;;
;;; #define GTK_STYLE_CLASS_HIGHLIGHT "highlight"
;;;
;;; A CSS class defining a highlighted area, such as headings in assistants and
;;; calendars.
;;;
;;; This is used in GtkAssistant and GtkCalendar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HORIZONTAL
;;;
;;; #define GTK_STYLE_CLASS_HORIZONTAL "horizontal"
;;;
;;; A CSS class for horizontally layered widgets.
;;;
;;; This is used by widgets implementing GtkOrientable.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_IMAGE
;;;
;;; #define GTK_STYLE_CLASS_IMAGE "image"
;;;
;;; A CSS class defining an image, such as the icon in an entry.
;;;
;;; This is used when rendering icons in GtkEntry.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_INFO
;;;
;;; #define GTK_STYLE_CLASS_INFO "info"
;;;
;;; A CSS class for an area displaying an informational message, such as those
;;; in infobars.
;;;
;;; This is used by GtkInfoBar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_INLINE_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_INLINE_TOOLBAR "inline-toolbar"
;;;
;;; A CSS class to match inline toolbars.
;;;
;;; This should be used for toolbars that are used to hold actions below lists,
;;; as seen e.g. in the left pane of the file chooser.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_INSERTION_CURSOR
;;;
;;; #define GTK_STYLE_CLASS_INSERTION_CURSOR "insertion-cursor"
;;;
;;; A CSS class used when rendering a drag handle for the insertion cursor
;;; position.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LABEL
;;;
;;; #define GTK_STYLE_CLASS_LABEL "label"
;;;
;;; A CSS class to match labels.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LEFT
;;;
;;; #define GTK_STYLE_CLASS_LEFT "left"
;;;
;;; A CSS class to indicate an area at the left of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LEVEL_BAR
;;;
;;; #define GTK_STYLE_CLASS_LEVEL_BAR "level-bar"
;;;
;;; A CSS class used when rendering a level indicator, such as a battery charge
;;; level, or a password strength.
;;;
;;; This is used by GtkLevelBar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LINKED
;;;
;;; #define GTK_STYLE_CLASS_LINKED "linked"
;;;
;;; A CSS class to match a linked area, such as a box containing buttons
;;; belonging to the same control.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LIST
;;;
;;; #define GTK_STYLE_CLASS_LIST "list"
;;;
;;; A CSS class to match lists.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LIST_ROW
;;;
;;; #define GTK_STYLE_CLASS_LIST_ROW "list-row"
;;;
;;; A CSS class to match list rows.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MARK
;;;
;;; #define GTK_STYLE_CLASS_MARK "mark"
;;;
;;; A CSS class defining marks in a widget, such as in scales.
;;;
;;; Used in GtkScale.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENU
;;;
;;; #define GTK_STYLE_CLASS_MENU "menu"
;;;
;;; A CSS class to match popup menus.
;;;
;;; This is used in GtkMenu.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENUBAR
;;;
;;; #define GTK_STYLE_CLASS_MENUBAR "menubar"
;;;
;;; A CSS class to menubars.
;;;
;;; This is used in GtkMenuBar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENUITEM
;;;
;;; #define GTK_STYLE_CLASS_MENUITEM "menuitem"
;;;
;;; A CSS class to match menu items.
;;;
;;; This is used in GtkMenuItem and its subclasses.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MESSAGE_DIALOG
;;;
;;; #define GTK_STYLE_CLASS_MESSAGE_DIALOG "message-dialog"
;;;
;;; A CSS class that is added to message dialogs.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MONOSPACE
;;;
;;; #define GTK_STYLE_CLASS_MONOSPACE "monospace"
;;;
;;; A CSS class that is added to text view that should use a monospace font.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_NEEDS_ATTENTION
;;;
;;; #define GTK_STYLE_CLASS_NEEDS_ATTENTION "needs-attention"
;;;
;;; A CSS class used when an element needs the user attention, for instance a
;;; button in a stack switcher corresponding to a hidden page that changed
;;; state.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_NOTEBOOK
;;;
;;; #define GTK_STYLE_CLASS_NOTEBOOK "notebook"
;;;
;;; A CSS class defining a notebook.
;;;
;;; Used in GtkNotebook.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_OSD
;;;
;;; #define GTK_STYLE_CLASS_OSD "osd"
;;;
;;; A CSS class used when rendering an OSD (On Screen Display) element, on top
;;; of another container.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_OVERSHOOT
;;;
;;; #define GTK_STYLE_CLASS_OVERSHOOT "overshoot"
;;;
;;; A CSS class that is added on the visual hints that happen when scrolling is
;;; attempted past the limits of a scrollable area.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PANE_SEPARATOR
;;;
;;; #define GTK_STYLE_CLASS_PANE_SEPARATOR "pane-separator"
;;;
;;; A CSS class for a pane separator, such as those in GtkPaned.
;;;
;;; Used in GtkPaned.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PAPER
;;;
;;; #define GTK_STYLE_CLASS_PAPER "paper"
;;;
;;; A CSS class that is added to areas that should look like paper.
;;;
;;; This is used in print previews and themes are encouraged to style it as
;;; black text on white background.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_POPUP
;;;
;;; #define GTK_STYLE_CLASS_POPUP "popup"
;;;
;;; A CSS class that is added to the toplevel windows used for menus.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_POPOVER
;;;
;;; #define GTK_STYLE_CLASS_POPOVER "popover"
;;;
;;; A CSS class that matches popovers.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PRIMARY_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_PRIMARY_TOOLBAR "primary-toolbar"
;;;
;;; A CSS class to match primary toolbars.
;;;
;;; This should be used for the 'main' toolbar of an application, right below
;;; its menubar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PROGRESSBAR
;;;
;;; #define GTK_STYLE_CLASS_PROGRESSBAR "progressbar"
;;;
;;; A CSS class to use when rendering activity as a progressbar.
;;;
;;; This is used in GtkProgressBar and when drawing progress inside a GtkEntry
;;; or in GtkCellRendererProgress.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PULSE
;;;
;;; #define GTK_STYLE_CLASS_PULSE "pulse"
;;;
;;; A CSS class to use when rendering a pulse in an indeterminate progress bar.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_QUESTION
;;;
;;; #define GTK_STYLE_CLASS_QUESTION "question"
;;;
;;; A CSS class for an area displaying a question to the user, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RADIO
;;;
;;; #define GTK_STYLE_CLASS_RADIO "radio"
;;;
;;; A CSS class to match radio buttons.
;;;
;;; This is used in GtkRadioButton, GtkRadioMenuItem and GtkCellRendererToggle.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RAISED
;;;
;;; #define GTK_STYLE_CLASS_RAISED "raised"
;;;
;;; A CSS class to match a raised control, such as a raised button on a toolbar.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_READ_ONLY
;;;
;;; #define GTK_STYLE_CLASS_READ_ONLY "read-only"
;;;
;;; A CSS class used to indicate a read-only state.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RIGHT
;;;
;;; #define GTK_STYLE_CLASS_RIGHT "right"
;;;
;;; A CSS class to indicate an area at the right of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RUBBERBAND
;;;
;;; #define GTK_STYLE_CLASS_RUBBERBAND "rubberband"
;;;
;;; A CSS class to match the rubberband selection rectangle.
;;;
;;; This is used in GtkIconView and GtkTreeView.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE
;;;
;;; #define GTK_STYLE_CLASS_SCALE "scale"
;;;
;;; A CSS class to match scale widgets.
;;;
;;; This is used in GtkScale.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
;;;
;;; #define GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE "scale-has-marks-above"
;;;
;;; A CSS class to match scale widgets with marks attached, all the marks are
;;; above for horizontal GtkScale. left for vertical GtkScale.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
;;;
;;; #define GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW "scale-has-marks-below"
;;;
;;; A CSS class to match scale widgets with marks attached, all the marks are
;;; below for horizontal GtkScale, right for vertical GtkScale.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCROLLBAR
;;;
;;; #define GTK_STYLE_CLASS_SCROLLBAR "scrollbar"
;;;
;;; A CSS class to match scrollbars.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
;;;
;;; #define GTK_STYLE_CLASS_SCROLLBARS_JUNCTION "scrollbars-junction"
;;;
;;; A CSS class to match the junction area between an horizontal and vertical
;;; scrollbar, when they're both shown.
;;;
;;; This is used in GtkScrolledWindow.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SEPARATOR
;;;
;;; #define GTK_STYLE_CLASS_SEPARATOR "separator"
;;;
;;; A CSS class for a separator.
;;;
;;; This is used in GtkSeparator, GtkSeparatorMenuItem, GtkSeparatorToolItem,
;;; and when drawing separators in GtkTreeView.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SIDEBAR
;;;
;;; #define GTK_STYLE_CLASS_SIDEBAR "sidebar"
;;;
;;; A CSS class defining a sidebar, such as the left side in a file chooser.
;;;
;;; This is used in GtkFileChooser and in GtkAssistant.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SLIDER
;;;
;;; #define GTK_STYLE_CLASS_SLIDER "slider"
;;;
;;; A CSS class to match sliders.
;;;
;;; This is used by GtkSwitch and GtkRange and its subclasses.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SPINBUTTON
;;;
;;; #define GTK_STYLE_CLASS_SPINBUTTON "spinbutton"
;;;
;;; A CSS class defining an spinbutton.
;;;
;;; This is used in GtkSpinButton.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SPINNER
;;;
;;; #define GTK_STYLE_CLASS_SPINNER "spinner"
;;;
;;; A CSS class to use when rendering activity as a 'spinner'.
;;;
;;; This is used by GtkSpinner and GtkCellRendererSpinner.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_STATUSBAR
;;;
;;; #define GTK_STYLE_CLASS_STATUSBAR "statusbar"
;;;
;;; A CSS class to match statusbars.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SUBTITLE
;;;
;;; #define GTK_STYLE_CLASS_SUBTITLE "subtitle"
;;;
;;; A CSS class used for the subtitle label in a titlebar in a toplevel window.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SUGGESTED_ACTION
;;;
;;; #define GTK_STYLE_CLASS_SUGGESTED_ACTION "suggested-action"
;;;
;;; A CSS class used when an action (usually a button) is the primary suggested
;;; action in a specific context.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TITLE
;;;
;;; #define GTK_STYLE_CLASS_TITLE "title"
;;;
;;; A CSS class used for the title label in a titlebar in a toplevel window.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TITLEBAR
;;;
;;; #define GTK_STYLE_CLASS_TITLEBAR "titlebar"
;;;
;;; A CSS class used when rendering a titlebar in a toplevel window.
;;;
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_TOOLBAR "toolbar"
;;;
;;; A CSS class to match toolbars.
;;;
;;; This is used in GtkToolbar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOOLTIP
;;;
;;; #define GTK_STYLE_CLASS_TOOLTIP "tooltip"
;;;
;;; A CSS class to match tooltip windows.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOUCH_SELECTION
;;;
;;; #define GTK_STYLE_CLASS_TOUCH_SELECTION "touch-selection"
;;;
;;; A CSS class for touch selection popups on entries and text views.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOP
;;;
;;; #define GTK_STYLE_CLASS_TOP "top"
;;;
;;; A CSS class to indicate an area at the top of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TROUGH
;;;
;;; #define GTK_STYLE_CLASS_TROUGH "trough"
;;;
;;; A CSS class to match troughs, as in scrollbars and progressbars.
;;;
;;; This is used in GtkRange and its subclasses, GtkProgressBar and GtkSwitch.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_UNDERSHOOT
;;;
;;; #define GTK_STYLE_CLASS_UNDERSHOOT "undershoot"
;;;
;;; A CSS class that is added on the visual hints that happen where content is
;;; 'scrolled off' and can be made visible by scrolling.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_VERTICAL
;;;
;;; #define GTK_STYLE_CLASS_VERTICAL "vertical"
;;;
;;; A CSS class for vertically layered widgets.
;;;
;;; This is used by widgets implementing GtkOrientable.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_VIEW
;;;
;;; #define GTK_STYLE_CLASS_VIEW "view"
;;;
;;; A CSS class defining a view, such as iconviews or treeviews.
;;;
;;; This is used in GtkTreeView, GtkIconView, GtkTextView, as well as
;;; GtkCalendar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_WARNING
;;;
;;; #define GTK_STYLE_CLASS_WARNING "warning"
;;;
;;; A CSS class for an area displaying a warning message, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar.
;;; Refer to individual widget documentation for used style classes.
;;; ----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_WIDE
;;;
;;; #define GTK_STYLE_CLASS_WIDE "wide"
;;;
;;; A CSS class to indicate that a UI element should be 'wide'. Used by
;;; GtkPaned.
;;;
;;; Refer to individual widget documentation for used style classes.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_COLUMN
;;;
;;; #define GTK_STYLE_REGION_COLUMN "column"
;;;
;;; A widget region name to define a treeview column.
;;;
;;; Warning
;;;
;;; GTK_STYLE_REGION_COLUMN has been deprecated since version 3.20 and should
;;; not be used in newly-written code.
;;;
;;; Don't use regions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_COLUMN_HEADER
;;;
;;; #define GTK_STYLE_REGION_COLUMN_HEADER "column-header"
;;;
;;; A widget region name to define a treeview column header.
;;;
;;; Warning
;;;
;;; GTK_STYLE_REGION_COLUMN_HEADER has been deprecated since version 3.20 and
;;; should not be used in newly-written code.
;;;
;;; Don't use regions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_ROW
;;;
;;; #define GTK_STYLE_REGION_ROW "row"
;;;
;;; A widget region name to define a treeview row.
;;;
;;; Warning
;;;
;;; GTK_STYLE_REGION_ROW has been deprecated since version 3.20 and should not
;;; be used in newly-written code.
;;;
;;; Don't use regions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_TAB
;;;
;;; #define GTK_STYLE_REGION_TAB "tab"
;;;
;;; A widget region name to define a notebook tab.
;;;
;;; Warning
;;;
;;; GTK_STYLE_REGION_TAB has been deprecated since version 3.20 and should not
;;; be used in newly-written code.
;;;
;;; Don't use regions.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.style-context.lisp -------------------------------------

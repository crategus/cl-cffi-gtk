;;; ----------------------------------------------------------------------------
;;; gtk.style-context.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
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
;;; Functions
;;;
;;;     gtk_style_context_new
;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_get                              not needed
;;;     gtk_style_context_get_direction                    Accessor
;;;     gtk_style_context_get_junction_sides
;;;     gtk_style_context_get_parent                       Accessor
;;;     gtk_style_context_get_path
;;;     gtk_style_context_get_property
;;;     gtk_style_context_get_screen                       Accessor
;;;     gtk_style_context_get_frame_clock
;;;     gtk_style_context_get_state
;;;     gtk_style_context_get_style                        not needed
;;;     gtk_style_context_get_style_property
;;;     gtk_style_context_get_style_valist                 not needed
;;;     gtk_style_context_get_valist                       not needed
;;;     gtk_style_context_get_section
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_background_color
;;;     gtk_style_context_get_border_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_get_font
;;;     gtk_style_context_invalidate
;;;     gtk_style_context_state_is_running                 not implemented
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_lookup_icon_set
;;;     gtk_style_context_notify_state_change              not exported
;;;     gtk_style_context_pop_animatable_region            not exported
;;;     gtk_style_context_push_animatable_region           not exported
;;;     gtk_style_context_cancel_animations                not implemented
;;;     gtk_style_context_scroll_animations                not implemented
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
(setf (gethash 'gtk-junction-sides atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-junction-sides atdoc:*external-symbols*)
 "@version{2021-11-26}
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
(setf (gethash 'gtk-style-context-print-flags atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-style-context-print-flags atdoc:*external-symbols*)
 "@version{2021-11-26}
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
    @entry[:recurse]{Print the entire tree of CSS nodes starting at the node
    of the style context.}
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
(setf (gethash 'gtk-border-style atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-border-style atdoc:*external-symbols*)
 "@version{2021-7-6}
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
    @entry[:hidden]{Same as the @code{:none} value.}
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
(setf (gethash 'gtk-border atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-border 'type)
 "@version{2021-11-26}
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
  @see-slot{gtk-border-left}
  @see-slot{gtk-border-right}
  @see-slot{gtk-border-top}
  @see-slot{gtk-border-bottom}")

(export 'gtk-border)

;;; ----------------------------------------------------------------------------
;;; gtk_border_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-border-new))

(defun gtk-border-new (&key (left 0) (right 0) (top 0) (bottom 0))
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[left]{an integer with the width of the left border}
  @argument[right]{an integer with the width of the right border}
  @argument[top]{an integer with the width of the top border}
  @argument[bottom]{an integer with the width of the bottom border}
  @return{A newly allocated @class{gtk-border} instance.}
  @begin{short}
    Allocates a new @class{gtk-border} instance and initializes its elements.
  @end{short}
  @see-class{gtk-border}"
  (make-gtk-border :left left :right right :top top :bottom bottom))

(export 'gtk-border-new)

;;; ----------------------------------------------------------------------------
;;; gtk_border_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-border-copy))

(defun gtk-border-copy (border)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[border]{a @class{gtk-border} instance}
  @return{A copy of @arg{border}.}
  @short{Copies a @class{gtk-border} instance.}
  @see-class{gtk-border}"
  (copy-gtk-border border))

(export 'gtk-border-copy)

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkBorder
;;; ----------------------------------------------------------------------------

;;; --- gtk-border-left --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-left atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-left 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-border-left instance) => left}
  @syntax[]{(setf gtk-border-left instance) left)}
  @argument[instance]{a @class{gtk-border} instance}
  @argument[left]{an integer with the width of the left border}
  @begin{short}
    Accessor of the @code{left} slot of the @class{gtk-border} structure.
  @end{short}
  @see-class{gtk-border}")

(export 'gtk-border-left)

;;; --- gtk-border-right -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-right atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-right 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-border-right instance) => right}
  @syntax[]{(setf gtk-border-right instance) right)}
  @argument[instance]{a @class{gtk-border} instance}
  @argument[right]{an integer with the width of the right border}
  @begin{short}
    Accessor of the @code{right} slot of the @class{gtk-border} structure.
  @end{short}
  @see-class{gtk-border}")

(export 'gtk-border-right)

;;; --- gtk-border-top ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-top atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-top 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-border-top instance) => top}
  @syntax[]{(setf gtk-border-top instance) top)}
  @argument[instance]{a @class{gtk-border} instance}
  @argument[top]{an integer with the width of the top border}
  @begin{short}
    Accessor of the @code{top} slot of the @class{gtk-border} structure.
  @end{short}
  @see-class{gtk-border}")

(export 'gtk-border-top)

;;; --- gtk-border-bottom ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-border-bottom atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-border-bottom 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-border-top instance) => bottom}
  @syntax[]{(setf gtk-border-top instance) bottom)}
  @argument[instance]{a @class{gtk-border} instance}
  @argument[bottom]{an integer with the width of the bottom border}
  @begin{short}
    Accessor of the @code{bottom} slot of the @class{gtk-border} structure.
  @end{short}
  @see-class{gtk-border}")

(export 'gtk-border-bottom)

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
 "@version{2021-11-26}
  @begin{short}
    The @sym{gtk-style-context} object stores styling information affecting a
    widget defined by a @class{gtk-widget-path} instance.
  @end{short}

  In order to construct the final style information, the @sym{gtk-style-context}
  object queries information from all attached @class{gtk-style-provider}
  objects. Style providers can be either attached explicitly to the style
  context through the @fun{gtk-style-context-add-provider} function, or to the
  screen through the @fun{gtk-style-context-add-provider-for-screen} function.
  The resulting style is a combination of all information of the style provider
  in priority order.

  For GTK widgets, any @sym{gtk-style-context} object returned by the
  @fun{gtk-widget-style-context} function will already have a
  @class{gtk-widget-path} instance, a @class{gdk-screen} object and a text
  direction information set. The style context will be also updated
  automatically if any of these settings change on the widget.

  If you are using the theming layer standalone, you will need to set a widget
  path and a screen yourself to the created style context through the
  @fun{gtk-style-context-path} and @fun{gtk-style-context-screen} functions.
  See the \"Custom Drawing\" example in the GTK Lisp Demo.

  @subheading{Style Classes}
  Widgets can add style classes to their style context, which can be used to
  associate different styles by class. The documentation for individual widgets
  lists which style classes it uses itself, and which style classes may be added
  by applications to affect their appearance.

  @subheading{Style Regions}
  Widgets can also add regions with flags to their style context. This feature
  is deprecated and will be removed in a future GTK update. Please use style
  classes instead.

  @subheading{Custom styling in UI libraries and applications}
  If you are developing a library with custom widgets that render differently
  than standard components, you may need to add a @class{gtk-style-provider}
  object yourself with the @var{+gtk-style-provider-priority-fallback+}
  priority, either a @class{gtk-css-provider} object or a custom object
  implementing the @class{gtk-style-provider} interface. This way themes may
  still attempt to style your UI elements in a different way if needed so.

  If you are using custom styling on an application, you probably want then to
  make your style information prevail to the style information of the theme, so
  you must use a @class{gtk-style-provider} object with the
  @var{+gtk-style-provider-priority-application+} priority. Keep in mind that
  the user settings in @file{XDG_CONFIG_HOME/gtk-3.0/gtk.css} will still take
  precedence over your changes, as it uses the
  @var{+gtk-style-provider-priority-user+} priority.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (context)    :run-first
      @end{pre}
      The signal is emitted when there is a change in the style context. For a
      style context returned by the @fun{gtk-widget-style-context} function, the
      \"style-updated\" signal of the @class{gtk-widget} class might be more
      convenient to use. The signal is useful when using the theming layer
      standalone.
      @begin[code]{table}
        @entry[context]{The @sym{gtk-style-context} object which received the
          signal.}
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
  The text direction of the style context. @br{}
  Default value: @code{:ltr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-direction 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-direction object) => direction}
  @syntax[]{(setf (gtk-style-context-direction object) direction)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[direction]{a value of the @symbol{gtk-text-direction} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{direction} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The @sym{gtk-style-context-direction} slot access function returns the widget
  direction used for rendering. The @sym{(setf gtk-style-context-direction)}
  slot access function sets the reading direction.

  If you are using a style context returned from the
  @fun{gtk-widget-style-context} function, you do not need to call this
  yourself.
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-direction} slot access function has been
    deprecated since version 3.8 and should not be used in newly written code.
    Use the @fun{gtk-style-context-state} function and check for the
    @code{:dir-ltr} and @code{:dir-rtl} values instead.
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
  The associated frame clock of the style context.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-paint-clock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-paint-clock 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-paint-clock object) => clock}
  @syntax[]{(setf (gtk-style-context-paint-clock object) clock)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{paint-clock} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The associated frame clock of the style context.
  @see-class{gtk-style-context}
  @see-class{gdk-frame-clock}")

;;; --- gtk-style-context-parent -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parent" 'gtk-style-context) 't)
 "The @code{parent} property of type @class{gtk-style-context}
  (Read / Write) @br{}
  The parent of the style context.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-parent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-parent 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-parent object) => parent}
  @syntax[]{(setf (gtk-style-context-parent object) parent)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[parent]{a @class{gtk-style-context} parent object or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{parent} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The @sym{gtk-style-context-parent} slot access function gets the parent style
  context. The @sym{(setf gtk-style-context-parent)} slot access function sets
  the parent style context.

  The parent style context is used to implement inheritance of properties. If
  you are using a style context returned from the @fun{gtk-widget-style-context}
  function, the parent will be set for you.
  @see-class{gtk-style-context}
  @see-function{gtk-widget-style-context}")

;;; --- gtk-style-context-screen -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-style-context) 't)
 "The @code{screen} property of type @class{gdk-screen} (Read / Write) @br{}
  The associated screen of the style context.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-context-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-style-context-screen 'function)
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-screen object) => screen}
  @syntax[]{(setf (gtk-style-context-screen object) screen)}
  @argument[object]{a @class{gtk-style-context} object}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Accessor of the @slot[gtk-style-context]{screen} slot of the
    @class{gtk-style-context} class.
  @end{short}

  The @sym{gtk-style-context-screen} slot access function returns the screen to
  which the style context is attached. The @sym{(setf gtk-style-context-screen)}
  slot access function attaches the style context to the given screen.

  The screen is used to add style information from 'global' style providers,
  such as the @class{gtk-settings} object of the screen.

  If you are using a style context returned from the
  @fun{gtk-widget-style-context} function, you do not need to call this
  yourself.
  @see-class{gtk-style-context}
  @see-class{gdk-screen}
  @see-class{gtk-settings}
  @see-function{gtk-widget-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-style-context-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @return{A newly created @class{gtk-style-context} object.}
  @begin{short}
    Creates a standalone style context object.
  @end{short}
  This style context will not be attached to any widget, so you may want to
  call the @fun{gtk-style-context-path} function yourself.
  @begin[Note]{dictionary}
    This function is only useful when using the theming layer separated from
    GTK, if you are using a style context to theme widgets, use the
    @fun{gtk-widget-style-context} function in order to get a style context
    ready to theme the widget.
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
 "@version{*2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a style provider to the style context, to be used in style
    construction.
  @end{short}
  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{+gtk-style-provider-priority-fallback+} and
  @var{+gtk-style-provider-priority-user+} priorities.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through this
    function takes precedence over another added through the
    @fun{gtk-style-context-add-provider-for-screen} function.
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
 "@version{*2021-11-30}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a global style provider to the screen, which will be used in style
    construction for all style contexts under the screen.
  @end{short}

  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{+gtk-style-provider-priority-fallback+} and
  @var{+gtk-style-provider-priority-user+} priorities.

  GTK uses this to make styling information from the @class{gtk-settings}
  object available.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through the
    @fun{gtk-style-context-add-provider} function takes precedence over another
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
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-junction-sides context) => sides}
  @syntax[]{(setf (gtk-style-context-junction-sides context) sides)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[sides]{a value of the @symbol{gtk-junction-sides} flags}
  @begin{short}
    Accessor of the junction sides of a style context.
  @end{short}

  The @sym{gtk-style-context-junction-sides} function returns the sides where
  rendered elements, mostly through the @fun{gtk-render-frame} function, will
  visually connect with other visual elements. The
  @sym{(setf gtk-style-context-function-sides)} function sets the sides where
  rendered elements will visually connect with other visual elements.

  This is merely a hint that may or may not be honored by theming engines.

  Container widgets are expected to set junction hints as appropriate for their
  children, so it should not normally be necessary to call this function
  manually.
  @see-class{gtk-style-context}
  @see-symbol{gtk-junction-sides}
  @see-function{gtk-render-frame}"
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
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-path context) => path}
  @syntax[]{(setf (gtk-style-context-path context) path)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[path]{a @class{gtk-widget-path} instance}
  @begin{short}
    Accessor of the widget path of the style context.
  @end{short}

  The @sym{gtk-style-context-path} function returns the widget path used for
  style matching. The @sym{(setf gtk-style-context-path)} function sets the
  widget path. As a consequence, the style will be regenerated to match the new
  given path.

  If you are using a style context returned from the
  @fun{gtk-widget-style-context} function, you do not need to call this
  yourself.
  @see-class{gtk-style-context}
  @see-class{gtk-widget-path}
  @see-function{gtk-widget-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-path)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_property () -> gtk-style-context-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_property" %gtk-style-context-property) :void
  (context (g-object gtk-style-context))
  (property :string)
  (state gtk-state-flags)
  (value (:pointer (:struct g-value))))

(defun gtk-style-context-property (context property state)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[property]{a string with a style property name}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the property value for}
  @return{The value for the style property.}
  @begin{short}
    Gets a style property from the style context for the given state.
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
      ;; TODO: Handle the case for an invalid property
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
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-frame-clock context) => clock}
  @syntax[]{(setf (gtk-style-context-frame-clock context) clock)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Accessor of the @class{gdk-frame-clock} object of the style context.
  @end{short}

  The @sym{gtk-style-context-frame-clock} function returns the frame clock to
  which the style context is attached. The
  @sym{(setf gtk-style-context-frame-clock)} function attaches the style context
  to the given frame clock.

  The frame clock is used for the timing of animations. If you are using a
  style context returned from the @fun{gtk-widget-style-context} function, you
  do not need to call this yourself.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{gtk-style-context-paint-clock}
    slot access function.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gdk-frame-clock}
  @see-function{gtk-widget-style-context}
  @see-function{gtk-style-context-paint-clock}"
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
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-state context) => state}
  @syntax[]{(setf (gtk-style-context-state context) state)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to represent}
  @begin{short}
    Accessor of the state used when rendering.
  @end{short}

  The @sym{gtk-style-context-state} function returns the state to be used when
  rendering with any of the @sym{gtk-render-*} functions. The
  @sym{(setf gtk-style-context-state)} function sets the state.
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

;; TODO: The C implementation does not have the argument widget. The GtkWidget
;; class is looked up in the function gtk_style_context_get_style_property().
;; The Lisp implementation uses the widget to get the GType of the returned
;; value. In contrast to the function gtk-style-context-property the C
;; function must be called with the correct GType of the value.
;; Consider to change the implementation.

(defcfun ("gtk_style_context_get_style_property"
          %gtk-style-context-style-property) :void
  (context (g-object gtk-style-context))
  (property :string)
  (value (:pointer (:struct g-value))))

(defun gtk-style-context-style-property (context widget property)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[widget]{a @class{gtk-widget} object the style property is looked up
    for}
  @argument[property]{a string with the name of the widget style property}
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
  @see-class{gtk-style-context}
  @see-class{gtk-widget}"
  (let ((gtype (g-param-spec-value-type
                   (gtk-widget-class-find-style-property
                       (g-type-from-instance widget)
                       property))))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value gtype)
      (prog2
        (%gtk-style-context-style-property context property value)
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[property]{a string with the name of the style property}
  @return{Returns @code{nil} or the @class{gtk-css-section} instance where the
    property was defined.}
  @begin{short}
    Queries the location in the CSS where @arg{property} was defined for the
    current style context.
  @end{short}
  Note that the state to be queried is taken from the
  @fun{gtk-style-context-state} function.

  If the location is not available, @code{nil} will be returned. The location
  might not be available for various reasons, such as the property being
  overridden, the @arg{property} argument not naming a supported CSS property or
  tracking of definitions being disabled for performance reasons.

  Shorthand CSS properties cannot be queried for a location and will always
  return @code{nil}.
  @see-class{gtk-style-context}
  @see-class{gtk-css-section}
  @see-function{gtk-style-context-state}"
  (context (g-object gtk-style-context))
  (property :string))

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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the color for}
  @return{The @class{gdk-rgba} foreground color.}
  @begin{short}
    Gets the foreground color for a given state.
  @end{short}
  See the @fun{gtk-style-context-property} function for details.
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the color for}
  @return{Returns the @class{gdk-rgba} background color.}
  @begin{short}
    Gets the background color for a given state.
  @end{short}

  This function is far less useful than it seems, and it should not be used in
  newly written code. CSS has no concept of \"background color\", as a
  background can be an image, or a gradient, or any other pattern including
  solid colors. The only reason why you would call the
  @sym{gtk-style-context-background-color} function is to use the returned value
  to draw the background with it. The correct way to achieve this result is to
  use the @fun{gtk-render-background} function instead, along with CSS style
  classes to modify the color to be rendered.
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-background-color} function has been deprecated
    since version 3.16 and should not be used in newly written code. Use the
    @fun{gtk-render-background} function instead.
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the color for}
  @return{Returns the @class{gdk-rgba} border color.}
  @begin{short}
    Gets the border color for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-border-color} function has been deprecated since
    version 3.16 and should not be used in newly written code. Use the
    @fun{gtk-render-frame} function instead.
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the border for}
  @return{Returns border settings as a @class{gtk-border} instance.}
  @begin{short}
    Gets the value for the border settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((border (gtk-border-new)))
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the padding for}
  @return{Returns padding settings as a @class{gtk-border} instance.}
  @begin{short}
    Gets the value for the padding settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((padding (gtk-border-new)))
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the margin for}
  @return{Returns margin settings as a @class{gtk-border} instance.}
  @begin{short}
    Gets the value for the margin settings for a given state.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-border}
  @see-symbol{gtk-state-flags}"
  (let ((margin (gtk-border-new)))
    (%gtk-style-context-margin context state margin)
    margin))

(export 'gtk-style-context-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_font () -> gtk-style-context-font
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_font" gtk-style-context-font)
    (g-boxed-foreign pango-font-description)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[state]{a value of the @symbol{gtk-state-flags} flags to retrieve
    the font for}
  @return{Returns a @class{pango-font-description} instance for the given
    state.}
  @begin{short}
    Returns the Pango font description for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-font} function has been deprecated since version
    3.8 and should not be used in newly written code. Use the
    @fun{gtk-style-context-property} function for \"font\" or subproperties
    instead.
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Invalidates style context information, so it will be reconstructed again.
  @end{short}
  If you are using a style context returned from the
  @fun{gtk-widget-style-context} function, you do not need to call this
  yourself.
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-invalidate} function has been deprecated since
    version 3.12 and should not be used in newly written code. Style contexts
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
;;; should not be used in newly written code.
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
  (name :string)
  (color (g-boxed-foreign gdk-rgba)))

(defun gtk-style-context-lookup-color (context name)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[name]{a string with a color name to lookup}
  @return{The looked up @class{gdk-rgba} color, or @code{nil}.}
  @begin{short}
    Looks up and resolves a color name in the style context color map.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gdk-rgba}"
  (let ((color (make-gdk-rgba)))
    (when (%gtk-style-context-lookup-color context name color)
      color)))

(export 'gtk-style-context-lookup-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_lookup_icon_set" gtk-style-context-lookup-icon-set)
    (g-boxed-foreign gtk-icon-set)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[stock]{a string with an icon name}
  @return{The looked up @class{gtk-icon-set} instance, or @code{nil}.}
  @begin{short}
    Looks up a stock icon in the icon factories associated to the style
    context and the default icon factory, returning an icon set if found,
    otherwise @code{nil}.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-lookup-icon-set} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk-icon-theme-lookup-icon} function instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-icon-set}
  @see-function{gtk-icon-theme-lookup-icon}"
  (context (g-object gtk-style-context))
  (stock :string))

(export 'gtk-style-context-lookup-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_notify_state_change ()               not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_notify_state_change"
           gtk-style-context-notify-state-change) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-6}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[window]{a @class{gdk-window} object}
  @argument[region-id]{a pointer to the animatable region to notify on, or
    @code{NULL}}
  @argument[state]{a value of the @symbol{gtk-state-type} flags to trigger
    transition for}
  @argument[value]{@em{true} if @arg{state} is the state we are changing to,
    @em{false} if we are changing away from it}
  @begin{short}
    Notifies a state change on the style context.
  @end{short}
  So if the current style makes use of transition animations, one will be
  started so all rendered elements under @arg{region-id} are animated for the
  state being set to @arg{value}.

  The @arg{window} parameter is used in order to invalidate the rendered area
  as the animation runs, so make sure it is the same window that is being
  rendered on by the @sym{gtk-render-*} functions.

  If @arg{region-id} is @code{NULL}, all rendered elements using the style
  context will be affected by this state transition.
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-notify-state-change} has been deprecated
    since version 3.6 and should not be used in newly written code. This
    function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gdk-window}
  @see-symbol{gtk-state-type}"
  (context (g-object gtk-style-context))
  (window (g-object gdk-window))
  (region-id :pointer)
  (state gtk-state-type)
  (value :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_pop_animatable_region ()             not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_pop_animatable_region"
           gtk-style-context-pop-animatable-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-6}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Pops an animatable region from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-pop-animatable-region} has been
    deprecated since version 3.6 and should not be used in newly written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context)))

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_push_animatable_region ()            not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_push_animatable_region"
           gtk-style-context-push-animatable-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-6}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[region-id]{a pointer which is an unique identifier for the
    animatable region}
  @begin{short}
    Pushes an animatable region, so all further @sym{gtk-render-*} calls between
    this call and the following a @fun{gtk-style-context-pop-animatable-region}
    call will potentially show transition animations for this region.
  @end{short}
  If the function @fun{gtk-style-context-notify-state-change} is called for a
  given state, and the current theme/style defines transition animations for
  state changes.

  The @arg{region-id} used must be unique in the style context so the theming
  engine can uniquely identify rendered elements subject to a state transition.
  @begin[Warning]{dictionary}
    The function @sym{gtk-style-context-push-animatable-region} has been
    deprecated since version 3.6 and should not be used in newly written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (region-id :pointer))

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
;;; and should not be used in newly written code.
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
;;; and should not be used in newly written code.
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @begin{short}
    Removes the style provider from the style providers list in the style
    context.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-style-provider}
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
 "@version{2021-11-26}
  @argument[screen]{a @class{gdk-screen} object}
  @argument[provider]{a @class{gtk-style-provider} object}
  @begin{short}
    Removes the style provider from the global style providers list in the
    screen.
  @end{short}
  @see-class{gdk-screen}
  @see-class{gtk-style-provider}
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
 "@version{*2021-11-30}
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the background of the GDK window to the background pattern or color
    specified in the style context for its current state.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-set-background} function has been deprecated
    since version 3.18 and should not be used in newly written code. Use the
    @fun{gtk-render-background} function instead. Note that clients still using
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Restores the style context state to a previous stage.
  @end{short}
  See the @fun{gtk-style-context-save} function.
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-save}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_save" gtk-style-context-save) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @begin{short}
    Saves the style context state.
  @end{short}
  So all modifications done through the @fun{gtk-style-context-add-class},
  @fun{gtk-style-context-remove-class} or @fun{gtk-style-context-junction-sides}
  functions can be reverted in one go through the
  @fun{gtk-style-context-restore} function.
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
 "@version{*2021-12-17}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[classname]{a string with a class name to use in styling}
  @begin{short}
    Adds a style class to the context, so posterior calls to the
    @fun{gtk-style-context-property} function or any of the @sym{gtk-render-*}
    functions will make use of this new class for styling.
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
  (classname :string))

(export 'gtk-style-context-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_class" gtk-style-context-remove-class) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[classname]{a string with a class name to remove}
  @begin{short}
    Removes a class name from the style context.
  @end{short}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (classname :string))

(export 'gtk-style-context-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_has_class" gtk-style-context-has-class) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[classname]{a string with a class name}
  @return{@em{True} if the style context has @arg{classname} defined.}
  @begin{short}
    Returns @em{true} if the style context currently has defined the given
    class name.
  @end{short}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (classname :string))

(export 'gtk-style-context-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_classes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_list_classes" gtk-style-context-list-classes)
    (g-list :string)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
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
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[regionname]{a string with a region name to use in styling}
  @argument[flags]{a value of the @symbol{gtk-region-flags} flags that apply to
    the region}
  @begin{short}
    Adds a region to the style context, so posterior calls to the
    @fun{gtk-style-context-property} function or any of the @sym{gtk-render-*}
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
    The @sym{gtk-style-context-add-region} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-region-flags}
  @see-function{gtk-style-context-property}"
  (context (g-object gtk-style-context))
  (regionname :string)
  (flags gtk-region-flags))

(export 'gtk-style-context-add-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_region" gtk-style-context-remove-region)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[regionname]{a string with a region name to unset}
  @begin{short}
    Removes a region from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-remove-region} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context))
  (regionname :string))

(export 'gtk-style-context-remove-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_has_region" %gtk-style-context-has-region) :boolean
  (context (g-object gtk-style-context))
  (regionname :string)
  (flags (:pointer gtk-region-flags)))

(defun gtk-style-context-has-region (context regionname)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[regionname]{a string with a region name}
  @return{Returns a @symbol{gtk-region-flags} value.}
  @begin{short}
    Returns the region flags if the style context has the region defined.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-has-region} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{gtk-region-flags}"
  (with-foreign-object (flags 'gtk-region-flags)
    (when (%gtk-style-context-has-region context regionname flags)
      (mem-ref flags 'gtk-region-flags))))

(export 'gtk-style-context-has-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_regions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_list_regions" gtk-style-context-list-regions)
    (g-list :string)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @return{Returns a list of strings with the currently defined regions.}
  @begin{short}
    Returns the list of regions currently defined in the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-style-context-list-regions} function has been deprecated since
    version 3.14 and should not be used in newly written code.
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
 "@version{2021-11-26}
  @syntax[]{(gtk-style-context-scale context) => scale}
  @syntax[]{(setf (gtk-style-context-scale context) scale)}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[scale]{an integer with a scale}
  @begin{short}
    Accessor of the scale used for image assets for the style context.
  @end{short}

  The @sym{gtk-style-context-scale} function returns the scale used for image
  assets for the style context. The @sym{(setf gtk-style-context-scale)}
  function sets the scale.
  @see-class{gtk-style-context}"
  (context (g-object gtk-style-context)))

(export 'gtk-style-context-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_to_string ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_style_context_to_string" gtk-style-context-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[flags]{a value of the @symbol{gtk-style-context-print-flags} flags
    that determine what to print}
  @return{A string representing the style context.}
  @begin{short}
    Converts the style context into a string representation.
  @end{short}

  The string representation always includes information about the name, state,
  ID, visibility and style classes of the CSS node that is backing the style
  context. Depending on the flags, more information may be included.

  This function is intended for testing and debugging of the CSS implementation
  in GTK. There are no guarantees about the format of the returned string, it
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

(defcfun ("gtk_render_arrow" %gtk-render-arrow) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (angle :double)
  (x :double)
  (y :double)
  (size :double))

(defun gtk-render-arrow (context cr angle x y size)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[angle]{a number, coerced to a double float, with an arrow angle
    from 0 to 2 * Pi, being 0 the arrow pointing to the north}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[size]{a number, coerced to a double float, with the square side for
    render area}
  @begin{short}
    Renders an arrow pointing to an angle.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (%gtk-render-arrow context cr (coerce angle 'double-float)
                                (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce size 'double-float)))

(export 'gtk-render-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_background" %gtk-render-background) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-background (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-30}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[width]{a number, coerced to a double float, with the rectangle
    width}
  @argument[height]{a number, coerced to a double float, with the rectangle
    height}
  @begin{short}
    Renders the background of an element.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (%gtk-render-background context cr (coerce x 'double-float)
                                     (coerce y 'double-float)
                                     (coerce width 'double-float)
                                     (coerce height 'double-float)))

(export 'gtk-render-background)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background_get_clip () -> gtk-render-background-clip
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_render_background_get_clip" %gtk-render-background-clip) :void
  (context (g-object gtk-style-context))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (out-clip (g-boxed-foreign gdk-rectangle)))

#+gtk-3-20
(defun gtk-render-background-clip (context x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @return{A @class{gdk-rectangle} instance.}
  @begin{short}
    Returns the area that will be affected, i.e. drawn to, when calling the
    @fun{gtk-render-background} function for the given context and rectangle.
  @end{short}

  Since 3.20
  @see-class{gtk-style-context}
  @see-class{gdk-rectangle}
  @see-function{gtk-render-background}"
  (let ((out-clip (gdk-rectangle-new)))
    (%gtk-render-background-clip context (coerce x 'double-float)
                                         (coerce y 'double-float)
                                         (coerce width 'double-float)
                                         (coerce height 'double-float)
                                         out-clip)
    out-clip))

#+gtk-3-20
(export 'gtk-render-background-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_render_check ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_check" %gtk-render-check) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-check (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a checkmark as in a @class{gtk-check-button} widget.
  @end{short}

  The @code{:active} state of the @symbol{gtk-state-flags} flags determines
  whether the check is on or off, and the @code{:inconsistent} state determines
  whether it should be marked as undefined.
  @see-class{gtk-style-context}
  @see-class{gtk-check-button}
  @see-symbol{cairo-t}
  @see-symbol{gtk-state-flags}"
  (%gtk-render-check context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'gtk-render-check)

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_expander" %gtk-render-expander) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-expander (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an expander as used in the @class{gtk-tree-view} widget and
    @class{gtk-expander} widget in the area defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  The @code{:active} state of the @symbol{gtk-state-flags} flags determines
  whether the expander is collapsed or expanded.
  @see-class{gtk-style-context}
  @see-class{gtk-expander}
  @see-class{gtk-tree-view}
  @see-symbol{cairo-t}
  @see-symbol{gtk-state-flags}"
  (%gtk-render-expander context cr (coerce x 'double-float)
                                   (coerce y 'double-float)
                                   (coerce width 'double-float)
                                   (coerce height 'double-float)))

(export 'gtk-render-expander)

;;; ----------------------------------------------------------------------------
;;; gtk_render_extension ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_extension" %gtk-render-extension) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (gap-side gtk-position-type))

(defun gtk-render-extension (context cr x y width height side)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[side]{a value of the @symbol{gtk-position-type} enumeration where
    the gap is}
  @begin{short}
    Renders a extension as in a @class{gtk-notebook} widget tab in the rectangle
    defined by @arg{x}, @arg{y}, @arg{width}, @arg{height}.
  @end{short}
  The side where the extension connects to is defined by @arg{side}.
  @see-class{gtk-style-context}
  @see-class{gtk-notebook}
  @see-symbol{cairo-t}
  @see-symbol{gtk-position-type}"
  (%gtk-render-extension context cr (coerce x 'double-float)
                                    (coerce y 'double-float)
                                    (coerce width 'double-float)
                                    (coerce height 'double-float)
                                    side))

(export 'gtk-render-extension)

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_focus" %gtk-render-focus) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-focus (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a focus indicator on the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (%gtk-render-focus context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'gtk-render-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_frame" %gtk-render-frame) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-frame (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-30}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with the rectangle
    width}
  @argument[height]{a number, coerced to a double float, with the rectangle
    height}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (%gtk-render-frame context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'gtk-render-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame_gap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_frame_gap" %gtk-render-frame-gap) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (side gtk-position-type)
  (xy0 :double)
  (xy1 :double))

(defun gtk-render-frame-gap (context cr x y width height side xy0 xy1)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[side]{a value of the @symbol{gtk-position-type} enumeration where
    the gap is}
  @argument[xy0]{a number, coerced to a double float, with the initial
    coordinate for the gap}
  @argument[xy1]{a number, coerced to a double float, with the end coordinate
    for the gap}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height} leaving a gap on one side.
  @end{short}
  The arguments @arg{xy0} and @arg{xy1} will mean x coordinates for @code{:top}
  and @code{:bottom} gap sides, and y coordinates for @code{:left} and
  @code{:right} gap sides.
  @begin[Warning]{dictionary}
    The @sym{gtk-render-frame-gap} function has been deprecated since version
    3.24 and should not be used in newly written code. Use the
    @fun{gtk-render-frame} function instead. Themes can create gaps by omitting
    borders via CSS.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-symbol{gtk-position-type}
  @see-function{gtk-render-frame}"
  (%gtk-render-frame-gap context cr (coerce x 'double-float)
                                    (coerce y 'double-float)
                                    (coerce width 'double-float)
                                    (coerce height 'double-float)
                                    side
                                    (coerce xy0 'double-float)
                                    (coerce xy1 'double-float)))

(export 'gtk-render-frame-gap)

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_handle" %gtk-render-handle) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-handle (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a handle, as the resize grip of the @class{gtk-paned} widget and
    @class{gtk-window} widget, in the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk-style-context}
  @see-class{gtk-paned}
  @see-class{gtk-window}
  @see-symbol{cairo-t}"
  (%gtk-render-handle context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'gtk-render-handle)

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_layout" %gtk-render-layout) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (layout (g-object pango-layout)))

(defun gtk-render-layout (context cr x y layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[layout]{a @symbol{pango-layout} object to render}
  @begin{short}
    Renders a Pango layout on the coordinates @arg{x}, @arg{y}.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-symbol{pango-layout}"
  (%gtk-render-layout context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 layout))

(export 'gtk-render-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_render_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_line" %gtk-render-line) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun gtk-render-line (context cr x0 y0 x1 y1)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x0]{a number, coerced to a double float, with the x coordinate for
    the origin of the line}
  @argument[y0]{a number, coerced to a double float, with the y coordinate for
    the origin of the line}
  @argument[x1]{a number, coerced to a double float, with the x coordinate for
    the end of the line}
  @argument[y1]{a number, coerced to a double float, with the y coordinate for
    the end of the line}
  @begin{short}
    Renders a line from (@arg{x0}, @arg{y0}) to (@arg{x1}, @arg{y1}).
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}"
  (%gtk-render-line context cr (coerce x0 'double-float)
                               (coerce y0 'double-float)
                               (coerce x1 'double-float)
                               (coerce y1 'double-float)))

(export 'gtk-render-line)

;;; ----------------------------------------------------------------------------
;;; gtk_render_option ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_option" %gtk-render-option) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-option (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an option mark as in a @class{gtk-radio-button} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk-state-flags} flags will determine
  whether the option is on or off, and the @code{:inconsistent} state whether
  it should be marked as undefined.
  @see-class{gtk-style-context}
  @see-class{gtk-radio-button}
  @see-symbol{cairo-t}
  @see-symbol{gtk-state-flags}"
  (%gtk-render-option context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'gtk-render-option)

;;; ----------------------------------------------------------------------------
;;; gtk_render_slider ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_slider" %gtk-render-slider) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (orientation gtk-orientation))

(defun gtk-render-slider (context cr x y width height orientation)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @begin{short}
    Renders a slider as in the @class{gtk-scale} widget in the rectangle
    defined by @arg{x}, @arg{y}, @arg{width}, @arg{height}.
  @end{short}
  The @arg{orientation} argument defines whether the slider is vertical or
  horizontal.
  @see-class{gtk-style-context}
  @see-class{gtk-scale}
  @see-symbol{cairo-t}
  @see-symbol{gtk-orientation}"
  (%gtk-render-slider context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)
                                 orientation))

(export 'gtk-render-slider)

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_activity" %gtk-render-activity) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun gtk-render-activity (context cr x y width height)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an activity area such as in the @class{gtk-spinner} widget or the
    fill line in the @class{gtk-range} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk-stage-flags} flags determines
  whether there is activity going on.
  @see-class{gtk-style-context}
  @see-class{gtk-spinner}
  @see-class{gtk-range}
  @see-symbol{cairo-t}
  @see-symbol{gtk-state-flags}"
  (%gtk-render-activity context cr (coerce x 'double-float)
                                   (coerce y 'double-float)
                                   (coerce width 'double-float)
                                   (coerce height 'double-float)))

(export 'gtk-render-activity)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon_pixbuf" gtk-render-icon-pixbuf) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[source]{a @class{gtk-icon-source} instance specifying the icon to
    render}
  @argument[size]{a @symbol{gtk-icon-size} size to render the icon at}
  @return{A newly-created @class{gdk-pixbuf} object containing the rendered
    icon.}
  @begin{short}
    Renders the icon specified by @arg{source} at the given @arg{size},
    returning the result in a pixbuf.
  @end{short}
  A size of -1 means render at the size of the source and do not scale.
  @begin[Warning]{dictionary}
    The @sym{gtk-render-icon-pixbuf} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk-icon-theme-load-icon} function instead.
  @end{dictionary}
  @see-class{gtk-style-context}
  @see-class{gtk-icon-source}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-icon-theme-load-icon}"
  (context (g-object gtk-style-context))
  (source (g-boxed-foreign gtk-icon-source))
  (size gtk-icon-size))

(export 'gtk-render-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon_surface" %gtk-render-icon-surface) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (surface (:pointer (:struct cairo-surface-t)))
  (x :double)
  (y :double))

(defun gtk-render-icon-surface (context cr surface x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[surface]{a @symbol{cairo-surface-t} instance containing the icon
    to draw}
  @argument[x]{a number, coerced to a double float, with a x position for the
    icon}
  @argument[y]{a number, coerced to a double float, with a y position for the
    icon}
  @begin{short}
    Renders the icon in the Cairo surface at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-symbol{cairo-surface-t}"
  (%gtk-render-icon-surface context cr surface (coerce x 'double-float)
                                               (coerce y 'double-float)))

(export 'gtk-render-icon-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon" %gtk-render-icon) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (pixbuf (g-object gdk-pixbuf))
  (x :double)
  (y :double))

(defun gtk-render-icon (context cr pixbuf x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[pixbuf]{a @class{gdk-pixbuf} object containing the icon to draw}
  @argument[x]{a number, coerced to a double float, with the x position for the
    pixbuf}
  @argument[y]{a number, coerced to a double float, with the y position for the
    pixbuf}
  @begin{short}
    Renders the icon in a pixbuf at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-class{gdk-pixbuf}"
  (%gtk-render-icon context cr pixbuf (coerce x 'double-float)
                                      (coerce y 'double-float)))

(export 'gtk-render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_render_insertion_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_insertion_cursor" %gtk-render-insertion-cursor) :void
  (context (g-object gtk-style-context))
  (cr (:pointer (:struct cairo-t)))
  (x :double)
  (y :double)
  (layout (g-object pango-layout))
  (index :int)
  (direction pango-direction))

(defun gtk-render-insertion-cursor (context cr x y layout index direction)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-26}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[cr]{a @symbol{cairo-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin}
  @argument[y]{a number, coerced to a double float, with the y origin}
  @argument[layout]{the @class{pango-layout} object of the text}
  @argument[index]{an integer with the index in the Pango layout}
  @argument[direction]{a value of the @symbol{pango-direction} enumeration}
  @begin{short}
    Draws a text caret on the Cairo context at the specified index of the
    Pango layout.
  @end{short}
  @see-class{gtk-style-context}
  @see-symbol{cairo-t}
  @see-class{pango-layout}
  @see-symbol{pango-direction}"
  (%gtk-render-insertion-cursor context cr (coerce x 'double-float)
                                           (coerce y 'double-float)
                                           layout
                                           index
                                           direction))

(export 'gtk-render-insertion-cursor)

;;; --- End of file gtk.style-context.lisp -------------------------------------

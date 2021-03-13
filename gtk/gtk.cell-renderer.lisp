;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer.lisp
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
;;; GtkCellRenderer
;;;
;;;     An object for rendering a single cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererState
;;;     GtkCellRendererMode
;;;     GtkCellRenderer
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_class_set_accessible_type
;;;     gtk_cell_renderer_get_aligned_area
;;;     gtk_cell_renderer_get_size
;;;     gtk_cell_renderer_render
;;;     gtk_cell_renderer_activate
;;;     gtk_cell_renderer_start_editing
;;;     gtk_cell_renderer_stop_editing
;;;     gtk_cell_renderer_get_fixed_size
;;;     gtk_cell_renderer_set_fixed_size
;;;     gtk_cell_renderer_get_visible                      Accessor
;;;     gtk_cell_renderer_set_visible                      Accessor
;;;     gtk_cell_renderer_get_sensitive                    Accessor
;;;     gtk_cell_renderer_set_sensitive                    Accessor
;;;     gtk_cell_renderer_get_alignment
;;;     gtk_cell_renderer_set_alignment
;;;     gtk_cell_renderer_get_padding
;;;     gtk_cell_renderer_set_padding
;;;     gtk_cell_renderer_get_state
;;;     gtk_cell_renderer_is_activatable
;;;     gtk_cell_renderer_get_preferred_height
;;;     gtk_cell_renderer_get_preferred_height_for_width
;;;     gtk_cell_renderer_get_preferred_size
;;;     gtk_cell_renderer_get_preferred_width
;;;     gtk_cell_renderer_get_preferred_width_for_height
;;;     gtk_cell_renderer_get_request_mode
;;;
;;; Properties
;;;
;;;               gchar*   cell-background         Write
;;;            GdkColor*   cell-background-gdk     Read / Write
;;;             GdkRGBA*   cell-background-rgba    Read / Write
;;;            gboolean    cell-background-set     Read / Write
;;;            gboolean    editing                 Read
;;;                gint    height                  Read / Write
;;;            gboolean    is-expanded             Read / Write
;;;            gboolean    is-expander             Read / Write
;;; GtkCellRendererMode    mode                    Read / Write
;;;            gboolean    sensitive               Read / Write
;;;            gboolean    visible                 Read / Write
;;;                gint    width                   Read / Write
;;;              gfloat    xalign                  Read / Write
;;;               guint    xpad                    Read / Write
;;;              gfloat    yalign                  Read / Write
;;;               guint    ypad                    Read / Write
;;;
;;; Signals
;;;
;;;                void    editing-canceled        Run First
;;;                void    editing-started         Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ├── GtkCellRendererText
;;;             ├── GtkCellRendererPixbuf
;;;             ├── GtkCellRendererProgress
;;;             ├── GtkCellRendererSpinner
;;;             ╰── GtkCellRendererToggle
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererState
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkCellRendererState" gtk-cell-renderer-state
  (:export t
   :type-initializer "gtk_cell_renderer_state_get_type")
  (:selected    #.(ash 1 0))
  (:prelit      #.(ash 1 1))
  (:insensitive #.(ash 1 2))
  (:sorted      #.(ash 1 3))
  (:focused     #.(ash 1 4))
  (:expandable  #.(ash 1 5))
  (:expanded    #.(ash 1 6)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-state atdoc:*symbol-name-alias*)
      "Flags"
      (gethash 'gtk-cell-renderer-state atdoc:*external-symbols*)
 "@version{2021-3-7}
  @short{Tells how a cell is to be rendererd.}
  @begin{pre}
(define-g-flags \"GtkCellRendererState\" gtk-cell-renderer-state
  (:export t
   :type-initializer \"gtk_cell_renderer_state_get_type\")
  (:selected    #.(ash 1 0))
  (:prelit      #.(ash 1 1))
  (:insensitive #.(ash 1 2))
  (:sorted      #.(ash 1 3))
  (:focused     #.(ash 1 4))
  (:expandable  #.(ash 1 5))
  (:expanded    #.(ash 1 6)))
  @end{pre}
  @begin[code]{table}
    @entry[:selected]{The cell is currently selected, and probably has a
      selection colored background to render to.}
    @entry[:prelit]{The mouse is hovering over the cell.}
    @entry[:insensitive]{The cell is drawn in an insensitive manner.}
    @entry[:sorted]{The cell is in a sorted row.}
    @entry[:focused]{The cell is in the focus row.}
    @entry[:expandable]{The cell is in a row that can be expanded.}
    @entry[:expanded]{The cell is in a row that is expanded.}
  @end{table}
  @see-class{gtk-cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCellRendererMode" gtk-cell-renderer-mode
  (:export t
   :type-initializer "gtk_cell_renderer_mode_get_type")
  (:inert 0)
  (:activatable 1)
  (:editable 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-mode atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-cell-renderer-mode atdoc:*external-symbols*)
 "@version{2021-3-7}
  @short{Identifies how the user can interact with a particular cell.}
  @begin{pre}
(define-g-enum \"GtkCellRendererMode\" gtk-cell-renderer-mode
  (:export t
   :type-initializer \"gtk_cell_renderer_mode_get_type\")
  (:inert 0)
  (:activatable 1)
  (:editable 2))
  @end{pre}
  @begin[code]{table}
    @entry[:inert]{The cell is just for display and cannot be interacted with.
      Note that this does not mean that e.g. the row being drawn cannot be
      selected - just that a particular element of it cannot be individually
      modified.}
    @entry[:activatable]{The cell can be clicked.}
    @entry[:editable]{The cell can be edited or otherwise modified.}
  @end{table}
  @see-class{gtk-cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRenderer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRenderer" gtk-cell-renderer
  (:superclass g-initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_get_type")
  ((cell-background
    gtk-cell-renderer-cell-background
    "cell-background" "gchararray" nil t)
   (cell-background-gdk
    gtk-cell-renderer-cell-background-gdk
    "cell-background-gdk" "GdkColor" t t)
   (cell-background-rgba
    gtk-cell-renderer-cell-background-rgba
    "cell-background-rgba" "GdkRGBA" t t)
   (cell-background-set
    gtk-cell-renderer-cell-background-set
    "cell-background-set" "gboolean" t t)
   (editing
    gtk-cell-renderer-editing
    "editing" "gboolean" t nil)
   (height
    gtk-cell-renderer-height
    "height" "gint" t t)
   (is-expanded
    gtk-cell-renderer-is-expanded
    "is-expanded" "gboolean" t t)
   (is-expander
    gtk-cell-renderer-is-expander
    "is-expander" "gboolean" t t)
   (mode
    gtk-cell-renderer-mode
    "mode" "GtkCellRendererMode" t t)
   (sensitive
    gtk-cell-renderer-sensitive
    "sensitive" "gboolean" t t)
   (visible
    gtk-cell-renderer-visible
    "visible" "gboolean" t t)
   (width
    gtk-cell-renderer-width
    "width" "gint" t t)
   (xalign
    gtk-cell-renderer-xalign
    "xalign" "gfloat" t t)
   (xpad
    gtk-cell-renderer-xpad
    "xpad" "guint" t t)
   (yalign
    gtk-cell-renderer-yalign
    "yalign" "gfloat" t t)
   (ypad
    gtk-cell-renderer-ypad
    "ypad" "guint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer 'type)
 "@version{2021-3-2}
  @begin{short}
    The @sym{gtk-cell-renderer} class is a base class of a set of objects used
    for rendering a cell to a @symbol{cairo-t} context.
  @end{short}
  These objects are used primarily by the @class{gtk-tree-view} widget, though
  they are not tied to them in any specific way. It is worth noting that the
  @sym{gtk-cell-renderer} object is not a @class{gtk-widget} object and cannot
  be treated as such.

  The primary use of a @sym{gtk-cell-renderer} object is for drawing a certain
  graphical elements on a Cairo context. Typically, one cell renderer is used
  to draw many cells on the screen. To this extent, it is not expected that a
  @sym{gtk-cell-renderer} object keep any permanent state around. Instead, any
  state is set just prior to use using GObjects property system. Then, the cell
  is measured using the function @fun{gtk-cell-renderer-preferred-size}.
  Finally, the cell is rendered in the correct location using the function
  @fun{gtk-cell-renderer-render}.

  There are a number of rules that must be followed when writing a new
  @sym{gtk-cell-renderer}. First and formost, its important that a certain set
  of properties will always yield a cell renderer of the same size, barring a
  @code{GtkStyle} change. The @sym{gtk-cell-renderer} also has a number of
  generic properties that are expected to be honored by all children.

  Beyond merely rendering a cell, cell renderers can optionally provide active
  user interface elements. A cell renderer can be \"activatable\" like the
  @class{gtk-cell-renderer-toggle} object, which toggles when it gets activated
  by a mouse click, or it can be \"editable\" like the
  @class{gtk-cell-renderer-text} object, which allows the user to edit the text
  using a @class{gtk-entry} widget. To make a cell renderer activatable or
  editable, you have to implement the @code{GtkCellRendererClass.activate} or
  @code{GtkCellRendererClass.start_editing} virtual functions, respectively.

  Many properties of the @sym{gtk-cell-renderer} class and its subclasses have
  a corresponding @code{set} property, e.g. @code{cell-background-set}
  corresponds to @code{cell-background}. These @code{set} properties reflect
  whether a property has been set or not. You should not set them independently.
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-canceled\" signal}
      @begin{pre}
 lambda (renderer)    : Run First
      @end{pre}
      This signal gets emitted when the user cancels the process of editing a
      cell. For example, an editable cell renderer could be written to cancel
      editing when the user presses Escape. See also the function
      @fun{gtk-cell-renderer-stop-editing}.
      @begin[code]{table}
        @entry[renderer]{The @sym{gtk-cell-renderer} object which received the
          signal.}
      @end{table}
    @subheading{The \"editing-started\" signal}
      @begin{pre}
 lambda (renderer editable path)    : Run First
      @end{pre}
      This signal gets emitted when a cell starts to be edited. The intended
      use of this signal is to do special setup on editable, e.g. adding a
      @class{gtk-entry-completion} object or setting up additional columns in a
      @class{gtk-combo-box} widget. Note that GTK+ does not guarantee that cell
      renderers will continue to use the same kind of widget for editing in
      future releases, therefore you should check the type of editable before
      doing any specific setup.
      @begin[code]{table}
        @entry[renderer]{The @sym{gtk-cell-renderer} object which received the
          signal.}
        @entry[editable]{The @class{gtk-cell-editable} widget.}
        @entry[path]{A string with the path identifying the edited cell.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-renderer-cell-background}
  @see-slot{gtk-cell-renderer-cell-background-gdk}
  @see-slot{gtk-cell-renderer-cell-background-rgba}
  @see-slot{gtk-cell-renderer-cell-background-set}
  @see-slot{gtk-cell-renderer-editing}
  @see-slot{gtk-cell-renderer-height}
  @see-slot{gtk-cell-renderer-is-expanded}
  @see-slot{gtk-cell-renderer-is-expander}
  @see-slot{gtk-cell-renderer-mode}
  @see-slot{gtk-cell-renderer-sensitive}
  @see-slot{gtk-cell-renderer-visible}
  @see-slot{gtk-cell-renderer-width}
  @see-slot{gtk-cell-renderer-xalign}
  @see-slot{gtk-cell-renderer-xpad}
  @see-slot{gtk-cell-renderer-yalign}
  @see-slot{gtk-cell-renderer-ypad}
  @see-class{gtk-cell-editable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-cell-background --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background} property of type @code{:string} (Write) @br{}
  Cell background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background 'function)
 "@version{2021-3-2}
  @syntax[]{(setf (gtk-cell-renderer-cell-background object) background)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[background]{a string with the cell background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{cell-background} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Cell background color as a string. This property is not readable. After
  setting the background color is readable with the function
  @fun{gtk-cell-renderer-cell-background-rgba}.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-cell-background-rgba}")

;;; --- gtk-cell-renderer-cell-background-gdk ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-gdk"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-gdk} property of type @class{gdk-color}
  (Read / Write) @br{}
  Cell background color. @br{}
  @em{Warning:} The @code{cell-background-gdk} property has been deprecated
  since version 3.4 and should not be used in newly-written code. Use the
  @code{cell-background-rgba} property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-gdk
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-gdk 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-cell-background-gdk object) => background}
  @syntax[]{(setf (gtk-cell-renderer-cell-background-gdk object) background)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[background]{a @class{gdk-color} color with the cell background
    color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{cell-background-gdk} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Cell background color.
  @begin[Warning]{dictionary}
    The function @sym{gtk-cell-renderer-cell-background-gdk} has been deprecated
    since version 3.4 and should not be used in newly-written code. Use the
    function @fun{gtk-cell-renderer-cell-background-rgba} instead.
  @end{dictionary}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-cell-background-rgba}")

;;; --- gtk-cell-renderer-cell-background-rgba ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-rgba"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-rgba} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  Cell background RGBA color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-rgba
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-rgba 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-cell-background-rgba object) => background}
  @syntax[]{(setf (gtk-cell-renderer-cell-background-rgba object) background)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[background]{a @class{gdk-rgba} color with the cell background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{cell-background-rgba} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Cell background RGBA color.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-cell-background-set}")

;;; --- gtk-cell-renderer-cell-background-set ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-set"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the cell background color. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-set 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-cell-background-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-cell-background-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[setting]{a boolean wether this tag affects the cell background
    color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{cell-background-set} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Whether this tag affects the cell background color.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-cell-background}
  @see-function{gtk-cell-renderer-cell-background-rgba}")

;;; --- gtk-cell-renderer-editing ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editing" 'gtk-cell-renderer) 't)
 "The @code{editing} property of type @code{:boolean} (Read) @br{}
  Whether the cell renderer is currently in editing mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-editing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-editing 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-editing object) => setting}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[setting]{a boolean wether the cell renderer is in editing mode}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{editing} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Whether the cell renderer is currently in editing mode.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-height -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height" 'gtk-cell-renderer) 't)
 "The @code{height} property of type @code{:int} (Read / Write) @br{}
  The fixed height. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-height 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-height object) => height}
  @syntax[]{(setf (gtk-cell-renderer-height object) height)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[height]{an integer with the fixed height}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{height} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The fixed height.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-width}")

;;; --- gtk-cell-renderer-is-expanded ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-expanded"
                                               'gtk-cell-renderer) 't)
 "The @code{is-expanded} property of type @code{:boolean} (Read / Write) @br{}
  Row is an expander row, and is expanded. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-is-expanded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-is-expanded 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-is-expanded object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-is-expanded object) setting)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[setting]{a boolean wether the row is expanded}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{is-expanded} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Row is an expander row, and is expanded.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-is-expander ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-expander"
                                               'gtk-cell-renderer) 't)
 "The @code{is-expander} property of type @code{:boolean} (Read / Write) @br{}
  Row has children. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-is-expander atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-is-expander 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-is-expander object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-is-expander object) setting)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[setting]{a boolean wether the row has children}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{is-expander} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Row has children.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-mode -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-cell-renderer) 't)
 "The @code{mode} property of type @symbol{gtk-cell-renderer-mode}
  (Read / Write) @br{}
  Editable mode of the cell renderer. @br{}
  Default value: @code{:inert}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-mode 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-mode object) => mode}
  @syntax[]{(setf (gtk-cell-renderer-mode object) mode)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[mode]{a value of the @symbol{gtk-cell-renderer-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{mode} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  Editable mode of the cell renderer.
  @see-class{gtk-cell-renderer}
  @see-sybmol{gtk-cell-renderer-mode}")

;;; --- gtk-cell-renderer-sensitive --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sensitive"
                                               'gtk-cell-renderer) 't)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Display the cell sensitive. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-sensitive 'function)
 "@version{*2021-3-2}
  @syntax[]{(gtk-cell-renderer-sensitive object) => sensitive}
  @syntax[]{(setf (gtk-cell-renderer-sensitive object) sensitive)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[sensitive]{a boolean with the sensitivity of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{sensitive} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The slot access function @sym{gtk-cell-renderer-sensitive} returns the cell
  renderer's sensitivity. The slot access function
  @sym{(setf gtk-cell-renderer-sensitive)} sets the sensitivity.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-visible ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-cell-renderer) 't)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Display the cell. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-visible 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-visible object) => visible}
  @syntax[]{(setf (gtk-cell-renderer-visible object) visible)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[visible]{a boolean with the visibility of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{visible} of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The slot access function @sym{gtk-cell-renderer-sensitive} returns the cell
  renderer's visibility. The slot access function
  @sym{(setf gtk-cell-renderer-sensitive)} sets the visibility.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-width ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width" 'gtk-cell-renderer) 't)
 "The @code{width} property of type @code{:int} (Read / Write) @br{}
  The fixed width. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-width 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-width object) => width}
  @syntax[]{(setf (gtk-cell-renderer-width object) width)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[width]{an integer with the fixed width}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{width} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The fixed width.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-height}")

;;; --- gtk-cell-renderer-xalign -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-cell-renderer) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for
  RTL layouts. @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-xalign 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-xalign object) => align}
  @syntax[]{(setf (gtk-cell-renderer-xalign object) align)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[align]{a float with the x-align}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{xalign} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for RTL
  layouts.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-yalign}")

;;; --- gtk-cell-renderer-xpad -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xpad" 'gtk-cell-renderer) 't)
 "The @code{xpad} property of type @code{:uint} (Read / Write) @br{}
  The amount of space to add on the left and right, in pixels. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-xpad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-xpad 'function)
 "@version{*2021-3-13}
  @syntax[]{(gtk-cell-renderer-xpad object) => padding}
  @syntax[]{(setf (gtk-cell-renderer-xpad object) padding)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[padding]{a unsigned integer with the padding}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{xpad} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The amount of space to add on the left and right, in pixels.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-ypad}")

;;; --- gtk-cell-renderer-yalign -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-cell-renderer) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment, from 0.0 (top) to 1.0 (bottom). @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-yalign 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-yalign object) => align}
  @syntax[]{(setf (gtk-cell-renderer-yalign object) align)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[align]{a float with the y-align}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{yalign} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The vertical alignment, from 0.0 (top) to 1.0 (bottom).
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-xalign}")

;;; --- gtk-cell-renderer-ypad -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ypad" 'gtk-cell-renderer) 't)
 "The @code{ypad} property of tpye @code{:uint} (Read / Write) @br{}
  The amount of space to add on the top and bottom, in pixels. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-ypad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-ypad 'function)
 "@version{2021-3-2}
  @syntax[]{(gtk-cell-renderer-ypad object) => padding}
  @syntax[]{(setf (gtk-cell-renderer-ypad object) padding)}
  @argument[object]{a @class{gtk-cell-renderer} object}
  @argument[padding]{a unsigned integer with the padding}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{ypad} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The amount of space to add on the top and bottom, in pixels.
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-renderer-xpad}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_class_set_accessible_type ()
;;;
;;; void
;;; gtk_cell_renderer_class_set_accessible_type
;;;                                (GtkCellRendererClass *renderer_class,
;;;                                 GType type);
;;;
;;; Sets the type to be used for creating accessibles for cells rendered by cell
;;; renderers of renderer_class . Note that multiple accessibles will be
;;; created.
;;;
;;; This function should only be called from class init functions of cell
;;; renderers.
;;;
;;; renderer_class :
;;;     class to set the accessible type for
;;;
;;; type :
;;;     The object type that implements the accessible for widget_class . The
;;;     type must be a subtype of GtkRendererCellAccessible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_aligned_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_aligned_area" %gtk-cell-renderer-aligned-area)
    :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (flags gtk-cell-renderer-state)
  (area (g-boxed-foreign gdk-rectangle))
  (aligned (g-boxed-foreign gdk-rectangle)))

(defun gtk-cell-renderer-aligned-area (cell widget flags area)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell will be rendering
    to}
  @argument[flags]{the @symbol{gtk-cell-renderer-state} render flags}
  @argument[area]{a @class{gdk-rectangle} instance  with the cell area which
    would be passed to the function @fun{gtk-cell-renderer-render}}
  @begin{return}
    A @class{gdk-rectangle} area for the space inside @arg{area} that would
    acually be used to render.
  @end{return}
  @begin{short}
    Gets the aligned area used by @arg{cell} inside @arg{area}.
  @end{short}
  Used for finding the appropriate edit and focus rectangle.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-symbol{gtk-cell-renderer-state}
  @see-function{gtk-cell-renderer-render}"
  (let ((aligned (gdk-rectangle-new)))
    (%gtk-cell-renderer-aligned-area cell widget flags area aligned)
    aligned))

(export 'gtk-cell-renderer-aligned-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_size () -> gtk-cell-renderer-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_size" %gtk-cell-renderer-size) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (area (g-boxed-foreign gdk-rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-cell-renderer-size (cell widget area)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object the renderer is rendering to}
  @argument[area]{a @class{gdk-rectangle} with the area a cell will be
    allocated, or @code{nil}}
  @begin{return}
    @code{x-offset} -- an integer with the x offset of cell relative to
                       @arg{area}, or @code{nil} @br{}
    @code{y-offset} -- an integer with the y offset of cell relative to
                       @arg{area}, or @code{nil} @br{}
    @code{width}    -- an integer with the width needed to render a cell,
                       or @code{nil} @br{}
    @code{height}   -- an integer with the height needed to render a cell,
                       or @code{nil}
  @end{return}
  @begin{short}
    Obtains the width and height needed to render the cell.
  @end{short}
  Used by tree view widgets to determine the appropriate size for the
  cell area passed to the function @fun{gtk-cell-renderer-render}. If
  @arg{area} is not @code{nil}, fills in the x and y offsets (if set)
  of the cell relative to this location.

  Please note that the values set in @arg{width} and @arg{height}, as well as
  those in @arg{x-offset} and @arg{y-offset} are inclusive of the
  @slot[gtk-cell-renderer]{xpad} and @slot[gtk-cell-renderer]{ypad} properties.
  @begin[Warning]{dictionary}
    The function @sym{gtk-cell-renderer-size} has been deprecated since version
    3.0 and should not be used in newly-written code. Use the function
    @fun{gtk-cell-renderer-preferred-size} instead.
  @end{dictionary}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-function{gtk-cell-renderer-render}
  @see-function{gtk-cell-renderer-preferred-size}"
  (with-foreign-objects ((x-offset :int)
                         (y-offset :int)
                         (width :int)
                         (height :int))
    (%gtk-cell-renderer-size cell
                             widget
                             area
                             x-offset
                             y-offset
                             width
                             height)
    (values (mem-ref x-offset :int)
            (mem-ref y-offset :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-cell-renderer-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_render ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_render" gtk-cell-renderer-render) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[cr]{a @symbol{cario-t} context to draw to}
  @argument[widget]{a @class{gtk-widget} object owning window}
  @argument[background]{a @class{gdk-rectangle} instance with entire cell area
    including tree expanders and maybe padding on the sides}
  @argument[cell]{a @class{gdk-rectangle} instance with the area normally
    rendered by a cell renderer}
  @argument[flags]{the @symbol{gtk-cell-renderer-state} flags] that affect
    rendering}
  @begin{short}
    Invokes the virtual render function of the cell renderer.
  @end{short}
  The passed-in rectangles are areas in @arg{cr}. Most renderers will draw
  within @arg{area}. The @code{xalign}, @code{yalign}, @code{xpad},
  and @code{ypad} fields of the cell renderer should be honored with respect to
  @arg{area}. The argument @arg{background} includes the blank space around the
  cell, and also the area containing the tree expander. So the @arg{background}
  rectangles for all cells tile to cover the entire window.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-symbol{cairo-t}
  @see-symbol{gtk-cell-renderer-state}"
  (cell (g-object gtk-cell-renderer))
  (cr (:pointer (:struct cairo-t)))
  (widget (g-object gtk-widget))
  (background (g-boxed-foreign gdk-rectangle))
  (area (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-renderer-render)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_activate" gtk-cell-renderer-activate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[event]{a @class{gdk-event} event}
  @argument[widget]{a @class{gtk-widget} object that received the event}
  @argument[path]{widget-dependent string representation of the event location,
    e.g. for a @class{gtk-tree-view} widget, a string representation of
    @class{gtk-tree-path} instance}
  @argument[background]{a @class{gdk-rectangle} with the background area as
    passed to the function @fun{gtk-cell-renderer-render}}
  @argument[area]{a @class{gdk-rectangle} instance with the cell area as passed
    to the function @fun{gtk-cell-renderer-render}}
  @argument[flags]{the @symbol{gtk-cell-renderer-state} render flags}
  @return{@em{True} if the event was consumed/handled.}
  @begin{short}
    Passes an activate event to the cell renderer for possible processing.
  @end{short}
  Some cell renderers may use events. For example, the
  @class{gtk-cell-renderer-toggle} object toggles when it gets a mouse click.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-cell-renderer-toggle}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-class{gdk-event}
  @see-symbol{gtk-cell-renderer-state}
  @see-function{gtk-cell-renderer-render}"
  (cell (g-object gtk-cell-renderer))
  (event (g-boxed-foreign gdk-event))
  (widget (g-object gtk-widget))
  (path :string)
  (background (g-boxed-foreign gdk-rectangle))
  (area (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-renderer-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_start_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_start_editing" gtk-cell-renderer-start-editing)
    (g-object gtk-cell-editable)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[event]{a @class{gdk-event} event}
  @argument[widget]{a @class{gtk-widget} object that received the event}
  @argument[path]{widget-dependent string representation of the event location,
    e.g. for @class{gtk-tree-view} widget, a string representation of
    a @class{gtk-tree-path} instance}
  @argument[background]{a @class{gdk-rectangle} instance with the background
    area as passed to the function @fun{gtk-cell-renderer-render}}
  @argument[area]{a @class{gdk-rectangle} instance with the cell area as passed
    to the function @fun{gtk-cell-renderer-render}}
  @argument[flags]{the @symbol{gtk-cell-renderer-state} render flags}
  @return{A new @class{gtk-cell-editable} widget, or @code{nil}.}
  @begin{short}
    Passes an activate event to the cell renderer for possible processing.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-class{gdk-event}
  @see-class{gtk-cell-editable}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gdk-rectangle}
  @see-symbol{gtk-cell-renderer-state}
  @see-function{gtk-cell-renderer-render}"
  (cell (g-object gtk-cell-renderer))
  (event (g-boxed-foreign gdk-event))
  (widget (g-object gtk-widget))
  (path :string)
  (background (g-boxed-foreign gdk-rectangle))
  (area (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-renderer-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_stop_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_stop_editing" gtk-cell-renderer-stop-editing) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[canceled]{@em{true} if the editing has been canceled}
  @begin{short}
    Informs the cell renderer that the editing is stopped.
  @end{short}
  If @arg{canceled} is @em{true}, the cell renderer will emit the
  \"editing-canceled\" signal.

  This function should be called by cell renderer implementations in response
  to the \"editing-done\" signal of the @class{gtk-cell-editable} widget.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-cell-editable}
  @see-function{gtk-cell-renderer-start-editing}"
  (cell (g-object gtk-cell-renderer))
  (canceled :boolean))

(export 'gtk-cell-renderer-stop-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_fixed_size ()
;;; gtk_cell_renderer_set_fixed_size () -> gtk-cell-renderer-fixed-size
;;; ----------------------------------------------------------------------------

(defun (setf gtk-cell-renderer-fixed-size) (value cell)
  (destructuring-bind (width height) value
    (foreign-funcall "gtk_cell_renderer_set_fixed_size"
                     (g-object gtk-cell-renderer) cell
                     :int width
                     :int height
                     :void)
    (values width height)))

(defcfun ("gtk_cell_renderer_get_fixed_size" %gtk-cell-renderer-fixed-size)
    :void
  (cell (g-object gtk-cell-renderer))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-cell-renderer-fixed-size (cell)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @syntax[]{(gtk-cell-renderer-fixed-size cell) => width, height}
  @syntax[]{(setf (gtk-cell-renderer-fixe-size cell) (list width height))}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[width]{an integer with the width of the cell renderer, or -1}
  @argument[height]{an integer with the height of the cell renderer, or -1}
  @begin{short}
    The function @sym{gtk-cell-renderer-fixed-size} returns @arg{width} and
    @arg{height} with the appropriate size of @arg{cell}.
  @end{short}
  The function @sym{(setf gtk-cell-renderer-fixed-size)} sets the renderer size
  to be explicit, independent of the properties set.
  @see-class{gtk-cell-renderer}"
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-cell-renderer-fixed-size cell width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-cell-renderer-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_alignment ()
;;; gtk_cell_renderer_set_alignment () -> gtk-cell-renderer-alignment
;;; ----------------------------------------------------------------------------

(defun (setf gtk-cell-renderer-alignment) (value cell)
  (destructuring-bind (xalign yalign) value
    (foreign-funcall "gtk_cell_renderer_set_alignment"
                     (g-object gtk-cell-renderer) cell
                     :float xalign
                     :float yalign
                     :void)
     (values xalign yalign)))

(defcfun ("gtk_cell_renderer_get_alignment" %gtk-cell-renderer-alignment) :void
  (cell (g-object gtk-cell-renderer))
  (xalign (:pointer :float))
  (yalign (:pointer :float)))

(defun gtk-cell-renderer-alignment (cell)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @syntax[]{(gtk-cell-renderer-alignment cell) => xalign, yalign}
  @syntax[]{(setf (gtk-cell-renderer-alignment cell) (list xalign yalign))}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[xalign]{a float with the x alignment of the cell renderer}
  @argument[yalign]{a float with the y alignment of the cell renderer}
  @begin{short}
    The function @sym{gtk-cell-renderer-alignment} returns the appropriate
    @arg{xalign} and @arg{yalign} of @arg{cell}.
  @end{short}
  The function @sym{(setf gtk-cell-renderer-alignment)} sets the cell renderer's
  alignment within its available space.
  @see-class{gtk-cell-renderer}"
  (with-foreign-objects ((xalign :float) (yalign :float))
    (%gtk-cell-renderer-alignment cell xalign yalign)
    (values (mem-ref xalign :float)
            (mem-ref yalign :float))))

(export 'gtk-cell-renderer-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_padding ()
;;; gtk_cell_renderer_set_padding () -> gtk-cell-renderer-padding
;;; ----------------------------------------------------------------------------

(defun (setf gtk-cell-renderer-padding) (value cell)
  (destructuring-bind (xpad ypad) value
    (foreign-funcall "gtk_cell_renderer_set_padding"
                     (g-object gtk-cell-renderer) cell
                     :int xpad
                     :int ypad
                     :void)
     (values xpad ypad)))

(defcfun ("gtk_cell_renderer_get_padding" %gtk-cell-renderer-padding) :void
  (cell (g-object gtk-cell-renderer))
  (xpad (:pointer :int))
  (ypad (:pointer :int)))

(defun gtk-cell-renderer-padding (cell)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @syntax[]{(gtk-cell-renderer-padding cell) => xpad, ypad}
  @syntax[]{(setf gtk-cell-renderer-padding cell) (list xpad ypad))}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[xpad]{an integer with the x padding of the cell renderer}
  @argument[ypad]{an integer with the y padding of the cell renderer}
  @begin{short}
    The function @sym{gtk-cell-renderer-padding} returns the appropriate
    @arg{xpad} and @arg{ypad} of the cell renderer.
  @end{short}
  The function @sym{(setf gtk-cell-renderer-padding)} sets the cell renderer's
  padding.
  @see-class{gtk-cell-renderer}"
  (with-foreign-objects ((xpad :int) (ypad :int))
    (%gtk-cell-renderer-padding cell xpad ypad)
    (values (mem-ref xpad :int)
            (mem-ref ypad :int))))

(export 'gtk-cell-renderer-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_state" gtk-cell-renderer-state)
    gtk-state-flags
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer}, or @code{nil}}
  @argument[widget]{a @class{gtk-widget}, or @code{nil}}
  @argument[state]{the @symbol{gtk-cell-renderer-state} cell renderer state}
  @return{The widget @symbol{gtk-state-flags} state flags applying to the cell
    renderer.}
  @begin{short}
    Translates the cell renderer state to @symbol{gtk-state-flags} flags,
    based on the cell renderer and widget sensitivity, and the given
    @symbol{gtk-cell-renderer-state} flags.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-symbol{gtk-state-flags}
  @see-symbol{gtk-cell-renderer-state}"
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (state gtk-cell-renderer-state))

(export 'gtk-cell-renderer-state)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_is_activatable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_is_activatable" gtk-cell-renderer-is-activatable)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @return{@em{True} if the cell renderer can do anything when activated.}
  @begin{short}
    Checks whether the cell renderer can do something when activated.
  @end{short}
  @see-class{gtk-cell-renderer}"
  (cell (g-object gtk-cell-renderer)))

(export 'gtk-cell-renderer-is-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_preferred_height"
          %gtk-cell-renderer-preferred-height) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (minimum-size (:pointer :int))
  (natural-size (:pointer :int)))

(defun gtk-cell-renderer-preferred-height (cell widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell renderer will be
    rendering to}
  @begin{return}
    @code{minimum-size} -- an integer with the minimum size @br{}
    @code{natural-size} -- an integer with the natural size
  @end{return}
  @begin{short}
    Retreives a cell renderer's natural size when rendered to widget.
  @end{short}
  @see-class{gtk-cell-renderer}"
  (with-foreign-objects ((minimum-size :int) (natural-size :int))
    (%gtk-cell-renderer-preferred-height cell widget minimum-size natural-size)
    (values (mem-ref minimum-size :int)
            (mem-ref natural-size :int))))

(export 'gtk-cell-renderer-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height_for_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_preferred_height_for_width"
          %gtk-cell-renderer-preferred-height-for-width) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (width :int)
  (minimum-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun gtk-cell-renderer-preferred-height-for-width (cell widget width)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell renderer will be
    rendering to}
  @argument[width]{an integer with the size which is available for allocation}
  @begin{return}
    @code{minimum-height} -- an integer with the minimum size @br{}
    @code{natural-height} -- an integer with  the preferred size
  @end{return}
  @begin{short}
    Retreives a cell renderers's minimum and natural height if it were rendered
    to @arg{widget} with the specified width.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%gtk-cell-renderer-preferred-height-for-width cell
                                                   widget
                                                   width
                                                   minimum-height
                                                   natural-height)
    (values (mem-ref minimum-height :int)
            (mem-ref natural-height :int))))

(export 'gtk-cell-renderer-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_preferred_size"
          %gtk-cell-renderer-preferred-size) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (minimum-size (g-boxed-foreign gtk-requisition))
  (natural-size (g-boxed-foreign gtk-requisition)))

(defun gtk-cell-renderer-preferred-size (cell widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell renderer will be
    rendering to}
  @begin{return}
    @code{minimum-size} -- a @symbol{gtk-requisition} instance with the minimum
    size @br{}
    @code{natural-size} -- a @symbol{gtk-requisition} instance with the natural
    size
  @end{return}
  @begin{short}
    Retrieves the minimum and natural size of a cell renderer taking into
    account the widget's preference for height-for-width management.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}"
  (let ((minimum-size (make-gtk-requisition))
        (natural-size (make-gtk-requisition)))
    (%gtk-cell-renderer-preferred-size cell widget minimum-size natural-size)
    (values minimum-size
            natural-size)))

(export 'gtk-cell-renderer-preferred-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_preferred_width"
          %gtk-cell-renderer-preferred-width) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (minimum-size (:pointer :int))
  (natural-size (:pointer :int)))

(defun gtk-cell-renderer-preferred-width (cell widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell renderer will be
    rendering to}
  @begin{return}
    @code{minimum-size} -- an integer with the minimum size @br{}
    @code{natural-size} -- an integer the natural size
  @end{return}
  @begin{short}
    Retreives a cell renderer's natural size when rendered to widget.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}"
  (with-foreign-objects ((minimum-size :int) (natural-size :int))
    (%gtk-cell-renderer-preferred-width cell widget minimum-size natural-size)
    (values (mem-ref minimum-size :int)
            (mem-ref natural-size :int))))

(export 'gtk-cell-renderer-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width_for_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_preferred_width_for_height"
          %gtk-cell-renderer-preferred-width-for-height) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (height :int)
  (minimum-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun gtk-cell-renderer-preferred-width-for-height (cell widget height)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the @class{gtk-widget} object this cell renderer will be
    rendering to}
  @argument[height]{an integer with the size which is available for allocation}
  @begin{return}
    @code{minimum-width} -- an integer with the minimum size @br{}
    @code{natural-width} -- an integer with the preferred size
  @end{return}
  @begin{short}
    Retreives a cell renderers's minimum and natural width if it were rendered
    to @arg{widget} with the specified height.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%gtk-cell-renderer-preferred-width-for-height cell
                                                   widget
                                                   height
                                                   minimum-width
                                                   natural-width)
    (values (mem-ref minimum-width :int)
            (mem-ref natural-width :int))))

(export 'gtk-cell-renderer-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_request_mode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_request_mode" gtk-cell-renderer-request-mode)
    gtk-size-request-mode
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @return{The @symbol{gtk-size-request-mode} mode preferred by this cell
    renderer.}
  @begin{short}
    Gets whether the cell renderer prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @see-class{gtk-cell-renderer}
  @see-symbol{gtk-size-request-mode}"
  (cell (g-object gtk-cell-renderer)))

(export 'gtk-cell-renderer-request-mode)

;;; --- End of file gtk.cell-renderer.lisp -------------------------------------

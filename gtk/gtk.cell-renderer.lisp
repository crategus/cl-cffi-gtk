;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     GtkCellRendererClass
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
;;;               gchar*  cell-background         Write
;;;            GdkColor*  cell-background-gdk     Read / Write
;;;             GdkRGBA*  cell-background-rgba    Read / Write
;;;            gboolean   cell-background-set     Read / Write
;;;            gboolean   editing                 Read
;;;                gint   height                  Read / Write
;;;            gboolean   is-expanded             Read / Write
;;;            gboolean   is-expander             Read / Write
;;; GtkCellRendererMode   mode                    Read / Write
;;;            gboolean   sensitive               Read / Write
;;;            gboolean   visible                 Read / Write
;;;                gint   width                   Read / Write
;;;              gfloat   xalign                  Read / Write
;;;               guint   xpad                    Read / Write
;;;              gfloat   yalign                  Read / Write
;;;               guint   ypad                    Read / Write
;;;
;;; Signals
;;;
;;;                void   editing-canceled        Run First
;;;                void   editing-started         Run First
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
  (:selected 1)
  (:prelit 2)
  (:insensitive 4)
  (:sorted 8)
  (:focused 16)
  (:expandable #.(ash 1 5))
  (:expanded #.(ash 1 6)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-state atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-cell-renderer-state atdoc:*external-symbols*)
 "@version{2013-11-26}
  @short{Tells how a cell is to be rendererd.}
  @begin{pre}
(define-g-flags \"GtkCellRendererState\" gtk-cell-renderer-state
  (:export t
   :type-initializer \"gtk_cell_renderer_state_get_type\")
  (:selected 1)
  (:prelit 2)
  (:insensitive 4)
  (:sorted 8)
  (:focused 16)
  (:expandable #.(ash 1 5))
  (:expanded #.(ash 1 6)))
  @end{pre}
  @begin[code]{table}
    @entry[:selected]{The cell is currently selected, and probably has a
      selection colored background to render to.}
    @entry[:prelit]{The mouse is hovering over the cell.}
    @entry[:insensitive]{The cell is drawn in an insensitive manner.}
    @entry[:sorted]{The cell is in a sorted row.}
    @entry[:focused]{The cell is in the focus row.}
    @entry[:expandable]{The cell is in a row that can be expanded. Since 3.4}
    @entry[:expanded]{The cell is in a row that is expanded. Since 3.4}
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
(setf (gethash 'gtk-cell-renderer-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-cell-renderer-mode atdoc:*external-symbols*)
 "@version{2013-6-22}
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
      Note that this doesn't mean that e. g. the row being drawn cannot be
      selected - just that a particular element of it cannot be individually
      modified.}
    @entry[:activatable]{The cell can be clicked.}
    @entry[:editable]{The cell can be edited or otherwise modified.}
  @end{table}")

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
 "@version{2013-6-21}
  @begin{short}
    The @sym{gtk-cell-renderer} is a base class of a set of objects used for
    rendering a cell to a @symbol{cairo-t}. These objects are used primarily by
    the @class{gtk-tree-view} widget, though they are not tied to them in any
    specific way. It is worth noting that @sym{gtk-cell-renderer} is not a
    @class{gtk-widget} and cannot be treated as such.
  @end{short}

  The primary use of a @sym{gtk-cell-renderer} is for drawing a certain
  graphical elements on a @symbol{cairo-t}. Typically, one cell renderer is used
  to draw many cells on the screen. To this extent, it is not expected that a
  CellRenderer keep any permanent state around. Instead, any state is set just
  prior to use using GObjects property system. Then, the cell is measured using
  the function @fun{gtk-cell-renderer-get-size}. Finally, the cell is rendered
  in the correct location using the function @fun{gtk-cell-renderer-render}.

  There are a number of rules that must be followed when writing a new
  @sym{gtk-cell-renderer}. First and formost, its important that a certain set
  of properties will always yield a cell renderer of the same size, barring a
  @class{gtk-style} change. The @sym{gtk-cell-renderer} also has a number of
  generic properties that are expected to be honored by all children.

  Beyond merely rendering a cell, cell renderers can optionally provide active
  user interface elements. A cell renderer can be activatable like
  @class{gtk-cell-renderer-toggle}, which toggles when it gets activated by a
  mouse click, or it can be editable like @class{gtk-cell-renderer-text}, which
  allows the user to edit the text using a @class{gtk-entry}. To make a cell
  renderer activatable or editable, you have to implement the
  @code{GtkCellRendererClass.activate} or
  @code{GtkCellRendererClass.start_editing} virtual functions, respectively.
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-canceled\" signal}
      @begin{pre}
 lambda (renderer)    : Run First
      @end{pre}
      This signal gets emitted when the user cancels the process of editing a
      cell. For example, an editable cell renderer could be written to cancel
      editing when the user presses Escape.
      See also the function @fun{gtk-cell-renderer-stop-editing}.
      @begin[code]{table}
        @entry[renderer]{The object which received the signal.}
      @end{table}
    @subheading{The \"editing-started\" signal}
      @begin{pre}
 lambda (renderer editable path)    : Run First
      @end{pre}
      This signal gets emitted when a cell starts to be edited. The intended use
      of this signal is to do special setup on editable, e. g. adding a
      @class{gtk-entry-completion} or setting up additional columns in a
      @class{gtk-combo-box}.
      Note that GTK+ does not guarantee that cell renderers will continue to use
      the same kind of widget for editing in future releases, therefore you
      should check the type of editable before doing any specific setup, as in
      the following example:
      @begin{pre}
 static void
 text_editing_started (GtkCellRenderer *cell,
                       GtkCellEditable *editable,
                       const gchar     *path,
                       gpointer         data)
 {
   if (GTK_IS_ENTRY (editable))
     {
       GtkEntry *entry = GTK_ENTRY (editable);

       /* ... create a GtkEntryCompletion */

       gtk_entry_set_completion (entry, completion);
     @}
 @}
      @end{pre}
      @begin[code]{table}
        @entry[renderer]{The object which received the signal.}
        @entry[editable]{The @class{gtk-cell-editable}.}
        @entry[path]{The path identifying the edited cell.}
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
  @see-slot{gtk-cell-renderer-ypad}")

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
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{cell-background} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-cell-background-gdk ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-gdk"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-gdk} property of type @class{gdk-color}
  (Read / Write) @br{}
  @em{Warning:}
  @code{cell-background-gdk} has been deprecated since version 3.4
  and should not be used in newly-written code.
  Use @code{cell-background-rgba} instead. @br{}
  Cell background as a @class{gdk-color}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-gdk
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-gdk 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{cell-background-gdk} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-cell-background-rgba ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-rgba"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-rgba} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  Cell background as a @class{gdk-rgba}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-rgba
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-rgba 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{cell-background-rgba} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-cell-background-set ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-background-set"
                                               'gtk-cell-renderer) 't)
 "The @code{cell-background-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the cell background color. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-cell-background-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-cell-background-set 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{cell-background-set} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-editing ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editing" 'gtk-cell-renderer) 't)
 "The @code{editing} property of type @code{:boolean} (Read) @br{}
  Whether the cell renderer is currently in editing mode. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-editing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-editing 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{editing} slot of the
  @class{gtk-cell-renderer} class.
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
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{height} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-is-expanded ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-expanded"
                                               'gtk-cell-renderer) 't)
 "The @code{is-expanded} property of type @code{:boolean}
  (Read / Write) @br{}
  Row is an expander row, and is expanded. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-is-expanded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-is-expanded 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{is-expanded} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-is-expander ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-expander"
                                               'gtk-cell-renderer) 't)
 "The @code{is-expander} property of type @code{:boolean}
  (Read / Write) @br{}
  Row has children. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-is-expander atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-is-expander 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{is-expander} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-mode -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-cell-renderer) 't)
 "The @code{mode} property of type @symbol{gtk-cell-renderer-mode}
  (Read / Write) @br{}
  Editable mode of the CellRenderer. @br{}
  Default value: @code{:inert}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-mode 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{mode} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

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
 "@version{2013-11-29}
  @syntax[]{(gtk-cell-renderer-sensitive object) => sensitive}
  @syntax[]{(setf (gtk-cell-renderer-sensitive object) sensitive)}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[sensitive]{the sensitivity of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{sensitive} slot of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The @sym{gtk-cell-renderer-sensitive} slot access function
  returns the cell renderer's sensitivity.

  The @sym{(setf gtk-cell-renderer-sensitive)} slot access function
  sets the cell renderer's sensitivity.
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
 "@version{2013-11-29}
  @syntax[]{(gtk-cell-renderer-visible object) => visible}
  @syntax[]{(setf (gtk-cell-renderer-visible object) visible)}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[visible]{the visibility of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{visible} of the
    @class{gtk-cell-renderer} class.
  @end{short}

  The @sym{gtk-cell-renderer-sensitive} slot access function
  returns the cell renderer's visibility.

  The @sym{(setf gtk-cell-renderer-sensitive)} slot access function
  sets the cell renderer's visibility.
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
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{width} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-xalign -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-cell-renderer) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The x-align. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-xalign 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{xalign} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-xpad -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xpad" 'gtk-cell-renderer) 't)
 "The @code{xpad} property of type @code{:uint} (Read / Write) @br{}
  The xpad. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-xpad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-xpad 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{xpad} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-yalign -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-cell-renderer) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The y-align. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-yalign 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{yalign} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-renderer-ypad -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ypad" 'gtk-cell-renderer) 't)
 "The @code{ypad} property of tpye @code{:uint} (Read / Write) @br{}
  The ypad. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-ypad atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-ypad 'function)
 "@version{2013-11-29}
  Accessor of the @slot[gtk-cell-renderer]{ypad} slot of the
  @class{gtk-cell-renderer} class.
  @see-class{gtk-cell-renderer}")

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
;;;
;;; void gtk_cell_renderer_get_aligned_area (GtkCellRenderer *cell,
;;;                                          GtkWidget *widget,
;;;                                          GtkCellRendererState flags,
;;;                                          const GdkRectangle *cell_area,
;;;                                          GdkRectangle *aligned_area);
;;;
;;; Gets the aligned area used by cell inside cell_area. Used for finding the
;;; appropriate edit and focus rectangle.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
;;;
;;; flags :
;;;     render flags
;;;
;;; cell_area :
;;;     cell area which would be passed to gtk_cell_renderer_render()
;;;
;;; aligned_area :
;;;     the return location for the space inside cell_area that would acually be
;;;     used to render
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_size" %gtk-cell-renderer-get-size) :void
  (cell (g-object gtk-cell-renderer))
  (widget (g-object gtk-widget))
  (cell-area (g-boxed-foreign gdk-rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-cell-renderer-get-size (cell widget cell-area)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-29}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[widget]{the widget the renderer is rendering to}
  @argument[cell-area]{the area a cell will be allocated, or @code{nil}}
  @begin{return}
    @code{x-offset} -- x offset of cell relative to @arg{cell-area},
                       or @code{nil} @br{}
    @code{y-offset} -- y offset of cell relative to @arg{cell-area},
                       or @code{nil} @br{}
    @code{width} -- width needed to render a cell, or @code{nil} @br{}
    @code{height} --  height needed to render a cell, or @code{nil}
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-cell-renderer-get-size} has been deprecated since
    version 3.0 and should not be used in newly-written code.
    Use the function @fun{gtk-cell-renderer-get-preferred-size} instead.

  @begin{short}
    Obtains the width and height needed to render the cell. Used by view widgets
    to determine the appropriate size for the @arg{cell-area} passed to the
    function @fun{gtk-cell-renderer-render}.
  @end{short}
  If @arg{cell-area} is not @code{nil}, fills in the x and y offsets (if set)
  of the cell relative to this location.

  Please note that the values set in @arg{width} and @arg{height}, as well as
  those in @arg{x-offset} and @arg{y-offset} are inclusive of the
  @code{\"xpad\"} and @code{\"ypad\"} properties.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-function{gtk-cell-renderer-render}
  @see-function{gtk-cell-renderer-get-preferred-size}"
  (with-foreign-objects ((x-offset :int)
                         (y-offset :int)
                         (width :int)
                         (height :int))
    (%gtk-cell-renderer-get-size cell
                                 widget
                                 cell-area
                                 x-offset
                                 y-offset
                                 width
                                 height)
    (values (mem-ref x-offset :int)
            (mem-ref y-offset :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-cell-renderer-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_render ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_render" gtk-cell-renderer-render) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[cr]{a cairo context to draw to}
  @argument[widget]{the widget owning window}
  @argument[background-area]{entire cell area including tree expanders and
    maybe padding on the sides}
  @argument[cell-area]{area normally rendered by a cell renderer}
  @argument[flags]{flags that affect rendering}
  @begin{short}
    Invokes the virtual render function of the @class{gtk-cell-renderer}.
  @end{short}
  The three passed-in rectangles are areas in @arg{cr}. Most renderers will
  draw within @arg{cell-area}; the @code{xalign}, @code{yalign}, @code{xpad},
  and @code{ypad} fields of the @class{gtk-cell-renderer} should be honored with
  respect to @arg{cell-area}. @arg{background-area} includes the blank space
  around the cell, and also the area containing the tree expander; so the
  @arg{background-area} rectangles for all cells tile to cover the entire
  window."
  (cell (g-object gtk-cell-renderer))
  (cr (:pointer (:struct cairo-t)))
  (widget (g-object gtk-widget))
  (background-area (g-boxed-foreign gdk-rectangle))
  (cell-area (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-renderer-render)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_activate ()
;;;
;;; gboolean gtk_cell_renderer_activate (GtkCellRenderer *cell,
;;;                                      GdkEvent *event,
;;;                                      GtkWidget *widget,
;;;                                      const gchar *path,
;;;                                      const GdkRectangle *background_area,
;;;                                      const GdkRectangle *cell_area,
;;;                                      GtkCellRendererState flags);
;;;
;;; Passes an activate event to the cell renderer for possible processing. Some
;;; cell renderers may use events; for example, GtkCellRendererToggle toggles
;;; when it gets a mouse click.
;;;
;;; cell :
;;;     a GtkCellRenderer
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; widget :
;;;     widget that received the event
;;;
;;; path :
;;;     widget-dependent string representation of the event location; e.g. for
;;;     GtkTreeView, a string representation of GtkTreePath
;;;
;;; background_area :
;;;     background area as passed to gtk_cell_renderer_render()
;;;
;;; cell_area :
;;;     cell area as passed to gtk_cell_renderer_render()
;;;
;;; flags :
;;;     render flags
;;;
;;; Returns :
;;;     TRUE if the event was consumed/handled
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_start_editing ()
;;;
;;; GtkCellEditable * gtk_cell_renderer_start_editing
;;;                                        (GtkCellRenderer *cell,
;;;                                         GdkEvent *event,
;;;                                         GtkWidget *widget,
;;;                                         const gchar *path,
;;;                                         const GdkRectangle *background_area,
;;;                                         const GdkRectangle *cell_area,
;;;                                         GtkCellRendererState flags);
;;;
;;; Passes an activate event to the cell renderer for possible processing.
;;;
;;; cell :
;;;     a GtkCellRenderer
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; widget :
;;;     widget that received the event
;;;
;;; path :
;;;     widget-dependent string representation of the event location; e.g. for
;;;     GtkTreeView, a string representation of GtkTreePath
;;;
;;; background_area :
;;;     background area as passed to gtk_cell_renderer_render()
;;;
;;; cell_area :
;;;     cell area as passed to gtk_cell_renderer_render()
;;;
;;; flags :
;;;     render flags
;;;
;;; Returns :
;;;     A new GtkCellEditable, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_stop_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_stop_editing" gtk-cell-renderer-stop-editing) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-29}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[canceled]{@em{true} if the editing has been canceled}
  @begin{short}
    Informs the cell renderer that the editing is stopped.
  @end{short}
  If @arg{canceled} is @em{true}, the cell renderer will emit the
  \"editing-canceled\" signal.

  This function should be called by cell renderer implementations in response
  to the \"editing-done\" signal of @class{gtk-cell-editable}.
  @see-class{gtk-cell-renderer}
  @see-class{gtk-cell-editable}
  @see-function{gtk-cell-renderer-start-editing}"
  (cell (g-object gtk-cell-renderer))
  (canceled :boolean))

(export 'gtk-cell-renderer-stop-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_fixed_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_fixed_size" %gtk-cell-renderer-get-fixed-size)
    :void
  (cell (g-object gtk-cell-renderer))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-cell-renderer-get-fixed-size (cell)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @begin{return}
    @code{width} -- the fixed width of the cell, or @code{nil} @br{}
    @code{height} -- the fixed height of the cell, or @code{nil}
  @end{return}
  Returns @arg{width} and @arg{height} with the appropriate size of cell."
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-cell-renderer-get-fixed-size cell width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-cell-renderer-get-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_fixed_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_set_fixed_size" gtk-cell-renderer-set-fixed-size)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[width]{the width of the cell renderer, or -1}
  @argument[height]{the height of the cell renderer, or -1}
  Sets the renderer size to be explicit, independent of the properties set."
  (cell (g-object gtk-cell-renderer))
  (width :int)
  (height :int))

(export 'gtk-cell-renderer-set-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_alignment ()
;;;
;;; void gtk_cell_renderer_get_alignment (GtkCellRenderer *cell,
;;;                                       gfloat *xalign,
;;;                                       gfloat *yalign);
;;;
;;; Fills in xalign and yalign with the appropriate values of cell.
;;;
;;; cell :
;;;     A GtkCellRenderer
;;;
;;; xalign :
;;;     location to fill in with the x alignment of the cell, or NULL
;;;
;;; yalign :
;;;     location to fill in with the y alignment of the cell, or NULL
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_alignment ()
;;;
;;; void gtk_cell_renderer_set_alignment (GtkCellRenderer *cell,
;;;                                       gfloat xalign,
;;;                                       gfloat yalign);
;;;
;;; Sets the renderer's alignment within its available space.
;;;
;;; cell :
;;;     A GtkCellRenderer
;;;
;;; xalign :
;;;     the x alignment of the cell renderer
;;;
;;; yalign :
;;;     the y alignment of the cell renderer
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_padding ()
;;;
;;; void gtk_cell_renderer_get_padding (GtkCellRenderer *cell,
;;;                                     gint *xpad,
;;;                                     gint *ypad);
;;;
;;; Fills in xpad and ypad with the appropriate values of cell.
;;;
;;; cell :
;;;     A GtkCellRenderer
;;;
;;; xpad :
;;;     location to fill in with the x padding of the cell, or NULL
;;;
;;; ypad :
;;;     location to fill in with the y padding of the cell, or NULL
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_padding ()
;;;
;;; void gtk_cell_renderer_set_padding (GtkCellRenderer *cell,
;;;                                     gint xpad,
;;;                                     gint ypad);
;;;
;;; Sets the renderer's padding.
;;;
;;; cell :
;;;     A GtkCellRenderer
;;;
;;; xpad :
;;;     the x padding of the cell renderer
;;;
;;; ypad :
;;;     the y padding of the cell renderer
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_state ()
;;;
;;; GtkStateFlags gtk_cell_renderer_get_state (GtkCellRenderer *cell,
;;;                                            GtkWidget *widget,
;;;                                            GtkCellRendererState cell_state);
;;;
;;; Translates the cell renderer state to GtkStateFlags, based on the cell
;;; renderer and widget sensitivity, and the given GtkCellRendererState.
;;;
;;; cell :
;;;     a GtkCellRenderer, or NULL
;;;
;;; widget :
;;;     a GtkWidget, or NULL
;;;
;;; cell_state :
;;;     cell renderer state
;;;
;;; Returns :
;;;     the widget state flags applying to cell
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_is_activatable ()
;;;
;;; gboolean gtk_cell_renderer_is_activatable (GtkCellRenderer *cell);
;;;
;;; Checks whether the cell renderer can do something when activated.
;;;
;;; cell :
;;;     A GtkCellRenderer
;;;
;;; Returns :
;;;     TRUE if the cell renderer can do anything when activated
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height ()
;;;
;;; void gtk_cell_renderer_get_preferred_height (GtkCellRenderer *cell,
;;;                                              GtkWidget *widget,
;;;                                              gint *minimum_size,
;;;                                              gint *natural_size);
;;;
;;; Retreives a renderer's natural size when rendered to widget.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
;;;
;;; minimum_size :
;;;     location to store the minimum size, or NULL
;;;
;;; natural_size :
;;;     location to store the natural size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height_for_width ()
;;;
;;; void gtk_cell_renderer_get_preferred_height_for_width
;;;                                                      (GtkCellRenderer *cell,
;;;                                                       GtkWidget *widget,
;;;                                                       gint width,
;;;                                                       gint *minimum_height,
;;;                                                       gint *natural_height);
;;;
;;; Retreives a cell renderers's minimum and natural height if it were rendered
;;; to widget with the specified width.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
;;;
;;; width :
;;;     the size which is available for allocation
;;;
;;; minimum_height :
;;;     location for storing the minimum size, or NULL
;;;
;;; natural_height :
;;;     location for storing the preferred size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_size ()
;;;
;;; void gtk_cell_renderer_get_preferred_size (GtkCellRenderer *cell,
;;;                                            GtkWidget *widget,
;;;                                            GtkRequisition *minimum_size,
;;;                                            GtkRequisition *natural_size);
;;;
;;; Retrieves the minimum and natural size of a cell taking into account the
;;; widget's preference for height-for-width management.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
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
;;; gtk_cell_renderer_get_preferred_width ()
;;;
;;; void gtk_cell_renderer_get_preferred_width (GtkCellRenderer *cell,
;;;                                             GtkWidget *widget,
;;;                                             gint *minimum_size,
;;;                                             gint *natural_size);
;;;
;;; Retreives a renderer's natural size when rendered to widget.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
;;;
;;; minimum_size :
;;;     location to store the minimum size, or NULL
;;;
;;; natural_size :
;;;     location to store the natural size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width_for_height ()
;;;
;;; void gtk_cell_renderer_get_preferred_width_for_height
;;;                                                      (GtkCellRenderer *cell,
;;;                                                       GtkWidget *widget,
;;;                                                       gint height,
;;;                                                       gint *minimum_width,
;;;                                                       gint *natural_width);
;;;
;;; Retreives a cell renderers's minimum and natural width if it were rendered
;;; to widget with the specified height.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; widget :
;;;     the GtkWidget this cell will be rendering to
;;;
;;; height :
;;;     the size which is available for allocation
;;;
;;; minimum_width :
;;;     location for storing the minimum size, or NULL
;;;
;;; natural_width :
;;;     location for storing the preferred size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_request_mode ()
;;;
;;; GtkSizeRequestMode gtk_cell_renderer_get_request_mode
;;;                                                     (GtkCellRenderer *cell);
;;;
;;; Gets whether the cell renderer prefers a height-for-width layout or a
;;; width-for-height layout.
;;;
;;; cell :
;;;     a GtkCellRenderer instance
;;;
;;; Returns :
;;;     The GtkSizeRequestMode preferred by this renderer.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.cell-renderer.lisp -------------------------------------

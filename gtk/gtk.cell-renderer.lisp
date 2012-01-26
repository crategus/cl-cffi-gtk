;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; An object for rendering a single cell
;;; 	
;;; Synopsis
;;; 
;;;     GtkCellRendererState
;;;     GtkCellRendererMode
;;;     GtkCellRenderer
;;;     GtkCellRendererClass
;;;
;;;     gtk_cell_renderer_get_aligned_area
;;;     gtk_cell_renderer_get_size
;;;     gtk_cell_renderer_render
;;;     gtk_cell_renderer_activate
;;;     gtk_cell_renderer_start_editing
;;;     gtk_cell_renderer_stop_editing
;;;     gtk_cell_renderer_get_fixed_size
;;;     gtk_cell_renderer_set_fixed_size
;;;     gtk_cell_renderer_get_visible
;;;     gtk_cell_renderer_set_visible
;;;     gtk_cell_renderer_get_sensitive
;;;     gtk_cell_renderer_set_sensitive
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererText
;;;                +----GtkCellRendererPixbuf
;;;                +----GtkCellRendererProgress
;;;                +----GtkCellRendererSpinner
;;;                +----GtkCellRendererToggle
;;; 
;;; Properties
;;; 
;;;   "cell-background"          gchar*                : Write
;;;   "cell-background-gdk"      GdkColor*             : Read / Write
;;;   "cell-background-rgba"     GdkRGBA*              : Read / Write
;;;   "cell-background-set"      gboolean              : Read / Write
;;;   "editing"                  gboolean              : Read
;;;   "height"                   gint                  : Read / Write
;;;   "is-expanded"              gboolean              : Read / Write
;;;   "is-expander"              gboolean              : Read / Write
;;;   "mode"                     GtkCellRendererMode   : Read / Write
;;;   "sensitive"                gboolean              : Read / Write
;;;   "visible"                  gboolean              : Read / Write
;;;   "width"                    gint                  : Read / Write
;;;   "xalign"                   gfloat                : Read / Write
;;;   "xpad"                     guint                 : Read / Write
;;;   "yalign"                   gfloat                : Read / Write
;;;   "ypad"                     guint                 : Read / Write
;;; 
;;; Signals
;;; 
;;;   "editing-canceled"                               : Run First
;;;   "editing-started"                                : Run First
;;; 
;;; Description
;;; 
;;; The GtkCellRenderer is a base class of a set of objects used for rendering
;;; a cell to a cairo_t. These objects are used primarily by the GtkTreeView
;;; widget, though they aren't tied to them in any specific way. It is worth
;;; noting that GtkCellRenderer is not a GtkWidget and cannot be treated as
;;; such.
;;; 
;;; The primary use of a GtkCellRenderer is for drawing a certain graphical
;;; elements on a cairo_t. Typically, one cell renderer is used to draw many
;;; cells on the screen. To this extent, it isn't expected that a CellRenderer
;;; keep any permanent state around. Instead, any state is set just prior to use
;;; using GObjects property system. Then, the cell is measured using
;;; gtk_cell_renderer_get_size(). Finally, the cell is rendered in the correct
;;; location using gtk_cell_renderer_render().
;;; 
;;; There are a number of rules that must be followed when writing a new
;;; GtkCellRenderer. First and formost, its important that a certain set of
;;; properties will always yield a cell renderer of the same size, barring a
;;; GtkStyle change. The GtkCellRenderer also has a number of generic properties
;;; that are expected to be honored by all children.
;;; 
;;; Beyond merely rendering a cell, cell renderers can optionally provide active
;;; user interface elements. A cell renderer can be activatable like
;;; GtkCellRendererToggle, which toggles when it gets activated by a mouse
;;; click, or it can be editable like GtkCellRendererText, which allows the user
;;; to edit the text using a GtkEntry. To make a cell renderer activatable or
;;; editable, you have to implement the GtkCellRendererClass.activate or
;;; GtkCellRendererClass.start_editing virtual functions, respectively.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-background" property
;;; 
;;;   "cell-background"          gchar*                : Write
;;; 
;;; Cell background color as a string.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-background-gdk" property
;;; 
;;;   "cell-background-gdk"      GdkColor*             : Read / Write
;;; 
;;; Cell background color as a GdkColor.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-background-rgba" property
;;; 
;;;   "cell-background-rgba"     GdkRGBA*              : Read / Write
;;; 
;;; Cell background as a GdkRGBA
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-background-set" property
;;; 
;;;   "cell-background-set"      gboolean              : Read / Write
;;; 
;;; Whether this tag affects the cell background color.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "editing" property
;;; 
;;;   "editing"                  gboolean              : Read
;;; 
;;; Whether the cell renderer is currently in editing mode.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "height" property
;;; 
;;;   "height"                   gint                  : Read / Write
;;; 
;;; The fixed height.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-expanded" property
;;; 
;;;   "is-expanded"              gboolean              : Read / Write
;;; 
;;; Row is an expander row, and is expanded.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-expander" property
;;; 
;;;   "is-expander"              gboolean              : Read / Write
;;; 
;;; Row has children.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "mode" property
;;; 
;;;   "mode"                     GtkCellRendererMode   : Read / Write
;;; 
;;; Editable mode of the CellRenderer.
;;; 
;;; Default value: GTK_CELL_RENDERER_MODE_INERT
;;;
;;; ----------------------------------------------------------------------------
;;; The "sensitive" property
;;; 
;;;   "sensitive"                gboolean              : Read / Write
;;; 
;;; Display the cell sensitive.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;; 
;;;   "visible"                  gboolean              : Read / Write
;;; 
;;; Display the cell.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "width" property
;;; 
;;;   "width"                    gint                  : Read / Write
;;; 
;;; The fixed width.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;; 
;;; The x-align.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "xpad" property
;;; 
;;;   "xpad"                     guint                 : Read / Write
;;; 
;;; The xpad.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "yalign" property
;;; 
;;;   "yalign"                   gfloat                : Read / Write
;;; 
;;; The y-align.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;;
;;; ----------------------------------------------------------------------------
;;; The "ypad" property
;;; 
;;;   "ypad"                     guint                 : Read / Write
;;; 
;;; The ypad.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "editing-canceled" signal
;;; 
;;; void user_function (GtkCellRenderer *renderer,
;;;                     gpointer         user_data)      : Run First
;;; 
;;; This signal gets emitted when the user cancels the process of editing a
;;; cell. For example, an editable cell renderer could be written to cancel
;;; editing when the user presses Escape.
;;; 
;;; See also: gtk_cell_renderer_stop_editing().
;;; 
;;; renderer :
;;; 	the object which received the signal
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "editing-started" signal
;;; 
;;; void user_function (GtkCellRenderer *renderer,
;;;                     GtkCellEditable *editable,
;;;                     gchar           *path,
;;;                     gpointer         user_data)      : Run First
;;; 
;;; This signal gets emitted when a cell starts to be edited. The intended use
;;; of this signal is to do special setup on editable, e.g. adding a
;;; GtkEntryCompletion or setting up additional columns in a GtkComboBox.
;;; 
;;; Note that GTK+ doesn't guarantee that cell renderers will continue to use
;;; the same kind of widget for editing in future releases, therefore you
;;; should check the type of editable before doing any specific setup, as in
;;; the following example:
;;; 
;;; static void
;;; text_editing_started (GtkCellRenderer *cell,
;;;                       GtkCellEditable *editable,
;;;                       const gchar     *path,
;;;                       gpointer         data)
;;; {
;;;   if (GTK_IS_ENTRY (editable)) 
;;;     {
;;;       GtkEntry *entry = GTK_ENTRY (editable);
;;;       
;;;       /* ... create a GtkEntryCompletion */
;;;       
;;;       gtk_entry_set_completion (entry, completion);
;;;     }
;;; }
;;; 
;;; renderer :
;;; 	the object which received the signal
;;; 
;;; editable :
;;; 	the GtkCellEditable
;;; 
;;; path :
;;; 	the path identifying the edited cell
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererState
;;; 
;;; typedef enum {
;;;   GTK_CELL_RENDERER_SELECTED    = 1 << 0,
;;;   GTK_CELL_RENDERER_PRELIT      = 1 << 1,
;;;   GTK_CELL_RENDERER_INSENSITIVE = 1 << 2,
;;;   /* this flag means the cell is in the sort column/row */
;;;   GTK_CELL_RENDERER_SORTED      = 1 << 3,
;;;   GTK_CELL_RENDERER_FOCUSED     = 1 << 4
;;; } GtkCellRendererState;
;;; 
;;; Tells how a cell is to be rendererd.
;;; 
;;; GTK_CELL_RENDERER_SELECTED
;;; 	The cell is currently selected, and probably has a selection colored
;;;     background to render to.
;;; 
;;; GTK_CELL_RENDERER_PRELIT
;;; 	The mouse is hovering over the cell.
;;; 
;;; GTK_CELL_RENDERER_INSENSITIVE
;;; 	The cell is drawn in an insensitive manner
;;; 
;;; GTK_CELL_RENDERER_SORTED
;;; 	The cell is in a sorted row
;;; 
;;; GTK_CELL_RENDERER_FOCUSED
;;; 	The cell is in the focus row.
;;; enum GtkCellRendererMode
;;; 
;;; typedef enum {
;;;   GTK_CELL_RENDERER_MODE_INERT,
;;;   GTK_CELL_RENDERER_MODE_ACTIVATABLE,
;;;   GTK_CELL_RENDERER_MODE_EDITABLE
;;; } GtkCellRendererMode;
;;; 
;;; Identifies how the user can interact with a particular cell.
;;; 
;;; GTK_CELL_RENDERER_MODE_INERT
;;; 	The cell is just for display and cannot be interacted with. Note that
;;;     this doesn't mean that eg. the row being drawn can't be selected -- just
;;;     that a particular element of it cannot be individually modified.
;;; 
;;; GTK_CELL_RENDERER_MODE_ACTIVATABLE
;;; 	The cell can be clicked.
;;; 
;;; GTK_CELL_RENDERER_MODE_EDITABLE
;;; 	The cell can be edited or otherwise modified.
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkCellRendererState" gtk-cell-renderer-state
  (:export t
   :type-initializer "gtk_cell_renderer_state_get_type")
  (:selected 1)
  (:prelit 2)
  (:insensitive 4)
  (:sorted 8)
  (:focused 16))

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRenderer
;;; 
;;; struct GtkCellRenderer;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRenderer" gtk-cell-renderer
  (:superclass gtk-object
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_get_type")
  ((cell-background cell-renderer-cell-background
    "cell-background" "gchararray" nil t)
   (cell-background-gdk cell-renderer-cell-background-gdk
    "cell-background-gdk" "GdkColor" t t)
   (cell-background-set cell-renderer-cell-background-set
    "cell-background-set" "gboolean" t t)
   (editing cell-renderer-editing
    "editing" "gboolean" t nil)
   (height cell-renderer-height
    "height" "gint" t t)
   (is-expanded cell-renderer-is-expanded
    "is-expanded" "gboolean" t t)
   (is-expander cell-renderer-is-expander
    "is-expander" "gboolean" t t)
   (mode cell-renderer-mode
    "mode" "GtkCellRendererMode" t t)
   (sensitive cell-renderer-sensitive
    "sensitive" "gboolean" t t)
   (visible cell-renderer-visible
    "visible" "gboolean" t t)
   (width cell-renderer-width
    "width" "gint" t t)
   (xalign cell-renderer-xalign
    "xalign" "gfloat" t t)
   (xpad cell-renderer-xpad
    "xpad" "guint" t t)
   (yalign cell-renderer-yalign
    "yalign" "gfloat" t t)
   (ypad cell-renderer-ypad
    "ypad" "guint" t t)))

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererClass
;;; 
;;; struct GtkCellRendererClass {
;;;   GInitiallyUnownedClass parent_class;
;;; 
;;;   /* vtable - not signals */
;;;   GtkSizeRequestMode (* get_request_mode) (GtkCellRenderer *cell);
;;;   void (* get_preferred_width)            (GtkCellRenderer *cell,
;;;                                            GtkWidget       *widget,
;;;                                            gint            *minimum_size,
;;;                                            gint            *natural_size);
;;;   void (* get_preferred_height_for_width) (GtkCellRenderer *cell,
;;;                                            GtkWidget       *widget,
;;;                                            gint             width,
;;;                                            gint            *minimum_height,
;;;                                            gint            *natural_height);
;;;   void (* get_preferred_height)           (GtkCellRenderer *cell,
;;;                                            GtkWidget       *widget,
;;;                                            gint            *minimum_size,
;;;                                            gint            *natural_size);
;;;   void (* get_preferred_width_for_height)
;;;                                      (GtkCellRenderer      *cell,
;;;                                       GtkWidget            *widget,
;;;                                       gint                  height,
;;;                                       gint                 *minimum_width,
;;;                                       gint                 *natural_width);
;;;   void (* get_aligned_area)          (GtkCellRenderer      *cell,
;;;                                       GtkWidget            *widget,
;;;                                       GtkCellRendererState  flags,
;;;                                       const GdkRectangle   *cell_area,
;;;                                       GdkRectangle         *aligned_area);
;;;   void (* get_size)                  (GtkCellRenderer      *cell,
;;;                                       GtkWidget            *widget,
;;;                                       const GdkRectangle   *cell_area,
;;;                                       gint                 *x_offset,
;;;                                       gint                 *y_offset,
;;;                                       gint                 *width,
;;;                                       gint                 *height);
;;;   void (* render)                    (GtkCellRenderer      *cell,
;;;                                       cairo_t              *cr,
;;;                                       GtkWidget            *widget,
;;;                                       const GdkRectangle   *background_area,
;;;                                       const GdkRectangle   *cell_area,
;;;                                       GtkCellRendererState  flags);
;;;   gboolean (* activate)              (GtkCellRenderer      *cell,
;;;                                       GdkEvent             *event,
;;;                                       GtkWidget            *widget,
;;;                                       const gchar          *path,
;;;                                       const GdkRectangle   *background_area,
;;;                                       const GdkRectangle   *cell_area,
;;;                                       GtkCellRendererState  flags);
;;;   GtkCellEditable* (* start_editing) (GtkCellRenderer      *cell,
;;;                                       GdkEvent             *event,
;;;                                       GtkWidget            *widget,
;;;                                       const gchar          *path,
;;;                                       const GdkRectangle   *background_area,
;;;                                       const GdkRectangle   *cell_area,
;;;                                       GtkCellRendererState  flags);
;;; 
;;;   /* Signals */
;;;   void (* editing_canceled) (GtkCellRenderer *cell);
;;;   void (* editing_started)  (GtkCellRenderer *cell,
;;; 			         GtkCellEditable *editable,
;;; 			         const gchar     *path);
;;; 
;;;   /* Padding for future expansion */
;;;   void (*_gtk_reserved1) (void);
;;;   void (*_gtk_reserved2) (void);
;;;   void (*_gtk_reserved3) (void);
;;;   void (*_gtk_reserved4) (void);
;;; };
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
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; flags :
;;; 	render flags
;;; 
;;; cell_area :
;;; 	cell area which would be passed to gtk_cell_renderer_render()
;;; 
;;; aligned_area :
;;; 	the return location for the space inside cell_area that would acually
;;;     be used to render.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_size ()
;;; 
;;; void gtk_cell_renderer_get_size (GtkCellRenderer *cell,
;;;                                  GtkWidget *widget,
;;;                                  const GdkRectangle *cell_area,
;;;                                  gint *x_offset,
;;;                                  gint *y_offset,
;;;                                  gint *width,
;;;                                  gint *height);
;;; 
;;; Warning
;;; 
;;; gtk_cell_renderer_get_size has been deprecated since version 3.0 and should
;;; not be used in newly-written code.
;;; Use gtk_cell_renderer_get_preferred_size() instead.
;;; 
;;; Obtains the width and height needed to render the cell. Used by view widgets
;;; to determine the appropriate size for the cell_area passed to
;;; gtk_cell_renderer_render(). If cell_area is not NULL, fills in the x and y
;;; offsets (if set) of the cell relative to this location.
;;; 
;;; Please note that the values set in width and height, as well as those in
;;; x_offset and y_offset are inclusive of the xpad and ypad properties.
;;; 
;;; cell :
;;; 	a GtkCellRenderer
;;; 
;;; widget :
;;; 	the widget the renderer is rendering to
;;; 
;;; cell_area :
;;; 	The area a cell will be allocated, or NULL.
;;; 
;;; x_offset :
;;; 	location to return x offset of cell relative to cell_area, or NULL.
;;; 
;;; y_offset :
;;; 	location to return y offset of cell relative to cell_area, or NULL.
;;; 
;;; width :
;;; 	location to return width needed to render a cell, or NULL.
;;; 
;;; height :
;;; 	location to return height needed to render a cell, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_render ()
;;; 
;;; void gtk_cell_renderer_render (GtkCellRenderer *cell,
;;;                                cairo_t *cr,
;;;                                GtkWidget *widget,
;;;                                const GdkRectangle *background_area,
;;;                                const GdkRectangle *cell_area,
;;;                                GtkCellRendererState flags);
;;; 
;;; Invokes the virtual render function of the GtkCellRenderer. The three
;;; passed-in rectangles are areas in cr. Most renderers will draw within
;;; cell_area; the xalign, yalign, xpad, and ypad fields of the GtkCellRenderer
;;; should be honored with respect to cell_area. background_area includes the
;;; blank space around the cell, and also the area containing the tree expander;
;;; so the background_area rectangles for all cells tile to cover the entire
;;; window.
;;; 
;;; cell :
;;; 	a GtkCellRenderer
;;; 
;;; cr :
;;; 	a cairo context to draw to
;;; 
;;; widget :
;;; 	the widget owning window
;;; 
;;; background_area :
;;; 	entire cell area (including tree expanders and maybe padding on the
;;;     sides)
;;; 
;;; cell_area :
;;; 	area normally rendered by a cell renderer
;;; 
;;; flags :
;;; 	flags that affect rendering
;;; ----------------------------------------------------------------------------

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
;;; 	a GtkCellRenderer
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; widget :
;;; 	widget that received the event
;;; 
;;; path :
;;; 	widget-dependent string representation of the event location; e.g. for
;;;     GtkTreeView, a string representation of GtkTreePath
;;; 
;;; background_area :
;;; 	background area as passed to gtk_cell_renderer_render()
;;; 
;;; cell_area :
;;; 	cell area as passed to gtk_cell_renderer_render()
;;; 
;;; flags :
;;; 	render flags
;;; 
;;; Returns :
;;; 	TRUE if the event was consumed/handled
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
;;; 	a GtkCellRenderer
;;; 
;;; event :
;;; 	a GdkEvent
;;; 
;;; widget :
;;; 	widget that received the event
;;; 
;;; path :
;;; 	widget-dependent string representation of the event location; e.g. for
;;;     GtkTreeView, a string representation of GtkTreePath
;;; 
;;; background_area :
;;; 	background area as passed to gtk_cell_renderer_render()
;;; 
;;; cell_area :
;;; 	cell area as passed to gtk_cell_renderer_render()
;;; 
;;; flags :
;;; 	render flags
;;; 
;;; Returns :
;;; 	A new GtkCellEditable, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_stop_editing ()
;;; 
;;; void gtk_cell_renderer_stop_editing (GtkCellRenderer *cell,
;;;                                      gboolean canceled);
;;; 
;;; Informs the cell renderer that the editing is stopped. If canceled is TRUE,
;;; the cell renderer will emit the "editing-canceled" signal.
;;; 
;;; This function should be called by cell renderer implementations in response
;;; to the "editing-done" signal of GtkCellEditable.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; canceled :
;;; 	TRUE if the editing has been canceled
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_fixed_size ()
;;; 
;;; void gtk_cell_renderer_get_fixed_size (GtkCellRenderer *cell,
;;;                                        gint *width,
;;;                                        gint *height);
;;; 
;;; Fills in width and height with the appropriate size of cell.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; width :
;;; 	location to fill in with the fixed width of the cell, or NULL.
;;; 
;;; height :
;;; 	location to fill in with the fixed height of the cell, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_get_fixed_size" %gtk-cell-renderer-get-fixed-size)
    :void
  (cell (g-object gtk-cell-renderer))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-cell-renderer-get-fixed-size (cell)
  (with-foreign-objects ((width :int) (height :int))
    (%gtk-cell-renderer-get-fixed-size cell width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-cell-renderer-get-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_fixed_size ()
;;; 
;;; void gtk_cell_renderer_set_fixed_size (GtkCellRenderer *cell,
;;;                                        gint width,
;;;                                        gint height);
;;; 
;;; Sets the renderer size to be explicit, independent of the properties set.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; width :
;;; 	the width of the cell renderer, or -1
;;; 
;;; height :
;;; 	the height of the cell renderer, or -1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_set_fixed_size" gtk-cell-renderer-set-fixed-size)
    :void
  (cell (g-object gtk-cell-renderer))
  (width :int)
  (height :int))

(export 'gtk-cell-renderer-set-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_visible ()
;;; 
;;; gboolean gtk_cell_renderer_get_visible (GtkCellRenderer *cell);
;;; 
;;; Returns the cell renderer's visibility.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; Returns :
;;; 	TRUE if the cell renderer is visible
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_visible ()
;;; 
;;; void gtk_cell_renderer_set_visible (GtkCellRenderer *cell, gboolean visible)
;;; 
;;; Sets the cell renderer's visibility.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; visible :
;;; 	the visibility of the cell
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_sensitive ()
;;; 
;;; gboolean gtk_cell_renderer_get_sensitive (GtkCellRenderer *cell)
;;; 
;;; Returns the cell renderer's sensitivity.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; Returns :
;;; 	TRUE if the cell renderer is sensitive
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_set_sensitive ()
;;; 
;;; void gtk_cell_renderer_set_sensitive (GtkCellRenderer *cell,
;;;                                       gboolean sensitive);
;;; 
;;; Sets the cell renderer's sensitivity.
;;; 
;;; cell :
;;; 	A GtkCellRenderer
;;; 
;;; sensitive :
;;; 	the sensitivity of the cell
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

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
;;; 	A GtkCellRenderer
;;; 
;;; xalign :
;;; 	location to fill in with the x alignment of the cell, or NULL.
;;; 
;;; yalign :
;;; 	location to fill in with the y alignment of the cell, or NULL.
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
;;; 	A GtkCellRenderer
;;; 
;;; xalign :
;;; 	the x alignment of the cell renderer
;;; 
;;; yalign :
;;; 	the y alignment of the cell renderer
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
;;; 	A GtkCellRenderer
;;; 
;;; xpad :
;;; 	location to fill in with the x padding of the cell, or NULL.
;;; 
;;; ypad :
;;; 	location to fill in with the y padding of the cell, or NULL.
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
;;; 	A GtkCellRenderer
;;; 
;;; xpad :
;;; 	the x padding of the cell renderer
;;; 
;;; ypad :
;;; 	the y padding of the cell renderer
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_state ()
;;; 
;;; GtkStateFlags gtk_cell_renderer_get_state (GtkCellRenderer *cell,
;;;                                            GtkWidget *widget,
;;;                                            GtkCellRendererState cell_state)
;;; 
;;; Translates the cell renderer state to GtkStateFlags, based on the cell
;;; renderer and widget sensitivity, and the given GtkCellRendererState.
;;; 
;;; cell :
;;; 	a GtkCellRenderer, or NULL
;;; 
;;; widget :
;;; 	a GtkWidget, or NULL
;;; 
;;; cell_state :
;;; 	cell renderer state
;;; 
;;; Returns :
;;; 	the widget state flags applying to cell
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
;;; 	A GtkCellRenderer
;;; 
;;; Returns :
;;; 	TRUE if the cell renderer can do anything when activated
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
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; minimum_size :
;;; 	location to store the minimum size, or NULL
;;; 
;;; natural_size :
;;; 	location to store the natural size, or NULL
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
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; width :
;;; 	the size which is available for allocation
;;; 
;;; minimum_height :
;;; 	location for storing the minimum size, or NULL.
;;; 
;;; natural_height :
;;; 	location for storing the preferred size, or NULL.
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
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; minimum_size :
;;; 	location for storing the minimum size, or NULL.
;;; 
;;; natural_size :
;;; 	location for storing the natural size, or NULL.
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
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; minimum_size :
;;; 	location to store the minimum size, or NULL.
;;; 
;;; natural_size :
;;; 	location to store the natural size, or NULL.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width_for_height ()
;;; 
;;; void                gtk_cell_renderer_get_preferred_width_for_height
;;;                                                         (GtkCellRenderer *cell,
;;;                                                          GtkWidget *widget,
;;;                                                          gint height,
;;;                                                          gint *minimum_width,
;;;                                                          gint *natural_width);
;;; 
;;; Retreives a cell renderers's minimum and natural width if it were rendered
;;; to widget with the specified height.
;;; 
;;; cell :
;;; 	a GtkCellRenderer instance
;;; 
;;; widget :
;;; 	the GtkWidget this cell will be rendering to
;;; 
;;; height :
;;; 	the size which is available for allocation
;;; 
;;; minimum_width :
;;; 	location for storing the minimum size, or NULL.
;;; 
;;; natural_width :
;;; 	location for storing the preferred size, or NULL.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_request_mode ()
;;; 
;;; GtkSizeRequestMode gtk_cell_renderer_get_request_mode
;;;                                                      (GtkCellRenderer *cell)
;;; 
;;; Gets whether the cell renderer prefers a height-for-width layout or a
;;; width-for-height layout.
;;; 
;;; cell :
;;; 	a GtkCellRenderer instance
;;; 
;;; Returns :
;;; 	The GtkSizeRequestMode preferred by this renderer.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.cell-renderer.lisp -------------------------------------

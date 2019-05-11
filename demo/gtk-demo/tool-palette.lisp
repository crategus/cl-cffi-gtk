;;;; A demo for GtkToolPalette
;;;;
;;;; A tool palette widget shows groups of toolbar items as a grid of icons or
;;;; a list of names.

(in-package #:gtk-demo)

(defun load-stock-items (palette)
  (let ((group (make-instance 'gtk-tool-item-group
                              :label "Stock Icons"))
        (group-empty (make-instance 'gtk-tool-item-group
                                    :label "For Drag and drop"))
        (stock-ids (gtk-stock-list-ids)))

    (dolist (id stock-ids)
      (let ((item (make-instance 'gtk-tool-button
                                 :stock-id id
                                 :is-important t
                                 :tooltip-text id)))
      (gtk-tool-item-group-insert group item -1)
    ))

    ;; Add one button to the empty group
    (gtk-tool-item-group-insert group-empty
                                (make-instance 'gtk-tool-button
                                               :stock-id "gtk-ok"
                                               :is-important t
                                               :tooltip-text "gtk-ok")
                                -1)

    (gtk-container-add palette group-empty)
    (gtk-container-add palette group)))

#|
{
  if (GTK_TOOL_ITEM_GROUP (drag_group) != drop_group)
    {
      gboolean homogeneous, expand, fill, new_row;

      g_object_ref (drag_item);
      gtk_container_child_get (GTK_CONTAINER (drag_group), GTK_WIDGET (drag_item),
                               "homogeneous", &homogeneous,
                               "expand", &expand,
                               "fill", &fill,
                               "new-row", &new_row,
                               NULL);
      gtk_container_remove (GTK_CONTAINER (drag_group), GTK_WIDGET (drag_item));
      gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (drop_group),
                                  drag_item, drop_position);
      gtk_container_child_set (GTK_CONTAINER (drop_group), GTK_WIDGET (drag_item),
                               "homogeneous", homogeneous,
                               "expand", expand,
                               "fill", fill,
                               "new-row", new_row,
                               NULL);
      g_object_unref (drag_item);
    }
  else
    gtk_tool_item_group_set_item_position (GTK_TOOL_ITEM_GROUP (drop_group),
                                           drag_item, drop_position);
}
|#

(defun palette-drop-item (drag-item drop-group x y)
  (let ((drag-group (gtk-widget-parent drag-item))
        (drop-item (gtk-tool-item-group-get-drop-item drop-group x y))
        (drop-position -1))
    (format t "PALETTE-DROP-ITEM~%")
    (format t "   drag-group = ~A~%" drag-group)
    (format t "   drop-item  = ~A~%" drop-item)
    (format t "   drop-pos   = ~A~%" drop-position)

    (when drop-item
      (setf drop-position
            (gtk-tool-item-group-get-item-position drop-group drop-item)))

    (format t "   drop-pos   = ~A~%" drop-position)

#|
    {
      gboolean homogeneous, expand, fill, new_row;

      g_object_ref (drag_item);
      gtk_container_child_get (GTK_CONTAINER (drag_group), GTK_WIDGET (drag_item),
                               "homogeneous", &homogeneous,
                               "expand", &expand,
                               "fill", &fill,
                               "new-row", &new_row,
                               NULL);
      gtk_container_remove (GTK_CONTAINER (drag_group), GTK_WIDGET (drag_item));
      gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (drop_group),
                                  drag_item, drop_position);
      gtk_container_child_set (GTK_CONTAINER (drop_group), GTK_WIDGET (drag_item),
                               "homogeneous", homogeneous,
                               "expand", expand,
                               "fill", fill,
                               "new-row", new_row,
                               NULL);
      g_object_unref (drag_item);
    }
|#

    (if (not (equal drag-group drop-group))
        (progn
          (format t "DRAG-GROUP and DROP-GROUP are different~%")
          (let ((child-props (gtk-container-child-get drag-group drag-item
                                                      "homogeneous" "expand" "fill" "new-row")))
            (format t "   child-props = ~A~%" child-props)

            (gtk-container-remove drag-group drag-item)
            (gtk-tool-item-group-insert drop-group drag-item drop-position)
; Check this
;            (gtk-container-child-set drop-group drag-item
;                                     "homogenous" (pop child-props)
;                                     "expand" (pop child-props)
;                                     "fill" (pop child-props)
;                                     "new-row" (pop child-props))

            (format t "   child-props = ~A~%"
                      (gtk-container-child-get drop-group drag-item
                                               "homogeneous" "expand" "fill" "new-row"))

          )
        )
        (gtk-tool-item-group-set-item-position drop-group drag-item drop-position))

    ))

(defun demo-tool-palette ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Demo Tool Palette"
                                  :border-width 12))
           ;; A horizontal Box for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A scrollable
           (scroller (make-instance 'gtk-scrolled-window
                                    :hscrollbar-policy :never
                                    :vscrollbar-policy :automatic
                                    :hexpand t
                                    :vexpand t
                                    :default-width 300))
           ;; A tool palette
           (palette (make-instance 'gtk-tool-palette
                                   :default-width 300))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           (notebook (make-instance 'gtk-notebook
                                    :border-width 6))
           (page-1 (make-instance 'gtk-grid
                                  :border-width 12
                                  :orientation :vertical
                                  :row-spacing 6))
           (page-2 (make-instance 'gtk-grid
                                  :border-width 12
                                  :orientation :vertical
                                  :row-spacing 6))


          )
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Orientation combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))

        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-get-active-text combobox))
                    (orientation (cdr (assoc text
                                             '(("Vertical" . :vertical)
                                               ("Horizontal" . :horizontal))
                                             :test #'equal))))
               (format t "Signal CHANGED text = ~A, orientation = ~A~%" text orientation)
               (setf (gtk-orientable-orientation palette) orientation)
               (if (eq orientation :horizontal)
                   (gtk-scrolled-window-set-policy scroller :automatic :never)
                   (gtk-scrolled-window-set-policy scroller :never :automatic))
           )))

        (gtk-combo-box-text-append-text combo "Vertical")
        (gtk-combo-box-text-append-text combo "Horizontal")
        (setf (gtk-combo-box-active combo) 0)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Orientation</b>"))
        (gtk-container-add page-1 combo))

      ;; Style combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))

        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-get-active-text combobox))
                    (style (cdr (assoc text
                                             '(("Icons" . :icons)
                                               ("Text" . :text)
                                               ("Both" . :both)
                                               ("Both Horizontal" . :both-horiz)
                                               ("Default" . :default))
                                             :test #'equal))))
               (format t "Signal CHANGED text = ~A, style = ~A~%" text style)
               (if (eq style :default)
                   ;; TODO: This seems to not work.
                   (gtk-tool-palette-unset-style palette)
                   (gtk-tool-palette-set-style palette style))
             )))

        (gtk-combo-box-text-append-text combo "Icons")
        (gtk-combo-box-text-append-text combo "Text")
        (gtk-combo-box-text-append-text combo "Both")
        (gtk-combo-box-text-append-text combo "Both Horizontal")
        (gtk-combo-box-text-append-text combo "Default")
        (setf (gtk-combo-box-active combo) 0)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Style</b>"))
        (gtk-container-add page-1 combo))

      ;; Icon size combo box
      (let ((combo (make-instance 'gtk-combo-box-text)))

        (g-signal-connect combo "changed"
           (lambda (combobox)
             (let* ((text (gtk-combo-box-text-get-active-text combobox))
                    (size (cdr (assoc text
                                      '(("Menu" . :menu)
                                        ("Small Toolbar" . :small-toolbar)
                                        ("Large Toolbar" . :large-toolbar)
                                        ("Button" . :button)
                                        ("Dnd" . :dnd)
                                        ("Dialog" . :dialog))
                                      :test #'equal))))
               (format t "Signal CHANGED text = ~A, size = ~A~%" text size)
               (setf (gtk-tool-palette-icon-size palette) size)
             )))

        (gtk-combo-box-text-append-text combo "Menu")
        (gtk-combo-box-text-append-text combo "Small Toolbar")
        (gtk-combo-box-text-append-text combo "Large Toolbar")
        (gtk-combo-box-text-append-text combo "Button")
        (gtk-combo-box-text-append-text combo "Dnd")
        (gtk-combo-box-text-append-text combo "Dialog")
        (setf (gtk-combo-box-active combo) 1)
        (gtk-container-add page-1
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Icon Size</b>"))
        (gtk-container-add page-1 combo))

      (gtk-notebook-append-page notebook
                                page-1
                                (make-instance 'gtk-label
                                               :label "Properties"))

      (gtk-notebook-append-page notebook
                                page-2
                                (make-instance 'gtk-label
                                               :label "DnD"))

      ;; Fill the tool palette
      (load-stock-items palette)

;      load_toggle_items (GTK_TOOL_PALETTE (palette));
;      load_special_items (GTK_TOOL_PALETTE (palette));


      ;; Add the palette to the scrolled window
      (gtk-container-add scroller palette)
      ;; Add the scrolled window to the content
      (gtk-container-add content scroller)


      ;; DnD for tool items
      (gtk-tool-palette-add-drag-dest palette
                                      palette
                                      '(:all)
                                      '(:items :groups)
                                      '(:move))

      (g-signal-connect palette "drag-data-received"
         (lambda (widget context x y selection info time)
           (let ((drag-palette (gtk-drag-get-source-widget context)))
             (format t "   DRAG-DATA-RECEIVED~%")
             (format t "   widget    = ~A~%" widget)
             (format t "   context   = ~A~%" context)
             (format t "   x,y       = ~A, ~A~%" x y)
             (format t "   selection = ~A~%" selection)
             (format t "   info      = ~A~%" info)
             (format t "   time      = ~A~%~%" time)

             (loop while (and drag-palette
                              (not (g-type-is-a (g-object-type drag-palette)
                                                "GtkToolPalette")))
                   do (setf drag-palette
                            (gtk-widget-parent drag-palette)))

             (format t "   drag-palette = ~A~%" drag-palette)

             (when drag-palette
               (let ((drag-item (gtk-tool-palette-get-drag-item drag-palette selection))
                     (drop-group (gtk-tool-palette-get-drop-group widget x y)))
                 (format t "   drag-item    = ~A~%" drag-item)
                 (format t "   drop-group   = ~A~%" drop-group)

                 (cond
                   ((g-type-is-a (g-object-type drag-item) "GtkToolItemGroup")
                    (format t "PALETTE DROP GROUP~%"))
                   ((and (g-type-is-a (g-object-type drag-item) "GtkToolItem")
                         drop-group)
                    (format t "PALETTE DROP ITEM~%")
                    (let ((allocation (gtk-widget-get-allocation drop-group)))
                      (format t "   allocation = ~A~%" allocation)
                      (palette-drop-item drag-item drop-group
                                         (- x (gdk-rectangle-x allocation))
                                         (- y (gdk-rectangle-y allocation)))
                         ))
                   (t
                    (format t "NO VALID DRAG~%")))




               )
             )

           )))

#|
{
  GtkAllocation     allocation;
  GtkToolItemGroup *drop_group = NULL;
  GtkWidget        *drag_palette = gtk_drag_get_source_widget (context);
  GtkWidget        *drag_item = NULL;

  while (drag_palette && !GTK_IS_TOOL_PALETTE (drag_palette))
    drag_palette = gtk_widget_get_parent (drag_palette);

  if (drag_palette)
    {
      drag_item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (drag_palette),
                                                  selection);
      drop_group = gtk_tool_palette_get_drop_group (GTK_TOOL_PALETTE (widget),
                                                    x, y);
    }

  if (GTK_IS_TOOL_ITEM_GROUP (drag_item))
    palette_drop_group (GTK_TOOL_PALETTE (drag_palette),
                        GTK_TOOL_ITEM_GROUP (drag_item),
                        drop_group);
  else if (GTK_IS_TOOL_ITEM (drag_item) && drop_group)
    {
      gtk_widget_get_allocation (GTK_WIDGET (drop_group), &allocation);
      palette_drop_item (GTK_TOOL_ITEM (drag_item),
                         drop_group,
                         x - allocation.x,
                         y - allocation.y);
    }
}

      /* ===== DnD for tool items ===== */





      /* ===== passive DnD dest ===== */

      contents = gtk_drawing_area_new ();
      gtk_widget_set_app_paintable (contents, TRUE);

      g_object_connect (contents,
                        "signal::draw", canvas_draw, NULL,
                        "signal::drag-data-received", passive_canvas_drag_data_received, NULL,
                        NULL);

      gtk_tool_palette_add_drag_dest (GTK_TOOL_PALETTE (palette),
                                      contents,
                                      GTK_DEST_DEFAULT_ALL,
                                      GTK_TOOL_PALETTE_DRAG_ITEMS,
                                      GDK_ACTION_COPY);

      contents_scroller = gtk_scrolled_window_new (NULL, NULL);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (contents_scroller),
                                      GTK_POLICY_AUTOMATIC,
                                      GTK_POLICY_ALWAYS);
      gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (contents_scroller),
                                             contents);
      gtk_container_set_border_width (GTK_CONTAINER (contents_scroller), 6);

      gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                contents_scroller,
                                gtk_label_new ("Passive DnD Mode"));

      /* ===== interactive DnD dest ===== */

      contents = gtk_drawing_area_new ();
      gtk_widget_set_app_paintable (contents, TRUE);

      g_object_connect (contents,
                        "signal::draw", canvas_draw, NULL,
                        "signal::drag-motion", interactive_canvas_drag_motion, NULL,
                        "signal::drag-data-received", interactive_canvas_drag_data_received, NULL,
                        "signal::drag-leave", interactive_canvas_drag_leave, NULL,
                        "signal::drag-drop", interactive_canvas_drag_drop, NULL,
                        NULL);

      gtk_tool_palette_add_drag_dest (GTK_TOOL_PALETTE (palette),
                                      contents,
                                      GTK_DEST_DEFAULT_HIGHLIGHT,
                                      GTK_TOOL_PALETTE_DRAG_ITEMS,
                                      GDK_ACTION_COPY);

      contents_scroller = gtk_scrolled_window_new (NULL, NULL);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (contents_scroller),
                                      GTK_POLICY_AUTOMATIC,
                                      GTK_POLICY_ALWAYS);
      gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (contents_scroller),
                                             contents);
      gtk_container_set_border_width (GTK_CONTAINER (contents_scroller), 6);

      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), contents_scroller,
                                gtk_label_new ("Interactive DnD Mode"));
    }
|#



      ;; Add the notebook to the action container
      (gtk-container-add action notebook)

      ;; A Quit button
      (let ((button (make-instance 'gtk-button
                                   :label "Quit"
                                   :margin-top 12)))
        (g-signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk-widget-destroy window)))
        ;; Add the quit button to the action container
        (gtk-container-add action button))

      ;; Add frame, content, and action to the window.
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window)
)))


#|

static void load_stock_items (GtkToolPalette *palette);
static void load_toggle_items (GtkToolPalette *palette);
static void load_special_items (GtkToolPalette *palette);

typedef struct _CanvasItem CanvasItem;

struct _CanvasItem
{
  GdkPixbuf *pixbuf;
  gdouble x, y;
};

static CanvasItem *drop_item = NULL;
static GList *canvas_items = NULL;

/********************************/
/* ====== Canvas drawing ====== */
/********************************/

static CanvasItem*
canvas_item_new (GtkWidget     *widget,
                 GtkToolButton *button,
                 gdouble        x,
                 gdouble        y)
{
  CanvasItem *item = NULL;
  const gchar *stock_id;
  GdkPixbuf *pixbuf;

  stock_id = gtk_tool_button_get_stock_id (button);
  pixbuf = gtk_widget_render_icon_pixbuf (widget, stock_id, GTK_ICON_SIZE_DIALOG);

  if (pixbuf)
    {
      item = g_slice_new0 (CanvasItem);
      item->pixbuf = pixbuf;
      item->x = x;
      item->y = y;
    }

  return item;
}

static void
canvas_item_free (CanvasItem *item)
{
  g_object_unref (item->pixbuf);
  g_slice_free (CanvasItem, item);
}

static void
canvas_item_draw (const CanvasItem *item,
                  cairo_t          *cr,
                  gboolean          preview)
{
  gdouble cx = gdk_pixbuf_get_width (item->pixbuf);
  gdouble cy = gdk_pixbuf_get_height (item->pixbuf);

  gdk_cairo_set_source_pixbuf (cr,
                               item->pixbuf,
                               item->x - cx * 0.5,
                               item->y - cy * 0.5);

  if (preview)
    cairo_paint_with_alpha (cr, 0.6);
  else
    cairo_paint (cr);
}

static gboolean
canvas_draw (GtkWidget *widget,
             cairo_t   *cr)
{
  GList *iter;

  cairo_set_source_rgb (cr, 1, 1, 1);
  cairo_paint (cr);

  for (iter = canvas_items; iter; iter = iter->next)
    canvas_item_draw (iter->data, cr, FALSE);

  if (drop_item)
    canvas_item_draw (drop_item, cr, TRUE);

  return TRUE;
}

/*****************************/
/* ====== Palette DnD ====== */
/*****************************/



static void
palette_drop_group (GtkToolPalette   *palette,
                    GtkToolItemGroup *drag_group,
                    GtkToolItemGroup *drop_group)
{
  gint drop_position = -1;

  if (drop_group)
    drop_position = gtk_tool_palette_get_group_position (palette, drop_group);

  gtk_tool_palette_set_group_position (palette, drag_group, drop_position);
}


/********************************/
/* ====== Passive Canvas ====== */
/********************************/

static void
passive_canvas_drag_data_received (GtkWidget        *widget,
                                   GdkDragContext   *context,
                                   gint              x,
                                   gint              y,
                                   GtkSelectionData *selection,
                                   guint             info,
                                   guint             time,
                                   gpointer          data)
{
  /* find the tool button, which is the source of this DnD operation */

  GtkWidget *palette = gtk_drag_get_source_widget (context);
  CanvasItem *canvas_item = NULL;
  GtkWidget *tool_item = NULL;

  while (palette && !GTK_IS_TOOL_PALETTE (palette))
    palette = gtk_widget_get_parent (palette);

  if (palette)
    tool_item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
                                                selection);

  g_assert (NULL == drop_item);

  /* append a new canvas item when a tool button was found */

  if (GTK_IS_TOOL_ITEM (tool_item))
    canvas_item = canvas_item_new (widget, GTK_TOOL_BUTTON (tool_item), x, y);

  if (canvas_item)
    {
      canvas_items = g_list_append (canvas_items, canvas_item);
      gtk_widget_queue_draw (widget);
    }
}

/************************************/
/* ====== Interactive Canvas ====== */
/************************************/

static gboolean
interactive_canvas_drag_motion (GtkWidget      *widget,
                                GdkDragContext *context,
                                gint            x,
                                gint            y,
                                guint           time,
                                gpointer        data)
{
  if (drop_item)
    {
      /* already have a drop indicator - just update position */

      drop_item->x = x;
      drop_item->y = y;

      gtk_widget_queue_draw (widget);
      gdk_drag_status (context, GDK_ACTION_COPY, time);
    }
  else
    {
      /* request DnD data for creating a drop indicator */

      GdkAtom target = gtk_drag_dest_find_target (widget, context, NULL);

      if (!target)
        return FALSE;

      gtk_drag_get_data (widget, context, target, time);
    }

  return TRUE;
}

static void
interactive_canvas_drag_data_received (GtkWidget        *widget,
                                       GdkDragContext   *context,
                                       gint              x,
                                       gint              y,
                                       GtkSelectionData *selection,
                                       guint             info,
                                       guint             time,
                                       gpointer          data)

{
  /* find the tool button which is the source of this DnD operation */

  GtkWidget *palette = gtk_drag_get_source_widget (context);
  GtkWidget *tool_item = NULL;

  while (palette && !GTK_IS_TOOL_PALETTE (palette))
    palette = gtk_widget_get_parent (palette);

  if (palette)
    tool_item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
                                                selection);

  /* create a drop indicator when a tool button was found */

  g_assert (NULL == drop_item);

  if (GTK_IS_TOOL_ITEM (tool_item))
    {
      drop_item = canvas_item_new (widget, GTK_TOOL_BUTTON (tool_item), x, y);
      gdk_drag_status (context, GDK_ACTION_COPY, time);
      gtk_widget_queue_draw (widget);
    }
}

static gboolean
interactive_canvas_drag_drop (GtkWidget      *widget,
                              GdkDragContext *context,
                              gint            x,
                              gint            y,
                              guint           time,
                              gpointer        data)
{
  if (drop_item)
    {
      /* turn the drop indicator into a real canvas item */

      drop_item->x = x;
      drop_item->y = y;

      canvas_items = g_list_append (canvas_items, drop_item);
      drop_item = NULL;

      /* signal the item was accepted and redraw */

      gtk_drag_finish (context, TRUE, FALSE, time);
      gtk_widget_queue_draw (widget);

      return TRUE;
    }

  return FALSE;
}

static gboolean
interactive_canvas_real_drag_leave (gpointer data)
{
  if (drop_item)
    {
      GtkWidget *widget = GTK_WIDGET (data);

      canvas_item_free (drop_item);
      drop_item = NULL;

      gtk_widget_queue_draw (widget);
    }

  return G_SOURCE_REMOVE;
}

static void
interactive_canvas_drag_leave (GtkWidget      *widget,
                               GdkDragContext *context,
                               guint           time,
                               gpointer        data)
{
  /* defer cleanup until a potential "drag-drop" signal was received */
  g_idle_add (interactive_canvas_real_drag_leave, widget);
}






static void
load_toggle_items (GtkToolPalette *palette)
{
  GSList *toggle_group = NULL;
  GtkToolItem *item;
  GtkWidget *group;
  char *label;
  int i;

  group = gtk_tool_item_group_new ("Radio Item");
  gtk_container_add (GTK_CONTAINER (palette), group);

  for (i = 1; i <= 10; ++i)
    {
      label = g_strdup_printf ("#%d", i);
      item = gtk_radio_tool_button_new (toggle_group);
      gtk_tool_button_set_label (GTK_TOOL_BUTTON (item), label);
      g_free (label);

      gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
      toggle_group = gtk_radio_tool_button_get_group (GTK_RADIO_TOOL_BUTTON (item));
    }
}

static GtkToolItem *
create_entry_item (const char *text)
{
  GtkToolItem *item;
  GtkWidget *entry;

  entry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (entry), text);
  gtk_entry_set_width_chars (GTK_ENTRY (entry), 5);

  item = gtk_tool_item_new ();
  gtk_container_add (GTK_CONTAINER (item), entry);

  return item;
}

static void
load_special_items (GtkToolPalette *palette)
{
  GtkToolItem *item;
  GtkWidget *group;
  GtkWidget *label_button;

  group = gtk_tool_item_group_new (NULL);
  label_button = gtk_button_new_with_label ("Advanced Features");
  gtk_widget_show (label_button);
  gtk_tool_item_group_set_label_widget (GTK_TOOL_ITEM_GROUP (group),
                                        label_button);
  gtk_container_add (GTK_CONTAINER (palette), group);

  item = create_entry_item ("homogeneous=FALSE");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_container_child_set (GTK_CONTAINER (group), GTK_WIDGET (item),
                           "homogeneous", FALSE, NULL);

  item = create_entry_item ("homogeneous=FALSE, expand=TRUE");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_container_child_set (GTK_CONTAINER (group), GTK_WIDGET (item),
                           "homogeneous", FALSE, "expand", TRUE,
                           NULL);

  item = create_entry_item ("homogeneous=FALSE, expand=TRUE, fill=FALSE");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_container_child_set (GTK_CONTAINER (group), GTK_WIDGET (item),
                           "homogeneous", FALSE, "expand", TRUE,
                           "fill", FALSE, NULL);

  item = create_entry_item ("homogeneous=FALSE, expand=TRUE, new-row=TRUE");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_container_child_set (GTK_CONTAINER (group), GTK_WIDGET (item),
                           "homogeneous", FALSE, "expand", TRUE,
                           "new-row", TRUE, NULL);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_UP);
  gtk_tool_item_set_tooltip_text (item, "Show on vertical palettes only");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_tool_item_set_visible_horizontal (item, FALSE);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_FORWARD);
  gtk_tool_item_set_tooltip_text (item, "Show on horizontal palettes only");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_tool_item_set_visible_vertical (item, FALSE);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_DELETE);
  gtk_tool_item_set_tooltip_text (item, "Do not show at all");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_widget_set_no_show_all (GTK_WIDGET (item), TRUE);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_FULLSCREEN);
  gtk_tool_item_set_tooltip_text (item, "Expanded this item");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  gtk_container_child_set (GTK_CONTAINER (group), GTK_WIDGET (item),
                           "homogeneous", FALSE,
                           "expand", TRUE,
                           NULL);

  item = gtk_tool_button_new_from_stock (GTK_STOCK_HELP);
  gtk_tool_item_set_tooltip_text (item, "A regular item");
  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
}
|#

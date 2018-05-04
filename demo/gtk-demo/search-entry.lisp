;;;; Search Entry
;;;;
;;;; GtkEntry allows to display icons and progress information. This demo shows
;;;; how to use these features in a search entry.

(in-package #:gtk-demo)

#|
static void
show_find_button (void)
{
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 0);
}

static void
show_cancel_button (void)
{
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 1);
}

static gboolean
search_progress (gpointer data)
{
  gtk_entry_progress_pulse (GTK_ENTRY (data));
  return G_SOURCE_CONTINUE;
}

static void
search_progress_done (GtkEntry *entry)
{
  gtk_entry_set_progress_fraction (entry, 0.0);
}

static gboolean
finish_search (GtkButton *button)
{
  show_find_button ();
  g_source_remove (search_progress_id);
  search_progress_id = 0;

  return FALSE;
}

static gboolean
start_search_feedback (gpointer data)
{
  search_progress_id = g_timeout_add_full (G_PRIORITY_DEFAULT, 100,
                                           (GSourceFunc)search_progress, data,
                                           (GDestroyNotify)search_progress_done);
  return FALSE;
}

static void
start_search (GtkButton *button,
              GtkEntry  *entry)
{
  show_cancel_button ();
  search_progress_id = g_timeout_add_seconds (1, (GSourceFunc)start_search_feedback, entry);
  finish_search_id = g_timeout_add_seconds (15, (GSourceFunc)finish_search, button);
}


static void
stop_search (GtkButton *button,
             gpointer   data)
{
  g_source_remove (finish_search_id);
  finish_search (button);
}

static void
clear_entry (GtkEntry *entry)
{
  gtk_entry_set_text (entry, "");
}

static void
search_by_name (GtkWidget *item,
                GtkEntry  *entry)
{
  gtk_entry_set_icon_from_stock (entry,
                                 GTK_ENTRY_ICON_PRIMARY,
                                 GTK_STOCK_FIND);
  gtk_entry_set_icon_tooltip_text (entry,
                                   GTK_ENTRY_ICON_PRIMARY,
                                   "Search by name\n"
                                   "Click here to change the search type");
  gtk_entry_set_placeholder_text (entry, "name");
}

static void
search_by_description (GtkWidget *item,
                       GtkEntry  *entry)
{
  gtk_entry_set_icon_from_stock (entry,
                                 GTK_ENTRY_ICON_PRIMARY,
                                 GTK_STOCK_EDIT);
  gtk_entry_set_icon_tooltip_text (entry,
                                   GTK_ENTRY_ICON_PRIMARY,
                                   "Search by description\n"
                                   "Click here to change the search type");
  gtk_entry_set_placeholder_text (entry, "description");
}

static void
search_by_file (GtkWidget *item,
                GtkEntry  *entry)
{
  gtk_entry_set_icon_from_stock (entry,
                                 GTK_ENTRY_ICON_PRIMARY,
                                 GTK_STOCK_OPEN);
  gtk_entry_set_icon_tooltip_text (entry,
                                   GTK_ENTRY_ICON_PRIMARY,
                                   "Search by file name\n"
                                   "Click here to change the search type");
  gtk_entry_set_placeholder_text (entry, "file name");
}

GtkWidget *
create_search_menu (GtkWidget *entry)
{
  GtkWidget *menu;
  GtkWidget *item;
  GtkWidget *image;

  menu = gtk_menu_new ();

  item = gtk_image_menu_item_new_with_mnemonic ("Search by _name");
  image = gtk_image_new_from_stock (GTK_STOCK_FIND, GTK_ICON_SIZE_MENU);
  gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
  gtk_image_menu_item_set_always_show_image (GTK_IMAGE_MENU_ITEM (item), TRUE);
  g_signal_connect (item, "activate",
                    G_CALLBACK (search_by_name), entry);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_image_menu_item_new_with_mnemonic ("Search by _description");
  image = gtk_image_new_from_stock (GTK_STOCK_EDIT, GTK_ICON_SIZE_MENU);
  gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
  gtk_image_menu_item_set_always_show_image (GTK_IMAGE_MENU_ITEM (item), TRUE);
  g_signal_connect (item, "activate",
                    G_CALLBACK (search_by_description), entry);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_image_menu_item_new_with_mnemonic ("Search by _file name");
  image = gtk_image_new_from_stock (GTK_STOCK_OPEN, GTK_ICON_SIZE_MENU);
  gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
  gtk_image_menu_item_set_always_show_image (GTK_IMAGE_MENU_ITEM (item), TRUE);
  g_signal_connect (item, "activate",
                    G_CALLBACK (search_by_file), entry);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  gtk_widget_show_all (menu);

  return menu;
}

static void
icon_press_cb (GtkEntry       *entry,
               gint            position,
               GdkEventButton *event,
               gpointer        data)
{
  if (position == GTK_ENTRY_ICON_PRIMARY)
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                    event->button, event->time);
}

static void
activate_cb (GtkEntry  *entry,
             GtkButton *button)
{
  if (search_progress_id != 0)
    return;

  start_search (button, entry);

}

static void
search_entry_destroyed (GtkWidget  *widget)
{
  if (finish_search_id != 0)
    g_source_remove (finish_search_id);

  if (search_progress_id != 0)
    g_source_remove (search_progress_id);

  window = NULL;
}

static void
entry_populate_popup (GtkEntry *entry,
                      GtkMenu  *menu,
                      gpointer user_data)
{
  GtkWidget *item;
  GtkWidget *search_menu;
  gboolean has_text;

  has_text = gtk_entry_get_text_length (entry) > 0;

  item = gtk_separator_menu_item_new ();
  gtk_widget_show (item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_menu_item_new_with_mnemonic ("C_lear");
  gtk_widget_show (item);
  g_signal_connect_swapped (item, "activate",
                            G_CALLBACK (clear_entry), entry);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_set_sensitive (item, has_text);

  search_menu = create_search_menu (GTK_WIDGET (entry));
  item = gtk_menu_item_new_with_label ("Search by");
  gtk_widget_show (item);
  gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), search_menu);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
}

GtkWidget *
do_search_entry (GtkWidget *do_widget)
{
  GtkWidget *content_area;
  GtkWidget *vbox;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *entry;
  GtkWidget *button;
  GtkWidget *find_button;
  GtkWidget *cancel_button;

  if (!window)
    {
      window = gtk_dialog_new_with_buttons ("Search Entry",
                                            GTK_WINDOW (do_widget),
                                            0,
                                            GTK_STOCK_CLOSE,
                                            GTK_RESPONSE_NONE,
                                            NULL);
      gtk_window_set_resizable (GTK_WINDOW (window), FALSE);

      g_signal_connect (window, "response",
                        G_CALLBACK (gtk_widget_destroy), NULL);
      g_signal_connect (window, "destroy",
                        G_CALLBACK (search_entry_destroyed), &window);

      content_area = gtk_dialog_get_content_area (GTK_DIALOG (window));

      vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
      gtk_box_pack_start (GTK_BOX (content_area), vbox, TRUE, TRUE, 0);
      gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);

      label = gtk_label_new (NULL);
      gtk_label_set_markup (GTK_LABEL (label), "Search entry demo");
      gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

      hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
      gtk_container_set_border_width (GTK_CONTAINER (hbox), 0);

      /* Create our entry */
      entry = gtk_search_entry_new ();
      gtk_box_pack_start (GTK_BOX (hbox), entry, FALSE, FALSE, 0);

      /* Create the find and cancel buttons */
      notebook = gtk_notebook_new ();
      gtk_notebook_set_show_tabs (GTK_NOTEBOOK (notebook), FALSE);
      gtk_notebook_set_show_border (GTK_NOTEBOOK (notebook), FALSE);
      gtk_box_pack_start (GTK_BOX (hbox), notebook, FALSE, FALSE, 0);

      find_button = gtk_button_new_with_label ("Find");
      g_signal_connect (find_button, "clicked",
                        G_CALLBACK (start_search), entry);
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), find_button, NULL);
      gtk_widget_show (find_button);

      cancel_button = gtk_button_new_with_label ("Cancel");
      g_signal_connect (cancel_button, "clicked",
                        G_CALLBACK (stop_search), NULL);
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), cancel_button, NULL);
      gtk_widget_show (cancel_button);

      /* Set up the search icon */
      search_by_name (NULL, GTK_ENTRY (entry));

      /* Set up the clear icon */
      g_signal_connect (entry, "icon-press",
                        G_CALLBACK (icon_press_cb), NULL);
      g_signal_connect (entry, "activate",
                        G_CALLBACK (activate_cb), NULL);

      /* Create the menu */
      menu = create_search_menu (entry);
      gtk_menu_attach_to_widget (GTK_MENU (menu), entry, NULL);

      /* add accessible alternatives for icon functionality */
      g_signal_connect (entry, "populate-popup",
                        G_CALLBACK (entry_populate_popup), NULL);

      /* Give the focus to the close button */
      button = gtk_dialog_get_widget_for_response (GTK_DIALOG (window), GTK_RESPONSE_NONE);
      gtk_widget_grab_focus (button);
    }

  if (!gtk_widget_get_visible (window))
    gtk_widget_show_all (window);
  else
    {
      gtk_widget_destroy (menu);
      gtk_widget_destroy (window);
      window = NULL;
    }

  return window;
}
|#

(defun start-search (button entry)
  (declare (ignore button entry))
)

(defun finish-search ()
)

(defun start-search-feedback ()
)

(defvar search-progress-id 0)
(defvar finish-search-id 0)

(defun search-by-name (item entry)
  (declare (ignore item))
  (gtk-entry-set-icon-from-stock entry :primary "gtk-find")
  (gtk-entry-set-icon-tooltip-text entry
                                   :primary
                                   (format nil
                                           "Search by name~%~
                                            Click here to change the search type"))
  (setf (gtk-entry-placeholder-text entry) "name"))



(defun create-search-menu (entry)
  (let ((menu (make-instance 'gtk-menu)))

    (let* ((image (gtk-image-new-from-stock "gtk-find" :menu))
           (item (make-instance 'gtk-image-menu-item
                               :use-underline t
                               :label "Search by _name"
                               :always-show-image t
                               :image image)))
      (g-signal-connect item "activate"
         (lambda (item)
           (declare (ignore item))
           (gtk-entry-set-icon-from-stock entry :primary "gtk-find")
           (gtk-entry-set-icon-tooltip-text entry
                                            :primary
                                            (format nil
                                                    "Search by name~%~
                                                     Click here to change the search type"))
           (setf (gtk-entry-placeholder-text entry) "name")))
      (gtk-menu-shell-append menu item))

    (let* ((image (gtk-image-new-from-stock "gtk-edit" :menu))
           (item (make-instance 'gtk-image-menu-item
                               :use-underline t
                               :label "Search by _description"
                               :always-show-image t
                               :image image)))
      (g-signal-connect item "activate"
         (lambda (item)
           (declare (ignore item))
           (gtk-entry-set-icon-from-stock entry :primary "gtk-edit")
           (gtk-entry-set-icon-tooltip-text entry
                                            :primary
                                            (format nil
                                                    "Search by description~%~
                                                     Click here to change the search type"))
           (setf (gtk-entry-placeholder-text entry) "description")))
      (gtk-menu-shell-append menu item))

    (let* ((image (gtk-image-new-from-stock "gtk-open" :menu))
           (item (make-instance 'gtk-image-menu-item
                               :use-underline t
                               :label "Search by _file name"
                               :always-show-image t
                               :image image)))

      (g-signal-connect item "activate"
         (lambda (item)
           (declare (ignore item))
           (gtk-entry-set-icon-from-stock entry :primary "gtk-open")
           (gtk-entry-set-icon-tooltip-text entry
                                            :primary
                                            (format nil
                                                    "Search by file name~%~
                                                     Click here to change the search type"))
           (setf (gtk-entry-placeholder-text entry) "file name")))
      (gtk-menu-shell-append menu item))
    (gtk-widget-show-all menu)
    menu))


(defun example-search-entry ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Search Entry"
                                  :border-width 12
                                  :default-width 400))
           ;; Create the search entry
           (entry (make-instance 'gtk-search-entry))
           ;; Create a search menu
           (menu (create-search-menu entry))
           (hbox (make-instance 'gtk-grid
                                :orientation :horizontal))
           (vbox (make-instance 'gtk-grid
                                :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Add the search entry to the horizontal grid
      (gtk-container-add hbox entry)

      ;; Create the find and cancel buttons
      (let ((notebook (make-instance 'gtk-notebook
                                     :show-border nil
                                     :show-tabs nil)))

        ;; Create the find button
        (let ((button (gtk-button-new-with-label "Find")))
          (g-signal-connect button "clicked"
             (lambda (button)
               (declare (ignore button))
               (gtk-notebook-set-current-page notebook 1)
               (setf search-progress-id
                     (g-timeout-add-seconds 1 #'start-search-feedback))
               (setf finish-search-id
                     (g-timeout-add-seconds 10 #'finish-search))))
          (gtk-notebook-append-page notebook button nil)
          (gtk-widget-show button))

        ;; Create the cancel button
        (let ((button (gtk-button-new-with-label "Cancel")))
          (g-signal-connect button "clicked"
             (lambda (button)
               (declare (ignore button))
               (g-source-remove finish-search-id)
               (gtk-notebook-set-current-page notebook 0)))
          (gtk-notebook-append-page notebook button nil)
          (gtk-widget-show button))

        (gtk-container-add hbox notebook))

      ;; Set up the search icon
      (search-by-name nil entry)

      ;; Set up the clear icon
      (g-signal-connect entry "icon-press"
         (lambda (entry position event)
           (declare (ignore entry))
           (when (eq position :primary)
             (gtk-menu-popup-at-pointer menu event))))

      (g-signal-connect entry "activate"
         (lambda (entry button)
           (when (eql 0 search-progress-id)
             (start-search button entry))))

      ;; Attach the menu to the entry
      (gtk-menu-attach-to-widget menu entry (null-pointer))


      (gtk-container-add vbox hbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

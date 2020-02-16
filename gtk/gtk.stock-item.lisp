;;; ----------------------------------------------------------------------------
;;; gtk.stock-item.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;;﻿
;;; Stock Items
;;;
;;;     Prebuilt common menu/toolbar items and corresponding icons.
;;;
;;; Types and Values
;;;
;;;     GtkStockItem
;;;
;;;     GTK_STOCK_ABOUT                 "gtk-about"
;;;     GTK_STOCK_ADD                   "gtk-add"
;;;     GTK_STOCK_APPLY                 "gtk-apply"
;;;     GTK_STOCK_BOLD                  "gtk-bold"
;;;     GTK_STOCK_CANCEL                "gtk-cancel"
;;;     GTK_STOCK_CAPS_LOCK_WARNING     "gtk-caps-lock-warning"
;;;     GTK_STOCK_CDROM                 "gtk-cdrom"
;;;     GTK_STOCK_CLEAR                 "gtk-clear"
;;;     GTK_STOCK_CLOSE                 "gtk-close"
;;;     GTK_STOCK_COLOR_PICKER          "gtk-color-picker"
;;;     GTK_STOCK_CONVERT               "gtk-convert"
;;;     GTK_STOCK_CONNECT               "gtk-connect"
;;;     GTK_STOCK_COPY                  "gtk-copy"
;;;     GTK_STOCK_CUT                   "gtk-cut"
;;;     GTK_STOCK_DELETE                "gtk-delete"
;;;     GTK_STOCK_DIALOG_AUTHENTICATION "gtk-dialog-authentication"
;;;     GTK_STOCK_DIALOG_ERROR          "gtk-dialog-error"
;;;     GTK_STOCK_DIALOG_INFO           "gtk-dialog-info"
;;;     GTK_STOCK_DIALOG_QUESTION       "gtk-dialog-question"
;;;     GTK_STOCK_DIALOG_WARNING        "gtk-dialog-warning"
;;;     GTK_STOCK_DIRECTORY             "gtk-directory"
;;;     GTK_STOCK_DISCARD               "gtk-discard"
;;;     GTK_STOCK_DISCONNECT            "gtk-disconnect"
;;;     GTK_STOCK_DND                   "gtk-dnd"
;;;     GTK_STOCK_DND_MULTIPLE          "gtk-dnd-multiple"
;;;     GTK_STOCK_EDIT                  "gtk-edit"
;;;     GTK_STOCK_EXECUTE               "gtk-execute"
;;;     GTK_STOCK_FILE                  "gtk-file"
;;;     GTK_STOCK_FIND                  "gtk-find"
;;;     GTK_STOCK_FIND_AND_REPLACE      "gtk-find-and-replace"
;;;     GTK_STOCK_FLOPPY                "gtk-floppy"
;;;     GTK_STOCK_FULLSCREEN            "gtk-fullscreen"
;;;     GTK_STOCK_GOTO_BOTTOM           "gtk-goto-bottom"
;;;     GTK_STOCK_GOTO_FIRST            "gtk-goto-first"
;;;     GTK_STOCK_GOTO_LAST             "gtk-goto-last"
;;;     GTK_STOCK_GOTO_TOP              "gtk-goto-top"
;;;     GTK_STOCK_GO_BACK               "gtk-go-back"
;;;     GTK_STOCK_GO_DOWN               "gtk-go-down"
;;;     GTK_STOCK_GO_FORWARD            "gtk-go-forward"
;;;     GTK_STOCK_GO_UP                 "gtk-go-up"
;;;     GTK_STOCK_HARDDISK              "gtk-harddisk"
;;;     GTK_STOCK_HELP                  "gtk-help"
;;;     GTK_STOCK_HOME                  "gtk-home"
;;;     GTK_STOCK_INDENT                "gtk-indent"
;;;     GTK_STOCK_INDEX                 "gtk-index"
;;;     GTK_STOCK_INFO                  "gtk-info"
;;;     GTK_STOCK_ITALIC                "gtk-italic"
;;;     GTK_STOCK_JUMP_TO               "gtk-jump-to"
;;;     GTK_STOCK_JUSTIFY_CENTER        "gtk-justify-center"
;;;     GTK_STOCK_JUSTIFY_FILL          "gtk-justify-fill"
;;;     GTK_STOCK_JUSTIFY_LEFT          "gtk-justify-left"
;;;     GTK_STOCK_JUSTIFY_RIGHT         "gtk-justify-right"
;;;     GTK_STOCK_LEAVE_FULLSCREEN      "gtk-leave-fullscreen"
;;;     GTK_STOCK_MEDIA_FORWARD         "gtk-media-forward"
;;;     GTK_STOCK_MEDIA_NEXT            "gtk-media-next"
;;;     GTK_STOCK_MEDIA_PAUSE           "gtk-media-pause"
;;;     GTK_STOCK_MEDIA_PLAY            "gtk-media-play"
;;;     GTK_STOCK_MEDIA_PREVIOUS        "gtk-media-previous"
;;;     GTK_STOCK_MEDIA_RECORD          "gtk-media-record"
;;;     GTK_STOCK_MEDIA_REWIND          "gtk-media-rewind"
;;;     GTK_STOCK_MEDIA_STOP            "gtk-media-stop"
;;;     GTK_STOCK_MISSING_IMAGE         "gtk-missing-image"
;;;     GTK_STOCK_NETWORK               "gtk-network"
;;;     GTK_STOCK_NEW                   "gtk-new"
;;;     GTK_STOCK_NO                    "gtk-no"
;;;     GTK_STOCK_OK                    "gtk-ok"
;;;     GTK_STOCK_OPEN                  "gtk-open"
;;;     GTK_STOCK_ORIENTATION_LANDSCAPE "gtk-orientation-landscape"
;;;     GTK_STOCK_ORIENTATION_PORTRAIT  "gtk-orientation-portrait"
;;;     GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE
;;;                                     "gtk-orientation-reverse-landscape"
;;;     GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT
;;;                                     "gtk-orientation-reverse-portrait"
;;;     GTK_STOCK_PAGE_SETUP            "gtk-page-setup"
;;;     GTK_STOCK_PASTE                 "gtk-paste"
;;;     GTK_STOCK_PREFERENCES           "gtk-preferences"
;;;     GTK_STOCK_PRINT                 "gtk-print"
;;;     GTK_STOCK_PRINT_ERROR           "gtk-print-error"
;;;     GTK_STOCK_PRINT_PAUSED          "gtk-print-paused"
;;;     GTK_STOCK_PRINT_PREVIEW         "gtk-print-preview"
;;;     GTK_STOCK_PRINT_REPORT          "gtk-print-report"
;;;     GTK_STOCK_PRINT_WARNING         "gtk-print-warning"
;;;     GTK_STOCK_PROPERTIES            "gtk-properties"
;;;     GTK_STOCK_QUIT                  "gtk-quit"
;;;     GTK_STOCK_REDO                  "gtk-redo"
;;;     GTK_STOCK_REFRESH               "gtk-refresh"
;;;     GTK_STOCK_REMOVE                "gtk-remove"
;;;     GTK_STOCK_REVERT_TO_SAVED       "gtk-revert-to-saved"
;;;     GTK_STOCK_SAVE                  "gtk-save"
;;;     GTK_STOCK_SAVE_AS               "gtk-save-as"
;;;     GTK_STOCK_SELECT_ALL            "gtk-select-all"
;;;     GTK_STOCK_SELECT_COLOR          "gtk-select-color"
;;;     GTK_STOCK_SELECT_FONT           "gtk-select-font"
;;;     GTK_STOCK_SORT_ASCENDING        "gtk-sort-ascending"
;;;     GTK_STOCK_SORT_DESCENDING       "gtk-sort-descending"
;;;     GTK_STOCK_SPELL_CHECK           "gtk-spell-check"
;;;     GTK_STOCK_STOP                  "gtk-stop"
;;;     GTK_STOCK_STRIKETHROUGH         "gtk-strikethrough"
;;;     GTK_STOCK_UNDELETE              "gtk-undelete"
;;;     GTK_STOCK_UNDERLINE             "gtk-underline"
;;;     GTK_STOCK_UNDO                  "gtk-undo"
;;;     GTK_STOCK_UNINDENT              "gtk-unindent"
;;;     GTK_STOCK_YES                   "gtk-yes"
;;;     GTK_STOCK_ZOOM_100              "gtk-zoom-100"
;;;     GTK_STOCK_ZOOM_FIT              "gtk-zoom-fit"
;;;     GTK_STOCK_ZOOM_IN               "gtk-zoom-in"
;;;     GTK_STOCK_ZOOM_OUT              "gtk-zoom-out"
;;;
;;; Functions
;;;
;;;     gtk_stock_add
;;;     gtk_stock_add_static
;;;     gtk_stock_item_copy
;;;     gtk_stock_item_free
;;;     gtk_stock_list_ids
;;;     gtk_stock_lookup
;;;     gtk_stock_set_translate_func
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStockItem
;;;
;;; struct GtkStockItem {
;;;   gchar *stock_id;
;;;   gchar *label;
;;;   GdkModifierType modifier;
;;;   guint keyval;
;;;   gchar *translation_domain;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_add ()
;;;
;;; void gtk_stock_add (const GtkStockItem *items, guint n_items);
;;;
;;; gtk_stock_add has been deprecated since version 3.10 and should not be used
;;; in newly-written code.
;;;
;;; Registers each of the stock items in items. If an item already exists with
;;; the same stock ID as one of the items, the old item gets replaced. The stock
;;; items are copied, so GTK+ does not hold any pointer into items and items can
;;; be freed. Use gtk_stock_add_static() if items is persistent and GTK+ need
;;; not copy the array.
;;;
;;; items :
;;;     a GtkStockItem or array of items
;;;
;;; n_items :
;;;     number of GtkStockItem in items
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_add_static ()
;;;
;;; void gtk_stock_add_static (const GtkStockItem *items, guint n_items);
;;;
;;; gtk_stock_add_static has been deprecated since version 3.10 and should not
;;; be used in newly-written code.
;;;
;;; Same as gtk_stock_add(), but doesn't copy items, so items must persist until
;;; application exit.
;;;
;;; items :
;;;     a GtkStockItem or array of GtkStockItem
;;;
;;; n_items :
;;;     number of items
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_item_copy ()
;;;
;;; GtkStockItem * gtk_stock_item_copy (const GtkStockItem *item);
;;;
;;; gtk_stock_item_copy has been deprecated since version 3.10 and should not
;;; be used in newly-written code.
;;;
;;; Copies a stock item, mostly useful for language bindings and not in
;;; applications.
;;;
;;; item :
;;;     a GtkStockItem
;;;
;;; Returns :
;;;     a new GtkStockItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_item_free ()
;;;
;;; void gtk_stock_item_free (GtkStockItem *item);
;;;
;;; gtk_stock_item_free has been deprecated since version 3.10 and should not
;;; be used in newly-written code.
;;;
;;; Frees a stock item allocated on the heap, such as one returned by
;;; gtk_stock_item_copy(). Also frees the fields inside the stock item, if they
;;; are not NULL.
;;;
;;; item :
;;;     a GtkStockItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_list_ids ()
;;; ----------------------------------------------------------------------------

;;; TODO: We free the list. What about the strings?
;;; From the documentation of the C Libraray:
;;;   The list must be freed with g_slist_free(),
;;;   and each string in the list must be freed with g_free().

(defcfun ("gtk_stock_list_ids" gtk-stock-list-ids)
    (g-slist :string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-18}
  @return{A list of known stock IDs.}
  @begin{short}
    Retrieves a list of all known stock IDs added to a @class{gtk-icon-factory}
    or registered with @fun{gtk-stock-add}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
  (gtk-stock-list-ids)
=> (\"gtk-zoom-out\" \"gtk-zoom-in\" \"gtk-zoom-fit\" ...)
  (member \"gtk-cancel\" (gtk-stock-list-ids) :test #'equal)
=> (\"gtk-cancel\" \"gtk-bold\" \"gtk-apply\" \"gtk-add\" \"gtk-about\")
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-stock-list-ids} has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-function{gtk-stock-add}")

(export 'gtk-stock-list-ids)

;;; ----------------------------------------------------------------------------
;;; gtk_stock_lookup ()
;;;
;;; gboolean gtk_stock_lookup (const gchar *stock_id, GtkStockItem *item);
;;;
;;; gtk_stock_lookupp has been deprecated since version 3.10 and should not
;;; be used in newly-written code.
;;;
;;; Fills item with the registered values for stock_id, returning TRUE if
;;; stock_id was known.
;;;
;;; stock_id :
;;;     a stock item name
;;;
;;; item :
;;;     stock item to initialize with values
;;;
;;; Returns :
;;;     TRUE if item was initialized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_stock_set_translate_func ()
;;;
;;; void gtk_stock_set_translate_func (const gchar *domain,
;;;                                    GtkTranslateFunc func,
;;;                                    gpointer data,
;;;                                    GDestroyNotify notify);
;;;
;;; gtk_stock_set_translate_func has been deprecated since version 3.10 and
;;; should not be used in newly-written code.
;;;
;;; Sets a function to be used for translating the label of a stock item.
;;;
;;; If no function is registered for a translation domain, g_dgettext() is used.
;;;
;;; The function is used for all stock items whose translation_domain matches
;;; domain. Note that it is possible to use strings different from the actual
;;; gettext translation domain of your application for this, as long as your
;;; GtkTranslateFunc uses the correct domain when calling dgettext(). This can
;;; be useful, e.g. when dealing with message contexts:
;;;
;;; GtkStockItem items[] = {
;;;  { MY_ITEM1, NC_("odd items", "Item 1"), 0, 0, "odd-item-domain" },
;;;  { MY_ITEM2, NC_("even items", "Item 2"), 0, 0, "even-item-domain" },
;;; };
;;;
;;; gchar *
;;; my_translate_func (const gchar *msgid,
;;;                    gpointer     data)
;;; {
;;;   gchar *msgctxt = data;
;;;
;;;   return (gchar*)g_dpgettext2 (GETTEXT_PACKAGE, msgctxt, msgid);
;;; }
;;;
;;; /* ... */
;;;
;;; gtk_stock_add (items, G_N_ELEMENTS (items));
;;; gtk_stock_set_translate_func ("odd-item-domain",
;;;                               my_translate_func,
;;;                               "odd items");
;;; gtk_stock_set_translate_func ("even-item-domain",
;;;                               my_translate_func,
;;;                               "even items");
;;;
;;; domain :
;;;     the translation domain for which func shall be used
;;;
;;; func :
;;;     a GtkTranslateFunc
;;;
;;; data :
;;;     data to pass to func
;;;
;;; notify :
;;;     a GDestroyNotify that is called when data is no longer needed
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.stock-item.lisp ----------------------------------------

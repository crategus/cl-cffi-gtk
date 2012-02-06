;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser.lisp
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
;;; GtkRecentChooser
;;; 
;;; Interface implemented by widgets displaying recently used files
;;; 
;;; Synopsis
;;; 
;;;     GtkRecentChooser
;;;     GtkRecentChooserIface
;;;     GTK_RECENT_CHOOSER_ERROR
;;;     GtkRecentChooserError
;;;
;;;     gtk_recent_chooser_set_show_private
;;;     gtk_recent_chooser_get_show_private
;;;     gtk_recent_chooser_set_show_not_found
;;;     gtk_recent_chooser_get_show_not_found
;;;     gtk_recent_chooser_set_show_icons
;;;     gtk_recent_chooser_get_show_icons
;;;     gtk_recent_chooser_set_select_multiple
;;;     gtk_recent_chooser_get_select_multiple
;;;     gtk_recent_chooser_set_local_only
;;;     gtk_recent_chooser_get_local_only
;;;     gtk_recent_chooser_set_limit
;;;     gtk_recent_chooser_get_limit
;;;     gtk_recent_chooser_set_show_tips
;;;     gtk_recent_chooser_get_show_tips
;;;
;;;     GtkRecentSortType
;;;
;;;     gtk_recent_chooser_set_sort_type
;;;     gtk_recent_chooser_get_sort_type
;;;     gtk_recent_chooser_set_sort_func
;;;     gtk_recent_chooser_set_current_uri
;;;     gtk_recent_chooser_get_current_uri
;;;     gtk_recent_chooser_get_current_item
;;;     gtk_recent_chooser_select_uri
;;;     gtk_recent_chooser_unselect_uri
;;;     gtk_recent_chooser_select_all
;;;     gtk_recent_chooser_unselect_all
;;;     gtk_recent_chooser_get_items
;;;     gtk_recent_chooser_get_uris
;;;     gtk_recent_chooser_add_filter
;;;     gtk_recent_chooser_remove_filter
;;;     gtk_recent_chooser_list_filters
;;;     gtk_recent_chooser_set_filter
;;;     gtk_recent_chooser_get_filter
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkRecentChooser
;;; 
;;; Prerequisites
;;; 
;;; GtkRecentChooser requires GObject.
;;;
;;; Known Implementations
;;; 
;;; GtkRecentChooser is implemented by GtkRecentAction, GtkRecentChooserDialog,
;;; GtkRecentChooserMenu and GtkRecentChooserWidget.
;;;
;;; Properties
;;; 
;;;   "filter"                   GtkRecentFilter*      : Read / Write
;;;   "limit"                    gint                  : Read / Write
;;;   "local-only"               gboolean              : Read / Write
;;;   "recent-manager"           GtkRecentManager*     : Write / Construct Only
;;;   "select-multiple"          gboolean              : Read / Write
;;;   "show-icons"               gboolean              : Read / Write
;;;   "show-not-found"           gboolean              : Read / Write
;;;   "show-private"             gboolean              : Read / Write
;;;   "show-tips"                gboolean              : Read / Write
;;;   "sort-type"                GtkRecentSortType     : Read / Write
;;; 
;;; Signals
;;; 
;;;   "item-activated"                                 : Run Last
;;;   "selection-changed"                              : Run Last
;;; 
;;; Description
;;; 
;;; GtkRecentChooser is an interface that can be implemented by widgets
;;; displaying the list of recently used files. In GTK+, the main objects that
;;; implement this interface are GtkRecentChooserWidget, GtkRecentChooserDialog
;;; and GtkRecentChooserMenu.
;;; 
;;; Recently used files are supported since GTK+ 2.10.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "filter" property
;;; 
;;;   "filter"                   GtkRecentFilter*      : Read / Write
;;; 
;;; The GtkRecentFilter object to be used when displaying the recently used
;;; resources.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "limit" property
;;; 
;;;   "limit"                    gint                  : Read / Write
;;; 
;;; The maximum number of recently used resources to be displayed, or -1 to
;;; display all items. By default, the GtkSetting:gtk-recent-files-limit setting
;;; is respected: you can override that limit on a particular instance of
;;; GtkRecentChooser by setting this property.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "local-only" property
;;; 
;;;   "local-only"               gboolean              : Read / Write
;;; 
;;; Whether this GtkRecentChooser should display only local (file:) resources.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "recent-manager" property
;;; 
;;;   "recent-manager"           GtkRecentManager*     : Write / Construct Only
;;; 
;;; The GtkRecentManager instance used by the GtkRecentChooser to display the
;;; list of recently used resources.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-multiple" property
;;; 
;;;   "select-multiple"          gboolean              : Read / Write
;;; 
;;; Allow the user to select multiple resources.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-icons" property
;;; 
;;;   "show-icons"               gboolean              : Read / Write
;;; 
;;; Whether this GtkRecentChooser should display an icon near the item.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-not-found" property
;;; 
;;;   "show-not-found"           gboolean              : Read / Write
;;; 
;;; Whether this GtkRecentChooser should display the recently used resources
;;; even if not present anymore. Setting this to FALSE will perform a
;;; potentially expensive check on every local resource (every remote resource
;;; will always be displayed).
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-private" property
;;; 
;;;   "show-private"             gboolean              : Read / Write
;;; 
;;; Whether the private items should be displayed.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-tips" property
;;; 
;;;   "show-tips"                gboolean              : Read / Write
;;; 
;;; Whether this GtkRecentChooser should display a tooltip containing the full
;;; path of the recently used resources.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "sort-type" property
;;; 
;;;   "sort-type"                GtkRecentSortType     : Read / Write
;;; 
;;; Sorting order to be used when displaying the recently used resources.
;;; 
;;; Default value: GTK_RECENT_SORT_NONE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "item-activated" signal
;;; 
;;; void user_function (GtkRecentChooser *chooser,
;;;                     gpointer          user_data)      : Run Last
;;; 
;;; This signal is emitted when the user "activates" a recent item in the recent
;;; chooser. This can happen by double-clicking on an item in the recently used
;;; resources list, or by pressing Enter.
;;; 
;;; chooser :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-changed" signal
;;; 
;;; void user_function (GtkRecentChooser *chooser,
;;;                     gpointer          user_data)      : Run Last
;;; 
;;; This signal is emitted when there is a change in the set of selected
;;; recently used resources. This can happen when a user modifies the selection
;;; with the mouse or the keyboard, or when explicitely calling functions to
;;; change the selection.
;;; 
;;; chooser :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentChooser
;;; 
;;; typedef struct _GtkRecentChooser GtkRecentChooser;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkRecentChooser" gtk-recent-chooser
  (:export t
   :type-initializer "gtk_recent_chooser_get_type")
  (filter gtk-recent-chooser-filter
   "filter" "GtkRecentFilter" t t)
  (limit gtk-recent-chooser-limit
   "limit" "gint" t t)
  (local-only gtk-recent-chooser-local-only
   "local-only" "gboolean" t t)
  (recent-manager gtk-recent-chooser-recent-manager
   "recent-manager" "GtkRecentManager" nil nil)
  (select-multiple gtk-recent-chooser-select-multiple
   "select-multiple" "gboolean" t t)
  (show-icons gtk-recent-chooser-show-icons
   "show-icons" "gboolean" t t)
  (show-not-found gtk-recent-chooser-show-not-found
   "show-not-found" "gboolean" t t)
  (show-private gtk-recent-chooser-show-private
   "show-private" "gboolean" t t)
  (show-tips gtk-recent-chooser-show-tips
   "show-tips" "gboolean" t t)
  (sort-type gtk-recent-chooser-sort-type
   "sort-type" "GtkRecentSortType" t t))

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserIface
;;; 
;;; struct GtkRecentChooserIface {
;;;   GTypeInterface base_iface;
;;; 
;;;   /*
;;;    * Methods
;;;    */
;;;   gboolean          (* set_current_uri)    (GtkRecentChooser  *chooser,
;;;                                             const gchar       *uri,
;;;                                             GError           **error);
;;;   gchar *           (* get_current_uri)    (GtkRecentChooser  *chooser);
;;;   gboolean          (* select_uri)         (GtkRecentChooser  *chooser,
;;;                                             const gchar       *uri,
;;;                                             GError           **error);
;;;   void              (* unselect_uri)       (GtkRecentChooser  *chooser,
;;;                                             const gchar       *uri);
;;;   void              (* select_all)         (GtkRecentChooser  *chooser);
;;;   void              (* unselect_all)       (GtkRecentChooser  *chooser);
;;;   GList *           (* get_items)          (GtkRecentChooser  *chooser);
;;;   GtkRecentManager *(* get_recent_manager) (GtkRecentChooser  *chooser);
;;;   void              (* add_filter)         (GtkRecentChooser  *chooser,
;;;                                             GtkRecentFilter   *filter);
;;;   void              (* remove_filter)      (GtkRecentChooser  *chooser,
;;;                                             GtkRecentFilter   *filter);
;;;   GSList *          (* list_filters)       (GtkRecentChooser  *chooser);
;;;   void              (* set_sort_func)      (GtkRecentChooser  *chooser,
;;;                                             GtkRecentSortFunc  sort_func,
;;;                                             gpointer           sort_data,
;;;                                             GDestroyNotify     data_destroy)
;;; 
;;;   /*
;;;    * Signals
;;;    */
;;;   void              (* item_activated)     (GtkRecentChooser  *chooser);
;;;   void              (* selection_changed)  (GtkRecentChooser  *chooser);
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_RECENT_CHOOSER_ERROR
;;; 
;;; #define GTK_RECENT_CHOOSER_ERROR (gtk_recent_chooser_error_quark ())
;;; 
;;; Used to get the GError quark for GtkRecentChooser errors.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentChooserError
;;; 
;;; typedef enum {
;;;   GTK_RECENT_CHOOSER_ERROR_NOT_FOUND,
;;;   GTK_RECENT_CHOOSER_ERROR_INVALID_URI
;;; } GtkRecentChooserError;
;;; 
;;; These identify the various errors that can occur while calling
;;; GtkRecentChooser functions.
;;; 
;;; GTK_RECENT_CHOOSER_ERROR_NOT_FOUND
;;;     Indicates that a file does not exist
;;; 
;;; GTK_RECENT_CHOOSER_ERROR_INVALID_URI
;;;     Indicates a malformed URI
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentChooserError" gtk-recent-chooser-error
  (:export t
   :type-initializer "gtk_recent_chooser_error_get_type")
  (:not-found 0)
  (:invalid-uri 1))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_private ()
;;; 
;;; void gtk_recent_chooser_set_show_private (GtkRecentChooser *chooser,
;;;                                           gboolean show_private);
;;; 
;;; Whether to show recently used resources marked registered as private.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; show_private :
;;;     TRUE to show private items, FALSE otherwise
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_private ()
;;; 
;;; gboolean gtk_recent_chooser_get_show_private (GtkRecentChooser *chooser);
;;; 
;;; Returns whether chooser should display recently used resources registered
;;; as private.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if the recent chooser should show private items, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_not_found ()
;;; 
;;; void gtk_recent_chooser_set_show_not_found (GtkRecentChooser *chooser,
;;;                                             gboolean show_not_found);
;;; 
;;; Sets whether chooser should display the recently used resources that it
;;; didn't find. This only applies to local resources.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; show_not_found :
;;;     whether to show the local items we didn't find
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_not_found ()
;;; 
;;; gboolean gtk_recent_chooser_get_show_not_found (GtkRecentChooser *chooser);
;;; 
;;; Retrieves whether chooser should show the recently used resources that were
;;; not found.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if the resources not found should be displayed, and FALSE
;;;     otheriwse.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_icons ()
;;; 
;;; void gtk_recent_chooser_set_show_icons (GtkRecentChooser *chooser,
;;;                                         gboolean show_icons);
;;; 
;;; Sets whether chooser should show an icon near the resource when displaying
;;; it.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; show_icons :
;;;     whether to show an icon near the resource
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_icons ()
;;; 
;;; gboolean gtk_recent_chooser_get_show_icons (GtkRecentChooser *chooser);
;;; 
;;; Retrieves whether chooser should show an icon near the resource.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if the icons should be displayed, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_select_multiple ()
;;; 
;;; void gtk_recent_chooser_set_select_multiple (GtkRecentChooser *chooser,
;;;                                              gboolean select_multiple);
;;; 
;;; Sets whether chooser can select multiple items.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; select_multiple :
;;;     TRUE if chooser can select more than one item
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_select_multiple ()
;;; 
;;; gboolean gtk_recent_chooser_get_select_multiple (GtkRecentChooser *chooser)
;;; 
;;; Gets whether chooser can select multiple items.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if chooser can select more than one item.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_local_only ()
;;; 
;;; void gtk_recent_chooser_set_local_only (GtkRecentChooser *chooser,
;;;                                         gboolean local_only);
;;; 
;;; Sets whether only local resources, that is resources using the file:// URI
;;; scheme, should be shown in the recently used resources selector. If
;;; local_only is TRUE (the default) then the shown resources are guaranteed to
;;; be accessible through the operating system native file system.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; local_only :
;;;     TRUE if only local files can be shown
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_local_only ()
;;; 
;;; gboolean gtk_recent_chooser_get_local_only (GtkRecentChooser *chooser);
;;; 
;;; Gets whether only local resources should be shown in the recently used
;;; resources selector. See gtk_recent_chooser_set_local_only()
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if only local resources should be shown.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_limit ()
;;; 
;;; void gtk_recent_chooser_set_limit (GtkRecentChooser *chooser, gint limit);
;;; 
;;; Sets the number of items that should be returned by
;;; gtk_recent_chooser_get_items() and gtk_recent_chooser_get_uris().
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; limit :
;;;     a positive integer, or -1 for all items
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_limit ()
;;; 
;;; gint gtk_recent_chooser_get_limit (GtkRecentChooser *chooser);
;;; 
;;; Gets the number of items returned by gtk_recent_chooser_get_items() and
;;; gtk_recent_chooser_get_uris().
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     A positive integer, or -1 meaning that all items are returned.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_tips ()
;;; 
;;; void gtk_recent_chooser_set_show_tips (GtkRecentChooser *chooser,
;;;                                        gboolean show_tips);
;;; 
;;; Sets whether to show a tooltips containing the full path of each recently
;;; used resource in a GtkRecentChooser widget.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; show_tips :
;;;     TRUE if tooltips should be shown
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_tips ()
;;; 
;;; gboolean gtk_recent_chooser_get_show_tips (GtkRecentChooser *chooser);
;;; 
;;; Gets whether chooser should display tooltips containing the full path of
;;; a recently user resource.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     TRUE if the recent chooser should show tooltips, FALSE otherwise.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentSortType
;;; 
;;; typedef enum {
;;;   GTK_RECENT_SORT_NONE = 0,
;;;   GTK_RECENT_SORT_MRU,
;;;   GTK_RECENT_SORT_LRU,
;;;   GTK_RECENT_SORT_CUSTOM
;;; } GtkRecentSortType;
;;; 
;;; Used to specify the sorting method to be applyed to the recently used
;;; resource list.
;;; 
;;; GTK_RECENT_SORT_NONE
;;;     Do not sort the returned list of recently used resources.
;;; 
;;; GTK_RECENT_SORT_MRU
;;;     Sort the returned list with the most recently used items first.
;;; 
;;; GTK_RECENT_SORT_LRU
;;;     Sort the returned list with the least recently used items first.
;;; 
;;; GTK_RECENT_SORT_CUSTOM
;;;     Sort the returned list using a custom sorting function passed using
;;;     gtk_recent_manager_set_sort_func().
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentSortType" gtk-recent-sort-type
  (:export t
   :type-initializer "gtk_recent_sort_type_get_type")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_sort_type ()
;;; 
;;; void gtk_recent_chooser_set_sort_type (GtkRecentChooser *chooser,
;;;                                        GtkRecentSortType sort_type);
;;; 
;;; Changes the sorting order of the recently used resources list displayed by
;;; chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; sort_type :
;;;     sort order that the chooser should use
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_sort_type ()
;;; 
;;; GtkRecentSortType gtk_recent_chooser_get_sort_type
;;;                                                  (GtkRecentChooser *chooser)
;;; 
;;; Gets the value set by gtk_recent_chooser_set_sort_type().
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     the sorting order of the chooser.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkRecentSortFunc ()
;;; 
;;; gint (*GtkRecentSortFunc) (GtkRecentInfo *a,
;;;                            GtkRecentInfo *b,
;;;                            gpointer user_data);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_sort_func ()
;;; 
;;; void gtk_recent_chooser_set_sort_func (GtkRecentChooser *chooser,
;;;                                        GtkRecentSortFunc sort_func,
;;;                                        gpointer sort_data,
;;;                                        GDestroyNotify data_destroy);
;;; 
;;; Sets the comparison function used when sorting to be sort_func. If the
;;; chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the chooser
;;; will sort using this function.
;;; 
;;; To the comparison function will be passed two GtkRecentInfo structs and
;;; sort_data; sort_func should return a positive integer if the first item
;;; comes before the second, zero if the two items are equal and a negative
;;; integer if the first item comes after the second.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; sort_func :
;;;     the comparison function
;;; 
;;; sort_data :
;;;     user data to pass to sort_func, or NULL
;;; 
;;; data_destroy :
;;;     destroy notifier for sort_data, or NULL
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_current_uri ()
;;; 
;;; gboolean gtk_recent_chooser_set_current_uri (GtkRecentChooser *chooser,
;;;                                              const gchar *uri,
;;;                                              GError **error);
;;; 
;;; Sets uri as the current URI for chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; uri :
;;;     a URI
;;; 
;;; error :
;;;     return location for a GError, or NULL
;;; 
;;; Returns :
;;;     TRUE if the URI was found.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_uri ()
;;; 
;;; gchar * gtk_recent_chooser_get_current_uri (GtkRecentChooser *chooser);
;;; 
;;; Gets the URI currently selected by chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     a newly allocated string holding a URI.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_item ()
;;; 
;;; GtkRecentInfo * gtk_recent_chooser_get_current_item
;;;                                                  (GtkRecentChooser *chooser)
;;; 
;;; Gets the GtkRecentInfo currently selected by chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     a GtkRecentInfo. Use gtk_recent_info_unref() when when you have
;;;     finished using it.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_uri ()
;;; 
;;; gboolean gtk_recent_chooser_select_uri (GtkRecentChooser *chooser,
;;;                                         const gchar *uri,
;;;                                         GError **error);
;;; 
;;; Selects uri inside chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; uri :
;;;     a URI
;;; 
;;; error :
;;;     return location for a GError, or NULL
;;; 
;;; Returns :
;;;     TRUE if uri was found.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_uri ()
;;; 
;;; void gtk_recent_chooser_unselect_uri (GtkRecentChooser *chooser,
;;;                                       const gchar *uri);
;;; 
;;; Unselects uri inside chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; uri :
;;;     a URI
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_all ()
;;; 
;;; void gtk_recent_chooser_select_all (GtkRecentChooser *chooser);
;;; 
;;; Selects all the items inside chooser, if the chooser supports multiple
;;; selection.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_all ()
;;; 
;;; void gtk_recent_chooser_unselect_all (GtkRecentChooser *chooser);
;;; 
;;; Unselects all the items inside chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_items ()
;;; 
;;; GList * gtk_recent_chooser_get_items (GtkRecentChooser *chooser);
;;; 
;;; Gets the list of recently used resources in form of GtkRecentInfo objects.
;;; 
;;; The return value of this function is affected by the "sort-type" and
;;; "limit" properties of chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     A newly allocated list of GtkRecentInfo objects. You should use
;;;     gtk_recent_info_unref() on every item of the list, and then free the
;;;     list itself using g_list_free().
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_uris ()
;;; 
;;; gchar ** gtk_recent_chooser_get_uris (GtkRecentChooser *chooser,
;;;                                       gsize *length);
;;; 
;;; Gets the URI of the recently used resources.
;;; 
;;; The return value of this function is affected by the "sort-type" and
;;; "limit" properties of chooser.
;;; 
;;; Since the returned array is NULL terminated, length may be NULL.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; length :
;;;     return location for a the length of the URI list, or NULL
;;; 
;;; Returns :
;;;     A newly allocated, NULL-terminated array of strings. Use g_strfreev()
;;;     to free it.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_add_filter ()
;;; 
;;; void gtk_recent_chooser_add_filter (GtkRecentChooser *chooser,
;;;                                     GtkRecentFilter *filter);
;;; 
;;; Adds filter to the list of GtkRecentFilter objects held by chooser.
;;; 
;;; If no previous filter objects were defined, this function will call
;;; gtk_recent_chooser_set_filter().
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; filter :
;;;     a GtkRecentFilter
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_remove_filter ()
;;; 
;;; void gtk_recent_chooser_remove_filter (GtkRecentChooser *chooser,
;;;                                        GtkRecentFilter *filter);
;;; 
;;; Removes filter from the list of GtkRecentFilter objects held by chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; filter :
;;;     a GtkRecentFilter
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_list_filters ()
;;; 
;;; GSList * gtk_recent_chooser_list_filters (GtkRecentChooser *chooser);
;;; 
;;; Gets the GtkRecentFilter objects held by chooser.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     A singly linked list of GtkRecentFilter objects. You should just free
;;;     the returned list using g_slist_free().
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_filter ()
;;; 
;;; void gtk_recent_chooser_set_filter (GtkRecentChooser *chooser,
;;;                                     GtkRecentFilter *filter);
;;; 
;;; Sets filter as the current GtkRecentFilter object used by chooser to affect
;;; the displayed recently used resources.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; filter :
;;;     a GtkRecentFilter
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_filter ()
;;; 
;;; GtkRecentFilter * gtk_recent_chooser_get_filter (GtkRecentChooser *chooser)
;;; 
;;; Gets the GtkRecentFilter object currently used by chooser to affect the
;;; display of the recently used resources.
;;; 
;;; chooser :
;;;     a GtkRecentChooser
;;; 
;;; Returns :
;;;     a GtkRecentFilter object
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.recent-chooser.lisp ------------------------------------

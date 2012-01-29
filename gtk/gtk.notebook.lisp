;;; ----------------------------------------------------------------------------
;;; gtk.notebook.lisp
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
;;; GtkNotebook
;;; 
;;; A tabbed notebook container
;;; 
;;; Synopsis
;;; 
;;;     GtkNotebook
;;;     
;;;     gtk_notebook_new
;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child
;;;     gtk_notebook_set_tab_pos
;;;     gtk_notebook_set_show_tabs
;;;     gtk_notebook_set_show_border
;;;     gtk_notebook_set_scrollable
;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text
;;;     gtk_notebook_get_scrollable
;;;     gtk_notebook_get_show_border
;;;     gtk_notebook_get_show_tabs
;;;     gtk_notebook_get_tab_label_text
;;;     gtk_notebook_get_tab_pos
;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_get_tab_hborder
;;;     gtk_notebook_get_tab_vborder
;;;     gtk_notebook_set_current_page
;;;     gtk_notebook_set_group_name
;;;     gtk_notebook_get_group_name
;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkNotebook
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkNotebook implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "enable-popup"             gboolean              : Read / Write
;;;   "group-name"               gchar*                : Read / Write
;;;   "page"                     gint                  : Read / Write
;;;   "scrollable"               gboolean              : Read / Write
;;;   "show-border"              gboolean              : Read / Write
;;;   "show-tabs"                gboolean              : Read / Write
;;;   "tab-pos"                  GtkPositionType       : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "detachable"               gboolean              : Read / Write
;;;   "menu-label"               gchar*                : Read / Write
;;;   "position"                 gint                  : Read / Write
;;;   "reorderable"              gboolean              : Read / Write
;;;   "tab-expand"               gboolean              : Read / Write
;;;   "tab-fill"                 gboolean              : Read / Write
;;;   "tab-label"                gchar*                : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "arrow-spacing"            gint                  : Read
;;;   "has-backward-stepper"     gboolean              : Read
;;;   "has-forward-stepper"      gboolean              : Read
;;;   "has-secondary-backward-stepper" gboolean        : Read
;;;   "has-secondary-forward-stepper" gboolean         : Read
;;;   "initial-gap"              gint                  : Read
;;;   "tab-curvature"            gint                  : Read
;;;   "tab-overlap"              gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "change-current-page"                            : Action
;;;   "create-window"                                  : Run Last
;;;   "focus-tab"                                      : Action
;;;   "move-focus-out"                                 : Action
;;;   "page-added"                                     : Run Last
;;;   "page-removed"                                   : Run Last
;;;   "page-reordered"                                 : Run Last
;;;   "reorder-tab"                                    : Action
;;;   "select-page"                                    : Action
;;;   "switch-page"                                    : Run Last
;;; 
;;; Description
;;; 
;;; The GtkNotebook widget is a GtkContainer whose children are pages that can
;;; be switched between using tab labels along one edge.
;;; 
;;; There are many configuration options for GtkNotebook. Among other things,
;;; you can choose on which edge the tabs appear (see
;;; gtk_notebook_set_tab_pos()), whether, if there are too many tabs to fit the
;;; notebook should be made bigger or scrolling arrows added (see
;;; gtk_notebook_set_scrollable()), and whether there will be a popup menu
;;; allowing the users to switch pages. (see gtk_notebook_popup_enable(),
;;; gtk_notebook_popup_disable())
;;; 
;;; GtkNotebook as GtkBuildable
;;; 
;;; The GtkNotebook implementation of the GtkBuildable interface supports
;;; placing children into tabs by specifying "tab" as the "type" attribute of a
;;; <child> element. Note that the content of the tab must be created before
;;; the tab can be filled. A tab child can be specified without specifying a
;;; <child> type attribute.
;;; 
;;; To add a child widget in the notebooks action area, specify "action-start"
;;; or "action-end" as the "type" attribute of the <child> element.
;;; 
;;; Example 92. A UI definition fragment with GtkNotebook
;;; 
;;; <object class="GtkNotebook">
;;;   <child>
;;;     <object class="GtkLabel" id="notebook-content">
;;;       <property name="label">Content</property>
;;;     </object>
;;;   </child>
;;;   <child type="tab">
;;;     <object class="GtkLabel" id="notebook-tab">
;;;       <property name="label">Tab</property>
;;;     </object>
;;;   </child>
;;; </object>
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "enable-popup" property
;;; 
;;;   "enable-popup"             gboolean              : Read / Write
;;; 
;;; If TRUE, pressing the right mouse button on the notebook pops up a menu
;;; that you can use to go to a page.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "group-name" property
;;; 
;;;   "group-name"               gchar*                : Read / Write
;;; 
;;; Group name for tab drag and drop.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.24
;;;
;;; ----------------------------------------------------------------------------
;;; The "page" property
;;; 
;;;   "page"                     gint                  : Read / Write
;;; 
;;; The index of the current page.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "scrollable" property
;;; 
;;;   "scrollable"               gboolean              : Read / Write
;;; 
;;; If TRUE, scroll arrows are added if there are too many tabs to fit.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-border" property
;;; 
;;;   "show-border"              gboolean              : Read / Write
;;; 
;;; Whether the border should be shown.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-tabs" property
;;; 
;;;   "show-tabs"                gboolean              : Read / Write
;;; 
;;; Whether tabs should be shown.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-pos" property
;;; 
;;;   "tab-pos"                  GtkPositionType       : Read / Write
;;; 
;;; Which side of the notebook holds the tabs.
;;; 
;;; Default value: GTK_POS_TOP
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "detachable" child property
;;; 
;;;   "detachable"               gboolean              : Read / Write
;;; 
;;; Whether the tab is detachable.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "menu-label" child property
;;; 
;;;   "menu-label"               gchar*                : Read / Write
;;; 
;;; The string displayed in the child's menu entry.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "position" child property
;;; 
;;;   "position"                 gint                  : Read / Write
;;; 
;;; The index of the child in the parent.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "reorderable" child property
;;; 
;;;   "reorderable"              gboolean              : Read / Write
;;; 
;;; Whether the tab is reorderable by user action.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-expand" child property
;;; 
;;;   "tab-expand"               gboolean              : Read / Write
;;; 
;;; Whether to expand the child's tab.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-fill" child property
;;; 
;;;   "tab-fill"                 gboolean              : Read / Write
;;; 
;;; Whether the child's tab should fill the allocated area.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-label" child property
;;; 
;;;   "tab-label"                gchar*                : Read / Write
;;; 
;;; The string displayed on the child's tab label.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-spacing" style property
;;; 
;;;   "arrow-spacing"            gint                  : Read
;;; 
;;; The "arrow-spacing" property defines the spacing between the scroll arrows
;;; and the tabs.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-backward-stepper" style property
;;; 
;;;   "has-backward-stepper"     gboolean              : Read
;;; 
;;; The "has-backward-stepper" property determines whether the standard
;;; backward arrow button is displayed.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-forward-stepper" style property
;;; 
;;;   "has-forward-stepper"      gboolean              : Read
;;; 
;;; The "has-forward-stepper" property determines whether the standard forward
;;; arrow button is displayed.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-secondary-backward-stepper" style property
;;; 
;;;   "has-secondary-backward-stepper" gboolean              : Read
;;; 
;;; The "has-secondary-backward-stepper" property determines whether a second
;;; backward arrow button is displayed on the opposite end of the tab area.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-secondary-forward-stepper" style property
;;; 
;;;   "has-secondary-forward-stepper" gboolean              : Read
;;; 
;;; The "has-secondary-forward-stepper" property determines whether a second
;;; forward arrow button is displayed on the opposite end of the tab area.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "initial-gap" style property
;;; 
;;;   "initial-gap"              gint                  : Read
;;; 
;;; The "initial-gap" property defines the minimum size for the initial gap
;;; between the first tab.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-curvature" style property
;;; 
;;;   "tab-curvature"            gint                  : Read
;;; 
;;; The "tab-curvature" property defines size of tab curvature.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "tab-overlap" style property
;;; 
;;;   "tab-overlap"              gint                  : Read
;;; 
;;; The "tab-overlap" property defines size of tab overlap area.
;;; 
;;; Default value: 2
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "change-current-page" signal
;;; 
;;; gboolean user_function (GtkNotebook *notebook,
;;;                         gint         arg1,
;;;                         gpointer     user_data)      : Action
;;; 
;;; ----------------------------------------------------------------------------
;;; The "create-window" signal
;;; 
;;; GtkNotebook* user_function (GtkNotebook *notebook,
;;;                             GtkWidget   *page,
;;;                             gint         x,
;;;                             gint         y,
;;;                             gpointer     user_data)      : Run Last
;;; 
;;; The ::create-window signal is emitted when a detachable tab is dropped on
;;; the root window.
;;; 
;;; A handler for this signal can create a window containing a notebook where
;;; the tab will be attached. It is also responsible for moving/resizing the
;;; window and adding the necessary properties to the notebook
;;; (e.g. the "group").
;;; 
;;; notebook :
;;;     the GtkNotebook emitting the signal
;;; 
;;; page :
;;;     the tab of notebook that is being detached
;;; 
;;; x :
;;;     the X coordinate where the drop happens
;;; 
;;; y :
;;;     the Y coordinate where the drop happens
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     a GtkNotebook that page should be added to, or NULL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-tab" signal
;;; 
;;; gboolean user_function (GtkNotebook   *notebook,
;;;                         GtkNotebookTab arg1,
;;;                         gpointer       user_data)      : Action
;;; 
;;; ----------------------------------------------------------------------------
;;; The "move-focus-out" signal
;;; 
;;; void user_function (GtkNotebook     *notebook,
;;;                     GtkDirectionType arg1,
;;;                     gpointer         user_data)      : Action
;;; 
;;; ----------------------------------------------------------------------------
;;; The "page-added" signal
;;; 
;;; void user_function (GtkNotebook *notebook,
;;;                     GtkWidget   *child,
;;;                     guint        page_num,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; the ::page-added signal is emitted in the notebook right after a page is
;;; added to the notebook.
;;; 
;;; notebook :
;;;     the GtkNotebook
;;; 
;;; child :
;;;     the child GtkWidget affected
;;; 
;;; page_num :
;;;     the new page number for child
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-removed" signal
;;; 
;;; void user_function (GtkNotebook *notebook,
;;;                     GtkWidget   *child,
;;;                     guint        page_num,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; the ::page-removed signal is emitted in the notebook right after a page
;;; is removed from the notebook.
;;; 
;;; notebook :
;;;     the GtkNotebook
;;; 
;;; child :
;;;     the child GtkWidget affected
;;; 
;;; page_num :
;;;     the child page number
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-reordered" signal
;;; 
;;; void user_function (GtkNotebook *notebook,
;;;                     GtkWidget   *child,
;;;                     guint        page_num,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; the ::page-reordered signal is emitted in the notebook right after a page
;;; has been reordered.
;;; 
;;; notebook :
;;;     the GtkNotebook
;;; 
;;; child :
;;;     the child GtkWidget affected
;;; 
;;; page_num :
;;;     the new page number for child
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "reorder-tab" signal
;;; 
;;; gboolean user_function (GtkNotebook     *notebook,
;;;                         GtkDirectionType arg1,
;;;                         gboolean         arg2,
;;;                         gpointer         user_data)      : Action
;;; 
;;; ----------------------------------------------------------------------------
;;; The "select-page" signal
;;; 
;;; gboolean user_function (GtkNotebook *notebook,
;;;                         gboolean     arg1,
;;;                         gpointer     user_data)      : Action
;;; 
;;; ----------------------------------------------------------------------------
;;; The "switch-page" signal
;;; 
;;; void user_function (GtkNotebook *notebook,
;;;                     GtkWidget   *page,
;;;                     guint        page_num,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; Emitted when the user or a function changes the current page.
;;; 
;;; notebook :
;;;     the object which received the signal.
;;; 
;;; page :
;;;     the new current page
;;; 
;;; page_num :
;;;     the index of the page
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(defun gtk-notebook-add-page (notebook child tab-label &key
                                       (position :end) menu)
  (assert (typep position '(or integer (member :start :end))))
  (assert (typep menu '(or null g-object (member :default))))
  (case position
    (:end (if menu
              (gtk-notebook-append-page-menu notebook
                                             child
                                             tab-label
                                             (if (eq menu :default)
                                                 (null-pointer)
                                                 menu))
              (gtk-notebook-append-page notebook child tab-label)))
    (:start (if menu
                (gtk-notebook-prepend-page-menu notebook
                                                child
                                                tab-label
                                                (if (eq menu :default)
                                                    (null-pointer)
                                                    menu))
                (gtk-notebook-prepend-page notebook child tab-label)))
    (otherwise (if menu
                   (gtk-notebook-insert-page-menu notebook
                                                  child
                                                  tab-label
                                                  (if (eq menu :default)
                                                      (null-pointer)
                                                      menu)
                                                  position)
                   (gtk-notebook-insert-page notebook
                                             child
                                             tab-label
                                             position)))))

(export 'notebook-add-page)

;;; ----------------------------------------------------------------------------

(defcallback gtk-notebook-window-creation-func-callback g-object
    ((source g-object) (page g-object) (x :int) (y :int) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               source page x y)
    (return-null () nil)))

(defcfun gtk-notebook-set-window-creation-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun notebook-set-window-creation-hook (function)
  (gtk-notebook-set-window-creation-hook
                        (callback gtk-notebook-window-creation-func-callback)
                        (allocate-stable-pointer function)
                        (callback stable-pointer-free-destroy-notify-cb)))

(export 'notebook-set-window-creation-hook)

;;; ----------------------------------------------------------------------------
;;; struct GtkNotebook
;;; 
;;; struct GtkNotebook;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkNotebook" gtk-notebook
  (:superclass gtk-container
    :export t
    :interfaces ("AtkImplementorIface" "GtkBuildable")
    :type-initializer "gtk_notebook_get_type")
  ((enable-popup gtk-notebook-enable-popup
    "enable-popup" "gboolean" t t)
   (group gtk-notebook-group
    "group" "gpointer" t t)
   (group-id gtk-notebook-group-id
    "group-id" "gint" t t)
   (homogeneous gtk-notebook-homogeneous
    "homogeneous" "gboolean" t t)
   (page gtk-notebook-page
    "page" "gint" t t)
   (scrollable gtk-notebook-scrollable
    "scrollable" "gboolean" t t)
   (show-border gtk-notebook-show-border
    "show-border" "gboolean" t t)
   (show-tabs gtk-notebook-show-tabs
    "show-tabs" "gboolean" t t)
   (tab-border gtk-notebook-tab-border
    "tab-border" "guint" nil t)
   (tab-hborder gtk-notebook-tab-hborder
    "tab-hborder" "guint" t t)
   (tab-pos gtk-notebook-tab-pos
    "tab-pos" "GtkPositionType" t t)
   (tab-vborder gtk-notebook-tab-vborder
    "tab-vborder" "guint" t t)))

;;; ---------------------------------------------------------------------------- 
;;; gtk_notebook_new ()
;;; 
;;; GtkWidget * gtk_notebook_new (void);
;;; 
;;; Creates a new GtkNotebook widget with no pages.
;;; 
;;; Returns :
;;;     the newly created GtkNotebook
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page ()
;;; 
;;; gint gtk_notebook_append_page (GtkNotebook *notebook,
;;;                                GtkWidget *child,
;;;                                GtkWidget *tab_label);
;;; 
;;; Appends a page to notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'. [allow-none]
;;; 
;;; Returns :
;;;     the index (starting from 0) of the appended page in the notebook,
;;;     or -1 if function fails
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page" gtk-notebook-append-page) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu ()
;;; 
;;; gint gtk_notebook_append_page_menu (GtkNotebook *notebook,
;;;                                     GtkWidget *child,
;;;                                     GtkWidget *tab_label,
;;;                                     GtkWidget *menu_label);
;;; 
;;; Appends a page to notebook, specifying the widget to use as the label in
;;; the popup menu.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'. [allow-none]
;;; 
;;; menu_label :
;;;     the widget to use as a label for the page-switch menu, if that is
;;;     enabled. If NULL, and tab_label is a GtkLabel or NULL, then the menu
;;;     label will be a newly created label with the same text as tab_label; if
;;;     tab_label is not a GtkLabel, menu_label must be specified if the
;;;     page-switch menu is to be used. [allow-none]
;;; 
;;; Returns :
;;;     the index (starting from 0) of the appended page in the notebook,
;;;     or -1 if function fails
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page_menu" gtk-notebook-append-page-menu) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object))

(export 'gtk-notebook-append-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page ()
;;; 
;;; gint gtk_notebook_prepend_page (GtkNotebook *notebook,
;;;                                 GtkWidget *child,
;;;                                 GtkWidget *tab_label);
;;; 
;;; Prepends a page to notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'. [allow-none]
;;; 
;;; Returns :
;;;     the index (starting from 0) of the prepended page in the notebook,
;;;     or -1 if function fails
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page" gtk-notebook-prepend-page) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu ()
;;; 
;;; gint gtk_notebook_prepend_page_menu (GtkNotebook *notebook,
;;;                                      GtkWidget *child,
;;;                                      GtkWidget *tab_label,
;;;                                      GtkWidget *menu_label);
;;; 
;;; Prepends a page to notebook, specifying the widget to use as the label in
;;; the popup menu.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'. [allow-none]
;;; 
;;; menu_label :
;;;     the widget to use as a label for the page-switch menu, if that is
;;;     enabled. If NULL, and tab_label is a GtkLabel or NULL, then the menu
;;;     label will be a newly created label with the same text as tab_label;
;;;     if tab_label is not a GtkLabel, menu_label must be specified if the
;;;     page-switch menu is to be used. [allow-none]
;;; 
;;; Returns :
;;;     the index (starting from 0) of the prepended page in the notebook,
;;;     or -1 if function fails
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page_menu" gtk-notebook-prepend-page-menu) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object))

(export 'gtk-notebook-prepend-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page ()
;;; 
;;; gint gtk_notebook_insert_page (GtkNotebook *notebook,
;;;                                GtkWidget *child,
;;;                                GtkWidget *tab_label,
;;;                                gint position);
;;; 
;;; Insert a page into notebook at the given position.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'.
;;; 
;;; position :
;;;     the index (starting at 0) at which to insert the page, or -1 to append
;;;     the page after all other pages
;;; 
;;; Returns :
;;;     the index (starting from 0) of the inserted page in the notebook,
;;;     or -1 if function fails
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_insert_page" gtk-notebook-insert-page) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (position :int))

(export 'gtk-notebook-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page_menu ()
;;; 
;;; gint gtk_notebook_insert_page_menu (GtkNotebook *notebook,
;;;                                     GtkWidget *child,
;;;                                     GtkWidget *tab_label,
;;;                                     GtkWidget *menu_label,
;;;                                     gint position);
;;; 
;;; Insert a page into notebook at the given position, specifying the widget
;;; to use as the label in the popup menu.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the GtkWidget to use as the contents of the page
;;; 
;;; tab_label :
;;;     the GtkWidget to be used as the label for the page, or NULL to use the
;;;     default label, 'page N'. [allow-none]
;;; 
;;; menu_label :
;;;     the widget to use as a label for the page-switch menu, if that is
;;;     enabled. If NULL, and tab_label is a GtkLabel or NULL, then the menu
;;;     label will be a newly created label with the same text as tab_label;
;;;     if tab_label is not a GtkLabel, menu_label must be specified if the
;;;     page-switch menu is to be used. [allow-none]
;;; 
;;; position :
;;;     the index (starting at 0) at which to insert the page, or -1 to append
;;;     the page after all other pages.
;;; 
;;; Returns :
;;;     the index (starting from 0) of the inserted page in the notebook
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_insert_page_menu" gtk-notebook-insert-page-menu) :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object)
  (position :int))

(export 'gtk-notebook-insert-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_remove_page ()
;;; 
;;; void gtk_notebook_remove_page (GtkNotebook *notebook, gint page_num);
;;; 
;;; Removes a page from the notebook given its index in the notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; page_num :
;;;     the index of a notebook page, starting from 0. If -1, the last page
;;;     will be removed.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_remove_page" %gtk-notebook-remove-page) :void
  (notebook g-object)
  (page-num :int))

(defun gtk-notebook-remove-page (notebook page-or-number)
  (%gtk-notebook-remove-page notebook
                  (etypecase page-or-number
                    (integer page-or-number)
                    (gtk-widget (gtk-notebook-page-num notebook page-or-number)))))

(export 'gtk-notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num ()
;;; 
;;; gint gtk_notebook_page_num (GtkNotebook *notebook, GtkWidget *child);
;;; 
;;; Finds the index of the page which contains the given child widget.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the index of the page containing child, or -1 if child is not in the
;;;     notebook
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_page_num" gtk-notebook-page-num) :int
  (notebook g-object)
  (child g-object))

(export 'gtk-notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page ()
;;; 
;;; void gtk_notebook_next_page (GtkNotebook *notebook);
;;; 
;;; Switches to the next page. Nothing happens if the current page is the
;;; last page.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_next_page" gtk-notebook-next-page) :void
  (notebook g-object))

(export 'gtk-notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page ()
;;; 
;;; void gtk_notebook_prev_page (GtkNotebook *notebook);
;;; 
;;; Switches to the previous page. Nothing happens if the current page is the
;;; first page.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prev_page" gtk-notebook-prev-page) :void
  (notebook g-object))

(export 'gtk-notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child ()
;;; 
;;; void gtk_notebook_reorder_child (GtkNotebook *notebook,
;;;                                  GtkWidget *child,
;;;                                  gint position);
;;; 
;;; Reorders the page containing child, so that it appears in position position.
;;; If position is greater than or equal to the number of children in the list
;;; or negative, child will be moved to the end of the list.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the child to move
;;; 
;;; position :
;;;     the new position, or -1 to move to the end
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_reorder_child" gtk-notebook-reorder-child) :void
  (notebook g-object)
  (child g-object)
  (position :int))

(export 'gtk-notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_pos ()
;;; 
;;; void gtk_notebook_set_tab_pos (GtkNotebook *notebook, GtkPositionType pos);
;;; 
;;; Sets the edge at which the tabs for switching pages in the notebook are
;;; drawn.
;;; 
;;; notebook :
;;;     a GtkNotebook.
;;; 
;;; pos :
;;;     the edge to draw the tabs at
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_show_tabs ()
;;; 
;;; void gtk_notebook_set_show_tabs (GtkNotebook *notebook, gboolean show_tabs)
;;; 
;;; Sets whether to show the tabs for the notebook or not.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; show_tabs :
;;;     TRUE if the tabs should be shown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_show_border ()
;;; 
;;; void gtk_notebook_set_show_border (GtkNotebook *notebook,
;;;                                    gboolean show_border);
;;; 
;;; Sets whether a bevel will be drawn around the notebook pages. This only has
;;; a visual effect when the tabs are not shown.
;;; See gtk_notebook_set_show_tabs().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; show_border :
;;;     TRUE if a bevel should be drawn around the notebook
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_scrollable ()
;;; 
;;; void gtk_notebook_set_scrollable (GtkNotebook *notebook,
;;;                                   gboolean scrollable);
;;; 
;;; Sets whether the tab label area will have arrows for scrolling if there are
;;; too many tabs to fit in the area.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; scrollable :
;;;     TRUE if scroll arrows should be added
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable ()
;;; 
;;; void gtk_notebook_popup_enable (GtkNotebook *notebook);
;;; 
;;; Enables the popup menu: if the user clicks with the right mouse button on
;;; the tab labels, a menu with all the pages will be popped up.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable ()
;;; 
;;; void gtk_notebook_popup_disable (GtkNotebook *notebook);
;;; 
;;; Disables the popup menu.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page ()
;;; 
;;; gint gtk_notebook_get_current_page (GtkNotebook *notebook);
;;; 
;;; Returns the page number of the current page.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     the index (starting from 0) of the current page in the notebook. If
;;;     the notebook has no pages, then -1 will be returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label ()
;;; 
;;; GtkWidget * gtk_notebook_get_menu_label (GtkNotebook *notebook,
;;;                                          GtkWidget *child);
;;; 
;;; Retrieves the menu label widget of the page containing child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a widget contained in a page of notebook
;;; 
;;; Returns :
;;;     the menu label, or NULL if the notebook page does not have a menu label
;;;     other than the default (the tab label).
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_menu_label" gtk-notebook-menu-label-widget) g-object
  (notebook g-object)
  (child g-object))

(export 'gtk-notebook-menu-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page ()
;;; 
;;; GtkWidget * gtk_notebook_get_nth_page (GtkNotebook *notebook,
;;;                                        gint page_num);
;;; 
;;; Returns the child widget contained in page number page_num.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; page_num :
;;;     the index of a page in the notebook, or -1 to get the last page
;;; 
;;; Returns :
;;;     the child widget, or NULL if page_num is out of bounds.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_nth_page" gtk-notebook-nth-page) g-object
  (notebook g-object)
  (page-num :int))

(export 'gtk-notebook-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages ()
;;; 
;;; gint gtk_notebook_get_n_pages (GtkNotebook *notebook);
;;; 
;;; Gets the number of pages in a notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     the number of pages in the notebook
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_n_pages" gtk-notebook-n-pages) :int
  (notebook g-object))

(export 'gtk-notebook-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label ()
;;; 
;;; GtkWidget * gtk_notebook_get_tab_label (GtkNotebook *notebook,
;;;                                         GtkWidget *child);
;;; 
;;; Returns the tab label widget for the page child. NULL is returned if child
;;; is not in notebook or if no tab label has specifically been set for child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the page
;;; 
;;; Returns :
;;;     the tab label
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_tab_label" gtk-notebook-tab-label-widget) g-object
  (notebook g-object)
  (child g-object))

(export 'gtk-notebook-tab-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label ()
;;; 
;;; void gtk_notebook_set_menu_label (GtkNotebook *notebook,
;;;                                   GtkWidget *child,
;;;                                   GtkWidget *menu_label);
;;; 
;;; Changes the menu label for the page containing child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the child widget
;;; 
;;; menu_label :
;;;     the menu label, or NULL for default.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_menu_label" gtk-notebook-set-menu-label-widget)
    :void
  (notebook g-object)
  (child g-object)
  (menu-label g-object))

(defun (setf notebook-menu-label-widget) (new-value notebook child)
  (gtk-notebook-set-menu-label-widget notebook child new-value)
  new-value)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label_text ()
;;; 
;;; void gtk_notebook_set_menu_label_text (GtkNotebook *notebook,
;;;                                        GtkWidget *child,
;;;                                        const gchar *menu_text);
;;; 
;;; Creates a new label and sets it as the menu label of child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the child widget
;;; 
;;; menu_text :
;;;     the label text
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_label ()
;;; 
;;; void gtk_notebook_set_tab_label (GtkNotebook *notebook,
;;;                                  GtkWidget *child,
;;;                                  GtkWidget *tab_label);
;;; 
;;; Changes the tab label for child. If NULL is specified for tab_label, then
;;; the page will have the label 'page N'.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the page
;;; 
;;; tab_label :
;;;     the tab label widget to use, or NULL for default tab label.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_tab_label" gtk-notebook-set-tab-label-widget) :void
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(defun (setf notebook-tab-label-widget) (new-value notebook child)
  (gtk-notebook-set-tab-label-widget notebook child new-value)
  new-value)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_label_text ()
;;; 
;;; void gtk_notebook_set_tab_label_text (GtkNotebook *notebook,
;;;                                       GtkWidget *child,
;;;                                       const gchar *tab_text);
;;; 
;;; Creates a new label and sets it as the tab label for the page containing
;;; child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the page
;;; 
;;; tab_text :
;;;     the label text
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_reorderable ()
;;; 
;;; void gtk_notebook_set_tab_reorderable (GtkNotebook *notebook,
;;;                                        GtkWidget *child,
;;;                                        gboolean reorderable);
;;; 
;;; Sets whether the notebook tab can be reordered via drag and drop or not.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a child GtkWidget
;;; 
;;; reorderable :
;;;     whether the tab is reorderable or not
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_detachable ()
;;; 
;;; void gtk_notebook_set_tab_detachable (GtkNotebook *notebook,
;;;                                       GtkWidget *child,
;;;                                       gboolean detachable);
;;; 
;;; Sets whether the tab can be detached from notebook to another notebook or
;;; widget.
;;; 
;;; Note that 2 notebooks must share a common group identificator (see
;;; gtk_notebook_set_group_name()) to allow automatic tabs interchange between
;;; them.
;;; 
;;; If you want a widget to interact with a notebook through DnD (i.e.: accept
;;; dragged tabs from it) it must be set as a drop destination and accept the
;;; target "GTK_NOTEBOOK_TAB". The notebook will fill the selection with a
;;; GtkWidget** pointing to the child widget that corresponds to the dropped
;;; tab.
;;; 
;;; static void
;;; on_drop_zone_drag_data_received (GtkWidget        *widget,
;;;                                  GdkDragContext   *context,
;;;                                  gint              x,
;;;                                  gint              y,
;;;                                  GtkSelectionData *selection_data,
;;;                                  guint             info,
;;;                                  guint             time,
;;;                                  gpointer          user_data)
;;; {
;;;   GtkWidget *notebook;
;;;   GtkWidget **child;
;;; 
;;;   notebook = gtk_drag_get_source_widget (context);
;;;   child = (void*) gtk_selection_data_get_data (selection_data);
;;; 
;;;   process_widget (*child);
;;;   gtk_container_remove (GTK_CONTAINER (notebook), *child);
;;; }
;;; 
;;; If you want a notebook to accept drags from other widgets, you will have
;;; to set your own DnD code to do it.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a child GtkWidget
;;; 
;;; detachable :
;;;     whether the tab is detachable or not
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text ()
;;; 
;;; const gchar * gtk_notebook_get_menu_label_text (GtkNotebook *notebook,
;;;                                                 GtkWidget *child);
;;; 
;;; Retrieves the text of the menu label for the page containing child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     the child widget of a page of the notebook.
;;; 
;;; Returns :
;;;     the text of the tab label, or NULL if the widget does not have a menu
;;;     label other than the default menu label, or the menu label widget is
;;;     not a GtkLabel. The string is owned by the widget and must not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_scrollable ()
;;; 
;;; gboolean gtk_notebook_get_scrollable (GtkNotebook *notebook);
;;; 
;;; Returns whether the tab label area has arrows for scrolling.
;;; See gtk_notebook_set_scrollable().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     TRUE if arrows for scrolling are present
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_show_border ()
;;; 
;;; gboolean gtk_notebook_get_show_border (GtkNotebook *notebook);
;;; 
;;; Returns whether a bevel will be drawn around the notebook pages.
;;; See gtk_notebook_set_show_border().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     TRUE if the bevel is drawn
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_show_tabs ()
;;; 
;;; gboolean gtk_notebook_get_show_tabs (GtkNotebook *notebook);
;;; 
;;; Returns whether the tabs of the notebook are shown.
;;; See gtk_notebook_set_show_tabs().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     TRUE if the tabs are shown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text ()
;;; 
;;; const gchar * gtk_notebook_get_tab_label_text (GtkNotebook *notebook,
;;;                                                GtkWidget *child);
;;; 
;;; Retrieves the text of the tab label for the page containing child.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a widget contained in a page of notebook
;;; 
;;; Returns :
;;;     the text of the tab label, or NULL if the tab label widget is not
;;;     a GtkLabel. The string is owned by the widget and must not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_pos ()
;;; 
;;; GtkPositionType gtk_notebook_get_tab_pos (GtkNotebook *notebook);
;;; 
;;; Gets the edge at which the tabs for switching pages in the notebook
;;; are drawn.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     the edge at which the tabs are drawn
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_reorderable ()
;;; 
;;; gboolean gtk_notebook_get_tab_reorderable (GtkNotebook *notebook,
;;;                                            GtkWidget *child);
;;; 
;;; Gets whether the tab can be reordered via drag and drop or not.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a child GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the tab is reorderable.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable ()
;;; 
;;; gboolean gtk_notebook_get_tab_detachable (GtkNotebook *notebook,
;;;                                           GtkWidget *child);
;;; 
;;; Returns whether the tab contents can be detached from notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; child :
;;;     a child GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the tab is detachable.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_hborder ()
;;; 
;;; guint16 gtk_notebook_get_tab_hborder (GtkNotebook *notebook);
;;; 
;;; Returns the horizontal width of a tab border.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     horizontal width of a tab border
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_vborder ()
;;; 
;;; guint16 gtk_notebook_get_tab_vborder (GtkNotebook *notebook);
;;; 
;;; Returns the vertical width of a tab border.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     vertical width of a tab border
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_current_page ()
;;; 
;;; void gtk_notebook_set_current_page (GtkNotebook *notebook, gint page_num);
;;; 
;;; Switches to the page number page_num.
;;; 
;;; Note that due to historical reasons, GtkNotebook refuses to switch to a
;;; page unless the child widget is visible. Therefore, it is recommended to
;;; show child widgets before adding them to a notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; page_num :
;;;     index of the page to switch to, starting from 0. If negative, the last
;;;     page will be used. If greater than the number of pages in the notebook,
;;;     nothing will be done.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_group_name ()
;;; 
;;; void gtk_notebook_set_group_name (GtkNotebook *notebook,
;;;                                   const gchar *group_name);
;;; 
;;; Sets a group name for notebook.
;;; 
;;; Notebooks with the same name will be able to exchange tabs via drag and
;;; drop. A notebook with a NULL group name will not be able to exchange tabs
;;; with any other notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; group_name :
;;;     the name of the notebook group, or NULL to unset it.
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_group_name ()
;;; 
;;; const gchar * gtk_notebook_get_group_name (GtkNotebook *notebook);
;;; 
;;; Gets the current group name for notebook.
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; Returns :
;;;     the group name, or NULL if none is set. [transfer none]
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_action_widget ()
;;; 
;;; void gtk_notebook_set_action_widget (GtkNotebook *notebook,
;;;                                      GtkWidget *widget,
;;;                                      GtkPackType pack_type);
;;; 
;;; Sets widget as one of the action widgets. Depending on the pack type the
;;; widget will be placed before or after the tabs. You can use a GtkBox if you
;;; need to pack more than one widget on the same side.
;;; 
;;; Note that action widgets are "internal" children of the notebook and thus
;;; not included in the list returned from gtk_container_foreach().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; pack_type :
;;;     pack type of the action widget
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_action_widget ()
;;; 
;;; GtkWidget * gtk_notebook_get_action_widget (GtkNotebook *notebook,
;;;                                             GtkPackType pack_type);
;;; 
;;; Gets one of the action widgets. See gtk_notebook_set_action_widget().
;;; 
;;; notebook :
;;;     a GtkNotebook
;;; 
;;; pack_type :
;;;     pack type of the action widget to receive
;;; 
;;; Returns :
;;;     The action widget with the given pack_type or NULL when this action
;;;     widget has not been set.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.notebook.lisp ------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.notebook.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; 
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
;;;   "arrow-spacing"                  gint                  : Read
;;;   "has-backward-stepper"           gboolean              : Read
;;;   "has-forward-stepper"            gboolean              : Read
;;;   "has-secondary-backward-stepper" gboolean              : Read
;;;   "has-secondary-forward-stepper"  gboolean              : Read
;;;   "initial-gap"                    gint                  : Read
;;;   "tab-curvature"                  gint                  : Read
;;;   "tab-overlap"                    gint                  : Read
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
;;; The "has-backward-stepper" property determines whether the standard backward
;;; arrow button is displayed.
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
;;; the ::page-removed signal is emitted in the notebook right after a page is
;;; removed from the notebook.
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

;;; ----------------------------------------------------------------------------

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

(export 'gtk-notebook-add-page)

;;; ----------------------------------------------------------------------------
;;; struct GtkNotebook
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkNotebook" 'gtk-notebook))

(define-g-object-class "GtkNotebook" gtk-notebook
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_notebook_get_type")
  ((enable-popup
    gtk-notebook-enable-popup
    "enable-popup" "gboolean" t t)
   (group-name
    gtk-notebook-group-name
    "group-name" "gchar" t t)
   (page
    gtk-notebook-page
    "page" "gint" t t)
   (scrollable
    gtk-notebook-scrollable
    "scrollable" "gboolean" t t)
   (show-border
    gtk-notebook-show-border
    "show-border" "gboolean" t t)
   (show-tabs
    gtk-notebook-show-tabs
    "show-tabs" "gboolean" t t)
   (tab-pos
    gtk-notebook-tab-pos
    "tab-pos" "GtkPositionType" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-notebook 'type)
 "@version{2013-3-17}
  @begin{short}
    The GtkNotebook widget is a GtkContainer whose children are pages that can
    be switched between using tab labels along one edge.
  @end{short}

  There are many configuration options for GtkNotebook. Among other things,
  you can choose on which edge the tabs appear (see
  gtk_notebook_set_tab_pos()), whether, if there are too many tabs to fit the
  notebook should be made bigger or scrolling arrows added (see
  gtk_notebook_set_scrollable()), and whether there will be a popup menu
  allowing the users to switch pages. (see gtk_notebook_popup_enable(),
  gtk_notebook_popup_disable())

  @subheading{GtkNotebook as GtkBuildable}
  The GtkNotebook implementation of the GtkBuildable interface supports
  placing children into tabs by specifying \"tab\" as the \"type\" attribute of
  a <child> element. Note that the content of the tab must be created before the
  tab can be filled. A tab child can be specified without specifying a <child>
  type attribute.

  To add a child widget in the notebooks action area, specify \"action-start\"
  or \"action-end\" as the \"type\" attribute of the <child> element.

  @b{Example.} A UI definition fragment with GtkNotebook
  @begin{pre}
 <object class=\"GtkNotebook\">
   <child>
     <object class=\"GtkLabel\" id=\"notebook-content\">
       <property name=\"label\">Content</property>
     </object>
   </child>
   <child type=\"tab\">
     <object class=\"GtkLabel\" id=\"notebook-tab\">
       <property name=\"label\">Tab</property>
     </object>
   </child>
 </object>
  @end{pre}

  @see-slot{gtk-notebook-enable-popup}
  @see-slot{gtk-notebook-group-name}
  @see-slot{gtk-notebook-page}
  @see-slot{gtk-notebook-scrollable}
  @see-slot{gtk-notebook-show-border}
  @see-slot{gtk-notebook-show-tabs}
  @see-slot{gtk-notebook-tab-pos}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-popup" 'gtk-notebook) 't)
 "The @code{\"enable-popup\"} property of type @code{:boolean}
  (Read / Write)@br{}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group-name" 'gtk-notebook) 't)
 "The @code{\"group-name\"} property of type @code{:string}
  (Read / Write)@br{}
  Group name for tab drag and drop. @br{}
  Default value: @code{nil}@br{}
  Since 2.24")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page" 'gtk-notebook) 't)
 "The @code{\"page\"} property of type @code{:int} (Read / Write)@br{}
  The index of the current page. @br{}
  Allowed values: >= @code{G_MAXULONG}@br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scrollable" 'gtk-notebook) 't)
 "The @code{\"scrollable\"} property of type @code{:boolean} (Read / Write)@br{}
  If @em{true}, scroll arrows are added if there are too many tabs to fit. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-border" 'gtk-notebook) 't)
 "The @code{\"show-border\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the border should be shown. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-tabs" 'gtk-notebook) 't)
 "The @code{\"show-tabs\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether tabs should be shown. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tab-pos" 'gtk-notebook) 't)
 "The @code{\"tab-pos\"} property of type @symbol{gtk-position-type}
  (Read / Write)@br{}
  Which side of the notebook holds the tabs. @br{}
  Default value: @code{:top}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-enable-popup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-enable-popup 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"enable-popup\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-group-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-group-name 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"group-name\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-page 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"page\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-scrollable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-scrollable 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"scrollable\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-show-border atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-show-border 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"show-border\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-show-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-show-tabs 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"show-tabs\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-tab-pos atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-tab-pos 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"tab-pos\"} of the @class{gtk-notebook}
  class.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-detachable
                       "detachable" "gboolean" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-menu-label
                       "menu-label" "gchararray" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-position
                       "position" "gint" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-reorderable
                       "reorderable" "gboolean" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-expand
                       "tab-expand" "gboolean" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-fill
                       "tab-fill" "gboolean" t t t)

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-label
                       "tab-label" "gchararray" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-detachable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-detachable 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"detachable\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-menu-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-menu-label 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"menu-label\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-position 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"position\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-reorderable 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"reorderable\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-expand 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"tab-expand\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-fill 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"tab-fill\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-label 'function)
 "@version{2013-3-17}
  Accessor of the child property @code{\"tab-label\"} of the
  @class{gtk-notebook} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-new))

(defun gtk-notebook-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @return{The newly created @class{gtk-notebook} widget}
  Creates a new @class{gtk-notebook} widget with no pages."
  (make-instance 'gtk-notebook))

(export 'gtk-notebook-new)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page" gtk-notebook-append-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, \"page N\"}
  @begin{return}
    The index (starting from 0) of the appended page in the notebook, or -1
    if function fails.
  @end{return}
  Appends a page to notebook."
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page_menu" gtk-notebook-append-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the GtkWidget to use as the contents of the page}
  @argument[tab-label]{the GtkWidget to be used as the label for the page, or
    NULL to use the default label, \"page N\"}
  @argument[menu-label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If NULL, and tab_label is a GtkLabel or NULL, then the
    menu label will be a newly created label with the same text as tab_label;
    if tab_label is not a GtkLabel, menu_label must be specified if the
    page-switch menu is to be used}
  @begin{return}
    The index (starting from 0) of the appended page in the notebook, or -1
    if function fails.
  @end{return}
  Appends a page to notebook, specifying the widget to use as the label in the
  popup menu."
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu-label g-object))

(export 'gtk-notebook-append-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page" gtk-notebook-prepend-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the GtkWidget to use as the contents of the page}
  @argument[tab_label]{the GtkWidget to be used as the label for the page, or
    NULL to use the default label, 'page N'}
  @begin{return}
    The index (starting from 0) of the prepended page in the notebook, or -1
    if function fails.
  @end{return}
  Prepends a page to notebook."
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page_menu" gtk-notebook-prepend-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the GtkWidget to use as the contents of the page}
  @argument[tab_label]{the GtkWidget to be used as the label for the page,
    or NULL to use the default label, 'page N'}
  @argument[menu_label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If NULL, and tab_label is a GtkLabel or NULL, then the
    menu label will be a newly created label with the same text as tab_label; if
    tab_label is not a GtkLabel, menu_label must be specified if the
    page-switch menu is to be used}
  @begin{return}
    The index (starting from 0) of the prepended page in the notebook, or -1
    if function fails.
  @end{return}
  Prepends a page to notebook, specifying the widget to use as the label in
  the popup menu."
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget))
  (menu-label (g-object gtk-widget)))

(export 'gtk-notebook-prepend-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_insert_page" gtk-notebook-insert-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the GtkWidget to use as the contents of the page}
  @argument[tab_label]{the GtkWidget to be used as the label for the page, or
    NULL to use the default label, 'page N'}
  @argument[position]{the index (starting at 0) at which to insert the page,
    or -1 to append the page after all other pages}
  @begin{return}
    The index (starting from 0) of the inserted page in the notebook, or -1
    if function fails.
  @end{return}
  Insert a page into notebook at the given position."
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (position :int))

(export 'gtk-notebook-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_insert_page_menu" gtk-notebook-insert-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the GtkWidget to use as the contents of the page}
  @argument[tab_label]{the GtkWidget to be used as the label for the page,
    or NULL to use the default label, 'page N'}
  @argument[menu_label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If NULL, and tab_label is a GtkLabel or NULL, then the
    menu label will be a newly created label with the same text as tab_label; if
    tab_label is not a GtkLabel, menu_label must be specified if the
    page-switch menu is to be used}
  @argument[position]{the index (starting at 0) at which to insert the page,
    or -1 to append the page after all other pages.}
  @return{the index (starting from 0) of the inserted page in the notebook}
  Insert a page into notebook at the given position, specifying the widget to
  use as the label in the popup menu."
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object)
  (position :int))

(export 'gtk-notebook-insert-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_remove_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_remove_page" %gtk-notebook-remove-page) :void
  (notebook g-object)
  (page-num :int))

(defun gtk-notebook-remove-page (notebook page-or-number)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[page_num]{the index of a notebook page, starting from 0. If -1, the
    last page will be removed.}
  Removes a page from the notebook given its index in the notebook."
  (%gtk-notebook-remove-page notebook
                  (etypecase page-or-number
                    (integer page-or-number)
                    (gtk-widget (gtk-notebook-page-num notebook
                                                       page-or-number)))))

(export 'gtk-notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_page_num" gtk-notebook-page-num) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a GtkWidget}
  @begin{return}
    The index of the page containing child, or -1 if child is not in the
    notebook.
  @end{return}
  Finds the index of the page which contains the given child widget."
  (notebook g-object)
  (child g-object))

(export 'gtk-notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_next_page" gtk-notebook-next-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  Switches to the next page. Nothing happens if the current page is the last
  page."
  (notebook g-object))

(export 'gtk-notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prev_page" gtk-notebook-prev-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  Switches to the previous page. Nothing happens if the current page is the
  first page."
  (notebook g-object))

(export 'gtk-notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_reorder_child" gtk-notebook-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the child to move}
  @argument[position]{the new position, or -1 to move to the end}
  Reorders the page containing child, so that it appears in position position.
  If position is greater than or equal to the number of children in the list
  or negative, child will be moved to the end of the list."
  (notebook g-object)
  (child g-object)
  (position :int))

(export 'gtk-notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_pos ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-tab-pos))

(defun gtk-notebook-set-tab-pos (notebook pos)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook.}
  @argument[pos]{the edge to draw the tabs at}
  Sets the edge at which the tabs for switching pages in the notebook are
  drawn."
  (setf (gtk-notebook-tab-pos notebook) pos))

(export 'gtk-notebook-set-tab-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_show_tabs ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-show-tabs))

(defun gtk-notebook-set-show-tabs (notebook show-tabs)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[show_tabs]{TRUE if the tabs should be shown}
  Sets whether to show the tabs for the notebook or not."
  (setf (gtk-notebook-show-tabs notebook) show-tabs))

(export 'gtk-notebook-set-show-tabs)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_show_border ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-show-border))

(defun gtk-notebook-set-show-border (notebook show-border)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[show_border]{TRUE if a bevel should be drawn around the notebook}
  Sets whether a bevel will be drawn around the notebook pages. This only has
  a visual effect when the tabs are not shown. See
  gtk_notebook_set_show_tabs()."
  (setf (gtk-notebook-show-border notebook) show-border))

(export 'gtk-notebook-set-show-border)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_scrollable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-scrollable))

(defun gtk-notebook-set-scrollable (notebook scrollable)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[scrollable]{TRUE if scroll arrows should be added}
 Sets whether the tab label area will have arrows for scrolling if there are
 too many tabs to fit in the area."
  (setf (gtk-notebook-scrollable notebook) scrollable))

(export 'gtk-notebook-set-scrollable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-enable))

(defun gtk-notebook-popup-enable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  Enables the popup menu: if the user clicks with the right mouse button on
  the tab labels, a menu with all the pages will be popped up."
  (setf (gtk-notebook-enable-popup notebook) t))

(export 'gtk-notebook-popup-enable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-disable))

(defun gtk-notebook-popup-disable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  Disables the popup menu."
  (setf (gtk-notebook-enable-popup notebook) nil))

(export 'gtk-notebook-popup-disable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_current_page" gtk-notebook-get-current-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @begin{return}
    The index (starting from 0) of the current page in the notebook. If the
    notebook has no pages, then -1 will be returned.
  @end{return}
  Returns the page number of the current page."
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-get-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-menu-label))

(defun gtk-notebook-get-menu-label (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a widget contained in a page of notebook}
  @begin{return}
    The menu label, or NULL if the notebook page does not have a menu label
    other than the default (the tab label).
  @end{return}
  Retrieves the menu label widget of the page containing child."
  (gtk-notebook-child-menu-label notebook child))

(export 'gtk-notebook-get-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_nth_page" gtk-notebook-nth-page) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[page_num]{the index of a page in the notebook, or -1 to get the
    last page}
  @return{The child widget, or NULL if page_num is out of bounds.}
  Returns the child widget contained in page number page_num."
  (notebook g-object)
  (page-num :int))

(export 'gtk-notebook-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_n_pages" gtk-notebook-n-pages) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{The number of pages in the notebook.}
  @short{Gets the number of pages in a notebook.}

  Since 2.2"
  (notebook g-object))

(export 'gtk-notebook-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-label))

(defun gtk-notebook-get-tab-label (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the page}
  @return{the tab label}
  Returns the tab label widget for the page child. NULL is returned if child
  is not in notebook or if no tab label has specifically been set for child."
  (gtk-notebook-child-tab-label notebook child))

(export 'gtk-notebook-tab-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-menu-label))

(defun gtk-notebook-set-menu-label (notebook child menu-label)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the child widget}
  @argument[menu_label]{the menu label, or NULL for default}
  Changes the menu label for the page containing child."
  (setf (gtk-notebook-child-menu-label notebook child) menu-label))

(export 'gtk-notebook-set-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_menu_label_text" gtk-notebook-set-menu-label-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the child widget}
  @argument[menu_text]{the label text}
  Creates a new label and sets it as the menu label of child."
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (menu-text :string))

(export 'gtk-notebook-set-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-tab-label))

(defun gtk-notebook-set-tab-label (notebook child tab-label)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the page}
  @argument[tab_label]{the tab label widget to use, or NULL for default tab
    label}
  Changes the tab label for child. If NULL is specified for tab_label, then
  the page will have the label 'page N'."
  (setf (gtk-notebook-child-tab-label notebook child) tab-label))

(export 'gtk-notebook-set-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_tab_label_text" gtk-notebook-set-tab-label-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the page}
  @argument[tab_text]{the label text}
  Creates a new label and sets it as the tab label for the page containing
  child."
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-text :string))

(export 'gtk-notebook-set-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-tab-reorderable))

(defun gtk-notebook-set-tab-reorderable (notebook child reorderable)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a child GtkWidget}
  @argument[reorderable]{whether the tab is reorderable or not}
  @begin{short}
    Sets whether the notebook tab can be reordered via drag and drop or not.
  @end{short}

  Since 2.10"
  (setf (gtk-notebook-child-reorderable notebook child) reorderable))

(export 'gtk-notebook-set-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_detachable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-tab-detachable))

(defun gtk-notebook-set-tab-detachable (notebook child detachable)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a child GtkWidget}
  @argument[detachable]{whether the tab is detachable or not}
  @begin{short}
    Sets whether the tab can be detached from notebook to another notebook or
    widget.
  @end{short}

  Note that 2 notebooks must share a common group identificator (see
  gtk_notebook_set_group_name()) to allow automatic tabs interchange between
  them.

  If you want a widget to interact with a notebook through DnD (i.e.: accept
  dragged tabs from it) it must be set as a drop destination and accept the
  target \"GTK_NOTEBOOK_TAB\". The notebook will fill the selection with a
  GtkWidget** pointing to the child widget that corresponds to the dropped
  tab.
  @begin{pre}
 static void
 on_drop_zone_drag_data_received (GtkWidget        *widget,
                                  GdkDragContext   *context,
                                  gint              x,
                                  gint              y,
                                  GtkSelectionData *selection_data,
                                  guint             info,
                                  guint             time,
                                  gpointer          user_data)
 {
   GtkWidget *notebook;
   GtkWidget **child;

   notebook = gtk_drag_get_source_widget (context);
   child = (void*) gtk_selection_data_get_data (selection_data);

   process_widget (*child);
   gtk_container_remove (GTK_CONTAINER (notebook), *child);
 @}
  @end{pre}
  If you want a notebook to accept drags from other widgets, you will have to
  set your own DnD code to do it.

  Since 2.10"
  (setf (gtk-notebook-child-detachable notebook child) detachable))

(export 'gtk-notebook-set-tab-detachable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_menu_label_text" gtk-notebook-get-menu-label-text)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{the child widget of a page of the notebook.}
  @begin{return}
    The text of the tab label, or NULL if the widget does not have a menu
    label other than the default menu label, or the menu label widget is not
    a GtkLabel. The string is owned by the widget and must not be freed.
  @end{return}
  Retrieves the text of the menu label for the page containing child."
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-get-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_scrollable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-scrollable))

(defun gtk-notebook-get-scrollable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{TRUE if arrows for scrolling are present}
  Returns whether the tab label area has arrows for scrolling. See
  gtk_notebook_set_scrollable()."
  (gtk-notebook-scrollable notebook))

(export 'gtk-notebook-get-scrollable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_show_border ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-show-border))

(defun gtk-notebook-get-show-border (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{TRUE if the bevel is drawn}
  Returns whether a bevel will be drawn around the notebook pages. See
  gtk_notebook_set_show_border()."
  (gtk-notebook-show-border notebook))

(export 'gtk-notebook-get-show-border)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_show_tabs ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-show-tabs))

(defun gtk-notebook-get-show-tabs (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{TRUE if the tabs are shown}
  Returns whether the tabs of the notebook are shown. See
  gtk_notebook_set_show_tabs()."
  (gtk-notebook-show-tabs notebook))

(export 'gtk-notebook-get-show-tabs)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_tab_label_text" gtk-notebook-get-tab-label-text)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a widget contained in a page of notebook}
  @begin{return}
    The text of the tab label, or NULL if the tab label widget is not a
    GtkLabel. The string is owned by the widget and must not be freed.
  @end{return}
  Retrieves the text of the tab label for the page containing child."
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-get-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_pos ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-pos))

(defun gtk-notebook-get-tab-pos (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{the edge at which the tabs are drawn}
  Gets the edge at which the tabs for switching pages in the notebook are
  drawn."
  (gtk-notebook-tab-pos notebook))

(export 'gtk-notebook-get-tab-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-reorderable))

(defun gtk-notebook-get-tab-reorderable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a child GtkWidget}
  @return{TRUE if the tab is reorderable.}
  @short{Gets whether the tab can be reordered via drag and drop or not.}

  Since 2.10"
  (gtk-notebook-child-reorderable notebook child))

(export 'gtk-notebook-get-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-detachable))

(defun gtk-notebook-get-tab-detachable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[child]{a child GtkWidget}
  @return{TRUE if the tab is detachable.}
  @short{Returns whether the tab contents can be detached from notebook.}

  Since 2.10"
  (gtk-notebook-child-detachable notebook child))

(export 'gtk-notebook-get-tab-detachable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_hborder ()
;;; 
;;; guint16 gtk_notebook_get_tab_hborder (GtkNotebook *notebook);
;;; 
;;; Warning
;;; 
;;; gtk_notebook_get_tab_hborder has been deprecated since version 3.4 and
;;; should not be used in newly-written code. this function returns zero
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
;;; Warning
;;; 
;;; gtk_notebook_get_tab_vborder has been deprecated since version 3.4 and
;;; should not be used in newly-written code. this function returns zero
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_current_page" gtk-notebook-set-current-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[page_num]{index of the page to switch to, starting from 0. If
    negative, the last page will be used. If greater than the number of pages
    in the notebook, nothing will be done.}
  @short{Switches to the page number page_num.}

  Note that due to historical reasons, GtkNotebook refuses to switch to a page
  unless the child widget is visible. Therefore, it is recommended to show
  child widgets before adding them to a notebook."
  (notebook (g-object gtk-notebook))
  (page-num :int))

(export 'gtk-notebook-set-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_group_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-group-name))

(defun gtk-notebook-set-group-name (notebook group-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[group_name]{the name of the notebook group, or NULL to unset it}
  @short{Sets a group name for notebook.}

  Notebooks with the same name will be able to exchange tabs via drag and
  drop. A notebook with a NULL group name will not be able to exchange tabs
  with any other notebook.

  Since 2.24"
  (setf (gtk-notebook-group-name notebook) group-name))

(export 'gtk-notebook-set-group-name)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_group_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-group-name))

(defun gtk-notebook-get-group-name (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @return{The group name, or NULL if none is set.}
  @short{Gets the current group name for notebook.}

  Since 2.24"
  (gtk-notebook-group-name notebook))

(export 'gtk-notebook-get-group-name)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_action_widget" gtk-notebook-set-action-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[widget]{a GtkWidget}
  @argument[pack_type]{pack type of the action widget}
  @begin{short}
    Sets widget as one of the action widgets. Depending on the pack type the
    widget will be placed before or after the tabs. You can use a GtkBox if you
    need to pack more than one widget on the same side.
  @end{short}

  Note that action widgets are \"internal\" children of the notebook and thus
  not included in the list returned from gtk_container_foreach().

  Since 2.20"
  (notebook (g-object gtk-notebook))
  (widget (g-object gtk-widget))
  (pack-type gtk-pack-type))

(export 'gtk-notebook-set-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_action_widget" gtk-notebook-get-action-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a GtkNotebook}
  @argument[pack_type]{pack type of the action widget to receive}
  @begin{return}
    The action widget with the given pack_type or NULL when this action
    widget has not been set.
  @end{return}
  @begin{short}
    Gets one of the action widgets. See gtk_notebook_set_action_widget().
  @end{short}

  Since 2.20"
  (notebook (g-object gtk-notebook))
  (pack-type gtk-pack-type))

(export 'gtk-notebook-get-action-widget)

;;; --- End of file gtk.notebook.lisp ------------------------------------------

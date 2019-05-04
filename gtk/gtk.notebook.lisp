;;; ----------------------------------------------------------------------------
;;; gtk.notebook.lisp
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
;;; GtkNotebook
;;;
;;;     A tabbed notebook container
;;;
;;; Types and Values
;;;
;;;     GtkNotebook
;;;
;;; Functions
;;;
;;;     gtk_notebook_new
;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page
;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child
;;;     gtk_notebook_set_tab_pos                           Accessor
;;;     gtk_notebook_set_show_tabs                         Accessor
;;;     gtk_notebook_set_show_border                       Accessor
;;;     gtk_notebook_set_scrollable                        Accessor
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
;;;     gtk_notebook_get_scrollable                        Accessor
;;;     gtk_notebook_get_show_border                       Accessor
;;;     gtk_notebook_get_show_tabs                         Accessor
;;;     gtk_notebook_get_tab_label_text
;;;     gtk_notebook_get_tab_pos                           Accessor
;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_get_tab_hborder                       deprecated
;;;     gtk_notebook_get_tab_vborder                       deprecated
;;;     gtk_notebook_set_current_page
;;;     gtk_notebook_set_group_name                        Accessor
;;;     gtk_notebook_get_group_name                        Accessor
;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget
;;;
;;; Properties
;;;
;;;            gboolean  enable-popup  Read / Write
;;;             gchar *  group-name    Read / Write
;;;                gint  page          Read / Write
;;;            gboolean  scrollable    Read / Write
;;;            gboolean  show-border   Read / Write
;;;            gboolean  show-tabs     Read / Write
;;;     GtkPositionType  tab-pos       Read / Write
;;;
;;; Child Properties
;;;
;;;            gboolean  detachable    Read / Write
;;;             gchar *  menu-label    Read / Write
;;;                gint  position      Read / Write
;;;            gboolean  reorderable   Read / Write
;;;            gboolean  tab-expand    Read / Write
;;;            gboolean  tab-fill      Read / Write
;;;             gchar *  tab-label     Read / Write
;;;
;;; Style Properties
;;;
;;;                gint  arrow-spacing                   Read
;;;            gboolean  has-backward-stepper            Read
;;;            gboolean  has-forward-stepper             Read
;;;            gboolean  has-secondary-backward-stepper  Read
;;;            gboolean  has-secondary-forward-stepper   Read
;;;            gboolean  has-tab-gap                     Read
;;;                gint  initial-gap                     Read
;;;                gint  tab-curvature                   Read
;;;                gint  tab-overlap                     Read
;;;
;;; Signals
;;;
;;;            gboolean  change-current-page  Action
;;;        GtkNotebook*  create-window        Run Last
;;;            gboolean  focus-tab            Action
;;;                void  move-focus-out       Action
;;;                void  page-added           Run Last
;;;                void  page-removed         Run Last
;;;                void  page-reordered       Run Last
;;;            gboolean  reorder-tab	       Action
;;;            gboolean  select-page	       Action
;;;                void  switch-page          Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkNotebook
;;;
;;; Implemented Interfaces
;;;
;;;     GtkNotebook implements AtkImplementorIface and GtkBuildable.
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
    "group-name" "gchararray" t t)
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-notebook 'type)
 "@version{2014-8-20}
  @begin{short}
    The @sym{gtk-notebook} container is a @class{gtk-container} whose children
    are pages that can be switched between using tab labels along one edge.
  @end{short}

  @image[notebook]{}

  There are many configuration options for @sym{gtk-notebook}. Among other
  things, you can choose on which edge the tabs appear, see the function
  @fun{gtk-notebook-tab-pos}, whether, if there are too many tabs to fit
  the notebook should be made bigger or scrolling arrows added, see the
  function @fun{gtk-notebook-scrollable}, and whether there will be a popup
  menu allowing the users to switch pages, see the functions
  @fun{gtk-notebook-popup-enable} and @fun{gtk-notebook-popup-disable}.
  @begin[GtkNotebook as GtkBuildable]{dictionary}
    The @sym{gtk-notebook} implementation of the @class{gtk-buildable}
    interface supports placing children into tabs by specifying \"tab\" as the
    \"type\" attribute of a <child> element. Note that the content of the tab
    must be created before the tab can be filled. A tab child can be specified
    without specifying a <child> type attribute.

    To add a child widget in the notebooks action area, specify \"action-start\"
    or \"action-end\" as the \"type\" attribute of the <child> element.

    @b{Example:} A UI definition fragment with @sym{gtk-notebook}
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
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
  notebook
  ├── header.top
  │   ├── [<action widget>]
  │   ├── tabs
  │   │   ├── [arrow]
  │   │   ├── tab
  │   │   │   ╰── <tab label>
  ┊   ┊   ┊
  │   │   ├── tab[.reorderable-page]
  │   │   │   ╰── <tab label>
  │   │   ╰── [arrow]
  │   ╰── [<action widget>]
  │
  ╰── stack
      ├── <child>
      ┊
      ╰── <child>
    @end{pre}
    @sym{gtk-notebook} has a main CSS node with name @code{notebook}, a subnode
    with name @code{header} and below that a subnode with name @code{tabs} which
    contains one subnode per tab with name @code{tab}.

    If action widgets are present, their CSS nodes are placed next to the tabs
    node. If the notebook is scrollable, CSS nodes with name @code{arrow} are
    placed as first and last child of the tabs node.

    The main node gets the @code{.frame} style class when the notebook has a
    border, see the @fun{gtk-notebook-show-border} function.

    The header node gets one of the style class @code{.top}, @code{.bottom},
    @code{.left} or @code{.right}, depending on where the tabs are placed. For
    reorderable pages, the tab node gets the @code{.reorderable-page} class.

    A tab node gets the @code{.dnd} style class while it is moved with
    drag-and-drop.

    The nodes are always arranged from left-to-right, regarldess of text
    direction.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[detachable]{entry}
        The @code{detachable} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the tab is detachable. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[menu-label]{entry}
        The @code{menu-label} child property of type @code{:string}
        (Read / Write) @br{}
        The string displayed in the child's menu entry. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int}
        (Read / Write) @br{}
        The index of the child in the parent. @br{}
        Allowed values: >= @code{G_MAXULONG} @br{}
        Default value: 0
      @end{entry}
      @begin[reorderable]{entry}
        The @code{reorderable} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the tab is reorderable by user action. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[tab-expand]{entry}
        The @code{tab-expand} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether to expand the child's tab. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[tab-fill]{entry}
        The @code{tab-fill} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the child's tab should fill the allocated area. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[tab-label]{entry}
        The @code{tab-label} child property of type @code{:string}
        (Read / Write) @br{}
        The string displayed on the child's tab label. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-spacing]{entry}
        The @code{arrow-spacing} style property of type @code{:int} (Read) @br{}
        The @code{arrow-spacing} property defines the spacing between the
        scroll arrows and the tabs. @br{}
        @em{Warning:} @code{arrow-spacing} has been deprecated since version
        3.20 and should not be used in newly-written code.
        This property is ignored. Use margins on arrows or the \"tabs\" node to
        achieve the same effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[has-backward-stepper]{entry}
        The @code{has-backward-stepper} style property of type @code{:boolean}
        (Read) @br{}
        The @code{has-backward-stepper} property determines whether the
        standard backward arrow button is displayed. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-forward-stepper]{entry}
        The @code{has-forward-stepper} style property of type @code{:boolean}
        (Read) @br{}
        The @code{has-forward-stepper} property determines whether the
        standard forward arrow button is displayed. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-secondary-backward-stepper]{entry}
        The @code{has-secondary-backward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        The @code{has-secondary-backward-stepper} property determines whether
        a second backward arrow button is displayed on the opposite end of the
        tab area. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[has-secondary-forward-stepper]{entry}
        The @code{has-secondary-forward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        The @code{has-secondary-forward-stepper} property determines whether
        a second forward arrow button is displayed on the opposite end of the
        tab area. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[has-tab-gap]{entry}
        The @code{has-tab-gap} style property of type @code{:boolean}
        (Read) @br{}
        The @code{has-tab-gap} property defines whether the active tab is draw
        with a gap at the bottom. When @em{true} the theme engine uses
        @fun{gtk-render-extension} to draw the active tab. When @code{nil}
        @fun{gtk-render-background} and @fun{gtk-render-frame} are used. @br{}
        @em{Warning:} @code{has-tab-gap} has been deprecated since version 3.20
        and should not be used in newly-written code.
        This function always behaves as if it was set to @code{nil}. @br{}
        Default value: @em{true} @br{}
        Since 3.12
      @end{entry}
      @begin[initial-gap]{entry}
        The @code{initial-gap} style property of type @code{:int} (Read) @br{}
        The @code{initial-gap} property defines the minimum size for the
        initial gap between the first tab. @br{}
        @em{Warning:} @code{initial-gap} has been deprecated since version 3.20
        and should not be used in newly-written code.
        The intial gap is ignored. Use margins on the header node to achieve the
        same effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[tab-curvature]{entry}
        The @code{tab-curvature} style property of type @code{:int} (Read) @br{}
        The @code{tab-curvature} property defines size of tab curvature. @br{}
        @em{Warning:} @code{tab-curvature} has been deprecated since version
        3.20 and should not be used in newly-written code.
        This property is ignored. Use margins on tab nodes to achieve the same
        effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[tab-overlap]{entry}
        The @code{tab-overlap} style property of type @code{:int} (Read) @br{}
        The @code{tab-overlap} property defines size of tab overlap area. @br{}
        @em{Warning;} @code{tab-overlap} has been deprecated since version 3.20
        and should not be used in newly-written code.
        This property is ignored. Use margins on tab nodes to achieve the same
        effect. @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
 lambda (notebook arg1)   : Action
      @end{pre}

    @subheading{The \"create-window\" signal}
      @begin{pre}
 lambda (notebook page x y)   : Run Last
      @end{pre}
      The \"create-window\" signal is emitted when a detachable tab is dropped
      on the root window.
      A handler for this signal can create a window containing a notebook where
      the tab will be attached. It is also responsible for moving/resizing the
      window and adding the necessary properties to the notebook, e. g. the
      \"group\".
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} emitting the signal.}
        @entry[page]{The tab of notebook that is being detached.}
        @entry[x]{The x coordinate where the drop happens.}
        @entry[y]{The y coordinate where the drop happens.}
        @entry[Returns]{A @sym{gtk-notebook} that page should be added to,
          or @code{nil}.}
      @end{table}
    @subheading{The \"focus-tab\" signal}
      @begin{pre}
 lambda (notebook arg1)   : Action
      @end{pre}

    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
 lambda (notebook arg1)   : Action
      @end{pre}

    @subheading{The \"page-added\" signal}
      @begin{pre}
 lambda (notebook child page-num)   : Run Last
      @end{pre}
      The \"page-added\" signal is emitted in the notebook right after a page
      is added to the notebook.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook}.}
        @entry[child]{The child @class{gtk-widget} affected.}
        @entry[page-num]{The new page number for child.}
      @end{table}
    @subheading{The \"page-removed\" signal}
      @begin{pre}
 lambda (notebook child page-num)   : Run Last
      @end{pre}
      The \"page-removed\" signal is emitted in the notebook right after a page
      is removed from the notebook.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook}.}
        @entry[child]{The child @class{gtk-widget} affected.}
        @entry[page-num]{The child page number.}
      @end{table}
    @subheading{The \"page-reordered\" signal}
      @begin{pre}
 lambda (notebook child page-num)   : Run Last
      @end{pre}
      The \"page-reordered\" signal is emitted in the notebook right after a
      page has been reordered.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook}.}
        @entry[child]{The child @class{gtk-widget} affected.}
        @entry[page-num]{The new page number for child.}
      @end{table}
    @subheading{The \"reorder-tab\" signal}
      @begin{pre}
 lambda (notebook arg1 arg2)   : Action
      @end{pre}

    @subheading{The \"select-page\" signal}
      @begin{pre}
 lambda (notebook arg1)   : Action
      @end{pre}

    @subheading{The \"switch-page\" signal}
      @begin{pre}
 lambda (notebook page page-num)   : Run Last
      @end{pre}
      Emitted when the user or a function changes the current page.
      @begin[code]{table}
        @entry[notebook]{The object which received the signal.}
        @entry[page]{The new current page.}
        @entry[page-num]{The index of the page.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-notebook-enable-popup}
  @see-slot{gtk-notebook-group-name}
  @see-slot{gtk-notebook-page}
  @see-slot{gtk-notebook-scrollable}
  @see-slot{gtk-notebook-show-border}
  @see-slot{gtk-notebook-show-tabs}
  @see-slot{gtk-notebook-tab-pos}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-notebook-enable-popup ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-popup" 'gtk-notebook) 't)
 "The @code{enable-popup} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-enable-popup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-enable-popup 'function)
 "@version{2014-8-20}
  Accessor of the @slot[gtk-notebook]{enable-popup} slot of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-popup-enable}
  @see-function{gtk-notebook-popup-disable}")

;;; --- gtk-notebook-group-name ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group-name" 'gtk-notebook) 't)
 "The @code{group-name} property of type @code{:string}
  (Read / Write) @br{}
  Group name for tab drag and drop. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-group-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-group-name 'function)
 "@version{2014-8-20}
  @argument[object]{a @class{gtk-notebook} container}
  @argument[group-name]{the name of the notebook group, or @code{nil} to unset
    it}
  @syntax[]{(gtk-notebook-group-name object) => group-name}
  @syntax[]{(setf (gtk-notebook-group-name object) group-name)}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{group-name} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The generic function @sym{gtk-notebook-group-name} gets the current group
  name for the notebook.

  The generic function @sym{(setf gtk-notebook-group-name)} sets a group name
  for notebook.

  Notebooks with the same name will be able to exchange tabs via drag and
  drop. A notebook with a @code{nil} group name will not be able to exchange
  tabs with any other notebook.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-page ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page" 'gtk-notebook) 't)
 "The @code{page} property of type @code{:int} (Read / Write) @br{}
  The index of the current page. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-page 'function)
 "@version{2014-8-20}
  Accessor of the @slot[gtk-notebook]{page} slot of the @class{gtk-notebook}
  class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-scrollable ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scrollable" 'gtk-notebook) 't)
 "The @code{scrollable} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, scroll arrows are added if there are too many tabs to fit. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-scrollable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-scrollable 'function)
 "@version{2014-8-20}
  @argument[object]{a @class{gtk-notebook} container}
  @argument[scrollable]{@em{true} if scroll arrows should be added}
  @syntax[]{(gtk-notebook-scrollable object) => scrollable}
  @syntax[]{(setf (gtk-notebook-scrollable object) scrollable)}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{scrollable} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The generic function @sym{gtk-notebook-scrollable} returns whether the tab
  label area has arrows for scrolling.

  The generic function @sym{(setf gtk-notebook-scrollable)} sets whether the
  tab label area will have arrows for scrolling if there are too many tabs to
  fit in the area.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-show-border -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-border" 'gtk-notebook) 't)
 "The @code{show-border} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the border should be shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-show-border atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-show-border 'function)
 "@version{2014-8-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[show-border]{@em{true} if a bevel should be drawn around the
    notebook}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{show-border} slot of the
    @class{gtk-notebook} class.
  @end{short}

  @return{@em{True} if the bevel is drawn.}

  The generic function @sym{gtk-notebook-show-border} returns whether a bevel
  will be drawn around the notebook pages.

  The generic function @sym{(setf gtk-notebook-show-border)} sets whether a
  bevel will be drawn around the notebook pages. This only has a visual effect
  when the tabs are not shown. See the generic function
  @fun{gtk-notebook-show-tabs}.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-show-tabs}")

;;; --- gtk-notebook-show-tabs -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-tabs" 'gtk-notebook) 't)
 "The @code{show-tabs} property of type @code{:boolean} (Read / Write) @br{}
  Whether tabs should be shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-show-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-show-tabs 'function)
 "@version{2014-8-20}
  @argument[object]{a @class{gtk-notebook} container}
  @argument[show-tabs]{@em{true} if the tabs should be shown}
  @syntax[]{(gtk-notebook-show-tabs object) => show-tabs}
  @syntax[]{(setf (gtk-notebook-show-tabs object) show-tabs)}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{show-tabs} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The generic function @sym{gtk-notebook-show-tabs} returns whether the tabs of
  the notebook are shown.

  The generic function @sym{(setf gtk-notebook-show-tabs)} sets whether to show
  the tabs for the notebook or not.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-tab-pos ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tab-pos" 'gtk-notebook) 't)
 "The @code{tab-pos} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  Which side of the notebook holds the tabs. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-tab-pos atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-tab-pos 'function)
 "@version{2014-8-20}
  @argument[object]{a @class{gtk-notebook} container}
  @argument[pos]{the edge to draw the tabs at}
  @syntax[]{(gtk-notebook-tab-pos object) => pos}
  @syntax[]{(setf (gtk-notebook-tab-pos object) pos)}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{tab-pos} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The generic function @sym{gtk-notebook-tab-pos} gets the edge at which the
  tabs for switching pages in the notebook are drawn.

  The generic function @sym{(setf gtk-notebook-tab-pos)} sets the edge at which
  the tabs for switching pages in the notebook are drawn.
  @see-class{gtk-notebook}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-notebook-child-detachable ------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-detachable
                       "detachable" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-detachable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-detachable 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{detachable} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtl-notebook-child-menu-label ------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-menu-label
                       "menu-label" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-menu-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-menu-label 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{menu-label} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-child-position --------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-position 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{position} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-child-reorderable -----------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-reorderable
                       "reorderable" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-reorderable 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{reorderable} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-child-tab-expand ------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-expand
                       "tab-expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-expand 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{tab-expand} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-child-tab-fill --------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-fill
                       "tab-fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-fill 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{tab-fill} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-child-tab-level -------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-label
                       "tab-label" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-label 'function)
 "@version{2013-9-10}
  Accessor of the child property @code{tab-label} of the
  @class{gtk-notebook} class.
  @see-class{gtk-notebook}")

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-new))

(defun gtk-notebook-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-8-20}
  @return{The newly created @class{gtk-notebook} container.}
  Creates a new @class{gtk-notebook} container with no pages.
  @see-class{gtk-notebook}"
  (make-instance 'gtk-notebook))

(export 'gtk-notebook-new)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page" gtk-notebook-append-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-8-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, \"page N\"}
  @begin{return}
    The index starting from 0 of the appended page in the notebook, or -1
    if function fails.
  @end{return}
  Appends a page to notebook.
  @see-class{gtk-notebook}"
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page_menu" gtk-notebook-append-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-8-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, \"page N\"}
  @argument[menu-label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If @code{nil}, and @arg{tab-label} is a
    @class{gtk-label} or @code{nil}, then the menu label will be a newly created
    label with the same text as @arg{tab-label}; if @arg{tab-label} is not a
    @class{gtk-label}, @arg{menu-label} must be specified if the page-switch
    menu is to be used}
  @begin{return}
    The index starting from 0 of the appended page in the notebook, or -1
    if function fails.
  @end{return}
  Appends a page to notebook, specifying the widget to use as the label in the
  popup menu.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-append-page}"
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
 "@version{2013-5-18}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, 'page N'}
  @begin{return}
    The index (starting from 0) of the prepended page in the notebook, or -1
    if function fails.
  @end{return}
  Prepends a page to notebook.
  @see-class{gtk-notebook}"
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(export 'gtk-notebook-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page_menu" gtk-notebook-prepend-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, 'page N'}
  @argument[menu-label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If @code{nil}, and @arg{tab-label} is a
    @class{gtk-label} or @code{nil}, then the menu label will be a newly created
    label with the same text as @arg{tab-label}; if @arg{tab-label} is not a
    @class{gtk-label}, @arg{menu-label} must be specified if the page-switch
    menu is to be used}
  @begin{return}
    The index (starting from 0) of the prepended page in the notebook, or -1
    if function fails.
  @end{return}
  Prepends a page to notebook, specifying the widget to use as the label in
  the popup menu.
  @see-class{gtk-notebook}"
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
 "@version{2013-5-18}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, 'page N'}
  @argument[position]{the index (starting at 0) at which to insert the page,
    or -1 to append the page after all other pages}
  @begin{return}
    The index (starting from 0) of the inserted page in the notebook, or -1
    if function fails.
  @end{return}
  Insert a page into notebook at the given position.
  @see-class{gtk-notebook}"
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
 "@version{2013-5-18}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the @class{gtk-widget} to use as the contents of the page}
  @argument[tab-label]{the @class{gtk-widget} to be used as the label for the
    page, or @code{nil} to use the default label, 'page N'}
  @argument[menu-label]{the widget to use as a label for the page-switch menu,
    if that is enabled. If @code{nil}, and @arg{tab-label} is a
    @class{gtk-label} or @code{nil}, then the menu label will be a newly created
    label with the same text as @arg{tab-label}; if @arg{tab-label} is not a
    @arg{gtk-label}, @arg{menu-label} must be specified if the page-switch menu
    is to be used}
  @argument[position]{the index (starting at 0) at which to insert the page,
    or -1 to append the page after all other pages}
  @return{The index (starting from 0) of the inserted page in the
    @arg{notebook}.}
  Insert a page into @arg{notebook} at the given position, specifying the widget
  to use as the label in the popup menu.
  @see-class{gtk-notebook}"
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
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[page_num]{the index of a notebook page, starting from 0. If -1, the
    last page will be removed}
  Removes a page from the notebook given its index in the notebook.
  @see-class{gtk-notebook}"
  (%gtk-notebook-remove-page notebook
                  (etypecase page-or-number
                    (integer page-or-number)
                    (gtk-widget (gtk-notebook-page-num notebook
                                                       page-or-number)))))

(export 'gtk-notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_detach_tab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_detach_tab" gtk-notebook-detach-tab) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-3-16}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a @class{gtk-widget} object}
  @begin{short}
    Removes the child from the notebook.
  @end{short}

  This function is very similar to @fun{gtk-container-remove}, but additionally
  informs the notebook that the removal is happening as part of a tab DND
  operation, which should not be cancelled.

  Since 3.16
  @see-class{gtk-notebook}
  @see-function{gtk-container-remove}"
  (notebook g-object)
  (child (g-object gtk-widget)))

(export 'gtk-notebook-detach-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_page_num" gtk-notebook-page-num) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a @class{gtk-widget} object}
  @begin{return}
    The index of the page containing child, or -1 if child is not in the
    notebook.
  @end{return}
  Finds the index of the page which contains the given child widget.
  @see-class{gtk-notebook}"
  (notebook g-object)
  (child g-object))

(export 'gtk-notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_next_page" gtk-notebook-next-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  Switches to the next page. Nothing happens if the current page is the last
  page.
  @see-class{gtk-notebook}"
  (notebook g-object))

(export 'gtk-notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prev_page" gtk-notebook-prev-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  Switches to the previous page. Nothing happens if the current page is the
  first page.
  @see-class{gtk-notebook}"
  (notebook g-object))

(export 'gtk-notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_reorder_child" gtk-notebook-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the child to move}
  @argument[position]{the new position, or -1 to move to the end}
  Reorders the page containing child, so that it appears in position position.
  If position is greater than or equal to the number of children in the list
  or negative, child will be moved to the end of the list.
  @see-class{gtk-notebook}"
  (notebook g-object)
  (child g-object)
  (position :int))

(export 'gtk-notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-enable))

(defun gtk-notebook-popup-enable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  Enables the popup menu: if the user clicks with the right mouse button on
  the tab labels, a menu with all the pages will be popped up.
  @see-class{gtk-notebook}"
  (setf (gtk-notebook-enable-popup notebook) t))

(export 'gtk-notebook-popup-enable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-disable))

(defun gtk-notebook-popup-disable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  Disables the popup menu.
  @see-class{gtk-notebook}"
  (setf (gtk-notebook-enable-popup notebook) nil))

(export 'gtk-notebook-popup-disable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_current_page" gtk-notebook-get-current-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @begin{return}
    The index (starting from 0) of the current page in the notebook. If the
    notebook has no pages, then -1 will be returned.
  @end{return}
  Returns the page number of the current page.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-get-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-menu-label))

(defun gtk-notebook-get-menu-label (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a widget contained in a page of notebook}
  @begin{return}
    The menu label, or @code{nil} if the notebook page does not have a menu
    label other than the default (the tab label).
  @end{return}
  Retrieves the menu label widget of the page containing child.
  @see-class{gtk-notebook}"
  (gtk-notebook-child-menu-label notebook child))

(export 'gtk-notebook-get-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_nth_page" gtk-notebook-get-nth-page) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[page-num]{the index of a page in the notebook, or -1 to get the
    last page}
  @return{The child widget, or @code{nil} if @arg{page-num} is out of bounds.}
  Returns the child widget contained in page number @arg{page-num}.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook))
  (page-num :int))

(export 'gtk-notebook-get-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_n_pages" gtk-notebook-get-n-pages) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @return{The number of pages in the notebook.}
  @short{Gets the number of pages in a notebook.}
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-get-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-label))

(defun gtk-notebook-get-tab-label (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the page}
  @return{The tab label.}
  Returns the tab label widget for the page @arg{child}. @code{nil} is returned
  if @arg{child} is not in @arg{notebook} or if no tab label has specifically
  been set for @arg{child}.
  @see-class{gtk-notebook}"
  (gtk-notebook-child-tab-label notebook child))

(export 'gtk-notebook-get-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-menu-label))

(defun gtk-notebook-set-menu-label (notebook child menu-label)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the child widget}
  @argument[menu-label]{the menu label, or @code{nil} for default}
  Changes the menu label for the page containing @arg{child}.
  @see-class{gtk-notebook}"
  (setf (gtk-notebook-child-menu-label notebook child) menu-label))

(export 'gtk-notebook-set-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_menu_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_menu_label_text" gtk-notebook-set-menu-label-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the child widget}
  @argument[menu-text]{the label text}
  Creates a new label and sets it as the menu label of @arg{child}.
  @see-class{gtk-notebook}"
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
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the page}
  @argument[tab-label]{the tab label widget to use, or @code{nil} for default
    tab label}
  Changes the tab label for @arg{child}. If @code{nil} is specified for
  @arg{tab-label}, then the page will have the label 'page N'.
  @see-class{gtk-notebook}"
  (setf (gtk-notebook-child-tab-label notebook child) tab-label))

(export 'gtk-notebook-set-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_tab_label_text" gtk-notebook-set-tab-label-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the page}
  @argument[tab-text]{the label text}
  Creates a new label and sets it as the tab label for the page containing
  child.
  @see-class{gtk-notebook}"
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
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a child @class{gtk-widget}}
  @argument[reorderable]{whether the tab is reorderable or not}
  @begin{short}
    Sets whether the @arg{notebook} tab can be reordered via drag and drop or
    not.
  @end{short}
  @see-class{gtk-notebook}"
  (setf (gtk-notebook-child-reorderable notebook child) reorderable))

(export 'gtk-notebook-set-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_detachable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-set-tab-detachable))

(defun gtk-notebook-set-tab-detachable (notebook child detachable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a child @class{gtk-widget}}
  @argument[detachable]{whether the tab is detachable or not}
  @begin{short}
    Sets whether the tab can be detached from notebook to another notebook or
    widget.
  @end{short}

  Note that 2 notebooks must share a common group identificator (see the
  function @fun{gtk-notebook-set-group-name}) to allow automatic tabs
  interchange between them.

  If you want a widget to interact with a notebook through DnD (i. e.: accept
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
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-set-group-name}"
  (setf (gtk-notebook-child-detachable notebook child) detachable))

(export 'gtk-notebook-set-tab-detachable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_menu_label_text" gtk-notebook-get-menu-label-text)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{the child widget of a page of the notebook.}
  @begin{return}
    The text of the tab label, or @code{nil} if the widget does not have a
    menu label other than the default menu label, or the menu label widget is
    not a @class{gtk-label}. The string is owned by the widget and must not be
    freed.
  @end{return}
  Retrieves the text of the menu label for the page containing child.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-get-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_tab_label_text" gtk-notebook-get-tab-label-text)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a widget contained in a page of @arg{notebook}}
  @begin{return}
    The text of the tab label, or @code{nil} if the tab label widget is not a
    @class{gtk-label}. The string is owned by the widget and must not be freed.
  @end{return}
  Retrieves the text of the tab label for the page containing child.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-get-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-reorderable))

(defun gtk-notebook-get-tab-reorderable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a child @class{gtk-widget}}
  @return{@em{True} if the tab is reorderable.}
  @short{Gets whether the tab can be reordered via drag and drop or not.}
  @see-class{gtk-notebook}"
  (gtk-notebook-child-reorderable notebook child))

(export 'gtk-notebook-get-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-get-tab-detachable))

(defun gtk-notebook-get-tab-detachable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-17}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[child]{a child @class{gtk-widget}}
  @return{@em{True} if the tab is detachable.}
  @short{Returns whether the tab contents can be detached from @arg{notebook}.}
  @see-class{gtk-notebook}"
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
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[page-num]{index of the page to switch to, starting from 0. If
    negative, the last page will be used. If greater than the number of pages
    in the notebook, nothing will be done.}
  @short{Switches to the page number @arg{page-num}.}

  Note that due to historical reasons, @class{gtk-notebook} refuses to switch to
  a page unless the child widget is visible. Therefore, it is recommended to
  show child widgets before adding them to a notebook.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook))
  (page-num :int))

(export 'gtk-notebook-set-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_set_action_widget" gtk-notebook-set-action-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[widget]{a @class{gtk-widget}}
  @argument[pack-type]{pack type of the action widget}
  @begin{short}
    Sets widget as one of the action widgets. Depending on the pack type the
    widget will be placed before or after the tabs. You can use a
    @class{gtk-box} if you need to pack more than one widget on the same side.
  @end{short}

  Note that action widgets are \"internal\" children of the notebook and thus
  not included in the list returned from the function
  @fun{gtk-container-foreach}.
  @see-class{gtk-notebook}
  @see-function{gtk-container-foreach}"
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
 "@version{2013-5-20}
  @argument[notebook]{a @class{gtk-notebook} container}
  @argument[pack-type]{pack type of the action widget to receive}
  @begin{return}
    The action widget with the given @arg{pack-type} or @code{nil} when this
    action widget has not been set.
  @end{return}
  @begin{short}
    Gets one of the action widgets. See the function
    @fun{gtk-notebook-set-action-widget}.
  @end{short}
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-set-action-widget}"
  (notebook (g-object gtk-notebook))
  (pack-type gtk-pack-type))

(export 'gtk-notebook-get-action-widget)

;;; --- End of file gtk.notebook.lisp ------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.notebook.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;;            gboolean    enable-popup    Read / Write
;;;               gchar*   group-name      Read / Write
;;;                gint    page            Read / Write
;;;            gboolean    scrollable      Read / Write
;;;            gboolean    show-border     Read / Write
;;;            gboolean    show-tabs       Read / Write
;;;     GtkPositionType    tab-pos         Read / Write
;;;
;;; Child Properties
;;;
;;;            gboolean    detachable      Read / Write
;;;                gchar*  menu-label      Read / Write
;;;                gint    position        Read / Write
;;;            gboolean    reorderable     Read / Write
;;;            gboolean    tab-expand      Read / Write
;;;            gboolean    tab-fill        Read / Write
;;;               gchar*   tab-label       Read / Write
;;;
;;; Style Properties
;;;
;;;                gint    arrow-spacing                   Read
;;;            gboolean    has-backward-stepper            Read
;;;            gboolean    has-forward-stepper             Read
;;;            gboolean    has-secondary-backward-stepper  Read
;;;            gboolean    has-secondary-forward-stepper   Read
;;;            gboolean    has-tab-gap                     Read
;;;                gint    initial-gap                     Read
;;;                gint    tab-curvature                   Read
;;;                gint    tab-overlap                     Read
;;;
;;; Signals
;;;
;;;            gboolean    change-current-page    Action
;;;         GtkNotebook*   create-window          Run Last
;;;            gboolean    focus-tab              Action
;;;                void    move-focus-out         Action
;;;                void    page-added             Run Last
;;;                void    page-removed           Run Last
;;;                void    page-reordered         Run Last
;;;            gboolean    reorder-tab	          Action
;;;            gboolean    select-page	          Action
;;;                void    switch-page            Run Last
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
;;; enum GtkNotebookTab
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkNotebookTab" gtk-notebook-tab
  (:export t
   :type-initializer "gtk_notebook_tab_get_type")
  (:tab-first 0)
  (:tab-last 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-tab atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-notebook-tab atdoc:*external-symbols*)
 "@version{2021-6-4}
  @begin{short}
    The values of this enumeration are used as arguments of the \"focus-tab\"
    signal.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkNotebookTab\" gtk-notebook-tab
  (:export t
   :type-initializer \"gtk_notebook_tab_get_type\")
  (:tab-first 0)
  (:tab-last 1))
  @end{pre}
  @begin[code]{table}
    @entry[:tab-first]{}
    @entry[:tab-last]{}
  @end{table}
  @see-class{gtk-notebook}")

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
 "@version{2021-6-4}
  @begin{short}
    The @sym{gtk-notebook} widget is a @class{gtk-container} widget whose
    children are pages that can be switched between using tab labels along one
    edge.
  @end{short}

  @image[notebook]{}

  There are many configuration options for @sym{gtk-notebook} widgets. Among
  other things, you can choose on which edge the tabs appear, see the function
  @fun{gtk-notebook-tab-pos}, whether, if there are too many tabs to fit the
  notebook should be made bigger or scrolling arrows added, see the function
  @fun{gtk-notebook-scrollable}, and whether there will be a popup menu allowing
  the users to switch pages, see the functions @fun{gtk-notebook-popup-enable}
  and @fun{gtk-notebook-popup-disable}.
  @begin[GtkNotebook as GtkBuildable]{dictionary}
    The @sym{gtk-notebook} implementation of the @class{gtk-buildable}
    interface supports placing children into tabs by specifying \"tab\" as the
    \"type\" attribute of a @code{<child>} element. Note that the content of the
    tab must be created before the tab can be filled. A tab child can be
    specified without specifying a @code{<child>} type attribute.

    To add a child widget in the notebooks action area, specify
    @code{\"action-start\"} or @code{\"action-end\"} as the @code{\"type\"}
    attribute of the @code{<child>} element.

    @b{Example:} A UI definition fragment with the @sym{gtk-notebook} widget
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
    The @sym{gtk-notebook} widget has a main CSS node with name @code{notebook},
    a subnode with name @code{header} and below that a subnode with name
    @code{tabs} which contains one subnode per tab with name @code{tab}.

    If action widgets are present, their CSS nodes are placed next to the tabs
    node. If the notebook is scrollable, CSS nodes with name @code{arrow} are
    placed as first and last child of the tabs node.

    The main node gets the @code{.frame} style class when the notebook has a
    border, see the function @fun{gtk-notebook-show-border}.

    The header node gets one of the style class @code{.top}, @code{.bottom},
    @code{.left} or @code{.right}, depending on where the tabs are placed. For
    reorderable pages, the tab node gets the @code{.reorderable-page} class.

    A tab node gets the @code{.dnd} style class while it is moved with
    drag-and-drop.

    The nodes are always arranged from left-to-right, regardless of text
    direction.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[detachable]{entry}
        The @code{detachable} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the tab is detachable. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[menu-label]{entry}
        The @code{menu-label} child property of type @code{:string}
        (Read / Write) @br{}
        The string displayed in the menu entry of the child. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child in the parent. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[reorderable]{entry}
        The @code{reorderable} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the tab is reorderable by user action. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[tab-expand]{entry}
        The @code{tab-expand} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether to expand the tab of the child. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[tab-fill]{entry}
        The @code{tab-fill} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the tab of the child should fill the allocated area. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[tab-label]{entry}
        The @code{tab-label} child property of type @code{:string}
        (Read / Write) @br{}
        The string displayed on the tab label of the child. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-spacing]{entry}
        The @code{arrow-spacing} style property of type @code{:int} (Read) @br{}
        Defines the spacing between the scroll arrows and the tabs. @br{}
        @em{Warning:} The @code{arrow-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. This property is ignored. Use margins on arrows or the \"tabs\"
        node to achieve the same effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[has-backward-stepper]{entry}
        The @code{has-backward-stepper} style property of type @code{:boolean}
        (Read) @br{}
        Determines whether the standard backward arrow button is displayed.
        @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-forward-stepper]{entry}
        The @code{has-forward-stepper} style property of type @code{:boolean}
        (Read) @br{}
        Determines whether the standard forward arrow button is displayed. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[has-secondary-backward-stepper]{entry}
        The @code{has-secondary-backward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        Determines whether a second backward arrow button is displayed on the
        opposite end of the tab area. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[has-secondary-forward-stepper]{entry}
        The @code{has-secondary-forward-stepper} style property of type
        @code{:boolean} (Read) @br{}
        Determines whether a second forward arrow button is displayed on the
        opposite end of the tab area. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[has-tab-gap]{entry}
        The @code{has-tab-gap} style property of type @code{:boolean}
        (Read) @br{}
        Defines whether the active tab is draw with a gap at the bottom. When
        @em{true} the theme engine uses the function @fun{gtk-render-extension}
        to draw the active tab. When @em{false} the functions
        @fun{gtk-render-background} and @fun{gtk-render-frame} are used. @br{}
        @em{Warning:} The @code{has-tab-gap} style property has been deprecated
        since version 3.20 and should not be used in newly-written code.
        This function always behaves as if it was set to @em{false}. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[initial-gap]{entry}
        The @code{initial-gap} style property of type @code{:int} (Read) @br{}
        Defines the minimum size for the initial gap between the first tab.@br{}
        @em{Warning:} The @code{initial-gap} style property has been deprecated
        since version 3.20 and should not be used in newly-written code. The
        intial gap is ignored. Use margins on the header node to achieve the
        same effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[tab-curvature]{entry}
        The @code{tab-curvature} style property of type @code{:int} (Read) @br{}
        Defines size of tab curvature. @br{}
        @em{Warning:} The @code{tab-curvature} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. This property is ignored. Use margins on tab nodes to achieve the
        same effect. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[tab-overlap]{entry}
        The @code{tab-overlap} style property of type @code{:int} (Read) @br{}
        Defines size of tab overlap area. @br{}
        @em{Warning;} The @code{tab-overlap} style property has been deprecated
        since version 3.20 and should not be used in newly-written code. This
        property is ignored. Use margins on tab nodes to achieve the same
        effect. @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
 lambda (notebook offset)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[offset]{An integer with the offset to step forward or backward
          for a negative integer.}
      @end{table}
    @subheading{The \"create-window\" signal}
      @begin{pre}
 lambda (notebook page x y)    :run-last
      @end{pre}
      The signal is emitted when a detachable tab is dropped on the root window.
      A handler for this signal can create a window containing a notebook where
      the tab will be attached. It is also responsible for moving/resizing the
      window and adding the necessary properties to the notebook, e.g. the
      @code{group-name} property.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk-widget} tab of @arg{notebook} that is being
          detached.}
        @entry[x]{An integer with the x coordinate where the drop happens.}
        @entry[y]{An integer with the y coordinate where the drop happens.}
        @entry[Returns]{A @sym{gtk-notebook} widget that @arg{page} should be
          added to, or @code{nil}.}
      @end{table}
    @subheading{The \"focus-tab\" signal}
      @begin{pre}
 lambda (notebook tab)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[tab]{A value of the @symbol{gtk-notebook-tab} enumeration.}
      @end{table}
    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
 lambda (notebook direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[direction]{A value of the @symbol{gtk-direction-type}
          enumeration.}
      @end{table}
    @subheading{The \"page-added\" signal}
      @begin{pre}
 lambda (notebook child page-num)    :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page is added to the
      notebook.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk-widget} child affected.}
        @entry[page-num]{An unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"page-removed\" signal}
      @begin{pre}
 lambda (notebook child page-num)   :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page is removed from
      the notebook.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk-widget} child affected.}
        @entry[page-num]{An unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"page-reordered\" signal}
      @begin{pre}
 lambda (notebook child page-num)    :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page has been
      reordered.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk-widget} child affected.}
        @entry[page-num]{An unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"reorder-tab\" signal}
      @begin{pre}
 lambda (notebook direction move-to-last)   :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[direction]{A value of the @symbol{gtk-direction-type}
          enumeration.}
        @entry[move-to-last]{A boolean.}
      @end{table}
    @subheading{The \"select-page\" signal}
      @begin{pre}
 lambda (notebook move-focus)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[move-focus]{A boolean.}
      @end{table}
    @subheading{The \"switch-page\" signal}
      @begin{pre}
 lambda (notebook page page-num)    :run-last
      @end{pre}
      Emitted when the user or a function changes the current page.
      @begin[code]{table}
        @entry[notebook]{The @sym{gtk-notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk-widget} current page.}
        @entry[page-num]{An unsigned integer with the index of the page.}
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
 "The @code{enable-popup} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-enable-popup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-enable-popup 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-enable-popup object) => enable-popup}
  @syntax[]{(setf (gtk-notebook-enable-popup object) enable-popup)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[enable-popup]{if @em{true}, pops up a menu}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{enable-popup} slot of the
    @class{gtk-notebook} class.
  @end{short}

  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-popup-enable}
  @see-function{gtk-notebook-popup-disable}")

;;; --- gtk-notebook-group-name ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group-name" 'gtk-notebook) 't)
 "The @code{group-name} property of type @code{:string} (Read / Write) @br{}
  Group name for tab drag and drop. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-group-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-group-name 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-group-name object) => group-name}
  @syntax[]{(setf (gtk-notebook-group-name object) group-name)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[group-name]{a string with the name of the notebook group, or
    @code{nil} to unset it}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{group-name} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The slot access function @sym{gtk-notebook-group-name} gets the current group
  name for the notebook. The slot access function
  @sym{(setf gtk-notebook-group-name)} sets a group name.

  Notebooks with the same name will be able to exchange tabs via drag and
  drop. A notebook with a @code{nil} group name will not be able to exchange
  tabs with any other notebook.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-page ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page" 'gtk-notebook) 't)
 "The @code{page} property of type @code{:int} (Read / Write) @br{}
  The index of the current page. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-page atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-page 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-page object) => page}
  @syntax[]{(setf (gtk-notebook-page object) page)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[page]{an integer with the index of the current page}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{page} slot of the @class{gtk-notebook}
    class.
  @end{short}

  The index of the current page.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-scrollable ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scrollable" 'gtk-notebook) 't)
 "The @code{scrollable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, scroll arrows are added if there are too many tabs to fit. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-scrollable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-scrollable 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-scrollable object) => scrollable}
  @syntax[]{(setf (gtk-notebook-scrollable object) scrollable)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[scrollable]{@em{true} if scroll arrows should be added}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{scrollable} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The slot access function @sym{gtk-notebook-scrollable} returns whether the
  tab label area has arrows for scrolling if there are too many tabs to fit in
  the area. The slot access function @sym{(setf gtk-notebook-scrollable)} sets
  whether the tab label area will have arrows for scrolling.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-show-border -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-border" 'gtk-notebook) 't)
 "The @code{show-border} property of type @code{:boolean} (Read / Write) @br{}
  Whether the border should be shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-show-border atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-show-border 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-show-border object) => show-border}
  @syntax[]{(setf (gtk-notebook-show-border object) show-border)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[show-border]{@em{true} if a bevel should be drawn around the
    notebook}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{show-border} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The slot access function @sym{gtk-notebook-show-border} returns whether a
  bevel will be drawn around the notebook pages. The slot access function
  @sym{(setf gtk-notebook-show-border)} sets whether a bevel will be drawn.

  This only has a visual effect when the tabs are not shown. See the function
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
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-show-tabs object) => show-tabs}
  @syntax[]{(setf (gtk-notebook-show-tabs object) show-tabs)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[show-tabs]{@em{true} if the tabs should be shown}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{show-tabs} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The slot access function @sym{gtk-notebook-show-tabs} returns whether the
  tabs of the notebook are shown. The slot access function
  @sym{(setf gtk-notebook-show-tabs)} sets whether to show the tabs.
  @see-class{gtk-notebook}")

;;; --- gtk-notebook-tab-pos ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tab-pos" 'gtk-notebook) 't)
 "The @code{tab-pos} property of type @symbol{gtk-position-type} (Read / Write)
  @br{}
  Which side of the notebook holds the tabs. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-tab-pos atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-tab-pos 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-tab-pos object) => pos}
  @syntax[]{(setf (gtk-notebook-tab-pos object) pos)}
  @argument[object]{a @class{gtk-notebook} widget}
  @argument[pos]{a value of the @symbol{gtk-position-type} enumeration with the
    edge to draw the tabs at}
  @begin{short}
    Accessor of the @slot[gtk-notebook]{tab-pos} slot of the
    @class{gtk-notebook} class.
  @end{short}

  The slot access function @sym{gtk-notebook-tab-pos} gets the edge at which
  the tabs for switching pages in the notebook are drawn. The slot access
  function @sym{(setf gtk-notebook-tab-pos)} sets the edge.
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
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-detachable container child) => detachable}
  @syntax[]{(setf (gtk-notebook-child-detachable container child) detachable)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[detachable]{a boolean whether the tab is detachable}
  @begin{short}
    Accessor of the @code{detachable} child property of the
    @class{gtk-notebook} class.
  @end{short}

  Whether the tab is detachable.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtl-notebook-child-menu-label ------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-menu-label
                       "menu-label" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-menu-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-menu-label 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-menu-label container child) => menu-label}
  @syntax[]{(setf (gtk-notebook-child-menu-label container child) menu-label)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[menu-label]{a string displayed in the child's menu entry}
  @begin{short}
    Accessor of the child property @code{menu-label} of the
    @class{gtk-notebook} class.
  @end{short}

  The string displayed in the menu entry of the child.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtk-notebook-child-position --------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-position 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-position container child) => position}
  @syntax[]{(setf (gtk-notebook-child-position container child) position)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[position]{an integer with the index of the child in the notebook}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk-notebook} class.
  @end{short}

  The index of the child in the notebook.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtk-notebook-child-reorderable -----------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-reorderable
                       "reorderable" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-reorderable 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-reorderable container child) => reorderable}
  @syntax[]{(setf (gtk-notebook-child-reorderable container child) reorderable)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[reorderable]{a boolean whether the tab is reorderable}
  @begin{short}
    Accessor of the @code{reorderable} child property of the
    @class{gtk-notebook} class.
  @end{short}

  Whether the tab is reorderable by user action.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtk-notebook-child-tab-expand ------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-expand
                       "tab-expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-expand 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-tab-expand container child) => tab-expand}
  @syntax[]{(setf (gtk-notebook-child-tab-expand container child) tab-expand)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[tab-expand]{a boolean whether to expand the tab of the child}
  @begin{short}
    Accessor of the @code{tab-expand} child property of the
    @class{gtk-notebook} class.
  @end{short}

  Whether to expand the tab of the child.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtk-notebook-child-tab-fill --------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-fill
                       "tab-fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-fill 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-tab-fill container child) => tab-fill}
  @syntax[]{(setf (gtk-notebook-child-tab-fill container child) tab-fill)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[tab-fill]{a boolean whether the tab of the child should fill the
    allocated area}
  @begin{short}
    Accessor of the @code{tab-fill} child property of the
    @class{gtk-notebook} class.
  @end{short}

  Whether the tab of the child should fill the allocated area.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; --- gtk-notebook-child-tab-level -------------------------------------------

(define-child-property "GtkNotebook"
                       gtk-notebook-child-tab-label
                       "tab-label" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-notebook-child-tab-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-notebook-child-tab-label 'function)
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-child-tab-label container child) => tab-label}
  @syntax[]{(setf (gtk-notebook-child-tab-label container child) tab-label)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[tab-label]{a string displayed on the tab label of the child}
  @begin{short}
    Accessor of the @code{tab-label} child property of the
    @class{gtk-notebook} class.
  @end{short}

  The string displayed on the tab label of the child.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-new))

(defun gtk-notebook-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @return{The newly created @class{gtk-notebook} widget.}
  @begin{short}
    Creates a new notebook with no pages.
  @end{short}
  @see-class{gtk-notebook}"
  (make-instance 'gtk-notebook))

(export 'gtk-notebook-new)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page" gtk-notebook-append-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, \"page N\"}
  @begin{return}
    An integer with the index starting from 0 of the appended page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Appends a page to the notebook.
  @end{short}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-prepend-page}
  @see-function{gtk-notebook-insert-page}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget)))

(export 'gtk-notebook-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_append_page_menu" gtk-notebook-append-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, \"page N\"}
  @argument[menu-label]{the @class{gtk-widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The index starting from 0 of the appended page in the notebook, or -1
    if function fails.
  @end{return}
  @begin{short}
    Appends a page to the notebook, specifying the widget to use as the label
    in the popup menu.
  @end{short}

  If @arg{menu-label} is @code{nil}, and @arg{tab-label} is a @class{gtk-label}
  widget or @code{nil}, then the menu label will be a newly created label with
  the same text as @arg{tab-label}. If @arg{tab-label} is not a
  @class{gtk-label} widget, @arg{menu-label} must be specified if the
  page-switch menu is to be used.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-class{gtk-label}
  @see-function{gtk-notebook-append-page}"
  (notebook (g-object gtk-widget))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget))
  (menu-label (g-object gtk-widget)))

(export 'gtk-notebook-append-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page" gtk-notebook-prepend-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, 'page N'}
  @begin{return}
    The index starting from 0 of the prepended page in the notebook, or -1
    if the function fails.
  @end{return}
  @begin{short}
    Prepends a page to the notebook.
  @end{short}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-append-page}
  @see-function{gtk-notebook-insert-page}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget)))

(export 'gtk-notebook-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prepend_page_menu" gtk-notebook-prepend-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, 'page N'}
  @argument[menu-label]{the @class{gtk-widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The index starting from 0 of the prepended page in the notebook, or -1
    if function fails.
  @end{return}
  @begin{short}
    Prepends a page to the notebook, specifying the widget to use as the label
    in the popup menu.
  @end{short}

  If @arg{menu-label} is @code{nil}, and @arg{tab-label} is a @class{gtk-label}
  widget or @code{nil}, then the menu label will be a newly created label with
  the same text as @arg{tab-label}. If @arg{tab-label} is not a
  @class{gtk-label} widget, @arg{menu-label} must be specified if the
  page-switch menu is to be used.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-class{gtk-label}
  @see-function{gtk-notebook-prepend-page}"
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
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, 'page N'}
  @argument[position]{an integer with the index starting at 0 at which to
    insert the page, or -1 to append the page after all other pages}
  @begin{return}
    The index starting from 0 of the inserted page in the notebook, or -1
    if function fails.
  @end{return}
  @begin{short}
    Insert a page into the notebook at the given position.
  @end{short}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-append-page}
  @see-function{gtk-notebook-prepend-page}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget))
  (position :int))

(export 'gtk-notebook-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_insert_page_menu" gtk-notebook-insert-page-menu) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, 'page N'}
  @argument[menu-label]{the @class{gtk-widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @argument[position]{an integer with the index starting at 0 at which to
    insert the page, or -1 to append the page after all other pages}
  @return{The index starting from 0 of the inserted page in the notebook.}
  @begin{short}
    Insert a page into the notebook at the given position, specifying the
    widget to use as the label in the popup menu.
  @end{short}

  If @arg{menu-label} is @code{nil}, and @arg{tab-label} is a @class{gtk-label}
  widget or @code{nil}, then the menu label will be a newly created label with
  the same text as @arg{tab-label}. If @arg{tab-label} is not a
  @class{gtk-label} widget, @arg{menu-label} must be specified if the
  page-switch menu is to be used.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-class{gtk-label}
  @see-function{gtk-notebook-insert-page}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (tab-label (g-object gtk-widget))
  (menu-label (g-object gtk-widget))
  (position :int))

(export 'gtk-notebook-insert-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk-notebook-add-page
;;; ----------------------------------------------------------------------------

(defun gtk-notebook-add-page (notebook child tab-label
                              &key (position :end) menu-label)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to use as the content of the
    page}
  @argument[tab-label]{the @class{gtk-widget} object to use as the label for
    the page, or @code{nil} to use the default label, 'page N'}
  @argument[position]{an integer with the index starting at 0 at which to insert
    the page, or -1 to append the page after all other pages, or @code{:end} to
    append the page, @code{:start} to prepend the page, the default value is
    @code{:end}}
  @argument[menu-label]{the @class{gtk-widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{short}
    Insert a page into the notebook.
  @end{short}

  This function combines the following functions:
  @begin{itemize}
    @item{@fun{gtk-notebook-append-page}}
    @item{@fun{gtk-notebook-append-page-menu}}
    @item{@fun{gtk-notebook-prepend-page}}
    @item{@fun{gtk-notebook-prepend-page-menu}}
    @item{@fun{gtk-notebook-insert-page}}
    @item{@fun{gtk-notebook-insert-page-menu}}
  @end{itemize}
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-append-page}
  @see-function{gtk-notebook-append-page-menu}
  @see-function{gtk-notebook-prepend-page}
  @see-function{gtk-notebook-prepend-page-menu}
  @see-function{gtk-notebook-insert-page}
  @see-function{gtk-notebook-insert-page-menu}"
  (assert (typep position '(or integer (member :start :end))))
  (assert (typep menu-label '(or null g-object (member :default))))
  (case position
    (:end
      (if menu-label
          (gtk-notebook-append-page-menu notebook
                                         child
                                         tab-label
                                         (if (eq menu-label :default)
                                             (null-pointer)
                                             menu-label))
          (gtk-notebook-append-page notebook child tab-label)))
    (:start
     (if menu-label
         (gtk-notebook-prepend-page-menu notebook
                                         child
                                         tab-label
                                         (if (eq menu-label :default)
                                             (null-pointer)
                                             menu-label))
         (gtk-notebook-prepend-page notebook child tab-label)))
    (otherwise
     (if menu-label
         (gtk-notebook-insert-page-menu notebook
                                        child
                                        tab-label
                                        (if (eq menu-label :default)
                                            (null-pointer)
                                            menu-label)
                                        position)
         (gtk-notebook-insert-page notebook
                                   child
                                   tab-label
                                   position)))))

(export 'gtk-notebook-add-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_remove_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_remove_page" %gtk-notebook-remove-page) :void
  (notebook (g-object gtk-notebook))
  (page-num :int))

(defun gtk-notebook-remove-page (notebook page-or-number)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[page-or-number]{an integer with the index of a notebook page,
    starting from 0, if -1, the last page will be removed, or the
    @class{gtk-widget} page}
  @begin{short}
    Removes a page from the notebook given the page widget or its index in the
    notebook.
  @end{short}
  @begin[Note]{dictionary}
    In the Lisp implementation the argument can be an integer for the index or
    the page widget. The index of the page widget is got with the function
    @fun{gtk-notebook-page-num} and passed to the C function.
  @end{dictionary}
  @begin[Example]{dictionary}
    @begin{pre}
(defvar notebook (make-instance 'gtk-notebook))
=> NOTEBOOK
(defvar page (make-instance 'gtk-frame))
=> PAGE
(gtk-notebook-append-page notebook page nil)
=> 0
(gtk-notebook-remove-page notebook page)
(gtk-notebook-append-page notebook page nil)
=> 0
(gtk-notebook-remove-page notebook 0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-page-num}"
  (%gtk-notebook-remove-page notebook
                             (etypecase page-or-number
                               (integer page-or-number)
                               (gtk-widget
                                 (gtk-notebook-page-num notebook
                                                        page-or-number)))))

(export 'gtk-notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_detach_tab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_detach_tab" gtk-notebook-detach-tab) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @begin{short}
    Removes the child from the notebook.
  @end{short}

  This function is very similar to the function @fun{gtk-container-remove}, but
  additionally informs the notebook that the removal is happening as part of a
  tab DND operation, which should not be cancelled.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-container-remove}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-detach-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_page_num" gtk-notebook-page-num) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @begin{return}
    The index of the page containing child, or -1 if child is not in the
    notebook.
  @end{return}
  @begin{short}
    Finds the index of the page which contains the given child widget.
  @end{short}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_next_page" gtk-notebook-next-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @begin{short}
    Switches to the next page.
  @end{short}
  Nothing happens if the current page is the last page.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-prev-page}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_prev_page" gtk-notebook-prev-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @begin{short}
    Switches to the previous page.
  @end{short}
  Nothing happens if the current page is the first page.
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-next-page}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_reorder_child" gtk-notebook-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child to move}
  @argument[position]{an integer with the position, or -1 to move to the end}
  @begin{short}
    Reorders the page containing the child, so that it appears in the given
    position.
  @end{short}

  If the position is greater than or equal to the number of children in the
  list or negative, the child will be moved to the end of the list.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-enable))

(defun gtk-notebook-popup-enable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @begin{short}
    Enables the popup menu.
  @end{short}
  If the user clicks with the right mouse button on the tab labels, a menu with
  all the pages will be popped up.
  @begin[Note]{dictionary}
    This function calls the function @fun{gtk-notebook-enable-popup} with the
    value @em{true}.
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-popup-disable}
  @see-function{gtk-notebook-enable-popup}"
  (setf (gtk-notebook-enable-popup notebook) t))

(export 'gtk-notebook-popup-enable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-notebook-popup-disable))

(defun gtk-notebook-popup-disable (notebook)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @begin{short}
    Disables the popup menu.
  @end{short}
  See the function @fun{gtk-notebook-popup-enable}.
  @begin[Note]{dictionary}
    This function calls the function @fun{gtk-notebook-enable-popup} with the
    value @em{false}.
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-function{gtk-notebook-popup-enable}
  @see-function{gtk-notebook-enable-popup}"
  (setf (gtk-notebook-enable-popup notebook) nil))

(export 'gtk-notebook-popup-disable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page ()
;;; gtk_notebook_set_current_page () -> gtk-notebook-current-page
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-current-page) (page-num notebook)
  (foreign-funcall "gtk_notebook_set_current_page"
                   (g-object gtk-notebook) notebook
                   :int page-num
                   :void)
  page-num)

(defcfun ("gtk_notebook_get_current_page" gtk-notebook-current-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-current-page notebook) => page-num}
  @syntax[]{(setf (gtk-notebook-current-page notebook) page-num)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[page-num]{an integer with the index of the page to switch to,
    starting from 0, if negative, the last page will be used, if greater than
    the number of pages in the notebook, nothing will be done}
  @begin{short}
    Accessor of the current page of the notebook.
  @end{short}

  The function @sym{gtk-notebook-current-page} returns an integer with the
  index starting from 0 of the page number of the current page. The function
  @sym{(setf gtk-notebook-current-page)} switches to the given page number.

  Note that due to historical reasons, the @class{gtk-notebook} widget refuses
  to switch to a page unless the child widget is visible. Therefore, it is
  recommended to show child widgets before adding them to a notebook.
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label ()
;;; gtk_notebook_set_menu_label () -> gtk-notebook-menu-label
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-menu-label) (menu-label notebook child)
  (foreign-funcall "gtk_notebook_set_menu_label"
                   (g-object gtk-notebook) notebook
                   (g-object gtk-widget) child
                   (g-object gtk-widget) menu-label
                   :void)
  menu-label)

(defcfun ("gtk_notebook_get_menu_label" gtk-notebook-menu-label)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-menu-label notebook child) => menu-label}
  @syntax[]{(setf (gtk-notebook-menu-label notebook child) menu-label)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child contained in a page of notebook}
  @argument[menu-label]{the @class{gtk-widget} menu label, or @code{nil} for
    default}
  @begin{short}
    Accessor of the menu label of the notebook.
  @end{short}

  The function @sym{gtk-notebook-menu-label} returns the menu label, or
  @code{nil} if the notebook page does not have a menu label other than the
  default tab label. The function @sym{(setf gtk-notebook-menu-label)} changes
  the menu label for the page containing the child.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-menu-label-text}
  @see-function{gtk-notebook-child-menu-label}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page () -> gtk-notebook-nth-page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook" gtk-notebook-nth-page) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[page-num]{an integer with the index of a page in the notebook,
    or -1 to get the last page}
  @return{The @class{gtk-widget} child, or @code{nil} if @arg{page-num} is out
    of bounds.}
  @begin{short}
    Returns the child widget contained in page number @arg{page-num}.
  @end{short}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}"
  (notebook (g-object gtk-notebook))
  (page-num :int))

(export 'gtk-notebook-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages () -> gtk-notebook-n-pages
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_notebook_get_n_pages" gtk-notebook-n-pages) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @return{An integer with the number of pages in the notebook.}
  @short{Gets the number of pages in a notebook.}
  @see-class{gtk-notebook}"
  (notebook (g-object gtk-notebook)))

(export 'gtk-notebook-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label ()
;;; gtk_notebook_set_tab_label () -> gtk-notebook-tab-label
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-tab-label) (tab-label notebook child)
  (foreign-funcall "gtk_notebook_set_tab_label"
                   (g-object gtk-notebook) notebook
                   (g-object gtk-widget) child
                   (g-object gtk-widget) tab-label
                   :void)
  tab-label)

(defcfun ("gtk_notebook_get_tab_label" gtk-notebook-tab-label)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-tab-label notebook child) => tab-label}
  @syntax[]{(setf (gtk-notebook-tab-label notebook child) tab-label)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child page}
  @argument[tab-label]{the @class{gtk-widget} tab label to use, or @code{nil}
    for default tab label}
  @begin{short}
    Accessor of the tab label of the notebook child.
  @end{short}

  The function @sym{gtk-notebook-tab-label} returns the tab label widget for
  the page child. @code{nil} is returned if the child is not in the notebook
  or if no tab label has been set for the child. The function
  @sym{(setf gtk-notebook-tab-label)} changes the tab label for the child. If
  @code{nil} is specified for @arg{tab-label}, then the page will have the
  label 'page N'.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-tab-label-text}
  @see-function{gtk-notebook-child-tab-label}"
  (notebook (g-object gtk-notebook))
  (child (g-object gtk-widget)))

(export 'gtk-notebook-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text ()
;;; gtk_notebook_set_menu_label_text () -> gtk-notebook-menu-label-text
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-menu-label-text) (menu-text notebook child)
  (setf (gtk-notebook-child-menu-label notebook child) menu-text))

(defun gtk-notebook-menu-label-text (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{the @class{gtk-widget} child of a page of the notebook}
  @argument[menu-text]{a string with the label text}
  @begin{short}
    Accessor of the text of the menu label.
  @end{short}

  The function @sym{gtk-notebook-menu-label-text} retrieves the text of the
  menu label for the page containing child. The function
  @sym{(setf gtk-notebook-menu-label-text)} creates a new label and sets it as
  the menu label of the child.
  @begin[Note]{dictionary}
    This function is implemented with the child access function
    @fun{gtk-notebook-child-menu-label}.
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-menu-label}
  @see-function{gtk-notebook-child-menu-label}"
  (gtk-notebook-child-menu-label notebook child))

(export 'gtk-notebook-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text ()
;;; gtk_notebook_set_tab_label_text () -> gtk-notebook-tab-label-text
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-tab-label-text) (tab-text notebook child)
  (setf (gtk-notebook-child-tab-label notebook child) tab-text))

(defun gtk-notebook-tab-label-text (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-tab-label-text notebook child) => tab-text}
  @syntax[]{(setf (gtk-notebook-tab-label-text notebook child) tab-text)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child contained in a page of the
    notebook}
  @argument[tab-text]{a string with the label text}
  @begin{short}
    Accessor of the label text of the tab label widget.
  @end{short}

  The function @sym{gtk-notebook-tab-label-text} retrieves the text of the tab
  label for the page containing child. The function
  @sym{(setf gtk-notebook-tab-label-text)} creates a new label and sets it as
  the tab label for the page containing child.
  @begin[Note]{dictionary}
    This function is implemented with the child access function
    @fun{gtk-notebook-child-tab-label}.
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-tab-label}
  @see-function{gtk-notebook-child-tab-label}"
  (gtk-notebook-child-tab-label notebook child))

(export 'gtk-notebook-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_reorderable ()
;;; gtk_notebook_get_tab_reorderable () -> gtk-notebook-tab-reorderable
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-tab-reorderable) (reorderable notebook child)
  (setf (gtk-notebook-child-reorderable notebook child) reorderable))

(defun gtk-notebook-tab-reorderable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[reorderable]{a boolean whether the tab is reorderable or not}
  @begin{short}
    Accessor of the @code{reorderable} child property of the notebook page.
  @end{short}

  The function @sym{gtk-notebook-tab-reorderable} gets whether the tab can be
  reordered via drag and drop or not. The function
  @sym{(setf gtk-notebook-tab-reorderable)} sets whether the notebook tab can
  be reordered.
  @begin[Note]{dictionary}
    This function duplicates the implementation of the child accessor function
    @fun{gtk-notebook-child-reorderable}.
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-child-reorderable}"
  (gtk-notebook-child-reorderable notebook child))

(export 'gtk-notebook-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable ()
;;; gtk_notebook_set_tab_detachable () -> gtk-notebook-tab-detachable
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-tab-detachable) (detachable notebook child)
  (setf (gtk-notebook-child-detachable notebook child) detachable))

(defun gtk-notebook-tab-detachable (notebook child)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @syntax[]{(gtk-notebook-tab-detachable notebook child) => detachable}
  @syntax[]{(setf (gtk-notebook-tab-detachable notebook child) detachable)}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[child]{a @class{gtk-widget} child}
  @argument[detachable]{a boolean whether the tab is detachable or not}
  @begin{short}
    Accessor of the @code{detachable} child property of the notebook.
  @end{short}

  The function @sym{gtk-notebook-tab-detachable} returns whether the tab
  content can be detached from the notebook to another notebook or widget. The
  function @sym{(setf gtk-notebook-tab-detachable)} sets whether the tab can be
  detached.

  Note that two notebooks must share a common group identificator, see the
  function @fun{gtk-notebook-group-name}, to allow automatic tabs interchange
  between them.
  @begin[Example]{dictionary}
    If you want a widget to interact with a notebook through DnD, i.e. accept
    dragged tabs from it, it must be set as a drop destination and accept the
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
  @end{dictionary}
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-function{gtk-notebook-child-detachable}
  @see-function{gtk-notebook-group-name}"
  (gtk-notebook-child-detachable notebook child))

(export 'gtk-notebook-tab-detachable)

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
;;; gtk_notebook_get_action_widget ()
;;; gtk_notebook_set_action_widget () -> gtk-notebook-action-widget
;;; ----------------------------------------------------------------------------

(defun (setf gtk-notebook-action-widget) (pack-type notebook widget)
  (foreign-funcall "gtk_notebook_set_action_widget"
                   (g-object gtk-notebook) notebook
                   (g-object gtk-widget) widget
                   gtk-pack-type pack-type
                   :void)
  pack-type)

(defcfun ("gtk_notebook_get_action_widget" gtk-notebook-action-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-4}
  @argument[notebook]{a @class{gtk-notebook} widget}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[pack-type]{a value of the @symbol{gtk-pack-type} enumeration for
    the action}
  @begin{short}
    Accessor of the action widget of the notebook.
  @end{short}

  The function @sym{gtk-notebook-action-widget} gets one of the action widgets.
  The function @sym{(setf gtk-notebook-action-widget)} sets the widget as one
  of the action widgets. Depending on the pack type the widget will be placed
  before or after the tabs. You can use a @class{gtk-box} if you need to pack
  more than one widget on the same side.

  Note that action widgets are \"internal\" children of the notebook and thus
  not included in the list returned from the function
  @fun{gtk-container-foreach}.
  @see-class{gtk-notebook}
  @see-class{gtk-widget}
  @see-class{gtk-box}
  @see-symbol{gtk-pack-type}
  @see-function{gtk-container-foreach}"
  (notebook (g-object gtk-notebook))
  (pack-type gtk-pack-type))

(export 'gtk-notebook-action-widget)

;;; --- End of file gtk.notebook.lisp ------------------------------------------

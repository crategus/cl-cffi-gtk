;;; ----------------------------------------------------------------------------
;;; gtk.assistant.lisp
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
;;; GtkAssistant
;;;
;;;     A widget used to guide users through multi-step operations
;;;
;;; Types and Values
;;;
;;;     GtkAssistant
;;;     GtkAssistantPageType
;;;
;;; Functions
;;;
;;;     gtk_assistant_new
;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page
;;;
;;;     GtkAssistantPageFunc
;;;     gtk_assistant_set_forward_page_func
;;;
;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;     gtk_assistant_set_page_header_image                deprecated
;;;     gtk_assistant_get_page_header_image                deprecated
;;;     gtk_assistant_set_page_side_image                  deprecated
;;;     gtk_assistant_get_page_side_image                  deprecated
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_set_page_has_padding
;;;     gtk_assistant_get_page_has_padding
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page
;;;
;;; Properties
;;;
;;;                 gint    use-header-bar    Read / Write / Construct Only
;;;
;;; Child Properties
;;;
;;;             gboolean    complete          Read / Write
;;;             gboolean    has-padding       Read / Write
;;;            GdkPixbuf*   header-image      Read / Write
;;; GtkAssistantPageType    page-type         Read / Write
;;;            GdkPixbuf*   sidebar-image     Read / Write
;;;                gchar*   title             Read / Write
;;;
;;; Style Properties
;;;
;;;                 gint    content-padding   Read
;;;                 gint    header-padding    Read
;;;
;;; Signals
;;;
;;;                 void    apply             Run Last
;;;                 void    cancel            Run Last
;;;                 void    close             Run Last
;;;                 void    escape            Action
;;;                 void    prepare           Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkAssistant
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAssistant implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkAssistantPageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkAssistantPageType" gtk-assistant-page-type
  (:export t
   :type-initializer "gtk_assistant_page_type_get_type")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-page-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-assistant-page-type atdoc:*external-symbols*)
 "@version{*2021-12-3}
  @begin{short}
    An enumeration for determining the page role inside the
    @class{gtk-assistant} widget. It is used to handle buttons sensitivity and
    visibility.
  @end{short}

  Note that an assistant needs to end its page flow with a page of
  @code{:confirm}, @code{:summary} or @code{:progress} type to be correct. The
  Cancel button will only be shown if the page is not \"committed\". See the
  @fun{gtk-assistant-commit} function for details.
  @begin{pre}
(define-g-enum \"GtkAssistantPageType\" gtk-assistant-page-type
  (:export t
   :type-initializer \"gtk_assistant_page_type_get_type\")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))
  @end{pre}
  @begin[code]{table}
    @entry[:content]{The page has regular contents. Both the Back and Forward
      buttons will be shown.}
    @entry[:intro]{The page contains an introduction to the assistant task.
      Only the Forward button will be shown if there is a next page.}
    @entry[:confirm]{The page lets the user confirm or deny the changes. The
      Back and Apply buttons will be shown.}
    @entry[:summary]{The page informs the user of the changes done. Only the
      Close button will be shown.}
    @entry[:progress]{Used for tasks that take a long time to complete, blocks
      the assistant until the page is marked as complete. Only the Back button
      will be shown.}
    @entry[:custom]{Used for when other page types are not appropriate. No
      buttons will be shown, and the application must add its own buttons
      through the @fun{gtk-assistant-add-action-widget} function.}
  @end{table}
  @see-class{gtk-assistant}
  @see-function{gtk-assistant-commit}
  @see-function{gtk-assistant-add-action-widget}")

;;; ----------------------------------------------------------------------------
;;; struct GtkAssistant
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAssistant" gtk-assistant
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_assistant_get_type")
  ((use-header-bar
    gtk-assistant-use-header-bar
    "use-header-bar" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-assistant 'type)
 "@version{*2021-11-2}
  @begin{short}
    A @sym{gtk-assistant} widget is used to represent a generally complex
    operation splitted in several steps, guiding the user through its pages and
    controlling the page flow to collect the necessary data.
  @end{short}

  @image[assistant]{}

  The design of the @sym{gtk-assistant} widget is that it controls what buttons
  to show and to make sensitive, based on what it knows about the page sequence
  and the type of each page, in addition to state information like the page
  completion and committed status.

  If you have a case that does not quite fit in an assistants way of handling
  buttons, you can use the @code{:custom} page type of the
  @symbol{gtk-assistant-page-type} enumeration and handle buttons yourself.
  @begin[GtkAssistant as GtkBuildable]{dictionary}
    The @sym{gtk-assistant} implementation of the @class{gtk-buildable}
    interface exposes the action area as internal children with the
    name @code{\"action_area\"}. To add pages to an assistant in a
    @class{gtk-builder} object, simply add it as a @code{<child>} to the
    @sym{gtk-assistant} widget and set its child properties as necessary.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-assistant} implementation has a single CSS node with the name
    @code{assistant}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[complete]{entry}
        The @code{complete} child property of type @code{:boolean}
        (Read / Write) @br{}
        Setting to @em{true} marks a page as complete, i.e. all the required
        fields are filled out. GTK uses this information to control the
        sensitivity of the navigation buttons. @br{}
        Default value: @em{false} @br{}
      @end{entry}
      @begin[has-padding]{entry}
        The @code{has-padding} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the assistant adds padding around the page. Since 3.18 @br{}
        Default value: @em{true}
      @end{entry}
      @begin[header-image]{entry}
        The @code{header-image} child property of type @class{gdk-pixbuf}
        (Read / Write) @br{}
        The image used to be displayed in the page header. @br{}
        @em{Warning:} The @code{header-image} child property has been deprecated
        since version 3.2 and should not be used in newly written code. Since
        GTK 3.2, a header is no longer shown. Add your header decoration to the
        page content instead.
      @end{entry}
      @begin[page-type]{entry}
        The @code{page-type} child property of type
        @symbol{gtk-assistant-page-type} (Read / Write) @br{}
        The type of the assistant page. @br{}
        Default value: @code{:content}
      @end{entry}
      @begin[sidebar-image]{entry}
        The @code{sidebar-image} child property of type @class{gdk-pixbuf}
        (Read / Write) @br{}
        The image used to be displayed in the sidebar. @br{}
        @em{Warning:} The @code{sidebar-image} child property has been
        deprecated since version 3.2 and should not be used in newly written
        code. Since GTK 3.2, the sidebar image is no longer shown.
      @end{entry}
      @begin[title]{entry}
        The @code{title} child property of type @code{:string} (Read / Write)
        @br{}
        The title of the page. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[content-padding]{entry}
        The @code{content-padding} style property of type @code{:int} (Read)
        @br{}
        Number of pixels around the content pages. @br{}
        @em{Warning:} The @code{content-padding} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. This style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[header-padding]{entry}
        The @code{header-padding} style property of type @code{:int} (Read)
        @br{}
        Number of pixels around the header. @br{}
        @em{Warning:} The @code{content-padding} has been deprecated since
        version 3.20 and should not be used in newly written code. This style
        property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 6
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"apply\" signal}
      @begin{pre}
 lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted when the Apply button is clicked. The default
      behavior of the assistant is to switch to the page after the current page,
      unless the current page is the last one. A handler for the \"apply\"
      signal should carry out the actions for which the wizard has collected
      data. If the action takes a long time to complete, you might consider
      putting a @code{:progress} page after the @code{:confirm} page and handle
      this operation within the \"prepare\" signal of the progress page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk-assistant} widget which received the
          signal.}
    @end{table}
    @subheading{The \"cancel\" signal}
      @begin{pre}
 lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted when the Cancel button is clicked.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk-assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"close\" signal}
      @begin{pre}
 lambda (assistant)    :run-last
      @end{pre}
      The signal is emitted either when the Close button of a summary page is
      clicked, or when the Apply button in the last page in the flow is clicked,
      which is the @code{:confirm} page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk-assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"escape\" signal}
      @begin{pre}
 lambda (assistant)    :action
      @end{pre}
      No documentation.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk-assistant} widget which received the
          signal.}
      @end{table}
    @subheading{The \"prepare\" signal}
      @begin{pre}
 lambda (assistant page)    :run-last
      @end{pre}
      The signal is emitted when a new page is set as the assistants current
      page, before making the new page visible. A handler for this signal can
      do any preparations which are necessary before showing the page.
      @begin[code]{table}
        @entry[assistant]{The @sym{gtk-assistant} widget which received the
          signal.}
      @entry[page]{The @class{gtk-widget} widget for the current page.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-assistant-use-header-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkAssistant" 'gtk-assistant))

;;; --- gtk-assistant-use-header-bar -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-header-bar"
                                               'gtk-assistant) 't)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct) @br{}
  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-use-header-bar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-use-header-bar 'function)
 "@version{2021-10-27}
  @syntax[]{(gtk-assistant-use-header-bar object) => setting}
  @syntax[]{(setf (gtk-assistant-use-header-bar object) setting)}
  @argument[object]{a @class{gtk-assistant} widget}
  @argument[setting]{@em{true} if the assistant uses a header bar}
  @begin{short}
    Accessor of the @slot[gtk-assistant]{use-header-bar} slot of the
    @class{gtk-assistant} class.
  @end{short}

  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.
  @see-class{gtk-assistant}
  @see-class{gtk-header-bar}")

;;; ----------------------------------------------------------------------------
;;; Accessors of the Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-assistant-child-complete -------------------------------------------

(define-child-property "GtkAssistant"
                       gtk-assistant-child-complete
                       "complete" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-child-complete atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-complete 'function)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-child-complete container child) => complete}
  @syntax[]{(setf (gtk-assistant-child-complete container child) complete)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[complete]{a boolean whether the page is complete}
  @begin{short}
    Accessor of the @code{complete} child property of the @class{gtk-assistant}
    class.
  @end{short}

  Setting the @code{complete} child property to @em{true} marks a page as
  complete, i.e. all the required fields are filled out. GTK uses this
  information to control the sensitivity of the navigation buttons.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-page-complete}")

;;; --- gtk-assistant-child-has-padding ----------------------------------------

#+gtk-3-18
(define-child-property "GtkAssistant"
                       gtk-assistant-child-has-padding
                       "has-padding" "gboolean" t t t)

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-assistant-child-has-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-has-padding 'function)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-child-has-padding container child) => setting}
  @syntax[]{(setf (gtk-assistant-child-has-padding container child) setting)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[setting]{a boolean whether the assistant adds padding around the
    page}
  @begin{short}
    Accessor of the @code{has-padding} child property of the
    @class{gtk-assistant} class.
  @end{short}

  Whether the assistant adds padding around the page.

  Since 3.18
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-page-has-padding}")

;;; --- gtk-assistant-child-header-image ---------------------------------------

;; not exported

(define-child-property "GtkAssistant"
                       gtk-assistant-child-header-image
                       "header-image" "GdkPixbuf" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-child-header-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-header-image 'function)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-child-header-image container child) => image}
  @syntax[]{(setf (gtk-assistant-child-header-image container child) image)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[image]{a @class{gdk-pixbuf} image}
  @begin{short}
    Accessor of the @code{header-image} child property of the
    @class{gtk-assistant} class.
  @end{short}

  The image used to be displayed in the page header.
  @begin[Warning]{dictionary}
    The @code{header-image} child property has been deprecated since version 3.2
    and should not be used in newly written code. Since GTK 3.2, a header is no
    longer shown. Add your header decoration to the page content instead.
  @end{dictionary}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-class{gdk-pixbuf}")

;;; --- gtk-assistant-child-page-type ------------------------------------------

(define-child-property "GtkAssistant"
                       gtk-assistant-child-page-type
                       "page-type" "GtkAssistantPageType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-child-page-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-page-type 'function)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-child-page-type container child) => ptype}
  @syntax[]{(setf (gtk-assistant-child-page-type container child) ptype)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[ptype]{a value of the @symbol{gtk-assistant-page-type} enumeration}
  @begin{short}
    Accessor of the @code{page-type} child property of the @class{gtk-assistant}
    class.
  @end{short}

  The page type determines the page behavior in the assistant.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-symbol{gtk-assistant-page-type}
  @see-function{gtk-assistant-child-page-type}")

;;; --- gtk-assistant-child-sidebar-image --------------------------------------

;; not exported

(define-child-property "GtkAssistant"
                       gtk-assistant-child-sidebar-image
                       "sidebar-image" "GdkPixbuf" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-child-sidebar-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-sidebar-image 'function)
 "@version{2021-10-26}
  @syntax[]{(gtk-assistant-child-sidebar-image container child) => image}
  @syntax[]{(setf (gtk-assistant-child-sidebar-image container child) image)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[image]{a @class{gdk-pixbuf} image}
  @begin{short}
    Accessor of the @code{sidebar-image} child property of the
    @class{gtk-assistant} class.
  @end{short}

  The image used to be displayed in the sidebar.
  @begin[Warning]{dictionary}
    The @code{sidebar-image} child property has been deprecated since version
    3.2 and should not be used in newly written code. Since GTK 3.2, the
    sidebar image is no longer shown.
  @end{dictionary}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-class{gdk-pixbuf}")

;;; --- gtk-assistant-child-title ----------------------------------------------

(define-child-property "GtkAssistant"
                       gtk-assistant-child-title
                       "title" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-child-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-assistant-child-title 'function)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-child-title container child) => title}
  @syntax[]{(setf (gtk-assistant-child-title container child) title)}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} page of the assistant}
  @argument[title]{a string with the title of the page}
  @begin{short}
    Accessor of the @code{title} child property of the
    @class{gtk-assistant} class.
  @end{short}

  The title of the page.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-asssistant-page-title}")

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-assistant-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-10-26}
  @return{A @class{gtk-assistant} widget.}
  @begin{short}
    Creates a new assistant.
  @end{short}
  @see-class{gtk-assistant}"
  (make-instance 'gtk-assistant))

(export 'gtk-assistant-new)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_current_page ()
;;; gtk_assistant_set_current_page () -> gtk-assistant-current-page
;;; ----------------------------------------------------------------------------

(defun (setf gtk-assistant-current-page) (index assistant)
  (foreign-funcall "gtk_assistant_set_current_page"
                   (g-object gtk-assistant) assistant
                   :int index
                   :void)
  index)

(defcfun ("gtk_assistant_get_current_page" gtk-assistant-current-page) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-1}
  @syntax[]{(gtk-assistant-current-page assistant) => index}
  @syntax[]{(setf (gtk-assistant-current-page assistant) index)}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[index]{an integer with the index of the page to switch to, starting
    from 0, if negative, the last page will be used, if greater than the number
    of pages in the assistant, nothing will be done}
  @begin{short}
    Accessor of the current page of the assistant.
  @end{short}
  The @sym{gtk-assistant-current-page} function returns the page number of the
  current page in the assistant. The @sym{(setf gtk-assistant-current-page)}
  function switches the page in the assistant to @arg{index}.

  Note that this will only be necessary in custom buttons, as the assistant
  flow can be set with the @fun{gtk-assistant-set-forward-page-func} function.
  @see-class{gtk-assistant}
  @see-function{gtk-assistant-set-forward-page-func}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_n_pages () -> gtk-assistant-n-pages
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_n_pages" gtk-assistant-n-pages) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-1}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @return{An integer with the number of pages in @arg{assistant}.}
  @begin{short}
    Returns the number of pages in the assistant.
  @end{short}
  @see-class{gtk-assistant}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_nth_page () -> gtk-assistant-nth-page
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_nth_page" gtk-assistant-nth-page)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-2}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[index]{an integer with the index of a page in @arg{assistant},
    or -1 to get the last page}
  @return{The @class{gtk-widget} child widget, or @code{nil} if the @arg{index}
    argument is out of bounds.}
  @begin{short}
    Returns the child widget contained in the assistant with the given page
    index.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}"
  (assistant (g-object gtk-assistant))
  (index :int))

(export 'gtk-assistant-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_prepend_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_prepend_page" gtk-assistant-prepend-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-10-26}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of the assistant}
  @return{An integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Prepends a page to the assistant.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-append-page}
  @see-function{gtk-assistant-insert-page}
  @see-function{gtk-assistant-remove-page}"
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget)))

(export 'gtk-assistant-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_append_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_append_page" gtk-assistant-append-page) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-1}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of the assistant}
  @return{An integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Appends a page to the assistant.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-prepend-page}
  @see-function{gtk-assistant-insert-page}
  @see-function{gtk-assistant-remove-page}"
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget)))

(export 'gtk-assistant-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_insert_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_insert_page" gtk-assistant-insert-page) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-10-26}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of the assistant}
  @argument[position]{an integer with the index starting at 0 at which to
    insert @arg{page}, or -1 to append @arg{page} to the assistant}
  @return{The index starting from 0 of the inserted page.}
  @begin{short}
    Inserts a page in the assistant at a given position.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-append-page}
  @see-function{gtk-assistant-prepend-page}
  @see-function{gtk-assistant-remove-page}"
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget))
  (position :int))

(export 'gtk-assistant-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_remove_page" gtk-assistant-remove-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-1}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[index]{an integer with the index of a page in the assistant, or -1
    to remove the last page}
  @begin{short}
    Removes the page with the given page index from the assistant.
  @end{short}
  @see-class{gtk-assistant}
  @see-function{gtk-assistant-append-page}
  @see-function{gtk-assistant-prepend-page}
  @see-function{gtk-assistant-insert-page}"
  (assistant (g-object gtk-assistant))
  (index :int))

(export 'gtk-assistant-remove-page)

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageFunc ()
;;; ----------------------------------------------------------------------------

(define-cb-methods gtk-assistant-page-func :int ((current-page :int)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-assistant-page-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-assistant-page-func atdoc:*external-symbols*)
 "@version{2021-10-26}
  @begin{short}
    A callback function used by the @fun{gtk-assistant-set-forward-page-func}
    function to know which is the next page given a current one.
  @end{short}
  It is called both for computing the next page when the user presses the
  Forward button and for handling the behavior of the Last button.
  @begin{pre}
 lambda (current)
  @end{pre}
  @begin[code]{table}
    @entry[current]{An integer with the page number used to calculate the next
      page.}
    @entry[Returns]{An integer with the next page number.}
  @end{table}
  @see-class{gtk-assistant}
  @see-function{gtk-assistant-set-forward-page-func}")

(export 'gtk-assistant-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_forward_page_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_set_forward_page_func"
          %gtk-assistant-set-forward-page-func) :void
  (assistant (g-object gtk-assistant))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-assistant-set-forward-page-func (assistant func)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-26}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[func]{a @symbol{gtk-assistant-page-func} page forwarding callback
    function, or @code{nil} to use the default one}
  @begin{short}
    Sets the page forwarding function to be @arg{func}.
  @end{short}
  This function will be used to determine what will be the next page when the
  user presses the Forward button. Setting @arg{func} to @code{nil} will make
  the assistant to use the default forward function, which just goes to the next
  visible page.
  @see-class{gtk-assistant}
  @see-symbol{gtk-assistant-page-func}"
  (if func
      (%gtk-assistant-set-forward-page-func
          assistant
          (callback gtk-assistant-page-func)
          (create-fn-ref assistant func)
          (callback gtk-assistant-page-func-destroy-notify))
      (%gtk-assistant-set-forward-page-func assistant
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'gtk-assistant-set-forward-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_type ()
;;; gtk_assistant_get_page_type () -> gtk-assistant-page-type
;;; ----------------------------------------------------------------------------

(defun (setf gtk-assistant-page-type) (ptype assistant page)
  (setf (gtk-assistant-child-page-type assistant page) ptype))

(defun gtk-assistant-page-type (assistant page)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-2}
  @syntax[]{(gtk-assistant-page-type assistant page) => ptype}
  @syntax[]{(setf (gtk-assistant-page-type assistant page) ptype)}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of @arg{assistant}}
  @argument[ptype]{a value of the @symbol{gtk-assistant-page-type} enumeration}
  @begin{short}
    Accessor of the page type of a page in the assistant.
  @end{short}

  The page type determines the page behavior in the assistant. The function is
  implemented with the @fun{gtk-assistant-child-pack-type} function.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-symbol{gtk-assistant-page-type}
  @see-function{gtk-assistant-child-page-type}"
  (gtk-assistant-child-page-type assistant page))

(export 'gtk-assistant-page-type)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_title ()
;;; gtk_assistant_get_page_title () -> gtk-assistant-page-title
;;; ----------------------------------------------------------------------------

(defun (setf gtk-assistant-page-title) (title assistant page)
  (setf (gtk-assistant-child-title assistant page) title))

(defun gtk-assistant-page-title (assistant page)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-2}
  @syntax[]{(gtk-assistant-page-title assistant page) => title}
  @syntax[]{(setf (gtk-assistant-page-title assistant page) title)}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of @arg{assistant}}
  @argument[title]{a string with the new title for @arg{page}}
  @begin{short}
    Accessor of the title for the page in the assistant.
  @end{short}

  The @sym{gtk-assistant-page-title} function gets the title for the page in the
  assistant. The @sym{(setf gtk-assistant-page-title)} function sets a title.
  The title is displayed in the header area of the assistant when the page is
  the current page.

  The function is implemented with the @fun{gtk-assistant-child-title} function.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-child-title}"
  (gtk-assistant-child-title assistant page))

(export 'gtk-assistant-page-title)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_header_image ()
;;;
;;; void gtk_assistant_set_page_header_image (GtkAssistant *assistant,
;;;                                           GtkWidget *page,
;;;                                           GdkPixbuf *pixbuf);
;;;
;;; Warning
;;;
;;; gtk_assistant_set_page_header_image has been deprecated since version 3.2
;;; and should not be used in newly written code. Since GTK 3.2, a header is no
;;; longer shown; add your header decoration to the page content instead.
;;;
;;; Sets a header image for page.
;;;
;;; assistant :
;;;     a GtkAssistant
;;;
;;; page :
;;;     a page of assistant
;;;
;;; pixbuf :
;;;     the new header image page
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_header_image ()
;;;
;;; GdkPixbuf * gtk_assistant_get_page_header_image (GtkAssistant *assistant,
;;;                                                  GtkWidget *page);
;;;
;;; Warning
;;;
;;; gtk_assistant_get_page_header_image has been deprecated since version 3.2
;;; and should not be used in newly written code. Since GTK 3.2, a header is no
;;; longer shown; add your header decoration to the page content instead.
;;;
;;; Gets the header image for page.
;;;
;;; assistant :
;;;     a GtkAssistant
;;;
;;; page :
;;;     a page of assistant
;;;
;;; Returns :
;;;     the header image for page, or NULL if there's no header image for the
;;;     page
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_side_image ()
;;;
;;; void gtk_assistant_set_page_side_image (GtkAssistant *assistant,
;;;                                         GtkWidget *page,
;;;                                         GdkPixbuf *pixbuf);
;;;
;;; Warning
;;;
;;; gtk_assistant_set_page_side_image has been deprecated since version 3.2 and
;;; should not be used in newly written code. Since GTK 3.2, sidebar images are
;;; not shown anymore.
;;;
;;; Sets a side image for page.
;;;
;;; This image used to be displayed in the side area of the assistant when page
;;; is the current page.
;;;
;;; assistant :
;;;     a GtkAssistant
;;;
;;; page :
;;;     a page of assistant
;;;
;;; pixbuf :
;;;     the new side image page
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_side_image ()
;;;
;;; GdkPixbuf * gtk_assistant_get_page_side_image (GtkAssistant *assistant,
;;;                                                GtkWidget *page);
;;;
;;; Warning
;;;
;;; gtk_assistant_get_page_side_image has been deprecated since version 3.2 and
;;; should not be used in newly written code. Since GTK 3.2, sidebar images are
;;; not shown anymore.
;;;
;;; Gets the side image for page.
;;;
;;; assistant :
;;;     a GtkAssistant
;;;
;;; page :
;;;     a page of assistant
;;;
;;; Returns :
;;;     the side image for page, or NULL if there's no side image for the page
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_complete ()
;;; gtk_assistant_get_page_complete () -> gtk-assistant-page-complete
;;; ----------------------------------------------------------------------------

(defun (setf gtk-assistant-page-complete) (complete assistant page)
  (setf (gtk-assistant-child-complete assistant page) complete))

(defun gtk-assistant-page-complete (assistant page)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-2}
  @syntax[]{(gtk-assistant-page-complete assistant page) => complete}
  @syntax[]{(setf (gtk-assistant-page-complete assistant page) complete)}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of @arg{assistant}}
  @argument[complete]{a boolean with the completeness status of the page}
  @begin{short}
    Accessor of the completeness status of the page in the assistant.
  @end{short}

  The @sym{gtk-assistant-page-complete} function gets whether the page is
  complete. The @sym{(setf gtk-assistant-page-complete)} function sets whether
  the page contents are complete. This will make the assistant update the
  buttons state to be able to continue the task.

  The function is implemented with the @fun{gtk-assistant-child-complete}
  function.
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-child-complete}"
  (gtk-assistant-child-complete assistant page))

(export 'gtk-assistant-page-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_has_padding ()
;;; gtk_assistant_get_page_has_padding () -> gtk-assistant-page-has-padding
;;; ----------------------------------------------------------------------------

(defun (setf gtk-assistant-page-has-padding) (setting assistant page)
  (setf (gtk-assistant-child-has-padding assistant page) setting))

(defun gtk-assistant-page-has-padding (assistant page)
 "@version{2021-11-2}
  @syntax[]{(gtk-assistant-page-has-padding assistant page) => setting}
  @syntax[]{(setf (gtk-assistant-page-has-padding assistant page) setting)}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[page]{a @class{gtk-widget} page of @arg{assistant}}
  @argument[setting]{a boolean whether the page has padding}
  @begin{short}
    Accessor of the has padding status of the page in the assistant.
  @end{short}

  The @sym{gtk-assistant-page-has-padding} function gets whether the page has
  padding. The @sym{(setf gtk-assistant-page-has-padding)} function sets whether
  the assistant is adding padding around the page.

  The function is implemented with the @fun{gtk-assistant-child-has-padding}
  function.

  Since 3.18
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-child-has-padding}"
  (gtk-assistant-child-has-padding assistant page))

(export 'gtk-assistant-page-has-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_add_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_add_action_widget" gtk-assistant-add-action-widget)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-2}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @begin{short}
    Adds a child widget to the action area of the assistant.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-remove-action-widget}"
  (assistant (g-object gtk-assistant))
  (child (g-object gtk-widget)))

(export 'gtk-assistant-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_remove_action_widget"
           gtk-assistant-remove-action-widget)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-2}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @begin{short}
    Removes a child widget from the action area of the assistant.
  @end{short}
  @see-class{gtk-assistant}
  @see-class{gtk-widget}
  @see-function{gtk-assistant-add-action-widget}"
  (assistant (g-object gtk-assistant))
  (child (g-object gtk-widget)))

(export 'gtk-assistant-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_update_buttons_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_update_buttons_state"
          gtk-assistant-update-buttons-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-26}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @begin{short}
    Forces the assistant to recompute the buttons state.
  @end{short}
  GTK automatically takes care of this in most situations, e.g. when the user
  goes to a different page, or when the visibility or completeness of a page
  changes. One situation where it can be necessary to call this function is
  when changing a value on the current page affects the future page flow of the
  assistant.
  @see-class{gtk-assistant}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-update-buttons-state)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_commit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_commit" gtk-assistant-commit) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-1}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @begin{short}
    Erases the visited page history so the Back button is not shown on the
    current page, and removes the Cancel button from subsequent pages.
  @end{short}
  Use this when the information provided up to the current page is hereafter
  deemed permanent and cannot be modified or undone. For example, showing a
  progress page to track a long running, unreversible operation after the user
  has clicked the Apply button on a confirmation page.
  @see-class{gtk-assistant}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-commit)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_next_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_next_page" gtk-assistant-next-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-2}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @begin{short}
    Navigate to the next page.
  @end{short}
  It is a programming error to call this function when there is no next page.
  This function is for use when creating pages with the @code{:custom} value of
  the @symbol{gtk-assistant-page-type} enumeration.
  @see-class{gtk-assistant}
  @see-symbol{gtk-assistant-page-type}
  @see-function{gtk-assistant-previous-page}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_previous_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_previous_page" gtk-assistant-previous-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-2}
  @argument[assistant]{a @class{gtk-assistant} widget}
  @begin{short}
    Navigate to the previous visited page.
  @end{short}
  It is a programming error to call this function when no previous page is
  available. This function is for use when creating pages with the
  @code{:custom} value of the @symbol{gtk-assistant-page-type} enumeration.
  @see-class{gtk-assistant}
  @see-symbol{gtk-assistant-page-type}
  @see-function{gtk-assistant-next-page}"
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-previous-page)

;;; --- End of file gtk.assistant.lisp -----------------------------------------

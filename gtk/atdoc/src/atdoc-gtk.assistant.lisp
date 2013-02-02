;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.assistant.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; --- gtk-assistant ----------------------------------------------------------

(setf (documentation 'gtk-assistant 'type)
 "@version{2013-1-31}
  @begin{short}
    A @sym{gtk-assistant} is a widget used to represent a generally complex
    operation splitted in several steps, guiding the user through its pages and
    controlling the page flow to collect the necessary data.
  @end{short}

  The design of @sym{gtk-assistant} is that it controls what buttons to show and
  to make sensitive, based on what it knows about the page sequence and the type
  of each page, in addition to state information like the page completion and
  committed status.

  If you have a case that doesn't quite fit in @sym{gtk-assistant}'s way of
  handling buttons, you can use the @code{:custom} page type and handle
  buttons yourself.

  @heading{GtkAssistant as GtkBuildable}
  The @sym{gtk-assistant} implementation of the @class{gtk-buildable} interface
  exposes the @code{\"action-area\"} as internal children with the name
  \"action-area\".

  To add pages to an assistant in @class{gtk-builder}, simply add it as a
  @code{<child>} to the @sym{gtk-assistant} object, and set its child properties
  as necessary.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"complete\" child property}
    @code{\"complete\"} of type @code{gboolean} (Read / Write)@br{}
    Setting the @code{\"complete\"} child property to @arg{true} marks a page as
    complete (i.e.: all the required fields are filled out). GTK+ uses this
    information to control the sensitivity of the navigation buttons.@br{}
    Default value: @code{nil}@br{}
    Since 2.10

    @subheading{The \"header-image\" child property}
    @code{\"header-image\"} of type @class{gdk-pixbuf} (Read / Write)@br{}
    @b{Warning:}
    @code{GtkAssistant:header-image} has been deprecated since version 3.2 and
    should not be used in newly-written code. Since GTK+ 3.2, a header is no
    longer shown; add your header decoration to the page content instead.@br{}
    This image used to be displayed in the page header.@br{}
    Since 2.10

    @subheading{The \"page-type\" child property}
    @code{\"page-type\"} of type @symbol{gtk-assistant-page-type}
    (Read / Write)@br{}
    The type of the assistant page.@br{}
    Default value: @code{:content}@br{}
    Since 2.10

    @subheading{The \"sidebar-image\" child property}
    @code{\"sidebar-image\"} of type @class{gdk-pixbuf} (Read / Write)@br{}
    @b{Warning:} @code{GtkAssistant:sidebar-image} has been deprecated since
    version 3.2 and should not be used in newly-written code. Since GTK+ 3.2,
    the sidebar image is no longer shown.@br{}
    This image used to be displayed in the \"sidebar\".@br{}
    Since 2.10

    @subheading{The \"title\" child property}
    @code{\"title\"} of type @code{gchar*} (Read / Write)@br{}
    The title of the page.@br{}
    Default value: @code{nil}@br{}
    Since 2.10
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"content-padding\" style property}
    @code{\"content-padding\"} of type @code{gint} (Read)@br{}
    Number of pixels around the content pages.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{1}

    @subheading{The \"header-padding\" style property}
    @code{\"header-padding\"} of type @code{gint} (Read)@br{}
    Number of pixels around the header.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{6}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @b{The \"apply\" signal}
    @begin{pre}
 void user_function (GtkAssistant *assistant,
                     gpointer      user_data)      : Run Last
    @end{pre}
    The \"apply\" signal is emitted when the apply button is clicked.

    The default behavior of the GtkAssistant is to switch to the page after the
    current page, unless the current page is the last one.
 
    A handler for the ::apply signal should carry out the actions for which the
    wizard has collected data. If the action takes a long time to complete, you
    might consider putting a page of type GTK_ASSISTANT_PAGE_PROGRESS after the
    confirmation page and handle this operation within the \"prepare\" signal of
    the progress page.
    @begin[code]{table}
      @entry[assistant]{the GtkAssistant}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.10

    @b{The \"cancel\" signal}
    @begin{pre}
 void user_function (GtkAssistant *assistant,
                     gpointer      user_data)      : Run Last
    @end{pre}
    The ::cancel signal is emitted when then the cancel button is clicked.
    @begin[code]{table}
      @entry[assistant]{the GtkAssistant}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.10

    @b{The \"close\" signal}
    @begin{pre}
 void user_function (GtkAssistant *assistant,
                     gpointer      user_data)      : Run Last
    @end{pre}
    The ::close signal is emitted either when the close button of a summary page
    is clicked, or when the apply button in the last page in the flow (of type
    GTK_ASSISTANT_PAGE_CONFIRM) is clicked.
    @begin[code]{table}
      @entry[assistant]{the GtkAssistant}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.10

    @b{The \"prepare\" signal}
    @begin{pre}
 void user_function (GtkAssistant *assistant,
                     GtkWidget    *page,
                     gpointer      user_data)      : Run Last
    @end{pre}
    The ::prepare signal is emitted when a new page is set as the assistant's
    current page, before making the new page visible.

    A handler for this signal can do any preparations which are necessary before
    showing page.
    @begin[code]{table}
      @enty[assistant]{the GtkAssistant}
      @entry[page]{the current page}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.10
  @end{dictionary}")

;;; --- Accessors of the Child Properties --------------------------------------

(setf (gethash 'gtk-assistant-child-page-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-assistant-child-page-type 'function)
 "@version{2013-2-1}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a page of assistant}
  @begin{short}
    Accessor of the child property @code{\"page-type\"} of the
    @class{gtk-assistant} class.
  @end{short}")

(setf (gethash 'gtk-assistant-child-title atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-assistant-child-title 'function)
 "@version{2013-2-1}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a page of assistant}
  @begin{short}
    Accessor of the child property @code{\"title\"} of the
    @class{gtk-assistant} class.
  @end{short}")

(setf (gethash 'gtk-assistant-child-header-image atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-assistant-child-header-image 'function)
 "@version{2013-2-1}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a page of assistant}
  @begin{short}
    Accessor of the child property @code{\"header-image\"} of the
    @class{gtk-assistant} class.
  @end{short}")

(setf (gethash 'gtk-assistant-child-sidebar-image atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-assistant-child-sidebar-image 'function)
 "@version{2013-2-1}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a page of assistant}
  @begin{short}
    Accessor of the child property @code{\"sidebar-image\"} of the
    @class{gtk-assistant} class.
  @end{short}")

(setf (gethash 'gtk-assistant-child-complete atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-assistant-child-complete 'function)
 "@version{2013-2-1}
  @argument[container]{a @class{gtk-assistant} widget}
  @argument[child]{a page of assistant}
  @begin{short}
    Accessor of the child property @code{\"complete\"} of the
    @class{gtk-assistant} class.
  @end{short}")

;;; --- gtk-assistant-new ------------------------------------------------------

(setf (documentation 'gtk-assistant-new 'function)
 "@version{2013-2-1}
  @return{a @class{gtk-assistant} widget}
  @begin{short}
    Creates a new @class{gtk-assistant}.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-get-current-page -----------------------------------------

(setf (documentation 'gtk-assistant-get-current-page 'function)
 "@version{2013-1-31}
  @argument[assistant]{a GtkAssistant}
  @return{The index (starting from 0) of the current page in the assistant,
    or -1 if the assistant has no pages, or no current page.}
  @begin{short}
    Returns the page number of the current page.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-set-current-page -----------------------------------------

(setf (documentation 'gtk-assistant-set-current-page 'function)
 "@version{2013-1-31}
  @argument[assistant]{a GtkAssistant}
  @argument[page_num]{index of the page to switch to, starting from 0. If
    negative, the last page will be used. If greater than the number of pages in
    the assistant, nothing will be done.}
  @begin{short}
    Switches the page to page_num.
  @end{short}

  Note that this will only be necessary in custom buttons, as the assistant
  flow can be set with gtk_assistant_set_forward_page_func().

  Since 2.10")

;;; --- gtk-assistant-get-n-pages ----------------------------------------------

(setf (documentation 'gtk-assistant-get-n-pages 'function)
 "@version{2013-1-31}
  @argument[assistant]{a GtkAssistant}
  @return{the number of pages in the assistant}
  @begin{short}
    Returns the number of pages in the assistant
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-get-nth-page ---------------------------------------------

(setf (documentation 'gtk-assistant-get-nth-page 'function)
 "@version{2013-1-31}
  @argument[assistant]{a GtkAssistant}
  @argument[page_num]{the index of a page in the assistant, or -1 to get the
    last page}
  @return{the child widget, or NULL if page_num is out of bounds}
  @begin{short}
    Returns the child widget contained in page number page_num.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-prepend-page ---------------------------------------------

(setf (documentation 'gtk-assistant-prepend-page 'function)
 "@version{2013-1-31}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a GtkWidget}
  @return{the index (starting at 0) of the inserted page}
  @begin{short}
    Prepends a page to the assistant.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-append-page ----------------------------------------------

(setf (documentation 'gtk-assistant-append-page 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a GtkWidget}
  @return{the index (starting at 0) of the inserted page}
  @begin{short}
    Appends a page to the assistant.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-insert-page ----------------------------------------------

(setf (documentation 'gtk-assistant-insert-page 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a GtkWidget}
  @argument[position]{the index (starting at 0) at which to insert the page, or
    -1 to append the page to the assistant}
  @return{the index (starting from 0) of the inserted page}
  @begin{short}
    Inserts a page in the assistant at a given position.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-remove-page ----------------------------------------------

(setf (documentation 'gtk-assistant-remove-page 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page_num]{the index of a page in the assistant, or -1 to remove the
    last page}
  @begin{short}
    Removes the page_num's page from assistant.
  @end{short}

  Since 3.2")

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageFunc ()
;;; 
;;; gint (*GtkAssistantPageFunc) (gint current_page, gpointer data);
;;; 
;;; A function used by gtk_assistant_set_forward_page_func() to know which is
;;; the next page given a current one. It's called both for computing the next
;;; page when the user presses the "forward" button and for handling the
;;; behavior of the "last" button.
;;; 
;;; current_page :
;;;     The page number used to calculate the next page.
;;; 
;;; data :
;;;     user data
;;; 
;;; Returns :
;;;     The next page number.
;;; ----------------------------------------------------------------------------

;;; --- gtk-assistant-set-forward-page-func ------------------------------------

(setf (documentation 'gtk-assistant-set-forward-page-func 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page_func]{the GtkAssistantPageFunc, or NULL to use the default one}
  @argument[data]{user data for page_func}
  @argument[destroy]{destroy notifier for data}
  @begin{short}
    Sets the page forwarding function to be page_func.
  @end{short}

  This function will be used to determine what will be the next page when the
  user presses the forward button. Setting page_func to NULL will make the
  assistant to use the default forward function, which just goes to the next
  visible page.

  Since 2.10")

;;; --- gtk-assistant-page-type ------------------------------------------------

(setf (gethash 'gtk-assistant-page-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-assistant-page-type atdoc:*external-symbols*)
 "@version{2013-2-1}
  @begin{short}
    An enum for determining the page role inside the GtkAssistant. It's used to
   handle buttons sensitivity and visibility.
  @end{short}

  Note that an assistant needs to end its page flow with a page of type
  GTK_ASSISTANT_PAGE_CONFIRM, GTK_ASSISTANT_PAGE_SUMMARY or
  GTK_ASSISTANT_PAGE_PROGRESS to be correct.

  The Cancel button will only be shown if the page isn't \"committed\". See
  gtk_assistant_commit() for details.
  @begin{pre}
(define-g-enum \"GtkAssistantPageType\" gtk-assistant-page-type
  (:export t
   :type-initializer \"gtk_assistant_page_type_get_type\")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom 5))
  @end{pre}
  @begin[code]{table}
    @entry[:content]{The page has regular contents. Both the Back and forward
      buttons will be shown.}
    @entry[:intro]{The page contains an introduction to the assistant task. Only
      the Forward button will be shown if there is a next page.}
    @entry[:confirm]{The page lets the user confirm or deny the changes. The
      Back and Apply buttons will be shown.}
    @entry[:summary]{The page informs the user of the changes done. Only the
      Close button will be shown.}
    @entry[:progress]{Used for tasks that take a long time to complete, blocks
      the assistant until the page is marked as complete. Only the back button
      will be shown.}
    @entry[:custom]{Used for when other page types are not appropriate. No
      buttons will be shown, and the application must add its own buttons
      through gtk_assistant_add_action_widget().}
  @end{table}")

;;; --- gtk-assistant-set-page-type --------------------------------------------

(setf (documentation 'gtk-assistant-set-page-type 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @argument[type]{the new type for page}
  @begin{short}
    Sets the page type for page.
  @end{short}

  The page type determines the page behavior in the assistant.

  Since 2.10")

;;; --- gtk-assistant-get-page-type --------------------------------------------

(setf (documentation 'gtk-assistant-get-page-type 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @return{the page type of page}
  @begin{short}
    Gets the page type of page.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-set-page-title -------------------------------------------

(setf (documentation 'gtk-assistant-set-page-title 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @argument[title]{the new title for page}
  @begin{short}
    Sets a title for page.
  @end{short}

  The title is displayed in the header area of the assistant when page is the
  current page.

  Since 2.10")

;;; --- gtk-assistant-get-page-title -------------------------------------------

(setf (documentation 'gtk-assistant-get-page-title 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @return{the title for page}
  @begin{short}
    Gets the title for page.
  @end{short}

  Since 2.10")

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
;;; and should not be used in newly-written code. Since GTK+ 3.2, a header is no
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
;;; and should not be used in newly-written code. Since GTK+ 3.2, a header is no
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
;;; should not be used in newly-written code. Since GTK+ 3.2, sidebar images are
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
;;; should not be used in newly-written code. Since GTK+ 3.2, sidebar images are
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

;;; --- gtk-assistant-set-page-complete ----------------------------------------

(setf (documentation 'gtk-assistant-set-page-complete 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @argument[complete]{the completeness status of the page}
  @begin{short}
    Sets whether page contents are complete.
  @end{short}

  This will make assistant update the buttons state to be able to continue the
  task.

  Since 2.10")

;;; --- gtk-assistant-get-page-complete ----------------------------------------

(setf (documentation 'gtk-assistant-get-page-complete 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[page]{a page of assistant}
  @return{TRUE if page is complete.}
  @begin{short}
    Gets whether page is complete.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-add-action-widget ----------------------------------------

(setf (documentation 'gtk-assistant-add-action-widget 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[child]{a GtkWidget}
  @begin{short}
    Adds a widget to the action area of a GtkAssistant.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-remove-action-widget -------------------------------------

(setf (documentation 'gtk-assistant-remove-action-widget 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @argument[child]{a GtkWidget}
  @begin{short}
    Removes a widget from the action area of a GtkAssistant.
  @end{short}

  Since 2.10")

;;; --- gtk-assistant-update-buttons-state -------------------------------------

(setf (documentation 'gtk-assistant-update-buttons-state 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @begin{short}
    Forces assistant to recompute the buttons state.
  @end{short}

  GTK+ automatically takes care of this in most situations, e.g. when the user
  goes to a different page, or when the visibility or completeness of a page
  changes.

  One situation where it can be necessary to call this function is when
  changing a value on the current page affects the future page flow of the
  assistant.

  Since 2.10")

;;; --- gtk-assistant-commit ---------------------------------------------------

(setf (documentation 'gtk-assistant-commit 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @begin{short}
    Erases the visited page history so the back button is not shown on the
    current page, and removes the cancel button from subsequent pages.
  @end{short}

  Use this when the information provided up to the current page is hereafter
  deemed permanent and cannot be modified or undone. For example, showing a
  progress page to track a long-running, unreversible operation after the user
  has clicked apply on a confirmation page.

  Since 2.22")

;;; --- gtk-assistant-next-page ------------------------------------------------

(setf (documentation 'gtk-assistant-next-page 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @begin{short}
    Navigate to the next page.
  @end{short}

  It is a programming error to call this function when there is no next page.

  This function is for use when creating pages of the
  GTK_ASSISTANT_PAGE_CUSTOM type.

  Since 3.0")

;;; --- gtk-assistant-previous-page --------------------------------------------

(setf (documentation 'gtk-assistant-previous-page 'function)
 "@version{2013-2-1}
  @argument[assistant]{a GtkAssistant}
  @begin{short}
    Navigate to the previous visited page.
  @end{short}

  It is a programming error to call this function when no previous page is
  available.

  This function is for use when creating pages of the
  GTK_ASSISTANT_PAGE_CUSTOM type.

  Since 3.0")

;;; --- End of file gtk.assistant.lisp -----------------------------------------

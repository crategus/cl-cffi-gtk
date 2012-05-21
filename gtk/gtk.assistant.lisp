;;; ----------------------------------------------------------------------------
;;; gtk.assistant.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; A widget used to guide users through multi-step operations
;;;     
;;; Synopsis
;;; 
;;;     GtkAssistant
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
;;;     gtk_assistant_set_forward_page_func
;;;     
;;;     GtkAssistantPageType
;;;     
;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;     gtk_assistant_set_page_header_image
;;;     gtk_assistant_get_page_header_image
;;;     gtk_assistant_set_page_side_image
;;;     gtk_assistant_get_page_side_image
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page
;;;     
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkAssistant
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkAssistant implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Child Properties
;;; 
;;;   "complete"                 gboolean              : Read / Write
;;;   "header-image"             GdkPixbuf*            : Read / Write
;;;   "page-type"                GtkAssistantPageType  : Read / Write
;;;   "sidebar-image"            GdkPixbuf*            : Read / Write
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "content-padding"          gint                  : Read
;;;   "header-padding"           gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "apply"                                          : Run Last
;;;   "cancel"                                         : Run Last
;;;   "close"                                          : Run Last
;;;   "prepare"                                        : Run Last
;;; 
;;; Description
;;; 
;;; A GtkAssistant is a widget used to represent a generally complex operation
;;; splitted in several steps, guiding the user through its pages and
;;; controlling the page flow to collect the necessary data.
;;; 
;;; The design of GtkAssistant is that it controls what buttons to show and to
;;; make sensitive, based on what it knows about the page sequence and the type
;;; of each page, in addition to state information like the page completion and
;;; committed status.
;;; 
;;; If you have a case that doesn't quite fit in GtkAssistants way of handling
;;; buttons, you can use the GTK_ASSISTANT_PAGE_CUSTOM page type and handle
;;; buttons yourself.
;;; 
;;; GtkAssistant as GtkBuildable
;;; 
;;; The GtkAssistant implementation of the GtkBuildable interface exposes the
;;; action_area as internal children with the name "action_area".
;;; 
;;; To add pages to an assistant in GtkBuilder, simply add it as a <child> to
;;; the GtkAssistant object, and set its child properties as necessary.
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "complete" child property
;;; 
;;;   "complete"                 gboolean              : Read / Write
;;; 
;;; Setting the "complete" child property to TRUE marks a page as complete
;;; (i.e.: all the required fields are filled out). GTK+ uses this information
;;; to control the sensitivity of the navigation buttons.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "header-image" child property
;;; 
;;;   "header-image"             GdkPixbuf*            : Read / Write
;;; 
;;; Warning
;;; 
;;; GtkAssistant:header-image has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Since GTK+ 3.2, a header is no longer
;;; shown; add your header decoration to the page content instead.
;;; 
;;; This image used to be displayed in the page header.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-type" child property
;;; 
;;;   "page-type"                GtkAssistantPageType  : Read / Write
;;; 
;;; The type of the assistant page.
;;; 
;;; Default value: GTK_ASSISTANT_PAGE_CONTENT
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "sidebar-image" child property
;;; 
;;;   "sidebar-image"            GdkPixbuf*            : Read / Write
;;; 
;;; Warning
;;; 
;;; GtkAssistant:sidebar-image has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Since GTK+ 3.2, the sidebar image is no
;;; longer shown.
;;; 
;;; This image used to be displayed in the 'sidebar'.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" child property
;;; 
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; The title of the page.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "content-padding" style property
;;; 
;;;   "content-padding"          gint                  : Read
;;; 
;;; Number of pixels around the content pages.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "header-padding" style property
;;; 
;;;   "header-padding"           gint                  : Read
;;; 
;;; Number of pixels around the header.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "apply" signal
;;; 
;;; void user_function (GtkAssistant *assistant,
;;;                     gpointer      user_data)      : Run Last
;;; 
;;; The ::apply signal is emitted when the apply button is clicked.
;;; 
;;; The default behavior of the GtkAssistant is to switch to the page after the
;;; current page, unless the current page is the last one.
;;; 
;;; A handler for the ::apply signal should carry out the actions for which the
;;; wizard has collected data. If the action takes a long time to complete, you
;;; might consider putting a page of type GTK_ASSISTANT_PAGE_PROGRESS after the
;;; confirmation page and handle this operation within the "prepare" signal of
;;; the progress page.
;;; 
;;; assistant :
;;;     the GtkAssistant
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "cancel" signal
;;; 
;;; void user_function (GtkAssistant *assistant,
;;;                     gpointer      user_data)      : Run Last
;;; 
;;; The ::cancel signal is emitted when then the cancel button is clicked.
;;; 
;;; assistant :
;;;     the GtkAssistant
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "close" signal
;;; 
;;; void user_function (GtkAssistant *assistant,
;;;                     gpointer      user_data)      : Run Last
;;; 
;;; The ::close signal is emitted either when the close button of a summary page
;;; is clicked, or when the apply button in the last page in the flow (of type
;;; GTK_ASSISTANT_PAGE_CONFIRM) is clicked.
;;; 
;;; assistant :
;;;     the GtkAssistant
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "prepare" signal
;;; 
;;; void user_function (GtkAssistant *assistant,
;;;                     GtkWidget    *page,
;;;                     gpointer      user_data)      : Run Last
;;; 
;;; The ::prepare signal is emitted when a new page is set as the assistant's
;;; current page, before making the new page visible.
;;; 
;;; A handler for this signal can do any preparations which are necessary before
;;; showing page.
;;; 
;;; assistant :
;;;     the GtkAssistant
;;; 
;;; page :
;;;     the current page
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAssistant
;;; 
;;; struct GtkAssistant;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkAssistant" 'gtk-assistant))

(define-g-object-class "GtkAssistant" gtk-assistant
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_assistant_get_type")
  ((:cffi current-page gtk-assistant-current-page :int
          "gtk_assistant_get_current_page" "gtk_assistant_set_current_page")
   (:cffi n-pages gtk-assistant-n-pages :int
          "gtk_assistant_get_n_pages" nil)
   (:cffi forward-page-function gtk-assistant-forward-page-function
          nil nil set-assistant-forward-page-function)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkAssistant"
  gtk-assistant-child-page-type
  "page-type" "GtkAssistantPageType" t t t)

(define-child-property "GtkAssistant"
  gtk-assistant-child-title
  "title" "gchararray" t t t)

(define-child-property "GtkAssistant"
  gtk-assistant-child-header-image
  "header-image" "GdkPixbuf" t t t)

(define-child-property "GtkAssistant"
  gtk-assistant-child-sidebar-image
  "sidebar-image" "GdkPixbuf" t t t)

(define-child-property "GtkAssistant"
  gtk-assistant-child-complete
  "complete" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_new ()
;;; 
;;; GtkWidget * gtk_assistant_new (void);
;;; 
;;; Creates a new GtkAssistant.
;;; 
;;; Returns :
;;;     a newly created GtkAssistant
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_current_page ()
;;; 
;;; gint gtk_assistant_get_current_page (GtkAssistant *assistant);
;;; 
;;; Returns the page number of the current page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Returns :
;;;     The index (starting from 0) of the current page in the assistant, or -1
;;;     if the assistant has no pages, or no current page.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-assistant-get-current-page (assistant)
  (gtk-assistant-current-page assistant))

(export 'gtk-assistant-get-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_current_page ()
;;; 
;;; void gtk_assistant_set_current_page (GtkAssistant *assistant, gint page_num)
;;; 
;;; Switches the page to page_num.
;;; 
;;; Note that this will only be necessary in custom buttons, as the assistant
;;; flow can be set with gtk_assistant_set_forward_page_func().
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page_num :
;;;     index of the page to switch to, starting from 0. If negative, the last
;;;     page will be used. If greater than the number of pages in the assistant,
;;;     nothing will be done.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-assistant-set-current-page (assistant page-num)
  (setf (gtk-assistant-current-page assistant) page-num))

(export 'gtk-assistant-set-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_n_pages ()
;;; 
;;; gint gtk_assistant_get_n_pages (GtkAssistant *assistant);
;;; 
;;; Returns the number of pages in the assistant
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Returns :
;;;     the number of pages in the assistant
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-assistant-get-n-pages (assistant)
  (gtk-assistant-n-pages assistant))

(export 'gtk-assistant-get-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_nth_page ()
;;; 
;;; GtkWidget * gtk_assistant_get_nth_page (GtkAssistant *assistant,
;;;                                         gint page_num);
;;; 
;;; Returns the child widget contained in page number page_num.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page_num :
;;;     the index of a page in the assistant, or -1 to get the last page
;;; 
;;; Returns :
;;;     the child widget, or NULL if page_num is out of bounds
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_get_nth_page" gtk-assistant-nth-page)
    (g-object gtk-widget)
  (assistant (g-object gtk-assistant))
  (page-num :int))

(export 'gtk-assistant-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_prepend_page ()
;;; 
;;; gint gtk_assistant_prepend_page (GtkAssistant *assistant, GtkWidget *page);
;;; 
;;; Prepends a page to the assistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the index (starting at 0) of the inserted page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_prepend_page" gtk-assistant-prepend-page) :int
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget)))

(export 'gtk-assistant-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_append_page ()
;;; 
;;; gint gtk_assistant_append_page (GtkAssistant *assistant, GtkWidget *page);
;;; 
;;; Appends a page to the assistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the index (starting at 0) of the inserted page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_append_page" gtk-assistant-append-page) :int
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget)))

(export 'gtk-assistant-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_insert_page ()
;;; 
;;; gint gtk_assistant_insert_page (GtkAssistant *assistant,
;;;                                 GtkWidget *page,
;;;                                 gint position);
;;; 
;;; Inserts a page in the assistant at a given position.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a GtkWidget
;;; 
;;; position :
;;;     the index (starting at 0) at which to insert the page, or -1 to append
;;;     the page to the assistant
;;; 
;;; Returns :
;;;     the index (starting from 0) of the inserted page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_insert_page" gtk-assistant-insert-page) :int
  (assistant (g-object gtk-assistant))
  (page (g-object gtk-widget))
  (position :int))

(export 'gtk-assistant-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_page ()
;;; 
;;; void gtk_assistant_remove_page (GtkAssistant *assistant, gint page_num);
;;; 
;;; Removes the page_num's page from assistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page_num :
;;;     the index of a page in the assistant, or -1 to remove the last page
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_forward_page_func ()
;;; 
;;; void gtk_assistant_set_forward_page_func (GtkAssistant *assistant,
;;;                                           GtkAssistantPageFunc page_func,
;;;                                           gpointer data,
;;;                                           GDestroyNotify destroy);
;;; 
;;; Sets the page forwarding function to be page_func.
;;; 
;;; This function will be used to determine what will be the next page when the
;;; user presses the forward button. Setting page_func to NULL will make the
;;; assistant to use the default forward function, which just goes to the next
;;; visible page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page_func :
;;;     the GtkAssistantPageFunc, or NULL to use the default one
;;; 
;;; data :
;;;     user data for page_func
;;; 
;;; destroy :
;;;     destroy notifier for data
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_set_forward_page_func"
          %gtk-assistant-set-forward-page-func) :void
  (assistant (g-object gtk-assistant))
  (page-func :pointer)
  (data :pointer)
  (destroy :pointer))

(define-cb-methods assistant-page-func :int ((current-page :int)))

(defun gtk-assistant-set-forward-page-func (assistant func)
  (if func
      (%gtk-assistant-set-forward-page-func
                                  assistant
                                  (callback assistant-page-func-cb)
                                  (create-fn-ref assistant func)
                                  (callback assistant-page-func-destroy-notify))
      (%gtk-assistant-set-forward-page-func assistant
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'gtk-assistant-set-forward-page-func)

;;; ----------------------------------------------------------------------------
;;; enum GtkAssistantPageType
;;; 
;;; typedef enum {
;;;   GTK_ASSISTANT_PAGE_CONTENT,
;;;   GTK_ASSISTANT_PAGE_INTRO,
;;;   GTK_ASSISTANT_PAGE_CONFIRM,
;;;   GTK_ASSISTANT_PAGE_SUMMARY,
;;;   GTK_ASSISTANT_PAGE_PROGRESS,
;;;   GTK_ASSISTANT_PAGE_CUSTOM
;;; } GtkAssistantPageType;
;;; 
;;; An enum for determining the page role inside the GtkAssistant. It's used to
;;; handle buttons sensitivity and visibility.
;;; 
;;; Note that an assistant needs to end its page flow with a page of type
;;; GTK_ASSISTANT_PAGE_CONFIRM, GTK_ASSISTANT_PAGE_SUMMARY or
;;; GTK_ASSISTANT_PAGE_PROGRESS to be correct.
;;; 
;;; The Cancel button will only be shown if the page isn't "committed". See
;;; gtk_assistant_commit() for details.
;;; 
;;; GTK_ASSISTANT_PAGE_CONTENT
;;;     The page has regular contents. Both the Back and forward buttons will be
;;;     shown.
;;; 
;;; GTK_ASSISTANT_PAGE_INTRO
;;;     The page contains an introduction to the assistant task. Only the
;;;     Forward button will be shown if there is a next page.
;;; 
;;; GTK_ASSISTANT_PAGE_CONFIRM
;;;     The page lets the user confirm or deny the changes. The Back and Apply
;;;     buttons will be shown.
;;; 
;;; GTK_ASSISTANT_PAGE_SUMMARY
;;;     The page informs the user of the changes done. Only the Close button
;;;     will be shown.
;;; 
;;; GTK_ASSISTANT_PAGE_PROGRESS
;;;     Used for tasks that take a long time to complete, blocks the assistant
;;;     until the page is marked as complete. Only the back button will be
;;;     shown.
;;; 
;;; GTK_ASSISTANT_PAGE_CUSTOM
;;;     Used for when other page types are not appropriate. No buttons will be
;;;     shown, and the application must add its own buttons through
;;;     gtk_assistant_add_action_widget().
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkAssistantPageType" gtk-assistant-page-type
  (:export t
   :type-initializer "gtk_assistant_page_type_get_type")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4))

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_type ()
;;; 
;;; void gtk_assistant_set_page_type (GtkAssistant *assistant,
;;;                                   GtkWidget *page,
;;;                                   GtkAssistantPageType type);
;;; 
;;; Sets the page type for page.
;;; 
;;; The page type determines the page behavior in the assistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; type :
;;;     the new type for page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_type ()
;;; 
;;; GtkAssistantPageType gtk_assistant_get_page_type (GtkAssistant *assistant,
;;;                                                   GtkWidget *page);
;;; 
;;; Gets the page type of page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; Returns :
;;;     the page type of page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_title ()
;;; 
;;; void gtk_assistant_set_page_title (GtkAssistant *assistant,
;;;                                    GtkWidget *page,
;;;                                    const gchar *title);
;;; 
;;; Sets a title for page.
;;; 
;;; The title is displayed in the header area of the assistant when page is the
;;; current page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; title :
;;;     the new title for page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_title ()
;;; 
;;; const gchar * gtk_assistant_get_page_title (GtkAssistant *assistant,
;;;                                             GtkWidget *page);
;;; 
;;; Gets the title for page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; Returns :
;;;     the title for page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_complete ()
;;; 
;;; void gtk_assistant_set_page_complete (GtkAssistant *assistant,
;;;                                       GtkWidget *page,
;;;                                       gboolean complete);
;;; 
;;; Sets whether page contents are complete.
;;; 
;;; This will make assistant update the buttons state to be able to continue the
;;; task.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; complete :
;;;     the completeness status of the page
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_complete ()
;;; 
;;; gboolean gtk_assistant_get_page_complete (GtkAssistant *assistant,
;;;                                           GtkWidget *page);
;;; 
;;; Gets whether page is complete.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; page :
;;;     a page of assistant
;;; 
;;; Returns :
;;;     TRUE if page is complete.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_add_action_widget ()
;;; 
;;; void gtk_assistant_add_action_widget (GtkAssistant *assistant,
;;;                                       GtkWidget *child);
;;; 
;;; Adds a widget to the action area of a GtkAssistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; child :
;;;     a GtkWidget
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_add_action_widget" gtk-assistant-add-action-widget)
    :void
  (assistant (g-object gtk-assistant))
  (widget (g-object gtk-widget)))

(export 'gtk-assistant-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_action_widget ()
;;; 
;;; void gtk_assistant_remove_action_widget (GtkAssistant *assistant,
;;;                                          GtkWidget *child);
;;; 
;;; Removes a widget from the action area of a GtkAssistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; child :
;;;     a GtkWidget
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_remove_action_widget" assistant-remove-action-widget)
    :void
  (assistant (g-object gtk-assistant))
  (widget (g-object gtk-widget)))

(export 'gtk-assistant-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_update_buttons_state ()
;;; 
;;; void gtk_assistant_update_buttons_state (GtkAssistant *assistant);
;;; 
;;; Forces assistant to recompute the buttons state.
;;; 
;;; GTK+ automatically takes care of this in most situations, e.g. when the user
;;; goes to a different page, or when the visibility or completeness of a page
;;; changes.
;;; 
;;; One situation where it can be necessary to call this function is when
;;; changing a value on the current page affects the future page flow of the
;;; assistant.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_assistant_update_buttons_state"
          gtk-assistant-update-buttons-state) :void
  (assistant (g-object gtk-assistant)))

(export 'gtk-assistant-update-buttons-state)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_commit ()
;;; 
;;; void gtk_assistant_commit (GtkAssistant *assistant);
;;; 
;;; Erases the visited page history so the back button is not shown on the
;;; current page, and removes the cancel button from subsequent pages.
;;; 
;;; Use this when the information provided up to the current page is hereafter
;;; deemed permanent and cannot be modified or undone. For example, showing a
;;; progress page to track a long-running, unreversible operation after the user
;;; has clicked apply on a confirmation page.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_next_page ()
;;; 
;;; void gtk_assistant_next_page (GtkAssistant *assistant);
;;; 
;;; Navigate to the next page.
;;; 
;;; It is a programming error to call this function when there is no next page.
;;; 
;;; This function is for use when creating pages of the
;;; GTK_ASSISTANT_PAGE_CUSTOM type.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_previous_page ()
;;; 
;;; void gtk_assistant_previous_page (GtkAssistant *assistant);
;;; 
;;; Navigate to the previous visited page.
;;; 
;;; It is a programming error to call this function when no previous page is
;;; available.
;;; 
;;; This function is for use when creating pages of the
;;; GTK_ASSISTANT_PAGE_CUSTOM type.
;;; 
;;; assistant :
;;;     a GtkAssistant
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.assistant.lisp -----------------------------------------

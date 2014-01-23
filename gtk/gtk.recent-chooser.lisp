;;; ----------------------------------------------------------------------------
;;; gtk.recent-chooser.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;;
;;;     GTK_RECENT_CHOOSER_ERROR
;;;
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkRecentChooser" gtk-recent-chooser
  (:export t
   :type-initializer "gtk_recent_chooser_get_type")
  (filter
   gtk-recent-chooser-filter
   "filter" "GtkRecentFilter" t t)
  (limit
   gtk-recent-chooser-limit
   "limit" "gint" t t)
  (local-only
   gtk-recent-chooser-local-only
   "local-only" "gboolean" t t)
  (recent-manager
   gtk-recent-chooser-recent-manager
   "recent-manager" "GtkRecentManager" nil nil)
  (select-multiple
   gtk-recent-chooser-select-multiple
   "select-multiple" "gboolean" t t)
  (show-icons
   gtk-recent-chooser-show-icons
   "show-icons" "gboolean" t t)
  (show-not-found
   gtk-recent-chooser-show-not-found
   "show-not-found" "gboolean" t t)
  (show-private
   gtk-recent-chooser-show-private
   "show-private" "gboolean" t t)
  (show-tips
   gtk-recent-chooser-show-tips
   "show-tips" "gboolean" t t)
  (sort-type
   gtk-recent-chooser-sort-type
   "sort-type" "GtkRecentSortType" t t))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-chooser 'type)
 "@version{2013-5-26}
  @begin{short}
    @sym{gtk-recent-chooser} is an interface that can be implemented by widgets
    displaying the list of recently used files. In GTK+, the main objects that
    implement this interface are @class{gtk-recent-chooser-widget},
    @class{gtk-recent-chooser-dialog} and @class{gtk-recent-chooser-menu}.
  @end{short}

  Recently used files are supported since GTK+ 2.10.
  @begin[Signal Details]{dictionary}
    @subheading{The \"item-activated\" signal}
      @begin{pre}
 lambda (chooser)   : Run Last
      @end{pre}
      This signal is emitted when the user \"activates\" a recent item in the
      recent chooser. This can happen by double-clicking on an item in the
      recently used resources list, or by pressing Enter.
      @begin[code]{table}
        @entry[chooser]{The object which received the signal.}
      @end{table}
      Since 2.10

    @subheading{The \"selection-changed\" signal}
      @begin{pre}
 lambda (chooser)   : Run Last
      @end{pre}
      This signal is emitted when there is a change in the set of selected
      recently used resources. This can happen when a user modifies the
      selection with the mouse or the keyboard, or when explicitely calling
      functions to change the selection.
      @begin[code]{table}
        @entry[chooser]{The object which received the signal.}
      @end{table}
      Since 2.10
  @end{dictionary}
  @see-slot{gtk-recent-chooser-filter}
  @see-slot{gtk-recent-chooser-limit}
  @see-slot{gtk-recent-chooser-local-only}
  @see-slot{gtk-recent-chooser-recent-manager}
  @see-slot{gtk-recent-chooser-select-multiple}
  @see-slot{gtk-recent-chooser-show-icons}
  @see-slot{gtk-recent-chooser-show-not-found}
  @see-slot{gtk-recent-chooser-show-private}
  @see-slot{gtk-recent-chooser-show-tips}
  @see-slot{gtk-recent-chooser-sort-type}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "filter" 'gtk-recent-chooser) 't)
 "The @code{\"filter\"} property of type @class{gtk-recent-filter}
  (Read / Write) @br{}
  The @class{gtk-recent-filter} object to be used when displaying the recently
  used resources. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "limit" 'gtk-recent-chooser) 't)
 "The @code{\"limit\"} property of type @code{:int} (Read / Write) @br{}
  The maximum number of recently used resources to be displayed, or -1 to
  display all items. By default, the @symbol{gtk-recent-files-limit} setting
  is respected: you can override that limit on a particular instance of
  @sym{gtk-recent-chooser} by setting this property. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "local-only"
                                               'gtk-recent-chooser) 't)
 "The @code{\"local-only\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether this @sym{gtk-recent-chooser} should display only local (file:)
  resources. @br{}
  Default value: @em{true} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "recent-manager"
                                               'gtk-recent-chooser) 't)
 "The @code{\"recent-manager\"} property of type @class{gtk-recent-manager}
  (Write / Construct Only) @br{}
  The @class{gtk-recent-manager} instance used by the @sym{gtk-recent-chooser}
  to display the list of recently used resources. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "select-multiple"
                                               'gtk-recent-chooser) 't)
 "The @code{\"select-multiple\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Allow the user to select multiple resources. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-icons"
                                               'gtk-recent-chooser) 't)
 "The @code{\"show-icons\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether this @sym{gtk-recent-chooser} should display an icon near the item.
  @br{}
  Default value: @em{true} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-not-found"
                                               'gtk-recent-chooser) 't)
 "The @code{\"show-not-found\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this @sym{gtk-recent-chooser} should display the recently used
  resources even if not present anymore. Setting this to @code{nil} will perform
  a potentially expensive check on every local resource (every remote resource
  will always be displayed). @br{}
  Default value: @em{true} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-private"
                                               'gtk-recent-chooser) 't)
 "The @code{\"show-private\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the private items should be displayed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-tips"
                                               'gtk-recent-chooser) 't)
 "The @code{\"show-tips\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether this @sym{gtk-recent-chooser} should display a tooltip containing the
  full path of the recently used resources. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-type"
                                               'gtk-recent-chooser) 't)
 "The @code{\"sort-type\"} property of type @symbol{gtk-recent-sort-type}
  (Read / Write) @br{}
  Sorting order to be used when displaying the recently used resources. @br{}
  Default value: @code{:none} @br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-filter atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-filter 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"filter\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-filter}
  @see-function{gtk-recent-chooser-set-filter}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-limit atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-limit 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"limit\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-limit}
  @see-function{gtk-recent-chooser-set-limit}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-local-only atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-local-only 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"local-only\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-local-only}
  @see-function{gtk-recent-chooser-set-local-only}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-recent-manager atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-recent-manager 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"recent-manager\"} of the
  @class{gtk-recent-chooser} class.
  @see-class{gtk-recent-chooser}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-select-multiple atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-select-multiple 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"select-multiple\"} of the
  @class{gtk-recent-chooser} class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-select-multiple}
  @see-function{gtk-recent-chooser-set-select-multiple}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-show-icons atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-show-icons 'function)
 "@version{2014-1-23}
  Accessor of the slot @slot[gtk-recent-chooser]{show-icons} of the
  @class{gtk-recent-chooser} class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-icons}
  @see-function{gtk-recent-chooser-set-show-icons}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-show-not-found atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-show-not-found 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"show-not-found\"} of the
  @class{gtk-recent-chooser} class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-not-found}
  @see-function{gtk-recent-chooser-set-show-not-found}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-show-private atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-show-private 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"show-private\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-private}
  @see-function{gtk-recent-chooser-set-show-private}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-show-tips atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-show-tips 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"show-tips\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-tips}
  @see-function{gtk-recent-chooser-set-show-tips}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-sort-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-chooser-sort-type 'function)
 "@version{2013-11-23}
  Accessor of the slot @code{\"sort-type\"} of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-sort-type}
  @see-function{gtk-recent-chooser-set-sort-type}")

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
;;;   void            (* item_activated)     (GtkRecentChooser  *chooser);
;;;   void            (* selection_changed)  (GtkRecentChooser  *chooser);
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
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentChooserError" gtk-recent-chooser-error
  (:export t
   :type-initializer "gtk_recent_chooser_error_get_type")
  (:not-found 0)
  (:invalid-uri 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-chooser-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-recent-chooser-error atdoc:*external-symbols*)
 "@version{2013-5-26}
  @begin{short}
    These identify the various errors that can occur while calling
    @class{gtk-recent-chooser} functions.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRecentChooserError\" gtk-recent-chooser-error
  (:export t
   :type-initializer \"gtk_recent_chooser_error_get_type\")
  (:not-found 0)
  (:invalid-uri 1))
  @end{pre}
  @begin[code]{table}
    @entry[:not-found]{Indicates that a file does not exist.}
    @entry[:invalid-uri]{Indicates a malformed URI.}
  @end{table}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_private ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-show-private))

(defun gtk-recent-chooser-set-show-private (chooser show-private)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[show-private]{@em{true} to show private items, @code{nil} otherwise}
  @begin{short}
    Whether to show recently used resources marked registered as private.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-private}"
  (setf (gtk-recent-chooser-show-private chooser) show-private))

(export 'gtk-recent-chooser-set-show-private)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_private ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-show-private))

(defun gtk-recent-chooser-get-show-private (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{return}
    @em{True} if the recent chooser should show private items, @code{nil}
    otherwise.
  @end{return}
  @begin{short}
    Returns whether @arg{chooser} should display recently used resources
    registered as private.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-show-private}"
  (gtk-recent-chooser-show-private chooser))

(export 'gtk-recent-chooser-get-show-private)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_not_found ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-show-not-found))

(defun gtk-recent-chooser-set-show-not-found (chooser show-not-found)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[show-not-found]{whether to show the local items we did not find}
  @begin{short}
    Sets whether @arg{chooser} should display the recently used resources that
    it did not find.
  @end{short}
  This only applies to local resources.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-not-found}"
  (setf (gtk-recent-chooser-show-not-found chooser) show-not-found))

(export 'gtk-recent-chooser-set-show-not-found)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_not_found ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-show-not-found))

(defun gtk-recent-chooser-get-show-not-found (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{return}
    @em{True} if the resources not found should be displayed, and @code{nil}
    otheriwse.
  @end{return}
  @begin{short}
    Retrieves whether @arg{chooser} should show the recently used resources
    that were not found.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-show-not-found}"
  (gtk-recent-chooser-show-not-found chooser))

(export 'gtk-recent-chooser-get-show-not-found)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_icons ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-show-icons))

(defun gtk-recent-chooser-set-show-icons (chooser show-icons)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[show-icons]{whether to show an icon near the resource}
  @begin{short}
    Sets whether @arg{chooser} should show an icon near the resource when
    displaying it.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-icons}"
  (setf (gtk-recent-chooser-show-icons chooser) show-icons))

(export 'gtk-recent-chooser-set-show-icons)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_icons ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-show-icons))

(defun gtk-recent-chooser-get-show-icons (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{@em{True} if the icons should be displayed, @code{nil} otherwise.}
  @begin{short}
    Retrieves whether @arg{chooser} should show an icon near the resource.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-show-icons}"
  (gtk-recent-chooser-show-icons chooser))

(export 'gtk-recent-chooser-get-show-icons)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_select_multiple ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-select-multiple))

(defun gtk-recent-chooser-set-select-multiple (chooser select-multiple)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[select-multiple]{@em{true} if @arg{chooser} can select more than
    one item}
  @short{Sets whether @arg{chooser} can select multiple items.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-select-multiple}"
  (setf (gtk-recent-chooser-select-multiple chooser) select-multiple))

(export 'gtk-recent-chooser-set-select-multiple)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_select_multiple ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-select-multiple))

(defun gtk-recent-chooser-get-select-multiple (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{@em{True} if @arg{chooser} can select more than one item.}
  @begin{short}
    Gets whether @arg{chooser} can select multiple items.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-select-multiple}"
  (gtk-recent-chooser-select-multiple chooser))

(export 'gtk-recent-chooser-get-select-multiple)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_local_only ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-local-only))

(defun gtk-recent-chooser-set-local-only (chooser local-only)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[local-only]{@em{true} if only local files can be shown}
  @begin{short}
    Sets whether only local resources, that is resources using the file:// URI
    scheme, should be shown in the recently used resources selector.
  @end{short}
  If @arg{local-only} is @em{true} (the default) then the shown resources are
  guaranteed to be accessible through the operating system native file system.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-local-only}"
  (setf (gtk-recent-chooser-local-only chooser) local-only))

(export 'gtk-recent-chooser-set-local-only)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_local_only ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-local-only))

(defun gtk-recent-chooser-get-local-only (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{@em{True} if only local resources should be shown.}
  @begin{short}
    Gets whether only local resources should be shown in the recently used
    resources selector.
  @end{short}
  See the function @fun{gtk-recent-chooser-set-local-only}.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-local-only}"
  (gtk-recent-chooser-local-only chooser))

(export 'gtk-recent-chooser-get-local-only)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_limit ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-limit))

(defun gtk-recent-chooser-set-limit (chooser limit)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[limit]{a positive integer, or -1 for all items}
  @begin{short}
    Sets the number of items that should be returned by the functions
    @fun{gtk-recent-chooser-get-items} and @fun{gtk-recent-chooser-get-uris}.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-limit}
  @see-function{gtk-recent-chooser-get-items}
  @see-function{gtk-recent-chooser-get-uris}"
  (setf (gtk-recent-chooser-limit chooser) limit))

(export 'gtk-recent-chooser-set-limit)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_limit ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-limit))

(defun gtk-recent-chooser-get-limit (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{A positive integer, or -1 meaning that all items are returned.}
  @begin{short}
    Gets the number of items returned by the functions
    @fun{gtk-recent-chooser-get-items} and @fun{gtk-recent-chooser-get-uris}.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-limit}
  @see-function{gtk-recent-chooser-get-items}
  @see-function{gtk-recent-chooser-get-uris}"
  (gtk-recent-chooser-limit chooser))

(export 'gtk-recent-chooser-get-limit)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_show_tips ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-show-tips))

(defun gtk-recent-chooser-set-show-tips (chooser show-tips)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[show-tips]{@em{true} if tooltips should be shown}
  @begin{short}
    Sets whether to show a tooltips containing the full path of each recently
    used resource in a @class{gtk-recent-chooser} widget.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-show-tips}"
  (setf (gtk-recent-chooser-show-tips chooser) show-tips))

(export 'gtk-recent-chooser-set-show-tips)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_show_tips ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-show-tips))

(defun gtk-recent-chooser-get-show-tips (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{return}
    @em{True} if the recent chooser should show tooltips, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Gets whether @arg{chooser} should display tooltips containing the full path
    of a recently user resource.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-show-tips}"
  (gtk-recent-chooser-show-tips chooser))

(export 'gtk-recent-chooser-get-show-tips)

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentSortType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentSortType" gtk-recent-sort-type
  (:export t
   :type-initializer "gtk_recent_sort_type_get_type")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-sort-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-recent-sort-type atdoc:*external-symbols*)
 "@version{2013-11-10}
  @begin{short}
    Used to specify the sorting method to be applyed to the recently used
    resource list.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRecentSortType\" gtk-recent-sort-type
  (:export t
   :type-initializer \"gtk_recent_sort_type_get_type\")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not sort the returned list of recently used resources.}
    @entry[:mru]{Sort the returned list with the most recently used items
      first.}
    @entry[:lru]{Sort the returned list with the least recently used items
      first.}
    @entry[:custom]{Sort the returned list using a custom sorting function
      passed using the function @fun{gtk-recent-manager-set-sort-func}.}
  @end{table}
  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-sort-func}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_sort_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-sort-type))

(defun gtk-recent-chooser-set-sort-type (chooser sort-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{ a @class{gtk-recent-chooser} object}
  @argument[sort-type]{sort order of type @see-symbol{gtk-recent-sort-type}
    that the chooser should use}
  @begin{short}
    Changes the sorting order of the recently used resources list displayed by
    chooser.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-symbol{gtk-recent-sort-type}
  @see-function{gtk-recent-chooser-get-sort-type}"
  (setf (gtk-recent-chooser-sort-type chooser) sort-type))

(export 'gtk-recent-chooser-set-sort-type)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_sort_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-sort-type))

(defun gtk-recent-chooser-get-sort-type (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{The sorting of order of type @see-symbol{gtk-recent-sort-type}
    of the chooser.}
  @begin{short}
    Gets the value set by the function @fun{gtk-recent-chooser-set-sort-type}.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-symbol{gtk-recent-sort-type}
  @see-function{gtk-recent-chooser-set-sort-type}"
  (gtk-recent-chooser-sort-type chooser))

(export 'gtk-recent-chooser-get-sort-type)

;;; ----------------------------------------------------------------------------
;;; GtkRecentSortFunc ()
;;;
;;; gint (*GtkRecentSortFunc) (GtkRecentInfo *a,
;;;                            GtkRecentInfo *b,
;;;                            gpointer user_data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-recent-sort-func-cb :int
    ((a (g-boxed-foreign gtk-recent-info))
     (b (g-boxed-foreign gtk-recent-info))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) a b))

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_set_sort_func" %gtk-recent-chooser-set-sort-func)
    :void
  (chooser (g-object gtk-recent-chooser))
  (sort-func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-recent-chooser-set-sort-func (chooser func)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[sort-func]{the comparison function}
  @begin{short}
    Sets the comparison function used when sorting to be @arg{sort-func}.
  @end{short}
  If the chooser has the sort type set to @code{:custom} then the chooser
  will sort using this function.

  To the comparison function will be passed two @class{gtk-recent-info}
  structures; @arg{sort-func} should return a positive integer if the first
  item comes before the second, zero if the two items are equal and a negative
  integer if the first item comes after the second.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-info}"
  (%gtk-recent-chooser-set-sort-func
      chooser
      (callback gtk-recent-sort-func-cb)
      (glib:allocate-stable-pointer func)
      (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gtk-recent-chooser-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_current_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_set_current_uri"
          %gtk-recent-chooser-set-current-uri) :boolean
  (chooser (g-object gtk-recent-chooser))
  (uri :string)
  (error :pointer))

(defun gtk-recent-chooser-set-current-uri (chooser uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[uri]{a URI}
  @return{@em{True} if the URI was found.}
  @short{Sets @arg{uri} as the current URI for @arg{chooser}.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-current-uri}"
  (with-g-error (err)
    (%gtk-recent-chooser-set-current-uri chooser uri err)))

(export 'gtk-recent-chooser-set-current-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_current_uri"
           gtk-recent-chooser-get-current-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{A string holding a URI.}
  @short{Gets the URI currently selected by @arg{chooser}.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-set-current-uri}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-get-current-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_current_item"
           gtk-recent-chooser-get-current-item)
    (g-boxed-foreign gtk-recent-info)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{return}
    A @class{gtk-recent-info} structure. Use the function
    @fun{gtk-recent-info-unref} when when you have finished using it.
  @end{return}
  @begin{short}
    Gets the @class{gtk-recent-info} structure currently selected by
    @arg{chooser}.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-info}
  @see-function{gtk-recent-info-unref}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-get-current-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_select_uri" %gtk-recent-chooser-select-uri)
    :boolean
  (chooser (g-object gtk-recent-chooser))
  (uri :string)
  (error :pointer))

(defun gtk-recent-chooser-select-uri (chooser uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[uri]{a URI}
  @return{@em{True} if @arg{uri} was found.}
  @short{Selects @arg{uri} inside @arg{chooser}.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-unselect-uri}"
  (with-g-error (err)
    (%gtk-recent-chooser-select-uri chooser uri err)))

(export 'gtk-recent-chooser-select-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_unselect_uri" gtk-recent-chooser-unselect-uri)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[uri]{a URI}
  @short{Unselects @arg{uri} inside @arg{chooser}.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-select-uri}"
  (chooser (g-object gtk-recent-chooser))
  (uri :string))

(export 'gtk-recent-chooser-unselect-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_select_all" gtk-recent-chooser-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{short}
    Selects all the items inside @arg{chooser}, if the chooser supports multiple
    selection.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-unselect-all}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_unselect_all" gtk-recent-chooser-unselect-all)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @short{Unselects all the items inside @arg{chooser}.}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-unselect-all}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_items" gtk-recent-chooser-get-items)
    (g-list (g-boxed-foreign gtk-recent-info :free-from-foreign t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @begin{return}
    A list of @class{gtk-recent-info} objects.
  @end{return}
  @begin{short}
    Gets the list of recently used resources in form of
    @class{gtk-recent-info} objects.
  @end{short}

  The return value of this function is affected by the @code{\"sort-type\"} and
  @code{\"limit\"} properties of chooser.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-symbol{gtk-recent-info}
  @see-function{gtk-recent-chooser-get-uris}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-get-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_uris ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_uris" %gtk-recent-chooser-get-uris) g-strv
  (chooser (g-object gtk-recent-chooser))
  (length g-size))

(defun gtk-recent-chooser-get-uris (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{A list of strings.}
  @begin{short}
    Gets the URI of the recently used resources.
  @end{short}

  The return value of this function is affected by the @code{\"sort-type\"} and
  @code{\"limit\"} properties of @arg{chooser}.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-chooser-get-items}"
  (with-foreign-object (length 'g-size)
    (%gtk-recent-chooser-get-uris chooser length)))

(export 'gtk-recent-chooser-get-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_add_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_add_filter" gtk-recent-chooser-add-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @begin{short}
    Adds @arg{filter} to the list of @class{gtk-recent-filter} objects held by
    @arg{chooser}.
  @end{short}

  If no previous filter objects were defined, this function will call the
  function @fun{gtk-recent-chooser-set-filter}.

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-chooser-set-filter}
  @see-function{gtk-recent-chooser-remove-filter}"
  (chooser (g-object gtk-recent-chooser))
  (filter (g-object gtk-recent-filter)))

(export 'gtk-recent-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_remove_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_remove_filter" gtk-recent-chooser-remove-filter)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @begin{short}
    Removes @arg{filter} from the list of @class{gtk-recent-filter} objects
    held by @arg{chooser}.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-chooser-add-filter}"
  (chooser (g-object gtk-recent-chooser))
  (filter (g-object gtk-recent-filter)))

(export 'gtk-recent-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_list_filters ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_list_filters" gtk-recent-chooser-list-filters)
    (g-slist (g-object gtk-recent-filter) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{A list of @class{gtk-recent-filter} objects.}
  @begin{short}
    Gets the @class{gtk-recent-filter} objects held by chooser.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-filter}"
  (chooser (g-object gtk-recent-chooser)))

(export 'gtk-recent-chooser-list-filters)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_filter ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-set-filter))

(defun gtk-recent-chooser-set-filter (chooser filter)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @begin{short}
    Sets filter as the current @class{gtk-recent-filter} object used by
    @arg{chooser} to affect the displayed recently used resources.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-chooser-get-filter}"
  (setf (gtk-recent-chooser-filter chooser) filter))

(export 'gtk-recent-chooser-set-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_filter ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-chooser-get-filter))

(defun gtk-recent-chooser-get-filter (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[chooser]{a @class{gtk-recent-chooser} object}
  @return{A @class{gtk-recent-filter} object}
  @begin{short}
    Gets the @class{gtk-recent-filter} object currently used by chooser to
    affect the display of the recently used resources.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-chooser}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-chooser-set-filter}"
  (gtk-recent-chooser-filter chooser))

(export 'gtk-recent-chooser-get-filter)

;;; --- End of file gtk.recent-chooser.lisp ------------------------------------

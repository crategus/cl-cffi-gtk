;;; ----------------------------------------------------------------------------
;;; gtk.recent-manager.lisp
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
;;; GtkRecentManager
;;;
;;;     Managing recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentManager
;;;     GtkRecentInfo
;;;     GtkRecentData
;;;     GtkRecentManagerError
;;;
;;;     GTK_RECENT_MANAGER_ERROR
;;;
;;; Functions
;;;
;;;     gtk_recent_manager_new
;;;     gtk_recent_manager_get_default
;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_add_full
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_lookup_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_move_item
;;;     gtk_recent_manager_get_items
;;;     gtk_recent_manager_purge_items
;;;
;;;     gtk_recent_info_ref
;;;     gtk_recent_info_unref
;;;     gtk_recent_info_get_uri
;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint
;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application
;;;     gtk_recent_info_create_app_info
;;;     gtk_recent_info_get_groups
;;;     gtk_recent_info_has_group
;;;     gtk_recent_info_get_icon
;;;     gtk_recent_info_get_gicon
;;;     gtk_recent_info_get_short_name
;;;     gtk_recent_info_get_uri_display
;;;     gtk_recent_info_get_age
;;;     gtk_recent_info_is_local
;;;     gtk_recent_info_exists
;;;     gtk_recent_info_match
;;;
;;; Properties
;;;
;;;     gchar*  filename    Read / Write / Construct Only
;;;      gint   size        Read
;;;
;; Signals
;;;
;;;      void   changed     Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkRecentManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentInfo
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gtk-recent-info "GtkRecentInfo"
  :alloc (error "GtkRecentInfo can not be created from Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-info atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-recent-info 'type)
 "@version{2013-11-22}
  @begin{short}
    @sym{gtk-recent-info} constains all the meta-data associated with an entry
    in the recently used files list.
  @end{short}

  @sym{gtk-recent-info} is an opaque data structure whose members can only be
  accessed using the provided API.
  @begin{pre}
(define-g-boxed-opaque gtk-recent-info \"GtkRecentInfo\"
  :alloc (error \"GtkRecentInfo can not be created from Lisp side.\"))
  @end{pre}
  @see-class{gtk-recent-manager}")

(export (boxed-related-symbols 'gtk-recent-info))

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentData
;;;
;;; struct GtkRecentData {
;;;   gchar *display_name;
;;;   gchar *description;
;;;
;;;   gchar *mime_type;
;;;
;;;   gchar *app_name;
;;;   gchar *app_exec;
;;;
;;;   gchar **groups;
;;;
;;;   gboolean is_private;
;;; };
;;;
;;; Meta-data to be passed to gtk_recent_manager_add_full() when registering a
;;; recently used resource.
;;;
;;; gchar *display_name;
;;;     a UTF-8 encoded string, containing the name of the recently used
;;;     resource to be displayed, or NULL;
;;;
;;; gchar *description;
;;;     a UTF-8 encoded string, containing a short description of the resource,
;;;     or NULL;
;;;
;;; gchar *mime_type;
;;;     the MIME type of the resource;
;;;
;;; gchar *app_name;
;;;     the name of the application that is registering this recently used
;;;     resource;
;;;
;;; gchar *app_exec;
;;;     command line used to launch this resource; may contain the "%f" and "%u"
;;;     escape characters which will be expanded to the resource file path and
;;;     URI respectively when the command line is retrieved;
;;;
;;; gchar **groups;
;;;     a vector of strings containing groups names;
;;;
;;; gboolean is_private;
;;;     whether this resource should be displayed only by the applications that
;;;     have registered it or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_RECENT_MANAGER_ERROR
;;;
;;; #define GTK_RECENT_MANAGER_ERROR (gtk_recent_manager_error_quark ())
;;;
;;; The GError domain for GtkRecentManager errors.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentManagerError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentManagerError" gtk-recent-manager-error
  (:export t
   :type-initializer "gtk_recent_manager_error_get_type")
  (:not-found 0)
  (:invalid-uri 1)
  (:invalid-encoding 2)
  (:not-registered 3)
  (:read 4)
  (:write 5)
  (:unknown 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-manager-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-recent-manager-error atdoc:*external-symbols*)
 "@version{2013-11-22}
  @short{Error codes for @class{gtk-recent-manager} operations.}
  @begin{pre}
(define-g-enum \"GtkRecentManagerError\" gtk-recent-manager-error
  (:export t
   :type-initializer \"gtk_recent_manager_error_get_type\")
  (:not-found 0)
  (:invalid-uri 1)
  (:invalid-encoding 2)
  (:not-registered 3)
  (:read 4)
  (:write 5)
  (:unknown 6))
  @end{pre}
  @begin[code]{table}
    @entry[:not-found]{The URI specified does not exists in the recently used
      resources list.}
    @entry[:invalid-uri]{The URI specified is not valid.}
    @entry[:invalid-encoding]{The supplied string is not UTF-8 encoded.}
    @entry[:not-registered]{No application has registered the specified item.}
    @entry[:read]{Failure while reading the recently used resources file.}
    @entry[:write]{Failure while writing the recently used resources file.}
    @entry[:unknown]{Unspecified error.}
  @end{table}
  @see-class{gtk-recent-manager}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentManager
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentManager" gtk-recent-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_recent_manager_get_type")
  ((filename
    gtk-recent-manager-filename
    "filename" "gchararray" t nil)
   (size
    gtk-recent-manager-size
    "size" "gint" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-manager 'type)
 "@version{2013-5-26}
  @begin{short}
    @sym{gtk-recent-manager} provides a facility for adding, removing and
    looking up recently used files. Each recently used file is identified by its
    URI, and has meta-data associated to it, like the names and command lines of
    the applications that have registered it, the number of time each
    application has registered the same file, the mime type of the file and
    whether the file should be displayed only by the applications that have
    registered it.
  @end{short}

  @subheading{Note}
    The recently used files list is per user.

  The @sysm{gtk-recent-nanager} acts like a database of all the recently used
  files. You can create new @sym{gtk-recent-manager} objects, but it is more
  efficient to use the default manager created by GTK+.

  Adding a new recently used file is as simple as:
  @begin{pre}
 GtkRecentManager *manager;

 manager = gtk_recent_manager_get_default ();
 gtk_recent_manager_add_item (manager, file_uri);
  @end{pre}
  The @sym{gtk-recent-manager} will try to gather all the needed information
  from the file itself through GIO.

  Looking up the meta-data associated with a recently used file given its URI
  requires calling the function @fun{gtk-recent-manager-lookup-item}:
  @begin{pre}
 GtkRecentManager *manager;
 GtkRecentInfo *info;
 GError *error = NULL;

 manager = gtk_recent_manager_get_default ();
 info = gtk_recent_manager_lookup_item (manager, file_uri, &error);
 if (error)
   {
     g_warning (\"Could not find the file: %s\", error->message);
     g_error_free (error);
   @}
 else
  {
    /* Use the info object */
    gtk_recent_info_unref (info);
  @}
  @end{pre}
  In order to retrieve the list of recently used files, you can use the function
  @fun{gtk-recent-manager-get-items}, which returns a list of
  @class{gtk-recent-info} structures.

  A @sym{gtk-recent-manager} is the model used to populate the contents of
  one, or more @class{gtk-recent-chooser} implementations.

  @subheading{Note}
    The maximum age of the recently used files list is controllable through the
    \"gtk-recent-files-max-age\" property.

  Recently used files are supported since GTK+ 2.10.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (recent-manager)    : Run First
      @end{pre}
      Emitted when the current recently used resources manager changes its
      contents, either by calling the function @fun{gtk-recent-manager-add-item}
      or by another application.
      @begin[code]{table}
        @entry[recent-manager]{The recent manager.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-recent-manager-filename}
  @see-slot{gtk-recent-manager-size}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-recent-manager-filename --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "filename"
                                               'gtk-recent-manager) 't)
 "The @code{filename} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The full path to the file to be used to store and read the recently used
  resources list. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-manager-filename atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-manager-filename 'function)
 "@version{2013-11-22}
  @begin{short}
    Accessor of the slot @slot[gtk-recent-manager]{filename} of the
    @class{gtk-recent-manager} class.
  @end[short}
  @see-class{gtk-recent-manager}")

;;; --- gtk-recent-manager-size ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-recent-manager) 't)
 "The @code{size} property of type @code{:int} (Read) @br{}
  The size of the recently used resources list. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-manager-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-manager-size 'function)
 "@version{2013-11-22}
  @begin{short}
    Accessor of the slot @slot[gtk-recent-manager]{size} of the
    @class{gtk-recent-manager} class.
  @end{short}
  @see-class{gtk-recent-manager}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-recent-manager-new))

(defun gtk-recent-manager-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @return{A newly created @class{gtk-recent-manager} object.}
  @begin{short}
    Creates a new recent manager object.
  @end{short}
  Recent manager objects are used to handle the list of recently used resources.
  A @class{gtk-recent-manager} object monitors the recently used resources list,
  and emits the \"changed\" signal each time something inside the list changes.

  @class{gtk-recent-manager} objects are expensive: be sure to create them only
  when needed. You should use the function @fun{gtk-recent-manager-get-default}
  instead.
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-manager-get-default}"
  (make-instance 'gtk-recent-manager))

(export 'gtk-recent-manager-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_get_default" gtk-recent-manager-get-default)
    (g-object gtk-recent-manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @return{A unique @class{gtk-recent-manager}. Do not ref or unref it.}
  @begin{short}
    Gets a unique instance of @class{gtk-recent-manager}, that you can share in
    your application without caring about memory management.
  @end{short}
  @see-class{gtk-recent-manager}")

(export 'gtk-recent-manager-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_add_item" gtk-recent-manager-add-item) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @argument[uri]{a valid URI}
  @begin{return}
    @em{True} if the new item was successfully added to the recently used
    resources list.
  @end{return}
  @begin{short}
    Adds a new resource, pointed by uri, into the recently used resources list.
  @end{short}

  This function automatically retrieves some of the needed metadata and
  setting other metadata to common default values; it then feeds the data to
  the function @fun{gtk-recent-manager-add-full}.

  See the function @fun{gtk-recent-manager-add-full} if you want to explicitly
  define the metadata for the resource pointed by uri.
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-manager-add-full}"
  (manager (g-object gtk-recent-manager))
  (uri :string))

(export 'gtk-recent-manager-add-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_full ()
;;;
;;; gboolean gtk_recent_manager_add_full (GtkRecentManager *manager,
;;;                                       const gchar *uri,
;;;                                       const GtkRecentData *recent_data);
;;;
;;; Adds a new resource, pointed by uri, into the recently used resources list,
;;; using the metadata specified inside the GtkRecentData structure passed in
;;; recent_data.
;;;
;;; The passed URI will be used to identify this resource inside the list.
;;;
;;; In order to register the new recently used resource, metadata about the
;;; resource must be passed as well as the URI; the metadata is stored in a
;;; GtkRecentData structure, which must contain the MIME type of the resource
;;; pointed by the URI; the name of the application that is registering the
;;; item, and a command line to be used when launching the item.
;;;
;;; Optionally, a GtkRecentData structure might contain a UTF-8 string to be
;;; used when viewing the item instead of the last component of the URI; a short
;;; description of the item; whether the item should be considered private -
;;; that is, should be displayed only by the applications that have registered
;;; it.
;;;
;;; manager :
;;;     a GtkRecentManager
;;;
;;; uri :
;;;     a valid URI
;;;
;;; recent_data :
;;;     metadata of the resource
;;;
;;; Returns :
;;;     TRUE if the new item was successfully added to the recently used
;;;     resources list, FALSE otherwise.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_remove_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_remove_item" %gtk-recent-manager-remove-item)
    :boolean
  (manager (g-object gtk-recent-manager))
  (uri :string)
  (error :pointer))

(defun gtk-recent-manager-remove-item (manager uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @argument[uri]{the URI of the item you wish to remove}
  @begin{return}
    @em{True} if the item pointed by @arg{uri} has been successfully removed by
    the recently used resources list, and @code{nil} otherwise.
  @end{return}
  @begin{short}
    Removes a resource pointed by @arg{uri} from the recently used resources
    list handled by a recent manager.
  @end{short}
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-manager-add-item}"
  (with-g-error (err)
    (%gtk-recent-manager-remove-item manager uri err)))

(export 'gtk-recent-manager-remove-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_lookup_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_lookup_item" %gtk-recent-manager-lookup-item)
    (g-boxed-foreign gtk-recent-info)
  (manager (g-object gtk-recent-manager-lookup-item))
  (uri :string)
  (error :pointer))

(defun gtk-recent-manager-lookup-item (manager uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @argument[uri]{a URI}
  @begin{return}
    A @class{gtk-recent-info} structure containing information about the
    resource pointed by @arg{uri}, or @code{nil} if the URI was not registered
    in the recently used resources list. Free with the function
    @fun{gtk-recent-info-unref}.
  @end{return}
  @begin{short}
    Searches for a URI inside the recently used resources list, and returns a
    structure containing informations about the resource like its MIME type, or
    its display name.
  @end{short}
  @see-class{gtk-recent-manager}
  @see-class{gtk-recent-info}
  @see-function{gtk-recent-info-unref}"
  (with-g-error (err)
    (%gtk-recent-manager-lookup-item manager uri err)))

(export 'gtk-recent-manager-lookup-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_has_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_has_item" gtk-recent-manager-has-item) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @argument[uri]{a URI}
  @return{@em{True} if the resource was found, @code{nil} otherwise.}
  @begin{short}
    Checks whether there is a recently used resource registered with @arg{uri}
    inside the recent manager.
  @end{short}
  @see-class{gtk-recent-manager}
  @see-function{gtk-recent-manager-add-item}"
  (manager (g-object gtk-recent-manager))
  (uri :string))

(export 'gtk-recent-manager-has-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_move_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_move_item" %gtk-recent-manager-move-item) :boolean
  (manager (g-object gtk-recent-manager))
  (uri :string)
  (new-uri :string)
  (error :pointer))

(defun gtk-recent-manager-move-item (manager uri new-uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @argument[uri]{the URI of a recently used resource}
  @argument[new-uri]{the new URI of the recently used resource, or @code{nil} to
    remove the item pointed by @arg{uri} in the list}
  @return{@em{True} on success.}
  @begin{short}
    Changes the location of a recently used resource from @arg{uri}
    to @arg{new-uri}.
  @end{short}

  Please note that this function will not affect the resource pointed by the
  URIs, but only the URI used in the recently used resources list.
  @see-class{gtk-recent-manager}"
  (with-g-error (err)
    (%gtk-recent-manager-move-item manager uri new-uri err)))

(export 'gtk-recent-manager-move-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_get_items" gtk-recent-manager-get-items)
    (g-list (g-boxed-foreign gtk-recent-info :free-from-foreign t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @begin{return}
    A list of newly allocated @class{gtk-recent-info} objects.
  @end{return}
  @begin{short}
    Gets the list of recently used resources.
  @end{short}
  @see-class{gtk-recent-manager}
  @see-class{gtk-recent-info}"
  (manager (g-object gtk-recent-manager)))

(export 'gtk-recent-manager-get-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_purge_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_manager_purge_items" %gtk-recent-manager-purge-items) :int
  (manager (g-object gtk-recent-manager))
  (error :pointer))

(defun gtk-recent-manager-purge-items (manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[manager]{a @class{gtk-recent-manager} object}
  @begin{return}
    The number of items that have been removed from the recently used
    resources list.
  @end{return}
  @begin{short}
    Purges every item from the recently used resources list.
  @end{short}
  @see-class{gtk-recent-manager}"
  (with-g-error (err)
    (%gtk-recent-manager-purge-items manager err)))

(export 'gtk-recent-manager-purge-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_ref" gtk-recent-info-ref)
    (g-boxed-foreign gtk-recent-info)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{The recent info object with its reference count increased by one.}
  @begin{short}
    Increases the reference count of @arg{info} by one.
  @end{short}
  @see-class{gtk-recent-info}
  @see-function{gtk-recent-info-unref}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-ref)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_unref" gtk-recent-info-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{short}
    Decreases the reference count of info by one.
  @end{short}
  If the reference count reaches zero, info is deallocated, and the memory
  freed.
  @see-class{gtk-recent-info}
  @see-class{gtk-recent-info-ref}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-unref)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_uri" gtk-recent-info-get-uri)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{The URI of the resource.}
  @short{Gets the URI of the resource.}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_display_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_display_name" gtk-recent-info-get-display-name)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{The display name of the resource.}
  @begin{short}
    Gets the name of the resource. If none has been defined, the basename of the
    resource is obtained.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-display-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_description ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_description" gtk-recent-info-get-description)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{The description of the resource.}
  @begin{short}
    Gets the (short) description of the resource.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-description)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_mime_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_mime_type" gtk-recent-info-get-mime-type)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{The MIME type of the resource.}
  @short{Gets the MIME type of the resource.}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_added ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_added" gtk-recent-info-get-added) :long
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{return}
    The number of seconds elapsed from system's Epoch when the resource was
    added to the list, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was added
    to the recently used resources list.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-added)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_modified ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_modified" gtk-recent-info-get-modified) :long
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{return}
    The number of seconds elapsed from system's Epoch when the resource was
    last modified, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was last
    modified.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-modified)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_visited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_visited" gtk-recent-info-get-visited) :long
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{return}
    The number of seconds elapsed from system's Epoch when the resource was
    last visited, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was last
    visited.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-visited)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_private_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_private_hint" gtk-recent-info-get-private-hint)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{@em{True} if the private flag was found, @code{nil} otherwise.}
  @begin{short}
    Gets the value of the \"private\" flag.
  @end{short}
  Resources in the recently used list that have this flag set to @em{true}
  should only be displayed by the applications that have registered them.
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-private-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_application_info ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_application_info"
          %gtk-recent-info-get-application-info) :boolean
  (info (g-boxed-foreign gtk-recent-info))
  (app-name :string)
  (app-exec (:pointer (:string :free-from-foreign nil)))
  (count (:pointer :int))
  (time (:pointer :long)))

(defun gtk-recent-info-get-application-info (info app-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[info]{a @class{gtk-recent-info} structure}
  @argument[app-name]{the name of the application that has registered this item}
  @begin{return}
    @code{app-exec} -- the string containing the command line @br{}
    @code{count} -- the number of times this item was registered @br{}
    @code{time} -- the timestamp this item was last registered for this
                   application
  @end{return}
  @begin{short}
    Gets the data regarding the application that has registered the resource
    pointed by info.
  @end{short}

  If the command line contains any escape characters defined inside the
  storage specification, they will be expanded.
  @see-class{gtk-recent-info}"
  (with-foreign-objects ((app-exec :string) (count :uint) (time :long))
    (%gtk-recent-info-get-application-info info app-name app-exec count time)
    (values (mem-ref app-exec :string)
            (mem-ref count :uint)
            (mem-ref time :long))))

(export 'gtk-recent-info-get-application-info)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_applications ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_applications" %gtk-recent-info-get-applications)
    g-strv
  (info (g-boxed-foreign gtk-recent-info))
  (length (:pointer g-size)))

(defun gtk-recent-info-get-applications (info)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{A list of strings.}
  @begin{short}
    Retrieves the list of applications that have registered this resource.
  @end{short}
  @see-class{gtk-recent-info}"
  (with-foreign-object (length 'g-size)
    (%gtk-recent-info-get-applications info length)))

(export 'gtk-recent-info-get-applications)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_last_application ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_last_application" gtk-recent-info-last-application)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{An application name.}
  @begin{short}
    Gets the name of the last application that have registered the recently used
    resource represented by info.
  @end{short}
  @see-class{gtk-recent-manager}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-last-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_application ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_has_application" gtk-recent-info-has-application)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @argument[app-name]{a string containing an application name}
  @return{@em{True} if an application with name @arg{app-name} was found,
    @code{nil} otherwise.}
  @begin{short}
    Checks whether an application registered this resource using @arg{app-name}.
  @end{short}
  @see-class{gtk-recent-manager}"
  (info (g-boxed-foreign gtk-recent-info))
  (app-name :string))

(export 'gtk-recent-info-has-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_create_app_info ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_create_app_info" %gtk-recent-info-create-app-info)
    (g-object g-app-info)
  (info (g-boxed-foreign gtk-recent-info))
  (app-name :string)
  (error :pointer))

(defun gtk-recent-info-create-app-info (info app-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @argument[app-name]{the name of the application that should be mapped to a
    @class{g-app-info}; if @code{nil} is used then the default application for
    the MIME type is used}
  @begin{return}
    The newly created @class{g-app-info}, or @code{nil}. In case of error, error
    will be set either with a @code{GTK_RECENT_MANAGER_ERROR} or a
    @code{G_IO_ERROR}.
  @end{return}
  Creates a @class{g-app-info} for the specified @class{gtk-recent-info}.
  @see-class{gtk-recent-info}
  @see-class{g-app-info}"
  (with-g-error (err)
    (%gtk-recent-info-create-app-info info app-name err)))

(export 'gtk-recent-info-create-app-info)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_groups ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_groups" %gtk-recent-info-get-groups) g-strv
  (info (g-boxed-foreign gtk-recent-info))
  (length (:pointer g-size)))

(defun gtk-recent-info-get-groups (info)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{A list of strings.}
  @begin{short}
    Returns all groups registered for the recently used item info.
  @end{short}
  @see-class{gtk-recent-info}"
  (with-foreign-object (length 'g-size)
    (%gtk-recent-info-get-groups info length)))

(export 'gtk-recent-info-get-groups)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_has_group" gtk-recent-info-has-group) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @argument[group-name]{name of a group}
  @return{@em{True} if the group was found.}
  @begin{short}
    Checks whether @arg{group-name} appears inside the groups registered for
    the recently used item info.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info))
  (group-name :string))

(export 'gtk-recent-info-has-group)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_icon" gtk-recent-info-get-icon)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @argument[size]{the size of the icon in pixels}
  @begin{return}
    A @class{gdk-pixbuf} containing the icon, or @code{nil}.
  @end{return}
  @begin{short}
    Retrieves the icon of size @arg{size} associated to the resource MIME type.
  @end{short}
  @see-class{gtk-recent-info}
  @see-class{gdk-pixbuf}"
  (info (g-boxed-foreign gtk-recent-info))
  (size :int))

(export 'gtk-recent-info-get-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_gicon" gtk-recent-info-get-gicon)
    (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{A @class{g-icon} containing the icon, or @code{nil}.}
  @begin{short}
    Retrieves the icon associated to the resource MIME type.
  @end{short}
  @see-class{gtk-recent-info}
  @see-class{g-icon}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_short_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_short_name" gtk-recent-info-get-short-name)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{A string in UTF-8 encoding.}
  @begin{short}
    Computes a valid UTF-8 string that can be used as the name of the item in a
    menu or list. For example, calling this function on an item that refers to
    \"file:///foo/bar.txt\" will yield \"bar.txt\".
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-short-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_uri_display" gtk-recent-info-get-uri-display)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{return}
    A UTF-8 string containing the resource's URI or @code{nil}.
  @end{return}
  @begin{short}
    Gets a displayable version of the resource's URI.
  @end{short}
  If the resource is local, it returns a local path; if the resource is not
  local, it returns the UTF-8 encoded content of the function
  @fun{gtk-recent-info-get-uri}.
  @see-class{gtk-recent-info}
  @see-function{gtk-recent-info-get-uri}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-uri-display)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_age ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_get_age" gtk-recent-info-get-age) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[info]{a @class{gtk-recent-info} structure}
  @begin{return}
    A positive integer containing the number of days elapsed since the time
    this resource was last modified.
  @end{return}
  @begin{short}
    Gets the number of days elapsed since the last update of the resource
    pointed by info.
  @end{short}
  @see-class{gtk-recent-info}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-get-age)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_is_local ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_is_local" gtk-recent-info-is-local) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-22}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{@em{True} if the resource is local.}
  @begin{short}
    Checks whether the resource is local or not by looking at the scheme of its
    URI.
  @end{short}
  @see-class{gtk-recent-manager}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-is-local)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_exists ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_exists" gtk-recent-info-exists) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[info]{a @class{gtk-recent-info} structure}
  @return{@em{True} if the resource exists.}
  @begin{short}
    Checks whether the resource pointed by info still exists.
  @end{short}
  At the moment this check is done only on resources pointing to local files.
  @see-class{gtk-recent-manager}"
  (info (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-exists)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_match ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_info_match" gtk-recent-info-match) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-23}
  @argument[info-a]{a @class{gtk-recent-info}}
  @argument[info-b]{a @class{gtk-recent-info}}
  @begin{return}
    @em{True} if both @class{gtk-recent-info} structures point to se same
    resource, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Checks whether two @class{gtk-recent-info} structures point to the same
    resource.
  @end{short}
  @see-class{gtk-recent-info}"
  (info-a (g-boxed-foreign gtk-recent-info))
  (info-b (g-boxed-foreign gtk-recent-info)))

(export 'gtk-recent-info-match)

;;; --- End of file gtk.recent-manager.lisp ------------------------------------

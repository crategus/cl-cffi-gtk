;;; ----------------------------------------------------------------------------
;;; gtk.accel-map.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Accelerator Maps
;;;
;;; Loadable keyboard accelerator specifications
;;;
;;; Synopsis
;;;
;;;     GtkAccelMap
;;;
;;;     gtk_accel_map_add_entry
;;;     gtk_accel_map_lookup_entry
;;;     gtk_accel_map_change_entry
;;;     gtk_accel_map_load
;;;     gtk_accel_map_save
;;;     gtk_accel_map_foreach
;;;     gtk_accel_map_load_fd
;;;     gtk_accel_map_save_fd
;;;     gtk_accel_map_load_scanner
;;;     gtk_accel_map_add_filter
;;;     gtk_accel_map_foreach_unfiltered
;;;     gtk_accel_map_get
;;;     gtk_accel_map_lock_path
;;;     gtk_accel_map_unlock_path
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMap
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAccelMap" gtk-accel-map
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_map_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-accel-map 'type)
 "@version{2013-11-29}
  @begin{short}
    Accelerator maps are used to define runtime configurable accelerators.
  @end{short}
  Functions for manipulating them are usually used by higher level convenience
  mechanisms like @class{gtk-ui-manager} and are thus considered \"low-level\".
  You will want to use them if you are manually creating menus that should have
  user-configurable accelerators.

  An accelerator is uniquely defined by:
  @begin{itemize}
    @item{an accelerator path,}
    @item{an accelerator key, and}
    @item{accelerator modifiers.}
  @end{itemize}
  The accelerator path must consist of
  @code{\"<WINDOWTYPE>/Category1/Category2/.../Action\"}, where
  @code{WINDOWTYPE} should be a unique application specific identifier that
  corresponds to the kind of window the accelerator is being used in, e. g.
  @code{\"Gimp-Image\"}, @code{\"Abiword-Document\"} or
  @code{\"Gnumeric-Settings\"}. The @code{\"Category1/.../Action\"} portion is
  most appropriately chosen by the action the accelerator triggers, i. e. for
  accelerators on menu items, choose the item's menu path, e. g.
  @code{\"File/Save As\"}, @code{\"Image/View/Zoom\"} or
  @code{\"Edit/Select All\"}. So a full valid accelerator path may look like:
  @code{\"<Gimp-Toolbox>/File/Dialogs/Tool Options...\"}.

  All accelerators are stored inside one global @sym{gtk-accel-map} object that
  can be obtained using the function @fun{gtk-accel-map-get}. See Monitoring
  changes for additional details.

  @subheading{Manipulating accelerators}
    New accelerators can be added using the function
    @fun{gtk-accel-map-add-entry}. To search for a specific accelerator, use
    the function @fun{gtk-accel-map-lookup-entry}. Modifications of existing
    accelerators should be done using the function
    @fun{gtk-accel-map-change-entry}.

    In order to avoid having some accelerators changed, they can be locked using
    the function @fun{gtk-accel-map-lock-path}. Unlocking is done using
    the function @fun{gtk-accel-map-unlock-path}.

  @subheading{Saving and loading accelerator maps}
    Accelerator maps can be saved to and loaded from some external resource. For
    simple saving and loading from file, the functions @fun{gtk-accel-map-save}
    and @fun{gtk-accel-map-load} are provided.

  @subheading{Monitoring changes}
    A @sym{gtk-accel-map} object is only useful for monitoring changes of
    accelerators. By connecting to the \"changed\" signal, one can monitor
    changes of all accelerators. It is also possible to monitor only a single
    accelerator path by using it as a detail of the \"changed\" signal.

  Since 2.4
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (object accel-path accel-key accel-mods)   : Has Details
      @end{pre}
      Notifies of a change in the global accelerator map. The path is also used
      as the detail for the signal, so it is possible to connect to
      \"changed::accel-path\".
    @begin[arg]{table}
      @entry[object]{The global @sym{gtk-accel-map} object.}
      @entry[accel-path]{The path of the accelerator that changed of type
        @code{:string}.}
      @entry[accel-key]{The key value of type @code{:uint} for the new
        accelerator.}
      @entry[accel-mods]{The modifier mask of type @symbol{gdk-modifier-type}
        for the new accelerator.}
    @end{table}
  @end{dictionary}
  @see-class{gtk-ui-manager}
  @see-symbol{gdk-modifier-type}
  @see-function{gtk-accel-map-get}
  @see-function{gtk-accel-map-add-entry}
  @see-function{gtk-accel-map-lookup-entry}
  @see-function{gtk-accel-map-change-entry}
  @see-function{gtk-accel-map-lock-path}
  @see-function{gtk-accel-map-unlock-path}
  @see-function{gtk-accel-map-save}
  @see-function{gtk-accel-map-load}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_add_entry" gtk-accel-map-add-entry) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @argument[accel-path]{valid accelerator path of type @code{:string}}
  @argument[accel-key]{the accelerator key of type @code{:uint}}
  @argument[accel-mods]{the accelerator modifiers of type
    @symbol{gdk-modifier-type}}
  @begin{short}
    Registers a new accelerator with the global accelerator map.
  @end{short}
  This function should only be called once per @arg{accel-path} with the
  canonical @arg{accel-key} and @arg{accel-mods} for this path. To change the
  accelerator during runtime programatically, use the function
  @fun{gtk-accel-map-change-entry}.
  @see-class{gtk-accel-map}
  @see-symbol{gdk-modifier-type}
  @see-function{gtk-accel-map-change-entry}"
  (accel-path :string)
  (accel-key :uint)
  (accel-mods gdk-modifier-type))

(export 'gtk-accel-map-add-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lookup_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_lookup_entry" %gtk-accel-map-lookup-entry) :boolean
  (accel-path :string)
  (key (:pointer (:struct gtk-accel-key))))

(defun gtk-accel-map-lookup-entry (accel-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @argument[accel-path]{a valid accelerator path of @code{:string}}
  @begin{return}
    @code{accel-key} -- the accelerator key of type @code{:uint} @br{}
    @code{accel-mods} -- the accelerator modifiers @symbol{gdk-modifier-type}
    @br{}
    @code{accel-flags} -- the accelerator flags of type @code{:uint} @br{}
    if @arg{accel-path} is known, @code{nil} otherwise
  @end{return}
  Looks up the accelerator entry for @arg{accel-path}.
  @see-class{gtk-accel-map}
  @see-function{gtk-accel-map-add-entry}
  @see-function{gtk-accel-map-change-entry}"
  (with-foreign-object (key '(:struct gtk-accel-key))
    (when (%gtk-accel-map-lookup-entry accel-path key)
      (values (foreign-slot-value key '(:struct gtk-accel-key) 'accel-key)
              (foreign-slot-value key '(:struct gtk-accel-key) 'accel-mods)
              (foreign-slot-value key '(:struct gtk-accel-key) 'accel-flags)))))

(export 'gtk-accel-map-lookup-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_change_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_change_entry" gtk-accel-map-change-entry) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @argument[accel-path]{a valid accelerator path of type @code{:string}}
  @argument[accel-key]{the new accelerator key of type @code{:uint}}
  @argument[accel-mods]{the new accelerator modifiers of type
    @symbol{gdk-modifier-type}}
  @argument[replace]{@em{true} if other accelerators may be deleted upon
    conflicts}
  @begin{return}
    @em{True} if the accelerator could be changed, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Changes the @arg{accel-key} and @arg{accel-mods} currently associated with
    @arg{accel-path}.
  @end{short}
  Due to conflicts with other accelerators, a change may not always be
  possible, @arg{replace} indicates whether other accelerators may be deleted
  to resolve such conflicts. A change will only occur if all conflicts could be
  resolved, which might not be the case if conflicting accelerators are
  locked. Successful changes are indicated by a @em{true} return value.
  @see-class{gtk-accel-map}
  @see-symbol{gdk-modifier-mask}
  @see-function{gtk-accel-map-add-entry}"
  (accel-path :string)
  (accel-key :uint)
  (accel-mods gdk-modifier-type)
  (replace :boolean))

(export 'gtk-accel-map-change-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_load" gtk-accel-map-load) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-29}
  @argument[filename]{a file containing accelerator specifications, in the GLib
    file name encoding}
  Parses a file previously saved with the function @fun{gtk-accel-map-save}
  for accelerator specifications, and propagates them accordingly.
  @see-class{gtk-accel-map}
  @see-function{gtk-accel-map-save}"
  (filename :string))

(export 'gtk-accel-map-load)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_save" gtk-accel-map-save) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-29}
  @argument[filename]{the name of the file to contain accelerator
    specifications, in the GLib file name encoding}
  @begin{short}
    Saves current accelerator specifications, accelerator path, key and
    modifiers, to @arg{filename}.
  @end{short}
  The file is written in a format suitable to be read back in by the function
  @fun{gtk-accel-map-load}.
  @see-class{gtk-accel-map}
  @see-function{gtk-accel-map-load}"
  (filename :string))

(export 'gtk-accel-map-save)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMapForeach ()
;;;
;;; void (*GtkAccelMapForeach) (gpointer data,
;;;                             const gchar *accel_path,
;;;                             guint accel_key,
;;;                             GdkModifierType accel_mods,
;;;                             gboolean changed);
;;;
;;; data :
;;;     User data passed to gtk_accel_map_foreach() or
;;;     gtk_accel_map_foreach_unfiltered()
;;;
;;; accel_path :
;;;     Accel path of the current accelerator
;;;
;;; accel_key :
;;;     Key of the current accelerator
;;;
;;; accel_mods :
;;;     Modifiers of the current accelerator
;;;
;;; changed :
;;;     Changed flag of the accelerator (if TRUE, accelerator has changed during
;;;     runtime and would need to be saved during an accelerator dump)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach ()
;;;
;;; void gtk_accel_map_foreach (gpointer data, GtkAccelMapForeach foreach_func);
;;;
;;; Loops over the entries in the accelerator map whose accel path doesn't match
;;; any of the filters added with gtk_accel_map_add_filter(), and execute
;;; foreach_func on each. The signature of foreach_func is that of
;;; GtkAccelMapForeach, the changed parameter indicates whether this accelerator
;;; was changed during runtime (thus, would need saving during an accelerator
;;; map dump).
;;;
;;; data :
;;;     data to be passed into foreach_func
;;;
;;; foreach_func :
;;;     function to be executed for each accel map entry which is not filtered
;;;     out
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_fd ()
;;;
;;; void gtk_accel_map_load_fd (gint fd);
;;;
;;; Filedescriptor variant of gtk_accel_map_load().
;;;
;;; Note that the file descriptor will not be closed by this function.
;;;
;;; fd :
;;;     a valid readable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save_fd ()
;;;
;;; void gtk_accel_map_save_fd (gint fd);
;;;
;;; Filedescriptor variant of gtk_accel_map_save().
;;;
;;; Note that the file descriptor will not be closed by this function.
;;;
;;; fd :
;;;     a valid writable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_scanner ()
;;;
;;; void gtk_accel_map_load_scanner (GScanner *scanner);
;;;
;;; GScanner variant of gtk_accel_map_load().
;;;
;;; scanner :
;;;     a GScanner which has already been provided with an input file
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_filter ()
;;;
;;; void gtk_accel_map_add_filter (const gchar *filter_pattern);
;;;
;;; Adds a filter to the global list of accel path filters.
;;;
;;; Accel map entries whose accel path matches one of the filters are skipped by
;;; gtk_accel_map_foreach().
;;;
;;; This function is intended for GTK+ modules that create their own menus, but
;;; don't want them to be saved into the applications accelerator map dump.
;;;
;;; filter_pattern :
;;;     a pattern (see GPatternSpec)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach_unfiltered ()
;;;
;;; void gtk_accel_map_foreach_unfiltered (gpointer data,
;;;                                        GtkAccelMapForeach foreach_func);
;;;
;;; Loops over all entries in the accelerator map, and execute foreach_func on
;;; each. The signature of foreach_func is that of GtkAccelMapForeach, the
;;; changed parameter indicates whether this accelerator was changed during
;;; runtime (thus, would need saving during an accelerator map dump).
;;;
;;; data :
;;;     data to be passed into foreach_func
;;;
;;; foreach_func :
;;;     function to be executed for each accel map entry
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_get" gtk-accel-map-get) (g-object gtk-accel-map)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @return{The global @class{gtk-accel-map} object.}
  @begin{short}
    Gets the singleton global @class{gtk-accel-map} object.
  @end{short}
  This object is useful only for notification of changes to the accelerator map
  via the \"changed\" signal; it is not a parameter to the other accelerator
  map functions.

  Since 2.4
  @see-class{gtk-accel-map}")

(export 'gtk-accel-map-get)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lock_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_lock_path" gtk-accel-map-lock-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @argument[accel-path]{a valid accelerator path of type @code{:string}}
  @begin{short}
    Locks the given accelerator path.
  @end{short}
  If the accelerator map does not yet contain an entry for @arg{accel-path}, a
  new one is created.

  Locking an accelerator path prevents its accelerator from being changed
  during runtime. A locked accelerator path can be unlocked by the function
  @fun{gtk-accel-map-unlock-path}. Refer to the function
  @fun{gtk-accel-map-change-entry} for information about runtime accelerator
  changes.

  If called more than once, @arg{accel-path} remains locked until the function
  @fun{gtk-accel-map-unlock-path} has been called an equivalent number of times.

  Note that locking of individual accelerator paths is independent from
  locking the @class{gtk-accel-group} containing them. For runtime accelerator
  changes to be possible both the accelerator path and its
  @class{gtk-accel-group} have to be unlocked.

  Since 2.4
  @see-class{gtk-accel-map}
  @see-class{gtk-accel-group}
  @see-function{gtk-accel-map-unlock-path}
  @see-function{gtk-accel-map-change-entry}"
  (accel-path :string))

(export 'gtk-accel-map-lock-path)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_unlock_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_accel_map_unlock_path" gtk-accel-map-unlock-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-19}
  @argument[accel-path]{a valid accelerator path of type @code{:string}}
  @begin{short}
    Undoes the last call to the function @fun{gtk-accel-map-lock-path} on this
    @arg{accel-path}.
  @end{short}
  Refer to the function @fun{gtk-accel-map-lock-path} for information about
  accelerator path locking.

  Since 2.4
  @see-class{gtk-accel-map}
  @see-function{gtk-accel-map-lock-path}"
  (accel-path :string))

(export 'gtk-accel-map-unlock-path)

;;; --- End of file gtk.accel-map.lisp -----------------------------------------

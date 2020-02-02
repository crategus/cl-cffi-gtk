;;; ----------------------------------------------------------------------------
;;; gtk.icon-theme.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GtkIconTheme
;;;
;;;     Looking up icons by name
;;;
;;; Types and Values
;;;
;;;     GtkIconInfo
;;;     GtkIconTheme
;;;     GtkIconThemeClass
;;;     GtkIconLookupFlags
;;;     GtkIconThemeError
;;;
;;;     GTK_ICON_THEME_ERROR
;;;
;;; Functions
;;;
;;;     gtk_icon_theme_new
;;;     gtk_icon_theme_get_default
;;;     gtk_icon_theme_get_for_screen
;;;     gtk_icon_theme_set_screen
;;;     gtk_icon_theme_set_search_path
;;;     gtk_icon_theme_get_search_path
;;;     gtk_icon_theme_append_search_path
;;;     gtk_icon_theme_prepend_search_path
;;;     gtk_icon_theme_add_resource_path
;;;     gtk_icon_theme_set_custom_theme
;;;     gtk_icon_theme_has_icon
;;;     gtk_icon_theme_lookup_icon
;;;     gtk_icon_theme_lookup_icon_for_scale
;;;     gtk_icon_theme_choose_icon
;;;     gtk_icon_theme_choose_icon_for_scale
;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_lookup_by_gicon_for_scale
;;;     gtk_icon_theme_load_icon
;;;     gtk_icon_theme_load_icon_for_scale
;;;     gtk_icon_theme_load_surface
;;;     gtk_icon_theme_list_contexts
;;;     gtk_icon_theme_list_icons
;;;     gtk_icon_theme_get_icon_sizes
;;;     gtk_icon_theme_get_example_icon_name
;;;     gtk_icon_theme_rescan_if_needed
;;;     gtk_icon_theme_add_builtin_icon
;;;     gtk_icon_info_copy
;;;     gtk_icon_info_free
;;;     gtk_icon_info_new_for_pixbuf
;;;     gtk_icon_info_get_base_size
;;;     gtk_icon_info_get_base_scale
;;;     gtk_icon_info_get_filename
;;;     gtk_icon_info_get_builtin_pixbuf
;;;     gtk_icon_info_load_icon
;;;     gtk_icon_info_load_surface
;;;     gtk_icon_info_load_icon_async
;;;     gtk_icon_info_load_icon_finish
;;;     gtk_icon_info_load_symbolic
;;;     gtk_icon_info_load_symbolic
;;;     gtk_icon_info_load_symbolic_async
;;;     gtk_icon_info_load_symbolic_finish
;;;     gtk_icon_info_load_symbolic_for_style
;;;     gtk_icon_info_load_symbolic_for_context
;;;     gtk_icon_info_load_symbolic_for_context_async
;;;     gtk_icon_info_load_symbolic_for_context_finish
;;;     gtk_icon_info_set_raw_coordinates
;;;     gtk_icon_info_get_embedded_rect
;;;     gtk_icon_info_get_attach_points
;;;     gtk_icon_info_get_display_name
;;;     gtk_icon_info_is_symbolic
;;;
;;; Signals
;;;
;;;     void   changed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIconTheme
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIconInfo
;;; ----------------------------------------------------------------------------

(defcstruct gtk-icon-info)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-icon-info atdoc:*external-symbols*)
 "@version{2013-3-15}
  Contains information found when looking up an icon in an icon theme.")

(export 'gtk-icon-info)

;;; ----------------------------------------------------------------------------
;;; struct GtkIconTheme
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIconTheme" gtk-icon-theme
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_icon_theme_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-icon-theme 'type)
 "@version{2013-3-15}
  @begin{short}
    @sym{gtk-icon-theme} provides a facility for looking up icons by name and
    size.
  @end{short}
  The main reason for using a name rather than simply providing a filename is to
  allow different icons to be used depending on what icon theme is selected by
  the user. The operation of icon themes on Linux and Unix follows the Icon
  Theme Specification. There is a default icon theme, named hicolor where
  applications should install their icons, but more additional application
  themes can be installed as operating system vendors and users choose.

  Named icons are similar to the Themeable Stock Images facility, and the
  distinction between the two may be a bit confusing. A few things to keep in
  mind:
  @begin{itemize}
    @begin{item}
      Stock images usually are used in conjunction with Stock Items(3), such
      as @code{\"gtk-ok\"} or @code{\"gtk-open\"}. Named icons are easier to
      set up and therefore are more useful for new icons that an application
      wants to add, such as application icons or window icons.
    @end{item}
    @begin{item}
      Stock images can only be loaded at the symbolic sizes defined by the
      @symbol{gtk-icon-size} enumeration, or by custom sizes defined by the
      function @fun{gtk-icon-size-register}, while named icons are more flexible
      and any pixel size can be specified.
    @end{item}
    @begin{item}
      Because stock images are closely tied to stock items, and thus to
      actions in the user interface, stock images may come in multiple
      variants for different widget states or writing directions.
    @end{item}
  @end{itemize}
  A good rule of thumb is that if there is a stock image for what you want to
  use, use it, otherwise use a named icon. It turns out that internally stock
  images are generally defined in terms of one or more named icons. An
  example of the more than one case is icons that depend on writing direction;
  @code{\"gtk-go-forward\"} uses the two themed icons
  @code{\"gtk-stock-go-forward-ltr\"} and @code{\"gtk-stock-go-forward-rtl\"}.

  In many cases, named themes are used indirectly, via @class{gtk-image} or
  stock items, rather than directly, but looking up icons directly is also
  simple. The @sym{gtk-icon-theme} object acts as a database of all the icons in
  the current theme. You can create new @sym{gtk-icon-theme} objects, but its
  much more efficient to use the standard icon theme for the @class{gdk-screen}
  so that the icon information is shared with other people looking up icons. In
  the case where the default screen is being used, looking up an icon can be as
  simple as:
  @begin{pre}
   GError *error = NULL;
   GtkIconTheme *icon_theme;
   GdkPixbuf *pixbuf;

   icon_theme = gtk_icon_theme_get_default ();
   pixbuf = gtk_icon_theme_load_icon (icon_theme,
                                      \"my-icon-name\", // icon name
                                      48, // size
                                      0,  // flags
                                      &error);
   if (!pixbuf)
     {
       g_warning (\"Couldn't load icon: %s\", error->message);
       g_error_free (error);
     @}
   else
     {
       // Use the pixbuf
       g_object_unref (pixbuf);
     @}
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (icon-theme)   : Run Last
      @end{pre}
      Emitted when the current icon theme is switched or GTK+ detects that a
      change has occurred in the contents of the current icon theme.
      @begin[code]{table}
        @entry[icon-theme]{The icon theme of @sym{gtk-icon-theme}.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; enum GtkIconLookupFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkIconLookupFlags" gtk-icon-lookup-flags
  (:export t
   :type-initializer "gtk_icon_lookup_flags_get_type")
  (:no-svg           #.(ash 1 0))
  (:force-svg        #.(ash 1 1))
  (:use-builtin      #.(ash 1 2))
  (:generic-fallback #.(ash 1 3))
  (:force-size       #.(ash 1 4))
  #+gtk-3-14
  (:force-regular    #.(ash 1 5))
  #+gtk-3-14
  (:force-symbolic   #.(ash 1 6))
  #+gtk-3-14
  (:dir-ltr          #.(ash 1 7))
  #+gtk-3-14
  (:dir-rtl          #.(ash 1 8))
  )

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-lookup-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-icon-lookup-flags atdoc:*external-symbols*)
 "@version{2013-4-19}
  @short{}
  @begin{pre}
(define-g-flags \"GtkIconLookupFlags\" gtk-icon-lookup-flags
  (:export t
   :type-initializer \"gtk_icon_lookup_flags_get_type\")
  (:no-svg           #.(ash 1 0))
  (:force-svg        #.(ash 1 1))
  (:use-builtin      #.(ash 1 2))
  (:generic-fallback #.(ash 1 3))
  (:force-size       #.(ash 1 4))
  (:force-regular    #.(ash 1 5))
  (:force-symbolic   #.(ash 1 6))
  (:dir-ltr          #.(ash 1 7))
  (:dir-rtl          #.(ash 1 8)))
   @end{pre}
  @begin[code]{table}
    @entry[:no-svg]{Never get SVG icons, even if @class{gdk-pixbuf} supports
      them. Cannot be used together with @code{:force-svg}.}
    @entry[:force-svg]{Get SVG icons, even if @class{gdk-pixbuf} doesn’t support
      them. Cannot be used together with @code{:no-svg}.}
    @entry[:use-builtin]{When passed to the @fun{gtk-icon-theme-lookup-icon}
      function includes builtin icons as well as files. For a builtin icon,
      the @fun{gtk-icon-info-get-filename} functions is NULL and you need to
      call the @fun{gtk-icon-info-get-builtin-pixbuf} function.}
    @entry[:generic-fallback]{Try to shorten icon name at '-' characters before
      looking at inherited themes. This flag is only supported in functions that
      take a single icon name. For more general fallback, see the
      @fun{gtk-icon-theme-choose-icon} function.}
    @entry[:force-size]{Always get the icon scaled to the requested size.}
    @entry[:force-regular]{Try to always load regular icons, even when symbolic
      icon names are given. Since 3.14}
    @entry[:force-symbolic]{Try to always load symbolic icons, even when regular
      icon names are given. Since 3.14}
    @entry[:dir-ltr]{Try to load a variant of the icon for left-to-right
      text direction. Since 3.14}
    @entry[:dir-ltr]{Try to load a variant of the icon for right-to-left text
      direction. Since 3.14}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GTK_ICON_THEME_ERROR
;;;
;;; #define GTK_ICON_THEME_ERROR gtk_icon_theme_error_quark ()
;;;
;;; The GQuark used for GtkIconThemeError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkIconThemeError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIconThemeError" gtk-icon-theme-error
  (:export t
   :type-initializer "gtk_icon_theme_error_get_type")
  (:not-found 0)
  (:failed 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-theme-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-icon-theme-error atdoc:*external-symbols*)
 "@version{2013-3-15}
  @begin{short}
    Error codes for @class{gtk-icon-theme} operations.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkIconThemeError\" gtk-icon-theme-error
  (:export t
   :type-initializer \"gtk_icon_theme_error_get_type\")
  (:not-found 0)
  (:failed 1))
  @end{pre}
  @begin[code]{table}
    @entry[:not-found]{The icon specified does not exist in the theme.}
    @entry[:failed]{An unspecified error occurred.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-theme-new))

(defun gtk-icon-theme-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-19}
  @return{The newly created @class{gtk-icon-theme} object.}
  @begin{short}
    Creates a new icon theme object. Icon theme objects are used to lookup up
    an icon by name in a particular icon theme. Usually, you will want to use
    @fun{gtk-icon-theme-get-default} or @fun{gtk-icon-theme-get-for-screen}
    rather than creating a new icon theme object for scratch.
  @end{short}
  @see-function{gtk-icon-theme-get-default}
  @see-function{gtk-icon-theme-get-for-screen}"
  (make-instance 'gtk-icon-theme))

(export 'gtk-icon-theme-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_get_default" gtk-icon-theme-get-default)
    (g-object gtk-icon-theme)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-15}
  @begin{return}
    A unique @class{gtk-icon-theme} object associated with the default screen.
    This icon theme is associated with the screen and can be used as long as
    the screen is open. Do not ref or unref it.
  @end{return}
  @begin{short}
    Gets the icon theme for the default screen. See the function
    @fun{gtk-icon-theme-get-for-screen}.
  @end{short}
  @see-function{gtk-icon-theme-get-for-screen}")

(export 'gtk-icon-theme-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_for_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_get_for_screen" gtk-icon-theme-get-for-screen)
    (g-object gtk-icon-theme)
 #+cl-cffi-gtk-documentation
 "@version{2014-3-15}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{return}
    A unique @class{gtk-icon-theme} object associated with the given
    @arg{screen}. This icon theme is associated with the @arg{screen} and can
    be used as long as the @arg{screen} is open. Do not ref or unref it.
  @end{return}
  @begin{short}
    Gets the icon theme object associated with @arg{screen}; if this function
    has not previously been called for the given @arg{screen}, a new icon theme
    object will be created and associated with the @arg{screen}. Icon theme
    objects are fairly expensive to create, so using this function is usually a
    better choice than calling the function @fun{gtk-icon-theme-new} and setting
    the screen yourself; by using this function a single icon theme object will
    be shared between users.
  @end{short}
  @see-function{gtk-icon-theme-new}
  @see-function{gtk-icon-theme-get-default}"
  (screen (g-object gdk-screen)))

(export 'gtk-icon-theme-get-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_set_screen ()
;;;
;;; void gtk_icon_theme_set_screen (GtkIconTheme *icon_theme, GdkScreen *screen)
;;;
;;; Sets the screen for an icon theme; the screen is used to track the user's
;;; currently configured icon theme, which might be different for different
;;; screens.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_set_search_path ()
;;;
;;; void gtk_icon_theme_set_search_path (GtkIconTheme *icon_theme,
;;;                                      const gchar *path[],
;;;                                      gint n_elements);
;;;
;;; Sets the search path for the icon theme object. When looking for an icon
;;; theme, GTK+ will search for a subdirectory of one or more of the directories
;;; in path with the same name as the icon theme. (Themes from multiple of the
;;; path elements are combined to allow themes to be extended by adding icons in
;;; the user's home directory.)
;;;
;;; In addition if an icon found isn't found either in the current icon theme or
;;; the default icon theme, and an image file with the right name is found
;;; directly in one of the elements of path, then that image will be used for
;;; the icon name. (This is legacy feature, and new icons should be put into the
;;; default icon theme, which is called DEFAULT_THEME_NAME, rather than directly
;;; on the icon path.)
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; path :
;;;     array of directories that are searched for icon themes
;;;
;;; n_elements :
;;;     number of elements in path.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_search_path ()
;;;
;;; void gtk_icon_theme_get_search_path (GtkIconTheme *icon_theme,
;;;                                      gchar **path[],
;;;                                      gint *n_elements);
;;;
;;; Gets the current search path. See gtk_icon_theme_set_search_path().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; path :
;;;     location to store a list of icon theme path directories or NULL. The
;;;     stored value should be freed with g_strfreev()
;;;
;;; n_elements :
;;;     location to store number of elements in path, or NULL
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_append_search_path ()
;;;
;;; void gtk_icon_theme_append_search_path (GtkIconTheme *icon_theme,
;;;                                         const gchar *path);
;;;
;;; Appends a directory to the search path. See
;;; gtk_icon_theme_set_search_path().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; path :
;;;     directory name to append to the icon path
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_prepend_search_path ()
;;;
;;; void gtk_icon_theme_prepend_search_path (GtkIconTheme *icon_theme,
;;;                                          const gchar *path);
;;;
;;; Prepends a directory to the search path. See
;;; gtk_icon_theme_set_search_path().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; path :
;;;     directory name to prepend to the icon path
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_resource_path ()
;;;
;;; void
;;; gtk_icon_theme_add_resource_path (GtkIconTheme *icon_theme,
;;;                                   const gchar *path);
;;;
;;; Adds a resource path that will be looked at when looking for icons, similar
;;; to search paths.
;;;
;;; This function should be used to make application-specific icons available as
;;; part of the icon theme.
;;;
;;; The resources are considered as part of the hicolor icon theme and must be
;;; located in subdirectories that are defined in the hicolor icon theme, such
;;; as @path/16x16/actions/run.png. Icons that are directly placed in the
;;; resource path instead of a subdirectory are also considered as ultimate
;;; fallback.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; path :
;;;     a resource path
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_set_custom_theme ()
;;;
;;; void gtk_icon_theme_set_custom_theme (GtkIconTheme *icon_theme,
;;;                                       const gchar *theme_name);
;;;
;;; Sets the name of the icon theme that the GtkIconTheme object uses overriding
;;; system configuration. This function cannot be called on the icon theme
;;; objects returned from gtk_icon_theme_get_default() and
;;; gtk_icon_theme_get_for_screen().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; theme_name :
;;;     name of icon theme to use instead of configured theme, or NULL to unset
;;;     a previously set custom theme
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_has_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_has_icon" gtk-icon-theme-has-icon) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @argument[icon-name]{the name of an icon}
  @return{@em{True} if @arg{icon-theme} includes an icon for @arg{icon-name}.}
  @begin{short}
    Checks whether an icon theme includes an icon for a particular name.
  @end{short}
  @see-class{gtk-icon-theme}"
  (icon-theme (g-object gtk-icon-theme))
  (icon-name :string))

(export 'gtk-icon-theme-has-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_lookup_icon" gtk-icon-theme-lookup-icon)
    (:pointer (:struct gtk-icon-info))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object.}
  @argument[icon-name]{the name of the icon to lookup}
  @argument[size]{desired icon size}
  @argument[flags]{flags modifying the behavior of the icon lookup}
  @begin{return}
    A @symbol{gtk-icon-info} structure containing information about the icon,
    or @code{nil} if the icon was not found. Free with the function
    @fun{gtk-icon-info-free}.
  @end{return}
  @begin{short}
    Looks up a named icon and returns a structure containing information such as
    the filename of the icon. The icon can then be rendered into a pixbuf using
    the function @fun{gtk-icon-info-load-icon}. The function
    @fun{gtk-icon-theme-load-icon} combines these two steps if all you need is
    the pixbuf.
  @end{short}
  @see-function{gtk-icon-info-load-icon}
  @see-function{gtk-icon-theme-load-icon}"
  (icon-theme (g-object gtk-icon-theme))
  (icon-name :string)
  (size :int)
  (flags gtk-icon-lookup-flags))

(export 'gtk-icon-theme-lookup-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_icon_for_scale ()
;;;
;;; GtkIconInfo *
;;; gtk_icon_theme_lookup_icon_for_scale (GtkIconTheme *icon_theme,
;;;                                       const gchar *icon_name,
;;;                                       gint size,
;;;                                       gint scale,
;;;                                       GtkIconLookupFlags flags);
;;;
;;; Looks up a named icon for a particular window scale and returns a
;;; GtkIconInfo containing information such as the filename of the icon. The
;;; icon can then be rendered into a pixbuf using gtk_icon_info_load_icon().
;;; (gtk_icon_theme_load_icon() combines these two steps if all you need is the
;;; pixbuf.)
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon_name :
;;;     the name of the icon to lookup
;;;
;;; size :
;;;     desired icon size
;;;
;;; scale :
;;;     the desired scale
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; Returns :
;;;     a GtkIconInfo object containing information about the icon, or NULL if
;;;     the icon wasn’t found.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_choose_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_choose_icon" gtk-icon-theme-choose-icon)
    (:pointer (:struct gtk-icon-info))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @argument[icon-names]{list of icon names to lookup}
  @argument[size]{desired icon size}
  @argument[flags]{flags modifying the behavior of the icon lookup}
  @begin{return}
    A @symbol{gtk-icon-info} structure containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up a named icon and returns a structure containing information such as
    the filename of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the function
  @fun{gtk-icon-info-load-icon}. The function @fun{gtk-icon-theme-load-icon}
  combines these two steps if all you need is the pixbuf.

  If @arg{icon-names} contains more than one name, this function tries them all
  in the given order before falling back to inherited icon themes.
  @see-class{gtk-icon-theme}
  @see-symbol{gtk-icon-info}
  @see-function{gtk-icon-info-load-icon}
  @see-function{gtk-icon-theme-load-icon}"
  (icon-theme (g-object gtk-icon-theme))
  (icon-names g-strv)
  (size :int)
  (flags gtk-icon-lookup-flags))

(export 'gtk-icon-theme-choose-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_choose_icon_for_scale ()
;;;
;;; GtkIconInfo *
;;; gtk_icon_theme_choose_icon_for_scale (GtkIconTheme *icon_theme,
;;;                                       const gchar *icon_names[],
;;;                                       gint size,
;;;                                       gint scale,
;;;                                       GtkIconLookupFlags flags);
;;;
;;; Looks up a named icon for a particular window scale and returns a
;;; GtkIconInfo containing information such as the filename of the icon. The
;;; icon can then be rendered into a pixbuf using gtk_icon_info_load_icon().
;;; (gtk_icon_theme_load_icon() combines these two steps if all you need is the
;;; pixbuf.)
;;;
;;; If icon_names contains more than one name, this function tries them all in
;;; the given order before falling back to inherited icon themes.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon_names :
;;;     NULL-terminated array of icon names to lookup.
;;;
;;; size :
;;;     desired icon size
;;;
;;; scale :
;;;     desired scale
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; Returns :
;;;     a GtkIconInfo object containing information about the icon, or NULL if
;;;     the icon wasn’t found.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_by_gicon ()
;;;
;;; GtkIconInfo * gtk_icon_theme_lookup_by_gicon (GtkIconTheme *icon_theme,
;;;                                               GIcon *icon,
;;;                                               gint size,
;;;                                               GtkIconLookupFlags flags);
;;;
;;; Looks up an icon and returns a structure containing information such as the
;;; filename of the icon. The icon can then be rendered into a pixbuf using
;;; gtk_icon_info_load_icon().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon :
;;;     the GIcon to look up
;;;
;;; size :
;;;     desired icon size
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; Returns :
;;;     a GtkIconInfo structure containing information about the icon, or NULL
;;;     if the icon wasn't found. Free with gtk_icon_info_free()
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_by_gicon_for_scale ()
;;;
;;; GtkIconInfo *
;;; gtk_icon_theme_lookup_by_gicon_for_scale
;;;                                (GtkIconTheme *icon_theme,
;;;                                 GIcon *icon,
;;;                                 gint size,
;;;                                 gint scale,
;;;                                 GtkIconLookupFlags flags);
;;;
;;; Looks up an icon and returns a GtkIconInfo containing information such as
;;; the filename of the icon. The icon can then be rendered into a pixbuf using
;;; gtk_icon_info_load_icon().
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon :
;;;     the GIcon to look up
;;;
;;; size :
;;;     desired icon size
;;;
;;; scale :
;;;     the desired scale
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; Returns :
;;;     a GtkIconInfo containing information about the icon, or NULL if the icon
;;;     wasn’t found. Unref with g_object_unref().
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_load_icon" %gtk-icon-theme-load-icon)
    (g-object gdk-pixbuf)
  (icon-theme (g-object gtk-icon-theme))
  (icon-name :string)
  (size :int)
  (flags gtk-icon-lookup-flags)
  (error :pointer))

(defun gtk-icon-theme-load-icon (icon-theme icon-name size flags)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @argument[icon-name]{the name of the icon to lookup}
  @argument[size]{the desired icon size. The resulting icon may not be exactly
    this size; see the function @fun{gtk-icon-info-load-icon}.}
  @argument[flags]{flags modifying the behavior of the icon lookup}
  @return{The rendered icon.}
  @begin{short}
    Looks up an icon in an icon theme, scales it to the given size and renders
    it into a pixbuf.
  @end{short}
  This is a convenience function; if more details about the icon are needed, use
  the function @fun{gtk-icon-theme-lookup-icon} followed by the function
  @fun{gtk-icon-info-load-icon}.

  Note that you probably want to listen for icon theme changes and update the
  icon. This is usually done by connecting to the \"style-set\" signal.
  If for some reason you do not want to update the icon when the icon theme
  changes, you should consider using the function @fun{gdk-pixbuf-copy} to make
  a private copy of the pixbuf returned by this function. Otherwise GTK+ may
  need to keep the old icon theme loaded, which would be a waste of memory.
  @see-class{gtk-icon-theme}
  @see-function{gtk-icon-info-load-icon}
  @see-function{gtk-icon-theme-lookup-icon}
  @see-function{gdk-pixbuf-copy}"
  (with-g-error (err)
    (%gtk-icon-theme-load-icon icon-theme icon-name size flags err)))

(export 'gtk-icon-theme-load-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_icon_for_scale ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_theme_load_icon_for_scale (GtkIconTheme *icon_theme,
;;;                                     const gchar *icon_name,
;;;                                     gint size,
;;;                                     gint scale,
;;;                                     GtkIconLookupFlags flags,
;;;                                     GError **error);
;;;
;;; Looks up an icon in an icon theme for a particular window scale, scales it
;;; to the given size and renders it into a pixbuf. This is a convenience
;;; function; if more details about the icon are needed, use
;;; gtk_icon_theme_lookup_icon() followed by gtk_icon_info_load_icon().
;;;
;;; Note that you probably want to listen for icon theme changes and update the
;;; icon. This is usually done by connecting to the GtkWidget::style-set signal.
;;; If for some reason you do not want to update the icon when the icon theme
;;; changes, you should consider using gdk_pixbuf_copy() to make a private copy
;;; of the pixbuf returned by this function. Otherwise GTK+ may need to keep the
;;; old icon theme loaded, which would be a waste of memory.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon_name :
;;;     the name of the icon to lookup
;;;
;;; size :
;;;     the desired icon size. The resulting icon may not be exactly this size;
;;;     see gtk_icon_info_load_icon().
;;;
;;; scale :
;;;     desired scale
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; error :
;;;     Location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon. NULL if the
;;;     icon isn’t found.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_surface ()
;;;
;;; cairo_surface_t *
;;; gtk_icon_theme_load_surface (GtkIconTheme *icon_theme,
;;;                              const gchar *icon_name,
;;;                              gint size,
;;;                              gint scale,
;;;                              GdkWindow *for_window,
;;;                              GtkIconLookupFlags flags,
;;;                              GError **error);
;;;
;;; Looks up an icon in an icon theme for a particular window scale, scales it
;;; to the given size and renders it into a cairo surface. This is a convenience
;;; function; if more details about the icon are needed, use
;;; gtk_icon_theme_lookup_icon() followed by gtk_icon_info_load_surface().
;;;
;;; Note that you probably want to listen for icon theme changes and update the
;;; icon. This is usually done by connecting to the GtkWidget::style-set signal.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon_name :
;;;     the name of the icon to lookup
;;;
;;; size :
;;;     the desired icon size. The resulting icon may not be exactly this size;
;;;     see gtk_icon_info_load_icon().
;;;
;;; scale :
;;;     desired scale
;;;
;;; for_window :
;;;     GdkWindow to optimize drawing for, or NULL.
;;;
;;; flags :
;;;     flags modifying the behavior of the icon lookup
;;;
;;; error :
;;;     Location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     cairo_surface_destroy() to release your reference to the icon. NULL if
;;;     the icon isn’t found.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_list_contexts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_list_contexts" gtk-icon-theme-list-contexts)
    (g-list :string)
 #+cl-cffi-gtk-documentation
 "@version{2014-3-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @begin{return}
    A list holding the names of all the contexts in the theme.
  @end{return}
  @begin{short}
    Gets the list of contexts available within the current hierarchy of icon
    themes.
  @end{short}
  @begin[example]{dictionary}
    @begin{pre}
 (gtk-icon-theme-list-contexts (gtk-icon-theme-get-default))
=>(\"International\" \"Emotes\" \"Places\" \"stock\" \"FileSystems\"
   \"Devices\" \"Applications\" \"Actions\" \"Categories\" \"Animations\"
   \"MimeTypes\" \"Stock\" \"Status\" \"Emblems\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk-icon-theme}"
  (icon-theme (g-object gtk-icon-theme)))

(export 'gtk-icon-theme-list-contexts)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_list_icons ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_list_icons" %gtk-icon-theme-list-icons)
    (g-list :string)
  (icon-theme (g-object gtk-icon-theme))
  (context :string))

(defun gtk-icon-theme-list-icons (icon-theme context)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @argument[context]{a string identifying a particular type of icon, or
    @code{nil} to list all icons}
  @begin{return}
    A list holding the names of all the icons in the theme.
  @end{return}
  @begin{short}
    Lists the icons in the current icon theme. Only a subset of the icons can
    be listed by providing a context string. The set of values for the context
    string is system dependent, but will typically include such values as
    \"Applications\" and \"MimeTypes\".
  @end{short}
  @see-class{gtk-icon-theme}"
  (let ((context (if context context (null-pointer))))
    (%gtk-icon-theme-list-icons icon-theme context)))

(export 'gtk-icon-theme-list-icons)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_icon_sizes ()
;;;
;;; gint * gtk_icon_theme_get_icon_sizes (GtkIconTheme *icon_theme,
;;;                                       const gchar *icon_name);
;;;
;;; Returns an array of integers describing the sizes at which the icon is
;;; available without scaling. A size of -1 means that the icon is available in
;;; a scalable format. The array is zero-terminated.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; icon_name :
;;;     the name of an icon
;;;
;;; Returns :
;;;     An newly allocated array describing the sizes at which the icon is
;;;     available. The array should be freed with g_free() when it is no longer
;;;     needed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_example_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_theme_get_example_icon_name"
           gtk-icon-theme-get-example-icon-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-3-15}
  @argument[icon-theme]{a @class{gtk-icon-theme} object}
  @return{The name of an example icon or @code{nil}.}
  @begin{short}
    Gets the name of an icon that is representative of the current theme (for
    instance, to use when presenting a list of themes to the user.)
  @end{short}
  @see-class{gtk-icon-theme}"
  (icon-theme (g-object gtk-icon-theme)))

(export 'gtk-icon-theme-get-example-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_rescan_if_needed ()
;;;
;;; gboolean gtk_icon_theme_rescan_if_needed (GtkIconTheme *icon_theme);
;;;
;;; Checks to see if the icon theme has changed; if it has, any currently cached
;;; information is discarded and will be reloaded next time icon_theme is
;;; accessed.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; Returns :
;;;     TRUE if the icon theme has changed and needed to be reloaded.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_builtin_icon ()
;;;
;;; void gtk_icon_theme_add_builtin_icon (const gchar *icon_name,
;;;                                       gint size,
;;;                                       GdkPixbuf *pixbuf);
;;;
;;; Registers a built-in icon for icon theme lookups. The idea of built-in icons
;;; is to allow an application or library that uses themed icons to function
;;; requiring files to be present in the file system. For instance, the default
;;; images for all of GTK+'s stock icons are registered as built-icons.
;;;
;;; In general, if you use gtk_icon_theme_add_builtin_icon() you should also
;;; install the icon in the icon theme, so that the icon is generally available.
;;;
;;; This function will generally be used with pixbufs loaded via
;;; gdk_pixbuf_new_from_inline().
;;;
;;; Warning
;;;
;;; gtk_icon_theme_add_builtin_icon has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; Use gtk_icon_theme_add_resource_path() to add application-specific icons to
;;; the icon theme.
;;;
;;; icon_name :
;;;     the name of the icon to register
;;;
;;; size :
;;;     the size at which to register the icon (different images can be
;;;     registered for the same icon name at different sizes.)
;;;
;;; pixbuf :
;;;     GdkPixbuf that contains the image to use for icon_name.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_copy ()
;;;
;;; GtkIconInfo * gtk_icon_info_copy (GtkIconInfo *icon_info);
;;;
;;; Make a copy of a GtkIconInfo.
;;;
;;; Warning
;;;
;;; gtk_icon_info_copy has been deprecated since version 3.8 and should not be
;;; used in newly-written code.
;;;
;;; Use g_object_ref()
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     the new GtkIconInfo
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_free ()
;;;
;;; void gtk_icon_info_free (GtkIconInfo *icon_info);
;;;
;;; Free a GtkIconInfo and associated information
;;;
;;; Warning
;;;
;;; gtk_icon_info_free has been deprecated since version 3.8 and should not be
;;; used in newly-written code.
;;;
;;; Use g_object_unref()
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_new_for_pixbuf ()
;;;
;;; GtkIconInfo * gtk_icon_info_new_for_pixbuf (GtkIconTheme *icon_theme,
;;;                                             GdkPixbuf *pixbuf);
;;;
;;; Creates a GtkIconInfo for a GdkPixbuf.
;;;
;;; icon_theme :
;;;     a GtkIconTheme
;;;
;;; pixbuf :
;;;     the pixbuf to wrap in a GtkIconInfo
;;;
;;; Returns :
;;;     a GtkIconInfo
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_base_size ()
;;;
;;; gint gtk_icon_info_get_base_size (GtkIconInfo *icon_info);
;;;
;;; Gets the base size for the icon. The base size is a size for the icon that
;;; was specified by the icon theme creator. This may be different than the
;;; actual size of image; an example of this is small emblem icons that can be
;;; attached to a larger icon. These icons will be given the same base size as
;;; the larger icons to which they are attached.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     the base size, or 0, if no base size is known for the icon.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_base_scale ()
;;;
;;; gint gtk_icon_info_get_base_scale (GtkIconInfo *icon_info);
;;;
;;; Gets the base scale for the icon. The base scale is a scale for the icon
;;; that was specified by the icon theme creator. For instance an icon drawn for
;;; a high-dpi screen with window scale 2 for a base size of 32 will be 64
;;; pixels tall and have a base scale of 2.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     the base scale
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_info_get_filename" gtk-icon-info-get-filename) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[icon-info]{a @symbol{gtk-icon-info} structure}
  @begin{return}
   The filename for the icon, or @code{nil} if the function
   @fun{gtk-icon-info-get-builtin-pixbuf} should be used instead.
  @end{return}
  @begin{short}
    Gets the filename for the icon.
  @end{short}
  If the @code{:use-builtin} flag was passed to the function
  @fun{gtk-icon-theme-lookup-icon}, there may be no filename if a builtin icon
  is returned; in this case, you should use the function
  @fun{gtk-icon-info-get-builtin-pixbuf}.
  @begin[Example]{dictionary}
    @begin{pre}
 (gtk-icon-theme-lookup-icon (gtk-icon-theme-get-default)
                             \"battery-charged\" 0 0)
=> (SB-SYS:INT-SAP #X080CAD88)
 (gtk-icon-info-get-filename *)
=> \"/usr/share/icons/ubuntu-mono-light/status/16/battery-charged.svg\"
    @end{pre}
  @end{dictionary}
  @see-symbol{gtk-icon-info}
  @see-function{gtk-icon-info-get-builtin-pixbuf}
  @see-function{gtk-icon-theme-lookup-icon}"
  (icon-info (:pointer (:struct gtk-icon-info))))

(export 'gtk-icon-info-get-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_builtin_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_info_get_builtin_pixbuf" gtk-icon-info-get-builtin-pixbuf)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[icon-info]{a @symbol{gtk-icon-info} structure}
  @return{The built-in image pixbuf, or @code{nil}.}
  @begin{short}
    Gets the built-in image for this icon, if any.
  @end{short}
  To allow GTK+ to use built in icon images, you must pass the
  @code{:use-builtin} to the function @fun{gtk-icon-theme-lookup-icon}.
  @begin[Warning]{dictionary}
    The @sym{gtk-icon-info-get-builtin-pixbuf} function has been deprecated
    since version 3.14 and should not be used in newly-written code. Use the
    @fun{gtk-icon-theme-add-resource-path} function instead of builtin icons.
  @end{dictionary}
  @see-symbol{gtk-icon-info}
  @see-function{gtk-icon-theme-lookup-icon}"
  (icon-info (:pointer (:struct gtk-icon-info))))

(export 'gtk-icon-info-get-builtin-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_info_load_icon" %gtk-icon-info-load-icon)
    (g-object gdk-pixbuf)
  (icon-info (:pointer (:struct gtk-icon-info)))
  (error :pointer))

(defun gtk-icon-info-load-icon (icon-info)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[icon-info]{a @symbol{gtk-icon-info} structure from the function
    @fun{gtk-icon-theme-lookup-icon}}
  @return{The rendered icon.}
  @begin{short}
    Renders an icon previously looked up in an icon theme using the function
    @fun{gtk-icon-theme-lookup-icon}; the size will be based on the size passed
    to the function @fun{gtk-icon-theme-lookup-icon}.
  @end{short}
  Note that the resulting pixbuf may not be exactly this size; an icon theme may
  have icons that differ slightly from their nominal sizes, and in addition GTK+
  will avoid scaling icons that it considers sufficiently close to the requested
  size or for which the source image would have to be scaled up too far. This
  maintains sharpness. This behaviour can be changed by passing the
  @code{:force-size} flag when obtaining the @symbol{gtk-icon-info}. If this
  flag has been specified, the pixbuf returned by this function will be scaled
  to the exact size.
  @see-symbol{gtk-icon-info}
  @see-function{gtk-icon-theme-lookup-icon}"
  (with-g-error (err)
    (%gtk-icon-info-load-icon icon-info err)))

(export 'gtk-icon-info-load-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_surface ()
;;;
;;; cairo_surface_t *
;;; gtk_icon_info_load_surface (GtkIconInfo *icon_info,
;;;                             GdkWindow *for_window,
;;;                             GError **error);
;;;
;;; Renders an icon previously looked up in an icon theme using
;;; gtk_icon_theme_lookup_icon(); the size will be based on the size passed to
;;; gtk_icon_theme_lookup_icon(). Note that the resulting surface may not be
;;; exactly this size; an icon theme may have icons that differ slightly from
;;; their nominal sizes, and in addition GTK+ will avoid scaling icons that it
;;; considers sufficiently close to the requested size or for which the source
;;; image would have to be scaled up too far. (This maintains sharpness.). This
;;; behaviour can be changed by passing the GTK_ICON_LOOKUP_FORCE_SIZE flag when
;;; obtaining the GtkIconInfo. If this flag has been specified, the pixbuf
;;; returned by this function will be scaled to the exact size.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; for_window :
;;;     GdkWindow to optimize drawing for, or NULL.
;;;
;;; error :
;;;     location for error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     cairo_surface_destroy() to release your reference to the icon.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon_async ()
;;;
;;; void
;;; gtk_icon_info_load_icon_async (GtkIconInfo *icon_info,
;;;                                GCancellable *cancellable,
;;;                                GAsyncReadyCallback callback,
;;;                                gpointer user_data);
;;;
;;; Asynchronously load, render and scale an icon previously looked up from the
;;; icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_icon() which is the synchronous
;;; version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_icon_finish (GtkIconInfo *icon_info,
;;;                                 GAsyncResult *res,
;;;                                 GError **error);
;;;
;;; Finishes an async icon load, see gtk_icon_info_load_icon_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic ()
;;;
;;; GdkPixbuf * gtk_icon_info_load_symbolic (GtkIconInfo *icon_info,
;;;                                          const GdkRGBA *fg,
;;;                                          const GdkRGBA *success_color,
;;;                                          const GdkRGBA *warning_color,
;;;                                          const GdkRGBA *error_color,
;;;                                          gboolean *was_symbolic,
;;;                                          GError **error);
;;;
;;; Loads an icon, modifying it to match the system colours for the foreground,
;;; success, warning and error colors provided. If the icon is not a symbolic
;;; one, the function will return the result from gtk_icon_info_load_icon().
;;;
;;; This allows loading symbolic icons that will match the system theme.
;;;
;;; Unless you are implementing a widget, you will want to use
;;; g_themed_icon_new_with_default_fallbacks() to load the icon.
;;;
;;; As implementation details, the icon loaded needs to be of SVG type, contain
;;; the "symbolic" term as the last component of the icon name, and use the
;;; 'fg', 'success', 'warning' and 'error' CSS styles in the SVG file itself.
;;;
;;; See the Symbolic Icons spec for more information about symbolic icons.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; fg :
;;;     a GdkRGBA representing the foreground color of the icon
;;;
;;; success_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color
;;;
;;; warning_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color
;;;
;;; error_color :
;;;     a GdkRGBA representing the error color of the icon or NULL to use the
;;;     default color (allow-none)
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it
;;;
;;; error :
;;;     location to store error information on failure, or NULL
;;;
;;; Returns :
;;;     a GdkPixbuf representing the loaded icon
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_async ()
;;;
;;; void
;;; gtk_icon_info_load_symbolic_async (GtkIconInfo *icon_info,
;;;                                    const GdkRGBA *fg,
;;;                                    const GdkRGBA *success_color,
;;;                                    const GdkRGBA *warning_color,
;;;                                    const GdkRGBA *error_color,
;;;                                    GCancellable *cancellable,
;;;                                    GAsyncReadyCallback callback,
;;;                                    gpointer user_data);
;;;
;;; Asynchronously load, render and scale a symbolic icon previously looked up
;;; from the icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_symbolic() which is the synchronous
;;; version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; fg :
;;;     a GdkRGBA representing the foreground color of the icon
;;;
;;; success_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color.
;;;
;;; warning_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color.
;;;
;;; error_color :
;;;     a GdkRGBA representing the error color of the icon or NULL to use the
;;;     default color (allow-none).
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_symbolic_finish (GtkIconInfo *icon_info,
;;;                                     GAsyncResult *res,
;;;                                     gboolean *was_symbolic,
;;;                                     GError **error);
;;;
;;; Finishes an async icon load, see gtk_icon_info_load_symbolic_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it.
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_style ()
;;;
;;; GdkPixbuf * gtk_icon_info_load_symbolic_for_style (GtkIconInfo *icon_info,
;;;                                                    GtkStyle *style,
;;;                                                    GtkStateType state,
;;;                                                    gboolean *was_symbolic,
;;;                                                    GError **error);
;;;
;;; Warning
;;;
;;; gtk_icon_info_load_symbolic_for_style has been deprecated since version 3.0
;;; and should not be used in newly-written code. Use
;;; gtk_icon_info_load_symbolic_for_context() instead
;;;
;;; Loads an icon, modifying it to match the system colours for the foreground,
;;; success, warning and error colors provided. If the icon is not a symbolic
;;; one, the function will return the result from gtk_icon_info_load_icon().
;;;
;;; This allows loading symbolic icons that will match the system theme.
;;;
;;; See gtk_icon_info_load_symbolic() for more details.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; style :
;;;     a GtkStyle to take the colors from
;;;
;;; state :
;;;     the widget state to use for colors
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it
;;;
;;; error :
;;;     location to store error information on failure, or NULL
;;;
;;; Returns :
;;;     a GdkPixbuf representing the loaded icon
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context ()
;;;
;;; GdkPixbuf * gtk_icon_info_load_symbolic_for_context
;;;                                                   (GtkIconInfo *icon_info,
;;;                                                    GtkStyleContext *context,
;;;                                                    gboolean *was_symbolic,
;;;                                                    GError **error);
;;;
;;; Loads an icon, modifying it to match the system colors for the foreground,
;;; success, warning and error colors provided. If the icon is not a symbolic
;;; one, the function will return the result from gtk_icon_info_load_icon().
;;; This function uses the regular foreground color and the symbolic colors with
;;; the names "success_color", "warning_color" and "error_color" from the
;;; context.
;;;
;;; This allows loading symbolic icons that will match the system theme.
;;;
;;; See gtk_icon_info_load_symbolic() for more details.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it
;;;
;;; error :
;;;     location to store error information on failure, or NULL
;;;
;;; Returns :
;;;     a GdkPixbuf representing the loaded icon
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context_async ()
;;;
;;; void
;;; gtk_icon_info_load_symbolic_for_context_async
;;;                                (GtkIconInfo *icon_info,
;;;                                 GtkStyleContext *context,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Asynchronously load, render and scale a symbolic icon previously looked up
;;; from the icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_symbolic_for_context() which is the
;;; synchronous version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_symbolic_for_context_finish
;;;                                (GtkIconInfo *icon_info,
;;;                                 GAsyncResult *res,
;;;                                 gboolean *was_symbolic,
;;;                                 GError **error);
;;;
;;; Finishes an async icon load, see
;;; gtk_icon_info_load_symbolic_for_context_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it.
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_set_raw_coordinates ()
;;;
;;; void gtk_icon_info_set_raw_coordinates (GtkIconInfo *icon_info,
;;;                                         gboolean raw_coordinates);
;;;
;;; Sets whether the coordinates returned by gtk_icon_info_get_embedded_rect()
;;; and gtk_icon_info_get_attach_points() should be returned in their original
;;; form as specified in the icon theme, instead of scaled appropriately for the
;;; pixbuf returned by gtk_icon_info_load_icon().
;;;
;;; Raw coordinates are somewhat strange; they are specified to be with respect
;;; to the unscaled pixmap for PNG and XPM icons, but for SVG icons, they are in
;;; a 1000x1000 coordinate space that is scaled to the final size of the icon.
;;; You can determine if the icon is an SVG icon by using
;;; gtk_icon_info_get_filename(), and seeing if it is non-NULL and ends in
;;; '.svg'.
;;;
;;; This function is provided primarily to allow compatibility wrappers for
;;; older API's, and is not expected to be useful for applications.
;;;
;;; Warning
;;;
;;; gtk_icon_info_set_raw_coordinates has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; Embedded rectangles and attachment points are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; raw_coordinates :
;;;     whether the coordinates of embedded rectangles and attached points
;;;     should be returned in their original (unscaled) form.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_embedded_rect ()
;;;
;;; gboolean gtk_icon_info_get_embedded_rect (GtkIconInfo *icon_info,
;;;                                           GdkRectangle *rectangle);
;;;
;;; Gets the coordinates of a rectangle within the icon that can be used for
;;; display of information such as a preview of the contents of a text file. See
;;; gtk_icon_info_set_raw_coordinates() for further information about the
;;; coordinate system.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_embedded_rect has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; Embedded rectangles are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; rectangle :
;;;     GdkRectangle in which to store embedded rectangle coordinates;
;;;     coordinates are only stored when this function returns TRUE
;;;
;;; Returns :
;;;     TRUE if the icon has an embedded rectangle
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_attach_points ()
;;;
;;; gboolean gtk_icon_info_get_attach_points (GtkIconInfo *icon_info,
;;;                                           GdkPoint **points,
;;;                                           gint *n_points);
;;;
;;; Fetches the set of attach points for an icon. An attach point is a location
;;; in the icon that can be used as anchor points for attaching emblems or
;;; overlays to the icon.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_attach_points has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; Attachment points are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; points :
;;;     location to store pointer to an array of points, or NULL free the array
;;;     of points with g_free()
;;;
;;; n_points :
;;;     location to store the number of points in points, or NULL
;;;
;;; Returns :
;;;     TRUE if there are any attach points for the icon.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_display_name ()
;;;
;;; const gchar * gtk_icon_info_get_display_name (GtkIconInfo *icon_info);
;;;
;;; Gets the display name for an icon. A display name is a string to be used in
;;; place of the icon name in a user visible context like a list of icons.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_display_name has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; Display names are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     the display name for the icon or NULL, if the icon doesn't have a
;;;     specified display name. This value is owned icon_info and must not be
;;;     modified or free.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_is_symbolic ()
;;;
;;; gboolean gtk_icon_info_is_symbolic (GtkIconInfo *icon_info);
;;;
;;; Checks if the icon is symbolic or not. This currently uses only the file
;;; name and not the file contents for determining this. This behaviour may
;;; change in the future.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     TRUE if the icon is symbolic, FALSE otherwise
;;;
;;; Since 3.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.icon-theme.lisp ----------------------------------------

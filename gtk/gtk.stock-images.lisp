;;; ----------------------------------------------------------------------------
;;; gtk.stock-images.lisp
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
;;; Themeable Stock Images
;;;
;;; Manipulating stock icons
;;;
;;; Synopsis
;;;
;;;     GtkIconSource
;;;     GtkIconFactory
;;;     GtkIconSet
;;;     GtkIconSize
;;;
;;;     gtk_icon_source_copy
;;;     gtk_icon_source_free
;;;
;;;     gtk_icon_factory_add
;;;     gtk_icon_factory_add_default
;;;     gtk_icon_factory_lookup
;;;     gtk_icon_factory_lookup_default
;;;     gtk_icon_factory_new
;;;     gtk_icon_factory_remove_default
;;;
;;;     gtk_icon_set_add_source
;;;     gtk_icon_set_copy
;;;     gtk_icon_set_new
;;;     gtk_icon_set_new_from_pixbuf
;;;     gtk_icon_set_ref
;;;     gtk_icon_set_render_icon
;;;     gtk_icon_set_render_icon_pixbuf
;;;     gtk_icon_set_unref
;;;
;;;     gtk_icon_size_lookup
;;;     gtk_icon_size_lookup_for_settings
;;;     gtk_icon_size_register
;;;     gtk_icon_size_register_alias
;;;     gtk_icon_size_from_name
;;;     gtk_icon_size_get_name
;;;     gtk_icon_set_get_sizes
;;;
;;;     gtk_icon_source_get_direction
;;;     gtk_icon_source_get_direction_wildcarded
;;;     gtk_icon_source_get_filename
;;;     gtk_icon_source_get_pixbuf
;;;     gtk_icon_source_get_icon_name
;;;     gtk_icon_source_get_size
;;;     gtk_icon_source_get_size_wildcarded
;;;     gtk_icon_source_get_state
;;;     gtk_icon_source_get_state_wildcarded
;;;     gtk_icon_source_new
;;;     gtk_icon_source_set_direction
;;;     gtk_icon_source_set_direction_wildcarded
;;;     gtk_icon_source_set_filename
;;;     gtk_icon_source_set_pixbuf
;;;     gtk_icon_source_set_icon_name
;;;     gtk_icon_source_set_size
;;;     gtk_icon_source_set_size_wildcarded
;;;     gtk_icon_source_set_state
;;;     gtk_icon_source_set_state_wildcarded
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkIconFactory
;;;
;;;   GBoxed
;;;    +----GtkIconSet
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIconSource
;;; ----------------------------------------------------------------------------

(glib::at-init () (foreign-funcall "gtk_icon_source_get_type" :int))

(define-g-boxed-opaque gtk-icon-source "GtkIconSource"
  :alloc (gtk-icon-source-new))

(export 'gtk-icon-source)

;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-icon-source gtk-icon-source-filename
  :reader "gtk_icon_source_get_filename"
  :writer "gtk_icon_source_set_filename"
  :type (:string :free-from-foreign nil))

(export 'gtk-icon-source-filename)

(define-boxed-opaque-accessor gtk-icon-source gtk-icon-source-icon-name
  :reader "gtk_icon_source_get_icon_name"
  :writer "gtk_icon_source_set_icon_name"
  :type (:string :free-from-foreign nil))

(export 'gtk-icon-source-icon-name)

;;; ----------------------------------------------------------------------------
;;; struct GtkIconFactory
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIconFactory" gtk-icon-factory
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_icon_factory_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-icon-factory 'type)
 "@version{2013-3-18}
  @begin{short}
    Browse the available stock icons in the list of stock IDs found here.
  @end{short}

  An icon factory manages a collection of @class{gtk-icon-set}; a
  @class{gtk-icon-set} manages a set of variants of a particular icon, i. e. a
  @class{gtk-icon-set} contains variants for different sizes and widget states.
  Icons in an icon factory are named by a stock ID, which is a simple string
  identifying the icon. Each @class{gtk-style} has a list of
  @class{gtk-icon-factory} derived from the current theme; those icon
  factories are consulted first when searching for an icon. If the theme
  doesn't set a particular icon, GTK+ looks for the icon in a list of default
  icon factories, maintained by @fun{gtk-icon-factory-add-default} and
  @fun{gtk-icon-factory-remove-default}. Applications with icons should add a
  default icon factory with their icons, which will allow themes to override
  the icons for the application.

  To display an icon, always use @fun{gtk-style-lookup-icon-set} on the widget
  that will display the icon, or the convenience function
  @fun{gtk-widget-render-icon}. These functions take the theme into account when
  looking up the icon to use for a given stock ID.

  @subheading{GtkIconFactory as GtkBuildable}
  @sym{gtk-icon-factory} supports a custom @code{<sources>} element, which can
  contain multiple @code{<source>} elements. The following attributes are
  allowed:
  @begin[code]{table}
    @entry[stock-id]{The stock id of the source, a string. This attribute is
      mandatory.}
    @entry[filename]{The filename of the source, a string. This attribute is
      optional.}
    @entry[icon-name]{The icon name for the source, a string. This attribute is
      optional.}
    @entry[size]{Size of the icon, a @symbol{gtk-icon-size} enum value. This
      attribute is optional.}
    @entry[direction]{Direction of the source, a @symbol{gtk-text-direction}
      enum value. This attribute is optional.}
    @entry[state]{State of the source, a @symbol{gtk-state-type} enum value.
      This attribute is optional.}
  @end{table}
  @b{Example:} A @class{gtk-icon-factory} UI definition fragment.
  @begin{pre}
 <object class=\"GtkIconFactory\" id=\"iconfactory1\">
   <sources>
     <source stock-id=\"apple-red\" filename=\"apple-red.png\"/>
   </sources>
 </object>
 <object class=\"GtkWindow\" id=\"window1\">
   <child>
     <object class=\"GtkButton\" id=\"apple_button\">
       <property name=\"label\">apple-red</property>
       <property name=\"use-stock\">True</property>
     </object>
   </child>
 </object>
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GtkIconSet
;;;
;;; typedef struct _GtkIconSet GtkIconSet;
;;; ----------------------------------------------------------------------------

(glib::at-init () (foreign-funcall "gtk_icon_set_get_type" :int))

(defcfun gtk-icon-set-new :pointer)

(define-g-boxed-opaque gtk-icon-set "GtkIconSet"
  :alloc (gtk-icon-set-new))

(export 'gtk-icon-set)

;;; ----------------------------------------------------------------------------
;;; enum GtkIconSize
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIconSize" gtk-icon-size
  (:export t
   :type-initializer "gtk_icon_size_get_type")
  (:invalid 0)
  (:menu 1)
  (:small-toolbar 2)
  (:large-toolbar 3)
  (:button 4)
  (:dnd 5)
  (:dialog 6))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-size atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-icon-size atdoc:*external-symbols*)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-8}
  @short{}
  @begin{pre}
(define-g-enum \"GtkIconSize\" gtk-icon-size
  (:export t
   :type-initializer \"gtk_icon_size_get_type\")
  (:invalid 0)
  (:menu 1)
  (:small-toolbar 2)
  (:large-toolbar 3)
  (:button 4)
  (:dnd 5)
  (:dialog 6))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_copy ()
;;;
;;; GtkIconSource * gtk_icon_source_copy (const GtkIconSource *source);
;;;
;;; Creates a copy of source; mostly useful for language bindings.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     a new GtkIconSource
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_free ()
;;;
;;; void gtk_icon_source_free (GtkIconSource *source);
;;;
;;; Frees a dynamically-allocated icon source, along with its filename, size,
;;; and pixbuf fields if those are not NULL.
;;;
;;; source :
;;;     a GtkIconSource
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_add" gtk-icon-factory-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-8}
  @argument[factory]{a @class{GtkIconFactory} object}
  @argument[stock-id]{icon name}
  @argument[icon-set]{icon set}
  @begin{short}
    Adds the given @arg{icon-set} to the icon factory, under the name
    @arg{stock-id}.
  @end{short}
  @arg{stock-id} should be namespaced for your application, e. g.
  \"myapp-whatever-icon\". Normally applications create a
  @class{gtk-icon-factory}, then add it to the list of default factories with
  @fun{gtk-icon-factory-add-default}. Then they pass the @arg{stock-id} to
  widgets such as @class{gtk-image} to display the icon. Themes can provide an
  icon with the same name (such as \"myapp-whatever-icon\") to override your
  application's default icons. If an icon already existed in factory for
  @arg{stock-id}, it is unreferenced and replaced with the new icon_set.
  @see-function{gtk-icon-factory-add-default}"
  (factory (g-object gtk-icon-factory))
  (stock-id :string)
  (icon-set (g-boxed-foreign gtk-icon-set)))

(export 'gtk-icon-factory-add)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_add_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_add_default" gtk-icon-factory-add-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @argument[factory]{a @class{gtk-icon-factory} object}
  @begin{short}
    Adds an icon factory to the list of icon factories searched by
    @fun{gtk-style-lookup-icon-set}. This means that, for example,
    @fun{gtk-image-new-from-stock} will be able to find icons in factory. There
    will normally be an icon factory added for each library or application that
    comes with icons. The default icon factories can be overridden by themes.
  @end{short}
  @see-function{gtk-style-lookup-icon-set}
  @see-function{gtk-image-new-from-stock}"
  (factory (g-object icon-factory)))

(export 'gtk-icon-factory-add-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_lookup" gtk-icon-factory-lookup)
    (g-boxed-foreign gtk-icon-set :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @argument[factory]{a @class{gtk-icon-factory} object}
  @argument[stock-id]{an icon name}
  @return{The icon set of @arg{stock-id}.}
  @begin{short}
    Looks up @arg{stock-id} in the icon factory, returning an icon set if found,
    otherwise @code{nil}. For display to the user, you should use
    @fun{gtk-style-lookup-icon-set} on the @class{gtk-style} for the widget that
    will display the icon, instead of using this function directly, so that
    themes are taken into account.
  @end{short}"
  (factory (g-object gtk-icon-factory))
  (stock-id :string))

(export 'gtk-icon-factory-lookup)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_lookup_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_lookup_default" gtk-icon-factory-lookup-default)
    (g-boxed-foreign gtk-icon-set :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @argument[stock-id]{an icon name}
  @return{a @class{gtk-icon-set}, or @code{nil}}
  @begin{short}
    Looks for an icon in the list of default icon factories. For display to the
    user, you should use @fun{gtk-style-lookup-icon-set} on the
    @class{gtk-style} for the widget that will display the icon, instead of
    using this function directly, so that themes are taken into account.
  @end{short}"
  (stock-id :string))

(export 'gtk-icon-factory-lookup-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_new ()
;;;
;;; GtkIconFactory * gtk_icon_factory_new (void);
;;;
;;; Creates a new GtkIconFactory. An icon factory manages a collection of
;;; GtkIconSets; a GtkIconSet manages a set of variants of a particular icon
;;; (i.e. a GtkIconSet contains variants for different sizes and widget states).
;;; Icons in an icon factory are named by a stock ID, which is a simple string
;;; identifying the icon. Each GtkStyle has a list of GtkIconFactorys derived
;;; from the current theme; those icon factories are consulted first when
;;; searching for an icon. If the theme doesn't set a particular icon, GTK+
;;; looks for the icon in a list of default icon factories, maintained by
;;; gtk_icon_factory_add_default() and gtk_icon_factory_remove_default().
;;; Applications with icons should add a default icon factory with their icons,
;;; which will allow themes to override the icons for the application.
;;;
;;; Returns :
;;;     a new GtkIconFactory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_remove_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_remove_default" gtk-icon-factory-remove-default)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @argument[factory]{a @class{gtk-icon-factory} previously added with
    @fun{gtk-icon-factory-add-default}}
  Removes an icon factory from the list of default icon factories. Not
  normally used; you might use it for a library that can be unloaded or shut
  down."
  (factory (g-object gtk-icon-factory)))

(export 'gtk-icon-factory-remove-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_add_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_add_source" gtk-icon-set-add-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-8}
  @argument[icon-set]{a @class{gtk-icon-set} object}
  @argument[source]{a @class{gtk-icon-source} object}
  @begin{short}
    Icon sets have a list of @class{gtk-icon-source}, which they use as base
    icons for rendering icons in different states and sizes. Icons are scaled,
    made to look insensitive, etc. in @fun{gtk-icon-set-render-icon}, but
    @class{gtk-icon-set} needs base images to work with. The base images and
    when to use them are described by a @class{gtk-icon-source}.
  @end{short}

  This function copies source, so you can reuse the same source immediately
  without affecting the icon set.

  An example of when you'd use this function: a web browser's \"Back to
  Previous Page\" icon might point in a different direction in Hebrew and in
  English; it might look different when insensitive; and it might change size
  depending on toolbar mode (small/large icons). So a single icon set would
  contain all those variants of the icon, and you might add a separate source
  for each one.

  You should nearly always add a \"default\" icon source with all fields
  wildcarded, which will be used as a fallback if no more specific source
  matches. @class{gtk-icon-set} always prefers more specific icon sources to
  more generic icon sources. The order in which you add the sources to the icon
  set does not matter.

  @fun{gtk-icon-set-new-from-pixbuf} creates a new icon set with a default icon
  source based on the given pixbuf."
  (icon-set (g-boxed-foreign gtk-icon-set))
  (source (g-boxed-foreign gtk-icon-source)))

(export 'gtk-icon-set-add-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_copy ()
;;;
;;; GtkIconSet * gtk_icon_set_copy (GtkIconSet *icon_set);
;;;
;;; Copies icon_set by value.
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; Returns :
;;;     a new GtkIconSet identical to the first.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_new ()
;;;
;;; GtkIconSet * gtk_icon_set_new (void);
;;;
;;; Creates a new GtkIconSet. A GtkIconSet represents a single icon in various
;;; sizes and widget states. It can provide a GdkPixbuf for a given size and
;;; state on request, and automatically caches some of the rendered GdkPixbuf
;;; objects.
;;;
;;; Normally you would use gtk_widget_render_icon_pixbuf() instead of using
;;; GtkIconSet directly. The one case where you'd use GtkIconSet is to create
;;; application-specific icon sets to place in a GtkIconFactory.
;;;
;;; Returns :
;;;     a new GtkIconSet
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_new_from_pixbuf ()
;;;
;;; GtkIconSet * gtk_icon_set_new_from_pixbuf (GdkPixbuf *pixbuf);
;;;
;;; Creates a new GtkIconSet with pixbuf as the default/fallback source image.
;;; If you don't add any additional GtkIconSource to the icon set, all variants
;;; of the icon will be created from pixbuf, using scaling, pixelation, etc. as
;;; required to adjust the icon size or make the icon look
;;; insensitive/prelighted.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; Returns :
;;;     a new GtkIconSet
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_ref ()
;;;
;;; GtkIconSet * gtk_icon_set_ref (GtkIconSet *icon_set);
;;;
;;; Increments the reference count on icon_set.
;;;
;;; icon_set :
;;;     a GtkIconSet.
;;;
;;; Returns :
;;;     icon_set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_render_icon ()
;;;
;;; GdkPixbuf * gtk_icon_set_render_icon (GtkIconSet *icon_set,
;;;                                       GtkStyle *style,
;;;                                       GtkTextDirection direction,
;;;                                       GtkStateType state,
;;;                                       GtkIconSize size,
;;;                                       GtkWidget *widget,
;;;                                       const gchar *detail);
;;;
;;; Warning
;;;
;;; gtk_icon_set_render_icon has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_icon_set_render_icon_pixbuf()
;;; instead
;;;
;;; Renders an icon using gtk_style_render_icon(). In most cases,
;;; gtk_widget_render_icon() is better, since it automatically provides most of
;;; the arguments from the current widget settings. This function never returns
;;; NULL; if the icon can't be rendered (perhaps because an image file fails to
;;; load), a default "missing image" icon will be returned instead.
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; style :
;;;     a GtkStyle associated with widget, or NULL
;;;
;;; direction :
;;;     text direction
;;;
;;; state :
;;;     widget state
;;;
;;; size :
;;;     icon size. A size of (GtkIconSize)-1 means render at the size of the
;;;     source and don't scale
;;;
;;; widget :
;;;     widget that will display the icon, or NULL. The only use that is
;;;     typically made of this is to determine the appropriate GdkScreen.
;;;
;;; detail :
;;;     detail to pass to the theme engine, or NULL. Note that passing a detail
;;;     of anything but NULL will disable caching.
;;;
;;; Returns :
;;;     a GdkPixbuf to be displayed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_render_icon_pixbuf ()
;;;
;;; GdkPixbuf * gtk_icon_set_render_icon_pixbuf (GtkIconSet *icon_set,
;;;                                              GtkStyleContext *context,
;;;                                              GtkIconSize size);
;;;
;;; Renders an icon using gtk_render_icon_pixbuf(). In most cases,
;;; gtk_widget_render_icon_pixbuf() is better, since it automatically provides
;;; most of the arguments from the current widget settings. This function never
;;; returns NULL; if the icon can't be rendered (perhaps because an image file
;;; fails to load), a default "missing image" icon will be returned instead.
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; size :
;;;     icon size. A size of (GtkIconSize)-1 means render at the size of the
;;;     source and don't scale
;;;
;;; Returns :
;;;     a GdkPixbuf to be displayed
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_unref ()
;;;
;;; void gtk_icon_set_unref (GtkIconSet *icon_set);
;;;
;;; Decrements the reference count on icon_set, and frees memory if the
;;; reference count reaches 0.
;;;
;;; icon_set :
;;;     a GtkIconSet
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_lookup ()
;;;
;;; gboolean gtk_icon_size_lookup (GtkIconSize size, gint *width, gint *height);
;;;
;;; Obtains the pixel size of a semantic icon size, possibly modified by user
;;; preferences for the default GtkSettings. (See
;;; gtk_icon_size_lookup_for_settings().) Normally size would be
;;; GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_BUTTON, etc. This function isn't normally
;;; needed, gtk_widget_render_icon_pixbuf() is the usual way to get an icon for
;;; rendering, then just look at the size of the rendered pixbuf. The rendered
;;; pixbuf may not even correspond to the width/height returned by
;;; gtk_icon_size_lookup(), because themes are free to render the pixbuf however
;;; they like, including changing the usual size.
;;;
;;; size :
;;;     an icon size
;;;
;;; width :
;;;     location to store icon width
;;;
;;; height :
;;;     location to store icon height
;;;
;;; Returns :
;;;     TRUE if size was a valid size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_lookup_for_settings ()
;;;
;;; gboolean gtk_icon_size_lookup_for_settings (GtkSettings *settings,
;;;                                             GtkIconSize size,
;;;                                             gint *width,
;;;                                             gint *height);
;;;
;;; Obtains the pixel size of a semantic icon size, possibly modified by user
;;; preferences for a particular GtkSettings. Normally size would be
;;; GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_BUTTON, etc. This function isn't normally
;;; needed, gtk_widget_render_icon_pixbuf() is the usual way to get an icon for
;;; rendering, then just look at the size of the rendered pixbuf. The rendered
;;; pixbuf may not even correspond to the width/height returned by
;;; gtk_icon_size_lookup(), because themes are free to render the pixbuf however
;;; they like, including changing the usual size.
;;;
;;; settings :
;;;     a GtkSettings object, used to determine which set of user preferences to
;;;     used.
;;;
;;; size :
;;;     an icon size
;;;
;;; width :
;;;     location to store icon width
;;;
;;; height :
;;;     location to store icon height
;;;
;;; Returns :
;;;     TRUE if size was a valid size
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_register ()
;;;
;;; GtkIconSize gtk_icon_size_register (const gchar *name,
;;;                                     gint width,
;;;                                     gint height);
;;;
;;; Registers a new icon size, along the same lines as GTK_ICON_SIZE_MENU, etc.
;;; Returns the integer value for the size.
;;;
;;; name :
;;;     name of the icon size
;;;
;;; width :
;;;     the icon width
;;;
;;; height :
;;;     the icon height
;;;
;;; Returns :
;;;     integer value representing the size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_register_alias ()
;;;
;;; void gtk_icon_size_register_alias (const gchar *alias, GtkIconSize target);
;;;
;;; Registers alias as another name for target. So calling
;;; gtk_icon_size_from_name() with alias as argument will return target.
;;;
;;; alias :
;;;     an alias for target
;;;
;;; target :
;;;     an existing icon size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_from_name ()
;;;
;;; GtkIconSize gtk_icon_size_from_name (const gchar *name);
;;;
;;; Looks up the icon size associated with name.
;;;
;;; name :
;;;     the name to look up.
;;;
;;; Returns :
;;;     the icon size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_size_get_name ()
;;;
;;; const gchar * gtk_icon_size_get_name (GtkIconSize size);
;;;
;;; Gets the canonical name of the given icon size. The returned string is
;;; statically allocated and should not be freed.
;;;
;;; size :
;;;     a GtkIconSize
;;;
;;; Returns :
;;;     the name of the given icon size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_get_sizes ()
;;;
;;; void gtk_icon_set_get_sizes (GtkIconSet *icon_set,
;;;                              GtkIconSize **sizes,
;;;                              gint *n_sizes);
;;;
;;; Obtains a list of icon sizes this icon set can render. The returned array
;;; must be freed with g_free().
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; sizes :
;;;     return location for array of sizes
;;;
;;; n_sizes :
;;;     location to store number of elements in returned array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_direction ()
;;;
;;; GtkTextDirection gtk_icon_source_get_direction (const GtkIconSource *source)
;;;
;;; Obtains the text direction this icon source applies to. The return value is
;;; only useful/meaningful if the text direction is not wildcarded.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     text direction this source matches
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_direction_wildcarded ()
;;;
;;; gboolean gtk_icon_source_get_direction_wildcarded
;;;                                               (const GtkIconSource *source);
;;;
;;; Gets the value set by gtk_icon_source_set_direction_wildcarded().
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     TRUE if this icon source is a base for any text direction variant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_filename ()
;;;
;;; const gchar * gtk_icon_source_get_filename (const GtkIconSource *source);
;;;
;;; Retrieves the source filename, or NULL if none is set. The filename is not a
;;; copy, and should not be modified or expected to persist beyond the lifetime
;;; of the icon source.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     image filename. This string must not be modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_pixbuf ()
;;;
;;; GdkPixbuf * gtk_icon_source_get_pixbuf (const GtkIconSource *source);
;;;
;;; Retrieves the source pixbuf, or NULL if none is set. In addition, if a
;;; filename source is in use, this function in some cases will return the
;;; pixbuf from loaded from the filename. This is, for example, true for the
;;; GtkIconSource passed to the GtkStyle::render_icon() virtual function. The
;;; reference count on the pixbuf is not incremented.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     source pixbuf
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_icon_name ()
;;;
;;; const gchar * gtk_icon_source_get_icon_name (const GtkIconSource *source);
;;;
;;; Retrieves the source icon name, or NULL if none is set. The icon_name is not
;;; a copy, and should not be modified or expected to persist beyond the
;;; lifetime of the icon source.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     icon name. This string must not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_size ()
;;;
;;; GtkIconSize gtk_icon_source_get_size (const GtkIconSource *source);
;;;
;;; Obtains the icon size this source applies to. The return value is only
;;; useful/meaningful if the icon size is not wildcarded.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     icon size this source matches
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_size_wildcarded ()
;;;
;;; gboolean gtk_icon_source_get_size_wildcarded (const GtkIconSource *source);
;;;
;;; Gets the value set by gtk_icon_source_set_size_wildcarded().
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     TRUE if this icon source is a base for any icon size variant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_state ()
;;;
;;; GtkStateType gtk_icon_source_get_state (const GtkIconSource *source);
;;;
;;; Obtains the widget state this icon source applies to. The return value is
;;; only useful/meaningful if the widget state is not wildcarded.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     widget state this source matches
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_state_wildcarded ()
;;;
;;; gboolean gtk_icon_source_get_state_wildcarded (const GtkIconSource *source);
;;;
;;; Gets the value set by gtk_icon_source_set_state_wildcarded().
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; Returns :
;;;     TRUE if this icon source is a base for any widget state variant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_source_new" gtk-icon-source-new) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @return{A new @class{gtk-icon-source} object.}
  @begin{short}
    Creates a new @class{gtk-icon-source}. A @class{gtk-icon-source} contains a
    GDK-Pixbuf (or image filename) that serves as the base image for one or more
    of the icons in a @class{gtk-icon-set}, along with a specification for which
    icons in the icon set will be based on that pixbuf or image file. An icon
    set contains a set of icons that represent \"the same\" logical concept in
    different states, different global text directions, and different sizes.
  @end{short}

  So for example a web browser's \"Back to Previous Page\" icon might point in a
  different direction in Hebrew and in English; it might look different when
  insensitive; and it might change size depending on toolbar mode (small/large
  icons). So a single icon set would contain all those variants of the icon.
  @class{gtk-icon-set} contains a list of @class{gtk-icon-source} from which it
  can derive specific icon variants in the set.

  In the simplest case, @class{gtk-icon-set} contains one source pixbuf from
  which it derives all variants. The convenience function
  @fun{gtk-icon-set-new-from-pixbuf} handles this case; if you only have one
  source pixbuf, just use that function.

  If you want to use a different base pixbuf for different icon variants, you
  create multiple icon sources, mark which variants they'll be used to create,
  and add them to the icon set with @fun{gtk-icon-set-add-source}.

  By default, the icon source has all parameters wildcarded. That is, the icon
  source will be used as the base icon for any desired text direction, widget
  state, or icon size.")

(export 'gtk-icon-source-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_direction ()
;;;
;;; void gtk_icon_source_set_direction (GtkIconSource *source,
;;;                                     GtkTextDirection direction);
;;;
;;; Sets the text direction this icon source is intended to be used with.
;;;
;;; Setting the text direction on an icon source makes no difference if the text
;;; direction is wildcarded. Therefore, you should usually call
;;; gtk_icon_source_set_direction_wildcarded() to un-wildcard it in addition to
;;; calling this function.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; direction :
;;;     text direction this source applies to
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_direction_wildcarded ()
;;;
;;; void gtk_icon_source_set_direction_wildcarded (GtkIconSource *source,
;;;                                                gboolean setting);
;;;
;;; If the text direction is wildcarded, this source can be used as the base
;;; image for an icon in any GtkTextDirection. If the text direction is not
;;; wildcarded, then the text direction the icon source applies to should be set
;;; with gtk_icon_source_set_direction(), and the icon source will only be used
;;; with that text direction.
;;;
;;; GtkIconSet prefers non-wildcarded sources (exact matches) over wildcarded
;;; sources, and will use an exact match when possible.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; setting :
;;;     TRUE to wildcard the text direction
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_filename ()
;;;
;;; void gtk_icon_source_set_filename (GtkIconSource *source,
;;;                                    const gchar *filename);
;;;
;;; Sets the name of an image file to use as a base image when creating icon
;;; variants for GtkIconSet. The filename must be absolute.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; filename :
;;;     image file to use
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_pixbuf ()
;;;
;;; void gtk_icon_source_set_pixbuf (GtkIconSource *source, GdkPixbuf *pixbuf);
;;;
;;; Sets a pixbuf to use as a base image when creating icon variants for
;;; GtkIconSet.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; pixbuf :
;;;     pixbuf to use as a source
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_icon_name ()
;;;
;;; void gtk_icon_source_set_icon_name (GtkIconSource *source,
;;;                                     const gchar *icon_name);
;;;
;;; Sets the name of an icon to look up in the current icon theme to use as a
;;; base image when creating icon variants for GtkIconSet.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; icon_name :
;;;     name of icon to use
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_size ()
;;;
;;; void gtk_icon_source_set_size (GtkIconSource *source, GtkIconSize size);
;;;
;;; Sets the icon size this icon source is intended to be used with.
;;;
;;; Setting the icon size on an icon source makes no difference if the size is
;;; wildcarded. Therefore, you should usually call
;;; gtk_icon_source_set_size_wildcarded() to un-wildcard it in addition to
;;; calling this function.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; size :
;;;     icon size this source applies to
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_size_wildcarded ()
;;;
;;; void gtk_icon_source_set_size_wildcarded (GtkIconSource *source,
;;;                                           gboolean setting);
;;;
;;; If the icon size is wildcarded, this source can be used as the base image
;;; for an icon of any size. If the size is not wildcarded, then the size the
;;; source applies to should be set with gtk_icon_source_set_size() and the icon
;;; source will only be used with that specific size.
;;;
;;; GtkIconSet prefers non-wildcarded sources (exact matches) over wildcarded
;;; sources, and will use an exact match when possible.
;;;
;;; GtkIconSet will normally scale wildcarded source images to produce an
;;; appropriate icon at a given size, but will not change the size of source
;;; images that match exactly.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; setting :
;;;     TRUE to wildcard the widget state
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_state ()
;;;
;;; void gtk_icon_source_set_state (GtkIconSource *source, GtkStateType state);
;;;
;;; Sets the widget state this icon source is intended to be used with.
;;;
;;; Setting the widget state on an icon source makes no difference if the state
;;; is wildcarded. Therefore, you should usually call
;;; gtk_icon_source_set_state_wildcarded() to un-wildcard it in addition to
;;; calling this function.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; state :
;;;     widget state this source applies to
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_state_wildcarded ()
;;;
;;; void gtk_icon_source_set_state_wildcarded (GtkIconSource *source,
;;;                                            gboolean setting);
;;;
;;; If the widget state is wildcarded, this source can be used as the base image
;;; for an icon in any GtkStateType. If the widget state is not wildcarded, then
;;; the state the source applies to should be set with
;;; gtk_icon_source_set_state() and the icon source will only be used with that
;;; specific state.
;;;
;;; GtkIconSet prefers non-wildcarded sources (exact matches) over wildcarded
;;; sources, and will use an exact match when possible.
;;;
;;; GtkIconSet will normally transform wildcarded source images to produce an
;;; appropriate icon for a given state, for example lightening an image on
;;; prelight, but will not modify source images that match exactly.
;;;
;;; source :
;;;     a GtkIconSource
;;;
;;; setting :
;;;     TRUE to wildcard the widget state
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.stock-images.lisp --------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.stock-images.lisp
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
;;; Themeable Stock Images
;;;
;;;     Manipulating stock icons
;;;
;;; Types and Values
;;;
;;;     GtkIconSource
;;;     GtkIconFactory
;;;     GtkIconSet
;;;     GtkIconSize
;;;
;;; Functions
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
;;;     gtk_icon_set_render_icon_surface
;;;     gtk_icon_set_unref
;;;
;;;     gtk_icon_size_lookup
;;;     gtk_icon_size_lookup_for_settings
;;;     gtk_icon_size_register
;;;     gtk_icon_size_register_alias
;;;     gtk_icon_size_from_name
;;;     gtk_icon_size_get_name
;;;
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
;;;     GBoxed
;;;     ╰── GtkIconSet
;;;
;;;     GObject
;;;     ╰── GtkIconFactory
;;;
;;; Implemented Interfaces
;;;
;;;     GtkIconFactory implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-size atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-icon-size atdoc:*external-symbols*)
 "@version{*2021-7-21}
  @short{Built-in stock icon sizes.}
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
  @end{pre}
  @begin[code]{table}
    @entry[:invalid]{Invalid size.}
    @entry[:menu]{Size appropriate for menus (16px).}
    @entry[:small-toolbar]{Size appropriate for small toolbars (16px).}
    @entry[:large-toolbar]{Size appropriate for large toolbars (24px).}
    @entry[:button]{Size appropriate for buttons (16px).}
    @entry[:dnd]{Size appropriate for drag and drop (32px).}
    @entry[:dialog]{Size appropriate for dialogs (48px).}
  @end{table}
  @see-class{gtk-icon-theme}")

;;; ----------------------------------------------------------------------------
;;; GtkIconSource
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_icon_source_get_type" g-size))

(define-g-boxed-opaque gtk-icon-source "GtkIconSource"
  :alloc (gtk-icon-source-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-source atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-icon-source 'type)
 "@version{2021-7-21}
  @short{}
  @begin{pre}
(define-g-boxed-opaque gtk-icon-source \"GtkIconSource\"
  :alloc (gtk-icon-source-new))
  @end{pre}
  @see-class{gtk-icon-factory}")

(export 'gtk-icon-source)

;;; ----------------------------------------------------------------------------
;;; GtkIconSet
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_icon_set_get_type" g-size))

(defcfun ("gtk_icon_set_new" %gtk-icon-set-new) :pointer)

(define-g-boxed-opaque gtk-icon-set "GtkIconSet"
  :alloc (%gtk-icon-set-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-set atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-icon-set 'type)
 "@version{2021-7-21}
  @begin{short}
  A @sym{gtk-icon-set} structure manages a set of variants of a particular icon,
  i.e. a @sym{gtk-icon-set} instance contains variants for different sizes and
  widget states.
  @end{short}
  @see-class{gtk-icon-factory}")

(export 'gtk-icon-set)

;;; ----------------------------------------------------------------------------
;;; struct GtkIconFactory
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIconFactory" gtk-icon-factory
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_icon_factory_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-icon-factory 'type)
 "@version{*2021-7-24}
  @begin{short}
    An icon factory manages a collection of @class{gtk-icon-set} instances.
  @end{short}
  A @class{gtk-icon-set} instance manages a set of variants of a particular
  icon, i.e. a @class{gtk-icon-set} instance contains variants for different
  sizes and widget states. Icons in an icon factory are named by a stock ID,
  which is a simple string identifying the icon. Each @code{GtkStyle} object
  has a list of @sym{gtk-icon-factory} objects derived from the current theme.
  Those icon factories are consulted first when searching for an icon. If the
  theme does not set a particular icon, GTK looks for the icon in a list of
  default icon factories, maintained by the functions
  @fun{gtk-icon-factory-add-default} and @fun{gtk-icon-factory-remove-default}.
  Applications with icons should add a default icon factory with their icons,
  which will allow themes to override the icons for the application.

  To display an icon, use the function @fun{gtk-widget-render-icon} on the
  widget that will display the icon. This function takes the theme into account
  when looking up the icon to use for a given stock ID.
  @begin[GtkIconFactory as GtkBuildable]{dictionary}
    The @sym{gtk-icon-factory} object supports a custom @code{<sources>}
    element, which can contain multiple @code{<source>} elements. The following
    attributes are allowed:
    @begin[code]{table}
      @entry[stock-id]{A string with the stock ID of the source. This attribute
        is mandatory.}
      @entry[filename]{A string with the filename of the source. This attribute
        is optional.}
      @entry[icon-name]{A string with the icon name for the source. This
        attribute is optional.}
      @entry[size]{A value of the @symbol{gtk-icon-size} enumeration with the
        size of the icon. This attribute is optional.}
      @entry[direction]{A value of the @symbol{gtk-text-direction} enumeration
        with the direction of the source. This attribute is optional.}
      @entry[state]{A value of the @symbol{gtk-state-type} enumeration with the
        state of the source. This attribute is optional.}
    @end{table}
    @b{Example:} A @sym{gtk-icon-factory} UI definition fragment.
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
    @end{pre}
  @end{dictionary}
  @see-class{gtk-icon-set}
  @see-symbol{gtk-icon-size}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_copy ()
;;;
;;; GtkIconSource * gtk_icon_source_copy (const GtkIconSource *source);
;;;
;;; gtk_icon_source_copy has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_free has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
 "@version{*2021-7-24}
  @argument[factory]{a @class{gtk-icon-factory} object}
  @argument[stock-id]{a string with the icon name}
  @argument[iconset]{a @class{gtk-icon-set} instance}
  @begin{short}
    Adds the given @arg{iconset} to the icon factory, under the name
    @arg{stock-id}.
  @end{short}
  The argument @arg{stock-id} should be namespaced for your application, e.g.
  \"myapp-whatever-icon\". Normally applications create a
  @class{gtk-icon-factory} object, then add it to the list of default factories
  with the function @fun{gtk-icon-factory-add-default}. Then they pass the
  argument @arg{stock-id} to widgets such as a @class{gtk-image} widget to
  display the icon. Themes can provide an icon with the same name, such as
  \"myapp-whatever-icon\", to override your default icons of the application.
  If an icon already existed in @arg{factory} for @arg{stock-id}, it is
  unreferenced and replaced with the new @arg{iconset} instance.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-factory-add} has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-set}
  @see-class{gtk-icon-theme}
  @see-class{gtk-image}
  @see-function{gtk-icon-factory-add-default}"
  (factory (g-object gtk-icon-factory))
  (stock-id :string)
  (iconset (g-boxed-foreign gtk-icon-set)))

(export 'gtk-icon-factory-add)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_add_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_add_default" gtk-icon-factory-add-default) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @argument[factory]{a @class{gtk-icon-factory} object}
  @begin{short}
    Adds an icon factory to the list of icon factories searched by the
    function @code{gtk_style_lookup_icon_set()}.
  @end{short}
  This means that, for example, the function @fun{gtk-image-new-from-stock} will
  be able to find icons in @arg{factory}. There will normally be an icon factory
  added for each library or application that comes with icons. The default icon
  factories can be overridden by themes.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-factory-add-default} has been deprecated since
    version 3.10 and should not be used in newly written code.
    Use the @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-theme}
  @see-function{gtk-image-new-from-stock}"
  (factory (g-object gtk-icon-factory)))

(export 'gtk-icon-factory-add-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_lookup" gtk-icon-factory-lookup)
    (g-boxed-foreign gtk-icon-set :return)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[factory]{a @class{gtk-icon-factory} object}
  @argument[stock-id]{a string with an icon name}
  @return{The @class{gtk-icon-set} instance of @arg{stock-id}.}
  @begin{short}
    Looks up @arg{stock-id} in the icon factory, returning an icon set if
    found, otherwise @code{nil}.
  @end{short}
  For display to the user, you should use the function
  @code{gtk_style_lookup_icon_set()} on the @code{GtkStyle} object for the
  widget that will display the icon, instead of using this function directly,
  so that themes are taken into account.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-factory-lookup} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-theme}"
  (factory (g-object gtk-icon-factory))
  (stock-id :string))

(export 'gtk-icon-factory-lookup)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_lookup_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_factory_lookup_default" gtk-icon-factory-lookup-default)
    (g-boxed-foreign gtk-icon-set :return)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[stock-id]{a string with an icon name}
  @return{A @class{gtk-icon-set} instance, or @code{nil}.}
  @begin{short}
    Looks for an icon in the list of default icon factories.
  @end{short}
  For display to the user, you should use the function
  @code{gtk_style_lookup_icon_set()} on the @code{GtkStyle} object for the
  widget that will display the icon, instead of using this function directly,
  so that themes are taken into account.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-factory-lookup-default} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-set}
  @see-class{gtk-icon-theme}"
  (stock-id :string))

(export 'gtk-icon-factory-lookup-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_factory_new ()
;;;
;;; GtkIconFactory * gtk_icon_factory_new (void);
;;;
;;; gtk_icon_factory_new has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
;;;
;;; Creates a new GtkIconFactory. An icon factory manages a collection of
;;; GtkIconSets; a GtkIconSet manages a set of variants of a particular icon
;;; (i.e. a GtkIconSet contains variants for different sizes and widget states).
;;; Icons in an icon factory are named by a stock ID, which is a simple string
;;; identifying the icon. Each GtkStyle has a list of GtkIconFactorys derived
;;; from the current theme; those icon factories are consulted first when
;;; searching for an icon. If the theme does not set a particular icon, GTK
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
 "@version{2021-7-21}
  @argument[factory]{a @class{gtk-icon-factory} object previously added with
    the function @fun{gtk-icon-factory-add-default}}
  @begin{short}
    Removes an icon factory from the list of default icon factories.
  @end{short}
  Not normally used. You might use it for a library that can be unloaded or
  shut down.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-factory-remove-default} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-theme}
  @see-function{gtk-icon-factory-add-default}"
  (factory (g-object gtk-icon-factory)))

(export 'gtk-icon-factory-remove-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_add_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_add_source" gtk-icon-set-add-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[iconset]{a @class{gtk-icon-set} instance}
  @argument[source]{a @class{gtk-icon-source} instance}
  @begin{short}
    This function copies @arg{source}, so you can reuse the same source
    immediately without affecting the icon set.
  @end{short}

  Icon sets have a list of @class{gtk-icon-source} instances, which they use as
  base icons for rendering icons in different states and sizes. Icons are
  scaled, made to look insensitive, etc. in the function
  @fun{gtk-icon-set-render-icon}, but a @class{gtk-icon-set} instance needs
  base images to work with. The base images and when to use them are described
  by a @class{gtk-icon-source} instance.

  An example of when you would use this function: a web browser's \"Back to
  Previous Page\" icon might point in a different direction in Hebrew and in
  English. It might look different when insensitive, and it might change size
  depending on toolbar mode (small/large icons). So a single icon set would
  contain all those variants of the icon, and you might add a separate source
  for each one.

  You should nearly always add a \"default\" icon source with all fields
  wildcarded, which will be used as a fallback if no more specific source
  matches. A @class{gtk-icon-set} instance always prefers more specific icon
  sources to more generic icon sources. The order in which you add the sources
  to the icon set does not matter.

  The function @fun{gtk-icon-set-new-from-pixbuf} creates a new icon set with a
  default icon source based on the given pixbuf.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-set-add-source} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-set}
  @see-class{gtk-icon-source}
  @see-class{gtk-icon-theme}
  @see-function{gtk-icon-set-render-icon}
  @see-function{gtk-icon-set-new-from-pixbuf}"
  (iconset (g-boxed-foreign gtk-icon-set))
  (source (g-boxed-foreign gtk-icon-source)))

(export 'gtk-icon-set-add-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_copy ()
;;;
;;; GtkIconSet * gtk_icon_set_copy (GtkIconSet *icon_set);
;;;
;;; gtk_icon_set_copy has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_new" gtk-icon-set-new) (g-boxed-foreign gtk-icon-set)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @return{A new @class{gtk-icon-set} instance.}
  @begin{short}
    Creates a new icon set.
  @end{short}
  A icon set represents a single icon in various sizes and widget states. It
  can provide a @class{gdk-pixbuf} object for a given size and state on
  request, and automatically caches some of the rendered @class{gdk-pixbuf}
  objects.

  Normally you would use the function @fun{gtk-widget-render-icon-pixbuf}
  instead of using a @class{gtk-icon-set} instance directly. The one case where
  you would use a @class{gtk-icon-set} instance is to create application
  specific icon sets to place in a @class{gtk-icon-factory} object.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-set-new} has been deprecated since version 3.10
    and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-factory}
  @see-class{gtk-icon-set}
  @see-class{gdk-pixbuf}
  @see-class{gtk-icon-theme}
  @see-function{gtk-widget-render-icon-pixbuf}")

(export 'gtk-icon-set-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_new_from_pixbuf" gtk-icon-set-new-from-pixbuf)
    (g-boxed-foreign gtk-icon-set)
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @return{A new @class{gtk-icon-set} instance.}
  @begin{short}
    Creates a new icon set with @arg{pixbuf} as the default/fallback source
    image.
  @end{short}
  If you do not add any additional @class{gtk-icon-source} instance to the icon
  set, all variants of the icon will be created from @arg{pixbuf}, using
  scaling, pixelation, etc. as required to adjust the icon size or make the
  icon look insensitive/prelighted.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-set-new-from-pixbuf} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-set}
  @see-class{gdk-pixbuf}
  @see-class{gtk-icon-source}
  @see-class{gtk-icon-theme}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-icon-set-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_ref ()
;;;
;;; GtkIconSet * gtk_icon_set_ref (GtkIconSet *icon_set);
;;;
;;; gtk_icon_set_ref has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_render_icon" gtk-icon-set-render-icon)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[iconset]{a @class{gtk-icon-set} instance}
  @argument[style]{a @code{GtkStyle} object associated with @arg{widget},
    or @code{nil}}
  @argument[direction]{a value of the @symbol{gtk-text-direction} enumeration}
  @argument[state]{a value of the @symbol{gtk-state-type} enumeration with the
    state of the widget}
  @argument[size]{a @symbol{gtk-icon-size} value, a size of -1 means render at
    the size of the source and do not scale}
  @argument[widget]{a @class{gtk-widget} object that will display the icon, or
    @code{nil}, the only use that is typically made of this is to determine the
    appropriate @class{gdk-screen} object}
  @argument[detail]{a string with the detail to pass to the theme engine, or
    @code{nil}, note that passing a detail of anything but @code{nil} will
    disable caching}
  @return{A @class{gdk-pixbuf} object to be displayed.}
  @begin{short}
    Renders an icon using the function @code{gtk_style_render_icon()}.
  @end{short}
  In most cases, the function @fun{gtk-widget-render-icon} is better, since it
  automatically provides most of the arguments from the current widget settings.
  This function never returns @code{nil}. If the icon cannot be rendered,
  perhaps because an image file fails to load, a default \"missing image\" icon
  will be returned instead.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-set-render-icon} has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-set}
  @see-class{gdk-screen}
  @see-class{gdk-pixbuf}
  @see-class{gtk-icon-theme}
  @see-class{gtk-widget}
  @see-symbol{gtk-direction}
  @see-symbol{gtk-state-type}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-widget-render-icon}"
  (iconset (g-boxed-foreign gtk-icon-set))
  (style (g-object gtk-style))
  (direction :pointer) ; type gtk-text-direction is not defined at this point
  (state gtk-state-type)
  (size gtk-icon-size)
  (widget (g-object gtk-widget))
  (detail :string))

(export 'gtk-icon-set-render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_render_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_set_render_icon_pixbuf" gtk-icon-set-render-icon-pixbuf)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[iconset]{a @class{gtk-icon-set} instance}
  @argument[context]{a @class{gtk-style-context} object}
  @argument[size]{a @symbol{gtk-icon-size} value, a size of -1 means render
    at the size of the source and do not scale}
  @return{A @class{gdk-pixbuf} object to be displayed.}
  @begin{short}
    Renders an icon using the function @fun{gtk-render-icon-pixbuf}.
  @end{short}
  In most cases, the function @fun{gtk-widget-render-icon-pixbuf} is better,
  since it automatically provides most of the arguments from the current widget
  settings. This function never returns @code{nil}. If the icon cannot be
  rendered, perhaps because an image file fails to load, a default
  \"missing image\" icon will be returned instead.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-set-render-icon-pixbuf} has been deprecated
    since version 3.10 and should not be used in newly written code. Use
    the @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-set}
  @see-class{gtk-style-context}
  @see-class{gdk-pixbuf}
  @see-class{gtk-icon-theme}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-render-icon-pixbuf}
  @see-function{gtk-widget-render-icon-pixbuf}"
  (iconset (g-boxed-foreign gtk-icon-set))
  (context (g-object gtk-style-context))
  (size gtk-icon-size))

(export 'gtk-icon-set-render-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_render_icon_surface ()
;;;
;;; cairo_surface_t *
;;; gtk_icon_set_render_icon_surface (GtkIconSet *icon_set,
;;;                                   GtkStyleContext *context,
;;;                                   GtkIconSize size,
;;;                                   int scale,
;;;                                   GdkWindow *for_window);
;;;
;;; gtk_icon_set_render_icon_surface has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
;;;
;;; Renders an icon using gtk_render_icon_pixbuf() and converts it to a cairo
;;; surface.
;;;
;;; This function never returns NULL; if the icon can’t be rendered (perhaps
;;; because an image file fails to load), a default "missing image" icon will
;;; be returned instead.
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; size :
;;;     icon size (GtkIconSize). A size of (GtkIconSize)-1 means render at the
;;;     size of the source and don’t scale.
;;;
;;; scale :
;;;     the window scale to render for
;;;
;;; for_window :
;;;     GdkWindow to optimize drawing for, or NULL.
;;;
;;; Returns :
;;;     a cairo_surface_t to be displayed.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_set_unref ()
;;;
;;; void gtk_icon_set_unref (GtkIconSet *icon_set);
;;;
;;; gtk_icon_set_unref has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_size_lookup has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_size_lookup_for_settings has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_size_register has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_size_register_alias has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_icon_size_from_name has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_size_get_name has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_set_get_sizes has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_get_direction has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_get_direction_wildcarded has been deprecated since version
;;; 3.10 and should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_filename () -> gtk-icon-source-filename
;;; ----------------------------------------------------------------------------

(defun (setf gtk-icon-source-filename) (filename source)
  (foreign-funcall "gtk_icon_source_set_filename"
                   (g-boxed-foreign gtk-icon-source) source
                   :string filename
                   :void)
  filename)

(defcfun ("gtk_icon_source_get_filename" gtk-icon-source-filename)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @syntax[]{(gtk-icon-source-filename source) => filename}
  @syntax[]{(setf (gtk-icon-source-filename source) filename)}
  @argument[source]{a @class{gtk-icon-source} instance}
  @argument[filename]{a string with the image file to use}
  @begin{short}
    Accessor of the filename of a @class{gtk-icon-source} instance.
  @end{short}

  The function @sym{gtk-icon-source-filename} retrieves the filename of the
  icon source, or @code{nil} if none is set. The function
  @sym{(setf gtk-icon-source-filename)} sets the filename. The filename must be
  absolute.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-source-filename} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-source}
  @see-class{gtk-icon-theme}"
  (source (g-boxed-foreign gtk-icon-source)))

(export 'gtk-icon-source-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_pixbuf ()
;;;
;;; GdkPixbuf * gtk_icon_source_get_pixbuf (const GtkIconSource *source);
;;;
;;; gtk_icon_source_get_pixbuf has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_icon_name () -> gtk-icon-source-icon-name
;;; ----------------------------------------------------------------------------

(defun (setf gtk-icon-source-icon-name) (name source)
  (foreign-funcall "gtk_icon_source_set_icon_name"
                   (g-boxed-foreign gtk-icon-source) source
                   :string name
                   :void)
  name)

(defcfun ("gtk_icon_source_get_icon_name" gtk-icon-source-icon-name)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @syntax[]{(gtk-icon-source-icon-name source) => name}
  @syntax[]{(setf (gtk-icon-source-icon-name source) name)}
  @argument[source]{a @class{gtk-icon-source} instance}
  @argument[name]{a string with the name of the icon to use}
  @begin{short}
    Accessor of the icon name of a @class{gtk-icon-source} instance.
  @end{short}

  The function @sym{gtk-icon-source-icon-name} retrieves the icon name of the
  icon source, or @code{nil} if none is set. The function
  @sym{(setf gtk-icon-source-icon-name)} sets the name of an icon to look up in
  the current icon theme to use as a base image when creating icon variants for
  a @class{gtk-icon-set} instance.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-source-icon-name} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-source}
  @see-class{gtk-icon-set}
  @see-class{gtk-icon-theme}"
  (source (g-boxed-foreign gtk-icon-source)))

(export 'gtk-icon-source-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_get_size ()
;;;
;;; GtkIconSize gtk_icon_source_get_size (const GtkIconSource *source);
;;;
;;; gtk_icon_source_get_size has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_get_size_wildcarded has been deprecated since version 3.10
;;; and should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_get_state has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_get_state_wildcarded has been deprecated since version 3.10
;;; and should not be used in newly written code. Use GtkIconTheme instead.
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

(defcfun ("gtk_icon_source_new" gtk-icon-source-new)
    (g-boxed-foreign gtk-icon-source)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @return{A new @class{gtk-icon-source} instance.}
  @begin{short}
    Creates a new icon source.
  @end{short}
  A icon source contains a @class{gdk-pixbuf} object or image filename that
  serves as the base image for one or more of the icons in a
  @class{gtk-icon-set} instance, along with a specification for which icons in
  the icon set will be based on that pixbuf or image file. An icon set contains
  a set of icons that represent \"the same\" logical concept in different
  states, different global text directions, and different sizes.

  So for example a web browser's \"Back to Previous Page\" icon might point in
  a different direction in Hebrew and in English. It might look different when
  insensitive, and it might change size depending on toolbar mode (small/large
  icons). So a single icon set would contain all those variants of the icon.
  A @class{gtk-icon-set} instance contains a list of @class{gtk-icon-source}
  objects from which it can derive specific icon variants in the set.

  In the simplest case, a @class{gtk-icon-set} instance contains one source
  pixbuf from which it derives all variants. The convenience function
  @fun{gtk-icon-set-new-from-pixbuf} handles this case. If you only have one
  source pixbuf, just use that function.

  If you want to use a different base pixbuf for different icon variants, you
  create multiple icon sources, mark which variants they will be used to create,
  and add them to the icon set with the function @fun{gtk-icon-set-add-source}.

  By default, the icon source has all parameters wildcarded. That is, the icon
  source will be used as the base icon for any desired text direction, widget
  state, or icon size.
  @begin[Warning]{dictionary}
    The function @sym{gtk-icon-source-new} has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @class{gtk-icon-theme} class instead.
  @end{dictionary}
  @see-class{gtk-icon-source}
  @see-class{gtk-icon-set}
  @see-class{gtk-icon-theme}
  @see-class{gdk-pixbuf}
  @see-function{gtk-icon-set-new-from-pixbuf}
  @see-function{gtk-icon-set-add-source}")

(export 'gtk-icon-source-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_source_set_direction ()
;;;
;;; void gtk_icon_source_set_direction (GtkIconSource *source,
;;;                                     GtkTextDirection direction);
;;;
;;; gtk_icon_source_set_direction has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_direction_wildcarded has been deprecated since version
;;; 3.10 and should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_pixbuf ()
;;;
;;; void gtk_icon_source_set_pixbuf (GtkIconSource *source, GdkPixbuf *pixbuf);
;;;
;;; gtk_icon_source_set_pixbuf has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_size ()
;;;
;;; void gtk_icon_source_set_size (GtkIconSource *source, GtkIconSize size);
;;;
;;; gtk_icon_source_set_size has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_size_wildcarded has been deprecated since version 3.10
;;; and should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_state has been deprecated since version 3.10 and
;;; should not be used in newly written code. Use GtkIconTheme instead.
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
;;; gtk_icon_source_set_state_wildcarded has been deprecated since version 3.10
;;; and should not be used in newly written code. Use GtkIconTheme instead.
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

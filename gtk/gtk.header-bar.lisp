;;; ----------------------------------------------------------------------------
;;; gtk.header-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2016 Dieter Kaiser
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
;;; GtkHeaderBar
;;;
;;; A box with a centered child
;;;
;;; Types and Values
;;;
;;;     GtkHeaderBar
;;;
;;; Functions
;;;
;;;     gtk_header_bar_new
;;;     gtk_header_bar_set_title
;;;     gtk_header_bar_get_title
;;;     gtk_header_bar_set_subtitle
;;;     gtk_header_bar_get_subtitle
;;;     gtk_header_bar_set_has_subtitle
;;;     gtk_header_bar_get_has_subtitle
;;;     gtk_header_bar_set_custom_title
;;;     gtk_header_bar_get_custom_title
;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end
;;;     gtk_header_bar_set_show_close_button
;;;     gtk_header_bar_get_show_close_button
;;;     gtk_header_bar_set_decoration_layout
;;;     gtk_header_bar_get_decoration_layout
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkHeaderBar
;;;
;;; Implemented Interfaces
;;;
;;; GtkHeaderBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkHeaderBar
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkHeaderBar" gtk-header-bar
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_header_bar_get_type")
  ((custom-title
    gtk-header-bar-custom-title
    "custom-title" "GtkWidget" t t)
   #+gtk-3-12
   (decoration-layout
    gtk-header-bar-decoration-layout
    "decoration-layout" "gchar" t t)
   #+gtk-3-12
   (decoration-layout-set
    gtk-header-bar-decoration-layout-set
    "decoration-layout-set" "gboolean" t t)
   #+gtk-3-12
   (has-subtitle
    gtk-header-bar-has-subtitle
    "has-subtitle" "gboolean" t t)
   (show-close-button
    gtk-header-bar-show-close-button
    "show-close-button" "gboolean" t t)
   (spacing
    gtk-header-bar-spacing
    "spacing" "gint" t t)
   (subtitle
    gtk-header-bar-subtitle
    "subtitle" "gchar" t t)
   (title
    gtk-header-bar-title
    "title" "gchar" t t)))

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation 'gtk-header-bar 'type)
 "@version{2016-1-17}
  @begin{short}
    @sym{gtk-header-bar} is similar to a horizontal @class{gtk-box}. It allows
    children to be placed at the start or the end. In addition, it allows a
    title and subtitle to be displayed.
  @end{short}

  The title will be centered with respect to the width of the box, even if the
  children at either side take up different amounts of space. The height of the
  titlebar will be set to provide sufficient space for the subtitle, even if
  none is currently set. If a subtitle is not needed, the space reservation can
  be turned off with the function @fun{gtk-header-bar-has-subtitle}.

  @sym{gtk-header-bar} can add typical window frame controls, such as minimize,
  maximize and close buttons, or the window icon.

  Since 3.10

  @begin[Child Property Details]{dictionary}
    @subheading{The \"pack-type\" child property}
      @code{\"pack-type\"} child property of type @symbol{gtk-pack-type}
      (Read / Write) @br{}
      A @symbol{gtk-pack-type} indicating whether the child is packed with
      reference to the start or end of the parent. @br{}
      Default value: @code{:start}

    @subheading{The \"position\" child property}
      @code{\"position\"} child property of type @code{:int}
      (Read / Write) @br{}
      The index of the child in the parent. @br{}
      Allowed values: >= -1 @br{}
      Default value: 0
  @end{dictionary}
  @see-slot{gtk-header-bar-custom-title}
  @see-slot{gtk-header-bar-decoration-layout}
  @see-slot{gtk-header-bar-decoration-layout-set}
  @see-slot{gtk-header-bar-has-subtitle}
  @see-slot{gtk-header-bar-show-close-button}
  @see-slot{gtk-header-bar-spacing}
  @see-slot{gtk-header-bar-subtitle}
  @see-slot{gtk-header-bar-title}
  @see-class{gtk-box}")

#+gtk-3-10
(define-child-property "GtkHeaderBar"
                       gtk-header-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+gtk-3-10
(define-child-property "GtkHeaderBar"
                       gtk-header-bar-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-header-bar-custom-title --------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "custom-title"
                                               'gtk-header-bar) 't)
 "The @code{custom-title} property of type @class{gtk-widget}
  (Read / Write / Construct) @br{}
  Custom title widget to display. @br{}")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-custom-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-custom-title 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{custom-title} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-decoration-layout ---------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "decoration-layout"
                                               'gtk-header-bar) 't)
 "The @code{decoration-layout} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The decoration layout for buttons. If this property is not set, the
  \"gtk-decoration-layout\" setting is used.
  See the function @fun{gtk-header-bar-decoration-layout} for information about
  the format of this string. @br{}
  Since 3.12 @br{}
  Default value: @code{nil}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-decoration-layout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-decoration-layout 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{decoration-layout} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-decoration-layout-set -----------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "decoration-layout-set"
                                               'gtk-header-bar) 't)
 "The @code{decoration-layout-set} property of type @class{:boolean}
  (Read / Write) @br{}
  Set to @emph{true} if @code{decoration-layout} is set. @br{}
  Since 3.12 @br{}
  Default value: @code{nil}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-decoration-layout-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-decoration-layout-set 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{decoration-layout-set} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-has-subtitle --------------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "has-subtitle"
                                               'gtk-header-bar) 't)
 "The @code{has-subtitle} property of type @class{:boolean}
  (Read / Write) @br{}
  If @emph{true}, reserve space for a subtitle, even if none is currently set.
  @br{}
  Since 3.12 @br{}
  Default value: @emph{true}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-decoration-layout-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-decoration-layout-set 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{has-subtitle} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-show-close-button ---------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "show-close-button"
                                               'gtk-header-bar) 't)
 "The @code{show-close-button} property of type @class{:boolean}
  (Read / Write) @br{}
  Whether to show window decorations.
  Which buttons are actually shown and where is determined by the
  @code{decoration-layout} property, and by the state of the window (e. g. a
  close button will not be shown if the window can not be closed). @br{}
  Default value: @code{nil}")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-show-close-button
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-show-close-button 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{show-close-button} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-spacing -------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "spacing"
                                               'gtk-header-bar) 't)
 "The @code{spacing} property of type @class{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-spacing
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-spacing 'function)
 "@version{2016-1-17}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{spacing} of the
    @class{gtk-header-bar} class.
  @end{short}
    
")

;;; --- gtk-header-bar-subtitle ------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "subtitle"
                                               'gtk-header-bar) 't)
 "The @code{subtitle} property of type @class{:string} (Read / Write) @br{}
  The subtitle to display. @br{}
  Default value: @code{nil}")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-subtitle
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-subtitle 'function)
 "@version{2016-1-17}
  @argument[object]{A @class{gtk-header-bar} widget.}
  @argument[subtitle]{A subtitle, or @code{nil}.}
  @syntax[]{(gtk-header-bar-subtitle object) => subtitle}
  @syntax[]{(setf (gtk-header-bar-subtitle object) subtitle)}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{subtitle} of the
    @class{gtk-header-bar} class.
  @end{short}

  The generic function @sym{gtk-header-bar-subtitle} retrieves the subtitle of
  the header.

  The generic function @sym{(setf gtk-header-bar-subtitle)} sets the subtitle
  of the header bar. The title should give a user an additional detail to help
  him identify the current view.

  Note that @class{gtk-header-bar} by default reserves room for the subtitle,
  even if none is currently set. If this is not desired, set the
  @slot[gtk-header-bar]{has-subtitle} property to @code{nil}.

  Since: 3.10
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-title ---------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "title"
                                               'gtk-header-bar) 't)
 "The @code{title} property of type @class{:string} (Read / Write) @br{}
  The title to display. @br{}
  Default value: @code{nil}")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-title
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-title 'function)
 "@version{2016-1-17}
  @argument[object]{A @class{gtk-header-bar} widget.}
  @argument[title]{A title, or @code{nil}.}
  @syntax[]{(gtk-header-bar-title object) => title}
  @syntax[]{(setf (gtk-header-bar-title object) title)}
  @begin{short}
    Accessor of the slot @slot[gtk-header-bar]{title} of the
    @class{gtk-header-bar} class.
  @end{short}

  The generic function @sym{gtk-header-bar-title} retrieves the title of the
  header.

  The generic function @sym{(setf gtk-header-bar-title} returns the title of the
  header, or @code{nil} if none has been set explicitly. The title should help
  a user identify the current view. A good title should not include the
  application name.

  Since: 3.10
  @see-class{gtk-header-bar}")

#+gtk-3-10
(defcfun ("gtk_header_bar_pack_start" gtk-header-bar-pack-start) :void
  (bar (g-object gtk-header-bar))
  (child (g-object gtk-widget)))

#+gtk-3-10
(export 'gtk-header-bar-pack-start)

#+gtk-3-10
(defcfun ("gtk_header_bar_pack_end" gtk-header-bar-pack-end) :void
  (bar (g-object gtk-header-bar))
  (child (g-object gtk-widget)))

#+gtk-3-10
(export 'gtk-header-bar-pack-end)

#|

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_new ()
;;;
;;; GtkWidget *
;;; gtk_header_bar_new (void);
;;;
;;; Creates a new GtkHeaderBar widget.
;;;
;;; Returns
;;;     a new GtkHeaderBar
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------



gtk_header_bar_set_has_subtitle ()

void
gtk_header_bar_set_has_subtitle (GtkHeaderBar *bar,
                                 gboolean setting);

Sets whether the header bar should reserve space for a subtitle, even if none is currently set.
Parameters

bar
	

a GtkHeaderBar
	 

setting
	

TRUE to reserve space for a subtitle
	 

Since: 3.12

gtk_header_bar_get_has_subtitle ()

gboolean
gtk_header_bar_get_has_subtitle (GtkHeaderBar *bar);

Retrieves whether the header bar reserves space for a subtitle, regardless if one is currently set or not.
Parameters

bar
	

a GtkHeaderBar
	 
Returns

TRUE if the header bar reserves space for a subtitle

Since: 3.12

gtk_header_bar_set_custom_title ()

void
gtk_header_bar_set_custom_title (GtkHeaderBar *bar,
                                 GtkWidget *title_widget);

Sets a custom title for the GtkHeaderBar.

The title should help a user identify the current view. This supersedes any title set by gtk_header_bar_set_title() or gtk_header_bar_set_subtitle(). To achieve the same style as the builtin title and subtitle, use the “title” and “subtitle” style classes.

You should set the custom title to NULL, for the header title label to be visible again.
Parameters

bar
	

a GtkHeaderBar
	 

title_widget
	

a custom widget to use for a title.
	[allow-none]

Since: 3.10

gtk_header_bar_get_custom_title ()

GtkWidget *
gtk_header_bar_get_custom_title (GtkHeaderBar *bar);

Retrieves the custom title widget of the header. See gtk_header_bar_set_custom_title().
Parameters

bar
	

a GtkHeaderBar
	 
Returns

the custom title widget of the header, or NULL if none has been set explicitly.

[transfer none]

Since: 3.10

gtk_header_bar_pack_start ()

void
gtk_header_bar_pack_start (GtkHeaderBar *bar,
                           GtkWidget *child);

Adds child to bar , packed with reference to the start of the bar .
Parameters

bar
	

A GtkHeaderBar
	 

child
	

the GtkWidget to be added to bar
	 

Since: 3.10

gtk_header_bar_pack_end ()

void
gtk_header_bar_pack_end (GtkHeaderBar *bar,
                         GtkWidget *child);

Adds child to bar , packed with reference to the end of the bar .
Parameters

bar
	

A GtkHeaderBar
	 

child
	

the GtkWidget to be added to bar
	 

Since: 3.10

gtk_header_bar_set_show_close_button ()

void
gtk_header_bar_set_show_close_button (GtkHeaderBar *bar,
                                      gboolean setting);

Sets whether this header bar shows the standard window decorations, including close, maximize, and minimize.
Parameters

bar
	

a GtkHeaderBar
	 

setting
	

TRUE to show standard widow decorations
	 

Since: 3.10

gtk_header_bar_get_show_close_button ()

gboolean
gtk_header_bar_get_show_close_button (GtkHeaderBar *bar);

Returns whether this header bar shows the standard window decorations.
Parameters

bar
	

a GtkHeaderBar
	 
Returns

TRUE if the decorations are shown

Since: 3.10

gtk_header_bar_set_decoration_layout ()

void
gtk_header_bar_set_decoration_layout (GtkHeaderBar *bar,
                                      const gchar *layout);

Sets the decoration layout for this header bar, overriding the “gtk-decoration-layout” setting.

There can be valid reasons for overriding the setting, such as a header bar design that does not allow for buttons to take room on the right, or only offers room for a single close button. Split header bars are another example for overriding the setting.

The format of the string is button names, separated by commas. A colon separates the buttons that should appear on the left from those on the right. Recognized button names are minimize, maximize, close, icon (the window icon) and menu (a menu button for the fallback app menu).

For example, “menu:minimize,maximize,close” specifies a menu on the left, and minimize, maximize and close buttons on the right.
Parameters

bar
	

a GtkHeaderBar
	 

layout
	

a decoration layout, or NULL to unset the layout.
	[allow-none]

Since: 3.12

gtk_header_bar_get_decoration_layout ()

const gchar *
gtk_header_bar_get_decoration_layout (GtkHeaderBar *bar);

Gets the decoration layout set with gtk_header_bar_set_decoration_layout().
Parameters

bar
	

a GtkHeaderBar
	 
Returns

the decoration layout

Since: 3.12



|#

;;; --- End of file gtk.header-bar.lisp ----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.header-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2016 - 2020 Dieter Kaiser
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
;;;     A box with a centered child
;;;
;;; Types and Values
;;;
;;;     GtkHeaderBar
;;;
;;; Functions
;;;
;;;     gtk_header_bar_new
;;;     gtk_header_bar_set_title                           Accessor
;;;     gtk_header_bar_get_title                           Accessor
;;;     gtk_header_bar_set_subtitle                        Accessor
;;;     gtk_header_bar_get_subtitle                        Accessor
;;;     gtk_header_bar_set_has_subtitle                    Accessor
;;;     gtk_header_bar_get_has_subtitle                    Accessor
;;;     gtk_header_bar_set_custom_title                    Accessor
;;;     gtk_header_bar_get_custom_title                    Accessor
;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end
;;;     gtk_header_bar_set_show_close_button               Accessor
;;;     gtk_header_bar_get_show_close_button               Accessor
;;;     gtk_header_bar_set_decoration_layout               Accessor
;;;     gtk_header_bar_get_decoration_layout               Accessor
;;;
;;; Properties
;;;
;;;       GtkWidget*   custom-title             Read / Write
;;;           gchar*   decoration-layout        Read / Write
;;;        gboolean    decoration-layout-set    Read / Write
;;;        gboolean    has-subtitle             Read / Write
;;;        gboolean    show-close-button        Read / Write
;;;            gint    spacing                  Read / Write
;;;           gchar*   subtitle                 Read / Write
;;;           gchar*   title                    Read / Write
;;;
;;; Child Properties
;;;
;;;     GtkPackType    pack-type                Read / Write
;;;            gint    position                 Read / Write
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
    "decoration-layout" "gchararray" t t)
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
    "subtitle" "gchararray" t t)
   (title
    gtk-header-bar-title
    "title" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-header-bar 'type)
 "@version{2020-5-9}
  @begin{short}
    @sym{gtk-header-bar} is similar to a horizontal @class{gtk-box}. It allows
    children to be placed at the start or the end. In addition, it allows a
    title and subtitle to be displayed.
  @end{short}

  @image[headerbar]{}

  The title will be centered with respect to the width of the box, even if the
  children at either side take up different amounts of space. The height of the
  titlebar will be set to provide sufficient space for the subtitle, even if
  none is currently set. If a subtitle is not needed, the space reservation can
  be turned off with the function @fun{gtk-header-bar-has-subtitle}.

  @sym{gtk-header-bar} can add typical window frame controls, such as minimize,
  maximize and close buttons, or the window icon.

  Since 3.10
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk-pack-type}
        (Read / Write) @br{}
        A value of the @symbol{gtk-pack-type} enumeration indicating whether the
        child is packed with reference to the start or end of the parent. @br{}
        Default value: @code{:start}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child widget in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
    @end{table}
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

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-header-bar-custom-title --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "custom-title"
                                               'gtk-header-bar) 't)
 "The @code{custom-title} property of type @class{gtk-widget}
  (Read / Write / Construct) @br{}
  Custom title widget to display. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-custom-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-custom-title 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-custom-title object) => title-widget}
  @syntax[]{(setf (gtk-header-bar-custom-title object) title-widget)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[title-widget]{a @class{gtk-widget} custom widget to use for a title}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{custom-title} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-custom-title} retrieves the
  custom title widget of the header. The slot access function
  @sym{(setf gtk-header-bar-custom-title)} sets a custom title widget for the
  header bar.

  The title should help a user identify the current view. This supersedes any
  title set by the functions @fun{gtk-header-bar-title} or
  @fun{gtk-header-bar-subtitle}. To achieve the same style as the builtin
  title and subtitle, use the \"title\" and \"subtitle\" style classes.

  You should set the custom title to @code{nil}, for the header title label to
  be visible again.

  Since 3.10
  @see-class{gtk-header-bar}
  @see-function{gtk-header-bar-title}
  @see-function{gtk-header-bar-subtitle}")

;;; --- gtk-header-bar-decoration-layout ---------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "decoration-layout"
                                               'gtk-header-bar) 't)
 "The @code{decoration-layout} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The decoration layout for buttons. If this property is not set, the
  @slot[gtk-settings]{gtk-decoration-layout} setting is used. See the function
  @fun{gtk-header-bar-decoration-layout} for information about the format of
  this string. Since 3.12 @br{}
  Default value: @code{nil}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-decoration-layout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-decoration-layout 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-decoration-layout object) => layout}
  @syntax[]{(setf (gtk-header-bar-decoration-layout object) layout)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[layout]{a decoration layout, or @code{nil} to unset the layout}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{decoration-layout} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-decoration-layout} gets the
  decoration layout. The slot access function
  @sym{(setf gtk-header-bar-decoration-layout} sets the decoration layout for
  the header bar, overriding the @slot[gtk-settings]{gtk-decoration-layout}
  setting.

  There can be valid reasons for overriding the setting, such as a header bar
  design that does not allow for buttons to take room on the right, or only
  offers room for a single close button. Split header bars are another example
  for overriding the setting.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are minimize, maximize, close, icon for the window
  icon and menu for a menu button for the fallback app menu.

  For example, \"menu:minimize,maximize,close\" specifies a menu on the left,
  and minimize, maximize and close buttons on the right.

  Since 3.12
  @see-class{gtk-header-bar}
  @see-class{gtk-settings}")

;;; --- gtk-header-bar-decoration-layout-set -----------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "decoration-layout-set"
                                               'gtk-header-bar) 't)
 "The @code{decoration-layout-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the @code{decoration-layout} property is set.
  Since 3.12 @br{}
  Default value: @em{false}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-decoration-layout-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-decoration-layout-set 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-decoration-layout object) => layout-set}
  @syntax[]{(setf (gtk-header-bar-decoration-layout object) layout-set)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[layout-set]{a boolean wether a decoration layout is set}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{decoration-layout-set} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  Set to @em{true} if the @slot[gtk-header-bar]{decoration-layout} property
  is set.

  Since 3.12
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-has-subtitle --------------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "has-subtitle"
                                               'gtk-header-bar) 't)
 "The @code{has-subtitle} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, reserve space for a subtitle, even if none is currently set.
  Since 3.12 @br{}
  Default value: @em{true}")

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-header-bar-has-subtitle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-has-subtitle 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-has-subtitle object) => setting}
  @syntax[]{(setf gtk-header-bar-has-subtitle object) setting)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[setting]{@em{true} to reserve space for a subtitle}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{has-subtitle} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-has-subtitle} retrieves whether
  the header bar reserves space for a subtitle, regardless if one is currently
  set or not. The slot access function @sym{(setf gtk-header-bar-has-subtitle)}
  sets whether the header bar should reserve space for a subtitle.

  Since 3.12
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-show-close-button ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-close-button"
                                               'gtk-header-bar) 't)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to show window decorations. Which buttons are actually shown and
  where is determined by the @code{decoration-layout} property, and by the
  state of the window, e. g. a close button will not be shown if the window
  can not be closed. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-show-close-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-show-close-button 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-show-close-button object) => setting}
  @syntax[]{(setf gtk-header-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[setting]{@em{true} to show standard window decorations}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{show-close-button} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-show-close-button} returns
  whether the header bar shows the standard window decorations. The slot access
  function @sym{(setf gtk-header-bar-close-button)} sets whether the header bar
  shows the standard window decorations, including close, maximize, and
  minimize.

  Since 3.10
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-spacing -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing"
                                               'gtk-header-bar) 't)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-spacing 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-show-close-button object) => setting}
  @syntax[]{(setf gtk-header-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[spacint]{an integer with the amount of space between children}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{spacing} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The amount of space between children in pixels.

  Since 3.10
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-subtitle ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "subtitle"
                                               'gtk-header-bar) 't)
 "The @code{subtitle} property of type @code{:string} (Read / Write) @br{}
  The subtitle to display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-subtitle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-subtitle 'function)
 "@version{2020-5-9}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[subtitle]{a string with the subtitle, or @code{nil}}
  @syntax[]{(gtk-header-bar-subtitle object) => subtitle}
  @syntax[]{(setf (gtk-header-bar-subtitle object) subtitle)}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{subtitle} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-subtitle} retrieves the subtitle
  of the header. The slot access function @sym{(setf gtk-header-bar-subtitle)}
  sets the subtitle of the header bar. The title should give a user an
  additional detail to help him identify the current view.

  Note that @class{gtk-header-bar} by default reserves room for the subtitle,
  even if none is currently set. If this is not desired, set the
  @slot[gtk-header-bar]{has-subtitle} property to @em{false}.

  Since: 3.10
  @see-class{gtk-header-bar}")

;;; --- gtk-header-bar-title ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                                               'gtk-header-bar) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title to display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-title 'function)
 "@version{2020-5-9}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[title]{a string with the title, or @code{nil}.}
  @syntax[]{(gtk-header-bar-title object) => title}
  @syntax[]{(setf (gtk-header-bar-title object) title)}
  @begin{short}
    Accessor of the @slot[gtk-header-bar]{title} slot of the
    @class{gtk-header-bar} class.
  @end{short}

  The slot access function @sym{gtk-header-bar-title} retrieves the title of
  the header. The slot access function @sym{(setf gtk-header-bar-title)} returns
  the title of the header, or @code{nil} if none has been set explicitly.

  The title should help a user identify the current view. A good title should
  not include the application name.

  Since: 3.10
  @see-class{gtk-header-bar}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-header-bar-child-pack-type -----------------------------------------

(define-child-property "GtkHeaderBar"
                       gtk-header-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-child-pack-type 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-child-pack-type object) => pack-type)}
  @syntax[]{(setf (gtk-header-bar-child-pack-type object) pack-type)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[pack-type]{a value of the @symbol{gtk-pack-type} enumeration for
    the child widget}
  @begin{short}
    Accessor of the @code{pack-type} child property of the
    @class{gtk-header-bar} class.
  @end{short}

  A value of the @symbol{gtk-pack-type} enumeration indicating whether the
  child widget is packed with reference to the start or end of the parent.

  Since 3.10
  @see-class{gtk-header-bar}
  @see-symbol{gtk-pack-type}")

;;; --- gtk-header-bar-child-position ------------------------------------------

(define-child-property "GtkHeaderBar"
                       gtk-header-bar-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-header-bar-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-header-bar-child-position 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-header-bar-child-position object) => position)}
  @syntax[]{(setf (gtk-header-bar-child-position object) position)}
  @argument[object]{a @class{gtk-header-bar} container}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[position]{an integer with the index of the child widget in the
    header bar}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk-header-bar} class.
  @end{short}

  The index of the child widget in the heaer bar.

  Since 3.10
  @see-class{gtk-header-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-header-bar-new))

(defun gtk-header-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @return{A new @class{gtk-header-bar} container.}
  @begin{short}
    Creates a new @class{gtk-header-bar} container.
  @end{short}

  Since 3.10
  @see-class{gtk-header-bar}"
  (make-instance 'gtk-header-bar))

(export 'gtk-header-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_header_bar_pack_start" gtk-header-bar-pack-start) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[header-bar]{a @class{gtk-header-bar} container}
  @argument[child]{the @class{gtk-widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the start
    of the header bar.
  @end{short}

  Since 3.10
  @see-class{gtk-header-bar}
  @see-function{gtk-header-bar-pack-end}"
  (header-bar (g-object gtk-header-bar))
  (child (g-object gtk-widget)))

(export 'gtk-header-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_header_bar_pack_end" gtk-header-bar-pack-end) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[header-bar]{a @class{gtk-header-bar} container}
  @argument[child]{the @class{gtk-widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the end of
    the header bar.
  @end{short}

  Since 3.10
  @see-class{gtk-header-bar}
  @see-function{gtk-header-bar-pack-start}"
  (header-bar (g-object gtk-header-bar))
  (child (g-object gtk-widget)))

(export 'gtk-header-bar-pack-end)

;;; --- End of file gtk.header-bar.lisp ----------------------------------------

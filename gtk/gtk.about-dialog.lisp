;;; ----------------------------------------------------------------------------
;;; gtk.about-dialog.lisp
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
;;; GtkAboutDialog
;;;
;;;     Display information about an application
;;;
;;; Types and Values
;;;
;;;     GtkAboutDialog
;;;     GtkLicense
;;;
;;; Functions
;;;
;;;     gtk_about_dialog_new
;;;     gtk_about_dialog_get_program_name                  Accessor
;;;     gtk_about_dialog_set_program_name                  Accessor
;;;     gtk_about_dialog_get_version                       Accessor
;;;     gtk_about_dialog_set_version                       Accessor
;;;     gtk_about_dialog_get_copyright                     Accessor
;;;     gtk_about_dialog_set_copyright                     Accessor
;;;     gtk_about_dialog_get_comments                      Accessor
;;;     gtk_about_dialog_set_comments                      Accessor
;;;     gtk_about_dialog_get_license                       Accessor
;;;     gtk_about_dialog_set_license                       Accessor
;;;     gtk_about_dialog_get_wrap_license                  Accessor
;;;     gtk_about_dialog_set_wrap_license                  Accessor
;;;     gtk_about_dialog_get_license_type                  Accessor
;;;     gtk_about_dialog_set_license_type                  Accessor
;;;     gtk_about_dialog_get_website                       Accessor
;;;     gtk_about_dialog_set_website                       Accessor
;;;     gtk_about_dialog_get_website_label                 Accessor
;;;     gtk_about_dialog_set_website_label                 Accessor
;;;     gtk_about_dialog_get_authors                       Accessor
;;;     gtk_about_dialog_set_authors                       Accessor
;;;     gtk_about_dialog_get_artists                       Accessor
;;;     gtk_about_dialog_set_artists                       Accessor
;;;     gtk_about_dialog_get_documenters                   Accessor
;;;     gtk_about_dialog_set_documenters                   Accessor
;;;     gtk_about_dialog_get_translator_credits            Accessor
;;;     gtk_about_dialog_set_translator_credits            Accessor
;;;     gtk_about_dialog_get_logo                          Accessor
;;;     gtk_about_dialog_set_logo                          Accessor
;;;     gtk_about_dialog_get_logo_icon_name                Accessor
;;;     gtk_about_dialog_set_logo_icon_name                Accessor
;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog
;;;
;;; Properties
;;;
;;;          GStrv    artists               Read / Write
;;;          GStrv    authors               Read / Write
;;;          gchar*   comments              Read / Write
;;;          gchar*   copyright             Read / Write
;;;          GStrv    documenters           Read / Write
;;;          gchar*   license               Read / Write
;;;     GtkLicense    license-type          Read / Write
;;;      GdkPixbuf*   logo                  Read / Write
;;;          gchar*   logo-icon-name        Read / Write
;;;          gchar*   program-name          Read / Write
;;;          gchar*   translator-credits    Read / Write
;;;          gchar*   version               Read / Write
;;;          gchar*   website               Read / Write
;;;          gchar*   website-label         Read / Write
;;;       gboolean    wrap-license          Read / Write
;;;
;;; Signals
;;;
;;;       gboolean    activate-link         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkAboutDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAboutDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkLicense
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkLicense" gtk-license
  (:export t
   :type-initializer "gtk_license_get_type")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8)
  (:gpl-2-0-only 9)
  (:gpl-3-0-only 10)
  (:lgpl-2-1-only 11)
  (:lgpl-3-0-only 12)
  #+gtk-3-22
  (:agpl-3-0 13)
  #+gtk-3-22
  (:agpl-3-0-only 14)
)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-license atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-license atdoc:*external-symbols*)
 "@version{*2020-5-26}
  @short{The type of license for an application.}
  This enumeration can be expanded at later date.
  @begin{pre}
(define-g-enum \"GtkLicense\" gtk-license
  (:export t
   :type-initializer \"gtk_license_get_type\")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8)
  (:gpl-2-0-only 9)
  (:gpl-3-0-only 10)
  (:lgpl-2-1-only 11)
  (:lgpl-3-0-only 12)
  (:agpl-3-0 13)
  (:agpl-3-0-only 14))
  @end{pre}
  @begin[code]{table}
    @entry[:unknown]{No license specified.}
    @entry[:custom]{A license text is going to be specified by the developer.}
    @entry[:gpl-2-0]{The GNU General Public License, version 2.0.}
    @entry[:gpl-3-0]{The GNU General Public License, version 3.0.}
    @entry[:lgpl-2-1]{The GNU Lesser General Public License, version 2.1.}
    @entry[:lgpl-3-0]{The GNU Lesser General Public License, version 3.0.}
    @entry[:bsd]{The BSD standard license.}
    @entry[MIT_X11]{The MIT/X11 standard license.}
    @entry[:artistic]{The Artistic License, version 2.0.}
    @entry[:gpl-2-0-only]{The GNU General Public License, version 2.0 only.}
    @entry[:gpl-3-0-only]{The GNU General Public License, version 3.0 only.}
    @entry[:lgpl-2-1-only]{The GNU Lesser General Public License, version 2.1
      only.}
    @entry[:lgpl-3-0-only]{The GNU Lesser General Public License, version 3.0
      only.}
    @entry[:agpl-3-0]{The GNU Affero General Public License, version 3.0 or
      later. Since 3.22.}
    @entry[:agpl-3-0-only]{The GNU Affero General Public License, version 3.0
      only. Since 3.22.}
  @end{table}
  @see-class{gtk-about-dialog}")

;;; ----------------------------------------------------------------------------
;;; struct GtkAboutDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAboutDialog" gtk-about-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_about_dialog_get_type")
  ((artists
    gtk-about-dialog-artists
    "artists" "GStrv" t t)
   (authors
    gtk-about-dialog-authors
    "authors" "GStrv" t t)
   (comments
    gtk-about-dialog-comments
    "comments" "gchararray" t t)
   (copyright
    gtk-about-dialog-copyright
    "copyright" "gchararray" t t)
   (documenters
    gtk-about-dialog-documenters
    "documenters" "GStrv" t t)
   (license
    gtk-about-dialog-license
    "license" "gchararray" t t)
   (license-type
    gtk-about-dialog-license-type
    "license-type" "GtkLicense" t t)
   (logo
    gtk-about-dialog-logo
    "logo" "GdkPixbuf" t t)
   (logo-icon-name
    gtk-about-dialog-logo-icon-name
    "logo-icon-name" "gchararray" t t)
   (program-name
    gtk-about-dialog-program-name
    "program-name" "gchararray" t t)
   (translator-credits
    gtk-about-dialog-translator-credits
    "translator-credits" "gchararray" t t)
   (version
    gtk-about-dialog-version
    "version" "gchararray" t t)
   (website
    gtk-about-dialog-website
    "website" "gchararray" t t)
   (website-label
    gtk-about-dialog-website-label
    "website-label" "gchararray" t t)
   (wrap-license
    gtk-about-dialog-wrap-license
    "wrap-license" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-about-dialog 'type)
 "@version{*2020-5-26}
  @begin{short}
    The @sym{gtk-about-dialog} offers a simple way to display information about
    a program like its logo, name, copyright, website and license.
  @end{short}
  It is also possible to give credits to the authors, documenters, translators
  and artists who have worked on the program. An about dialog is typically
  opened when the user selects the About option from the Help menu. All parts
  of the dialog are optional.

  @image[aboutdialog]{}

  About dialog often contain links and email addresses. @sym{gtk-about-dialog}
  displays these as clickable links. By default, it calls the function
  @fun{gtk-show-uri} when a user clicks one. The behaviour can be overridden
  with the \"activate-link\" signal.

  To make constructing a @sym{gtk-about-dialog} as convenient as possible, you
  can use the function @fun{gtk-show-about-dialog} which constructs and shows
  a dialog and keeps it around so that it can be shown again.

  Note that GTK+ sets a default title of \"About @code{%s}\" on the dialog
  window where @code{%s} is replaced by the name of the application, but in
  order to ensure proper translation of the title, applications should set the
  title property explicitly when constructing a @sym{gtk-about-dialog}, as shown
  in the following example:
  @begin{pre}
(gtk-show-about-dialog nil
                       :program-name \"ExampleCode\"
                       :logo example-logo
                       :title \"About ExampleCode\")
  @end{pre}
  It is also possible to show a @sym{gtk-about-dialog} like any other
  @class{gtk-dialog}, e.g. using the function @fun{gtk-dialog-run}. In this
  case, you might need to know that the \"Close\" button returns the
  @code{:cancel} response ID.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
 lambda (dialog uri)    : Run Last
      @end{pre}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the function
      @fun{gtk-show-uri}.
      @begin[code]{table}
        @entry[dialog]{The @sym{gtk-about-dialog} widget on which the signal
          was emitted.}
        @entry[uri]{A string with the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-about-dialog-artists}
  @see-slot{gtk-about-dialog-authors}
  @see-slot{gtk-about-dialog-comments}
  @see-slot{gtk-about-dialog-copyright}
  @see-slot{gtk-about-dialog-documenters}
  @see-slot{gtk-about-dialog-license}
  @see-slot{gtk-about-dialog-license-type}
  @see-slot{gtk-about-dialog-logo}
  @see-slot{gtk-about-dialog-logo-icon-name}
  @see-slot{gtk-about-dialog-program-name}
  @see-slot{gtk-about-dialog-translator-credits}
  @see-slot{gtk-about-dialog-version}
  @see-slot{gtk-about-dialog-website}
  @see-slot{gtk-about-dialog-website-label}
  @see-slot{gtk-about-dialog-wrap-license}
  @see-class{gtk-dialog}
  @see-symbol{gtk-license}
  @see-function{gtk-dialog-run}
  @see-function{gtk-show-uri}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-about-dialog-artists -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "artists" 'gtk-about-dialog) 't)
 "The @code{artists} property of type @type{g-strv} (Read / Write) @br{}
  The people who contributed artwork to the program, as a list of strings. Each
  string may contain email addresses and URLs, which will be displayed as links,
  see the introduction for more details.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-artists atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-artists 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-artists object) => artists}
  @syntax[]{(setf (gtk-about-dialog-artists object) artists)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[artists]{a list of strings with the people who contributed artwork
    to the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{artists} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-artists} returns the strings
  which are displayed in the artists tab of the secondary credits dialog. The
  slot access function @sym{(setf gtk-about-dialog-artists)} function sets the
  strings which are displayed in the artists tab of the secondary credits
  dialog.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-authors -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "authors" 'gtk-about-dialog) 't)
 "The @code{authors} property of type @type{g-strv} (Read / Write) @br{}
  The authors of the program, as a list of strings. Each string may contain
  email addresses and URLs, which will be displayed as links, see the
  introduction for more details.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-authors atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-authors 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-authors object) => authors}
  @syntax[]{(setf (gtk-about-dialog-authors object) authors)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[authors]{a list of string with the authors of the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{authors} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-authors} returns the strings
  which are displayed in the authors tab of the secondary credits dialog. The
  slot access function @sym{(setf gtk-about-dialog-authors)} sets the strings
  which are displayed in the authors tab of the secondary credits dialog.
  @begin[Example]{dictionary}
    @begin{pre}
 (setq about (make-instance 'gtk-about-dialog))
=> ABOUT
 (setf (gtk-about-dialog-artists about)
       (list \"first author\" \"second author\"))
=> (\"first author\" \"second author\")
 (gtk-about-dialog-artists about)
=> (\"first author\" \"second author\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-comments ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "comments" 'gtk-about-dialog) 't)
 "The @code{comments} property of type @code{:string} (Read / Write) @br{}
  Comments about the program. This string is displayed in a label in the main
  dialog, thus it should be a short explanation of the main purpose of the
  program, not a detailed list of features. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-comments atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-comments 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-comments object) => comments}
  @syntax[]{(setf (gtk-about-dialog-comments object) comments)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[comments]{a string with comments about the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{comments} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-comments} returns the comments
  string. The slot access function @sym{(setf gtk-about-dialog-comments)} sets
  the comments string to display in the about dialog.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-copyright ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "copyright"
                                               'gtk-about-dialog) 't)
 "The @code{copyright} property of type @code{:string} (Read / Write) @br{}
  Copyright information for the program. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-copyright atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-copyright 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-copyright object) => copyright}
  @syntax[]{(setf (gtk-about-dialog-copyright object) copyright)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[copyright]{a string with copyright information}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{copyright} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-copyright} returns the
  copyright string. The slot access function
  @sym{(setf gtk-about-dialog-copyright)} sets the copyright string to display
  in the about dialog.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-documenters -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "documenters"
                                               'gtk-about-dialog) 't)
 "The @code{documenters} property of type @type{g-strv} (Read / Write) @br{}
  The people documenting the program, as a list of strings. Each string may
  contain email addresses and URLs, which will be displayed as links.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-documenters atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-documenters 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-documenters object) => documenters}
  @syntax[]{(setf (gtk-about-dialog-documenters object) documenters)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[documenters]{a string with the people documenting the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{documenters} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-documenters} returns the
  strings which are displayed in the documenters tab of the secondary credits
  dialog. The slot access function @sym{(setf gtk-about-dialog-documenters)}
  sets the strings which are displayed in the documenters tab of the secondary
  credits dialog.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-license -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "license" 'gtk-about-dialog) 't)
 "The @code{license} property of type @code{:string} (Read / Write) @br{}
  The license of the program. This string is displayed in a text view in a
  secondary dialog, therefore it is fine to use a long multi-paragraph text.
  Note that the text is only wrapped in the text view if the @code{wrap-license}
  property is set to @em{true}. Otherwise the text itself must contain the
  intended linebreaks. When setting this property to a non-@code{nil} value, the
  @code{license-type} property is set to the value @code{:custom} of the
  @symbol{gtk-license} enumeration as a side effect. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-license atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-license 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-license object) => license}
  @syntax[]{(setf (gtk-about-dialog-license object) license)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[license]{a string with the license of the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{license} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-license} returns the license
  information. The slot access function @sym{(setf gtk-about-dialog-license)}
  sets the license information to be displayed in the secondary license dialog.
  If the argument @arg{license} is @code{nil}, the license button is hidden.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-license-type ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "license-type"
                                               'gtk-about-dialog) 't)
 "The @code{license-type} property of type @symbol{gtk-license} (Read / Write)
  @br{}
  The license of the program, as a value of the @symbol{gtk-license}
  enumeration. The @sym{gtk-about-dialog} will automatically fill out a standard
  disclaimer and link the user to the appropriate online resource for the
  license text. If @code{:unknown} is used, the link used will be the same
  specified in the @code{website} property. If @code{:custom} is used, the
  current contents of the @code{license} property are used. For any other
  @symbol{gtk-license} value, the contents of the @code{license} property
  are also set by this property as a side effect. @br{}
  Default value: @code{:unkown}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-license-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-license-type 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-license-type object) => license-type}
  @syntax[]{(setf (gtk-about-dialog-license-type object) license-type)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[license-type]{a value of the @symbol{gtk-license} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{license-type} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-license-type} retrieves the
  license type of type @symbol{gtk-license}. The slot access function
  @sym{(setf gtk-about-dialog-license-type)} sets the license of of the
  application showing the about dialog from a list of known licenses. This
  function overrides the license set using the function
  @fun{gtk-about-dialog-license}.
  @see-class{gtk-about-dialog}
  @see-symbol{gtk-license}
  @see-function{gtk-about-dialog-license}")

;;; --- gtk-about-dialog-logo --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "logo" 'gtk-about-dialog) 't)
 "The @code{logo} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  A logo for the about box. If this is not set, it defaults to the return value
  of the @fun{gtk-window-default-icon-list} function.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-logo atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-logo 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-logo object) => logo}
  @syntax[]{(setf (gtk-about-dialog-logo object) logo)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[logo]{a @class{gdk-pixbuf} logo for the about box}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{logo} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-logo} returns the pixbuf
  displayed as logo in the about dialog. The slot access function
  @sym{(setf gtk-about-dialog-logo)} sets the pixbuf to be displayed as logo in
  the about dialog. If it is @code{nil}, the default window icon set with the
  function @fun{gtk-window-set-default-icon} will be used.
  @see-class{gtk-about-dialog}
  @see-class{gdk-pixbuf}
  @see-function{gtk-window-set-default-icon}")

;;; --- gtk-about-dialog-logo-icon-name ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "logo-icon-name"
                                               'gtk-about-dialog) 't)
 "The @code{logo-icon-name} property of type @code{:string} (Read / Write) @br{}
  A named icon to use as the logo for the about box. This property overrides
  the @code{logo} property. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-logo-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-logo-icon-name 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-logo-icon-name object) => icon-name}
  @syntax[]{(setf (gtk-about-dialog-logo-icon-name object) icon-name)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[icon-name]{a string with a namend icon to use as the logo}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{logo-icon-name} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The @sym{gtk-about-dialog-logo-icon-name} slot access function returns the
  icon name displayed as logo in the about dialog. The slot access function
  @sym{(setf gtk-about-dialog-logo-icon-name)} sets the pixbuf to be displayed
  as logo in the about dialog. If it is @code{nil}, the default window icon set
  with the function @fun{gtk-window-set-default-icon} will be used.
  @see-class{gtk-about-dialog}
  @see-function{gtk-window-set-default-icon}")

;;; --- gtk-about-dialog-program-name ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "program-name"
                                               'gtk-about-dialog) 't)
 "The @code{program-name} property of type @code{:string} (Read / Write) @br{}
  The name of the program. If this is not set, it defaults to the return value
  of the function @fun{g-application-name}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-program-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-program-name 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-program-name object) => program-name}
  @syntax[]{(setf (gtk-about-dialog-program-name object) program-name)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[program-name]{a string with the name of the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{program-name} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-program-name} returns the
  program name displayed in the about dialog. The slot access function
  @sym{(setf gtk-about-dialog-program-name)} sets the name to display in the
  about dialog. If this is not set, it defaults to the return value of the
  function @fun{g-application-name}.
  @see-class{gtk-about-dialog}
  @see-function{g-application-name}")

;;; --- gtk-about-dialog-translator-credits ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "translator-credits"
                                               'gtk-about-dialog) 't)
 "The @code{translator-credits} property of type @code{:string} (Read / Write)
  @br{}
  Credits to the translators. This string should be marked as translatable.
  The string may contain email addresses and URLs, which will be displayed as
  links, see the introduction for more details. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-translator-credits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-translator-credits 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-translator-credits object) => translator-credits}
  @syntax[]{(setf (gtk-about-dialog-translator-credits object) translator-credits)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[translator-credits]{a string with the credits to the translators}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{translator-credits} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-translator-credits} returns the
  translator credits string which is displayed in the translators tab of the
  secondary credits dialog. The slot access function
  @sym{(setf gtk-about-dialog-translator-credits)} sets the translator credits
  string which is displayed in the translators tab of the secondary credits
  dialog.

  The intended use for this string is to display the translator of the
  language which is currently used in the user interface. Using
  @code{gettext()}, a simple way to achieve that is to mark the string for
  translation:
  @begin{pre}
 gtk_about_dialog_set_translator_credits (about, _(\"translator-credits\"));
  @end{pre}
  It is a good idea to use the customary msgid \"translator-credits\" for this
  purpose, since translators will already know the purpose of that msgid, and
  since @class{gtk-about-dialog} will detect if \"translator-credits\" is
  untranslated and hide the tab.
  @see-class{gtk-about-dialog}
  @see-class{gtk-dialog}")

;;; --- gtk-about-dialog-version -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "version" 'gtk-about-dialog) 't)
 "The @code{version} property of type @code{:string} (Read / Write) @br{}
  The version of the program. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-version atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-version 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-version object) => version}
  @syntax[]{(setf (gtk-about-dialog-version object) version)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[version]{a string with the version of the program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{version} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-version} returns the version
  string. The slot access function @sym{(gtk-about-dialog-version)} sets the
  version string to display in the about dialog.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-website -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "website" 'gtk-about-dialog) 't)
 "The @code{website} property of type @code{:string} (Read / Write) @br{}
  The URL for the link to the website of the program. This should be a string
  starting with \"http://\". @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-website atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-website 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-website object) => website}
  @syntax[]{(setf (gtk-about-dialog-website object) website)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[website]{a string with URL for the link to the website of the
    program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{website} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-website} returns the website
  URL. The slot access function @sym{(setf gtk-about-dialog-website)} sets the
  URL string starting with \"http://\" to use for the website link.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-website-label -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "website-label"
                                               'gtk-about-dialog) 't)
 "The @code{website-label} property of type @code{:string} (Read / Write) @br{}
  The label for the link to the website of the program. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-website-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-website-label 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-website-label object) => label}
  @syntax[]{(setf (gtk-about-dialog-website-label object) label)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[label]{a string with the label for the link to the website of the
    program}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{website-label} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-website-label} returns the
  label used for the website link. The slot access function
  @sym{(setf gtk-about-dialog-website-label)} sets the label to be used for the
  website link.
  @see-class{gtk-about-dialog}")

;;; --- gtk-about-dialog-wrap-license ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-license"
                                               'gtk-about-dialog) 't)
 "The @code{wrap-license} property of type @code{:boolean} (Read / Write) @br{}
  Whether to wrap the text in the license dialog. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-wrap-license atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-wrap-license 'function)
 "@version{*2020-5-26}
  @syntax[]{(gtk-about-dialog-wrap-license object) => wrap-license}
  @syntax[]{(setf (gtk-about-dialog-wrap-license object) wrap-license)}
  @argument[object]{a @class{gtk-about-dialog} widget}
  @argument[wrap-license]{a boolean wether to wrap the text in the license
    dialog}
  @begin{short}
    Accessor of the @slot[gtk-about-dialog]{wrap-license} slot of the
    @class{gtk-about-dialog} class.
  @end{short}

  The slot access function @sym{gtk-about-dialog-wrap-license} returns whether
  the license text in the about dialog is automatically wrapped. The slot access
  function @sym{(setf gtk-about-dialog-wrap-license)} sets whether the license
  text in the about dialog is automatically wrapped.
  @see-class{gtk-about-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-new))

(defun gtk-about-dialog-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @return{A newly created @class{gtk-about-dialog} widget.}
  @short{Creates a new about dialog.}
  @see-class{gtk-about-dialog}"
  (make-instance 'gtk-about-dialog))

(export 'gtk-about-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_add_credit_section ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_about_dialog_add_credit_section"
           gtk-about-dialog-add-credit-section) :void
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-26}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[section-name]{a string with the name of the section}
  @argument[people]{a list of strings of the people who belong to that section}
  @begin{short}
    Creates a new section in the Credits page.
  @end{short}
  @see-class{gtk-about-dialog}"
  (about (g-object gtk-about-dialog))
  (section-name :string)
  (people g-strv))

(export 'gtk-about-dialog-add-credit-section)

;;; ----------------------------------------------------------------------------
;;; gtk_show_about_dialog ()
;;; ----------------------------------------------------------------------------

(let ((about-dialog (null-pointer)))
  (defun gtk-show-about-dialog (parent &rest args)
   #+cl-cffi-gtk-documentation
   "@version{*2020-5-26}
    @argument[parent]{a @class{gtk-window} transient parent, or @code{nil}
      for none}
    @argument[args]{pairs of property name and property value}
    @begin{short}
      This is a convenience function for showing an application's about box.
    @end{short}
    The constructed dialog is associated with the parent window and reused for
    future invocations of this function.
    @see-class{gtk-about-dialog}"
    (let ((dialog (if parent
                      (g-object-data parent "gtk-about-dialog")
                      about-dialog)))
      (when (null-pointer-p (if (pointerp dialog) dialog (pointer dialog)))
        (setf dialog (apply 'make-instance (cons 'gtk-about-dialog args)))
        (g-signal-connect dialog "delete-event"
                          (lambda (widget event)
                            (declare (ignore event))
                            (gtk-widget-hide-on-delete widget)))
        ;; Response to the user interaction
        (g-signal-connect dialog "response"
           (lambda (widget response-id)
             (declare (ignore response-id))
             (let ((button-box (gtk-dialog-action-area widget)))
               (dolist (button (gtk-container-children button-box))
                 (when (g-type-check-instance-type button "GtkToggleButton")
                   (setf (gtk-toggle-button-active button) nil))))
               (gtk-widget-hide widget)))
        (if parent
            (progn
              (setf (gtk-window-modal dialog) t)
              (setf (gtk-window-transient-for dialog) parent)
              (setf (gtk-window-destroy-with-parent dialog) t)
              (setf (g-object-data parent "gtk-about-dialog") (pointer dialog)))
            (setf about-dialog dialog)))
      (gtk-window-present dialog))))

(export 'gtk-show-about-dialog)

;;; --- End of file gtk.about-dialog.lisp --------------------------------------

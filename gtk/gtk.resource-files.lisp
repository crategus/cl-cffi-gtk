;;; ----------------------------------------------------------------------------
;;; gtk.resource-files.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; Resource Files
;;;
;;; Deprecated routines for handling resource files
;;;
;;; Synopsis
;;;
;;;     GtkRcStyle
;;;     GtkRcFlags
;;;     GtkRcTokenType
;;;
;;;     gtk_rc_scanner_new
;;;     gtk_rc_get_style
;;;     gtk_rc_get_style_by_paths
;;;     gtk_rc_parse
;;;     gtk_rc_parse_string
;;;     gtk_rc_reparse_all
;;;     gtk_rc_reparse_all_for_settings
;;;     gtk_rc_reset_styles
;;;     gtk_rc_add_default_file
;;;     gtk_rc_get_default_files
;;;     gtk_rc_set_default_files
;;;     gtk_rc_parse_color
;;;     gtk_rc_parse_color_full
;;;     gtk_rc_parse_state
;;;     gtk_rc_parse_priority
;;;     gtk_rc_find_module_in_path
;;;     gtk_rc_find_pixmap_in_path
;;;     gtk_rc_get_module_dir
;;;     gtk_rc_get_im_module_path
;;;     gtk_rc_get_im_module_file
;;;     gtk_rc_get_theme_dir
;;;     gtk_rc_style_new
;;;     gtk_rc_style_copy
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkRcStyle
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRcStyle
;;;
;;; typedef struct {
;;;   gchar *name;
;;;   gchar *bg_pixmap_name[5];
;;;   PangoFontDescription *font_desc;
;;;
;;;   GtkRcFlags color_flags[5];
;;;   GdkColor   fg[5];
;;;   GdkColor   bg[5];
;;;   GdkColor   text[5];
;;;   GdkColor   base[5];
;;;
;;;   gint xthickness;
;;;   gint ythickness;
;;; } GtkRcStyle;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRcStyle" gtk-rc-style
  (:superclass g-object
   :export nil
   :interfaces nil
   :type-initializer "gtk_rc_style_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-rc-style 'type)
 "@version{2013-4-21}
  @begin{short}
    GTK+ provides resource file mechanism for configuring various aspects
    of the operation of a GTK+ program at runtime.
    The @sym{gtk-rc-style} structure is used to represent a set of information
    about the appearance of a widget. This can later be composited together
    with other @sym{gtk-rc-style} structures to form a @code{GtkStyle} object.
  @end{short}

  @subheading{Warning}
    In GTK+ 3.0, resource files have been deprecated and replaced by
    CSS-like style sheets, which are understood by @class{gtk-css-provider}.

  @subheading{Default files}
    An application can cause GTK+ to parse a specific RC file by calling the
    function @sym{gtk-rc-parse}. In addition to this, certain files will be
    read at the end of the function @code{gtk_init()}. Unless modified, the
    files looked for will be @code{<SYSCONFDIR>/gtk-2.0/gtkrc} and
    @code{.gtkrc-3.0} in the users home directory. @code{(<SYSCONFDIR>} defaults
    to @code{/usr/local/etc}. It can be changed with the @code{--prefix} or
    @code{--sysconfdir} options when configuring GTK+.)

    The set of these default files can be retrieved with the function
    @sym{gtk-rc-get-default-files} and modified with the functions
    @sym{gtk-rc-add-default-file} and @sym{gtk-rc-set-default-files}.
    Additionally, the @code{GTK2_RC_FILES} environment variable can be set to a
    @code{G_SEARCHPATH_SEPARATOR_S-separated} list of files in order to
    overwrite the set of default files at runtime.

    For each RC file, in addition to the file itself, GTK+ will look for a
    locale-specific file that will be parsed after the main file. For
    instance, if @code{LANG} is set to @code{ja_JP.ujis}, when loading the
    default file @code{~/.gtkrc} then GTK+ looks for @code{~/.gtkrc.ja_JP} and
    @code{~/.gtkrc.ja}, and parses the first of those that exists.

  @subheading{Pathnames and patterns}
    A resource file defines a number of styles and key bindings and
    attaches them to particular widgets. The attachment is done by the
    widget, @code{widget_class}, and class declarations. As an example of such
    a statement:
    @begin{pre}
 widget \"mywindow.*.GtkEntry\" style \"my-entry-class\"
    @end{pre}
    attaches the style @code{\"my-entry-class\"} to all widgets whose widget
    path matches the pattern @code{\"mywindow.*.GtkEntry\"}. That is, all
    @class{gtk-entry} widgets which are part of a @class{gtk-window} named
    @code{\"mywindow\"}.

    The patterns here are given in the standard shell glob syntax. The \"?\"
    wildcard matches any character, while \"*\" matches zero or more of any
    character. The three types of matching are against the widget path, the
    class path and the class hierarchy. Both the widget path and the class
    path consist of a \".\" separated list of all the parents of the widget
    and the widget itself from outermost to innermost. The difference is
    that in the widget path, the name assigned by the generic function
    @fun{gtk-widget-name} is used if present, otherwise the class name of
    the widget, while for the class path, the class name is always used.

    Since GTK+ 2.10, widget_class paths can also contain @code{<classname>}
    substrings, which are matching the class with the given name and any
    derived classes. For instance,
    @begin{pre}
 widget_class \"*<GtkMenuItem>.GtkLabel\" style \"my-style\"
    @end{pre}
    will match @class{gtk-label} widgets which are contained in any kind of
    menu item.

    So, if you have a @class{gtk-entry} named @code{\"myentry\"}, inside of a
    horizontal box in a window named @code{\"mywindow\"}, then the widget path
    is: @code{\"mywindow.GtkHBox.myentry\"} while the class path is:
    @code{\"GtkWindow.GtkHBox.GtkEntry\"}.

    Matching against class is a little different. The pattern match is done
    against all class names in the widgets class hierarchy (not the layout
    hierarchy) in sequence, so the pattern:
    @begin{pre}
 class \"GtkButton\" style \"my-style\"
    @end{pre}
    will match not just @class{gtk-button} widgets, but also
    @class{gtk-toggle-button} and @class{gtk-check-button} widgets, since those
    classes derive from @class{gtk-button}.

    Additionally, a priority can be specified for each pattern, and styles
    override other styles first by priority, then by pattern type and then
    by order of specification (later overrides earlier). The priorities
    that can be specified are (highest to lowest):
    @begin{pre}
 highest
 rc
 theme
 application
 gtk
 lowest
    @end{pre}
    @code{rc} is the default for styles read from an RC file, @code{theme} is
    the default for styles read from theme RC files, @code{application} should
    be used for styles an application sets up, and @code{gtk} is used for styles
    that GTK+ creates internally.

  @subheading{Theme gtkrc files}
    Theme RC files are loaded first from under the @code{~/.themes/}, then from
    the directory from the function @sym{gtk-rc-get-theme-dir}. The files looked
    at will be @code{gtk-3.0/gtkrc}.

    When the application prefers dark themes (see the
    @code{\"gtk-application-prefer-dark-theme\"} property for details),
    @code{gtk-3.0/gtkrc-dark} will be loaded first, and if not present
    @code{gtk-3.0/gtkrc} will be loaded.

  @subheading{Optimizing RC Style Matches}
    Everytime a widget is created and added to the layout hierarchy of a
    @class{gtk-window} (\"anchored\" to be exact), a list of matching RC styles
    out of all RC styles read in so far is composed. For this, every RC
    style is matched against the widgets class path, the widgets name path
    and widgets inheritance hierarchy. As a consequence, significant
    slowdown can be caused by utilization of many RC styles and by using RC
    style patterns that are slow or complicated to match against a given
    widget. The following ordered list provides a number of advices
    (prioritized by effectiveness) to reduce the performance overhead
    associated with RC style matches:
    @begin{enumerate}
      @begin{item}
        Move RC styles for specific applications into RC files dedicated to
        those applications and parse application specific RC files only
        from applications that are affected by them. This reduces the
        overall amount of RC styles that have to be considered for a match
        across a group of applications.
      @end{item}
      @begin{item}
        Merge multiple styles which use the same matching rule, for instance:
        @begin{pre}
 style \"Foo\" { foo_content @}
 class \"X\" style \"Foo\"
 style \"Bar\" { bar_content @}
 class \"X\" style \"Bar\"
        @end{pre}
        is faster to match as:
        @begin{pre}
 style \"FooBar\" { foo_content bar_content @}
 class \"X\" style \"FooBar\"
        @end{pre}
      @end{item}
      @begin{item}
        Use of wildcards should be avoided, this can reduce the individual
        RC style match to a single integer comparison in most cases.
      @end{item}
      @begin{item}
        To avoid complex recursive matching, specification of full class
        names (for class matches) or full path names (for widget and
        widget_class matches) is to be preferred over shortened names
        containing \"*\" or \"?\".
      @end{item}
      @begin{item}
        If at all necessary, wildcards should only be used at the tail or
        head of a pattern. This reduces the match complexity to a string
        comparison per RC style.
      @end{item}
      @begin{item}
        When using wildcards, use of \"?\" should be preferred over \"*\". This
        can reduce the matching complexity from O(n^2) to O(n). For example
        @code{\"Gtk*Box\"} can be turned into @code{\"Gtk?Box\"} and will still
        match @code{GtkHBox} and @code{GtkVBox}.
      @end{item}
      @begin{item}
        The use of \"*\" wildcards should be restricted as much as possible,
        because matching @code{\"A*B*C*RestString\"} can result in matching
        complexities of O(n^2) worst case.
      @end{item}
    @end{enumerate}
  @subheading{Toplevel declarations}
    An RC file is a text file which is composed of a sequence of
    declarations. '#' characters delimit comments and the portion of a line
    after a '#' is ignored when parsing an RC file.

    The possible toplevel declarations are:
    @begin[code]{table}
      @entry[binding name { ... }]{Declares a binding set.}
      @entry[class pattern [ style | binding @][ : priority @] name]{Specifies a
        style or binding set for a particular branch of the inheritance
        hierarchy.}
      @entry[include filename]{Parses another file at this point. If filename is
        not an absolute filename, it is searched in the directories of the
        currently open RC files.
        GTK+ also tries to load a locale-specific variant of the included file.}
      @entry[module_path path]{Sets a path (a list of directories separated by
        colons) that will be searched for theme engines referenced in RC files.}
      @entry[pixmap_path path]{Sets a path (a list of directories separated by
        colons) that will be searched for pixmaps referenced in RC files.}
      @entry[im_module_file pathname]{Sets the pathname for the IM modules file.
        Setting this from RC files is deprecated; you should use the environment
        variable @code{GTK_IM_MODULE_FILE} instead.}
      @entry[style name [ = parent @] { ... }]{Declares a style.}
      @entry[widget pattern [ style | binding @][ : priority @] name]{Specifies
        a style or binding set for a particular group of widgets by matching on
        the widget pathname.}
      @entry[widget_class pattern [ style | binding @][ : priority @]
        name]{Specifies a style or binding set for a particular group of widgets
        by matching on the class pathname.}
      @entry[setting = value]{Specifies a value for a setting. Note that
        settings in RC files are overwritten by system-wide settings (which are
        managed by an XSettings manager on X11).}
    @end{table}
  @subheading{Styles}
    A RC style is specified by a style declaration in a RC file, and then
    bound to widgets with a widget, @code{widget_class}, or class declaration.
    All styles applying to a particular widget are composited together with
    widget declarations overriding @code{widget_class} declarations which, in
    turn, override class declarations. Within each type of declaration,
    later declarations override earlier ones.

    Within a style declaration, the possible elements are:
    @begin[code]{table}
      @entry[bg[state@] = color]{Sets the color used for the background of most
        widgets.}
      @entry[fg[state@] = color]{Sets the color used for the foreground of most
        widgets.}
      @entry[base[state@] = color]{Sets the color used for the background of
        widgets displaying editable text. This color is used for the background
        of, among others, @code{GtkText}, @class{gtk-entry}, @code{GtkList},
        and @code{GtkCList}.}
      @entry[text[state@] = color]{Sets the color used for foreground of widgets
        using base for the background color.}
      @entry[xthickness = number]{Sets the @code{xthickness}, which is used for
        various horizontal padding values in GTK+.}
      @entry[ythickness = number]{Sets the @code{ythickness}, which is used for
        various vertical padding values in GTK+.}
      @entry[bg_pixmap[state@] = pixmap]{Sets a background pixmap to be used in
        place of the bg color (or for @code{GtkText}, in place of the base
        color. The special value @code{\"<parent>\"} may be used to indicate
        that the widget should use the same background pixmap as its parent.
        The special value  @code{\"<none>\"} may be used to indicate no
        background pixmap.}
      @entry[font = font]{Starting with GTK+ 2.0, the @code{\"font\"} and
        @code{\"fontset\"} declarations are ignored; use @code{\"font_name\"}
        declarations instead.}
      @entry[fontset = font]{Starting with GTK+ 2.0, the @code{\"font\"} and
        @code{\"fontset\"} declarations are ignored; use @code{\"font_name\"}
        declarations instead.}
      @entry[font_name = font]{Sets the font for a widget. font must be a Pango
        font name, e.g. \"Sans Italic 10\". For details about Pango font names,
        see the function @fun{pango-font-description-from-string}.}
      @entry[stock[\"stock-id\"@] = { icon source specifications }]{Defines the
        icon for a stock item.}
      @entry[color[\"color-name\"@] = color specification]{Since 2.10, this
        element can be used to defines symbolic colors. See below for the syntax
        of color specifications.}
      @entry[engine \"engine\" { engine-specific settings }]{Defines the engine
        to be used when drawing with this style.}
      @entry[class::property = value]{Sets a style property for a widget class.}
    @end{table}
    The colors and background pixmaps are specified as a function of the
    state of the widget. The states are:
    @begin[code]{table}
      @entry[NORMAL]{A color used for a widget in its normal state.}
      @entry[ACTIVE]{A variant of the @code{NORMAL} color used when the widget
        is in the @code{GTK_STATE_ACTIVE} state, and also for the trough of a
        @class{gtk-scrollbar}, tabs of a @class{gtk-notebook} other than the
        current tab and similar areas. Frequently, this should be a darker
        variant of the @code{NORMAL} color.}
      @entry[PRELIGHT]{A color used for widgets in the @code{GTK_STATE_PRELIGHT}
        state. This state is the used for Buttons and MenuItems that have the
        mouse cursor over them, and for their children.}
      @entry[SELECTED]{A color used to highlight data selected by the user. For
        instance, the selected items in a list widget, and the selection in an
        editable widget.}
      @entry[INSENSITIVE]{A color used for the background of widgets that have
        been set insensitive with the function @fun{gtk-widget-sensitive}.}
    @end{table}
    Colors can be specified as a string containing a color name (GTK+ knows
    all names from the X color database @code{/usr/lib/X11/rgb.txt}), in one of
    the hexadecimal forms @code{rrrrggggbbbb}, @code{rrrgggbbb}, @code{rrggbb},
    or @code{rgb}, where @code{r}, @code{g} and @code{b} are hex digits, or they
    can be specified as a triplet @code{{ r, g, b@}}, where @code{r}, @code{g}
    and @code{b} are either integers in the range 0 - 65535 or floats
    in the range 0.0 - 1.0.

    Since 2.10, colors can also be specified by refering to a symbolic
    color, as follows: @@color-name, or by using expressions to combine
    colors. The following expressions are currently supported:
    @begin[code]{table}
      @entry[mix (factor, color1, color2)]{Computes a new color by mixing color1
        and color2. The factor determines how close the new color is to color1.
        A factor of 1.0 gives pure color1, a factor of 0.0 gives pure color2.}
      @entry[shade (factor, color)]{Computes a lighter or darker variant of
        color. A factor of 1.0 leaves the color unchanged, smaller factors yield
        darker colors, larger factors yield lighter colors.}
      @entry[lighter (color)]{This is an abbreviation for shade (1.3, color).}
      @entry[darker (color)]{This is an abbreviation for shade (0.7, color).}
    @end{table}
    Here are some examples of color expressions:
    @begin{pre}
 mix (0.5, \"red\", \"blue\")
 shade (1.5, mix (0.3,
                  \"<GTKDOCLINK HREF=\"0abbc0\">0abbc0</GTKDOCLINK>\",
                  { 0.3, 0.5, 0.9 @}))
 lighter (@@foreground)
    @end{pre}
    In a stock definition, icon sources are specified as a 4-tuple of image
    filename or icon name, text direction, widget state, and size, in that
    order. Each icon source specifies an image filename or icon name to use
    with a given direction, state, and size. Filenames are specified as a
    string such as @code{\"itemltr.png\"}, while icon names (looked up in the
    current icon theme), are specified with a leading @@, such as
    @@\"item-ltr\". The * character can be used as a wildcard, and if
    @code{direction/state/size} are omitted they default to *. So for example,
    the following specifies different icons to use for left-to-right and
    right-to-left languages:
    @begin{pre}
    stock[\"my-stock-item\"] =
    {
      { \"itemltr.png\", LTR, *, * @},
      { \"itemrtl.png\", RTL, *, * @}
    @}
    @end{pre}
    This could be abbreviated as follows:
    @begin{pre}
    stock[\"my-stock-item\"] =
    {
      { \"itemltr.png\", LTR @},
      { \"itemrtl.png\", RTL @}
    @}
    @end{pre}
    You can specify custom icons for specific sizes, as follows:
    @begin{pre}
    stock[\"my-stock-item\"] =
    {
      { \"itemmenusize.png\", *, *, \"gtk-menu\" @},
      { \"itemtoolbarsize.png\", *, *, \"gtk-large-toolbar\" @}
      { \"itemgeneric.png\" @} // implicit *, *, * as a fallback
    @}
    @end{pre}
    The sizes that come with GTK+ itself are \"gtk-menu\",
    \"gtk-small-toolbar\", \"gtk-large-toolbar\", \"gtk-button\", \"gtk-dialog\".
    Applications can define other sizes.

    It's also possible to use custom icons for a given state, for example:
    @begin{pre}
    stock[\"my-stock-item\"] =
    {
      { \"itemprelight.png\", *, PRELIGHT @},
      { \"iteminsensitive.png\", *, INSENSITIVE @},
      { \"itemgeneric.png\" @} // implicit *, *, * as a fallback
    @}
    @end{pre}
    When selecting an icon source to use, GTK+ will consider text direction
    most important, state second, and size third. It will select the best
    match based on those criteria. If an attribute matches exactly (e.g.
    you specified @code{PRELIGHT} or specified the size), GTK+ won't modify the
    image; if the attribute matches with a wildcard, GTK+ will scale or
    modify the image to match the state and size the user requested.

  @subheading{Key bindings}
    Key bindings allow the user to specify actions to be taken on
    particular key presses. The form of a binding set declaration is:
    @begin{pre}
 binding <em class=\"replaceable\"><code>name</code></em> {
   bind <em class=\"replaceable\"><code>key</code></em> {
     <em class=\"replaceable\"><code>signalname</code></em>
    (<em class=\"replaceable\"><code>param</code></em>, ...)
     ...
   @}
   ...
 @}
    @end{pre}
    key is a string consisting of a series of modifiers followed by the
    name of a key. The modifiers can be:
    @begin{pre}
    <alt>
    <ctl>
    <control>
    <meta>
    <hyper>
    <super>
    <mod1>
    <mod2>
    <mod3>
    <mod4>
    <mod5>
    <release>
    <shft>
    <shift>
    @end{pre}
    @code{<shft>} is an alias for @code{<shift>}, @code{<ctl>} is an alias for
    @code{<control>}, and @code{<alt>} is an alias for @code{<mod1>}.

    The action that is bound to the key is a sequence of signal names
    (strings) followed by parameters for each signal. The signals must be
    action signals. Each parameter can be a float, integer, string, or unquoted
    string representing an enumeration value. The types of the parameters
    specified must match the types of the parameters of the signal.

    Binding sets are connected to widgets in the same manner as styles,
    with one difference: Binding sets override other binding sets first by
    pattern type, then by priority and then by order of specification. The
    priorities that can be specified and their default values are the same
    as for styles.")

;;; ----------------------------------------------------------------------------
;;; enum GtkRcFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkRcFlags" gtk-rc-flags
  (:export nil
   :type-initializer "gtk_rc_flags_get_type")
  (:fg 1)
  (:bg 2)
  (:text 4)
  (:base 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-rc-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-rc-flags atdoc:*external-symbols*)
 "@version{2013-4-21}
  @short{ }
  @begin{pre}
(define-g-flags \"GtkRcFlags\" gtk-rc-flags
  (:export t
   :type-initializer \"gtk_rc_flags_get_type\")
  (:fg 1)
  (:bg 2)
  (:text 4)
  (:base 8))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; enum GtkRcTokenType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRcTokenType" gtk-rc-token-type
  (:export nil
   :type-initializer "gtk_rc_token_type_get_type")
  (:invalid 270)
  (:include 271)
  (:normal 272)
  (:active 273)
  (:prelight 274)
  (:selected 275)
  (:insensitive 276)
  (:fg 277)
  (:bg 278)
  (:text 279)
  (:base 280)
  (:xthickness 281)
  (:ythickness 282)
  (:font 283)
  (:fontset 284)
  (:font-name 285)
  (:bg-pixmap 286)
  (:pixmap-path 287)
  (:style 288)
  (:binding 289)
  (:bind 290)
  (:widget 291)
  (:widget-class 292)
  (:class 293)
  (:lowest 294)
  (:gtk 295)
  (:application 296)
  (:theme 297)
  (:rc 298)
  (:highest 299)
  (:engine 300)
  (:module-path 301)
  (:im-module-path 302)
  (:im-module-file 303)
  (:stock 304)
  (:ltr 305)
  (:rtl 306)
  (:color 307)
  (:unbind 308)
  (:last 309))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-rc-token-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-rc-token-type atdoc:*external-symbols*)
 "@version{2013-3-18}
  @subheading{Warning}
    @sym{gtk-rc-token-type} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use @class{gtk-css-provider} instead.

  @begin{short}
    The @sym{gtk-rc-token-type} enumeration represents the tokens in the RC
    file. It is exposed so that theme engines can reuse these tokens when
    parsing the theme-engine specific portions of a RC file.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRcTokenType\" gtk-rc-token-type
  (:export t
   :type-initializer \"gtk_rc_token_type_get_type\")
  (:invalid 270)
  (:include 271)
  (:normal 272)
  (:active 273)
  (:prelight 274)
  (:selected 275)
  (:insensitive 276)
  (:fg 277)
  (:bg 278)
  (:text 279)
  (:base 280)
  (:xthickness 281)
  (:ythickness 282)
  (:font 283)
  (:fontset 284)
  (:font-name 285)
  (:bg-pixmap 286)
  (:pixmap-path 287)
  (:style 288)
  (:binding 289)
  (:bind 290)
  (:widget 291)
  (:widget-class 292)
  (:class 293)
  (:lowest 294)
  (:gtk 295)
  (:application 296)
  (:theme 297)
  (:rc 298)
  (:highest 299)
  (:engine 300)
  (:module-path 301)
  (:im-module-path 302)
  (:im-module-file 303)
  (:stock 304)
  (:ltr 305)
  (:rtl 306)
  (:color 307)
  (:unbind 308)
  (:last 309))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_rc_scanner_new ()
;;;
;;; GScanner * gtk_rc_scanner_new (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_scanner_new has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_style ()
;;;
;;; GtkStyle * gtk_rc_get_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_style has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkStyleContext instead
;;;
;;;    Finds all matching RC styles for a given widget, composites them
;;;    together, and then creates a GtkStyle representing the composite
;;;    appearance. (GTK+ actually keeps a cache of previously created styles,
;;;    so a new style may not be created.)
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the resulting style. No refcount is added to the returned style, so if
;;;     you want to save this style around, you should add a reference
;;;     yourself
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_style_by_paths ()
;;;
;;; GtkStyle * gtk_rc_get_style_by_paths (GtkSettings * settings,
;;;                                       const char *widget_path,
;;;                                       const char *class_path,
;;;                                       GType type);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_style_by_paths has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkStyleContext
;;;    instead
;;;
;;;    Creates up a GtkStyle from styles defined in a RC file by
;;;    providing the raw components used in matching. This function may be
;;;    useful when creating pseudo-widgets that should be themed like widgets
;;;    but don't actually have corresponding GTK+ widgets. An example of this
;;;    would be items inside a GNOME canvas widget.
;;;
;;;    The action of gtk_rc_get_style() is similar to:
;;;
;;;    gtk_widget_path (widget, NULL, &path, NULL);
;;;    gtk_widget_class_path (widget, NULL, &class_path, NULL);
;;;    gtk_rc_get_style_by_paths (gtk_widget_get_settings (widget),
;;;                               path, class_path,
;;;                               G_OBJECT_TYPE (widget));
;;;
;;;    settings :
;;;    a GtkSettings object
;;;
;;;    widget_path :
;;;    the widget path to use when looking up the style, or NULL if no
;;;    matching against the widget path should be done
;;;
;;;    class_path :
;;;    the class path to use when looking up the style, or NULL if no
;;;    matching against the class path should be done
;;;
;;;    type :
;;;    a type that will be used along with parent types of this type when
;;;    matching against class styles, or G_TYPE_NONE
;;;
;;;    Returns :
;;;    A style created by matching with the supplied paths, or NULL if
;;;    nothing matching was specified and the default style should be used.
;;;    The returned value is owned by GTK+ as part of an internal cache, so
;;;    you must call g_object_ref() on the returned value if you want to
;;;    keep a reference to it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse ()
;;;
;;; void gtk_rc_parse (const gchar *filename);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Parses a given resource file.
;;;
;;; filename :
;;;     the filename of a file to parse. If filename is not absolute, it is
;;;     searched in the current directory.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse_string ()
;;;
;;; void gtk_rc_parse_string (const gchar *rc_string);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse_string has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Parses resource information directly from a string.
;;;
;;; rc_string :
;;;     a string to parse
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_reparse_all ()
;;;
;;; gboolean gtk_rc_reparse_all (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_reparse_all has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    If the modification time on any previously read file for the default
;;;    GtkSettings has changed, discard all style information and then
;;;    reread all previously read RC files.
;;;
;;;    Returns :
;;;             TRUE if the files were reread.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_reparse_all_for_settings ()
;;;
;;; gboolean gtk_rc_reparse_all_for_settings (GtkSettings * settings,
;;;                                           gboolean force_load);
;;;
;;; Warning
;;;
;;;    gtk_rc_reparse_all_for_settings has been deprecated since version 3.0
;;;    and should not be used in newly-written code. Use GtkCssProvider
;;;    instead.
;;;
;;;    If the modification time on any previously read file for the given
;;;    GtkSettings has changed, discard all style information and then
;;;    reread all previously read RC files.
;;;
;;; settings :
;;;     a GtkSettings
;;;
;;; force_load :
;;;     load whether or not anything changed
;;;
;;; Returns :
;;;     TRUE if the files were reread.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_reset_styles ()
;;;
;;; void gtk_rc_reset_styles (GtkSettings *settings);
;;;
;;; Warning
;;;
;;;    gtk_rc_reset_styles has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    This function recomputes the styles for all widgets that use a
;;;    particular GtkSettings object. (There is one GtkSettings
;;;    object per GdkScreen, see gtk_settings_get_for_screen()); It
;;;    is useful when some global parameter has changed that affects the
;;;    appearance of all widgets, because when a widget gets a new style, it
;;;    will both redraw and recompute any cached information about its
;;;    appearance. As an example, it is used when the default font size set by
;;;    the operating system changes. Note that this function does not affect
;;;    widgets that have a style set explicitely on them with
;;;    gtk_widget_set_style().
;;;
;;;    settings :
;;;              a GtkSettings
;;;
;;;    Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_add_default_file ()
;;;
;;; void gtk_rc_add_default_file (const gchar *filename);
;;;
;;; Warning
;;;
;;;    gtk_rc_add_default_file has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkStyleContext with
;;;    a custom GtkStyleProvider instead
;;;
;;;    Adds a file to the list of files to be parsed at the end of
;;;    gtk_init().
;;;
;;; filename :
;;;     the pathname to the file. If filename is not absolute, it is searched
;;;     in the current directory
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_default_files ()
;;;
;;; gchar ** gtk_rc_get_default_files (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_default_files has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkStyleContext
;;;    instead
;;;
;;;    Retrieves the current list of RC files that will be parsed at the end
;;;    of gtk_init().
;;;
;;; Returns :
;;;     A NULL-terminated array of filenames. This memory is owned by GTK+
;;;     and must not be freed by the application. If you want to store this
;;;     information, you should make a copy.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_set_default_files ()
;;;
;;; void gtk_rc_set_default_files (gchar **filenames);
;;;
;;; Warning
;;;
;;;    gtk_rc_set_default_files has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkStyleContext with
;;;    a custom GtkStyleProvider instead
;;;
;;;    Sets the list of files that GTK+ will read at the end of gtk_init().
;;;
;;;    filenames :
;;;               A NULL-terminated list of filenames
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse_color ()
;;;
;;; guint gtk_rc_parse_color (GScanner *scanner,
;;;                           GdkColor *color);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse_color has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead
;;;
;;;    Parses a color in the format expected in a RC file.
;;;
;;;    Note that theme engines should use gtk_rc_parse_color_full() in
;;;    order to support symbolic colors.
;;;
;;;    scanner :
;;;             a GScanner
;;;
;;;    color :
;;;             a pointer to a GdkColor structure in which to store the result.
;;;
;;;    Returns :
;;;             G_TOKEN_NONE if parsing succeeded, otherwise the token that was
;;;             expected but not found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse_color_full ()
;;;
;;; guint gtk_rc_parse_color_full (GScanner *scanner,
;;;                                GtkRcStyle *style,
;;;                                GdkColor *color);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse_color_full has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkCssProvider
;;;    instead
;;;
;;;    Parses a color in the format expected in a RC file. If style is not
;;;    NULL, it will be consulted to resolve references to symbolic
;;;    colors.
;;;
;;; scanner :
;;;     a GScanner
;;;
;;; style :
;;;     a GtkRcStyle, or NULL
;;;
;;; color :
;;;     a pointer to a GdkColor structure in which to store the result
;;;
;;; Returns :
;;;     G_TOKEN_NONE if parsing succeeded, otherwise the token that was
;;;     expected but not found
;;;
;;;    Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse_state ()
;;;
;;; guint gtk_rc_parse_state (GScanner *scanner,
;;;                           GtkStateType *state);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse_state has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead
;;;
;;;    Parses a GtkStateType variable from the format expected in a RC file.
;;;
;;; scanner :
;;;     a GtkScanner (must be initialized for parsing an RC file)
;;;
;;; state :
;;;     A pointer to a GtkStateType variable in which to store the result.
;;;
;;; Returns :
;;;     G_TOKEN_NONE if parsing succeeded, otherwise the token that was
;;;     expected but not found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_parse_priority ()
;;;
;;; guint gtk_rc_parse_priority (GScanner *scanner,
;;;                              GtkPathPriorityType *priority);
;;;
;;; Warning
;;;
;;;    gtk_rc_parse_priority has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use GtkCssProvider instead
;;;
;;;    Parses a GtkPathPriorityType variable from the format expected in
;;;    a RC file.
;;;
;;;   scanner :
;;;             a GtkScanner (must be initialized for parsing an RC file)
;;;
;;;   priority :
;;;             A pointer to GtkPathPriorityType variable in which to store the
;;;             result.
;;;
;;;   Returns :
;;;             G_TOKEN_NONE if parsing succeeded, otherwise the token that was
;;;             expected but not found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_find_module_in_path ()
;;;
;;; gchar * gtk_rc_find_module_in_path (const gchar * module_file);
;;;
;;; Warning
;;;
;;;    gtk_rc_find_module_in_path has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkCssProvider
;;;    instead.
;;;
;;;    Searches for a theme engine in the GTK+ search path. This function is
;;;    not useful for applications and should not be used.
;;;
;;;    module_file :
;;;    name of a theme engine
;;;
;;;    Returns :
;;;    The filename, if found (must be freed with g_free()), otherwise NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_find_pixmap_in_path ()
;;;
;;; gchar * gtk_rc_find_pixmap_in_path (GtkSettings * settings,
;;;                                     GScanner *scanner,
;;;                                     const gchar *pixmap_file);
;;;
;;; Warning
;;;
;;;    gtk_rc_find_pixmap_in_path has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkCssProvider
;;;    instead.
;;;
;;;    Looks up a file in pixmap path for the specified GtkSettings. If
;;;    the file is not found, it outputs a warning message using
;;;    g_warning() and returns NULL.
;;;
;;;    settings :
;;;    a GtkSettings
;;;
;;;    scanner :
;;;    Scanner used to get line number information for the warning message, or
;;;    NULL
;;;
;;;    pixmap_file :
;;;    name of the pixmap file to locate.
;;;
;;;    Returns :
;;;    the filename
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_module_dir ()
;;;
;;; gchar * gtk_rc_get_module_dir (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_module_dir has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Returns a directory in which GTK+ looks for theme engines. For full
;;;    information about the search for theme engines, see the docs for
;;;    GTK_PATH in Running GTK+ Applications(3).
;;;
;;;    Returns :
;;;             the directory. (Must be freed with g_free())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_im_module_path ()
;;;
;;; gchar * gtk_rc_get_im_module_path (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_im_module_path has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Obtains the path in which to look for IM modules. See the documentation
;;;    of the GTK_PATH environment variable for more details about looking up
;;;    modules. This function is useful solely for utilities supplied with
;;;    GTK+ and should not be used by applications under normal circumstances.
;;;
;;; Returns :
;;;     a newly-allocated string containing the path in which to look for IM
;;;     modules
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_im_module_file ()
;;;
;;; gchar * gtk_rc_get_im_module_file (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_im_module_file has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use GtkCssProvider
;;;    instead.
;;;
;;;    Obtains the path to the IM modules file. See the documentation of the
;;;    GTK_IM_MODULE_FILE environment variable for more details.
;;;
;;; Returns :
;;;     a newly-allocated string containing the name of the file listing the IM
;;;     modules available for loading
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_get_theme_dir ()
;;;
;;; gchar * gtk_rc_get_theme_dir (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_get_theme_dir has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Returns the standard directory in which themes should be installed.
;;;    (GTK+ does not actually use this directory itself.)
;;;
;;;    Returns :
;;;             The directory (must be freed with g_free()).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_style_new ()
;;;
;;; GtkRcStyle * gtk_rc_style_new (void);
;;;
;;; Warning
;;;
;;;    gtk_rc_style_new has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Creates a new GtkRcStyle with no fields set and a reference count of 1.
;;;
;;;    Returns :
;;;             the newly-created GtkRcStyle
;;; ----------------------------------------------------------------------------
;;;
;;; ----------------------------------------------------------------------------
;;; gtk_rc_style_copy ()
;;;
;;; GtkRcStyle * gtk_rc_style_copy (GtkRcStyle *orig);
;;;
;;; Warning
;;;
;;;    gtk_rc_style_copy has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkCssProvider instead.
;;;
;;;    Makes a copy of the specified GtkRcStyle. This function will
;;;    correctly copy an RC style that is a member of a class derived from
;;;    GtkRcStyle.
;;;
;;;    orig :
;;;        the style to copy
;;;
;;;    Returns :
;;;        the resulting GtkRcStyle
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.resource-files.lisp ------------------------------------

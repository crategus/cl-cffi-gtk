;;; ----------------------------------------------------------------------------
;;; gtk.css-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK+ Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; GtkCssProvider
;;;
;;; CSS-like styling for widgets
;;;
;;; Synopsis
;;;
;;;     GtkCssProvider
;;;
;;;     gtk_css_provider_get_default
;;;     gtk_css_provider_get_named
;;;     gtk_css_provider_load_from_data
;;;     gtk_css_provider_load_from_file
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;     GTK_CSS_PROVIDER_ERROR
;;;     GtkCssProviderError
;;;
;;;     GtkCssSection
;;;     GtkCssSectionType
;;;
;;;     gtk_css_section_get_end_line
;;;     gtk_css_section_get_end_position
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_section_type
;;;     gtk_css_section_get_start_line
;;;     gtk_css_section_get_start_position
;;;     gtk_css_section_ref
;;;     gtk_css_section_unref
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkCssProvider
;;;
;;;   GBoxed
;;;    +----GtkCssSection
;;;
;;; Implemented Interfaces
;;;
;;; GtkCssProvider implements GtkStyleProvider and GtkStyleProviderPrivate.
;;;
;;; Signals
;;;
;;;   "parsing-error"                                  : Run Last
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCssProvider
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCssProvider" gtk-css-provider
  (:superclass g-object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_css_provider_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-css-provider 'type)
 "@version{2013-8-2}
  @begin{short}
    @sym{gtk-css-provider} is an object implementing the
    @class{gtk-style-provider} interface. It is able to parse CSS-like input in
    order to style widgets.
  @end{short}

  @subheading{Default files}
    An application can cause GTK+ to parse a specific CSS style sheet by calling
    the function @fun{gtk-css-provider-load-from-file} and adding the provider
    with the functions @fun{gtk-style-context-add-provider} or
    @fun{gtk-style-context-add-provider-for-screen}. In addition, certain files
    will be read when GTK+ is initialized. First, the file
    @code{$XDG_CONFIG_HOME/gtk-3.0/gtk.css} is loaded if it exists. Then, GTK+
    tries to load @code{$HOME/.themes/theme-name/gtk-3.0/gtk.css}, falling back
    to @code{datadir/share/themes/theme-name/gtk-3.0/gtk.css}, where theme-name
    is the name of the current theme, see the @code{\"gtk-theme-name\"} setting,
    and @code{datadir} is the prefix configured when GTK+ was compiled, unless
    overridden by the @code{GTK_DATA_PREFIX} environment variable.

  @subheading{Style sheets}
    The basic structure of the style sheets understood by this provider is a
    series of statements, which are either rule sets or '@@-rules', separated by
    whitespace.

    A rule set consists of a selector and a declaration block, which is a series
    of declarations enclosed in curly braces ({ and @}). The declarations are
    separated by semicolons (;). Multiple selectors can share the same
    declaration block, by putting all the separators in front of the block,
    separated by commas.

    Example 23. A rule set with two selectors
    @begin{pre}
 GtkButton, GtkEntry {
     color: #ff00ea;
     font: Comic Sans 12
 @}
    @end{pre}

  @subheading{Selectors}
    Selectors work very similar to the way they do in CSS, with widget class
    names taking the role of element names, and widget names taking the role of
    IDs. When used in a selector, widget names must be prefixed with a '#'
    character. The '*' character represents the so-called universal selector,
    which matches any widget.

    To express more complicated situations, selectors can be combined in various
    ways:
    @begin{itemize}
      @begin{item}
        To require that a widget satisfies several conditions, combine several
        selectors into one by concatenating them. E. g. @code{GtkButton#button1}
        matches a @code{GtkButton} widget with the name @code{button1}.
      @end{item}
      @begin{item}
        To only match a widget when it occurs inside some other widget, write
        the two selectors after each other, separated by whitespace. E. g.
        @code{GtkToolBar GtkButton} matches @code{GtkButton} widgets that occur
        inside a @code{GtkToolBar}.
      @end{item}
      @begin{item}
        In the previous example, the @code{GtkButton} is matched even if it
        occurs deeply nested inside the toolbar. To restrict the match to direct
        children of the parent widget, insert a '>' character between the two
        selectors. E. g. @code{GtkNotebook > GtkLabel} matches @code{GtkLabel}
        widgets that are direct children of a @code{GtkNotebook}.
      @end{item}
    @end{itemize}

    Example 24. Widget classes and names in selectors
    @begin{pre}
 /* Theme labels that are descendants of a window */
 GtkWindow GtkLabel {
     background-color: #898989
 @}

 /* Theme notebooks, and anything that's within these */
 GtkNotebook {
     background-color: #a939f0
 @}

 /* Theme combo boxes, and entries that
  are direct children of a notebook */
 GtkComboBox,
 GtkNotebook > GtkEntry {
     color: @@fg_color;
     background-color: #1209a2
 @}

 /* Theme any widget within a GtkBin */
 GtkBin * {
     font: Sans 20
 @}

 /* Theme a label named title-label */
 GtkLabel#title-label {
     font: Sans 15
 @}

 /* Theme any widget named main-entry */
 #main-entry {
     background-color: #f0a810
 @}
    @end{pre}
    Widgets may also define style classes, which can be used for matching. When
    used in a selector, style classes must be prefixed with a '.' character.

    Refer to the documentation of individual widgets to learn which style
    classes they define and see the section called \"Style classes and regions\"
    for a list of all style classes used by GTK+ widgets.

    Note that there is some ambiguity in the selector syntax when it comes to
    differentiation widget class names from regions. GTK+ currently treats a
    string as a widget class name if it contains any uppercase characters (which
    should work for more widgets with names like @code{GtkLabel}).

    Example 25. Style classes in selectors
    @begin{pre}
 /* Theme all widgets defining the class entry */
 .entry {
     color: #39f1f9;
 @}

 /* Theme spinbuttons' entry */
 GtkSpinButton.entry {
     color: #900185
 @}
    @end{pre}
    In complicated widgets like e. g. a @code{GtkNotebook}, it may be desirable
    to style different parts of the widget differently. To make this possible,
    container widgets may define regions, whose names may be used for matching
    in selectors.

    Some containers allow to further differentiate between regions by applying
    so-called pseudo-classes to the region. For example, the tab region in
    @code{GtkNotebook} allows to single out the first or last tab by using the
    @code{:first-child} or @code{:last-child} pseudo-class. When used in
    selectors, pseudo-classes must be prefixed with a ':' character.

    Refer to the documentation of individual widgets to learn which regions and
    pseudo-classes they define and see the section called \"Style classes and
    regions\" for a list of all regions used by GTK+ widgets.

    Example 26. Regions in selectors
    @begin{pre}
 /* Theme any label within a notebook */
 GtkNotebook GtkLabel {
     color: #f90192;
 @}

 /* Theme labels within notebook tabs */
 GtkNotebook tab GtkLabel {
     color: #703910;
 @}

 /* Theme labels in the any first notebook
  tab, both selectors are equivalent */
 GtkNotebook tab:nth-child(first) GtkLabel,
 GtkNotebook tab:first-child GtkLabel {
     color: #89d012;
 @}
    @end{pre}
    Another use of pseudo-classes is to match widgets depending on their state.
    This is conceptually similar to the @code{:hover}, @code{:active} or
    @code{:focus} pseudo-classes in CSS. The available pseudo-classes for widget
    states are :active, @code{:prelight} or @code{:hover}, @code{:insensitive},
    @code{:selected}, @code{:focused} and @code{:inconsistent}.

    Example 27. Styling specific widget states
    @begin{pre}
 /* Theme active (pressed) buttons */
 GtkButton:active {
     background-color: #0274d9;
 @}

 /* Theme buttons with the mouse pointer on it,
    both are equivalent */
 GtkButton:hover,
 GtkButton:prelight {
     background-color: #3085a9;
 @}

 /* Theme insensitive widgets, both are equivalent */
 :insensitive,
 *:insensitive {
     background-color: #320a91;
 @}

 /* Theme selection colors in entries */
 GtkEntry:selected {
     background-color: #56f9a0;
 @}

 /* Theme focused labels */
 GtkLabel:focused {
     background-color: #b4940f;
 @}

 /* Theme inconsistent checkbuttons */
 GtkCheckButton:inconsistent {
     background-color: #20395a;
 @}
    @end{pre}
    Widget state pseudoclasses may only apply to the last element in a selector.

    To determine the effective style for a widget, all the matching rule sets
    are merged. As in CSS, rules apply by specificity, so the rules whose
    selectors more closely match a widget path will take precedence over the
    others.

  @subheading{@@ Rules}
    GTK+'s CSS supports the @code{@@import} rule, in order to load another
    CSS style sheet in addition to the currently parsed one.

    Example 28. Using the @code{@@import} rule
    @begin{pre}
      @@import url (\"path/to/common.css\");
    @end{pre}
    In order to extend key bindings affecting different widgets, GTK+ supports
    the @code{@@binding-set} rule to parse a set of bind/unbind directives, see
    @code{GtkBindingSet} for the supported syntax. Note that the binding sets
    defined in this way must be associated with rule sets by setting the
    gtk-key-bindings style property.

    Customized key bindings are typically defined in a separate
    @code{gtk-keys.css} CSS file and GTK+ loads this file according to the
    current key theme, which is defined by the @code{\"gtk-key-theme-name\"}
    setting.

    Example 29. Using the @code{@@binding} rule
    @begin{pre}
 @@binding-set binding-set1 {
   bind \"<alt>Left\" { \"move-cursor\" (visual-positions, -3, 0) @};
   unbind \"End\";
 @};

 @@binding-set binding-set2 {
   bind \"<alt>Right\" { \"move-cursor\" (visual-positions, 3, 0) @};
   bind \"<alt>KP_space\" { \"delete-from-cursor\" (whitespace, 1)
                          \"insert-at-cursor\" (\" \") @};
 @};

 GtkEntry {
   gtk-key-bindings: binding-set1, binding-set2;
 @}
    @end{pre}
    GTK+ also supports an additional @code{@@define-color} rule, in order to
    define a color name which may be used instead of color numeric
    representations. Also see the @code{\"gtk-color-scheme\"} setting for a way
    to override the values of these named colors.

    Example 30. Defining colors
    @begin{pre}
 @@define-color bg_color #f9a039;

 * {
     background-color: @@bg_color;
 @}
    @end{pre}

  @subheading{Symbolic colors}
    Besides being able to define color names, the CSS parser is also able to
    read different color expressions, which can also be nested, providing a rich
    language to define colors which are derived from a set of base colors.

    Example 31. Using symbolic colors
    @begin{pre}
 @@define-color entry-color shade (@@bg_color, 0.7);

 GtkEntry {
     background-color: @@entry-color;
 @}

 GtkEntry:focused {
     background-color: mix (@@entry-color,
                            shade (#fff, 0.5),
                            0.8);
 @}
    @end{pre}
    The various ways to express colors in GTK+ CSS are:
    @begin[code]{table}
      @begin[rgb(r, g, b)]{entry}
        An opaque color; r, g, b can be either integers between 0 and 255 or
        percentages.

        Examples:
        @begin{pre}
 rgb(128, 10, 54)
 rgb(20%, 30%, 0%)
        @end{pre}
      @end{entry}
      @begin[rgba(r, g, b, a)]{entry}
        A translucent color; r, g, b are as in the previous row, a is a
        floating point number between 0 and 1.

        Examples:
        @begin{pre}
 rgba(255, 255, 0, 0.5)
        @end{pre}
      @end{entry}
      @begin[#xxyyzz]{entry}
        An opaque color; xx, yy, zz are hexadecimal numbers specifying r, g, b
        variants with between 1 and 4 hexadecimal digits per component are
        allowed

        Examples:
        @begin{pre}
 #ff12ab
 #f0c
        @end{pre}
      @end{entry}
      @begin[@name]{entry}
        Reference to a color that has been defined with @@define-color.

        Examples:
        @begin{pre}
 @@bg_color
        @end{pre}
      @end{entry}
      @begin[mix(color1, color2, f)]{entry}
        A linear combination of color1 and color2. f is a floating point number
        between 0 and 1.

        Examples:
        @begin{pre}
 mix(#ff1e0a, @@bg_color, 0.8)
        @end{pre}
      @end{entry}
      @begin[shade(color, f)]{entry}
        A lighter or darker variant of color. f is a floating point number.

        Examples:
        @begin{pre}
 shade(@@fg_color, 0.5)
        @end{pre}
      @end{entry}
      @begin[lighter(color)]{entry}
        A lighter variant of color.
      @end{entry}
      @begin[darker(color)]{entry}
        A darker variant of color.
      @end{entry}
      @begin[alpha(color, f)]{entry}
        Modifies passed color's alpha by a factor f. f is a floating point
        number. f < 1.0 results in a more transparent color while f > 1.0
        results in a more opaque color.
        
        Examples:
        @begin{pre}
 alhpa(blue, 0.5)
        @end{pre}
      @end{entry}
    @end{table}

  @subheading{Gradients}
    Linear or radial Gradients can be used as background images.

    A linear gradient along the line from (@code{start_x}, @code{start_y}) to
    (@code{end_x}, @code{end_y}) is specified using the syntax
    @begin{pre}
 -gtk-gradient (linear,
                start_x start_y, end_x end_y,
                color-stop (position, color),
                ...)
    @end{pre}
    where @code{start_x} and @code{end_x} can be either a floating point number
    between 0 and 1 or one of the special values 'left', 'right' or 'center',
    @code{start_y} and @code{end_y} can be either a floating point number
    between 0 and 1 or one of the special values 'top', 'bottom' or 'center',
    position is a floating point number between 0 and 1 and color is a color
    expression (see above). The color-stop can be repeated multiple times to add
    more than one color stop. 'from (color)' and 'to (color)' can be used as
    abbreviations for color stops with position 0 and 1, respectively.

    Example 32. A linear gradient

    @image[gradient1]{}
    This gradient was specified with
    @begin{pre}
 -gtk-gradient (linear,
                left top, right bottom,
                from(@@yellow), to(@@blue))
    @end{pre}

    Example 33. Another linear gradient

    @image[gradient2]{}
    This gradient was specified with
    @begin{pre}
 -gtk-gradient (linear,
                0 0, 0 1,
                color-stop(0, @@yellow),
                color-stop(0.2, @@blue),
                color-stop(1, #0f0))
    @end{pre}
    A radial gradient along the two circles defined by
    @code{(start_x, start_y, start_radius)} and
    @code{(end_x, end_y, end_radius)} is specified using the syntax
    @begin{pre}
 -gtk-gradient (radial,
                start_x start_y, start_radius,
                end_x end_y, end_radius,
                color-stop (position, color),
                ...)
    @end{pre}
    where @code{start_radius} and @code{end_radius} are floating point numbers
    and the other parameters are as before.

    Example 34. A radial gradient

    @image[gradient3]{}
    This gradient was specified with
    @begin{pre}
 -gtk-gradient (radial,
                center center, 0,
                center center, 1,
                from(@@yellow), to(@@green))
    @end{pre}

    Example 35. Another radial gradient

    @image[gradient4]{}
    This gradient was specified with
    @begin{pre}
 -gtk-gradient (radial,
                0.4 0.4, 0.1,
                0.6 0.6, 0.7,
                color-stop (0, #f00),
                color-stop (0.1, #a0f),
                color-stop (0.2, @@yellow),
                color-stop (1, @@green))
    @end{pre}

  @subheading{Text shadow}
    A shadow list can be applied to text or symbolic icons, using the CSS3
    text-shadow syntax, as defined in the CSS3 specification.

    A text shadow is specified using the syntax
    @begin{pre}
 text-shadow: horizontal_offset vertical_offset [ blur_radius ] color
    @end{pre}
    The offset of the shadow is specified with the @code{horizontal_offset} and
    @code{vertical_offset} parameters. The optional blur radius is parsed, but
    it is currently not rendered by the GTK+ theming engine.

    To set multiple shadows on an element, you can specify a comma-separated
    list of shadow elements in the text-shadow property. Shadows are always
    rendered front-back, i. e. the first shadow specified is on top of the
    others. Shadows can thus overlay each other, but they can never overlay the
    text itself, which is always rendered on top of the shadow layer.

  @subheadint{Box shadow}
    Themes can apply shadows on framed elements using the CSS3 box-shadow
    syntax, as defined in the CSS3 specification.

    A box shadow is specified using the syntax
    @begin{pre}
 box-shadow: [ inset ] horizontal_offset vertical_offset [ blur_radius ] [ spread ] color
    @end{pre}
    A positive offset will draw a shadow that is offset to the right (down) of
    the box, a negative offset to the left (top). The optional spread parameter
    defines an additional distance to expand the shadow shape in all directions,
    by the specified radius. The optional blur radius parameter is parsed, but
    it is currently not rendered by the GTK+ theming engine. The inset parameter
    defines whether the drop shadow should be rendered inside or outside the box
    canvas. Only inset box-shadows are currently supported by the GTK+ theming
    engine, non-inset elements are currently ignored.

    To set multiple box-shadows on an element, you can specify a comma-separated
    list of shadow elements in the box-shadow property. Shadows are always
    rendered front-back, i. e. the first shadow specified is on top of the
    others, so they may overlap other boxes or other shadows.

  @subheading{Border images}
    Images and gradients can also be used in slices for the purpose of creating
    scalable borders. For more information, see the CSS3 documentation for the
    border-image property, which can be found here.

    @image[slices]{}

    The parameters of the slicing process are controlled by four separate
    properties. Note that you can use the

    @code{border-image}

    shorthand property to set values for the three properties at the same time.

    @code{border-image-source: url(path)}

    or 

    @code{border-image-source: -gtk-gradient(...)}

    Specifies the source of the border image, and it can either be an URL or
    a gradient (see above).

    @code{border-image-slice: top right bottom left}

    The sizes specified by the top, right, bottom and left parameters are the
    offsets, in pixels, from the relevant edge where the image should be
    \"cut off\" to build the slices used for the rendering of the border.

    @code{border-image-width: top right bottom left}

    The sizes specified by the top, right, bottom and left parameters are inward
    distances from the border box edge, used to specify the rendered size of
    each slice determined by border-image-slice. If this property is not
    specified, the values of border-width will be used as a fallback.

    @code{border-image-repeat: [stretch|repeat|round|space]}

    Specifies how the image slices should be rendered in the area outlined by
    border-width. The default (stretch) is to resize the slice to fill in the
    whole allocated area. If the value of this property is 'repeat', the image
    slice will be tiled to fill the area. If the value of this property is
    'round', the image slice will be tiled to fill the area, and scaled to fit
    it exactly a whole number of times. If the value of this property is
    'space', the image slice will be tiled to fill the area, and if it doesn't
    fit it exactly a whole number of times, the extra space is distributed as
    padding around the slices. If two options are specified, the first one
    affects the horizontal behaviour and the second one the vertical behaviour.
    If only one option is specified, it affects both.

    Example 36. A border image

    @image[border1]{}

    This border image was specified with
    @begin{pre}
 url(\"gradient1.png\") 10 10 10 10
    @end{pre}

    Example 37. A repeating border image

    @image[border2]{}

    This border image was specified with
    @begin{pre}
 url(\"gradient1.png\") 10 10 10 10 repeat
    @end{pre}

    Example 38. A stretched border image

    @image[border3]{}

    This border image was specified with
    @begin{pre}
 url(\"gradient1.png\") 10 10 10 10 stretch
    @end{pre}
    Styles can specify transitions that will be used to create a gradual change
    in the appearance when a widget state changes. The following syntax is used
    to specify transitions:
    @begin{pre}
 duration [s|ms] [linear|ease|ease-in|ease-out|ease-in-out] [loop]
    @end{pre}
    The duration is the amount of time that the animation will take for a
    complete cycle from start to end. If the loop option is given, the animation
    will be repated until the state changes again. The option after the duration
    determines the transition function from a small set of predefined functions.

    Figure 3. Linear transition
    @image[linear]{}

    Figure 4. Ease transition
    @image[ease]{}

    Figure 5. Ease-in-out transition
    @image[ease-in-out]{}

    Figure 6. Ease-in transition
    @image[ease-in]{}

    Figure 7. Ease-out transition
    @image[ease-out]{}

  @subheading{Supported properties}
    Properties are the part that differ the most to common CSS, not all
    properties are supported (some are planned to be supported eventually, some
    others are meaningless or don't map intuitively in a widget based
    environment).

    The currently supported properties are:
    @begin[code]{table}
      @begin[engine]{entry}
        @begin{table}
          @entry[Syntax]{@code{engine-name}}
          @entry[Maps to]{@code{GtkThemingEngine}}
          @begin[Examples]{entry}
            @begin{pre}
 engine: clearlooks;
 engine: none; /* use the default (i. e. builtin) engine) */
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[background-color]{entry}
        @begin{table}
          @entry[Syntax]{@code{color} (see above)}
          @entry[Maps to]{@class{gdk-rgba}}
          @begin[Examples]{entry}
            @begin{pre}
 background-color: #fff;
 color: &color1;
 background-color: shade (&color1, 0.5);
 color: mix (&color1, #f0f, 0.8);
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[color]{entry}
      @end{entry}
      @begin[border-top-color]{entry}
        @begin{table}
          @entry[Syntax]{@code{transparent|color} (see above)}
        @end{table}
      @end{entry}
      @begin[border-right-color]{entry}
      @end{entry}
      @begin[border-bottom-color]{entry}
      @end{entry}
      @begin[border-left-color]{entry}
      @end{entry}
      @begin[border-color]{entry}
        @begin{table}
          @entry[Syntax]{@code{[transparent|color]{1,4@}}}
        @end{table}
      @end{entry}
      @begin[font-family]{entry}
        @begin{table}
          @entry[Syntax]{@code{@@family [, @@family]*}}
          @entry[Maps to]{@code{gchararray}}
          @begin[Examples]{entry}
            @begin{pre}
 font-family: Sans, Arial;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[font-style]{entry}
        @begin{table}
          @entry[Syntax]{@code{[normal|oblique|italic]}}
          @entry[Maps to]{@code{PANGO_TYPE_STYLE}}
          @entry[Examples]{@begin{pre}
 font-style: italic;
            @end{pre}}
        @end{table}
      @end{entry}
      @begin[font-variant]{entry}
        @begin{table}
          @entry[Syntax]{@code{[normal|small-caps]}}
          @entry[Maps to]{@code{PANGO_TYPE_VARIANT}}
          @entry[Examples]{@begin{pre}
 font-variant: normal;
            @end{pre}}
        @end{table}
      @end{entry}
      @begin[font-weight]{entry}
        @begin{table}
          @entry[Syntax]{@code{[normal|bold|bolder|lighter|100|200|300|400|500|600|700|800|900]}}
          @entry[Maps to]{@code{PANGO_TYPE_WEIGHT}}
          @begin[Examples]{entry}
            @begin{pre}
 font-weight: bold;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[font-size]{entry}
        @begin{table}
          @entry[Syntax]{Font size in point}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 font-size: 13;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[font]{entry}
        @begin{table}
          @entry[Syntax]{@code{@@family [@@style] [@@size]}}
          @entry[Maps to]{@code{PangoFontDescription}}
          @begin[Examples]{entry}
            @begin{pre}
 font: Sans 15;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[margin-top]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 margin-top: 0;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[margin-left]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 margin-left: 1;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[margin-bottom]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 margin-bottom: 2;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[margin-right]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 margin-right: 4;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[margin]{entry}
        @begin{table}
          @entry[Syntax]{@code{@@width @@vertical_width @@horizontal_width
                         @@top_width @@horizontal_width @@bottom_width
                         @@top_width @@right_width @@bottom_width @@left_width}}
          @entry[Maps to]{@class{gtk-border}}
          @begin[Examples]{entry}
            @begin{pre}
 margin: 5;
 margin: 5 10;
 margin: 5 10 3;
 margin: 5 10 3 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[padding-top]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 padding-top: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[padding-left]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 padding-left: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[padding-bottom]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 padding-bottom: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[padding-right]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 padding-right: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[padding]{entry}
      @end{entry}
      @begin[background-image]{entry}
        @begin{table}
          @entry[Syntax]{gradient (see above) or @code{url(@@path)}}
          @entry[Maps to]{@code{cairo_pattern_t}}
          @begin[Examples]{entry}
            @begin{pre}
 -gtk-gradient (linear,
               left top, right top,
               from (&num;fff), to (&num;000));
 -gtk-gradient (linear, 0.0 0.5, 0.5 1.0,
               from (&num;fff),
               color-stop (0.5, &num;f00),
               to (&num;000));
 -gtk-gradient (radial,
               center center, 0.2,
               center center, 0.8,
               color-stop (0.0, &num;fff),
               color-stop (1.0, &num;000));
 url ('background.png');
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[background-repeat]{entry}
        @begin{table}
          @entry[Syntax]{@code{[repeat|no-repeat]}}
          @entry[Maps to]{internal}
          @begin[Examples]{entry}
            @begin{pre}
 background-repeat: no-repeat;
            @end{pre}
            If not specified, the style does not respect the CSS3 specification,
            since the background will be stretched to fill the area.
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-top-width]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 border-top-width: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-left-width]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 border-left-width: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-bottom-width]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 border-bottom-width: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-right-width]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{@code{:int}}
          @begin[Examples]{entry}
            @begin{pre}
 border-right-width: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-width]{entry}
        @begin{table}
          @entry[Syntax]{}
          @entry[Maps to]{@code{GtkBorder}}
          @begin[Examples]{entry}
            @begin{pre}
 border-width: 1;
 border-width: 1 2;
 border-width: 1 2 3;
 border-width: 1 2 3 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-radius]{entry}
        @begin{table}
          @entry[Syntax]{integer}
          @entry[Maps to]{:int}
          @begin[Examples]{entry}
            @begin{pre}
 border-radius: 5;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-style]{entry}
        @begin{table}
          @entry[Syntax]{@code{[none|solid|inset|outset]}}
          @entry[Maps to]{@code{GtkBorderStyle}}
          @begin[Examples]{entry}
            @begin{pre}
 border-style: solid;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[border-image]{entry}
        @begin{table}
          @entry[Syntax]{border image (see above)}
          @entry[Maps to]{internal use only}
          @begin[Examples]{entry}
            @begin{pre}
 border-image: url(\"/path/to/image.png\") 3 4 3 4 stretch;
 border-image: url(\"/path/to/image.png\") 3 4 4 3 repeat stretch;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[text-shadow]{entry}
        @begin{table}
          @entry[Syntax]{shadow list (see above)}
          @entry[Maps to]{internal use only}
          @begin[Examples]{entry}
            @begin{pre}
 text-shadow: 1 1 0 blue, -4 -4 red;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[transistion]{entry}
        @begin{table}
          @entry[Syntax]{transistion (see above)}
          @entry[Maps to]{internal use only}
          @begin[Examples]{entry}
            @begin{pre}
 transition: 150ms ease-in-out;
 transition: 1s linear loop;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
      @begin[gtk-key-bindings]{entry}
        @begin{table}
          @entry[Syntax]{binding set name list}
          @entry[Maps to]{internal use only}
          @begin[Examples]{entry}
            @begin{pre}
 gtk-bindings: binding1, binding2, ...;
            @end{pre}
          @end{entry}
        @end{table}
      @end{entry}
    @end{table}
    @code{GtkThemingEngines} can register their own, engine-specific style
    properties with the function @fun{gtk-theming-engine-register-property}.
    These properties can be set in CSS like other properties, using a name of
    the form

    @code{-namespace-name},

    where namespace is typically the name of the theming engine, and name is
    the name of the property. Style properties that have been registered by
    widgets using the function @fun{gtk-widget-class-install-style-property} can
    also be set in this way, using the widget class name for namespace.

    Example 39. Using engine-specific style properties
    @begin{pre}
 * {
     engine: clearlooks;
     border-radius: 4;
     -GtkPaned-handle-size: 6;
     -clearlooks-colorize-scrollbar: false;
 @}
    @end{pre}
  @see-function{gtk-theming-engine-register-property}
  @see-function{gtk-widget-class-install-style-property}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_default ()
;;;
;;; GtkCssProvider * gtk_css_provider_get_default (void);
;;;
;;; Returns the provider containing the style settings used as a fallback for
;;; all widgets.
;;;
;;; Returns :
;;;     The provider used for fallback styling. This memory is owned by GTK+,
;;;     and you must not free it.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_default" gtk-css-provider-get-default)
    (g-object gtk-css-provider))

(export 'gtk-css-provider-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_named ()
;;;
;;; GtkCssProvider * gtk_css_provider_get_named (const gchar *name,
;;;                                              const gchar *variant);
;;;
;;; Loads a theme from the usual theme paths
;;;
;;; name :
;;;     A theme name
;;;
;;; variant :
;;;     variant to load, for example, "dark", or NULL for the default.
;;;
;;; Returns :
;;;     a GtkCssProvider with the theme loaded. This memory is owned by GTK+,
;;;     and you must not free it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_data ()
;;;
;;; gboolean gtk_css_provider_load_from_data (GtkCssProvider *css_provider,
;;;                                           const gchar *data,
;;;                                           gssize length,
;;;                                           GError **error);
;;;
;;; Loads data into css_provider, making it clear any previously loaded
;;; information.
;;;
;;; css_provider :
;;;     a GtkCssProvider
;;;
;;; data :
;;;     CSS data loaded in memory
;;;
;;; length :
;;;     the length of data in bytes, or -1 for NUL terminated strings. If length
;;;     is not -1, the code will assume it is not NUL terminated and will
;;;     potentially do a copy.
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE. The return value is deprecated and FALSE will only be returned for
;;;     backwards compatibility reasons if an error is not NULL and a loading
;;;     error occured. To track errors while loading CSS, connect to the
;;;     GtkCssProvider::parsing-error signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_file ()
;;;
;;; gboolean gtk_css_provider_load_from_file (GtkCssProvider *css_provider,
;;;                                           GFile *file,
;;;                                           GError **error);
;;;
;;; Loads the data contained in file into css_provider, making it clear any
;;; previously loaded information.
;;;
;;; css_provider :
;;;     a GtkCssProvider
;;;
;;; file :
;;;     GFile pointing to a file to load
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE. The return value is deprecated and FALSE will only be returned for
;;;     backwards compatibility reasons if an error is not NULL and a loading
;;;     error occured. To track errors while loading CSS, connect to the
;;;     GtkCssProvider::parsing-error signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_path ()
;;;
;;; gboolean gtk_css_provider_load_from_path (GtkCssProvider *css_provider,
;;;                                           const gchar *path,
;;;                                           GError **error);
;;;
;;; Loads the data contained in path into css_provider, making it clear any
;;; previously loaded information.
;;;
;;; css_provider :
;;;     a GtkCssProvider
;;;
;;; path :
;;;     the path of a filename to load, in the GLib filename encoding
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE. The return value is deprecated and FALSE will only be returned for
;;;     backwards compatibility reasons if an error is not NULL and a loading
;;;     error occured. To track errors while loading CSS, connect to the
;;;     GtkCssProvider::parsing-error signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new ()
;;;
;;; GtkCssProvider * gtk_css_provider_new (void);
;;;
;;; Returns a newly created GtkCssProvider.
;;;
;;; Returns :
;;;     A new GtkCssProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string ()
;;;
;;; char * gtk_css_provider_to_string (GtkCssProvider *provider);
;;;
;;; Convertes the provider into a string representation in CSS format.
;;;
;;; Using gtk_css_provider_load_from_data() with the return value from this
;;; function on a new provider created with gtk_css_provider_new() will
;;; basically create a duplicate of this provider.
;;;
;;; provider :
;;;     the provider to write to a string
;;;
;;; Returns :
;;;     a new string representing the provider.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_to_string" gtk-css-provider-to-string) :string
  (provider (g-object gtk-css-provider)))

(export 'gtk-css-provider-to-string)

;;; ----------------------------------------------------------------------------
;;; GTK_CSS_PROVIDER_ERROR
;;;
;;; #define GTK_CSS_PROVIDER_ERROR (gtk_css_provider_error_quark ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssProviderError
;;;
;;; typedef enum {
;;;   GTK_CSS_PROVIDER_ERROR_FAILED,
;;;   GTK_CSS_PROVIDER_ERROR_SYNTAX,
;;;   GTK_CSS_PROVIDER_ERROR_IMPORT,
;;;   GTK_CSS_PROVIDER_ERROR_NAME,
;;;   GTK_CSS_PROVIDER_ERROR_DEPRECATED,
;;;   GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;; } GtkCssProviderError;
;;;
;;; GTK_CSS_PROVIDER_ERROR_FAILED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_SYNTAX
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_IMPORT
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_NAME
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_DEPRECATED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;;
;;; GtkCssSection
;;;
;;; typedef struct _GtkCssSection GtkCssSection;
;;;
;;; Defines a part of a CSS document. Because sections are nested into one
;;; another, you can use gtk_css_section_get_parent() to get the containing
;;; region.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssSectionType
;;;
;;; typedef enum {
;;;   GTK_CSS_SECTION_DOCUMENT,
;;;   GTK_CSS_SECTION_IMPORT,
;;;   GTK_CSS_SECTION_COLOR_DEFINITION,
;;;   GTK_CSS_SECTION_BINDING_SET,
;;;   GTK_CSS_SECTION_RULESET,
;;;   GTK_CSS_SECTION_SELECTOR,
;;;   GTK_CSS_SECTION_DECLARATION,
;;;   GTK_CSS_SECTION_VALUE,
;;;   GTK_CSS_SECTION_KEYFRAMES
;;; } GtkCssSectionType;
;;;
;;; The different types of sections indicate parts of a CSS document as parsed
;;; by GTK's CSS parser. They are oriented towards the CSS grammar CSS grammer,
;;; but may contain extensions.
;;;
;;; More types might be added in the future as the parser incorporates more
;;; features.
;;;
;;; GTK_CSS_SECTION_DOCUMENT
;;;     The section describes a complete document. This section time is the only
;;;     one where gtk_css_section_get_parent() might return NULL.
;;;
;;; GTK_CSS_SECTION_IMPORT
;;;     The section defines an import rule.
;;;
;;; GTK_CSS_SECTION_COLOR_DEFINITION
;;;     The section defines a color. This is a GTK extension to CSS.
;;;
;;; GTK_CSS_SECTION_BINDING_SET
;;;     The section defines a binding set. This is a GTK extension to CSS.
;;;
;;; GTK_CSS_SECTION_RULESET
;;;     The section defines a CSS ruleset.
;;;
;;; GTK_CSS_SECTION_SELECTOR
;;;     The section defines a CSS selector.
;;;
;;; GTK_CSS_SECTION_DECLARATION
;;;     The section defines the declaration of a CSS variable.
;;;
;;; GTK_CSS_SECTION_VALUE
;;;     The section defines the value of a CSS declaration.
;;;
;;; GTK_CSS_SECTION_KEYFRAMES
;;;     The section defines keyframes. See CSS animations for details. Since 3.6
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_line ()
;;;
;;; guint gtk_css_section_get_end_line (const GtkCssSection *section);
;;;
;;; Returns the line in the CSS document where this section end. The line number
;;; is 0-indexed, so the first line of the document will return 0. This value
;;; may change in future invocations of this function if section is not yet
;;; parsed completely. This will for example happen in the
;;; GtkCssProvider::parsing-error signal. The end position and line may be
;;; identical to the start position and line for sections which failed to parse
;;; anything successfully.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     the line number
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_position ()
;;;
;;; guint gtk_css_section_get_end_position (const GtkCssSection *section);
;;;
;;; Returns the offset in bytes from the start of the current line returned via
;;; gtk_css_section_get_end_line(). This value may change in future invocations
;;; of this function if section is not yet parsed completely. This will for
;;; example happen in the GtkCssProvider::parsing-error signal. The end position
;;; and line may be identical to the start position and line for sections which
;;; failed to parse anything successfully.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     The offset in bytes from the start of the line.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file ()
;;;
;;; GFile * gtk_css_section_get_file (const GtkCssSection *section);
;;;
;;; Gets the file that section was parsed from. If no such file exists, for
;;; example because the CSS was loaded via gtk_css_provider_load_from_data(),
;;; then NULL is returned.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     The GFile that section was parsed from or NULL if section was parsed
;;;     from other data.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent ()
;;;
;;; GtkCssSection * gtk_css_section_get_parent (const GtkCssSection *section);
;;;
;;; Gets the parent section for the given section. The parent section is the
;;; section that contains this section. A special case are sections of type
;;; GTK_CSS_SECTION_DOCUMENT. Their parent will either be NULL if they are the
;;; original CSS document that was loaded by gtk_css_provider_load_from_file()
;;; or a section of type GTK_CSS_SECTION_IMPORT if it was loaded with an import
;;; rule from a different file.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     the parent section or NULL if none
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_section_type ()
;;;
;;; GtkCssSectionType gtk_css_section_get_section_type
;;;                                              (const GtkCssSection *section);
;;;
;;; Gets the type of information that section describes.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     the type of section
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_line ()
;;;
;;; guint gtk_css_section_get_start_line (const GtkCssSection *section);
;;;
;;; Returns the line in the CSS document where this section starts. The line
;;; number is 0-indexed, so the first line of the document will return 0.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     the line number
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_position ()
;;;
;;; guint gtk_css_section_get_start_position (const GtkCssSection *section);
;;;
;;; Returns the offset in bytes from the start of the current line returned via
;;; gtk_css_section_get_start_line().
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     the offset in bytes from the start of the line.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_ref ()
;;;
;;; GtkCssSection * gtk_css_section_ref (GtkCssSection *section);
;;;
;;; Increments the reference count on section.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Returns :
;;;     section itself.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_unref ()
;;;
;;; void gtk_css_section_unref (GtkCssSection *section);
;;;
;;; Decrements the reference count on section, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; The "parsing-error" signal
;;;
;;; void user_function (GtkCssProvider *provider,
;;;                     GtkCssSection  *section,
;;;                     GError         *error,
;;;                     gpointer        user_data)      : Run Last
;;;
;;; Signals that a parsing error occured. the path, line and position describe
;;; the actual location of the error as accurately as possible.
;;;
;;; Parsing errors are never fatal, so the parsing will resume after the error.
;;; Errors may however cause parts of the given data or even all of it to not be
;;; parsed at all. So it is a useful idea to check that the parsing succeeds by
;;; connecting to this signal.
;;;
;;; Note that this signal may be emitted at any time as the css provider may opt
;;; to defer parsing parts or all of the input to a later time than when a
;;; loading function was called.
;;;
;;; provider :
;;;     the provider that had a parsing error
;;;
;;; section :
;;;     section the error happened in
;;;
;;; error :
;;;     The parsing error
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.css-provider.lisp --------------------------------------

;;; ----------------------------------------------------------------------------
;;; glib.option-group.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.68 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; Commandline option parser
;;;
;;;     Parses commandline options
;;;
;;; Types and Values
;;;
;;;     GOptionError
;;;     G_OPTION_ERROR
;;;     GOptionContext
;;;     GOptionArg
;;;     GOptionFlags
;;;     G_OPTION_REMAINING
;;;     GOptionEntry
;;;     GOptionGroup
;;;
;;; Functions
;;;
;;;     GOptionArgFunc
;;;
;;;     g_option_context_new
;;;     g_option_context_set_summary
;;;     g_option_context_get_summary
;;;     g_option_context_set_description
;;;     g_option_context_get_description
;;;
;;;     GTranslateFunc
;;;     g_option_context_set_translate_func
;;;     g_option_context_set_translation_domain
;;;     g_option_context_free
;;;     g_option_context_parse
;;;     g_option_context_parse_strv
;;;     g_option_context_set_help_enabled
;;;     g_option_context_get_help_enabled
;;;     g_option_context_set_ignore_unknown_options
;;;     g_option_context_get_ignore_unknown_options
;;;     g_option_context_get_help
;;;     g_option_context_get_strict_posix
;;;     g_option_context_set_strict_posix
;;;     g_option_context_add_main_entries
;;;     g_option_context_add_group
;;;     g_option_context_set_main_group
;;;     g_option_context_get_main_group
;;;
;;;     g_option_group_new
;;;     g_option_group_ref
;;;     g_option_group_unref
;;;     g_option_group_free
;;;     g_option_group_add_entries
;;;
;;;     GOptionParseFunc
;;;     g_option_group_set_parse_hooks
;;;     GOptionErrorFunc
;;;     g_option_group_set_error_hook
;;;     g_option_group_set_translate_func
;;;     g_option_group_set_translation_domain
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; enum GOptionError
;;;
;;; typedef enum {
;;;   G_OPTION_ERROR_UNKNOWN_OPTION,
;;;   G_OPTION_ERROR_BAD_VALUE,
;;;   G_OPTION_ERROR_FAILED
;;; } GOptionError;
;;;
;;; Error codes returned by option parsing.
;;;
;;; G_OPTION_ERROR_UNKNOWN_OPTION
;;;     An option was not known to the parser. This error will only be reported,
;;;     if the parser hasn't been instructed to ignore unknown options, see
;;;     g_option_context_set_ignore_unknown_options().
;;;
;;; G_OPTION_ERROR_BAD_VALUE
;;;     A value couldn't be parsed.
;;;
;;; G_OPTION_ERROR_FAILED
;;;     A GOptionArgFunc callback failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_OPTION_ERROR
;;;
;;; #define G_OPTION_ERROR (g_option_error_quark ())
;;;
;;; Error domain for option parsing. Errors in this domain will be from the
;;; GOptionError enumeration. See GError for information on error domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GOptionArg
;;; ----------------------------------------------------------------------------

(defcenum g-option-arg
  :none
  :string
  :int
  :callback
  :filename
  :string-array
  :filename-array
  :double
  :int64)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-arg atdoc:*symbol-name-alias*)
      "CEnum"
      (gethash 'g-option-arg atdoc:*external-symbols*)
 "@version{2021-4-13}
  @begin{short}
    The @sym{g-option-arg} enumeration determines which type of extra argument
    the options expect to find. If an option expects an extra argument, it can
    be specified in several ways; with a short option: -x arg, with a long
    option: --name arg or combined in a single argument: --name=arg.
  @end{short}
  @begin{pre}
(defcenum g-option-arg
  :none
  :string
  :int
  :callback
  :filename
  :string-array
  :filename-array
  :double
  :int64)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No extra argument. This is useful for simple flags.}
    @entry[:string]{The option takes a string argument.}
    @entry[:int]{The option takes an integer argument.}
    @entry[:callback]{The option provides a callback to parse the extra
      argument.}
    @entry[:filename]{The option takes a filename as argument.}
    @entry[:string-array]{The option takes a string argument, multiple uses of
      the option are collected into a list of strings.}
    @entry[:filename-array]{The option takes a filename as argument, multiple
      uses of the option are collected into a list of strings.}
    @entry[:double]{The option takes a double float argument. The argument can
      be formatted either for the user's locale or for the \"C\" locale.}
    @entry[:int64]{The option takes a 64-bit integer. Like @code{:int} but for
      larger numbers. The number can be in decimal base, or in hexadecimal
      (when prefixed with 0x, for example, 0xffffffff).}
  @end{table}
  @see-type{g-option-context}")

(export 'g-option-arg)

;;; ----------------------------------------------------------------------------
;;; enum GOptionFlags
;;;-----------------------------------------------------------------------------

(defbitfield g-option-flags
  :hidden
  :in-main
  :reverse
  :no-arg
  :filename
  :optional-arg
  :noalias)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-flags atdoc:*symbol-name-alias*)
      "Bitfield"
      (gethash 'g-option-flags atdoc:*external-symbols*)
 "@version{2021-4-13}
  @short{Flags which modify individual options.}
  @begin{pre}
(defbitfield g-option-flags
  :hidden
  :in-main
  :reverse
  :no-arg
  :filename
  :optional-arg
  :noalias)
  @end{pre}
  @begin[code]{table}
    @entry[:hidden]{The option does not appear in --help output.}
    @entry[:in-main]{The option appears in the main section of the --help
      output, even if it is defined in a group.}
    @entry[:reverse]{For options of the @code{:none} kind, this flag indicates
      that the sense of the option is reversed.}
    @entry[:no-arg]{For options of the @code{:callback} kind, this flag
      indicates that the callback does not take any argument (like a
      @code{:none} option).}
    @entry[:filename]{For options of the @code{:arg-callback}, this flag
      indicates that the argument should be passed to the callback in the GLib
      filename encoding rather than UTF-8.}
    @entry[:optional-arg]{For options of the @code{:arg-callback} kind, this
      flag indicates that the argument supply is optional. If no argument is
      given then data of the @code{GOptionParseFunc} callback function will be
      set to @code{NULL}.}
    @entry[:noalias]{This flag turns off the automatic conflict resolution which
      prefixes long option names with groupname - if there is a conflict. This
      option should only be used in situations where aliasing is necessary to
      model some legacy commandline interface. It is not safe to use this
      option, unless all option groups are under your direct control.}
  @end{table}
  @see-type{g-option-context}
  @see-symbol{g-option-arg}")

(export 'g-option-flags)

;;; ----------------------------------------------------------------------------
;;; G_OPTION_REMAINING
;;;
;;; #define G_OPTION_REMAINING ""
;;;
;;; If a long option in the main group has this name, it is not treated as a
;;; regular option. Instead it collects all non-option arguments which would
;;; otherwise be left in argv. The option must be of type G_OPTION_ARG_CALLBACK,
;;; G_OPTION_ARG_STRING_ARRAY or G_OPTION_ARG_FILENAME_ARRAY.
;;;
;;; Using G_OPTION_REMAINING instead of simply scanning argv for leftover
;;; arguments has the advantage that GOption takes care of necessary encoding
;;; conversions for strings or filenames.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GOptionContext
;;; ----------------------------------------------------------------------------

(defcstruct g-option-context)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-context atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-option-context 'type)
 "@version{2021-4-13}
  @begin{short}
    The GOption commandline parser is intended to be a simpler replacement for
    the Popt library.
  @end{short}
  It supports short and long commandline options, as shown in the following
  example:
  @begin{pre}
testtreemodel -r 1 --max-size 20 --rand --display=:1.0 -vb -- file1 file2
  @end{pre}
  The example demonstrates a number of features of the GOption commandline
  parser:
  @begin{itemize}
    @item{Options can be single letters, prefixed by a single dash.}
    @item{Multiple short options can be grouped behind a single dash.}
    @item{Long options are prefixed by two consecutive dashes.}
    @item{Options can have an extra argument, which can be a number, a string
      or a filename. For long options, the extra argument can be appended with
      an equals sign after the option name, which is useful if the extra
      argument starts with a dash, which would otherwise cause it to be
      interpreted as another option.}
    @item{Non-option arguments are returned to the application as rest
      arguments.}
    @item{An argument consisting solely of two dashes turns off further parsing,
      any remaining arguments (even those starting with a dash) are returned to
      the application as rest arguments.}
  @end{itemize}
  Another important feature of GOption is that it can automatically generate
  nicely formatted help output. Unless it is explicitly turned off with the
  function @fun{g-option-context-set-help-enabled}, GOption will recognize the
  --help, -?, --help-all and --help-groupname options (where groupname is the
  name of a @symbol{g-option-group} instance) and write a text similar to the
  one shown in the following example to stdout.
  @begin{pre}
Usage:
  testtreemodel [OPTION...] - test tree model performance

Help Options:
  -h, --help               Show help options
  --help-all               Show all help options
  --help-gtk               Show GTK+ Options

Application Options:
  -r, --repeats=N          Average over N repetitions
  -m, --max-size=M         Test up to 2^M items
  --display=DISPLAY        X display to use
  -v, --verbose            Be verbose
  -b, --beep               Beep when done
  --rand                   Randomize the data
  @end{pre}
  GOption groups options in a @symbol{g-option-group} instance, which makes it
  easy to incorporate options from multiple sources. The intended use for this
  is to let applications collect option groups from the libraries it uses, add
  them to their @sym{g-option-context} instance, and parse all options by a
  single call to the function @fun{g-option-context-parse}. See the function
  @fun{gtk-option-group} for an example.

  If an option is declared to be of type string or filename, GOption takes care
  of converting it to the right encoding; strings are returned in UTF-8,
  filenames are returned in the GLib filename encoding. Note that this only
  works if @code{setlocale()} has been called before the function
  @fun{g-option-context-parse}.

  Here is a complete example of setting up GOption to parse the example
  commandline above and produce the example help output.
  @begin{pre}
static gint repeats = 2;
static gint max_size = 8;
static gboolean verbose = FALSE;
static gboolean beep = FALSE;
static gboolean randomize = FALSE;

static GOptionEntry entries[] =
{
  { \"repeats\",
    'r', 0, G_OPTION_ARG_INT, &repeats, \"Average over N repetitions\", \"N\" @},
  { \"max-size\",
    'm', 0, G_OPTION_ARG_INT, &max_size, \"Test up to 2^M items\", \"M\" @},
  { \"verbose\", 'v', 0, G_OPTION_ARG_NONE, &verbose, \"Be verbose\", NULL @},
  { \"beep\", 'b', 0, G_OPTION_ARG_NONE, &beep, \"Beep when done\", NULL @},
  { \"rand\", 0, 0, G_OPTION_ARG_NONE, &randomize, \"Randomize the data\", NULL @},
  { NULL @}
@};

int
main (int argc, char *argv[])
{
  GError *error = NULL;
  GOptionContext *context;

  context = g_option_context_new (\"- test tree model performance\");
  g_option_context_add_main_entries (context, entries, GETTEXT_PACKAGE);
  g_option_context_add_group (context, gtk_get_option_group (TRUE));
  if (!g_option_context_parse (context, &argc, &argv, &error))
    {
      g_print (\"option parsing failed: %s\n\", error->message);
      exit (1);
    @}
  ...
@}
  @end{pre}
  On UNIX systems, the @code{argv} that is passed to @code{main()} has no
  particular encoding, even to the extent that different parts of it may have
  different encodings. In general, normal arguments and flags will be in the
  current locale and filenames should be considered to be opaque byte strings.
  Proper use of @code{:filename} vs @code{:string} is therefore important.

  Note that on Windows, filenames do have an encoding, but using a
  @sym{g-option-context} instance with the @code{argv} as passed to
  @code{main()} will result in a program that can only accept commandline
  arguments with characters from the system codepage. This can cause problems
  when attempting to deal with filenames containing Unicode characters that
  fall outside of the codepage.

  A solution to this is to use the functions @code{g_win32_get_command_line()}
  and @code{g_option_context_parse_strv()} which will properly handle full
  Unicode filenames. If you are using a @class{g-application} object, this is
  done automatically for you.

  The following example shows how you can use a @sym{g-option-context} instance
  directly in order to correctly deal with Unicode filenames on Windows:
  @begin{pre}
int
main (int argc, char **argv)
{
  GError *error = NULL;
  GOptionContext *context;
  gchar **args;

#ifdef G_OS_WIN32
  args = g_win32_get_command_line ();
#else
  args = g_strdupv (argv);
#endif

  // set up context
  if (!g_option_context_parse_strv (context, &args, &error))
    {
      // error happened
    @}
  ...
  g_strfreev (args);
  ...
@}
  @end{pre}
  @see-function{g-option-context-add-main-entries}
  @see-function{g-option-context-parse}")

(export 'g-option-context)

;;; ----------------------------------------------------------------------------
;;; struct GOptionEntry
;;; ----------------------------------------------------------------------------

(defcstruct g-option-entry
  (long-name :string)
  (short-name :char)
  (flags g-option-flags)
  (arg g-option-arg)
  (arg-data :pointer)
  (description :string)
  (arg-description :string))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-entry atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-option-entry 'type)
 "@version{2021-4-13}
  @begin{short}
    A @sym{g-option-entry} structure defines a single option.
  @end{short}
  To have an effect, they must be added to a @type{g-option-group} instance
  with the function @fun{g-option-context-add-main-entries} or the function
  @fun{g-option-group-add-entries}.
  @begin{pre}
(defcstruct g-option-entry
  (long-name :string)
  (short-name :char)
  (flags g-option-flags)
  (arg g-option-arg)
  (arg-data :pointer)
  (description :string)
  (arg-description :string))
  @end{pre}
  @begin[code]{table}
    @entry[long-name]{The long name of an option can be used to specify it in a
      commandline as --long_name. Every option must have a long name. To resolve
      conflicts if multiple option groups contain the same long name, it is also
      possible to specify the option as --groupname-long_name.}
    @entry[short-name]{If an option has a short name, it can be specified
      -short_name in a commandline. @arg{short-name} must be a printable ASCII
      character different from '-', or zero if the option has no short name.}
    @entry[flags]{Flags from the @symbol{g-option-flags} bitfield.}
    @entry[arg]{The type of the option, as a @symbol{g-option-arg} value.}
    @entry[arg-data]{If the @arg{arg} type is @code{:callback}, then
      @arg{arg-data} must point to a @code{GOptionArgFunc} callback function,
      which will be called to handle the extra argument. Otherwise,
      @arg{arg-data} is a pointer to a location to store the value, the required
      type of the location depends on the @arg{arg} type.
      If @arg{arg} type is @code{:string} or @code{:filename} the location
      will contain a newly allocated string if the option was given.}
    @entry[description]{The description for the option in --help output. The
      description is translated using the @code{translate-func} of the group,
      see the function @fun{g-option-group-set-translation-domain}.}
    @entry[arg-description]{The placeholder to use for the extra argument parsed
      by the option in --help output. The @arg{arg-description} is translated
      using the @code{translate-func} of the group, see the function
      @fun{g-option-group-set-translation-domain}.}
  @end{table}
  @see-function{g-option-context-add-main-entries}
  @see-function{g-option-group-add-entries}
  @see-function{g-option-group-set-translation-domain}")

(export 'g-option-entry)

;;; ----------------------------------------------------------------------------

(defun (setf g-option-entry-long-name) (long-name entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'long-name)
        long-name))

(defun g-option-entry-long-name (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'long-name))

(defun (setf g-option-entry-short-name) (short-name entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'short-name)
        short-name))

(defun g-option-entry-short-name (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'short-name))

(defun (setf g-option-entry-flags) (flags entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'flags)
        flags))

(defun g-option-entry-flags (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'flags))

(defun (setf g-option-entry-arg) (arg entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'arg)
        arg))

(defun g-option-entry-arg (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'arg))

(defun (setf g-option-entry-arg-data) (arg-data entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'arg-data)
        arg-data))

(defun g-option-entry-arg-data (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'arg-data))

(defun (setf g-option-entry-description) (description entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'description)
        description))

(defun g-option-entry-description (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'description))

(defun (setf g-option-entry-arg-description) (arg-description entry)
  (setf (foreign-slot-value entry '(:struct g-option-entry) 'arg-description)
        arg-description))

(defun g-option-entry-arg-description (entry)
  (foreign-slot-value entry '(:struct g-option-entry) 'arg-description))

;;; ----------------------------------------------------------------------------
;;; GOptionGroup
;;; ----------------------------------------------------------------------------

(defcstruct g-option-group)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-group atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-option-group 'type)
 "@version{2021-4-13}
  @begin{short}
    A @sym{g-option-group} structure defines the options in a single group.
  @end{short}
  The structure has only private fields and should not be directly accessed.

  All options in a group share the same translation function. Libraries which
  need to parse commandline options are expected to provide a function for
  getting a @sym{g-option-group} instance holding their options, which the
  application can then add to its @type{g-option-context} instance.
  @see-type{g-option-context}")

(export 'g-option-group)

;;; ----------------------------------------------------------------------------
;;; GOptionArgFunc ()
;;;
;;; gboolean (*GOptionArgFunc) (const gchar *option_name,
;;;                             const gchar *value,
;;;                             gpointer data,
;;;                             GError **error);
;;;
;;; The type of function to be passed as callback for G_OPTION_ARG_CALLBACK
;;; options.
;;;
;;; option_name :
;;;     The name of the option being parsed. This will be either a single dash
;;;     followed by a single letter (for a short name) or two dashes followed by
;;;     a long option name.
;;;
;;; value :
;;;     The value to be parsed.
;;;
;;; data :
;;;     User data added to the GOptionGroup containing the option when it was
;;;     created with g_option_group_new()
;;;
;;; error :
;;;     A return location for errors. The error code G_OPTION_ERROR_FAILED is
;;;     intended to be used for errors in GOptionArgFunc callbacks.
;;;
;;; Returns :
;;;     TRUE if the option was successfully parsed, FALSE if an error occurred,
;;;     in which case error should be set with g_set_error()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_new" %g-option-context-new)
    (:pointer (:struct g-option-context))
  (parameter-string :string))

(defun g-option-context-new (&optional (parameter-string (null-pointer)))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[parameter-string]{a string which is displayed in the first line of
    --help output, after the usage summary programname [OPTION...]}
  @begin{return}
    A newly created @type{g-option-context} instance, which must be freed with
    the function @fun{g-option-context-free} after use.
  @end{return}
  @short{Creates a new option context.}

  The argument @arg{parameter-string} can serve multiple purposes. It can be
  used to add descriptions for \"rest\" arguments, which are not parsed by the
  @type{g-option-context} instance, typically something like \"FILES\" or
  \"FILE1 FILE2...\". If you are using @code{G_OPTION_REMAINING} for collecting
  \"rest\" arguments, GLib handles this automatically by using the
  @arg{arg-description} of the corresponding @type{g-option-entry} instance in
  the usage summary.

  Another usage is to give a short summary of the program functionality, like
  \" - frob the strings\", which will be displayed in the same line as the
  usage. For a longer description of the program functionality that should be
  displayed as a paragraph below the usage line, use the function
  @fun{g-option-context-set-summary}.

  Note that the argument @arg{parameter-string} is translated using the function
  set with the function @fun{g-option-context-set-translate-func}, so it should
  normally be passed untranslated.
  @begin[Example]{dictionary}
    @begin{pre}
(g-option-context-new \"This is an example for a description.\")
=> #.(SB-SYS:INT-SAP #X0817EB48)
(g-option-context-get-help * nil)
=>
\"Aufruf:
  sbcl [OPTION …] This is an example for a description.

Hilfeoptionen:
  -h, --help       Hilfeoptionen anzeigen
\"
    @end{pre}
  @end{dictionary}
  @see-type{g-option-context}
  @see-function{g-option-context-free}
  @see-function{g-option-context-set-summary}
  @see-function{g-option-context-set-translate-func}"
  (%g-option-context-new parameter-string))

(export 'g-option-context-new)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_summary ()
;;; g_option_context_set_summary () -> g-option-context-summary
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-summary) (summary context)
  (foreign-funcall "g_option_context_set_summary"
                   (:pointer (:struct g-option-context)) context
                   :string (if summary summary (null-pointer))
                   :void)
  summary)

(defcfun ("g_option_context_get_summary" g-option-context-summary) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @syntax[]{(g-option-context-summary context) => summary}
  @syntax[]{(setf (g-option-context-summary context) summary)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[summary]{a string to be shown in --help output before the list of
    options, or @code{nil}}
  @begin{short}
    The function @sym{g-option-context-summary} returns the summary.
  @end{short}
  The function @sym{(setf g-option-context-summary)} adds a string to be
  displayed in --help output before the list of options. This is typically a
  summary of the program functionality.

  Note that the summary is translated. See the functions
  @fun{g-option-context-set-translate-func} and
  @fun{g-option-context-set-translation-domain}.
  @see-type{g-option-context}
  @see-function{g-option-context-set-translate-func}
  @see-function{g-option-context-set-translation-domain}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-summary)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_description ()
;;; g_option_context_set_description () -> g-option-context-description
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-description) (description context)
  (foreign-funcall "g_option_context_set_description"
                   (:pointer (:struct g-option-context)) context
                   :string (if description description (null-pointer))
                   :void)
  description)

(defcfun ("g_option_context_get_description" g-option-context-description)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @syntax[]{(g-option-context-description context) => description}
  @syntax[]{(setf (g-option-context-descripton context) description)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[description]{a string to be shown in --help output after the list
    of options, or @code{nil}}
  @begin{short}
    The function @sym{g-option-context-description} returns the description.
  @end{short}
  The function @sym{(setf g-option-context-description} adds a string to be
  displayed in --help output after the list of options. This text often
  includes a bug reporting address.

  Note that the summary is translated. See the function
  @fun{g-option-context-set-translate-func}.
  @see-type{g-main-context}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-description)

;;; ----------------------------------------------------------------------------
;;; GTranslateFunc ()
;;;
;;; const gchar * (*GTranslateFunc) (const gchar *str, gpointer data);
;;;
;;; The type of functions which are used to translate user-visible strings, for
;;; --help output.
;;;
;;; str :
;;;     the untranslated string
;;;
;;; data :
;;;     user data specified when installing the function, e.g. in
;;;     g_option_group_set_translate_func()
;;;
;;; Returns :
;;;     a translation of the string for the current locale. The returned string
;;;     is owned by GLib and must not be freed.
;;; ----------------------------------------------------------------------------

(defcallback g-translate-func-cb (:string :free-from-foreign nil)
    ((str :string)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn str)
      (return-from-g-translate-func-cb () nil))))

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_translate_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_set_translate_func"
          %g-option-context-set-translate-func) :void
  (context (:pointer (:struct g-option-context)))
  (func :pointer)
  (func-data :pointer)
  (destroy-notify :pointer))

(defun g-option-context-set-translate-func (context func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @argument[func]{the @code{GTranslateFunc}, or @code{NULL}}
  @begin{short}
    Sets the function which is used to translate the contexts user-visible
    strings, for --help output. If @arg{func} is @code{NULL}, strings are not
    translated.
  @end{short}

  Note that option groups have their own translation functions, this function
  only affects the @code{parameter-string}, see the function
  @fun{g-option-context-new}, the @code{summary}, see the function
  @fun{g-option-context-summary}, and the @code{description}, see the
  function @fun{g-option-context-description}.

  If you are using @code{gettext()}, you only need to set the translation
  domain, see the function @fun{g-option-context-set-translation-domain}.
  @see-type{g-option-context}
  @see-function{g-option-context-new}
  @see-function{g-option-context-summary}
  @see-function{g-option-context-description}
  @see-function{g-option-context-set-translation-domain}"
  (%g-option-context-set-translate-func
                               context
                               (callback g-translate-func-cb)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'g-option-context-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_translation_domain ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_set_translation_domain"
           g-option-context-set-translation-domain) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @argument[domain]{a string with the domain to use}
  @begin{short}
    A convenience function to use @code{gettext()} for translating user-visible
    strings.
  @end{short}
  @see-type{g-option-context}"
  (context (:pointer (:struct g-option-context)))
  (domain :string))

(export 'g-option-context-set-translation-domain)

;;; ----------------------------------------------------------------------------
;;; g_option_context_free ()
;;;
;;; void g_option_context_free (GOptionContext *context);
;;;
;;; Frees context and all the groups which have been added to it.
;;;
;;; Please note that parsed arguments need to be freed separately
;;; (see GOptionEntry).
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_parse" %g-option-context-parse) :boolean
  (context (:pointer (:struct g-option-context)))
  (argc (:pointer :int))
  (argv (:pointer g-strv))
  (err :pointer))

(defun g-option-context-parse (context argv)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @argument[argv]{a list of strings with the command line arguments}
  @return{@em{True} if the parsing was successful, @em{false} if an error
    occurred.}
  @begin{short}
    Parses the command line arguments, recognizing options which have been
    added to @arg{context}.
  @end{short}
  A side-effect of calling this function is that the function @fun{g-prgname}
  will be called.

  If the parsing is successful, any parsed arguments are removed from the
  list @arg{argv} are updated accordingly. A '--' option is stripped
  from @arg{argv} unless there are unparsed options before and after it, or
  some of the options after it start with '-'. In case of an error, @arg{argv}
  are left unmodified.

  If automatic --help support is enabled, see the function
  @fun{g-option-context-set-help-enabled}, and the @arg{argv} list contains one
  of the recognized help options, this function will produce help output to
  stdout and call exit (0).

  Note that function depends on the current locale for automatic character set
  conversion of string and filename arguments.
  @see-type{g-option-context}
  @see-function{g-prgname}
  @see-function{g-option-context-help-enabled}"
  (with-g-error (err)
    (with-foreign-objects ((argc-ptr :int)
                           (argv-ptr '(:pointer g-strv)))
      (setf (mem-ref argc-ptr :int)
            (length argv)
            (mem-ref argv-ptr '(:pointer g-strv))
            (convert-to-foreign argv 'g-strv))
      (%g-option-context-parse context argc-ptr argv-ptr err))))

(export 'g-option-context-parse)

;;; ----------------------------------------------------------------------------
;;; g_option_context_parse_strv ()
;;;
;;; gboolean
;;; g_option_context_parse_strv (GOptionContext *context,
;;;                              gchar ***arguments,
;;;                              GError **error);
;;;
;;; Parses the command line arguments.
;;;
;;; This function is similar to g_option_context_parse() except that it respects
;;; the normal memory rules when dealing with a strv instead of assuming that
;;; the passed-in array is the argv of the main function.
;;;
;;; In particular, strings that are removed from the arguments list will be
;;; freed using g_free().
;;;
;;; On Windows, the strings are expected to be in UTF-8. This is in contrast to
;;; g_option_context_parse() which expects them to be in the system codepage,
;;; which is how they are passed as argv to main(). See
;;; g_win32_get_command_line() for a solution.
;;;
;;; This function is useful if you are trying to use GOptionContext with
;;; GApplication.
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; arguments :
;;;     a pointer to the command line arguments (which must be in UTF-8 on
;;;     Windows). Starting with GLib 2.62, arguments can be NULL, which matches
;;;     g_option_context_parse().
;;;
;;; error :
;;;     a return location for errors
;;;
;;; Returns :
;;;     TRUE if the parsing was successful, FALSE if an error occurred
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_help_enabled ()
;;; g_option_context_set_help_enabled () -> g-option-context-help-enabled
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-help-enabled) (help-enabled context)
  (foreign-funcall "g_option_context_set_help_enabled"
                   (:pointer (:struct g-option-context)) context
                   :boolean help-enabled
                   :void)
  help-enabled)

(defcfun ("g_option_context_get_help_enabled" g-option-context-help-enabled)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @syntax[]{(g-option-context-help-enabled context) => help-enabled}
  @syntax[]{(setf (g-option-context-help-enabled context) help-enabled)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[help-enabled]{@em{true} to enable --help, @em{false} to disable it}
  @begin{short}
    Enables or disables automatic generation of --help output.
  @end{short}
  By default, the function @fun{g-option-context-parse} recognizes --help, -h,
  -?, --help-all and --help-groupname and creates suitable output to stdout.
  @see-type{g-option-context}
  @see-function{g-option-context-parse}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-help-enabled)

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_ignore_unknown_options ()
;;;
;;; void
;;; g_option_context_set_ignore_unknown_options (GOptionContext *context,
;;;                                              gboolean ignore_unknown);
;;;
;;; Sets whether to ignore unknown options or not. If an argument is ignored,
;;; it is left in the argv array after parsing. By default,
;;; g_option_context_parse() treats unknown options as error.
;;;
;;; This setting does not affect non-option arguments (i.e. arguments which
;;; don't start with a dash). But note that GOption cannot reliably determine
;;; whether a non-option belongs to a preceding unknown option.
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; ignore_unknown :
;;;     TRUE to ignore unknown options, FALSE to produce an error when unknown
;;;     options are met
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_ignore_unknown_options ()
;;;
;;; gboolean
;;; g_option_context_get_ignore_unknown_options (GOptionContext *context)
;;;
;;; Returns whether unknown options are ignored or not. See
;;; g_option_context_set_ignore_unknown_options().
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; Returns :
;;;     TRUE if unknown options are ignored.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_help ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_get_help" %g-option-context-help) :string
  (context (:pointer (:struct g-option-context)))
  (main-help :boolean)
  (group :pointer))

(defun g-option-context-help (context main-help
                              &optional (group (null-pointer)))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @argument[main-help]{if @em{true}, only include the main group}
  @argument[group]{the @type{g-option-group} instance to create help for, or
    @code{nil}}
  @return{A newly allocated string containing the help text.}
  @begin{short}
    Returns a formatted, translated help text for the given context.
  @end{short}
  To obtain the text produced by --help, call the function
  @code{(g-option-context-help context t)}. To obtain the text produced by
  --help-all, call @code{(g-option-context-help context nil)}. To obtain the
  help text for an option group, call the function
  @code{(g-option-context-help context nil group)}.
  @see-type{g-option-context}
  @see-type{g-option-group}"
  (%g-option-context-help context main-help group))

(export 'g-option-context-help)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_strict_posix ()
;;;
;;; gboolean
;;; g_option_context_get_strict_posix (GOptionContext *context);
;;;
;;; Returns whether strict POSIX code is enabled.
;;;
;;; See g_option_context_set_strict_posix() for more information.
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; Returns :
;;;     TRUE if strict POSIX is enabled, FALSE otherwise.
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_strict_posix ()
;;;
;;; void
;;; g_option_context_set_strict_posix (GOptionContext *context,
;;;                                    gboolean strict_posix);
;;;
;;; Sets strict POSIX mode.
;;;
;;; By default, this mode is disabled.
;;;
;;; In strict POSIX mode, the first non-argument parameter encountered (eg:
;;; filename) terminates argument processing. Remaining arguments are treated
;;; as non-options and are not attempted to be parsed.
;;;
;;; If strict POSIX mode is disabled then parsing is done in the GNU way where
;;; option arguments can be freely mixed with non-options.
;;;
;;; As an example, consider "ls foo -l". With GNU style parsing, this will list
;;; "foo" in long mode. In strict POSIX style, this will list the files named
;;; "foo" and "-l".
;;;
;;; It may be useful to force strict POSIX mode when creating "verb style"
;;; command line tools. For example, the "gsettings" command line tool supports
;;; the global option "--schemadir" as well as many subcommands ("get", "set",
;;; etc.) which each have their own set of arguments. Using strict POSIX mode
;;; will allow parsing the global options up to the verb name while leaving the
;;; remaining options to be parsed by the relevant subcommand (which can be
;;; determined by examining the verb name, which should be present in argv[1]
;;; after parsing).
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; strict_posix :
;;;     the new value
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_add_main_entries ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_add_main_entries"
          %g-option-context-add-main-entries) :void
  (context (:pointer (:struct g-option-context)))
  (entries (:pointer (:pointer (:struct g-option-entry))))
  (translation-domain :string))

(defun g-option-context-add-main-entries (context entries translation-domain)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @arg[entries]{a list of  of @symbol{g-option-entry}s}
  @argument[translation-domain]{a translation domain to use for translating the
    --help output for the options in entries with @code{gettext()},
    or @code{NULL}}
  @begin{short}
    A convenience function which creates a main group if it does not exist,
    adds the @arg{entries} to it and sets the translation domain.
  @end{short}
  @see-type{g-option-context}
  @see-symbol{g-option-entry}"
  (let ((n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-option-entry) i)
        do (setf (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'long-name)
                 (first entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'short-name)
                 (if (second entry)
                     (char-code (second entry))
                     0)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'flags)
                 (third entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg)
                 (fourth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg-data)
                 (progn
                   (cond ((member (fourth entry)
                                  '(:none :int :double :string :filename
                                    :string-array :filename-array :int64))
                          (symbol-value (fifth entry)))
                         (t (error "Case not handled for g-option-entry"))))
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'description)
                 (sixth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg-description)
                 (if (seventh entry) (seventh entry) (null-pointer))))
      (let ((entry-ptr (mem-aptr entries-ptr
                                 '(:struct g-option-entry)
                                 n-entries)))
        (setf (foreign-slot-value entry-ptr
                                  '(:struct g-option-entry) 'long-name)
              (null-pointer))
        (%g-option-context-add-main-entries context
                                            entries-ptr
                                            (if translation-domain
                                                translation-domain
                                                (null-pointer)))))))

(export 'g-option-context-add-main-entries)

;;; ----------------------------------------------------------------------------
;;; g_option_context_add_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_add_group" g-option-context-add-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[context]{a @type{g-option-context} instance}
  @argument[group]{the @type{g-option-group} instance to add}
  @begin{short}
    Adds a @type{g-option-group} instance to the @arg{context}, so that parsing
    with @arg{context} will recognize the options in the group.
  @end{short}
  Note that the group will be freed together with the context when the function
  @fun{g-option-context-free} is called, so you must not free the group yourself
  after adding it to a context.
  @see-function{g-option-context-free}"
  (context (:pointer (:struct g-option-context)))
  (group (:pointer (:struct g-option-group))))

(export 'g-option-context-add-group)

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_main_group ()
;;;
;;; void g_option_context_set_main_group (GOptionContext *context,
;;;                                       GOptionGroup *group);
;;;
;;; Sets a GOptionGroup as main group of the context. This has the same effect
;;; as calling g_option_context_add_group(), the only difference is that the
;;; options in the main group are treated differently when generating --help
;;; output.
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; group :
;;;     the group to set as main group
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_main_group ()
;;;
;;; GOptionGroup * g_option_context_get_main_group (GOptionContext *context);
;;;
;;; Returns a pointer to the main group of context.
;;;
;;; context :
;;;     a GOptionContext
;;;
;;; Returns :
;;;     the main group of context, or NULL if context does not have a main
;;;     group. Note that group belongs to context and should not be modified or
;;;     freed.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_new ()
;;;
;;; GOptionGroup * g_option_group_new (const gchar *name,
;;;                                    const gchar *description,
;;;                                    const gchar *help_description,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify destroy);
;;;
;;; Creates a new GOptionGroup.
;;;
;;; name :
;;;     the name for the option group, this is used to provide help for the
;;;     options in this group with --help-name
;;;
;;; description :
;;;     a description for this group to be shown in --help. This string is
;;;     translated using the translation domain or translation function of the
;;;     group
;;;
;;; help_description :
;;;     a description for the --help-name option. This string is translated
;;;     using the translation domain or translation function of the group
;;;
;;; user_data :
;;;     user data that will be passed to the pre- and post-parse hooks, the
;;;     error hook and to callbacks of G_OPTION_ARG_CALLBACK options, or NULL
;;;
;;; destroy :
;;;     a function that will be called to free user_data, or NULL
;;;
;;; Returns :
;;;     a newly created option group. It should be added to a GOptionContext or
;;;     freed with g_option_group_free().
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_ref ()
;;;
;;; GOptionGroup *
;;; g_option_group_ref (GOptionGroup *group);
;;;
;;; Increments the reference count of group by one.
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; Returns :
;;;     a GOptionGroup
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_unref ()
;;;
;;; void
;;; g_option_group_unref (GOptionGroup *group);
;;;
;;; Decrements the reference count of group by one. If the reference count
;;; drops to 0, the group will be freed. and all memory allocated by the group
;;; is released.
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_free ()
;;;
;;; void g_option_group_free (GOptionGroup *group);
;;;
;;; Warning
;;;
;;; g_option_group_free has been deprecated since version 2.44 and should not
;;; be used in newly-written code. Use g_option_group_unref() instead.
;;;
;;; Frees a GOptionGroup. Note that you must not free groups which have been
;;; added to a GOptionContext.
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_add_entries ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_group_add_entries"
          %g-option-group-add-entries) :void
  (context (:pointer (:struct g-option-group)))
  (entries (:pointer (:pointer (:struct g-option-entry)))))

(defun g-option-group-add-entries (group entries)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[group]{a @type{g-option-group} instance}
  @argument[entries]{a list of options}
  @begin{short}
    Adds the options specified in @arg{entries} to @arg{group}.
  @end{short}
  @see-type{g-option-group}"
  (let ((n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-option-entry) i)
        do (setf (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'long-name)
                 (first entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'short-name)
                 (if (second entry)
                     (char-code (second entry))
                     0)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'flags)
                 (third entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg)
                 (fourth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg-data)
                 (progn
                   (cond ((member (fourth entry)
                                  '(:none :int :double :string :filename
                                    :string-array :filename-array :int64))
                          (symbol-value (fifth entry)))
                         (t (error "Case not handled for g-option-entry"))))
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'description)
                 (sixth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct g-option-entry)
                                     'arg-description)
                 (if (seventh entry) (seventh entry) (null-pointer))))
      (let ((entry-ptr (mem-aptr entries-ptr
                                 '(:struct g-option-entry)
                                 n-entries)))
        (setf (foreign-slot-value entry-ptr
                                  '(:struct g-option-entry) 'long-name)
              (null-pointer))
        (%g-option-group-add-entries group entries-ptr)))))

(export 'g-option-group-add-entries)

;;; ----------------------------------------------------------------------------
;;; GOptionParseFunc ()
;;;
;;; gboolean (*GOptionParseFunc) (GOptionContext *context,
;;;                               GOptionGroup *group,
;;;                               gpointer data,
;;;                               GError **error);
;;;
;;; The type of function that can be called before and after parsing.
;;;
;;; context :
;;;     The active GOptionContext
;;;
;;; group :
;;;     The group to which the function belongs
;;;
;;; data :
;;;     User data added to the GOptionGroup containing the option when it was
;;;     created with g_option_group_new()
;;;
;;; error :
;;;     A return location for error details
;;;
;;; Returns :
;;;     TRUE if the function completed successfully, FALSE if an error occurred,
;;;     in which case error should be set with g_set_error()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_set_parse_hooks ()
;;;
;;; void g_option_group_set_parse_hooks (GOptionGroup *group,
;;;                                      GOptionParseFunc pre_parse_func,
;;;                                      GOptionParseFunc post_parse_func);
;;;
;;; Associates two functions with group which will be called from
;;; g_option_context_parse() before the first option is parsed and after the
;;; last option has been parsed, respectively.
;;;
;;; Note that the user data to be passed to pre_parse_func and post_parse_func
;;; can be specified when constructing the group with g_option_group_new().
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; pre_parse_func :
;;;     a function to call before parsing, or NULL
;;;
;;; post_parse_func :
;;;     a function to call after parsing, or NULL
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GOptionErrorFunc ()
;;;
;;; void (*GOptionErrorFunc) (GOptionContext *context,
;;;                           GOptionGroup *group,
;;;                           gpointer data,
;;;                           GError **error);
;;;
;;; The type of function to be used as callback when a parse error occurs.
;;;
;;; context :
;;;     The active GOptionContext
;;;
;;; group :
;;;     The group to which the function belongs
;;;
;;; data :
;;;     User data added to the GOptionGroup containing the option when it was
;;;     created with g_option_group_new()
;;;
;;; error :
;;;     The GError containing details about the parse error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_set_error_hook ()
;;;
;;; void g_option_group_set_error_hook (GOptionGroup *group,
;;;                                     GOptionErrorFunc error_func);
;;;
;;; Associates a function with group which will be called from
;;; g_option_context_parse() when an error occurs.
;;;
;;; Note that the user data to be passed to error_func can be specified when
;;; constructing the group with g_option_group_new().
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; error_func :
;;;     a function to call when an error occurs
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_set_translate_func ()
;;;
;;; void g_option_group_set_translate_func (GOptionGroup *group,
;;;                                         GTranslateFunc func,
;;;                                         gpointer data,
;;;                                         GDestroyNotify destroy_notify);
;;;
;;; Sets the function which is used to translate user-visible strings, for
;;; --help output. Different groups can use different GTranslateFuncs. If func
;;; is NULL, strings are not translated.
;;;
;;; If you are using gettext(), you only need to set the translation domain,
;;; see g_option_group_set_translation_domain().
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; func :
;;;     the GTranslateFunc, or NULL
;;;
;;; data :
;;;     user data to pass to func, or NULL
;;;
;;; destroy_notify :
;;;     a function which gets called to free data, or NULL
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_option_group_set_translation_domain ()
;;;
;;; void g_option_group_set_translation_domain (GOptionGroup *group,
;;;                                             const gchar *domain);
;;;
;;; A convenience function to use gettext() for translating user-visible
;;; strings.
;;;
;;; group :
;;;     a GOptionGroup
;;;
;;; domain :
;;;     the domain to use
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; --- End of file g.option-group.lisp ----------------------------------------

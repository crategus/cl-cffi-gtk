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
;;; Command line option parser
;;;
;;;     Parses command line options
;;;
;;; Types and Values
;;;
;;;     GOptionError
;;;     G_OPTION_ERROR
;;;
;;;     GOptionArg
;;;     GOptionFlags
;;;     GOptionEntry
;;;
;;;     GOptionContext
;;;     G_OPTION_REMAINING
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
 "@version{2021-8-11}
  @begin{short}
    The @sym{g-option-arg} enumeration determines which type of extra argument
    the options expect to find. If an option expects an extra argument, it can
    be specified in several ways, with a short option: @code{-x arg}, with a
    long option: @code{--name arg} or combined in a single argument:
    @code{--name=arg}.
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
    @entry[:callback]{The option provides a callback function to parse the
      extra argument.}
    @entry[:filename]{The option takes a filename as argument.}
    @entry[:string-array]{The option takes a string argument, multiple uses of
      the option are collected into a list of strings.}
    @entry[:filename-array]{The option takes a filename as argument, multiple
      uses of the option are collected into a list of strings.}
    @entry[:double]{The option takes a double float argument. The argument can
      be formatted either for the locale of the user or for the \"C\" locale.}
    @entry[:int64]{The option takes a 64-bit integer. Like @code{:int} but for
      larger numbers. The number can be in decimal base, or in hexadecimal,
      when prefixed with @code{0x}, for example, @code{0xffffffff}.}
  @end{table}
  @see-type{g-option-context}")

(export 'g-option-arg)

;;; ----------------------------------------------------------------------------
;;; enum GOptionFlags
;;;-----------------------------------------------------------------------------

(defbitfield g-option-flags
  (:none 0)
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
 "@version{2021-8-11}
  @short{Flags which modify individual options.}
  @begin{pre}
(defbitfield g-option-flags
  :none
  :hidden
  :in-main
  :reverse
  :no-arg
  :filename
  :optional-arg
  :noalias)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No flags.}
    @entry[:hidden]{The option does not appear in @code{--help} output.}
    @entry[:in-main]{The option appears in the main section of the @code{--help}
      output, even if it is defined in a group.}
    @entry[:reverse]{For options of the @code{:none} kind, this flag indicates
      that the sense of the option is reversed.}
    @entry[:no-arg]{For options of the @code{:callback} kind, this flag
      indicates that the callback function does not take any argument, like a
      @code{:none} option.}
    @entry[:filename]{For options of the @code{:callback} kind, this flag
      indicates that the argument should be passed to the callback function in
      the GLib filename encoding rather than UTF-8.}
    @entry[:optional-arg]{For options of the @code{:callback} kind, this flag
      indicates that the argument supply is optional. If no argument is
      given then data of the @code{GOptionParseFunc} callback function will be
      set to @code{NULL}.}
    @entry[:noalias]{This flag turns off the automatic conflict resolution which
      prefixes long option names with a group name, if there is a conflict. This
      option should only be used in situations where aliasing is necessary to
      model some legacy command line interface. It is not safe to use this
      option, unless all option groups are under your direct control.}
  @end{table}
  @see-type{g-option-context}")

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
;;; struct GOptionEntry                                    not exported
;;; ----------------------------------------------------------------------------

(defcstruct g-option-entry
  (long-name :string)
  (short-name :char)
  (flags g-option-flags)
  (arg g-option-arg)
  (arg-data :pointer)
  (description :string)
  (arg-description :string))

;;; ----------------------------------------------------------------------------
;;; GOptionContext
;;; ----------------------------------------------------------------------------

(defcstruct g-option-context)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-context atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-option-context 'type)
 "@version{2021-8-11}
  @begin{short}
    The GOption command line parser is intended to be a simpler replacement for
    the popt library.
  @end{short}
  It supports short and long command line options, as shown in the following
  example:
  @begin{pre}
testtreemodel -r 1 --max-size 20 --rand --display=:1.0 -vb -- file1 file2
  @end{pre}
  The example demonstrates a number of features of the GOption command line
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
      any remaining arguments, even those starting with a dash, are returned to
      the application as rest arguments.}
  @end{itemize}
  Another important feature of GOption is that it can automatically generate
  nicely formatted help output. Unless it is explicitly turned off with the
  function @fun{g-option-context-help-enabled}, GOption will recognize the
  @code{--help}, @code{-?}, @code{--help-all} and @code{--help-groupname}
  options, where @code{groupname} is the name of a @type{g-option-group}
  instance, and write a text similar to the one shown in the following example.
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
  GOption groups options in a @type{g-option-group} instance, which makes it
  easy to incorporate options from multiple sources. The intended use for this
  is to let applications collect option groups from the libraries it uses, add
  them to their @sym{g-option-context} instance, and parse all options by a
  single call to the function @fun{g-option-context-parse}. See the function
  @fun{gtk-option-group} for an example.

  If an option is declared to be of type string or filename, GOption takes care
  of converting it to the right encoding. Strings are returned in UTF-8,
  filenames are returned in the GLib filename encoding. Note that this only
  works if the function @code{setlocale()} has been called before the function
  @fun{g-option-context-parse}.

  Here is a complete example of setting up GOption to parse the example
  command line above and produce the example help output.
  @begin{pre}
(defvar repeats (cffi:foreign-alloc :int :initial-element 2))
(defvar max-size (cffi:foreign-alloc :int :initial-element 0))
(defvar verbose (cffi:foreign-alloc :boolean :initial-element nil))
(defvar beep (cffi:foreign-alloc :boolean :initial-element nil))
(defvar randomize (cffi:foreign-alloc :boolean :initial-element nil))

(defun main (argv)
  (let ((entries '((\"repeats\"                        ; long-name
                    #\\r                              ; short-name
                    :none                            ; flags
                    :int                             ; arg
                    repeats                          ; arg-data
                    \"Average over N repetitions\"     ; description
                    \"N\")                             ; arg-description
                   (\"max-size\"
                    #\\m
                    0
                    :int
                    max-size
                    \"Test up to 2^M items\"
                    \"M\")
                   (\"verbose\"
                    #\\v
                    0
                    :none
                    verbose
                    \"Be verbose\"
                    nil)
                   (\"beep\"
                    #\\b
                    0
                    :none
                    beep
                    \"Beep when done\"
                    nil)
                   (\"rand\"
                    #\\Nul
                    0
                    :none
                    randomize
                    \"Randomize the data\"
                    nil)))
         (context (g-option-context-new \"- test tree model performance\")))

    (g-option-context-add-main-entries context entries nil)
    (g-option-context-add-group context (gtk-option-group t))

    (if (g-option-context-parse context argv)
        (progn
          (format t \"Option parsing failed.~%\")
          ;; Print the Help Usage
          (format t \"~&~%~a~%\" (g-option-context-help context t)))
        (progn
          ;; Print the parsed arguments
          (format t \"~&Parsed arguments~%\")
          (format t \"   repeats : ~a~%\" (mem-ref repeats :int))
          (format t \"  max-size : ~a~%\" (mem-ref max-size :int))
          (format t \"   verbose : ~a~%\" (mem-ref verbose :boolean))
          (format t \"      beep : ~a~%\" (mem-ref beep :boolean))
          (format t \" randomize : ~a~%\" (mem-ref randomize :boolean))))
  ... ))
  @end{pre}
  On UNIX systems, the argument @code{argv} that is passed to the function
  @code{main} has no particular encoding, even to the extent that different
  parts of it may have different encodings. In general, normal arguments and
  flags will be in the current locale and filenames should be considered to be
  opaque byte strings. Proper use of the @code{:filename} vs @code{:string}
  option arguments is therefore important.

  Note that on Windows, filenames do have an encoding, but using a
  @sym{g-option-context} instance with the argument @code{argv} as passed to the
  function @code{main} will result in a program that can only accept command
  line arguments with characters from the system codepage. This can cause
  problems when attempting to deal with filenames containing Unicode characters
  that fall outside of the codepage.

  A solution to this is to use the functions @code{g_win32_get_command_line()}
  and @fun{g-option-context-parse-strv} which will properly handle full Unicode
  filenames. If you are using a @class{g-application} instance, this is done
  automatically for you.
  @see-type{g-option-group}
  @see-class{g-application}
  @see-symbol{g-option-arg}
  @see-symbol{g-option-flags}")

(export 'g-option-context)

;;; ----------------------------------------------------------------------------
;;; GOptionGroup
;;; ----------------------------------------------------------------------------

(defcstruct g-option-group)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-option-group atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-option-group 'type)
 "@version{2021-8-11}
  @begin{short}
    A @sym{g-option-group} structure defines the options in a single group.
  @end{short}
  The structure has only private fields and should not be directly accessed.

  All options in a group share the same translation function. Libraries which
  need to parse command line options are expected to provide a function for
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
  (parameter :string))

(defun g-option-context-new (&optional (parameter nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[parameter]{a string which is displayed in the first line of
    @code{--help} output}
  @begin{return}
    A newly created @type{g-option-context} instance, which must be freed with
    the function @fun{g-option-context-free} after use.
  @end{return}
  @short{Creates a new option context.}

  The argument @arg{parameter} can serve multiple purposes. It can be
  used to add descriptions for \"rest\" arguments, which are not parsed by the
  @type{g-option-context} instance, typically something like \"FILES\" or
  \"FILE1 FILE2...\". If you are using @code{G_OPTION_REMAINING} for collecting
  \"rest\" arguments, GLib handles this automatically by using the
  @arg{arg-description} of the corresponding option entry in the usage summary.

  Another usage is to give a short summary of the program functionality, like
  \" - frob the strings\", which will be displayed in the same line as the
  usage. For a longer description of the program functionality that should be
  displayed as a paragraph below the usage line, use the function
  @fun{g-option-context-summary}.

  Note that the argument @arg{parameter} is translated using the function
  set with the function @fun{g-option-context-set-translate-func}, so it should
  normally be passed untranslated.
  @begin[Example]{dictionary}
    @begin{pre}
(g-option-context-new \"This is an example for a description.\")
=> #.(SB-SYS:INT-SAP #X0817EB48)
(g-option-context-help * nil)
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
  @see-function{g-option-context-summary}
  @see-function{g-option-context-set-translate-func}"
  (%g-option-context-new (if parameter parameter (null-pointer))))

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
 "@version{2021-8-11}
  @syntax[]{(g-option-context-summary context) => summary}
  @syntax[]{(setf (g-option-context-summary context) summary)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[summary]{a string to be shown in @code{--help} output before the
    list of options, or @code{nil}}
  @begin{short}
    The function @sym{g-option-context-summary} returns the summary.
  @end{short}
  The function @sym{(setf g-option-context-summary)} adds a string to be
  displayed in @code{--help} output before the list of options. This is
  typically a summary of the program functionality.

  Note that the summary is translated. See the functions
  @fun{g-option-context-set-translate-func} and
  @fun{g-option-context-set-translation-domain}.
  @begin[Example]{dictionary}
    @begin{pre}
(setq context (g-option-context-new \"A short description.\"))
=> #.(SB-SYS:INT-SAP #X561C3BDDC430)
(setf (g-option-context-summary context) \"This is the summary.\")
=> \"This is the summary.\"
(g-option-context-help context nil)
=>
\"Aufruf:
  sbcl [OPTION …] A short description.

This is the summary.

Hilfeoptionen:
  -h, --help       Hilfeoptionen anzeigen
\"
    @end{pre}
  @end{dictionary}
  @see-type{g-option-context}
  @see-function{g-option-context-description}
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
 "@version{2021-8-11}
  @syntax[]{(g-option-context-description context) => description}
  @syntax[]{(setf (g-option-context-descripton context) description)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[description]{a string to be shown in @code{--help} output after the
    list of options, or @code{nil}}
  @begin{short}
    The function @sym{g-option-context-description} returns the description.
  @end{short}
  The function @sym{(setf g-option-context-description)} adds a string to be
  displayed in @code{--help} output after the list of options. This text often
  includes a bug reporting address.

  Note that the summary is translated. See the function
  @fun{g-option-context-set-translate-func}.
  @begin[Example]{dictionary}
    @begin{pre}
(setq context (g-option-context-new \"A short description\"))
=> #.(SB-SYS:INT-SAP #X55637A1CF6D0)
(setf (g-option-context-description context) \"More descriptions.\")
=> \"More descriptions.\"
(g-option-context-help context nil)
\"Aufruf:
  sbcl [OPTION …] A short description

Hilfeoptionen:
  -h, --help       Hilfeoptionen anzeigen

More descriptions.
\"
    @end{pre}
  @end{dictionary}
  @see-type{g-option-context}
  @see-function{g-option-context-summary}
  @see-function{g-option-context-set-translate-func}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-description)

;;; ----------------------------------------------------------------------------
;;; GTranslateFunc ()
;;; ----------------------------------------------------------------------------

(defcallback g-translate-func (:string :free-from-foreign nil)
    ((str :string)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn str)
      (return-from-g-translate-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-translate-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'g-translate-func atdoc:*external-symbols*)
 "@version{2021-8-11}
  @begin{short}
    The type of functions which are used to translate user visible strings, for
    @code{--help} output.
  @end{short}
  @begin{pre}
 lambda (str)
  @end{pre}
  @begin[code]{table}
    @entry[str]{The unstranslated string.}
    @entry[Returns]{A translation of the string for the current locale.}
  @end{table}
  @see-type{g-option-context}
  @see-function{g-option-context-set-translate-func}
  @see-function{g-option-group-set-translate-func}")

(export 'g-translate-func)

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_translate_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_set_translate_func"
          %g-option-context-set-translate-func) :void
  (context (:pointer (:struct g-option-context)))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun g-option-context-set-translate-func (context func)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[func]{the @symbol{g-translate-func} callback function, or
    @code{nil}}
  @begin{short}
    Sets the function which is used to translate the option contexts user
    visible strings, for @code{--help} output.
  @end{short}
  If @arg{func} is @code{nil}, strings are not translated.

  Note that option groups have their own translation functions, this function
  only affects the argument @code{parameter} of the function
  @fun{g-option-context-new}, the argument @code{summary} of the function
  @fun{g-option-context-summary}, and the argument @code{description} of the
  function @fun{g-option-context-description}.

  If you are using the function @code{gettext()}, you only need to set the
  translation domain, see the function
  @fun{g-option-context-set-translation-domain}.
  @see-type{g-option-context}
  @see-function{g-option-context-new}
  @see-function{g-option-context-summary}
  @see-function{g-option-context-description}
  @see-function{g-option-context-set-translation-domain}"
  (if func
      (%g-option-context-set-translate-func
                                       context
                                       (callback g-translate-func)
                                       (allocate-stable-pointer func)
                                       (callback stable-pointer-destroy-notify))
      (%g-option-context-set-translate-func context
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'g-option-context-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; g_option_context_set_translation_domain ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_set_translation_domain"
           g-option-context-set-translation-domain) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[domain]{a string with the translation domain to use}
  @begin{short}
    A convenience function to use the function @code{gettext()} for translating
    user visible strings.
  @end{short}
  @see-type{g-option-context}"
  (context (:pointer (:struct g-option-context)))
  (domain :string))

(export 'g-option-context-set-translation-domain)

;;; ----------------------------------------------------------------------------
;;; g_option_context_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_free" g-option-context-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @begin{short}
    Frees @arg{context} and all the groups which have been added to it.
  @end{short}
  Please note that parsed arguments need to be freed separately.
  @see-type{g-option-context}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-free)

;;; ----------------------------------------------------------------------------
;;; g_option_context_parse ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_parse" %g-option-context-parse) :boolean
  (context (:pointer (:struct g-option-context)))
  (argc :pointer)
  (argv :pointer)
  (err :pointer))

(defun g-option-context-parse (context argv)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
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
  list and @arg{argv} is updated accordingly. A @code{--} option is stripped
  from @arg{argv} unless there are unparsed options before and after it, or
  some of the options after it start with @code{-}. In case of an error,
  @arg{argv} is left unmodified.

  If automatic @code{--help} support is enabled, see the function
  @fun{g-option-context-help-enabled}, and the @arg{argv} list contains one
  of the recognized help options, this function will produce help output to
  stdout and call @code{exit(0)}.

  Note that function depends on the current locale for automatic character set
  conversion of string and filename arguments.
  @see-type{g-option-context}
  @see-function{g-prgname}
  @see-function{g-option-context-help-enabled}"
  (with-g-error (err)
    (with-foreign-objects ((argc :int) (argv-ptr :pointer))
      (setf (mem-ref argc :int) (length argv))
      (setf (mem-ref argv-ptr :pointer)
            (convert-to-foreign argv 'g-strv))
      (%g-option-context-parse context argc argv-ptr err))))

(export 'g-option-context-parse)

;;; ----------------------------------------------------------------------------
;;; g_option_context_parse_strv ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_parse_strv" %g-option-context-parse-strv) :boolean
  (context (:pointer (:struct g-option-context)))
  (arguments :pointer)
  (err :pointer))

(defun g-option-context-parse-strv (context arguments)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[arguments]{a list of strings with the command line arguments, which
    must be in UTF-8 on Windows, starting with GLib 2.62, @arg{arguments} can
    be @code{nil}, which matches the function @fun{g-option-context-parse}}
  @return{@em{True} if the parsing was successful, @em{false} if an error
    occurred.}
  @begin{short}
    Parses the command line arguments.
  @end{short}

  This function is similar to the function @fun{g-option-context-parse} except
  that it respects the normal memory rules when dealing with a list of strings
  instead of assuming that the passed-in list is the @code{argv} of the main
  function.

  In particular, strings that are removed from the arguments list will be freed
  using the function @code{g_free()}.

  On Windows, the strings are expected to be in UTF-8. This is in contrast to
  the function @fun{g-option-context-parse} which expects them to be in the
  system codepage, which is how they are passed as @code{argv} to
  @code{main()}. See the function @code{g_win32_get_command_line()} for a
  solution.

  This function is useful if you are trying to use a @type{g-option-context}
  instance with a @class{g-application} instance.
  @see-type{g-option-context}
  @see-class{g-application}
  @see-function{g-option-context-parse}"
  (with-g-error (err)
    (with-foreign-object (argv-ptr :pointer)
      (setf (mem-ref argv-ptr :pointer)
            (if arguments
                (convert-to-foreign arguments '(g-strv :free-to-foreign nil))
                (null-pointer)))
      (%g-option-context-parse-strv context argv-ptr err))))

(export 'g-option-context-parse-strv)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_help_enabled ()
;;; g_option_context_set_help_enabled () -> g-option-context-help-enabled
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-help-enabled) (enabled context)
  (foreign-funcall "g_option_context_set_help_enabled"
                   (:pointer (:struct g-option-context)) context
                   :boolean enabled
                   :void)
  enabled)

(defcfun ("g_option_context_get_help_enabled" g-option-context-help-enabled)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @syntax[]{(g-option-context-help-enabled context) => enabled}
  @syntax[]{(setf (g-option-context-help-enabled context) enabled)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[enabled]{@em{true} to enable @code{--help} output, @em{false} to
    disable it}
  @begin{short}
    Enables or disables automatic generation of @code{--help} output.
  @end{short}
  By default, the function @fun{g-option-context-parse} recognizes the options
  @code{--help}, @code{-h}, @code{-?}, @code{--help-all} and
  @code{--help-groupname} and creates suitable output to stdout.
  @see-type{g-option-context}
  @see-function{g-option-context-parse}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-help-enabled)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_ignore_unknown_options ()
;;; g_option_context_set_ignore_unknown_options ()
;;; -> g-option-context-ignore-unknown-options
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-ignore-unknown-options) (value context)
  (foreign-funcall "g_option_context_set_ignore_unknown_options"
                   (:pointer (:struct g-option-context)) context
                   :boolean value
                   :void)
  value)

(defcfun ("g_option_context_get_ignore_unkown_options"
           g-option-context-ignore-unknown-options) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @syntax[]{(g-option-context-ignore-unknown-options context) => ignore}
  @syntax[]{(setf (g-option-context-ignore-unknown-options context) ignore)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[ignore]{@em{true} to ignore unknown options, @em{false} to produce
    an error when unknown options are met}
  @begin{short}
    The function @sym{g-option-context-ignore-unknown-options} returns whether
    unknown options are ignored or not.
  @end{short}
  The function @sym{(setf g-option-context-ignore-unknown-options)} sets
  whether to ignore unknown options or not. If an argument is ignored, it is
  left in the @code{argv} list after parsing. By default, the function
  @fun{g-option-context-parse} treats unknown options as error.

  This setting does not affect non-option arguments, i.e. arguments which
  do not start with a dash. But note that GOption cannot reliably determine
  whether a non-option belongs to a preceding unknown option.
  @see-type{g-option-context}
  @see-function{g-option-context-parse}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-ignore-unknown-options)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_help () -> g-option-context-help
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_get_help" %g-option-context-help) :string
  (context (:pointer (:struct g-option-context)))
  (main :boolean)
  (group :pointer))

(defun g-option-context-help (context main &optional (group (null-pointer)))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[main]{if @em{true}, only include the main group}
  @argument[group]{the optional @type{g-option-group} instance to create help
    for}
  @return{A newly allocated string containing the help text.}
  @begin{short}
    Returns a formatted, translated help text for the given option context.
  @end{short}
  To obtain the text produced by the @code{--help} output, call the
  @code{(g-option-context-help context t)}. To obtain the text produced by
  @code{--help-all}, call @code{(g-option-context-help context nil)}. To obtain
  the help text for an option group, call
  @code{(g-option-context-help context nil group)}.
  @see-type{g-option-context}
  @see-type{g-option-group}"
  (%g-option-context-help context main group))

(export 'g-option-context-help)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_strict_posix ()
;;; g_option_context_set_strict_posix () -> g-option-context-strict-posix
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-strict-posix) (value context)
  (foreign-funcall "g_option_context_set_strict_posix"
                   (:pointer (:struct g-option-context)) context
                   :boolean value
                   :void)
  value)

(defcfun ("g_option_context_get_strict_posix" g-option-context-strict-posix)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[strict]{@em{true} if strict POSIX is enabled, @em{false} otherwise}
  @begin{short}
    The function @sym{g-option-context-strict-posix} returns whether strict
    POSIX code is enabled.
  @end{short}
  The function @sym{(setf g-option-context-strict-posix)} sets strict POSIX
  mode. By default, this mode is disabled.

  In strict POSIX mode, the first non-argument parameter encountered, e.g.
  @code{filename}, terminates argument processing. Remaining arguments are
  treated as non-options and are not attempted to be parsed.

  If strict POSIX mode is disabled then parsing is done in the GNU way where
  option arguments can be freely mixed with non-options.

  As an example, consider @code{ls foo -l}. With GNU style parsing, this will
  list @code{foo} in long mode. In strict POSIX style, this will list the files
  named @code{foo} and @code{-l}.

  It may be useful to force strict POSIX mode when creating \"verb style\"
  command line tools. For example, the @code{gsettings} command line tool
  supports the global option @code{--schemadir} as well as many subcommands,
  @code{get}, @code{set}, etc., which each have their own set of arguments.
  Using strict POSIX mode will allow parsing the global options up to the verb
  name while leaving the remaining options to be parsed by the relevant
  subcommand, which can be determined by examining the verb name, which should
  be present in @code{argv[1]} after parsing.
  @see-type{g-option-context}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-strict-posix)

;;; ----------------------------------------------------------------------------
;;; g_option_context_add_main_entries ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_add_main_entries"
          %g-option-context-add-main-entries) :void
  (context (:pointer (:struct g-option-context)))
  (entries (:pointer (:pointer (:struct g-option-entry))))
  (domain :string))

(defun g-option-context-add-main-entries (context entries domain)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[entries]{a list of option entries}
  @argument[domain]{a string with a translation domain to use for translating
    the @code{--help} output for the options in @arg{entries} with the function
    @code{gettext()}, or @code{nil}}
  @begin{short}
    A convenience function which creates a main group if it does not exist,
    adds the option entries to it and sets the translation domain.
  @end{short}

  The list of option entries has the following syntax. See the
  @type{g-option-context} documentation for a complete example.
  @begin{pre}
(let ((entries '((long-name
                  short-name
                  flags
                  arg
                  arg-data
                  description
                  arg-description)
                 (<next option>)
                 )))
   ...)
  @end{pre}
  @begin[code]{table}
    @entry[long-name]{A string with the long name of an option can be used to
      specify it in a command line as @code{--long-name}. Every option must
      have a long name. To resolve conflicts if multiple option groups contain
      the same long name, it is also possible to specify the option as
      @code{--groupname-long-name}.}
    @entry[short-name]{If an option has a short name, it can be specified
      @code{-short-name} in a command line. The argument @arg{short-name} must
      be a printable ASCII character different from '-', or @code{#\Nul} if the
      option has no short name.}
    @entry[flags]{Flags from the @symbol{g-option-flags} bitfield.}
    @entry[arg]{The type of the option, as a @symbol{g-option-arg} value.}
    @entry[arg-data]{If the @arg{arg} type is @code{:callback}, then
      @arg{arg-data} must point to a @code{GOptionArgFunc} callback function,
      which will be called to handle the extra argument. Otherwise,
      @arg{arg-data} is a pointer to a location to store the value, the required
      type of the location depends on the @arg{arg} type. If @arg{arg} type is
      @code{:string} or @code{:filename} the location will contain a newly
      allocated string if the option was given.}
    @entry[description]{A string with the description for the option in
      @code{--help} output. The description is translated using the
      @code{translate-func} of the group, see the function
      @fun{g-option-group-set-translation-domain}.}
    @entry[arg-description]{A string with the placeholder to use for the extra
      argument parsed by the option in @code{--help} output. The
      @arg{arg-description} is translated using the @code{translate-func} of the
      group, see the function @fun{g-option-group-set-translation-domain}.}
  @end{table}
  @see-type{g-option-context}
  @see-symbol{g-option-flags}
  @see-symbol{g-option-arg}
  @see-function{g-option-group-set-translation-domain}"
  (let ((group (or (not (null-pointer-p (g-option-context-main-group context)))
                   (setf (g-option-context-main-group context)
                         (g-option-group-new nil nil nil)))))
  (g-option-group-add-entries group entries)
  (g-option-group-set-translation-domain group
                                         (if domain domain (null-pointer)))))

(export 'g-option-context-add-main-entries)

;;; ----------------------------------------------------------------------------
;;; g_option_context_add_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_context_add_group" g-option-context-add-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[context]{a @type{g-option-context} instance}
  @argument[group]{the @type{g-option-group} instance to add}
  @begin{short}
    Adds a @type{g-option-group} instance to @arg{context}, so that parsing
    with @arg{context} will recognize the options in the group.
  @end{short}
  Note that the group will be freed together with the option context when the
  function @fun{g-option-context-free} is called, so you must not free the
  group yourself after adding it to an option context.
  @see-type{g-option-context}
  @see-type{g-option-group}
  @see-function{g-option-context-free}"
  (context (:pointer (:struct g-option-context)))
  (group (:pointer (:struct g-option-group))))

(export 'g-option-context-add-group)

;;; ----------------------------------------------------------------------------
;;; g_option_context_get_main_group ()
;;; g_option_context_set_main_group () -> g-option-context-main-group
;;; ----------------------------------------------------------------------------

(defun (setf g-option-context-main-group) (group context)
  (foreign-funcall "g_option_context_set_main_group"
                   (:pointer (:struct g-option-context)) context
                   (:pointer (:struct g-option-group)) group
                   :void)
  group)

(defcfun ("g_option_context_get_main_group" g-option-context-main-group)
    (:pointer (:struct g-option-group))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @syntax[]{(g-option-context-main-group context) => group}
  @syntax[]{(setf (g-option-context-main-group context) group)}
  @argument[context]{a @type{g-option-context} instance}
  @argument[group]{a @type{g-option-group} instance}
  @begin{short}
    The function @sym{g-option-context-main-group} returns the main group of
    @arg{context}.
  @end{short}
  The function @sym{(setf g-option-context-main-group)} sets a
  @type{g-option-group} instance as main group of the option context. This has
  the same effect as calling the function @fun{g-option-context-add-group}, the
  only difference is that the options in the main group are treated differently
  when generating @code{--help} output.
  @see-type{g-option-context}
  @see-type{g-option-group}
  @see-function{g-option-context-add-group}"
  (context (:pointer (:struct g-option-context))))

(export 'g-option-context-main-group)

;;; ----------------------------------------------------------------------------
;;; g_option_group_new ()
;;; ----------------------------------------------------------------------------

;; TODO: The arguments USER-DATA and DESTROY are not implemented.

(defcfun ("g_option_group_new" %g-option-group-new)
    (:pointer (:struct g-option-group))
  (name :string)
  (description :string)
  (help-description :string)
  (user-data :pointer)
  (destroy :pointer))

(defun g-option-group-new (name description help-description)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[name]{a string with the name for the option group, this is used to
    provide help for the options in this group with the @code{--help-name}
    option}
  @argument[description]{a string with a description for this group to be shown
    in @code{--help} output, this string is translated using the translation
    domain or translation function of the group}
  @argument[help-description]{a string with a description for the
    @code{--help-name} option, this string is translated using the translation
    domain or translation function of the group}
  @return{A newly created option group. It should be added to a
    @type{g-option-context} instance or freed with the function
    @fun{g-option-group-unref}.}
  @begin{short}
    Creates a new @type{g-option-group} instance.
  @end{short}
  @see-type{g-option-group}
  @see-type{g-option-context}
  @see-function{g-option-group-unref}"
  (%g-option-group-new (if name name (null-pointer))
                       (if description description (null-pointer))
                       (if help-description help-description (null-pointer))
                       (null-pointer)
                       (null-pointer)))

(export 'g-option-group-new)

;;; ----------------------------------------------------------------------------
;;; g_option_group_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_group_ref" g-option-group-ref)
    (:pointer (:struct g-option-group))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[group]{a @type{g-option-group} instance}
  @return{A @type{g-option-group} instance.}
  @begin{short}
    Increments the reference count of @arg{group} by one.
  @end{short}
  @see-type{g-option-group}"
  (group (:pointer (:struct g-option-group))))

(export 'g-option-group-ref)

;;; ----------------------------------------------------------------------------
;;; g_option_group_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_group_unref" g-option-group-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[group]{a @type{g-option-group} instance}
  @begin{short}
    Decrements the reference count of @arg{group} by one.
  @end{short}
  If the reference count drops to 0, the group will be freed and all memory
  allocated by the group is released.
  @see-type{g-option-group}"
  (group (:pointer (:struct g-option-group))))

(export 'g-option-group-unref)

;;; ----------------------------------------------------------------------------
;;; g_option_group_free ()
;;;
;;; void g_option_group_free (GOptionGroup *group);
;;;
;;; Warning
;;;
;;; g_option_group_free has been deprecated since version 2.44 and should not
;;; be used in newly written code. Use g_option_group_unref() instead.
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

(defcfun ("g_option_group_add_entries" %g-option-group-add-entries) :void
  (group (:pointer (:struct g-option-group)))
  (entries (:pointer (:pointer (:struct g-option-entry)))))

(defun g-option-group-add-entries (group entries)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[group]{a @type{g-option-group} instance}
  @argument[entries]{a list of option entries}
  @begin{short}
    Adds the options specified in @arg{entries} to @arg{group}.
  @end{short}

  The list of option entries has the following syntax. See the
  @type{g-option-context} documentation for a complete example.
  @begin{pre}
(let ((entries '((long-name
                  short-name
                  flags
                  arg
                  arg-data
                  description
                  arg-description)
                 (<next option>)
                 )))
   ...)
  @end{pre}
  @begin[code]{table}
    @entry[long-name]{A string with the long name of an option can be used to
      specify it in a command line as @code{--long-name}. Every option must
      have a long name. To resolve conflicts if multiple option groups contain
      the same long name, it is also possible to specify the option as
      @code{--groupname-long-name}.}
    @entry[short-name]{If an option has a short name, it can be specified
      @code{-short-name} in a command line. The argument @arg{short-name} must
      be a printable ASCII character different from '-', or @code{#\Nul} if the
      option has no short name.}
    @entry[flags]{Flags from the @symbol{g-option-flags} bitfield.}
    @entry[arg]{The type of the option, as a @symbol{g-option-arg} value.}
    @entry[arg-data]{If the @arg{arg} type is @code{:callback}, then
      @arg{arg-data} must point to a @code{GOptionArgFunc} callback function,
      which will be called to handle the extra argument. Otherwise,
      @arg{arg-data} is a pointer to a location to store the value, the required
      type of the location depends on the @arg{arg} type. If @arg{arg} type is
      @code{:string} or @code{:filename} the location will contain a newly
      allocated string if the option was given.}
    @entry[description]{A string with the description for the option in
      @code{--help} output. The description is translated using the
      @code{translate-func} of the group, see the function
      @fun{g-option-group-set-translation-domain}.}
    @entry[arg-description]{A string with the placeholder to use for the extra
      argument parsed by the option in @code{--help} output. The
      @arg{arg-description} is translated using the @code{translate-func} of the
      group, see the function @fun{g-option-group-set-translation-domain}.}
  @end{table}
  @see-type{g-option-group}
  @see-tyoe{g-option-context}
  @see-symbol{g-option-flags}
  @see-symbol{g-option-arg}
  @see-function{g-option-group-set-translation-domain}"
  (let ((n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-option-entry) i)
        do (with-foreign-slots ((long-name
                                 short-name
                                 flags
                                 arg
                                 arg-data
                                 description
                                 arg-description) entry-ptr
                                                  (:struct g-option-entry))
             (setf long-name (first entry)
                   short-name (if (second entry)
                                  (char-code (second entry))
                                  0)
                   flags (third entry)
                   arg (fourth entry)
                   ;; TODO: Check this. It is not correct?
                   arg-data (if (and (fifth entry)
                                     (member (fourth entry)
                                             '(:none :int :double :string
                                               :filename :string-array
                                               :filename-array :int64)))
                                (symbol-value (fifth entry))
                                (null-pointer))
                   description (sixth entry)
                   arg-description (if (seventh entry)
                                       (seventh entry)
                                       (null-pointer)))))
      ;; Set the fields of the last entry to NULL pointer or 0
      (let ((entry-ptr (mem-aptr entries-ptr
                                 '(:struct g-option-entry)
                                 n-entries)))
        (with-foreign-slots ((long-name
                              short-name
                              flags
                              arg
                              arg-data
                              description
                              arg-description) entry-ptr
                                               (:struct g-option-entry))
          (setf long-name (null-pointer))
          (setf short-name 0)
          (setf flags 0)
          (setf arg 0)
          (setf arg-data (null-pointer))
          (setf description (null-pointer))
          (setf arg-description (null-pointer)))
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_group_set_translate_func"
          %g-option-group-set-translate-func) :void
  (group (:pointer (:struct g-option-group)))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun g-option-group-set-translate-func (group func)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[group]{a @type{g-option-group} instance}
  @argument[func]{the @symbol{g-translate-func} callback function}
  @begin{short}
    Sets the function which is used to translate user visible strings, for
    @code{--help} output.
  @end{short}
  Different groups can use different @symbol{g-translate-func} functions. If
  @arg{func} is @code{nil}, strings are not translated.

  If you are using the function @code{gettext()}, you only need to set the
  translation domain, see the function
  @fun{g-option-group-set-translation-domain}.
  @see-type{g-option-group}
  @see-symbol{g-translate-func}
  @see-function{g-option-group-set-translation-domain}"
  (if func
      (%g-option-context-set-translate-func
                                      group
                                      (callback g-translate-func)
                                      (allocate-stable-pointer func)
                                      (callback stable-pointer-destroy-notify))
      (%g-option-context-set-translate-func group
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'g-option-group-set-translate-func)

;;; ----------------------------------------------------------------------------
;;; g_option_group_set_translation_domain ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_option_group_set_translation_domain"
           g-option-group-set-translation-domain) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[group]{a @type{g-option-group} instance}
  @argument[domain]{a string with the translation domain to use}
  @begin{short}
    A convenience function to use the function @code{gettext()} for translating
    user visible strings.
  @end{short}
  @see-type{g-option-group}"
  (group (:pointer (:struct g-option-group)))
  (domain :string))

(export 'g-option-group-set-translation-domain)

;;; --- End of file g.option-group.lisp ----------------------------------------

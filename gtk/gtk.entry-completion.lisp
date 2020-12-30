;;; ----------------------------------------------------------------------------
;;; gtk.entry-completion.lisp
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
;;; GtkEntryCompletion
;;;
;;;     Completion functionality for GtkEntry
;;;
;;; Synopsis
;;;
;;;     GtkEntryCompletion
;;;
;;; Functions
;;;
;;;     GtkEntryCompletionMatchFunc
;;;
;;;     gtk_entry_completion_new
;;;     gtk_entry_completion_new_with_area
;;;     gtk_entry_completion_get_entry
;;;     gtk_entry_completion_set_model
;;;     gtk_entry_completion_get_model
;;;     gtk_entry_completion_set_match_func
;;;     gtk_entry_completion_set_minimum_key_length
;;;     gtk_entry_completion_get_minimum_key_length
;;;     gtk_entry_completion_compute_prefix
;;;     gtk_entry_completion_complete
;;;     gtk_entry_completion_get_completion_prefix
;;;     gtk_entry_completion_insert_prefix
;;;     gtk_entry_completion_insert_action_text
;;;     gtk_entry_completion_insert_action_markup
;;;     gtk_entry_completion_delete_action
;;;     gtk_entry_completion_set_text_column
;;;     gtk_entry_completion_get_text_column
;;;     gtk_entry_completion_set_inline_completion
;;;     gtk_entry_completion_get_inline_completion
;;;     gtk_entry_completion_set_inline_selection
;;;     gtk_entry_completion_get_inline_selection
;;;     gtk_entry_completion_set_popup_completion
;;;     gtk_entry_completion_get_popup_completion
;;;     gtk_entry_completion_set_popup_set_width
;;;     gtk_entry_completion_get_popup_set_width
;;;     gtk_entry_completion_set_popup_single_match
;;;     gtk_entry_completion_get_popup_single_match
;;;
;;; Properties
;;;
;;;     GtkCellArea*   cell-area             Read / Write / Construct Only
;;;        gboolean    inline-completion     Read / Write
;;;        gboolean    inline-selection      Read / Write
;;;            gint    minimum-key-length    Read / Write
;;;    GtkTreeModel*   model                 Read / Write
;;;        gboolean    popup-completion      Read / Write
;;;        gboolean    popup-set-width       Read / Write
;;;        gboolean    popup-single-match    Read / Write
;;;            gint    text-column           Read / Write
;;;
;;; Signals
;;;
;;;            void    action-activated      Run Last
;;;        gboolean    cursor-on-match       Run Last
;;;        gboolean    insert-prefix         Run Last
;;;        gboolean    match-selected        Run Last
;;;            void    no-matches            Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEntryCompletion
;;;
;;; Implemented Interfaces
;;;
;;;     GtkEntryCompletion implements GtkCellLayout and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEntryCompletion
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEntryCompletion" gtk-entry-completion
  (:superclass g-object
   :export t
   :interfaces("GtkBuildable"
               "GtkCellLayout")
   :type-initializer "gtk_entry_completion_get_type")
  ((cell-area
    gtk-entry-completion-cell-area
    "cell-area" "GtkCellArea" t t)
   (inline-completion
    gtk-entry-completion-inline-completion
    "inline-completion" "gboolean" t t)
   (inline-selection
    gtk-entry-completion-inline-selection
    "inline-selection" "gboolean" t t)
   (minimum-key-length
    gtk-entry-completion-minimum-key-length
    "minimum-key-length" "gint" t t)
   (model
    gtk-entry-completion-model
    "model" "GtkTreeModel" t t)
   (popup-completion
    gtk-entry-completion-popup-completion
    "popup-completion" "gboolean" t t)
   (popup-set-width
    gtk-entry-completion-popup-set-width
    "popup-set-width" "gboolean" t t)
   (popup-single-match
    gtk-entry-completion-popup-single-match
    "popup-single-match" "gboolean" t t)
   (text-column
    gtk-entry-completion-text-column
    "text-column" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-entry-completion 'type)
 "@version{2020-5-31}
  @begin{short}
    @sym{gtk-entry-completion} is an auxiliary object to be used in
    conjunction with @class{gtk-entry} to provide the completion functionality.
    It implements the @class{gtk-cell-layout} interface, to allow the user to
    add extra cells to the @class{gtk-tree-view} with completion matches.
  @end{short}

  \"Completion functionality\" means that when the user modifies the text in the
  entry, @sym{gtk-entry-completion} checks which rows in the model match the
  current content of the entry, and displays a list of matches. By default, the
  matching is done by comparing the entry text case-insensitively against the
  text column of the model, see the function
  @fun{gtk-entry-completion-text-column}, but this can be overridden with a
  custom match function, see the function
  @fun{gtk-entry-completion-set-match-func}.

  When the user selects a completion, the content of the entry is updated. By
  default, the content of the entry is replaced by the text column of the
  model, but this can be overridden by connecting to the \"match-selected\"
  signal and updating the entry in the signal handler. Note that you should
  return @em{true} from the signal handler to suppress the default behaviour.

  To add completion functionality to an entry, use the function
  @fun{gtk-entry-completion}.

  In addition to regular completion matches, which will be inserted into the
  entry when they are selected, @sym{gtk-entry-completion} also allows to
  display \"actions\" in the popup window. Their appearance is similar to
  menu items, to differentiate them clearly from completion strings. When an
  action is selected, the \"action-activated\" signal is emitted.

  @sym{gtk-entry-completion} uses a @class{gtk-tree-model-filter} model to
  represent the subset of the entire model that is currently matching. While the
  @sym{gtk-entry-completion} signals \"match-selected\" and \"cursor-on-match\"
  take the original model and an iter pointing to that model as arguments, other
  callbacks and signals, such as @code{GtkCellLayoutDataFuncs} or
  \"apply-attributes\", will generally take the filter model as argument. As
  long as you are only calling the function @fun{gtk-tree-model-get}, this will
  make no difference to you. If for some reason, you need the original model,
  use the function @fun{gtk-tree-model-filter-model}. Do not forget to use
  the function @fun{gtk-tree-model-filter-convert-iter-to-child-iter} to
  obtain a matching iter.
  @begin[Signal Details]{dictionary}
    @subheading{The \"action-activated\" signal}
      @begin{pre}
 lambda (widget index)    : Run Last
      @end{pre}
      Gets emitted when an action is activated.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-entry-completion} object which received
          the signal.}
        @entry[index]{An integer with the index of the activated action.}
      @end{table}
    @subheading{The \"cursor-on-match\" signal}
      @begin{pre}
 lambda (widget model iter)    : Run Last
      @end{pre}
      Gets emitted when a match from the cursor is on a match of the list. The
      default behaviour is to replace the contents of the entry with the
      contents of the text column in the row pointed to by iter. Note that model
      is the model that was passed to the function
      @fun{gtk-entry-completion-model}.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-entry-completion} object which received
          the signal.}
        @entry[model]{The @class{gtk-tree-model} containing the matches.}
        @entry[iter]{A @class{gtk-tree-iter} positioned at the selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"insert-prefix\" signal}
      @begin{pre}
 lambda (widget prefix)    : Run Last
      @end{pre}
      Gets emitted when the inline autocompletion is triggered. The default
      behaviour is to make the entry display the whole prefix and select the
      newly inserted part. Applications may connect to this signal in order to
      insert only a smaller part of the prefix into the entry - e.g. the entry
      used in the @class{gtk-file-chooser} inserts only the part of the prefix
      up to the next '/'.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-entry-completion} object which received
          the signal.}
        @entry[prefix]{A string with the common prefix of all possible
          completions.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"match-selected\" signal}
      @begin{pre}
 lambda (widget model iter)    : Run Last
      @end{pre}
      Gets emitted when a match from the list is selected. The default behaviour
      is to replace the contents of the entry with the contents of the text
      column in the row pointed to by iter. Note that @arg{model} is the model
      that was passed to the function @fun{gtk-entry-completion-model}.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-entry-completion} object which received the
          signal.}
        @entry[model]{The @class{gtk-tree-model} containing the matches.}
        @entry[iter]{A @class{gtk-tree-iter} positioned at the selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"no-matches\" signal}
      @begin{pre}
 lambda (widget)    : Run Last
      @end{pre}
      Gets emitted when the entry completion is out of suggestions. Since 3.14
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-entry-completion} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-entry-completion-cell-area}
  @see-slot{gtk-entry-completion-inline-completion}
  @see-slot{gtk-entry-completion-inline-selection}
  @see-slot{gtk-entry-completion-minimum-key-length}
  @see-slot{gtk-entry-completion-model}
  @see-slot{gtk-entry-completion-popup-completion}
  @see-slot{gtk-entry-completion-popup-set-width}
  @see-slot{gtk-entry-completion-popup-single-match}
  @see-slot{gtk-entry-completion-text-column}
  @see-class{gtk-entry}
  @see-class{gtk-cell-layout}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model-filter}
  @see-function{gtk-entry-completion-set-match-func}
  @see-function{gtk-entry-completion}
  @see-function{gtk-tree-model-get}
  @see-function{gtk-tree-model-filter-model}
  @see-function{gtk-tree-model-filter-convert-iter-to-child-iter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-entry-completion-cell-area -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area"
                                               'gtk-entry-completion) 't)
 "The @code{cell-area} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers in the treeview column. If no area
  is specified when creating the entry completion with the function
  @fun{gtk-entry-completion-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} will be used.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-cell-area 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-cell-area object) => cell-area}
  @syntax[]{(setf (gtk-entry-completion-cell-area object) cell-area)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[cell-area]{a @class{gtk-cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{cell-area} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The cell area used to layout cell renderers in the treeview column. If no area
  is specified when creating the entry completion with the function
  @fun{gtk-entry-completion-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} will be used.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-inline-completion ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inline-completion"
                                               'gtk-entry-completion) 't)
 "The @code{inline-completion} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the common prefix of the possible completions should be
  inserted automatically in the entry. Note that this requires the
  @code{text-column} property to be set, even if you are using a custom
  match function. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-inline-completion
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-inline-completion 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-inline-completion object)
            => inline-completion}
  @syntax[]{(setf (gtk-entry-completion-inline-completion object)
                  inline-completion)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[inline-completion]{@em{true} to do inline completion}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{inline-completion} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-inline-completion} returns
  whether the common prefix of the possible completions should be automatically
  inserted in the entry. The slot access function
  @sym{(setf gtk-entry-completion-inline-completion)} sets whether the common
  prefix of the possible completions should be automatically inserted in the
  entry.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-inline-selection ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inline-selection"
                                               'gtk-entry-completion) 't)
 "The @code{inline-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the possible completions on the popup will appear in the
  entry as you navigate through them. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-inline-selection
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-inline-selection 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-inline-selection object)
            => inline-selection}
  @syntax[]{(setf (gtk-entry-completion-inline-selection object)
                  inline-selection)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[inline-selection]{@em{true} to do inline selection}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{inline-selection} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-inline-selection} returns
  @em{true} if inline selection mode is turned on. The slot access function
  @sym{(setf gtk-entry-completion-inline-selection)} sets whether it is possible
  to cycle through the possible completions inside the entry.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-minimum-key-length --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "minimum-key-length"
                                               'gtk-entry-completion) 't)
 "The @code{minimum-key-length} property of type @code{:int} (Read / Write)
  @br{}
  Minimum length of the search key in order to look up matches. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-minimum-key-length
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-minimum-key-length 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-minimum-key-length object) => length}
  @syntax[]{(setf (gtk-entry-completion-minimum-key-length object) length)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[length]{an integer with the minimum length of the key in order to
    start completing}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{minimum-key-length} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion} returns the minimum key
  length as set for the entry completion. The slot access function
  @sym{(setf gtk-entry-completion-minimum-key-length)} sets the length of the
  search key for completion to be at least @arg{length}.

  This is useful for long lists, where completing using a small key takes a
  lot of time and will come up with meaningless results anyway, i.e., a too
  large dataset.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-model ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-entry-completion) 't)
 "The @code{model} property of type @class{gtk-tree-model} (Read / Write) @br{}
  The model to find matches in.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-model 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-model object) => model}
  @syntax[]{(setf (gtk-entry-completion-model object) model)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[model]{the @class{gtk-tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{model} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-model} returns the
  @class{gtk-tree-model}, or @code{nil} if none is currently being used. The
  slot access function @sym{(setf gtk-entry-completion-model)} sets the model
  for a entry completion. If the entry completion already has a model set, it
  will remove it before setting the new model. If @arg{model} is @code{nil},
  then it will unset the model.
  @see-class{gtk-entry-completion}
  @see-class{gtk-tree-model}")

;;; --- gtk-entry-completion-popup-completion ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-completion"
                                               'gtk-entry-completion) 't)
 "The @code{popup-completion} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the possible completions should be shown in a popup
  window. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-completion
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-completion 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-popup object) => popup-completion}
  @syntax[]{(setf (gtk-entry-completion-popup object) popup-completion)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[popup-completion]{@em{true} to do popup completion}
  @begin{short}
    Accessor of the @slot[gtk-entry-completition]{popup-completion} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-popup-completion} returns
  whether the completions should be presented in a popup window. The slot access
  function @sym{(setf gtk-entry-completion-popup-completion)} sets whether the
  completions should be presented in a popup window.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-popup-set-width -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-set-width"
                                               'gtk-entry-completion) 't)
 "The @code{popup-set-width} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the completions popup window will be resized to the width
  of the entry. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-set-width
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-set-width 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-popup-set-width object) => popup-set-width}
  @syntax[]{(setf (gtk-entry-completion-popup-set-width object) popup-set-width)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[popup-set-width]{@em{true} to make the width of the popup the same
    as the entry}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{popup-set-width} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-popup-set-width} returns
  whether the completion popup window will be resized to the width of the entry.
  The slot access function @sym{(setf gtk-entry-completion-popup-set-width)}
  sets whether the completion popup window will be resized to be the same width
  as the entry.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-popup-single-match --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-single-match"
                                               'gtk-entry-completion) 't)
 "The @code{popup-single-match} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the completions popup window will shown for a single
  possible completion. You probably want to set this to @em{false} if you are
  using inline completion. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-single-match
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-single-match 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-popup-single-match object) => single-match}
  @syntax[]{(setf (gtk-entry-completion-popup-single-match object) single-match)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[single-match]{@em{true} if the popup should appear even for a single
    match}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{popup-single-match} slot
    of the @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-popup-single-match} returns
  @em{true} if the popup window will appear regardless of the number of matches.
  The slot access function @sym{(setf gtk-entry-completion-popup-single-match)}
  sets whether the completion popup window will appear even if there is only a
  single match.

  You may want to set this to @em{false} if you are using inline completion.
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-completion-text-column ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-column"
                                               'gtk-entry-completion) 't)
 "The @code{text-column} property of type @code{:int} (Read / Write) @br{}
  The column of the model containing the strings. Note that the strings must
  be UTF-8. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-text-column 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-entry-completion-text-column) => column}
  @syntax[]{(setf (gtk-entry-completion-text-column object) column)}
  @argument[object]{a @class{gtk-entry-completion} object}
  @argument[column]{an integer with the column in the model of the completion
    to get strings from}
  @begin{short}
    Accessor of the @slot[gtk-entry-completion]{text-column} slot of the
    @class{gtk-entry-completion} class.
  @end{short}

  The slot access function @sym{gtk-entry-completion-text-column} returns the
  column in the model of the completion to get strings from. The slot access
  function @sym{(setf gtk-entry-completion-text-column)} is a convenience
  function for setting up the most used case: a completion list with just
  strings.

  This function will set up completion to have a list displaying all, and just,
  strings in the completion list, and to get those strings from column in the
  model of completion.

  This functions creates and adds a @class{gtk-cell-renderer-text} for the
  selected column. If you need to set the text column, but do not want the cell
  renderer, use the function @fun{g-object-property} to set the
  @slot[gtk-entry-completion]{text-column} property directly.
  @see-class{gtk-entry-completion}
  @see-class{gtk-cell-renderer-text}
  @see-function{g-object-property}
  @see-function{gtk-entry-completion-inline-completion}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-completion-new))

(defun gtk-entry-completion-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @return{A newly created @class{gtk-entry-completion} object.}
  @short{Creates a new entry completion.}
  @see-class{gtk-entry-completion}
  @see-function{gtk-entry-completion-new-with-area}"
  (make-instance 'gtk-entry-completion))

(export 'gtk-entry-completion-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-completion-new-with-area))

(defun gtk-entry-completion-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[area]{the @class{gtk-cell-area} used to layout cells}
  @return{A newly created @class{gtk-entry-completion} object.}
  @begin{short}
    Creates a new entry completion using the specified area to layout cells in
    the underlying @class{gtk-tree-view-column} for the drop-down menu.
  @end{short}
  @see-class{gtk-entry-completion}
  @see-class{gtk-cell-area}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-entry-completion-new}"
  (make-instance 'gtk-entry-completion
                 :cell-area area))

(export 'gtk-entry-completion-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_entry () -> gtk-entry-completion-entry
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_entry" gtk-entry-completion-entry)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @return{The @class{gtk-widget} entry the entry completion has been attached
    to.}
  @short{Gets the entry the entry completion has been attached to.}
  @see-class{gtk-entry-completion}
  @see-class{gtk-entry}"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-entry)

;;; ----------------------------------------------------------------------------
;;; GtkEntryCompletionMatchFunc ()
;;;
;;; gboolean (*GtkEntryCompletionMatchFunc) (GtkEntryCompletion *completion,
;;;                                          const gchar *key,
;;;                                          GtkTreeIter *iter,
;;;                                          gpointer user_data);
;;;
;;; A function which decides whether the row indicated by iter matches a given
;;; key, and should be displayed as a possible completion for key. Note that key
;;; is normalized and case-folded (see g_utf8_normalize() and
;;; g_utf8_casefold()). If this is not appropriate, match functions have access
;;; to the unmodified key via gtk_entry_get_text
;;; (GTK_ENTRY (gtk_entry_completion_get_entry ())).
;;;
;;; completion :
;;;     the GtkEntryCompletion
;;;
;;; key :
;;;     the string to match, normalized and case-folded
;;;
;;; iter :
;;;     a GtkTreeIter indicating the row to match
;;;
;;; user_data :
;;;     user data given to gtk_entry_completion_set_match_func()
;;;
;;; Returns :
;;;     TRUE if iter should be displayed as a possible completion for key
;;; ----------------------------------------------------------------------------

(define-cb-methods gtk-entry-completion-match-func :boolean
  ((completion (g-object gtk-entry-completion))
   (key :string)
   (iter (g-boxed-foreign gtk-tree-iter))))

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_match_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_set_match_func"
          %gtk-entry-completion-set-match-func) :void
  (completion (g-object gtk-entry-completion))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-entry-completion-set-match-func (completion func)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[func]{the @code{GtkEntryCompletionMatchFunc} to use}
  @begin{short}
    Sets the match function for the entry completion to be @arg{func}.
  @end{short}
  The match function is used to determine if a row should or should not be in
  the completion list.
  @see-class{gtk-entry-completion}"
  (if func
      (%gtk-entry-completion-set-match-func
                          completion
                          (callback gtk-entry-completion-match-func-cb)
                          (create-fn-ref completion func)
                          (callback entry-completion-match-func-destroy-notify))
      (%gtk-entry-completion-set-match-func completion
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'gtk-entry-completion-set-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_compute_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_compute_prefix"
           gtk-entry-completion-compute-prefix) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[key]{a string with the text to complete for}
  @begin{return}
    The common prefix all rows starting with key or @code{nil} if no row matches
    key.
  @end{return}
  @begin{short}
    Computes the common prefix that is shared by all rows in completion that
    start with @arg{key}.
  @end{short}
  If no row matches @arg{key}, @code{nil} will be returned. Note that a text
  column must have been set for this function to work, see the function
  @fun{gtk-entry-completion-text-column} for details.
  @see-class{gtk-entry-completion}
  @see-function{gtk-entry-completion-text-column}"
  (completion (g-object gtk-entry-completion))
  (key :string))

(export 'gtk-entry-completion-compute-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_complete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_complete" gtk-entry-completion-complete) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @begin{short}
    Requests a completion operation, or in other words a refiltering of the
    current list with completions, using the current key.
  @end{short}
  The completion list view will be updated accordingly.
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_completion_prefix ()
;;; -> gtk-entry-completion-completion-prefix
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_completion_prefix"
           gtk-entry-completion-completion-prefix)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @return{A string with the prefix for the current completion.}
  @begin{short}
    Get the original text entered by the user that triggered the completion or
    @code{nil} if there is no completion ongoing.
  @end{short}
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_prefix"
          gtk-entry-completion-insert-prefix) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @begin{short}
    Requests a prefix insertion.
  @end{short}
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-insert-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_action_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_action_text"
          gtk-entry-completion-insert-action-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{an integer with the index of the item to insert}
  @argument[text]{a string with the text of the item to insert}
  @begin{short}
    Inserts an action in the entry completion's action item list at position
    @arg{index} with the given text.
  @end{short}
  If you want the action item to have markup, use the function
  @fun{gtk-entry-completion-insert-action-markup}.
  @see-class{gtk-entry-completion}
  @see-function{gtk-entry-completion-insert-action-markup}"
  (completion (g-object gtk-entry-completion))
  (index :int)
  (text :string))

(export 'gtk-entry-completion-insert-action-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_action_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_action_markup"
           gtk-entry-completion-insert-action-markup) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{an integer with the index of the item to insert}
  @argument[markup]{a string with the markup of the item to insert}
  @begin{short}
    Inserts an action in the entry completion's action item list at position
    @arg{index} with the given markup.
  @end{short}
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion))
  (index :int)
  (markup :string))

(export 'gtk-entry-completion-insert-action-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_delete_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_delete_action"
          gtk-entry-completion-delete-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{an integer with the index of the item to delete}
  @begin{short}
    Deletes the action at @arg{index} from the entry completion's action list.
  @end{short}
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion))
  (index :int))

(export 'gtk-entry-completion-delete-action)

;;; --- End of file gtk.entry-completion.lisp ----------------------------------

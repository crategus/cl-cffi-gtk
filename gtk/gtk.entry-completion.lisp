;;; ----------------------------------------------------------------------------
;;; gtk.entry-completion.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkEntryCompletion
;;;
;;; Completion functionality for GtkEntry
;;;
;;; Synopsis
;;;
;;;     GtkEntryCompletion
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-entry-completion 'type)
 "@version{2013-4-28}
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
  text column of the model (see the function
  @fun{gtk-entry-completion-set-text-column}), but this can be overridden with a
  custom match function (see the function
  @fun{gtk-entry-completion-set-match-func}).

  When the user selects a completion, the content of the entry is updated. By
  default, the content of the entry is replaced by the text column of the
  model, but this can be overridden by connecting to the \"match-selected\"
  signal and updating the entry in the signal handler. Note that you should
  return @em{true} from the signal handler to suppress the default behaviour.

  To add completion functionality to an entry, use the function
  @fun{gtk-entry-set-completion}.

  In addition to regular completion matches, which will be inserted into the
  entry when they are selected, @sym{gtk-entry-completion} also allows to
  display \"actions\" in the popup window. Their appearance is similar to
  menu items, to differentiate them clearly from completion strings. When an
  action is selected, the \"action-activated\" signal is emitted.

  @sym{gtk-entry-completion} uses a @class{gtk-tree-model-filter} model to
  represent the subset of the entire model that is currently matching. While the
  @sym{gtk-entry-completion} signals \"match-selected\" and \"cursor-on-match\"
  take the original model and an iter pointing to that model as arguments, other
  callbacks and signals (such as @code{GtkCellLayoutDataFuncs} or
  \"apply-attributes\") will generally take the filter model as argument. As
  long as you are only calling the function @fun{gtk-tree-model-get}, this will
  make no difference to you. If for some reason, you need the original model,
  use the function @fun{gtk-tree-model-filter-get-model}. Do not forget to use
  the function @fun{gtk-tree-model-filter-convert-iter-to-child-iter} to
  obtain a matching iter.
  @begin[Signal Details]{dictionary}
    @subheading{The \"action-activated\" signal}
      @begin{pre}
 lambda (widget index)    : Run Last
      @end{pre}
      Gets emitted when an action is activated.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
        @entry[index]{The index of the activated action.}
      @end{table}
      Since 2.4

    @subheading{The \"cursor-on-match\" signal}
      @begin{pre}
 lambda (widget model iter)   : Run Last
      @end{pre}
      Gets emitted when a match from the cursor is on a match of the list. The
      default behaviour is to replace the contents of the entry with the
      contents of the text column in the row pointed to by iter.
      Note that model is the model that was passed to the function
      @fun{gtk-entry-completion-set-model}.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
        @entry[model]{The @class{gtk-tree-model} containing the matches.}
        @entry[iter]{A @class{gtk-tree-iter} positioned at the selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
      Since 2.12

    @subheading{The \"insert-prefix\" signal}
      @begin{pre}
 lambda (widget prefix)   : Run Last
      @end{pre}
      Gets emitted when the inline autocompletion is triggered. The default
      behaviour is to make the entry display the whole prefix and select the
      newly inserted part.
      Applications may connect to this signal in order to insert only a smaller
      part of the prefix into the entry - e. g. the entry used in the
      @class{gtk-file-chooser} inserts only the part of the prefix up to the
      next '/'.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
        @entry[prefix]{The common prefix of all possible completions.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
      Since 2.6

    @subheading{The \"match-selected\" signal}
      @begin{pre}
 lambda (widget model  iter)   : Run Last
      @end{pre}
      Gets emitted when a match from the list is selected. The default behaviour
      is to replace the contents of the entry with the contents of the text
      column in the row pointed to by iter.
      Note that model is the model that was passed to the function
      @fun{gtk-entry-completion-set-model}.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
        @entry[model]{The @class{gtk-tree-model} containing the matches.}
        @entry[iter]{A @class{gtk-tree-iter} positioned at the selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
      Since 2.4
  @end{dictionary}
  @see-slot{gtk-entry-completion-cell-area}
  @see-slot{gtk-entry-completion-inline-completion}
  @see-slot{gtk-entry-completion-inline-selection}
  @see-slot{gtk-entry-completion-minimum-key-length}
  @see-slot{gtk-entry-completion-model}
  @see-slot{gtk-entry-completion-popup-completion}
  @see-slot{gtk-entry-completion-popup-set-width}
  @see-slot{gtk-entry-completion-popup-single-match}
  @see-slot{gtk-entry-completion-text-column}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area"
                                               'gtk-entry-completion) 't)
 "The @code{\"cell-area\"} property of type @class{gtk-cell-area}
  (Read / Write / Construct)@br{}
  The @class{gtk-cell-area} used to layout cell renderers in the treeview
  column.
  If no area is specified when creating the entry completion with the function
  @fun{gtk-entry-completion-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} will be used.@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inline-completion"
                                               'gtk-entry-completion) 't)
 "The @code{\"inline-completion\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Determines whether the common prefix of the possible completions should be
  inserted automatically in the entry. Note that this requires the
  @code{\"text-column\"} property to be set, even if you are using a custom
  match function.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inline-selection"
                                               'gtk-entry-completion) 't)
 "The @code{\"inline-selection\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Determines whether the possible completions on the popup will appear in the
  entry as you navigate through them.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "minimum-key-length"
                                               'gtk-entry-completion) 't)
 "The @code{\"minimum-key-length\"} property of type @code{:int}
  (Read / Write)@br{}
  Minimum length of the search key in order to look up matches.@br{}
  Allowed values: >= 0@br{}
  Default value: 1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-entry-completion) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write)@br{}
  The model to find matches in.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-completion"
                                               'gtk-entry-completion) 't)
 "The @code{\"popup-completion\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Determines whether the possible completions should be shown in a popup
  window.@br{}
  Default value: @em{true}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-set-width"
                                               'gtk-entry-completion) 't)
 "The @code{\"popup-set-width\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Determines whether the completions popup window will be resized to the width
  of the entry.@br{}
  Default value: @em{true}@br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-single-match"
                                               'gtk-entry-completion) 't)
 "The @code{\"popup-single-match\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Determines whether the completions popup window will shown for a single
  possible completion. You probably want to set this to @code{nil} if you are
  using inline completion.@br{}
  Default value: @em{true}@br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-column"
                                               'gtk-entry-completion) 't)
 "The @code{\"text-column\"} property of type @code{:int} (Read / Write)@br{}
  The column of the model containing the strings. Note that the strings must
  be UTF-8.@br{}
  Allowed values: >= @code{G_MAXULONG}@br{}
  Default value: -1@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-cell-area 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"cell-area\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-inline-completion atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-inline-completion 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"inline-completion\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-inline-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-inline-selection 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"inline-selection\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-minimum-key-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-minimum-key-length 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"minimum-key-length\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-model 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"model\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-completion atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-completion 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"popup-completion\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-set-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-set-width 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"popup-set-width\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-popup-single-match atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-popup-single-match 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"popup-single-match\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion-text-column 'function)
 "@version{2013-3-1}
  @begin{short}
    Accessor of the slot @code{\"text-column\"} of the
    @class{gtk-entry-completion} class.
  @end{short}")

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

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new ()
;;;
;;; GtkEntryCompletion * gtk_entry_completion_new (void);
;;;
;;; Creates a new GtkEntryCompletion object.
;;;
;;; Returns :
;;;     A newly created GtkEntryCompletion object
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new_with_area ()
;;;
;;; GtkEntryCompletion * gtk_entry_completion_new_with_area (GtkCellArea *area);
;;;
;;; Creates a new GtkEntryCompletion object using the specified area to layout
;;; cells in the underlying GtkTreeViewColumn for the drop-down menu.
;;;
;;; area :
;;;     the GtkCellArea used to layout cells
;;;
;;; Returns :
;;;     A newly created GtkEntryCompletion object
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_entry ()
;;;
;;; GtkWidget * gtk_entry_completion_get_entry (GtkEntryCompletion *completion);
;;;
;;; Gets the entry completion has been attached to.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     The entry completion has been attached to. [transfer none]
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_model ()
;;;
;;; void gtk_entry_completion_set_model (GtkEntryCompletion *completion,
;;;                                      GtkTreeModel *model);
;;;
;;; Sets the model for a GtkEntryCompletion. If completion already has a model
;;; set, it will remove it before setting the new model. If model is NULL, then
;;; it will unset the model.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; model :
;;;     the GtkTreeModel
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_model ()
;;;
;;; GtkTreeModel * gtk_entry_completion_get_model
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns NULL if the model is unset.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     A GtkTreeModel, or NULL if none is currently being used. [transfer none]
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_match_func ()
;;; ----------------------------------------------------------------------------

(define-cb-methods entry-completion-match-func :boolean
  ((completion (g-object gtk-entry-completion))
   (key :string)
   (iter (g-boxed-foreign gtk-tree-iter))))

(defcfun ("gtk_entry_completion_set_match_func"
          %gtk-entry-completion-set-match-func) :void
  (completion (g-object gtk-entry-completion))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-entry-completion-set-match-func (completion func)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[func]{the @code{GtkEntryCompletionMatchFunc} to use}
  @begin{short}
    Sets the match function for @arg{completion} to be @arg{func}.
  @end{short}
  The match function is used to determine if a row should or should not be in
  the completion list.

  Since 2.4"
  (if func
      (%gtk-entry-completion-set-match-func
                          completion
                          (callback entry-completion-match-func-cb)
                          (create-fn-ref completion func)
                          (callback entry-completion-match-func-destroy-notify))
      (%gtk-entry-completion-set-match-func completion
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(export 'gtk-entry-completion-set-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_minimum_key_length ()
;;;
;;; void gtk_entry_completion_set_minimum_key_length
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gint length);
;;;
;;; Requires the length of the search key for completion to be at least length.
;;; This is useful for long lists, where completing using a small key takes a
;;; lot of time and will come up with meaningless results anyway (ie, a too
;;; large dataset).
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; length :
;;;     the minimum length of the key in order to start completing
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_minimum_key_length ()
;;;
;;; gint gtk_entry_completion_get_minimum_key_length
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns the minimum key length as set for completion.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     The currently used minimum key length
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_compute_prefix ()
;;;
;;; gchar * gtk_entry_completion_compute_prefix (GtkEntryCompletion *completion,
;;;                                              const char *key);
;;;
;;; Computes the common prefix that is shared by all rows in completion that
;;; start with key. If no row matches key, NULL will be returned. Note that a
;;; text column must have been set for this function to work, see
;;; gtk_entry_completion_set_text_column() for details.
;;;
;;; completion :
;;;     the entry completion
;;;
;;; key :
;;;     The text to complete for
;;;
;;; Returns :
;;;     The common prefix all rows starting with key or NULL if no row matches
;;;     key.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_complete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_complete" gtk-entry-completion-complete) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-1}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @begin{short}
    Requests a completion operation, or in other words a refiltering of the
    current list with completions, using the current key. The completion list
    view will be updated accordingly.
  @end{short}

  Since 2.4"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_completion_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_completion_prefix"
           gtk-entry-completion-get-completion-prefix)
    (:string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-27}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @return{The prefix for the current completion.}
  @begin{short}
    Get the original text entered by the user that triggered the completion or
    @code{nil} if there is no completion ongoing.
  @end{short}

  Since 2.12
  @see-class{gtk-entry-completion}"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-get-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_prefix"
          gtk-entry-completion-insert-prefix) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @begin{short}
    Requests a prefix insertion.
  @end{short}

  Since 2.6"
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-insert-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_action_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_action_text"
          gtk-entry-completion-insert-action-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{the index of the item to insert}
  @argument[text]{text of the item to insert}
  @begin{short}
    Inserts an action in @arg{completion}'s action item list at position
    @arg{index} with text @arg{text}. If you want the action item to have
    markup, use the function @fun{gtk-entry-completion-insert-action-markup}.
  @end{short}

  Since 2.4
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
 "@version{2013-6-4}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{the index of the item to insert}
  @argument[markup]{markup of the item to insert}
  @begin{short}
    Inserts an action in @arg{completion}'s action item list at position
    @arg{index} with markup @arg{markup}.
  @end{short}

  Since 2.4"
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
 "@version{2013-4-28}
  @argument[completion]{a @class{gtk-entry-completion} object}
  @argument[index]{the index of the item to delete}
  @begin{short}
    Deletes the action at @arg{index} from @arg{completion}'s action list.
  @end{short}

  Since 2.4"
  (completion (g-object gtk-entry-completion))
  (index :int))

(export 'gtk-entry-completion-delete-action)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_text_column ()
;;;
;;; void gtk_entry_completion_set_text_column (GtkEntryCompletion *completion,
;;;                                            gint column);
;;;
;;; Convenience function for setting up the most used case of this code: a
;;; completion list with just strings. This function will set up completion to
;;; have a list displaying all (and just) strings in the completion list, and to
;;; get those strings from column in the model of completion.
;;;
;;; This functions creates and adds a GtkCellRendererText for the selected
;;; column. If you need to set the text column, but don't want the cell
;;; renderer, use g_object_set() to set the "text-column" property directly.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; column :
;;;     the column in the model of completion to get strings from
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_text_column ()
;;;
;;; gint gtk_entry_completion_get_text_column (GtkEntryCompletion *completion);
;;;
;;; Returns the column in the model of completion to get strings from.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     the column containing the strings
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_inline_completion ()
;;;
;;; void gtk_entry_completion_set_inline_completion
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gboolean inline_completion);
;;;
;;; Sets whether the common prefix of the possible completions should be
;;; automatically inserted in the entry.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; inline_completion :
;;;     TRUE to do inline completion
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_inline_completion ()
;;;
;;; gboolean gtk_entry_completion_get_inline_completion
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns whether the common prefix of the possible completions should be
;;; automatically inserted in the entry.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     TRUE if inline completion is turned on
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_inline_selection ()
;;;
;;; void gtk_entry_completion_set_inline_selection
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gboolean inline_selection);
;;;
;;; Sets whether it is possible to cycle through the possible completions inside
;;; the entry.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; inline_selection :
;;;     TRUE to do inline selection
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_inline_selection ()
;;;
;;; gboolean gtk_entry_completion_get_inline_selection
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns TRUE if inline-selection mode is turned on.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     TRUE if inline-selection mode is on
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_popup_completion ()
;;;
;;; void gtk_entry_completion_set_popup_completion
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gboolean popup_completion);
;;;
;;; Sets whether the completions should be presented in a popup window.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; popup_completion :
;;;     TRUE to do popup completion
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_popup_completion ()
;;;
;;; gboolean gtk_entry_completion_get_popup_completion
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns whether the completions should be presented in a popup window.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     TRUE if popup completion is turned on
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_popup_set_width ()
;;;
;;; void gtk_entry_completion_set_popup_set_width
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gboolean popup_set_width);
;;;
;;; Sets whether the completion popup window will be resized to be the same
;;; width as the entry.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; popup_set_width :
;;;     TRUE to make the width of the popup the same as the entry
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_popup_set_width ()
;;;
;;; gboolean gtk_entry_completion_get_popup_set_width
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns whether the completion popup window will be resized to the width of
;;; the entry.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     TRUE if the popup window will be resized to the width of the entry
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_popup_single_match ()
;;;
;;; void gtk_entry_completion_set_popup_single_match
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gboolean popup_single_match);
;;;
;;; Sets whether the completion popup window will appear even if there is only a
;;; single match. You may want to set this to FALSE if you are using inline
;;; completion.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; popup_single_match :
;;;     TRUE if the popup should appear even for a single match
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_popup_single_match ()
;;;
;;; gboolean gtk_entry_completion_get_popup_single_match
;;;                                            (GtkEntryCompletion *completion);
;;;
;;; Returns whether the completion popup window will appear even if there is
;;; only a single match.
;;;
;;; completion :
;;;     a GtkEntryCompletion
;;;
;;; Returns :
;;;     TRUE if the popup window will appear regardless of the number of matches
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.entry-completion.lisp ----------------------------------

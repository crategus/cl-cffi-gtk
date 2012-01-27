;;; ----------------------------------------------------------------------------
;;; gtk.entry-completion.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkEntryCompletion
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkEntryCompletion implements GtkCellLayout and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;;   "inline-completion"        gboolean             : Read / Write
;;;   "inline-selection"         gboolean             : Read / Write
;;;   "minimum-key-length"       gint                 : Read / Write
;;;   "model"                    GtkTreeModel*        : Read / Write
;;;   "popup-completion"         gboolean             : Read / Write
;;;   "popup-set-width"          gboolean             : Read / Write
;;;   "popup-single-match"       gboolean             : Read / Write
;;;   "text-column"              gint                 : Read / Write
;;; 
;;; Signals
;;; 
;;;   "action-activated"                              : Run Last
;;;   "cursor-on-match"                               : Run Last
;;;   "insert-prefix"                                 : Run Last
;;;   "match-selected"                                : Run Last
;;; 
;;; Description
;;; 
;;; GtkEntryCompletion is an auxiliary object to be used in conjunction with
;;; GtkEntry to provide the completion functionality. It implements the
;;; GtkCellLayout interface, to allow the user to add extra cells to the
;;; GtkTreeView with completion matches.
;;; 
;;; "Completion functionality" means that when the user modifies the text in the
;;; entry, GtkEntryCompletion checks which rows in the model match the current
;;; content of the entry, and displays a list of matches. By default, the
;;; matching is done by comparing the entry text case-insensitively against the
;;; text column of the model (see gtk_entry_completion_set_text_column()), but
;;; this can be overridden with a custom match function (see
;;; gtk_entry_completion_set_match_func()).
;;; 
;;; When the user selects a completion, the content of the entry is updated. By
;;; default, the content of the entry is replaced by the text column of the
;;; model, but this can be overridden by connecting to the "match-selected"
;;; signal and updating the entry in the signal handler. Note that you should
;;; return TRUE from the signal handler to suppress the default behaviour.
;;; 
;;; To add completion functionality to an entry, use gtk_entry_set_completion().
;;; 
;;; In addition to regular completion matches, which will be inserted into the
;;; entry when they are selected, GtkEntryCompletion also allows to display
;;; "actions" in the popup window. Their appearance is similar to menuitems, to
;;; differentiate them clearly from completion strings. When an action is
;;; selected, the "action-activated" signal is emitted.
;;; 
;;; GtkEntryCompletion uses a GtkTreeModelFilter model to represent the subset
;;; of the entire model that is currently matching. While the GtkEntryCompletion
;;; signals "match-selected" and "cursor-on-match" take the original model and
;;; an iter pointing to that model as arguments, other callbacks and signals
;;; (such as GtkCellLayoutDataFuncs or "apply-attributes") will generally take
;;; the filter model as argument. As long as you are only calling
;;; gtk_tree_model_get(), this will make no difference to you. If for some
;;; reason, you need the original model, use gtk_tree_model_filter_get_model().
;;; Don't forget to use gtk_tree_model_filter_convert_iter_to_child_iter() to
;;; obtain a matching iter.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area" property
;;; 
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;; 
;;; The GtkCellArea used to layout cell renderers in the treeview column.
;;; 
;;; If no area is specified when creating the entry completion with 
;;; gtk_entry_completion_new_with_area() a horizontally oriented GtkCellAreaBox
;;; will be used.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "inline-completion" property
;;; 
;;;   "inline-completion"        gboolean              : Read / Write
;;; 
;;; Determines whether the common prefix of the possible completions should be 
;;; inserted automatically in the entry. Note that this requires text-column to
;;; be set, even if you are using a custom match function.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "inline-selection" property
;;; 
;;;   "inline-selection"         gboolean              : Read / Write
;;; 
;;; Determines whether the possible completions on the popup will appear in 
;;; the entry as you navigate through them.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "minimum-key-length" property
;;; 
;;;   "minimum-key-length"       gint                  : Read / Write
;;; 
;;; Minimum length of the search key in order to look up matches.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;; 
;;;   "model"                    GtkTreeModel*         : Read / Write
;;; 
;;; The model to find matches in.
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-completion" property
;;; 
;;;   "popup-completion"         gboolean              : Read / Write
;;; 
;;; Determines whether the possible completions should be shown in a popup 
;;; window.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-set-width" property
;;; 
;;;   "popup-set-width"          gboolean              : Read / Write
;;; 
;;; Determines whether the completions popup window will be resized to the 
;;; width of the entry.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-single-match" property
;;; 
;;;   "popup-single-match"       gboolean              : Read / Write
;;; 
;;; Determines whether the completions popup window will shown for a single 
;;; possible completion. You probably want to set this to FALSE if you are
;;; using inline completion.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-column" property
;;; 
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; The column of the model containing the strings. Note that the strings must 
;;; be UTF-8.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-activated" signal
;;; 
;;; void user_function (GtkEntryCompletion *widget,
;;;                     gint                index,
;;;                     gpointer            user_data)      : Run Last
;;; 
;;; Gets emitted when an action is activated.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; index :
;;;     the index of the activated action
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-on-match" signal
;;; 
;;; gboolean user_function (GtkEntryCompletion *widget,
;;;                         GtkTreeModel       *model,
;;;                         GtkTreeIter        *iter,
;;;                         gpointer            user_data)      : Run Last
;;; 
;;; Gets emitted when a match from the cursor is on a match of the list. The 
;;; default behaviour is to replace the contents of the entry with the contents
;;; of the text column in the row pointed to by iter.
;;; 
;;; Note that model is the model that was passed to 
;;; gtk_entry_completion_set_model().
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; model :
;;;     the GtkTreeModel containing the matches
;;; 
;;; iter :
;;;     a GtkTreeIter positioned at the selected match
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the signal has been handled
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "insert-prefix" signal
;;; 
;;; gboolean user_function (GtkEntryCompletion *widget,
;;;                         gchar              *prefix,
;;;                         gpointer            user_data)      : Run Last
;;; 
;;; Gets emitted when the inline autocompletion is triggered. The default 
;;; behaviour is to make the entry display the whole prefix and select the
;;; newly inserted part.
;;; 
;;; Applications may connect to this signal in order to insert only a smaller
;;; part of the prefix into the entry - e.g. the entry used in the
;;; GtkFileChooser inserts only the part of the prefix up to the next '/'.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; prefix :
;;;     the common prefix of all possible completions
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the signal has been handled
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "match-selected" signal
;;; 
;;; gboolean user_function (GtkEntryCompletion *widget,
;;;                         GtkTreeModel       *model,
;;;                         GtkTreeIter        *iter,
;;;                         gpointer            user_data)      : Run Last
;;; 
;;; Gets emitted when a match from the list is selected. The default behaviour
;;; is to replace the contents of the entry with the contents of the text
;;; column in the row pointed to by iter.
;;; 
;;; Note that model is the model that was passed to
;;; gtk_entry_completion_set_model().
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; model :
;;;     the GtkTreeModel containing the matches
;;; 
;;; iter :
;;;     a GtkTreeIter positioned at the selected match
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the signal has been handled
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEntryCompletion
;;; 
;;; struct GtkEntryCompletion;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEntryCompletion" gtk-entry-completion
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable" "GtkCellLayout") :type-initializer
                        "gtk_entry_completion_get_type")
                       ((inline-completion entry-completion-inline-completion
                         "inline-completion" "gboolean" t t)
                        (inline-selection entry-completion-inline-selection
                         "inline-selection" "gboolean" t t)
                        (minimum-key-length entry-completion-minimum-key-length
                         "minimum-key-length" "gint" t t)
                        (model entry-completion-model "model" "GtkTreeModel" t
                         t)
                        (popup-completion entry-completion-popup-completion
                         "popup-completion" "gboolean" t t)
                        (popup-set-width entry-completion-popup-set-width
                         "popup-set-width" "gboolean" t t)
                        (popup-single-match entry-completion-popup-single-match
                         "popup-single-match" "gboolean" t t)
                        (text-column entry-completion-text-column "text-column"
                         "gint" t t)
                        (:cffi entry entry-completion-entry (g-object entry)
                         "gtk_entry_completion_get_entry" nil)
                        (:cffi match-function entry-completion-match-function
                          nil nil gtk-entry-completion-set-match-function)))

;;; ---------------------------------------------------------------------------- 
;;; GtkEntryCompletionMatchFunc ()
;;; 
;;; gboolean (*GtkEntryCompletionMatchFunc) (GtkEntryCompletion *completion,
;;;                                          const gchar *key,
;;;                                          GtkTreeIter *iter,
;;;                                          gpointer user_data);
;;; 
;;; A function which decides whether the row indicated by iter matches a given
;;; key, and should be displayed as a possible completion for key. Note that
;;; key is normalized and case-folded (see g_utf8_normalize() and
;;; g_utf8_casefold()). If this is not appropriate, match functions have access
;;; to the unmodified key via
;;; gtk_entry_get_text (GTK_ENTRY (gtk_entry_completion_get_entry ())).
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
;;; GtkWidget * gtk_entry_completion_get_entry (GtkEntryCompletion *completion)
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
;;;     the GtkTreeModel. [allow-none]
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_model ()
;;; 
;;; GtkTreeModel * gtk_entry_completion_get_model
;;;                                             (GtkEntryCompletion *completion)
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
;;; 
;;; void gtk_entry_completion_set_match_func (GtkEntryCompletion *completion,
;;;                                           GtkEntryCompletionMatchFunc func,
;;;                                           gpointer func_data,
;;;                                           GDestroyNotify func_notify);
;;; 
;;; Sets the match function for completion to be func. The match function is
;;; used to determine if a row should or should not be in the completion list.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; func :
;;;     the GtkEntryCompletionMatchFunc to use
;;; 
;;; func_data :
;;;     user data for func
;;; 
;;; func_notify :
;;;     destroy notify for func_data.
;;; 
;;; Since 2.4
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

(defun gtk-entry-completion-set-match-func (completion function)
  (if function
      (%gtk-entry-completion-set-match-func
                          completion
                          (callback entry-completion-match-func-cb)
                          (create-fn-ref completion function)
                          (callback entry-completion-match-func-destroy-notify))
      (%gtk-entry-completion-set-match-func completion
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

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
;;;                                             (GtkEntryCompletion *completion)
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
;;; gtk_entry_completion_complete ()
;;; 
;;; void gtk_entry_completion_complete (GtkEntryCompletion *completion)
;;; 
;;; Requests a completion operation, or in other words a refiltering of the
;;; current list with completions, using the current key. The completion list
;;; view will be updated accordingly.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_complete" gtk-entry-completion-complete) :void
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_completion_prefix ()
;;; 
;;; const gchar * gtk_entry_completion_get_completion_prefix
;;;                                             (GtkEntryCompletion *completion)
;;; 
;;; Get the original text entered by the user that triggered the completion or
;;; NULL if there's no completion ongoing.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; Returns :
;;;     the prefix for the current completion
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_completion_prefix"
          gtk-entry-completion-completion-prefix) (:string :free-from-foreign t)
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_prefix ()
;;; 
;;; void gtk_entry_completion_insert_prefix  (GtkEntryCompletion *completion)
;;; 
;;; Requests a prefix insertion.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_prefix"
          gtk-entry-completion-insert-prefix) :void
  (completion (g-object gtk-entry-completion)))

(export 'gtk-entry-completion-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_action_text ()
;;; 
;;; void gtk_entry_completion_insert_action_text(GtkEntryCompletion *completion,
;;;                                              gint index_,
;;;                                              const gchar *text);
;;; 
;;; Inserts an action in completion's action item list at position index_ with
;;; text text. If you want the action item to have markup, use
;;; gtk_entry_completion_insert_action_markup().
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; index_ :
;;;     the index of the item to insert
;;; 
;;; text :
;;;     text of the item to insert
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_action_text"
          gtk-entry-completion-insert-action-text) :void
  (completion (g-object gtk-entry-completion))
  (index :int)
  (text :string))

(export 'gtk-entry-completion-insert-action-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_action_markup ()
;;; 
;;; void gtk_entry_completion_insert_action_markup
;;;                                             (GtkEntryCompletion *completion,
;;;                                              gint index_,
;;;                                              const gchar *markup);
;;; 
;;; Inserts an action in completion's action item list at position
;;; index_ with markup markup.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; index_ :
;;;     the index of the item to insert
;;; 
;;; markup :
;;;     markup of the item to insert
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_action_markup"
          gtk-entry-copmletion-insert-action-markup) :void
  (completion (g-object gtk-entry-completion))
  (index :int)
  (markup :string))

(export 'gtk-entry-completion-insert-action-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_delete_action ()
;;; 
;;; void gtk_entry_completion_delete_action (GtkEntryCompletion *completion,
;;;                                          gint index_);
;;; 
;;; Deletes the action at index_ from completion's action list.
;;; 
;;; completion :
;;;     a GtkEntryCompletion
;;; 
;;; index_ :
;;;     the index of the item to delete
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_delete_action"
          gtk-entry-completion-delete-action) :void
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
;;; have a list displaying all (and just) strings in the completion list, and
;;; to get those strings from column in the model of completion.
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
;;;                                             (GtkEntryCompletion *completion)
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
;;; Sets whether it is possible to cycle through the possible completions 
;;; inside the entry.
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
;;;                                             (GtkEntryCompletion *completion)
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
;;;                                             (GtkEntryCompletion *completion)
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
;;;                                             (GtkEntryCompletion *completion)
;;; 
;;; Returns whether the completion popup window will be resized to the width 
;;; of the entry.
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
;;; Sets whether the completion popup window will appear even if there is only 
;;; a single match. You may want to set this to FALSE if you are using inline 
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
;;;                                             (GtkEntryCompletion *completion)
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

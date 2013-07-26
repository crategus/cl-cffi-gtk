
(asdf:load-system :cl-cffi-gtk)

(defpackage :demo-commandline
  (:use :gtk :glib :cffi :common-lisp)
  (:export #:main))

(in-package :demo-commandline)

;;; static gint repeats = 2;
;;; static gint max_size = 8;
;;; static gboolean verbose = FALSE;
;;; static gboolean beep = FALSE;
;;; static gboolean rand = FALSE;

(defvar repeats (foreign-alloc :int :initial-element 2))
(defvar max-size (foreign-alloc :int :initial-element 8))
(defvar verbose (foreign-alloc :boolean :initial-element nil))
(defvar beep (foreign-alloc :boolean :initial-element nil))
(defvar rand (foreign-alloc :boolean :initial-element nil))

;;; static GOptionEntry entries[] =
;;; {
;;;   { "repeats", 'r', 0, G_OPTION_ARG_INT, &repeats, "Average over N repetitions", "N" },
;;;   { "max-size", 'm', 0, G_OPTION_ARG_INT, &max_size, "Test up to 2^M items", "M" },
;;;   { "verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
;;;   { "beep", 'b', 0, G_OPTION_ARG_NONE, &beep, "Beep when done", NULL },
;;;   { "rand", 0, 0, G_OPTION_ARG_NONE, &rand, "Randomize the data", NULL },
;;;   { NULL }
;;; };

(defun main()
  (let ((argv sb-ext:*posix-argv*)
        (entries '(("repeats" #\r (:in-main) :int repeats "Average over N repetitions" "N")
                   ("max-size" #\m (:in-main) :int max-size "Test up to 2^M items" "M")
                   ("verbose" #\v (:in-main) :none verbose "Be verbose" nil)
                   ("beep" #\b (:in-main) :none beep "Beep when done" nil)
                   ("rand" nil (:in-main) :none  rand "Randomize the data" nil)))
        (context (g-option-context-new "- test tree model perfomance")))
    (format t "in main(): ~A~%" argv)
    (g-option-context-add-main-entries context entries nil)
    (g-option-context-add-group context (gtk-get-option-group t))
    (when (not (g-option-context-parse context argv))
      (error "option parsing failed"))))

;;; int
;;; main (int argc, char *argv[])
;;; {
;;;   GError *error = NULL;
;;;   GOptionContext *context;
;;;
;;;   context = g_option_context_new ("- test tree model performance");
;;;   g_option_context_add_main_entries (context, entries, GETTEXT_PACKAGE);
;;;   g_option_context_add_group (context, gtk_get_option_group (TRUE));
;;;   if (!g_option_context_parse (context, &argc, &argv, &error))
;;;     {
;;;       g_print ("option parsing failed: %s\n", error->message);
;;;       exit (1);
;;;     }
;;;
;;;   /* ... */
;;;
;;; }


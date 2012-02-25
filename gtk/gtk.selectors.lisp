;;; ----------------------------------------------------------------------------
;;; gtk.selectors.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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

(in-package :gtk)


(defcfun (file-chooser-select-filename #+win32 "gtk_file_chooser_select_filename_utf8"
                                       #-win32 "gtk_file_chooser_select_filename") :boolean
  (file-chooser g-object)
  (filename :string))

(export 'file-chooser-select-filename)

(defcfun (file-chooser-unselect-filename #+win32 "gtk_file_chooser_unselect_filename_utf8"
                                         #-win32 "gtk_file_chooser_unselect_filename") :void
  (file-chooser g-object)
  (filename :string))

(export 'file-chooser-unselect-filename)

(defcfun (file-chooser-select-all "gtk_file_chooser_select_all") :void
  (file-chooser g-object))

(export 'file-chooser-select-all)

(defcfun (file-chooser-unselect-all "gtk_file_chooser_unselect_all") :void
  (file-chooser g-object))

(export 'file-chooser-unselect-all)

(defcfun (file-chooser-filenames #+win32 "gtk_file_chooser_get_filenames_utf8"
                                 #-win32 "gtk_file_chooser_get_filenames") (g-slist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-filenames)

(defcfun (file-chooser-select-uri "gtk_file_chooser_select_uri") :boolean
  (file-chooser g-object)
  (uri :string))

(export 'file-chooser-select-uri)

(defcfun (file-chooser-unselect-uri "gtk_file_chooser_unselect_uri") :void
  (file-chooser g-object)
  (uri :string))

(export 'file-chooser-unselect-uri)

(defcfun (file-chooser-uris "gtk_file_chooser_get_uris") (g-slist (g-string :free-from-foreign t) :free-from-foreign t)
  (file-chooser g-object))

(export 'file-chooser-uris)

(defcfun (file-chooser-add-filter "gtk_file_chooser_add_filter") :void
  (chooser (g-object file-chooser))
  (filter (g-object file-filter)))

(export 'file-chooser-add-filter)

(defcfun (file-chooser-remove-filter "gtk_file_chooser_remove_filter") :void
  (chooser (g-object file-chooser))
  (filter (g-object file-filter)))

(export 'file-chooser-remove-filter)

(defcfun (file-chooser-filters "gtk_file_chooser_list_filters") (g-list (g-string :free-from-foreign t) :free-from-foreign t)
  (chooser (g-object file-chooser)))

(export 'file-chooser-filters)

(defcfun (gtk-file-chooser-add-shortcut-folder #+win32 "gtk_file_chooser_add_shortcut_folder_utf8"
                                               #-win32 "gtk_file_chooser_add_shortcut_folder") :boolean
  (file-chooser g-object)
  (folder :string)
  (error :pointer))

(defun file-chooser-add-shortcut-folder (file-chooser folder)
  (gtk-file-chooser-add-shortcut-folder file-chooser folder (null-pointer)))

(export 'file-chooser-add-shortcut-folder)

(defcfun (gtk-file-chooser-remove-shortcut-folder #+win32 "gtk_file_chooser_remove_shortcut_folder_utf8"
                                                  #-win32 "gtk_file_chooser_remove_shortcut_folder") :boolean
  (file-chooser g-object)
  (folder :string)
  (error :pointer))

(defun file-chooser-remove-shortcut-folder (file-chooser folder)
  (gtk-file-chooser-remove-shortcut-folder file-chooser folder (null-pointer)))

(export 'file-chooser-remove-shortcut-folder)

(defcfun (file-chooser-shortcut-folders #+win32 "gtk_file_chooser_list_shortcut_folders_utf8"
                                        #-win32 "gtk_file_chooser_list_shortcut_folders") (g-slist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-shortcut-folders)

(defcfun gtk-file-chooser-add-shortcut-folder-uri :boolean
  (file-chooser g-object)
  (folder-uri :string)
  (error :pointer))

(defun file-chooser-add-shortcut-folder-uri (file-chooser folder-uri)
  (gtk-file-chooser-add-shortcut-folder-uri file-chooser folder-uri (null-pointer)))

(export 'file-chooser-add-shortcut-folder-uri)

(defcfun gtk-file-chooser-remove-shortcut-folder-uri :boolean
  (file-chooser g-object)
  (folder-uri :string)
  (error :pointer))

(defun file-chooser-remove-shortcut-folder-uri (file-chooser folder-uri)
  (gtk-file-chooser-remove-shortcut-folder-uri file-chooser folder-uri (null-pointer)))

(export 'file-chooser-remove-shortcut-folder-uri)

(defcfun (file-chooser-shortcut-folder-uris "gtk_file_chooser_list_shortcut_folder_uris") (g-slist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-shortcut-folder-uris)

(defcfun (file-chooser-get-current-folder-file "gtk_file_chooser_get_current_folder_file") g-object
  (chooser (g-object file-chooser)))

(defcfun gtk-file-chooser-set-current-folder-file :boolean
  (file-chooser (g-object file-chooser))
  (file g-object)
  (error :pointer))

(defun file-chooser-set-current-folder-file (file-chooser file)
  (gtk-file-chooser-set-current-folder-file file-chooser file (null-pointer)))

(export '(file-chooser-get-current-folder-file file-chooser-set-current-folder-file))

(defcfun (file-chooser-get-file "gtk_file_chooser_get_file") g-object
  (file-chooser (g-object file-chooser)))

(defcfun gtk-file-chooser-set-file g-object
  (file-chooser (g-object file-chooser))
  (file g-object)
  (error :pointer))

(defun file-chooser-set-file (file-chooser file)
  (gtk-file-chooser-set-file file-chooser file (null-pointer)))

(export '(file-chooser-get-file file-chooser-set-file))

(defcfun (file-chooser-unselect-file "gtk_file_chooser_unselect_file") :void
  (file-chooser (g-object file-chooser))
  (file g-object))

(export 'file-chooser-unselect-file)


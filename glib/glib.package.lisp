;;; ----------------------------------------------------------------------------
;;; glib.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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

(defpackage :glib
  (:use :cl :cffi :iter)
  (:export ;; Symbols from glib.stable-pointer.lisp
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:set-stable-pointer-value
           #:stable-pointer-destroy-notify-cb
           #:with-stable-pointer
           ;; Symbols from glib.error.lisp
           #:with-catching-to-g-error
           #:with-g-error
           #:with-ignore-g-error))

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :glib) t)
 "GLib is a general-purpose utility library, which provides many useful data
  types, macros, type conversions, string utilities, file utilities, a mainloop
  abstraction, and so on. It works on many UNIX-like platforms, as well as
  Windows and OS X. GLib is released under the GNU Library General Public
  License (GNU LGPL).

  This is the API documentation of a Lisp binding to the library GLib. Only a
  small part of GLib is implemented in Lisp which is necessary to implement
  GTK+ in Lisp.
  @begin[Version Information]{section}
    Variables and functions to check the GLib version.

    GLib provides version information, primarily useful in configure checks for
    builds that have a configure script. Applications will not typically use
    the features described here.

    @about-symbol{+glib-major-version+}
    @about-symbol{+glib-minor-version+}
    @about-symbol{+glib-micro-version+}
    @about-symbol{+glib-binary-age+}
    @about-symbol{+glib-interface-age+}
    @about-function{glib-check-version}
  @end{section}
  @begin[Miscellaneous]{section}
    Documentation of several type definitions and functions, which are
    needed for the implemenation of the GTK+ library. Only a small part of the
    GLib library is implemented.
    @begin[Basic Types]{subsection}
      Standard GLib types, defined for ease-of-use and portability.
      Only the following types are implemented:

      @about-type{g-size}
      @about-type{g-ssize}
      @about-type{g-offset}
    @end{subsection}
    @begin[Memory Allocation]{subsection}
      The following functions for the general memory-handling are implemented:

      @about-function{g-malloc}
      @about-function{g-free}
    @end{subsection}
    @begin[Date and Time Functions]{subsection}
      Calendrical calculations and miscellaneous time stuff.
      Only the following struct is implemented:

      @about-type{g-time-val}
      @about-function{g-get-current-time}
      @about-function{g-get-monotonic-time}
      @about-function{g-get-real-time}
    @end{subsection}
    @begin[String Utility Functions]{subsection}
      Implemented is:

      @about-type{g-string}
      @about-type{g-strv}
    @end{subsection}
    @begin[Doubly-Linked Lists]{subsection}
      Linked lists containing integer values or pointers to data, with the ability
      to iterate over the list in both directions

      Implemented is:

      @about-type{g-list}
    @end{subsection}
    @begin[Singly-Linked Lists]{subsection}
      Linked lists containing integer values or pointers to data, limited to
      iterating over the list in one direction

      Implemented is:

      @about-type{g-slist}
    @end{subsection}
    @begin[Threads]{subsection}
      Portable support for threads, mutexes, locks, conditions and thread private
      data.

      Implemented is:

      @about-type{g-mutex}
      @about-type{g-cond}
    @end{subsection}
  @end{section}
  @begin[The Main Event Loop]{section}
    The main event loop manages all the available sources of events for GLib
    and GTK+ applications. These events can come from any number of different
    types of sources such as file descriptors (plain files, pipes or sockets)
    and timeouts. New types of event sources can also be added using the
    function @fun{g-source-attach}.

    To allow multiple independent sets of sources to be handled in different
    threads, each source is associated with a @type{g-main-context}. A
    @type{g-main-context} can only be running in a single thread, but sources
    can be added to it and removed from it from other threads.

    Each event source is assigned a priority. The default priority,
    @var{+g-priority-default+}, is 0. Values less than 0 denote higher
    priorities. Values greater than 0 denote lower priorities. Events from high
    priority sources are always processed before events from lower priority
    sources.

    Idle functions can also be added, and assigned a priority. These will be
    run whenever no events with a higher priority are ready to be processed.

    The @type{g-main-loop} data type represents a main event loop. A
    @type{g-main-loop} is created with the function @fun{g-main-loop-new}.
    After adding the initial event sources, the function @fun{g-main-loop-run}
    is called. This continuously checks for new events from each of the event
    sources and dispatches them. Finally, the processing of an event from one
    of the sources leads to a call to the function @fun{g-main-loop-quit} to
    exit the main loop, and the function @fun{g-main-loop-run} returns.

    It is possible to create new instances of @type{g-main-loop} recursively.
    This is often used in GTK+ applications when showing modal dialog boxes.
    Note that event sources are associated with a particular
    @type{g-main-context}, and will be checked and dispatched for all main
    loops associated with that @type{g-main-context}.

    GTK+ contains wrappers of some of these functions, e. g. the functions
    @fun{gtk-main}, @fun{gtk-main-quit} and @fun{gtk-events-pending}.

    @subheading{Creating new source types}
    One of the unusual features of the @type{g-main-loop} functionality is that
    new types of event source can be created and used in addition to the builtin
    type of event source. A new event source type is used for handling GDK
    events. A new source type is created by deriving from the  @type{g-source}
    structure. The derived type of source is represented by a structure that has
    the @type{g-source} structure as a first element, and other elements
    specific to the new source type. To create an instance of the new source
    type, call the function @fun{g-source-new} passing in the size of the
    derived structure and a table of functions. These @type{g-source-funcs}
    determine the behavior of the new source type.

    New source types basically interact with the main context in two ways. Their
    prepare function in @type{g-source-funcs} can set a timeout to determine the
    maximum amount of time that the main loop will sleep before checking the
    source again. In addition, or as well, the source can add file descriptors
    to the set that the main context checks using the function
    @fun{g-source-add-poll}.

    @subheading{Customizing the main loop iteration}
    Single iterations of a @type{g-main-context} can be run with the function
    @fun{g-main-context-iteration}. In some cases, more detailed control of
    exactly how the details of the main loop work is desired, for instance, when
    integrating the @type{g-main-loop} with an external main loop. In such
    cases, you can call the component functions of the function
    @fun{g-main-context-iteration} directly. These functions are
    @fun{g-main-context-prepare}, @fun{g-main-context-query},
    @fun{g-main-context-check} and @fun{g-main-context-dispatch}.

    On Unix, the GLib mainloop is incompatible with @code{fork()}. Any program
    using the mainloop must either @code{exec()} or @code{exit()} from the
    child without returning to the mainloop.

    @about-type{g-main-loop}
    @about-function{g-main-loop-new}
    @about-function{g-main-loop-ref}
    @about-function{g-main-loop-unref}
    @about-function{g-main-loop-run}
    @about-function{g-main-loop-quit}
    @about-function{g-main-loop-is-running}
    @about-function{g-main-loop-get-context}
    @about-function{g-main-new}
    @about-function{g-main-destroy}
    @about-function{g-main-run}
    @about-function{g-main-quit}
    @about-function{g-main-is-running}
    @about-variable{+g-priority-high+}
    @about-variable{+g-priority-default+}
    @about-variable{+g-priority-high-idle+}
    @about-variable{+g-priority-default-idle+}
    @about-variable{+g-priority-low+}
    @about-variable{+g-source-continue+}
    @about-variable{+g-source-remove+}
    @about-type{g-main-context}
    @about-function{g-main-context-new}
    @about-function{g-main-context-ref}
    @about-function{g-main-context-unref}
    @about-function{g-main-context-default}
    @about-function{g-main-context-iteration}
    @about-function{g-main-iteration}
    @about-function{g-main-context-pending}
    @about-function{g-main-pending}
    @about-function{g-main-context-find-source-by-id}
    @about-function{g-main-context-find-source-by-user-data}
    @about-function{g-main-context-find-source-by-funcs-user-data}
    @about-function{g-main-context-wakeup}
    @about-function{g-main-context-acquire}
    @about-function{g-main-context-release}
    @about-function{g-main-context-is-owner}
    @about-function{g-main-context-wait}
    @about-function{g-main-context-prepare}
    @about-function{g-main-context-query}
    @about-function{g-main-context-check}
    @about-function{g-main-context-dispatch}
    @about-function{g-main-context-set-poll-func}
    @about-function{g-main-context-get-poll-func}
    @about-function{g-main-context-add-poll}
    @about-function{g-main-context-remove-poll}
    @about-function{g-main-depth}
    @about-function{g-main-current-source}
    @about-function{g-main-set-poll-func}
    @about-function{g-main-context-invoke}
    @about-function{g-main-context-invoke-full}
    @about-function{g-main-context-get-thread-default}
    @about-function{g-main-context-ref-thread-default}
    @about-function{g-main-context-push-thread-default}
    @about-function{g-main-context-pop-thread-default}
    @about-function{g-timeout-source-new}
    @about-function{g-timeout-source-new-seconds}
    @about-function{g-timeout-add}
    @about-function{g-timeout-add-full}
    @about-function{g-timeout-add-seconds}
    @about-function{g-timeout-add-seconds-full}
    @about-function{g-idle-source-new}
    @about-function{g-idle-add}
    @about-function{g-idle-add-full}
    @about-function{g-idle-remove-by-data}
    @about-function{GPid}
    @about-function{g-child-watch-source-new}
    @about-function{g-child-watch-add}
    @about-function{g-child-watch-add-full}
    @about-type{g-poll-fd}
    @about-function{g-poll}
    @about-function{G_POLLFD_FORMAT}
    @about-type{g-source}
    @about-type{g-source-funcs}
    @about-type{g-source-callback-funcs}
    @about-function{g-source-new}
    @about-function{g-source-ref}
    @about-function{g-source-unref}
    @about-function{g-source-set-funcs}
    @about-function{g-source-attach}
    @about-function{g-source-destroy}
    @about-function{g-source-is-destroyed}
    @about-function{g-source-set-priority}
    @about-function{g-source-get-priority}
    @about-function{g-source-set-can-recurse}
    @about-function{g-source-get-can-recurse}
    @about-function{g-source-get-id}
    @about-function{g-source-get-name}
    @about-function{g-source-set-name}
    @about-function{g-source-set-name-by-id}
    @about-function{g-source-get-context}
    @about-function{g-source-set-callback}
    @about-function{g-source-set-callback-indirect}
    @about-function{g-source-set-ready-time}
    @about-function{g-source-get-ready-time}
    @about-function{g-source-add-unix-fd}
    @about-function{g-source-remove-unix-fd}
    @about-function{g-source-modify-unix-fd}
    @about-function{g-source-query-unix-fd}
    @about-function{g-source-add-poll}
    @about-function{g-source-remove-poll}
    @about-function{g-source-add-child-source}
    @about-function{g-source-remove-child-source}
    @about-function{g-source-get-time}
    @about-function{g-source-get-current-time}
    @about-function{g-source-remove}
    @about-function{g-source-remove-by-funcs-user-data}
    @about-function{g-source-remove-by-user-data}
  @end{section}
  @begin[Quarks]{section}
    A 2-way association between a string and a unique integer identifier.

    @about-type{g-quark}
    @about-function{g-quark-from-string}
    @about-function{g-quark-from-static-string}
    @about-function{g-quark-to-string}
    @about-function{g-quark-try-string}
    @about-function{g-intern-string}
    @about-function{g-intern-static-string}
  @end{section}
  @begin[Error Reporting]{section}
    A system for reporting errors.

    GLib provides a standard method of reporting errors from a called function
    to the calling code. In the Lisp binding we provide internally the macros
    @code{with-g-error} and @code{with-catching-to-g-error} to handle errors
    from the GTK+ and GLIB libraries. Only the type @type{g-error} is exported.

    @about-type{g-error}
    @about-function{g-error-new}
    @about-function{g-error-new-literal}
    @about-function{g-error-new-valist}
    @about-function{g-error-free}
    @about-function{g-error-copy}
    @about-function{g-error-matches}
    @about-function{g-set-error}
    @about-function{g-set-error-literal}
    @about-function{g-propagate-error}
    @about-function{g-clear-error}
    @about-function{g-prefix-error}
    @about-function{g-propagate-prefixed-error}
  @end{section}
  @begin[Utility Functions]{section}
    A selection of portable utility functions.

    @about-function{g-get-application-name}
    @about-function{g-set-application-name}
    @about-function{g-get-prgname}
    @about-function{g-set-prgname}
    @about-function{g-get-environ}
    @about-function{g-environ-getenv}
    @about-function{g-environ-setenv}
    @about-function{g-environ-unsetenv}
    @about-function{g-getenv}
    @about-function{g-setenv}
    @about-function{g-unsetenv}
    @about-function{g-listenv}
    @about-function{g-get-user-name}
    @about-function{g-get-real-name}
    @about-function{g-get-user-cache-dir}
    @about-function{g-get-user-data-dir}
    @about-function{g-get-user-config-dir}
    @about-function{g-get-user-runtime-dir}
    @about-type{g-user-directory}
    @about-function{g-get-user-special-dir}
    @about-function{g-get-system-data-dirs}
    @about-function{g-get-system-config-dirs}
    @about-function{g-reload-user-special-dirs-cache}
    @about-function{g-get-host-name}
    @about-function{g-get-home-dir}
    @about-function{g-get-tmp-dir}
    @about-function{g-get-current-dir}
    @about-function{g-basename}
    @about-function{g-dirname}
    @about-function{g-path-is-absolute}
    @about-function{g-path-skip-root}
    @about-function{g-path-get-basename}
    @about-function{g-path-get-dirname}
    @about-function{g-build-filename}
    @about-function{g-build-filenamev}
    @about-function{g-build-path}
    @about-function{g-build-pathv}
    @about-function{g-format-size}
    @about-function{GFormatSizeFlags}
    @about-function{g-format-size-full}
    @about-function{g-format-size-for-display}
    @about-function{g-find-program-in-path}
    @about-function{g-bit-nth-lsf}
    @about-function{g-bit-nth-msf}
    @about-function{g-bit-storage}
    @about-function{g-spaced-primes-closest}
    @about-function{g-atexit}
    @about-function{g-parse-debug-string}
    @about-function{GDebugKey}
    @about-function{g-qsort-with-data}
    @about-function{g-nullify-pointer}
  @end{section}
  @begin[Commandline option parser]{section}
    Parses commandline options.

    @about-symbol{g-option-error}
    @about-symbol{G_OPTION_ERROR}
    @about-type{g-option-context}
    @about-function{g-option-context-new}
    @about-function{g-option-context-set-summary}
    @about-function{g-option-context-get-summary}
    @about-function{g-option-context-set-description}
    @about-function{g-option-context-get-description}
    @about-function{g-option-context-set-translate-func}
    @about-function{g-option-context-set-translation-domain}
    @about-function{g-option-context-free}
    @about-function{g-option-context-parse}
    @about-function{g-option-context-set-help-enabled}
    @about-function{g-option-context-get-help-enabled}
    @about-function{g-option-context-set-ignore-unknown-options}
    @about-function{g-option-context-get-ignore-unknown-options}
    @about-function{g-option-context-get-help}
    @about-symbol{g-option-arg}
    @about-symbol{g-option-flags}
    @about-symbol{G_OPTION_REMAINING}
    @about-type{g-option-entry}
    @about-function{g-option-context-add-main-entries}
    @about-type{g-option-group}
    @about-function{g-option-context-add-group}
    @about-function{g-option-context-set-main-group}
    @about-function{g-option-context-get-main-group}
    @about-function{g-option-group-new}
    @about-function{g-option-group-free}
    @about-function{g-option-group-add-entries}
    @about-function{g-option-group-set-parse-hooks}
    @about-function{g-option-group-set-error-hook}
    @about-function{g-option-group-set-translate-func}
    @about-function{g-option-group-set-translation-domain}
  @end{section}
  @begin[Key-value file parser]{section}
    Parses .ini-like config files.

    @about-symbol{g-key-file-error}
    @about-symbol{g-key-file-flags}
    @about-type{g-key-file}
    @about-function{g-key-file-new}
    @about-function{g-key-file-free}
    @about-function{g-key-file-ref}
    @about-function{g-key-file-unref}
    @about-function{g-key-file-set-list-separator}
    @about-function{g-key-file-load-from-file}
    @about-function{g-key-file-load-from-data}
    @about-function{g-key-file-load-from-bytes}
    @about-function{g-key-file-load-from-data-dirs}
    @about-function{g-key-file-load-from-dirs}
    @about-function{g-key-file-to-data}
    @about-function{g-key-file-save-to-file}
    @about-function{g-key-file-get-start-group}
    @about-function{g-key-file-get-groups}
    @about-function{g-key-file-get-keys}
    @about-function{g-key-file-has-group}
    @about-function{g-key-file-has-key}
    @about-function{g-key-file-get-value}
    @about-function{g-key-file-get-string}
    @about-function{g-key-file-get-locale-string}
    @about-function{g-key-file-get-locale-for-key}
    @about-function{g-key-file-get-boolean}
    @about-function{g-key-file-get-integer}
    @about-function{g-key-file-get-int64}
    @about-function{g-key-file-get-uint64}
    @about-function{g-key-file-get-double}
    @about-function{g-key-file-get-string-list}
    @about-function{g-key-file-get-locale-string-list}
    @about-function{g-key-file-get-boolean-list}
    @about-function{g-key-file-get-integer-list}
    @about-function{g-key-file-get-double-list}
    @about-function{g-key-file-get-comment}
    @about-function{g-key-file-set-value}
    @about-function{g-key-file-set-string}
    @about-function{g-key-file-set-locale-string}
    @about-function{g-key-file-set-boolean}
    @about-function{g-key-file-set-integer}
    @about-function{g-key-file-set-int64}
    @about-function{g-key-file-set-uint64}
    @about-function{g-key-file-set-double}
    @about-function{g-key-file-set-string-list}
    @about-function{g-key-file-set-locale-string-list}
    @about-function{g-key-file-set-boolean-list}
    @about-function{g-key-file-set-integer-list}
    @about-function{g-key-file-set-double-list}
    @about-function{g-key-file-set-comment}
    @about-function{g-key-file-remove-group}
    @about-function{g-key-file-remove-key}
    @about-function{g-key-file-remove-comment}
  @end{section}
  @begin[GVariantType]{section}
    Introduction to the GVariant type system.

    @about-class{g-variant-type}
    @about-variable{+g-variant-type-boolean+}
    @about-variable{+g-variant-type-byte+}
    @about-variable{+g-variant-type-int16+}
    @about-variable{+g-variant-type-uint16+}
    @about-variable{+g-variant-type-int32}
    @about-variable{+g-variant-type-uint32+}
    @about-variable{+g-variant-type-int64+}
    @about-variable{+g-variant-type-uint64+}
    @about-variable{+g-variant-type-handle+}
    @about-variable{+g-variant-type-double+}
    @about-variable{+g-variant-type-string+}
    @about-variable{+g-variant-type-object-path+}
    @about-variable{+g-variant-type-signature+}
    @about-variable{+g-variant-type-variant+}
    @about-variable{+g-variant-type-any+}
    @about-variable{+g-variant-type-basic+}
    @about-variable{+g-variant-type-maxbe+}
    @about-variable{+g-variant-type-array+}
    @about-variable{+g-variant-type-tulpe+}
    @about-variable{+g-variant-type-unit+}
    @about-variable{+g-variant-type-dict-entry+}
    @about-variable{+g-variant-type-dictionary+}
    @about-variable{+g-variant-type-string-array+}
    @about-variable{+g-variant-type-object-path-array+}
    @about-variable{+g-variant-type-bytestring+}
    @about-variable{+g-variant-type-bytestring-array+}
    @about-variable{+g-variant-type-vardict+}
    @about-function{g-variant-type-checked}
    @about-function{g-variant-type-free}
    @about-function{g-variant-type-copy}
    @about-function{g-variant-type-new}
    @about-function{g-variant-type-string-is-valid}
    @about-function{g-variant-type-string-scan}
    @about-function{g-variant-type-get-string-length}
    @about-function{g-variant-type-peek-string}
    @about-function{g-variant-type-dup-string}
    @about-function{g-variant-type-is-definite}
    @about-function{g-variant-type-is-container}
    @about-function{g-variant-type-is-basic}
    @about-function{g-variant-type-is-maybe}
    @about-function{g-variant-type-is-array}
    @about-function{g-variant-type-is-tuple}
    @about-function{g-variant-type-is-dict-entry}
    @about-function{g-variant-type-is-variant}
    @about-function{g-variant-type-hash}
    @about-function{g-variant-type-equal}
    @about-function{g-variant-type-is-subtype-of}
    @about-function{g-variant-type-new-maybe}
    @about-function{g-variant-type-new-array}
    @about-function{g-variant-type-new-tuple}
    @about-function{g-variant-type-new-dict-entry}
    @about-function{g-variant-type-element}
    @about-function{g-variant-type-n-items}
    @about-function{g-variant-type-first}
    @about-function{g-variant-type-next}
    @about-function{g-variant-type-key}
    @about-function{g-variant-type-value}
  @end{section}
  @begin[GVariant]{section}
    Strongly typed value datatype.

    @about-type{g-variant}

    @about-symbol{g-variant-class}
    @about-symbol{g-variant-iter}
    @about-symbol{g-variant-builder}
    @about-symbol{g-variant-dict}
    @about-symbol{g-variant-parse-error}
    @about-function{g-variant-unref}
    @about-function{g-variant-ref}
    @about-function{g-variant-ref-sink}
    @about-function{g-variant-is-floating}
    @about-function{g-variant-take-ref}
    @about-function{g-variant-type}
    @about-function{g-variant-type-string}
    @about-function{g-variant-is-of-type}
    @about-function{g-variant-is-container}
    @about-function{g-variant-compare}
    @about-function{g-variant-classify}
    @about-function{g-variant-check-format-string}
    @about-function{g-variant-get}
    @about-function{g-variant-get-va}
    @about-function{g-variant-new}
    @about-function{g-variant-new-va}
    @about-function{g-variant-new-boolean}
    @about-function{g-variant-new-byte}
    @about-function{g-variant-new-int16}
    @about-function{g-variant-new-uint16}
    @about-function{g-variant-new-int32}
    @about-function{g-variant-new-uint32}
    @about-function{g-variant-new-int64}
    @about-function{g-variant-new-uint64}
    @about-function{g-variant-new-handle}
    @about-function{g-variant-new-double}
    @about-function{g-variant-new-string}
    @about-function{g-variant-new-take-string}
    @about-function{g-variant-new-printf}
    @about-function{g-variant-new_object-path}
    @about-function{g-variant-is-object-path}
    @about-function{g-variant-new-signature}
    @about-function{g-variant-is-signature}
    @about-function{g-variant-new-variant}
    @about-function{g-variant-new-strv}
    @about-function{g-variant-new-objv}
    @about-function{g-variant-new-bytestring}
    @about-function{g-variant-new-bytestring-array}
    @about-function{g-variant-boolean}
    @about-function{g-variant-byte}
    @about-function{g-variant-int16}
    @about-function{g-variant-uint16}
    @about-function{g-variant-int32}
    @about-function{g-variant-uint32}
    @about-function{g-variant-int64}
    @about-function{g-variant-int64}
    @about-function{g-variant-handle}
    @about-function{g-variant-double}
    @about-function{g-variant-string}
    @about-function{g-variant-variant}
    @about-function{g-variant-strv}
    @about-function{g-variant-dup-strv}
    @about-function{g-variant-objv}
    @about-function{g-variant-dup-objv}
    @about-function{g-variant-bytestring}
    @about-function{g-variant-dup-bytestring}
    @about-function{g-variant-bytestring_array}
    @about-function{g-variant-dup-bytestring-array}
    @about-function{g-variant-new-maybe}
    @about-function{g-variant-new-array}
    @about-function{g-variant-new-tuple}
    @about-function{g-variant-new-dict-entry}
    @about-function{g-variant-new-new-fixed-array}
    @about-function{g-variant-get-maybe}
    @about-function{g-variant-n-children}
    @about-function{g-variant-get-child-value}
    @about-function{g-variant-get-child}
    @about-function{g-variant-lookup-value}
    @about-function{g-variant-lookup}
    @about-function{g-variant-get-fixed-array}
    @about-function{g-variant-get-size}
    @about-function{g-variant-get-data}
    @about-function{g-variant-get-data-as-bytes}
    @about-function{g-variant-store}
    @about-function{g-variant-new-from-data}
    @about-function{g-variant-new-from-bytes}
    @about-function{g-variant-byteswap}
    @about-function{g-variant-get-normal-form}
    @about-function{g-variant-is-normal-form}
    @about-function{g-variant-hash}
    @about-function{g-variant-equal}
    @about-function{g-variant-print}
    @about-function{g-variant-print-string}
    @about-function{g-variant-iter-copy}
    @about-function{g-variant-iter-free}
    @about-function{g-variant-iter-init}
    @about-function{g-variant-iter-n-children}
    @about-function{g-variant-iter-new}
    @about-function{g-variant-iter-next-value}
    @about-function{g-variant-iter-next}
    @about-function{g-variant-iter-loop}
    @about-function{g-variant-builder-unref}
    @about-function{g-variant-builder-ref}
    @about-function{g-variant-builder-new}
    @about-function{g-variant-builder-init}
    @about-function{g-variant-builder-clear}
    @about-function{g-variant-builder-add-value}
    @about-function{g-variant-builder-add}
    @about-function{g-variant-builder-add-parsed}
    @about-function{g-variant-builder-end}
    @about-function{g-variant-builder-open}
    @about-function{g-variant-builder-close}
    @about-function{g-variant-dict-init}
    @about-function{g-variant-dict-unref}
    @about-function{g-variant-dict-ref}
    @about-function{g-variant-dict-new}
    @about-function{g-variant-dict-init}
    @about-function{g-variant-dict-clear}
    @about-function{g-variant-dict-contains}
    @about-function{g-variant-dict-lookup}
    @about-function{g-variant-dict-lookup-value}
    @about-function{g-variant-dict-insert}
    @about-function{g-variant-dict-insert-value}
    @about-function{g-variant-dict-remove}
    @about-function{g-variant-dict-end}
    @about-function{g-variant-parse}
    @about-function{g-variant-new-parsed-va}
    @about-function{g-variant-new-parsed}
    @about-function{g-variant-parse-error-print-context}
  @end{section}")

;;; --- End of file glib.package.lisp ------------------------------------------

;;; ----------------------------------------------------------------------------
;;; atdoc-glib.utils.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :glib)

(setf (documentation 'g-get-application-name 'function)
 "@version{2012-12-31}
  @return{Human-readable application name. May return @code{nil}.}
  @short{Gets a human-readable name for the application, as set by
    @fun{g-set-application-name}.}

  This name should be localized if possible, and is intended for display to the
  user. Contrast with @fun{g-get-prgname}, which gets a non-localized name. If
  @fun{g-set-application-name} has not been called, returns the result of
  @fun{g-get-prgname} (which may be @code{nil} if @fun{g-set-prgname} has also
  not been called).

  Since 2.2
  @see-function{g-set-application-name}
  @see-function{g-get-prgname}
  @see-function{g-set-prgname}")

(setf (documentation 'g-set-application-name 'function)
 "@version{2012-12-31}
  @argument[application-name]{localized name of the application}
  @short{Sets a human-readable name for the application.}

  This name should be localized if possible, and is intended for display to the
  user. Contrast with @fun{g-set-prgname}, which sets a non-localized name.
  @fun{g-set-prgname} will be called automatically by @fun{gtk-init}, but
  @sym{g-set-application-name} will not.

  Note that for thread safety reasons, this function can only be called once.

  The application name will be used in contexts such as error messages, or
  when displaying an application's name in the task list.

  Since 2.2
  @see-function{g-set-prgname}")

(setf (documentation 'g-get-prgname 'function)
 "@version{2012-12-31}
  @return{The name of the program. The returned string belongs to GLib and must
    not be modified or freed.}
  @short{Gets the name of the program.}
  This name should not be localized, contrast with @fun{g-get-application-name}.
  (If you are using GDK or GTK+ the program name is set in @code{gdk_init()},
  which is called by @fun{gtk-init}. The program name is found by taking the
  last component of @code{argv[0]}.)
  @see-function{g-get-application-name}")

(setf (documentation 'g-set-prgname 'function)
 "@version{2012-12-31}
  @argument[prgname]{the name of the program.}
  @short{Sets the name of the program.}
  This name should not be localized, contrast with @fun{g-set-application-name}.
  Note that for thread-safety reasons this function can only be called once.
  @see-function{g-set-application-name}")

(setf (documentation 'g-getenv 'function)
 "@version{2012-12-31}
  @argument[variable]{the environment variable to get, in the GLib file name
    encoding}
  @return{The value of the environment variable, or @code{nil} if the
    environment variable is not found. The returned string may be overwritten by
    the next call to @sym{g-getenv}, @fun{g-setenv} or @code{g_unsetenv()}.}
  @short{Returns the value of an environment variable.}

  The name and value are in the GLib file name encoding. On UNIX, this means
  the actual bytes which might or might not be in some consistent character
  set and encoding. On Windows, it is in UTF-8. On Windows, in case the
  environment variable's value contains references to other environment
  variables, they are expanded.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-getenv \"HOME\") => \"/home/dieter\"
    @end{pre}
  @end{dictionary}
  @see-function{g-setenv}")

(setf (documentation 'g-setenv 'function)
 "@version{2012-12-31}
  @argument[variable]{the environment variable to set, must not contain '='.}
  @argument[value]{the value for to set the variable to.}
  @argument[overwrite]{whether to change the variable if it already exists.}
  @return{@arg{false} if the environment variable couldn't be set.}
  @short{Sets an environment variable.}

  Both the variable's name and value should be in the GLib file name encoding.
  On UNIX, this means that they can be arbitrary byte strings. On Windows, they
  should be in UTF-8.

  Note that on some systems, when variables are overwritten, the memory used
  for the previous variables and its value isn't reclaimed.

  @begin[Warning]{dictionary}
    Environment variable handling in UNIX is not thread-safe, and your program
    may crash if one thread calls @sym{g-setenv} while another thread is calling
    @code{getenv()}. (And note that many functions, such as @code{gettext()},
    call @code{getenv()} internally.) This function is only safe to use at the
    very start of your program, before creating any other threads (or creating
    objects that create worker threads of their own).

    If you need to set up the environment for a child process, you can use
    @code{g_get_environ()} to get an environment array, modify that with
    @code{g_environ_setenv()} and @code{g_environ_unsetenv()}, and then pass
    that array directly to @code{execvpe()}, @code{g_spawn_async()}, or the
    like.
  @end{dictionary}
  Since 2.4")

(setf (documentation 'g-listenv 'function)
 "@version{2012-12-31}
  @return{A list of strings.}
  @short{Gets the names of all variables set in the environment.}

  Programs that want to be portable to Windows should typically use this
  function and @fun{g-getenv} instead of using the environ array from the C
  library directly. On Windows, the strings in the environ array are in system
  codepage encoding, while in most of the typical use cases for environment
  variables in GLib-using programs you want the UTF-8 encoding that this
  function and @fun{g-getenv} provide.

  Since 2.8
  @see-function{g-getenv}")

(setf (documentation 'g-get-user-name 'function)
 "@version{2012-12-31}
  @return{The user name of the current user.}
  @short{Gets the user name of the current user.}
  The encoding of the returned string is system-defined. On UNIX, it might be
  the preferred file name encoding, or something else, and there is no guarantee
  that it is even consistent on a machine. On Windows, it is always UTF-8.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-get-user-name) => \"dieter\"
    @end{pre}
  @end{dictionary}")

(setf (documentation 'g-get-real-name 'function)
 "@version{2012-12-31}
  @return{The user's real name.}
  @short{Gets the real name of the user.}
  This usually comes from the user's entry in the passwd file. The encoding of
  the returned string is system-defined. (On Windows, it is, however, always
  UTF-8.) If the real user name cannot be determined, the string \"Unknown\" is
  returned.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-get-real-name) => \"Dieter Kaiser\"
    @end{pre}
  @end{dictionary}")

(setf (documentation 'g-get-user-cache-dir 'function)
 "@version{2012-12-31}
 @return{A string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to store non-essential, cached data
    specific to particular user.}
  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be @code{XDG_CACHE_HOME}.

  On Windows is the directory that serves as a common repository for temporary
  Internet files. A typical path is
  @file{C:\Documents and Settings\username\Local Settings\Temporary Internet Files}.
  See documentation for @code{CSIDL_INTERNET_CACHE}.

  Since 2.6")

(setf (documentation 'g-get-user-data-dir 'function)
 "@version{2012-12-31}
  @return{A string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to access application data such as
    icons that is customized for a particular user.}
  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be @code{XDG_DATA_HOME}.

  On Windows this is the folder to use for local (as opposed to roaming)
  application data. See documentation for @code{CSIDL_LOCAL_APPDATA}. Note that
  on Windows it thus is the same as what @fun{g-get-user-config-dir} returns.

  Since 2.6
  @see-function{g-get-user-config-dir}")

(setf (documentation 'g-get-user-config-dir 'function)
 "@version{2012-12-31}
  @return{A string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to store user-specific application
    configuration information such as user preferences and settings.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be @code{XDG_CONFIG_HOME}.

  On Windows this is the folder to use for local (as opposed to roaming)
  application data. See documentation for @code{CSIDL_LOCAL_APPDATA}. Note that
  on Windows it thus is the same as what @fun{g-get-user-data-dir} returns.

  Since 2.6
  @see-function{g-get-user-data-dir}")

(setf (gethash 'g-user-directory atdoc:*type-name-alias*) "Enum")
(setf (documentation 'g-user-directory 'type)
 "@version{2012-12-31}
  @begin{short}
    These are logical ids for special directories which are defined depending on
    the platform used.
  @end{short}
  You should use @fun{g-get-user-special-dir} to retrieve the full path
  associated to the logical id.

  The @sym{g-user-directory} enumeration can be extended at later date. Not
  every platform has a directory for every logical id in this enumeration.

  Since 2.14
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcenum g-user-directory
  :desktop
  :documents
  :download
  :music
  :pictures
  :public-share
  :templates
  :videos
  :n-directories)
    @end{pre}
    @begin{table}
      @entry[:desktop]{the user's Desktop directory}
      @entry[:documents]{the user's Documents directory}
      @entry[:download]{the user's Downloads directory}
      @entry[:music]{the user's Music directory}
      @entry[:pictures]{the user's Pictures directory}
      @entry[:public-share]{the user's shared directory}
      @entry[:templates]{the user's Templates directory}
      @entry[:videos]{the user's Movies directory}
      @entry[:n-directories]{the number of enum values}
    @end{table}
  @end{dictionary}
  @see-function{g-get-user-special-dir}")

(setf (documentation 'g-get-user-special-dir 'function)
 "@version{2012-12-31}
  @argument[directory]{the logical id of special directory}
  @return{The path to the specified special directory, or @code{nil} if the
    logical id was not found. The returned string is owned by GLib and should
    not be modified or freed.}
  @short{Returns the full path of a special directory using its logical id.}

  On Unix this is done using the XDG special user directories. For
  compatibility with existing practise, @code{:desktop} falls back to
  @code{$HOME/Desktop} when XDG special user directories have not been set up.

  Depending on the platform, the user might be able to change the path of the
  special directory without requiring the session to restart; GLib will not
  reflect any change once the special directories are loaded.

  Since 2.14
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-get-user-special-dir :documents)
=> \"/home/dieter/Dokumente\"
 (g-get-user-special-dir :download)
=> \"/home/dieter/Downloads\"
    @end{pre}
  @end{dictionary}
  @see-type{g-user-directory}")

(setf (documentation 'g-get-system-data-dirs 'function)
 "@return{A list of strings owned by GLib that must not be modified or freed.}
  @short{Returns an ordered list of base directories in which to access
    system-wide application data.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification In this case the list of directories
  retrieved will be @code{XDG_DATA_DIRS}.

  On Windows the first elements in the list are the Application Data and
  Documents folders for All Users. (These can be determined only on Windows
  2000 or later and are not present in the list on other Windows versions.)
  See documentation for @code{CSIDL_COMMON_APPDATA} and
  @code{CSIDL_COMMON_DOCUMENTS}.

  Then follows the \"share\" subfolder in the installation folder for the
  package containing the DLL that calls this function, if it can be
  determined.

  Finally the list contains the \"share\" subfolder in the installation folder
  for GLib, and in the installation folder for the package the application's
  .exe file belongs to.

  The installation folders above are determined by looking up the folder where
  the module (DLL or EXE) in question is located. If the folder's name is
  \"bin\", its parent is used, otherwise the folder itself.

  Note that on Windows the returned list can vary depending on where this
  function is called.

  Since 2.6
  @begin[Example]{dictionary}
    Result for a call on a LINUX system.
    @begin{pre}
 (g-get-system-data-dirs)
=> (\"/usr/share/ubuntu\" \"/usr/share/gnome\"
    \"/usr/local/share/\" \"/usr/share/\")
    @end{pre}
  @end{dictionary}")

(setf (documentation 'g-get-system-config-dirs 'function)
 "@version{2012-12-31}
  @return{A list of strings owned by GLib that must not be modified or freed}
  @short{Returns an ordered list of base directories in which to access
    system-wide configuration information.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the list of directories
  retrieved will be @code{XDG_CONFIG_DIRS}.

  On Windows is the directory that contains application data for all users. A
  typical path is
  @file{C:\Documents and Settings\All Users\Application Data}. This folder is
  used for application data that is not user specific. For example, an
  application can store a spell-check dictionary, a database of clip art, or a
  log file in the @code{CSIDL_COMMON_APPDATA} folder. This information will not
  roam and is available to anyone using the computer.
 
  Since 2.6")

(setf (documentation 'g-get-host-name 'function)
 "@version{2012-12-31}
  @return{The host name of the machine.}
  @short{Return a name for the machine.}

  The returned name is not necessarily a fully-qualified domain name, or even
  present in DNS or some other name service at all. It need not even be unique
  on your local network or site, but usually it is. Callers should not rely on
  the return value having any specific properties like uniqueness for security
  purposes. Even if the name of the machine is changed while an application is
  running, the return value from this function does not change. The returned
  string is owned by GLib and should not be modified or freed. If no name can
  be determined, a default fixed string \"localhost\" is returned.")

(setf (documentation 'g-get-home-dir 'function)
 "@version{2012-12-31}
  @return{The current user's home directory.}
  @short{Gets the current user's home directory as defined in the password
    database.}

  Note that in contrast to traditional UNIX tools, this function prefers
  passwd entries over the @code{HOME} environment variable.
 
  One of the reasons for this decision is that applications in many cases need
  special handling to deal with the case where @code{HOME} is
  @begin{itemize}
    @item{Not owned by the user}
    @item{Not writeable}
    @item{Not even readable}
  @end{itemize}
  Since applications are in general not written to deal with these situations
  it was considered better to make @sym{g-get-home-dir} not pay attention to
  @code{HOME} and to return the real home directory for the user. If
  applications want to pay attention to @code{HOME}, they can do:
  @begin{pre}
  const char *homedir = g_getenv (\"HOME\");
  if (!homedir)
     homedir = g_get_home_dir ();
  @end{pre}")

(setf (documentation 'g-get-tmp-dir 'function)
 "@version{2012-12-31}
  @return{The directory to use for temporary files.}
  @short{Gets the directory to use for temporary files.}
  This is found from inspecting the environment variables @code{TMPDIR},
  @code{TMP}, and @code{TEMP} in that order. If none of those are defined
  @file{\"/tmp\"} is returned on UNIX and @file{\"C:\\\"} on Windows. The
  encoding of the returned string is system-defined. On Windows, it is always
  UTF-8. The return value is never NULL or the empty string.")

(setf (documentation 'g-get-current-dir 'function)
 "@version{2012-12-31}
  @return{The current directory.}
  @short{Gets the current directory.}

  The returned string should be freed when no longer needed. The encoding of
  the returned string is system defined. On Windows, it is always UTF-8.")

(setf (documentation 'g-path-is-absolute 'function)
 "@version{2012-12-31}
  @argument[file-name]{a file name}
  @return{@arg{true} if file_name is absolute.}
  @begin{short}
    Returns @arg{true} if the given file_name is an absolute file name.
  @end{short}
  Note that this is a somewhat vague concept on Windows.
 
  On POSIX systems, an absolute file name is well-defined. It always starts
  from the single root directory. For example @file{\"/usr/local\"}.

  On Windows, the concepts of current drive and drive-specific current
  directory introduce vagueness. This function interprets as an absolute file
  name one that either begins with a directory separator such as
  @file{\"\Users\tml\"} or begins with the root on a drive, for example
  @file{\"C:\Windows\"}. The first case also includes UNC paths such as
  @file{\"\\myserver\docs\foo\"}. In all cases, either slashes or backslashes
  are accepted.

  Note that a file name relative to the current drive root does not truly
  specify a file uniquely over time and across processes, as the current drive
  is a per-process value and can be changed.
 
  File names relative the current directory on some specific drive, such as
  @file{\"D:foo/bar\"}, are not interpreted as absolute by this function, but
  they obviously are not relative to the normal current directory as returned by
  @code{getcwd()} or @fun{g-get-current-dir} either. Such paths should be
  avoided, or need to be handled using Windows-specific code.
  @see-function{g-get-current-dir}")

(setf (documentation 'g-build-filename 'function)
 "@version{2012-12-31}
  @argument[args]{elements in path}
  @return{A newly-allocated string that must be freed with @code{g_free()}.}
  @begin{short}
    Creates a filename from a series of elements using the correct separator for
    filenames.
  @end{short}

  On Unix, this function behaves identically to
  @code{g_build_path (G_DIR_SEPARATOR_S, first_element, ....)}.

  On Windows, it takes into account that either the backslash (\\) or slash (/)
  can be used as separator in filenames, but otherwise behaves as on Unix.
  When file pathname separators need to be inserted, the one that last
  previously occurred in the parameters (reading from left to right) is used.

  No attempt is made to force the resulting filename to be an absolute path.
  If the first element is a relative path, the result will be a relative path.")

(setf (documentation 'g-build-filenamev 'function)
 "@version{2012-12-23}
  @argument[args]{array of strings containing the path elements}
  @return{a newly-allocated string that must be freed with @fun{g-free}.}
  @begin{short}
    Behaves exactly like @fun{g-build-filename}, but takes the path elements as
    a string array, instead of varargs.
  @end{short}
  This function is mainly meant for language bindings.
 
  Since 2.8")

;;; --- End of file atdoc-glib.utils.lisp --------------------------------------

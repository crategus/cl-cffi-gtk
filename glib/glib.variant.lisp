;;; ----------------------------------------------------------------------------
;;; glib.variant.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.66 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GVariant
;;;
;;;     Strongly typed value datatype
;;;
;;; Types and Values
;;;
;;;     GVariant
;;;     GVariantClass
;;;     GVariantIter
;;;     GVariantBuilder
;;;     GVariantDict
;;;     GVariantParseError
;;;
;;;     G_VARIANT_PARSE_ERROR
;;;
;;; Functions
;;;
;;;     g_variant_unref
;;;     g_variant_ref
;;;     g_variant_ref_sink
;;;     g_variant_is_floating
;;;     g_variant_take_ref
;;;     g_variant_get_type
;;;     g_variant_get_type_string
;;;     g_variant_is_of_type
;;;     g_variant_is_container
;;;     g_variant_compare
;;;     g_variant_classify
;;;
;;;     g_variant_check_format_string
;;;     g_variant_get
;;;     g_variant_get_va
;;;
;;;     g_variant_new
;;;     g_variant_new_va
;;;     g_variant_new_boolean
;;;     g_variant_new_byte
;;;     g_variant_new_int16
;;;     g_variant_new_uint16
;;;     g_variant_new_int32
;;;     g_variant_new_uint32
;;;     g_variant_new_int64
;;;     g_variant_new_uint64
;;;     g_variant_new_handle
;;;     g_variant_new_double
;;;     g_variant_new_string
;;;
;;;     g_variant_new_take_string
;;;     g_variant_new_printf
;;;
;;;     g_variant_new_object_path
;;;     g_variant_is_object_path
;;;     g_variant_new_signature
;;;     g_variant_is_signature
;;;     g_variant_new_variant
;;;     g_variant_new_strv
;;;     g_variant_new_objv
;;;     g_variant_new_bytestring
;;;     g_variant_new_bytestring_array
;;;
;;;     g_variant_get_boolean
;;;     g_variant_get_byte
;;;     g_variant_get_int16
;;;     g_variant_get_uint16
;;;     g_variant_get_int32
;;;     g_variant_get_uint32
;;;     g_variant_get_int64
;;;     g_variant_get_uint64
;;;     g_variant_get_handle
;;;     g_variant_get_double
;;;     g_variant_get_string
;;;     g_variant_dup_string
;;;     g_variant_get_variant
;;;     g_variant_get_strv
;;;     g_variant_dup_strv
;;;     g_variant_get_objv
;;;     g_variant_dup_objv
;;;     g_variant_get_bytestring
;;;     g_variant_dup_bytestring
;;;     g_variant_get_bytestring_array
;;;     g_variant_dup_bytestring_array
;;;
;;;     g_variant_new_maybe
;;;     g_variant_new_array
;;;     g_variant_new_tuple
;;;     g_variant_new_dict_entry
;;;     g_variant_new_fixed_array
;;;
;;;     g_variant_get_maybe
;;;     g_variant_n_children
;;;     g_variant_get_child_value
;;;     g_variant_get_child
;;;     g_variant_lookup_value
;;;     g_variant_lookup
;;;     g_variant_get_fixed_array
;;;
;;;     g_variant_get_size
;;;     g_variant_get_data
;;;     g_variant_get_data_as_bytes
;;;     g_variant_store
;;;     g_variant_new_from_data
;;;     g_variant_new_from_bytes
;;;     g_variant_byteswap
;;;     g_variant_get_normal_form
;;;     g_variant_is_normal_form
;;;
;;;     g_variant_hash
;;;     g_variant_equal
;;;
;;;     g_variant_print
;;;     g_variant_print_string
;;;
;;;     g_variant_iter_copy
;;;     g_variant_iter_free
;;;     g_variant_iter_init
;;;     g_variant_iter_n_children
;;;     g_variant_iter_new
;;;     g_variant_iter_next_value
;;;     g_variant_iter_next
;;;     g_variant_iter_loop
;;;
;;;     G_VARIANT_BUILDER_INIT
;;;     g_variant_builder_unref
;;;     g_variant_builder_ref
;;;     g_variant_builder_new
;;;     g_variant_builder_init
;;;     g_variant_builder_clear
;;;     g_variant_builder_add_value
;;;     g_variant_builder_add
;;;     g_variant_builder_add_parsed
;;;     g_variant_builder_end
;;;     g_variant_builder_open
;;;     g_variant_builder_close
;;;
;;;     G_VARIANT_DICT_INIT
;;;     g_variant_dict_unref
;;;     g_variant_dict_ref
;;;     g_variant_dict_new
;;;     g_variant_dict_init
;;;     g_variant_dict_clear
;;;     g_variant_dict_contains
;;;     g_variant_dict_lookup
;;;     g_variant_dict_lookup_value
;;;     g_variant_dict_insert
;;;     g_variant_dict_insert_value
;;;     g_variant_dict_remove
;;;     g_variant_dict_end
;;;
;;;     g_variant_parse
;;;     g_variant_new_parsed_va
;;;     g_variant_new_parsed
;;;
;;;     g_variant_parse_error_print_context
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GVariant
;;; ----------------------------------------------------------------------------

(defcstruct g-variant)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-variant atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-variant 'type)
 "@version{2020-11-30}
  @begin{short}
    The @sym{g-variant} structure is a variant datatype.
  @end{short}
  It stores a value along with information about the type of that value. The
  range of possible values is determined by the type. The type system used by
  @sym{g-variant} instances is @class{g-variant-type}.

  @sym{g-variant} instances always have a type and a value, which are given at
  construction time. The type and value of a @sym{g-variant} instance can never
  change other than by the @sym{g-variant} instance itself being destroyed. A
  @sym{g-variant} instance cannot contain a pointer.

  A @sym{g-variant} instance is reference counted using the functions
  @fun{g-variant-ref} and @fun{g-variant-unref}. The @sym{g-variant} instance
  also has floating reference counts -- see the function
  @fun{g-variant-ref-sink}.

  The @sym{g-variant} structure is completely threadsafe. A @sym{g-variant}
  instance can be concurrently accessed in any way from any number of threads
  without problems.

  The @sym{g-variant} structure is heavily optimised for dealing with data in
  serialised form. It works particularly well with data located in memory-mapped
  files. It can perform nearly all deserialisation operations in a small
  constant time, usually touching only a single memory page. Serialised
  @sym{g-variant} data can also be sent over the network.

  The @sym{g-variant} structure is largely compatible with D-Bus. Almost all
  types of @sym{g-variant} instances can be sent over D-Bus. See the
  @class{g-variant-type} documentation for exceptions. However, @sym{g-variant}
  structures serialisation format is not the same as the serialisation format of
  a D-Bus message body: use @code{GDBusMessage}, in the gio library, for those.

  For space-efficiency, the @sym{g-variant} serialisation format does not
  automatically include the variant's length, type or endianness, which must
  either be implied from context (such as knowledge that a particular file
  format always contains a little-endian \"v\" which occupies the whole length
  of the file) or supplied out-of-band (for instance, a length, type and/or
  endianness indicator could be placed at the beginning of a file, network
  message or network stream).

  A @sym{g-variant} instance size is limited mainly by any lower level
  operating system constraints, such as the number of bits in @type{g-size}.
  For example, it is reasonable to have a 2 GB file mapped into memory with
  @code{GMappedFile}, and call the function @code{g_variant_new_from_data()} on
  it.

  For convenience to C programmers, the @sym{g-variant} values features
  powerful varargs-based value construction and destruction. This feature is
  designed to be embedded in other libraries.

  There is a Python-inspired text language for describing @sym{g-variant}
  values. The @sym{g-variant} structure includes a printer for this language
  and a parser with type inferencing.

  @subheading{Memory Use}
  @sym{g-variant} tries to be quite efficient with respect to memory use. This
  section gives a rough idea of how much memory is used by the current
  implementation. The information here is subject to change in the future.

  The memory allocated by @sym{g-variant} can be grouped into 4 broad purposes:
  memory for serialised data, memory for the type information cache, buffer
   management memory and memory for the @sym{g-variant} structure itself.

  @subheading{Serialised Data Memory}
  This is the memory that is used for storing @sym{g-variant} data in serialised
  form. This is what would be sent over the network or what would end up on
  disk.

  The amount of memory required to store a boolean is 1 byte. 16, 32 and 64
  bit integers and double precision floating point numbers use their \"natural\"
  size. Strings (including object path and signature strings) are stored with a
  nul terminator, and as such use the length of the string plus 1 byte.

  Maybe types use no space at all to represent the null value and use the same
  amount of space (sometimes plus one byte) as the equivalent non-maybe-typed
  value to represent the non-null case.

  Arrays use the amount of space required to store each of their members,
  concatenated. Additionally, if the items stored in an array are not of a
  fixed-size (i.e. strings, other arrays, etc) then an additional framing
  offset is stored for each item. The size of this offset is either 1, 2 or 4
  bytes depending on the overall size of the container. Additionally, extra
  padding bytes are added as required for alignment of child values.

  Tuples (including dictionary entries) use the amount of space required to
  store each of their members, concatenated, plus one framing offset (as per
  arrays) for each non-fixed-sized item in the tuple, except for the last one.
  Additionally, extra padding bytes are added as required for alignment of
  child values.

  Variants use the same amount of space as the item inside of the variant, plus
  1 byte, plus the length of the type string for the item inside the variant.

  As an example, consider a dictionary mapping strings to variants. In the
  case that the dictionary is empty, 0 bytes are required for the serialisation.

  If we add an item \"width\" that maps to the int32 value of 500 then we will
  use 4 byte to store the int32 (so 6 for the variant containing it) and 6
  bytes for the string. The variant must be aligned to 8 after the 6 bytes of
  the string, so that's 2 extra bytes. 6 (string) + 2 (padding) + 6 (variant)
  is 14 bytes used for the dictionary entry. An additional 1 byte is added to
  the array as a framing offset making a total of 15 bytes.

  If we add another entry, \"title\" that maps to a nullable string that
  happens to have a value of null, then we use 0 bytes for the null value (and
  3 bytes for the variant to contain it along with its type string) plus 6
  bytes for the string. Again, we need 2 padding bytes. That makes a total of
  6 + 2 + 3 = 11 bytes.

  We now require extra padding between the two items in the array. After the
  14 bytes of the first item, that's 2 bytes required. We now require 2
  framing offsets for an extra two bytes. 14 + 2 + 11 + 2 = 29 bytes to encode
  the entire two-item dictionary.

  @subheading{Type Information Cache}
  For each @sym{g-variant} type that currently exists in the program a type
  information structure is kept in the type information cache. The type
  information structure is required for rapid deserialisation.

  Continuing with the above example, if a GVariant exists with the type
  \"a{sv@}\" then a type information struct will exist for \"a{sv@}\",
  \"{sv@}\", \"s\", and \"v\". Multiple uses of the same type will share the
  same type information. Additionally, all single-digit types are stored in
  read-only static memory and do not contribute to the writable memory
  footprint of a program using @sym{g-variant}.

  Aside from the type information structures stored in read-only memory, there
  are two forms of type information. One is used for container types where
  there is a single element type: arrays and maybe types. The other is used
  for container types where there are multiple element types: tuples and
  dictionary entries.

  Array type info structures are 6 * @code{sizeof (void *)}, plus the memory
  required to store the type string itself. This means that on 32 bit systems,
  the cache entry for \"a{sv@}\" would require 30 bytes of memory (plus malloc
  overhead).

  Tuple type info structures are 6 * @code{sizeof (void *)}, plus 4 *
  @code{sizeof (void *)} for each item in the tuple, plus the memory required
  to store the type string itself. A 2-item tuple, for example, would have a
  type information structure that consumed writable memory in the size of 14
  * @code{sizeof (void *)} (plus type string) This means that on 32 bit
  systems, the cache entry for \"{sv@}\" would require 61 bytes of memory
  (plus malloc overhead).

  This means that in total, for our \"a{sv@}\" example, 91 bytes of type
  information would be allocated.

  The type information cache, additionally, uses a @code{GHashTable} to store
  and lookup the cached items and stores a pointer to this hash table in
  static storage. The hash table is freed when there are zero items in the
  type cache.

  Although these sizes may seem large it is important to remember that a
  program will probably only have a very small number of different types of
  values in it and that only one type information structure is required for
  many different values of the same type.

  @subheading{Buffer Management Memory}
  @sym{g-variant} uses an internal buffer management structure to deal with
  the various different possible sources of serialised data that it uses. The
  buffer is responsible for ensuring that the correct call is made when the
  data is no longer in use by a @sym{g-variant} instance. This may involve the
  functions @fun{g-free} or a @code{g_slice_free()} or even
  @code{g_mapped_file_unref()}.

  One buffer management structure is used for each chunk of serialised data.
  The size of the buffer management structure is 4 * @code{(void *)}. On
  32 bit systems, that's 16 bytes.

  @subheading{GVariant structure}
  The size of a @sym{g-variant} structure is 6 * @code{(void *)}. On 32 bit
  systems, that's 24 bytes.

  @sym{g-variant} structures only exist if they are explicitly created with
  API calls. For example, if a @sym{g-variant} is constructed out of
  serialised data for the example given above (with the dictionary) then
  although there are 9 individual values that comprise the entire dictionary
  (two keys, two values, two variants containing the values, two dictionary
  entries, plus the dictionary itself), only 1 @sym{g-variant} instance exists
  -- the one referring to the dictionary.

  If calls are made to start accessing the other values then @sym{g-variant}
  instances will exist for those values only for as long as they are in use
  (i.e. until you call the function @fun{g-variant-unref}). The type
  information is shared. The serialised data and the buffer management
  structure for that serialised data is shared by the child.

  @subheading{Summary}
  To put the entire example together, for our dictionary mapping strings to
  variants (with two entries, as given above), we are using 91 bytes of memory
  for type information, 29 byes of memory for the serialised data, 16 bytes
  for buffer management and 24 bytes for the @sym{g-variant} instance, or a
  total of 160 bytes, plus malloc overhead. If we were to use the function
  @code{g_variant_get_child_value()} to access the two dictionary entries, we
  would use an additional 48 bytes. If we were to have other dictionaries of
  the same type, we would use more memory for the serialised data and buffer
  management for those dictionaries, but the type information would be shared.
  @see-class{g-variant-type}
  @see-function{g-variant-ref}
  @see-function{g-variant-unref}")

(export 'g-variant)

;;; ----------------------------------------------------------------------------
;;; enum GVariantClass
;;; ----------------------------------------------------------------------------

(defcenum g-variant-class
  (:boolean     #.(char-code #\b))
  (:byte        #.(char-code #\y))
  (:int16       #.(char-code #\n))
  (:uint16      #.(char-code #\q))
  (:int32       #.(char-code #\i))
  (:uint32      #.(char-code #\u))
  (:int64       #.(char-code #\x))
  (:uint64      #.(char-code #\t))
  (:handle      #.(char-code #\h))
  (:double      #.(char-code #\d))
  (:string      #.(char-code #\s))
  (:object-path #.(char-code #\o))
  (:signature   #.(char-code #\g))
  (:variant     #.(char-code #\v))
  (:maybe       #.(char-code #\m))
  (:array       #.(char-code #\a))
  (:tuple       #.(char-code #\())
  (:dict-entry  #.(char-code #\{)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-variant-class atdoc:*symbol-name-alias*) "Enum"
      (gethash 'g-variant-class atdoc:*external-symbols*)
 "@version{2020-12-1}
  @short{The range of possible toplevel types of @type{g-variant} instances.}
  @begin{pre}
(defcenum g-variant-class
  (:boolean     #.(char-code #\b))
  (:byte        #.(char-code #\y))
  (:int16       #.(char-code #\n))
  (:uint16      #.(char-code #\q))
  (:int32       #.(char-code #\i))
  (:uint32      #.(char-code #\u))
  (:int64       #.(char-code #\x))
  (:uint64      #.(char-code #\t))
  (:handle      #.(char-code #\h))
  (:double      #.(char-code #\d))
  (:string      #.(char-code #\s))
  (:object-path #.(char-code #\o))
  (:signature   #.(char-code #\g))
  (:variant     #.(char-code #\v))
  (:maybe       #.(char-code #\m))
  (:array       #.(char-code #\a))
  (:tuple       #.(char-code #\())
  (:dict-entry  #.(char-code #\{)))
  @end{pre}
  @begin[code]{table}
    @entry[:boolean]{The @type{g-variant} is a boolean.}
    @entry[:byte]{The @type{g-variant} is a byte.}
    @entry[:int16]{The @type{g-variant} is a signed 16 bit integer.}
    @entry[:uint16]{The @type{g-variant} is an unsigned 16 bit integer.}
    @entry[:int32]{The @type{g-variant} is a signed 32 bit integer.}
    @entry[:unit32]{The @type{g-variant} is an unsigned 32 bit integer.}
    @entry[:int64]{The @type{g-variant} is a signed 64 bit integer.}
    @entry[:uint64]{The @type{g-variant} is an unsigned 64 bit integer.}
    @entry[:handle]{The @type{g-variant} is a file handle index.}
    @entry[:double]{The @type{g-variant} is a double precision floating point
      value.}
    @entry[:string]{The @type{g-variant} is a normal string.}
    @entry[:object-path]{The @type{g-variant} is a D-Bus object path string.}
    @entry[:signature]{The @type{g-variant} is a D-Bus signature string.}
    @entry[:variant]{The @type{g-variant} is a variant.}
    @entry[:maybe]{The @type{g-variant} is a maybe-typed value.}
    @entry[:array]{The @type{g-variant} is an array.}
    @entry[:tuple]{The @type{g-variant} is a tuple.}
    @entry[:dict-entry]{The @type{g-variant} is a dictionary entry.}
  @end{table}
  @see-type{g-variant}")

(export 'g-variant-class)

;;; ----------------------------------------------------------------------------
;;; struct GVariantIter
;;;
;;; struct GVariantIter {
;;; };
;;;
;;; GVariantIter is an opaque data structure and can only be accessed using the
;;; following functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GVariantBuilder
;;;
;;; struct GVariantBuilder {
;;; };
;;;
;;; A utility type for constructing container-type GVariant instances.
;;;
;;; This is an opaque structure and may only be accessed using the following
;;; functions.
;;;
;;; GVariantBuilder is not threadsafe in any way. Do not attempt to access it
;;; from more than one thread.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GVariantDict
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "g_variant_dict_get_type" g-size))

(gobject::define-g-boxed-opaque g-variant-dict "GVariantDict"
  :alloc (error "GVariantDict cannot be created from the Lisp side."))

;(defcstruct g-variant-dict)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-variant-dict atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'g-variant-dict 'type)
 "@version{2021-8-12}
  @begin{short}
    The @sym{g-variant-dict} structure is a mutable interface to
    @type{g-variant} dictionaries.
  @end{short}

  It can be used for doing a sequence of dictionary lookups in an efficient
  way on an existing @type{g-variant} dictionary or it can be used to construct
  new dictionaries with a hashtable-like interface. It can also be used for
  taking existing dictionaries and modifying them in order to create new ones.

  The @sym{g-variant-dict} structure can only be used with \"a(sv)\"
  dictionaries.

  It is possible to use @sym{g-variant-dict} structures allocated on the stack
  or on the heap. When using a stack-allocated @sym{g-variant-dict} structure,
  you begin with a call to the function @fun{g-variant-dict-init} and free the
  resources with a call to the function @fun{g-variant-dict-clear}.

  Heap-allocated @sym{g-variant-dict} structures follows normal refcounting
  rules: you allocate it with the function @fun{g-variant-dict-new} and use
  the functions @fun{g-variant-dict-ref} and @fun{g-variant-dict-unref}.

  The function @fun{g-variant-dict-end} is used to convert the
  @sym{g-variant-dict} structure back into a dictionary-type @type{g-variant}.
  When used with stack-allocated instances, this also implicitly frees all
  associated memory, but for heap-allocated instances, you must still call the
  function @fun{g-variant-dict-unref} afterwards.

  You will typically want to use a heap-allocated @sym{g-variant-dict} structure
  when you expose it as part of an API. For most other uses, the stack-allocated
  form will be more convenient.

  Consider the following two examples that do the same thing in each style:
  take an existing dictionary and look up the \"count\" @code{:uint32} key,
  adding 1 to it if it is found, or returning an error if the key is not found.
  Each returns the new dictionary as a floating @type{g-variant} instance.

  Using a stack-allocated @sym{gvariant-dict} instance:
  @begin{pre}
GVariant *
add_to_count (GVariant  *orig,
              GError   **error)
{
  GVariantDict dict;
  guint32 count;

  g_variant_dict_init (&dict, orig);
  if (!g_variant_dict_lookup (&dict, \"count\", \"u\", &count))
    {
      g_set_error (...);
      g_variant_dict_clear (&dict);
      return NULL;
    @}

  g_variant_dict_insert (&dict, \"count\", \"u\", count + 1);

  return g_variant_dict_end (&dict);
@}
  @end{pre}
  Using a heap-allocated @sym{g-variant-dict} instance:
  @begin{pre}
GVariant *
add_to_count (GVariant  *orig,
              GError   **error)
{
  GVariantDict *dict;
  GVariant *result;
  guint32 count;

  dict = g_variant_dict_new (orig);

  if (g_variant_dict_lookup (dict, \"count\", \"u\", &count))
    {
      g_variant_dict_insert (dict, \"count\", \"u\", count + 1);
      result = g_variant_dict_end (dict);
    @}
  else
    {
      g_set_error (...);
      result = NULL;
    @}

  g_variant_dict_unref (dict);

  return result;
@}
  @end{pre}
  @see-type{g-variant}")

(export 'g-variant-dict)

;;; ----------------------------------------------------------------------------
;;; enum GVariantParseError
;;;
;;; typedef enum {
;;;   G_VARIANT_PARSE_ERROR_FAILED,
;;;   G_VARIANT_PARSE_ERROR_BASIC_TYPE_EXPECTED,
;;;   G_VARIANT_PARSE_ERROR_CANNOT_INFER_TYPE,
;;;   G_VARIANT_PARSE_ERROR_DEFINITE_TYPE_EXPECTED,
;;;   G_VARIANT_PARSE_ERROR_INPUT_NOT_AT_END,
;;;   G_VARIANT_PARSE_ERROR_INVALID_CHARACTER,
;;;   G_VARIANT_PARSE_ERROR_INVALID_FORMAT_STRING,
;;;   G_VARIANT_PARSE_ERROR_INVALID_OBJECT_PATH,
;;;   G_VARIANT_PARSE_ERROR_INVALID_SIGNATURE,
;;;   G_VARIANT_PARSE_ERROR_INVALID_TYPE_STRING,
;;;   G_VARIANT_PARSE_ERROR_NO_COMMON_TYPE,
;;;   G_VARIANT_PARSE_ERROR_NUMBER_OUT_OF_RANGE,
;;;   G_VARIANT_PARSE_ERROR_NUMBER_TOO_BIG,
;;;   G_VARIANT_PARSE_ERROR_TYPE_ERROR,
;;;   G_VARIANT_PARSE_ERROR_UNEXPECTED_TOKEN,
;;;   G_VARIANT_PARSE_ERROR_UNKNOWN_KEYWORD,
;;;   G_VARIANT_PARSE_ERROR_UNTERMINATED_STRING_CONSTANT,
;;;   G_VARIANT_PARSE_ERROR_VALUE_EXPECTED,
;;;   G_VARIANT_PARSE_ERROR_RECURSION
;;; } GVariantParseError;
;;;
;;; Error codes returned by parsing text-format GVariants.
;;;
;;; G_VARIANT_PARSE_ERROR_FAILED
;;;     generic error (unused)
;;;
;;; G_VARIANT_PARSE_ERROR_BASIC_TYPE_EXPECTED
;;;     a non-basic GVariantType was given where a basic type was expected
;;;
;;; G_VARIANT_PARSE_ERROR_CANNOT_INFER_TYPE
;;;     cannot infer the GVariantType
;;;
;;; G_VARIANT_PARSE_ERROR_DEFINITE_TYPE_EXPECTED
;;;     an indefinite GVariantType was given where a definite type was expected
;;;
;;; G_VARIANT_PARSE_ERROR_INPUT_NOT_AT_END
;;;     extra data after parsing finished
;;;
;;; G_VARIANT_PARSE_ERROR_INVALID_CHARACTER
;;;     invalid character in number or unicode escape
;;;
;;; G_VARIANT_PARSE_ERROR_INVALID_FORMAT_STRING
;;;     not a valid GVariant format string
;;;
;;; G_VARIANT_PARSE_ERROR_INVALID_OBJECT_PATH
;;;     not a valid object path
;;;
;;; G_VARIANT_PARSE_ERROR_INVALID_SIGNATURE
;;;     not a valid type signature
;;;
;;; G_VARIANT_PARSE_ERROR_INVALID_TYPE_STRING
;;;     not a valid GVariant type string
;;;
;;; G_VARIANT_PARSE_ERROR_NO_COMMON_TYPE
;;;     could not find a common type for array entries
;;;
;;; G_VARIANT_PARSE_ERROR_NUMBER_OUT_OF_RANGE
;;;     the numerical value is out of range of the given type
;;;
;;; G_VARIANT_PARSE_ERROR_NUMBER_TOO_BIG
;;;     the numerical value is out of range for any type
;;;
;;; G_VARIANT_PARSE_ERROR_TYPE_ERROR
;;;     cannot parse as variant of the specified type
;;;
;;; G_VARIANT_PARSE_ERROR_UNEXPECTED_TOKEN
;;;     an unexpected token was encountered
;;;
;;; G_VARIANT_PARSE_ERROR_UNKNOWN_KEYWORD
;;;     an unknown keyword was encountered
;;;
;;; G_VARIANT_PARSE_ERROR_UNTERMINATED_STRING_CONSTANT
;;;     unterminated string constant
;;;
;;; G_VARIANT_PARSE_ERROR_VALUE_EXPECTED
;;;     no value given
;;;
;;; G_VARIANT_PARSE_ERROR_RECURSION
;;;     variant was too deeply nested; GVariant is only guaranteed to handle
;;;     nesting up to 64 levels (Since 2.64)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_PARSE_ERROR
;;;
;;; #define G_VARIANT_PARSE_ERROR (g_variant_parser_get_error_quark ())
;;;
;;; Error domain for GVariant text format parsing. Specific error codes are not
;;; currently defined for this domain. See GError for information on error
;;; domains.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_unref" g-variant-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @begin{short}
    Decreases the reference count of @arg{value}.
  @end{short}
  When its reference count drops to 0, the memory used by the variant is freed.
  @see-type{g-variant}
  @see-function{g-variant-ref}
  @see-function{g-variant-ref-sink}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-unref)

;;; ----------------------------------------------------------------------------
;;; g_variant_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_ref" g-variant-ref) (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @return{The same @arg{value}.}
  @short{Increases the reference count of @arg{value}.}
  @see-type{g-variant}
  @see-function{g-variant-unref}
  @see-function{g-variant-ref-sink}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-ref)

;;; ----------------------------------------------------------------------------
;;; g_variant_ref_sink ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_ref_sink" g-variant-ref-sink) (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @return{The same @arg{value}.}
  @begin{short}
    The @type{g-variant} structure uses a floating reference count system. All
    functions with names starting with @sym{g_variant_new_} return floating
    references.
  @end{short}

  Calling the function @sym{g-variant-ref-sink} on a @type{g-variant} instance
  with a floating reference will convert the floating reference into a full
  reference. Calling the function @sym{g-variant-ref-sink} on a non-floating
  @type{g-variant} instance results in an additional normal reference being
  added.

  In other words, if @arg{value} is floating, then this call
  \"assumes ownership\" of the floating reference, converting it to a normal
  reference. If @arg{value} is not floating, then this call adds a new normal
  reference increasing the reference count by one.

  All calls that result in a @type{g-variant} instance being inserted into a
  container will call the function @sym{g-variant-ref-sink} on the instance.
  This means that if the value was just created (and has only its floating
  reference) then the container will assume sole ownership of the value at that
  point and the caller will not need to unreference it. This makes certain
  common styles of programming much easier while still maintaining normal
  refcounting semantics in situations where values are not floating.
  @see-type{g-variant}
  @see-function{g-variant-ref}
  @see-function{g-variant-unref}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-ref-sink)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_floating ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_is_floating" g-variant-is-floating) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @return{A boolean whether @arg{value} is floating.}
  @begin{short}
    Checks whether @arg{value} has a floating reference count.
  @end{short}

  This function should only ever be used to assert that a given variant is or
  is not floating, or for debug purposes. To acquire a reference to a variant
  that might be floating, always use the functions @fun{g-variant-ref-sink} or
  @fun{g-variant-take-ref}.

  See the function @fun{g-variant-ref-sink} for more information about floating
  reference counts.
  @see-type{g-variant}
  @see-function{g-variant-ref-sink}
  @see-function{g-variant-take-ref}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-is-floating)

;;; ----------------------------------------------------------------------------
;;; g_variant_take_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_take_ref" g-variant-take-ref)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @return{The same @arg{value}.}
  @begin{short}
    If @arg{value} is floating, sink it. Otherwise, do nothing.
  @end{short}

  Typically you want to use the function @fun{g-variant-ref-sink} in order to
  automatically do the correct thing with respect to floating or non-floating
  references, but there is one specific scenario where this function is helpful.

  The situation where this function is helpful is when creating an API that
  allows the user to provide a callback function that returns a @type{g-variant}
  instance. We certainly want to allow the user the flexibility to return a
  non-floating reference from this callback (for the case where the value that
  is being returned already exists).

  At the same time, the style of the @type{g-variant} API makes it likely that
  for newly-created @type{g-variant} instances, the user can be saved some
  typing if they are allowed to return a @type{g-variant} with a floating
  reference.

  Using this function on the return value of the user's callback allows the
  user to do whichever is more convenient for them. The caller will alway
  receives exactly one full reference to the value: either the one that was
  returned in the first place, or a floating reference that has been converted
  to a full reference.

  This function has an odd interaction when combined with the function
  @fun{g-variant-ref-sink} running at the same time in another thread on the
  same @type{g-variant} instance. If the function @fun{g-variant-ref-sink}
  runs first then the result will be that the floating reference is converted to
  a hard reference. If the function @sym{g-variant-take-ref} runs first then the
  result will be that the floating reference is converted to a hard reference
  and an additional reference on top of that one is added. It is best to avoid
  this situation.
  @see-type{g-variant}
  @see-function{g-variant-ref-sink}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-take-ref)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_type () -> g-variant-type
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_type" g-variant-type)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @return{A @class{g-variant-type} structure.}
  @begin{short}
    Determines the type of @arg{value}.
  @end{short}
  The return value is valid for the lifetime of @arg{value} and must not be
  freed.
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-type)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_type_string () -> g-variant-type-string
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_type_string" g-variant-type-string)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{*2021-5-11}
  @argument[value]{a @type{g-variant} instance}
  @return{The type string for the type of @arg{value}.}
  @begin{short}
    Returns the type string of @arg{value}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(g-variant-type-string (g-variant-new-double 10.0d0)) => \"d\"
(g-variant-type-string (g-variant-new-string \"test\")) => \"s\"
    @end{pre}
  @end{dictionary}
  @see-type{g-variant}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-type-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_of_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_is_of_type" g-variant-is-of-type) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-30}
  @argument[value]{a @type{g-variant} instance}
  @argument[variant-type]{a @class{g-variant-type} structure}
  @return{@em{True} if the type of @arg{value} matches @arg{variant-type}.}
  @begin{short}
    Checks if a @arg{value} has a type matching the provided @arg{variant-type}.
  @end{short}
  @see-type{g-variant}
  @see-class{g-varian-type}"
  (value (:pointer (:struct g-variant)))
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-is-of-type)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_container ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_is_container" g-variant-is-container) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @type{g-variant} instance}
  @return{@em{True} if @arg{value} is a container.}
  @begin{short}
    Checks if @arg{value} is a container.
  @end{short}
  @see-type{g-variant}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-is-container)

;;; ----------------------------------------------------------------------------
;;; g_variant_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_compare" g-variant-compare) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value-1]{a basic-typed @type{g-variant} instance}
  @argument[value-2]{a @type{g-variant} instance of the same type}
  @return{Negative value if a < b; zero if a = b; positive value if a > b.}
  @begin{short}
    Compares @arg{value-1} and @arg{value-2}.
  @end{short}

  The types of @arg{value-1} and @arg{value-2} are @code{:pointer} only to
  allow use of this function with @code{GTree}, @code{GPtrArray}, etc. They
  must each be a @type{g-variant} instance.

  Comparison is only defined for basic types (i.e. booleans, numbers, strings).
  For booleans, @em{false} is less than @em{true}. Numbers are ordered in the
  usual way. Strings are in ASCII lexographical order.

  It is a programmer error to attempt to compare container values or two
  values that have types that are not exactly equal. For example, you cannot
  compare a 32-bit signed integer with a 32-bit unsigned integer. Also note
  that this function is not particularly well-behaved when it comes to
  comparison of doubles; in particular, the handling of incomparable values
  (i.e. NaN) is undefined.

  If you only require an equality comparison, the function @fun{g-variant-equal}
  is more general.
  @see-type{g-variant}
  @see-function{g-variant-equal}"
  (value-1 (:pointer (:struct g-variant)))
  (value-2 (:pointer (:struct g-variant))))

(export 'g-variant-compare)

;;; ----------------------------------------------------------------------------
;;; g_variant_classify ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_classify" g-variant-classify) g-variant-class
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @type{g-variant} instance}
  @return{The @symbol{g-variant-class} value of @arg{value}.}
  @begin{short}
    Classifies @arg{value} according to its toplevel type.
  @end{short}
  @see-type{g-variant}
  @see-symbol{g-variant-class}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-classify)

;;; ----------------------------------------------------------------------------
;;; g_variant_check_format_string ()
;;;
;;; gboolean g_variant_check_format_string (GVariant *value,
;;;                                         const gchar *format_string,
;;;                                         gboolean copy_only);
;;;
;;; Checks if calling g_variant_get() with format_string on value would be valid
;;; from a type-compatibility standpoint. format_string is assumed to be a valid
;;; format string (from a syntactic standpoint).
;;;
;;; If copy_only is TRUE then this function additionally checks that it would
;;; be safe to call g_variant_unref() on value immediately after the call to
;;; g_variant_get() without invalidating the result. This is only possible if
;;; deep copies are made (ie: there are no pointers to the data inside of the
;;; soon-to-be-freed GVariant instance). If this check fails then a g_critical()
;;; is printed and FALSE is returned.
;;;
;;; This function is meant to be used by functions that wish to provide varargs
;;; accessors to GVariant values of uncertain values (eg: g_variant_lookup() or
;;; g_menu_model_get_item_attribute()).
;;;
;;; value :
;;;     a GVariant
;;;
;;; format_string :
;;;     a valid GVariant format string
;;;
;;; copy_only :
;;;     TRUE to ensure the format string makes deep copies
;;;
;;; Returns :
;;;     TRUE if format_string is safe to use
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get ()
;;;
;;; void g_variant_get (GVariant *value, const gchar *format_string, ...);
;;;
;;; Deconstructs a GVariant instance.
;;;
;;; Think of this function as an analogue to scanf().
;;;
;;; The arguments that are expected by this function are entirely determined by
;;; format_string. format_string also restricts the permissible types of value.
;;; It is an error to give a value with an incompatible type. See the section on
;;; GVariant Format Strings. Please note that the syntax of the format string is
;;; very likely to be extended in the future.
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_va ()
;;;
;;; void g_variant_get_va (GVariant *value,
;;;                        const gchar *format_string,
;;;                        const gchar **endptr,
;;;                        va_list *app);
;;;
;;; This function is intended to be used by libraries based on GVariant that
;;; want to provide g_variant_get()-like functionality to their users.
;;;
;;; The API is more general than g_variant_get() to allow a wider range of
;;; possible uses.
;;;
;;; format_string must still point to a valid format string, but it only need
;;; to be nul-terminated if endptr is NULL. If endptr is non-NULL then it is
;;; updated to point to the first character past the end of the format string.
;;;
;;; app is a pointer to a va_list. The arguments, according to format_string,
;;; are collected from this va_list and the list is left pointing to the
;;; argument following the last.
;;;
;;; These two generalisations allow mixing of multiple calls to
;;; g_variant_new_va() and g_variant_get_va() within a single actual varargs
;;; call by the user.
;;;
;;; value :
;;;     a GVariant
;;;
;;; format_string :
;;;     a string that is prefixed with a format string
;;;
;;; endptr :
;;;     location to store the end pointer, or NULL
;;;
;;; app :
;;;     a pointer to a va_list
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new ()
;;;
;;; GVariant * g_variant_new (const gchar *format_string, ...);
;;;
;;; Creates a new GVariant instance.
;;;
;;; Think of this function as an analogue to g_strdup_printf().
;;;
;;; The type of the created instance and the arguments that are expected by this
;;; function are determined by format_string. See the section on GVariant Format
;;; Strings. Please note that the syntax of the format string is very likely to
;;; be extended in the future.
;;;
;;; The first character of the format string must not be '*' '?' '@' or 'r'; in
;;; essence, a new GVariant must always be constructed by this function (and not
;;; merely passed through it unmodified).
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_va ()
;;;
;;; GVariant * g_variant_new_va (const gchar *format_string,
;;;                              const gchar **endptr,
;;;                              va_list *app);
;;;
;;; This function is intended to be used by libraries based on GVariant that
;;; want to provide g_variant_new()-like functionality to their users.
;;;
;;; The API is more general than g_variant_new() to allow a wider range of
;;; possible uses.
;;;
;;; format_string must still point to a valid format string, but it only needs
;;; to be nul-terminated if endptr is NULL. If endptr is non-NULL then it is
;;; updated to point to the first character past the end of the format string.
;;;
;;; app is a pointer to a va_list. The arguments, according to format_string,
;;; are collected from this va_list and the list is left pointing to the
;;; argument following the last.
;;;
;;; These two generalisations allow mixing of multiple calls to
;;; g_variant_new_va() and g_variant_get_va() within a single actual varargs
;;; call by the user.
;;;
;;; The return value will be floating if it was a newly created GVariant
;;; instance (for example, if the format string was "(ii)"). In the case that
;;; the format_string was '*', '?', 'r', or a format starting with '@' then the
;;; collected GVariant pointer will be returned unmodified, without adding any
;;; additional references.
;;;
;;; In order to behave correctly in all cases it is necessary for the calling
;;; function to g_variant_ref_sink() the return result before returning control
;;; to the user that originally provided the pointer. At this point, the caller
;;; will have their own full reference to the result. This can also be done by
;;; adding the result to a container, or by passing it to another
;;; g_variant_new() call.
;;;
;;; format_string :
;;;     a string that is prefixed with a format string
;;;
;;; endptr :
;;;     location to store the end pointer, or NULL
;;;
;;; app :
;;;     a pointer to a va_list
;;;
;;; Returns :
;;;     a new, usually floating, GVariant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_boolean ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_boolean" g-variant-new-boolean)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-5}
  @argument[value]{a boolean value}
  @return{A floating reference to a new boolean @type{g-variant} instance.}
  @begin{short}
    Creates a new boolean @type{g-variant} instance -- either @em{true} or
    @em{false}.
  @end{short}
  @see-type{g-variant}"
  (value :boolean))

(export 'g-variant-new-boolean)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_byte ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_byte" g-variant-new-byte)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{guint8} value}
  @return{A floating reference to a new byte @type{g-variant} instance.}
  @short{Creates a new byte @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :uchar))

(export 'g-variant-new-byte)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int16 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_int16" g-variant-new-int16)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{gint16} value}
  @return{A floating reference to a new @code{int16} @type{g-variant} instance.}
  @short{Creates a new @code{int16} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :int16))

(export 'g-variant-new-int16)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint16 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_uint16" g-variant-new-uint16)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{guint16} value}
  @begin{return}
    A floating reference to a new @code{uint16} @type{g-variant} instance.
  @end{return}
  @short{Creates a new @code{uint16} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :uint16))

(export 'g-variant-new-uint16)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int32 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_int32" g-variant-new-int32)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{gint32} value}
  @return{A floating reference to a new @code{int32} @type{g-variant} instance.}
  @short{Creates a new @code{int32} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :int32))

(export 'g-variant-new-int32)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint32 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_uint32" g-variant-new-uint32)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{guint32} value}
  @begin{return}
    A floating reference to a new @code{uint32} @type{g-variant} instance.
  @end{return}
  @short{Creates a new @code{uint32} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :uint32))

(export 'g-variant-new-uint32)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_int64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_int64" g-variant-new-int64)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{gint64} value}
  @return{A floating reference to a new @code{int64} @type{g-variant} instance.}
  @short{Creates a new @code{int64} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :int64))

(export 'g-variant-new-int64)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_uint64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_uint64" g-variant-new-uint64)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{guint64} value}
  @begin{return}
    A floating reference to a new @code{uint64} @type{g-variant} instance.
  @end{return}
  @short{Creates a new @code{uint64} @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :uint64))

(export 'g-variant-new-uint64)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_handle ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_handle" g-variant-new-handle)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{gint32} value}
  @return{A floating reference to a new handle @type{g-variant} instance.}
  @short{Creates a new handle @type{g-variant} instance.}

  By convention, handles are indexes into an array of file descriptors that
  are sent alongside a D-Bus message. If you are not interacting with D-Bus,
  you probably do not need them.
  @see-type{g-variant}"
  (value :int32))

(export 'g-variant-new-handle)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_double" g-variant-new-double)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @code{gdouble} floating point value}
  @return{A floating reference to a new double @type{g-variant} instance.}
  @short{Creates a new double @type{g-variant} instance.}
  @see-type{g-variant}"
  (value :double))

(export 'g-variant-new-double)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_string" g-variant-new-string)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[string]{a normal utf8 nul-terminated string}
  @return{A floating reference to a new string @type{g-variant} instance.}
  @short{Creates a string @type{g-variant} with the contents of string.}
  The string must be valid utf8.
  @see-type{g-variant}"
  (value :string))

(export 'g-variant-new-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_take_string ()
;;;
;;; GVariant *
;;; g_variant_new_take_string (gchar *string);
;;;
;;; Creates a string GVariant with the contents of string .
;;;
;;; string must be valid UTF-8, and must not be NULL. To encode potentially-NULL
;;; strings, use this with g_variant_new_maybe().
;;;
;;; This function consumes string . g_free() will be called on string when it
;;; is no longer required.
;;;
;;; You must not modify or access string in any other way after passing it to
;;; this function. It is even possible that string is immediately freed.
;;;
;;; string :
;;;     a normal UTF-8 nul-terminated string
;;;
;;; Returns :
;;;     a floating reference to a new string GVariant instance.
;;;
;;; Since 2.38
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_printf ()
;;;
;;; GVariant *
;;; g_variant_new_printf (const gchar *format_string,
;;;                       ...);
;;;
;;; Creates a string-type GVariant using printf formatting.
;;;
;;; This is similar to calling g_strdup_printf() and then g_variant_new_string()
;;; but it saves a temporary variable and an unnecessary copy.
;;;
;;; format_string :
;;;     a printf-style format string
;;;
;;; ... :
;;;     arguments for format_string
;;;
;;; Returns :
;;;     a floating reference to a new string GVariant instance.
;;;
;;; Since 2.38
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_object_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_object_path" g-variant-new-object-path)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[object-path]{a string with a D-Bus object path}
  @return{A floating reference to a new object path @type{g-variant} instance.}
  @begin{short}
    Creates a D-Bus object path @type{g-variant} with the contents of
    @arg{string}.
  @end{short}
  The argument @arg{string} must be a valid D-Bus object path. Use the function
  @fun{g-variant-is-object-path} if you are not sure.
  @see-type{g-variant}
  @see-function{g-variant-is-object-path}"
  (object-path :string))

(export 'g-variant-new-object-path)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_object_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_is_object_path" g-variant-is-object-path) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[string]{a string with a D-Bus object path}
  @return{@em{True} if @arg{string} is a D-Bus object path.}
  @begin{short}
    Determines if a given @arg{string} is a valid D-Bus object path.
  @end{short}
  You should ensure that a @arg{string} is a valid D-Bus object path before
  passing it to the function @fun{g-variant-new-object-path}.

  A valid object path starts with '/' followed by zero or more sequences of
  characters separated by '/' characters. Each sequence must contain only the
  characters \"[A-Z][a-z][0-9]_\". No sequence (including the one following the
  final '/' character) may be empty.
  @see-type{g-variant}
  @see-function{g-variant-new-object-path}"
  (string :string))

(export 'g-variant-is-object-path)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_signature ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_signature" g-variant-new-signature)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[signature]{a string with a signature}
  @return{A floating reference to a new signature @type{g-variant} instance.}
  @begin{short}
    Creates a D-Bus type @type{g-variant} signature with the contents of
    @arg{string}.
  @end{short}
  The argument @arg{string} must be a valid D-Bus type signature. Use the
  function @fun{g-variant-is-signature} if you are not sure.
  @see-type{g-variant}
  @see-function{g-variant-is-signature}"
  (signature :string))

(export 'g-variant-new-signature)

;;; ----------------------------------------------------------------------------
;;; g_variant_is_signature ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_is_signature" g-variant-is-signature) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[string]{a normal C nul-terminated string}
  @return{@em{True} if @arg{string} is a D-Bus type signature.}
  @begin{short}
    Determines if a given @arg{string} is a valid D-Bus type signature.
  @end{short}
  You should ensure that a string is a valid D-Bus type signature before passing
  it to the function @fun{g-variant-new-signature}.

  D-Bus type signatures consist of zero or more definite @class{g-variant-type}
  strings in sequence.
  @see-type{g-variant}
  @see-class{g-variant-type}
  @see-function{g-variant-new-signature}"
  (string :string))

(export 'g-variant-is-signature)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_variant ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_new_variant" g-variant-new-variant)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a @type{g-variant} instance}
  @return{A floating reference to a new variant @type{g-variant} instance.}
  @begin{short}
    Boxes @arg{value}.
  @end{short}
  The result is a @type{g-variant} instance representing a variant containing
  the original value.

  If child is a floating reference, see the function @fun{g-variant-ref-sink},
  the new instance takes ownership of child.
  @see-type{g-variant}
  @see-function{g-variant-ref-sink}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-new-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_strv ()
;;;
;;; GVariant * g_variant_new_strv (const gchar * const *strv, gssize length);
;;;
;;; Constructs an array of strings GVariant from the given array of strings.
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_objv ()
;;;
;;; GVariant * g_variant_new_objv (const gchar * const *strv, gssize length);
;;;
;;; Constructs an array of object paths GVariant from the given array of
;;; strings.
;;;
;;; Each string must be a valid GVariant object path; see
;;; g_variant_is_object_path().
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_bytestring ()
;;;
;;; GVariant * g_variant_new_bytestring (const gchar *string);
;;;
;;; Creates an array-of-bytes GVariant with the contents of string. This
;;; function is just like g_variant_new_string() except that the string need not
;;; be valid utf8.
;;;
;;; The nul terminator character at the end of the string is stored in the
;;; array.
;;;
;;; string :
;;;     a normal nul-terminated string in no particular encoding
;;;
;;; Returns :
;;;     a floating reference to a new bytestring GVariant instance
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_bytestring_array ()
;;;
;;; GVariant * g_variant_new_bytestring_array (const gchar * const *strv,
;;;                                            gssize length);
;;;
;;; Constructs an array of bytestring GVariant from the given array of strings.
;;;
;;; If length is -1 then strv is NULL-terminated.
;;;
;;; strv :
;;;     an array of strings
;;;
;;; length :
;;;     the length of strv, or -1
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_boolean () -> g-variant-boolean
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_boolean" g-variant-boolean) :boolean
 #+cl-cffi-gtk-documentation
 "@version{*2021-5-11}
  @argument[value]{a boolean @type{g-variant} instance}
  @return{The boolean values @em{true} or @em{false}.}
  @short{Returns the boolean value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"b\".
  @begin[Example]{dictionary}
    @begin{pre}
(g-variant-boolean (g-variant-new-boolean nil)) => NIL
(g-variant-boolean (g-variant-new-boolean t)) => T
    @end{pre}
  @end{dictionary}
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-boolean)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_byte () -> g-variant-byte
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_byte" g-variant-byte) :uchar
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a byte @type{g-variant} instance}
  @return{A @code{:uchar} value.}
  @short{Returns the byte value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string  \"y\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-byte)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int16 () -> g-variant-int16
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_int16" g-variant-int16) :int16
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 16-bit signed integer @type{g-variant} instance}
  @return{A @code{:int16} value.}
  @short{Returns the 16-bit signed integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"n\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-int16)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint16 () -> g-variant-uint16
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_uint16" g-variant-uint16) :uint16
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 16-bit unsigned integer @type{g-variant} instance}
  @return{A @code{:uint16} value.}
  @short{Returns the 16-bit unsigned integer value of value.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"q\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-uint16)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int32 () -> g-variant-int32
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_int32" g-variant-int32) :int32
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 32-bit signed integer @type{g-variant} instance}
  @return{A @code{:int32} value.}
  @short{Returns the 32-bit signed integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"i\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-int32)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint32 () -> g-variant-uint32
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_uint32" g-variant-uint32) :uint32
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 32-bit unsigned integer @type{g-variant} instance}
  @return{A @code{:uint32} value.}
  @short{Returns the 32-bit unsigned integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"u\".
  @see-symbol{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-uint32)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_int64 () -> g-variant-int64
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_int64" g-variant-int64) :int64
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 64-bit signed integer @type{g-variant} instance}
  @return{A @code{:int64} value.}
  @short{Returns the 64-bit signed integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"x\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-int64)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_uint64 () -> g-variant-uint64
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_uint64" g-variant-uint64) :uint64
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a 64-bit unsigned integer @type{g-variant} instance}
  @return{A @code{:uint64} value.}
  @short{Returns the 64-bit unsigned integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"t\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-uint64)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_handle () -> g-variant-handle
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_handle" g-variant-handle) :int32
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a handle @type{g-variant} instance}
  @return{A @code{:int32} value.}
  @short{Returns the 32-bit signed integer value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"h\".

  By convention, handles are indexes into an array of file descriptors that
  are sent alongside a D-Bus message. If you are not interacting with D-Bus,
  you probably do not need them.
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-handle)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_double () -> g-variant-double
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_double" g-variant-double) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a double float @type{g-variant} instance}
  @return{A double float value.}
  @short{Returns the double precision floating point value of @arg{value}.}
  It is an error to call this function with a value of any type other than
  a @class{g-variant-type} with the type string \"d\".
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (value (:pointer (:struct g-variant))))

(export 'g-variant-double)

;;; ----------------------------------------------------------------------------
;;; g_variant_get_string () -> g-variant-string
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_get_string" %g-variant-string) :string
  (value (:pointer (:struct g-variant)))
  (length :pointer))

(defun g-variant-string (value)
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value]{a string @type{g-variant} instance}
  @return{The constant string, utf8 encoded.}
  @begin{short}
    Returns the string value of a @type{g-variant} instance with a string
    type.
  @end{short}
  This includes the @class{g-variant-type} types with the type strings \"s\",
  \"o\", and \"g\". The string will always be utf8 encoded.

  It is an error to call this function with a value of any type other than
  those three. The return value remains valid as long as @arg{value} exists.
  @see-type{g-variant}
  @see-class{g-variant-type}"
  (%g-variant-string value (null-pointer)))

(export 'g-variant-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_string ()
;;;
;;; gchar * g_variant_dup_string (GVariant *value, gsize *length);
;;;
;;; Similar to g_variant_get_string() except that instead of returning a
;;; constant string, the string is duplicated.
;;;
;;; The string will always be utf8 encoded.
;;;
;;; The return value must be freed using g_free().
;;;
;;; value :
;;;     a string GVariant instance
;;;
;;; length :
;;;     a pointer to a gsize, to store the length
;;;
;;; Returns :
;;;     a newly allocated string, utf8 encoded
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_variant ()
;;;
;;; GVariant * g_variant_get_variant (GVariant *value);
;;;
;;; Unboxes value. The result is the GVariant instance that was contained in
;;; value.
;;;
;;; value :
;;;     a variant GVariant instance
;;;
;;; Returns :
;;;     the item contained in the variant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_strv ()
;;;
;;; const gchar ** g_variant_get_strv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of strings GVariant. This call makes a shallow
;;; copy; the return result should be released with g_free(), but the individual
;;; strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of strings GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_strv ()
;;;
;;; gchar ** g_variant_dup_strv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of strings GVariant. This call makes a deep
;;; copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of strings GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_objv ()
;;;
;;; const gchar ** g_variant_get_objv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of object paths GVariant. This call makes a
;;; shallow copy; the return result should be released with g_free(), but the
;;; individual strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of object paths GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_objv ()
;;;
;;; gchar ** g_variant_dup_objv (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of object paths GVariant. This call makes a
;;; deep copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of object paths GVariant
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_bytestring ()
;;;
;;; const gchar * g_variant_get_bytestring (GVariant *value);
;;;
;;; Returns the string value of a GVariant instance with an array-of-bytes type.
;;; The string has no particular encoding.
;;;
;;; If the array does not end with a nul terminator character, the empty string
;;; is returned. For this reason, you can always trust that a non-NULL
;;; nul-terminated string will be returned by this function.
;;;
;;; If the array contains a nul terminator character somewhere other than the
;;; last byte then the returned string is the string, up to the first such nul
;;; character.
;;;
;;; It is an error to call this function with a value that is not an array of
;;; bytes.
;;;
;;; The return value remains valid as long as value exists.
;;;
;;; value :
;;;     an array-of-bytes GVariant instance
;;;
;;; Returns :
;;;     the constant string
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_bytestring ()
;;;
;;; gchar * g_variant_dup_bytestring (GVariant *value, gsize *length);
;;;
;;; Similar to g_variant_get_bytestring() except that instead of returning a
;;; constant string, the string is duplicated.
;;;
;;; The return value must be freed using g_free().
;;;
;;; value :
;;;     an array-of-bytes GVariant instance
;;;
;;; length :
;;;     a pointer to a gsize, to store the length (not including the nul
;;;     terminator)
;;;
;;; Returns :
;;;     a newly allocated string
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_bytestring_array ()
;;;
;;; const gchar ** g_variant_get_bytestring_array (GVariant *value,
;;;                                                gsize *length);
;;;
;;; Gets the contents of an array of array of bytes GVariant. This call makes a
;;; shallow copy; the return result should be released with g_free(), but the
;;; individual strings must not be modified.
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of array of bytes GVariant ('aay')
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of constant strings
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dup_bytestring_array ()
;;;
;;; gchar ** g_variant_dup_bytestring_array (GVariant *value, gsize *length);
;;;
;;; Gets the contents of an array of array of bytes GVariant. This call makes a
;;; deep copy; the return result should be released with g_strfreev().
;;;
;;; If length is non-NULL then the number of elements in the result is stored
;;; there. In any case, the resulting array will be NULL-terminated.
;;;
;;; For an empty array, length will be set to 0 and a pointer to a NULL pointer
;;; will be returned.
;;;
;;; value :
;;;     an array of array of bytes GVariant ('aay')
;;;
;;; length :
;;;     the length of the result, or NULL
;;;
;;; Returns :
;;;     an array of strings
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_maybe ()
;;;
;;; GVariant * g_variant_new_maybe (const GVariantType *child_type,
;;;                                 GVariant *child);
;;;
;;; Depending on if child is NULL, either wraps child inside of a maybe
;;; container or creates a Nothing instance for the given type.
;;;
;;; At least one of child_type and child must be non-NULL. If child_type is
;;; non-NULL then it must be a definite type. If they are both non-NULL then
;;; child_type must be the type of child.
;;;
;;; If child is a floating reference (see g_variant_ref_sink()), the new
;;; instance takes ownership of child.
;;;
;;; child_type :
;;;     the GVariantType of the child, or NULL
;;;
;;; child :
;;;     the child value, or NULL
;;;
;;; Returns :
;;;     a floating reference to a new GVariant maybe instance
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_array ()
;;;
;;; GVariant * g_variant_new_array (const GVariantType *child_type,
;;;                                 GVariant * const *children,
;;;                                 gsize n_children);
;;;
;;; Creates a new GVariant array from children.
;;;
;;; child_type must be non-NULL if n_children is zero. Otherwise, the child type
;;; is determined by inspecting the first element of the children array. If
;;; child_type is non-NULL then it must be a definite type.
;;;
;;; The items of the array are taken from the children array. No entry in the
;;; children array may be NULL.
;;;
;;; All items in the array must have the same type, which must be the same as
;;; child_type, if given.
;;;
;;; If the children are floating references (see g_variant_ref_sink()), the new
;;; instance takes ownership of them as if via g_variant_ref_sink().
;;;
;;; child_type :
;;;     the element type of the new array
;;;
;;; children :
;;;     an array of GVariant pointers, the children
;;;
;;; n_children :
;;;     the length of children
;;;
;;; Returns :
;;;     a floating reference to a new GVariant array
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_tuple ()
;;;
;;; GVariant * g_variant_new_tuple (GVariant * const *children,
;;;                                 gsize n_children);
;;;
;;; Creates a new tuple GVariant out of the items in children. The type is
;;; determined from the types of children. No entry in the children array may be
;;; NULL.
;;;
;;; If n_children is 0 then the unit tuple is constructed.
;;;
;;; If the children are floating references (see g_variant_ref_sink()), the new
;;; instance takes ownership of them as if via g_variant_ref_sink().
;;;
;;; children :
;;;     the items to make the tuple out of
;;;
;;; n_children :
;;;     the length of children
;;;
;;; Returns :
;;;     a floating reference to a new GVariant tuple
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_dict_entry ()
;;;
;;; GVariant * g_variant_new_dict_entry (GVariant *key, GVariant *value);
;;;
;;; Creates a new dictionary entry GVariant. key and value must be non-NULL. key
;;; must be a value of a basic type (ie: not a container).
;;;
;;; If the key or value are floating references (see g_variant_ref_sink()), the
;;; new instance takes ownership of them as if via g_variant_ref_sink().
;;;
;;; key :
;;;     a basic GVariant, the key
;;;
;;; value :
;;;     a GVariant, the value
;;;
;;; Returns :
;;;     a floating reference to a new dictionary entry GVariant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_fixed_array ()
;;;
;;; GVariant * g_variant_new_fixed_array (const GVariantType *element_type,
;;;                                       gconstpointer elements,
;;;                                       gsize n_elements,
;;;                                       gsize element_size);
;;;
;;; Provides access to the serialised data for an array of fixed-sized items.
;;;
;;; value must be an array with fixed-sized elements. Numeric types are
;;; fixed-size as are tuples containing only other fixed-sized types.
;;;
;;; element_size must be the size of a single element in the array. For example,
;;; if calling this function for an array of 32 bit integers, you might say
;;; sizeof (gint32). This value isn't used except for the purpose of a
;;; double-check that the form of the serialised data matches the caller's
;;; expectation.
;;;
;;; n_elements, which must be non-NULL is set equal to the number of items in
;;; the array.
;;;
;;; element_type :
;;;     the GVariantType of each element
;;;
;;; elements :
;;;     a pointer to the fixed array of contiguous elements
;;;
;;; n_elements :
;;;     the number of elements
;;;
;;; element_size :
;;;     the size of each element
;;;
;;; Returns :
;;;     a floating reference to a new array GVariant instance
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_maybe ()
;;;
;;; GVariant * g_variant_get_maybe (GVariant *value);
;;;
;;; Given a maybe-typed GVariant instance, extract its value. If the value is
;;; Nothing, then this function returns NULL.
;;;
;;; value :
;;;     a maybe-typed value
;;;
;;; Returns :
;;;     the contents of value, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_n_children ()
;;;
;;; gsize g_variant_n_children (GVariant *value);
;;;
;;; Determines the number of children in a container GVariant instance. This
;;; includes variants, maybes, arrays, tuples and dictionary entries. It is an
;;; error to call this function on any other type of GVariant.
;;;
;;; For variants, the return value is always 1. For values with maybe types, it
;;; is always zero or one. For arrays, it is the length of the array. For tuples
;;; it is the number of tuple items (which depends only on the type). For
;;; dictionary entries, it is always 2
;;;
;;; This function is O(1).
;;;
;;; value :
;;;     a container GVariant
;;;
;;; Returns :
;;;     the number of children in the container
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_child_value ()
;;;
;;; GVariant * g_variant_get_child_value (GVariant *value, gsize index_);
;;;
;;; Reads a child item out of a container GVariant instance. This includes
;;; variants, maybes, arrays, tuples and dictionary entries. It is an error to
;;; call this function on any other type of GVariant.
;;;
;;; It is an error if index_ is greater than the number of child items in the
;;; container. See g_variant_n_children().
;;;
;;; The returned value is never floating. You should free it with
;;; g_variant_unref() when you're done with it.
;;;
;;; This function is O(1).
;;;
;;; value :
;;;     a container GVariant
;;;
;;; index_ :
;;;     the index of the child to fetch
;;;
;;; Returns :
;;;     the child at the specified index
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_child ()
;;;
;;; void g_variant_get_child (GVariant *value,
;;;                           gsize index_,
;;;                           const gchar *format_string,
;;;                           ...);
;;;
;;; Reads a child item out of a container GVariant instance and deconstructs it
;;; according to format_string. This call is essentially a combination of
;;; g_variant_get_child_value() and g_variant_get().
;;;
;;; value :
;;;     a container GVariant
;;;
;;; index_ :
;;;     the index of the child to deconstruct
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_lookup_value ()
;;;
;;; GVariant * g_variant_lookup_value (GVariant *dictionary,
;;;                                    const gchar *key,
;;;                                    const GVariantType *expected_type);
;;;
;;; Looks up a value in a dictionary GVariant.
;;;
;;; This function works with dictionaries of the type a{s*} (and equally well
;;; with type a{o*}, but we only further discuss the string case for sake of
;;; clarity).
;;;
;;; In the event that dictionary has the type a{sv}, the expected_type string
;;; specifies what type of value is expected to be inside of the variant. If the
;;; value inside the variant has a different type then NULL is returned. In the
;;; event that dictionary has a value type other than v then expected_type must
;;; directly match the key type and it is used to unpack the value directly or
;;; an error occurs.
;;;
;;; In either case, if key is not found in dictionary, NULL is returned.
;;;
;;; If the key is found and the value has the correct type, it is returned. If
;;; expected_type was specified then any non-NULL return value will have this
;;; type.
;;;
;;; dictionary :
;;;     a dictionary GVariant
;;;
;;; key :
;;;     the key to lookup in the dictionary
;;;
;;; expected_type :
;;;     a GVariantType, or NULL
;;;
;;; Returns :
;;;     the value of the dictionary key, or NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_lookup ()
;;;
;;; gboolean g_variant_lookup (GVariant *dictionary,
;;;                            const gchar *key,
;;;                            const gchar *format_string,
;;;                            ...);
;;;
;;; Looks up a value in a dictionary GVariant.
;;;
;;; This function is a wrapper around g_variant_lookup_value() and
;;; g_variant_get(). In the case that NULL would have been returned, this
;;; function returns FALSE. Otherwise, it unpacks the returned value and returns
;;; TRUE.
;;;
;;; See g_variant_get() for information about format_string.
;;;
;;; dictionary :
;;;     a dictionary GVariant
;;;
;;; key :
;;;     the key to lookup in the dictionary
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_fixed_array ()
;;;
;;; gconstpointer g_variant_get_fixed_array (GVariant *value,
;;;                                          gsize *n_elements,
;;;                                          gsize element_size);
;;;
;;; Provides access to the serialised data for an array of fixed-sized items.
;;;
;;; value must be an array with fixed-sized elements. Numeric types are
;;; fixed-size, as are tuples containing only other fixed-sized types.
;;;
;;; element_size must be the size of a single element in the array, as given by
;;; the section on Serialised Data Memory.
;;;
;;; In particular, arrays of these fixed-sized types can be interpreted as an
;;; array of the given C type, with element_size set to sizeof the appropriate
;;; type:
;;;
;;; element type                    C type
;;; G_VARIANT_TYPE_INT16 (etc.)     gint16 (etc.)
;;; G_VARIANT_TYPE_BOOLEAN          guchar (not gboolean!)
;;; G_VARIANT_TYPE_BYTE             guchar
;;; G_VARIANT_TYPE_HANDLE           guint32
;;; G_VARIANT_TYPE_DOUBLE           gdouble
;;;
;;; For example, if calling this function for an array of 32 bit integers, you
;;; might say sizeof (gint32). This value isn't used except for the purpose of
;;; a double-check that the form of the serialised data matches the caller's
;;; expectation.
;;;
;;; n_elements, which must be non-NULL is set equal to the number of items in
;;; the array.
;;;
;;; value :
;;;     a GVariant array with fixed-sized elements
;;;
;;; n_elements :
;;;     a pointer to the location to store the number of items
;;;
;;; element_size :
;;;     the size of each element
;;;
;;; Returns :
;;;     a pointer to the fixed array
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_size ()
;;;
;;; gsize g_variant_get_size (GVariant *value);
;;;
;;; Determines the number of bytes that would be required to store value with
;;; g_variant_store().
;;;
;;; If value has a fixed-sized type then this function always returned that
;;; fixed size.
;;;
;;; In the case that value is already in serialised form or the size has already
;;; been calculated (ie: this function has been called before) then this
;;; function is O(1). Otherwise, the size is calculated, an operation which is
;;; approximately O(n) in the number of values involved.
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     the serialised size of value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_data ()
;;;
;;; gconstpointer g_variant_get_data (GVariant *value);
;;;
;;; Returns a pointer to the serialised form of a GVariant instance. The
;;; returned data may not be in fully-normalised form if read from an untrusted
;;; source. The returned data must not be freed; it remains valid for as long
;;; as value exists.
;;;
;;; If value is a fixed-sized value that was deserialised from a corrupted
;;; serialised container then NULL may be returned. In this case, the proper
;;; thing to do is typically to use the appropriate number of nul bytes in place
;;; of value. If value is not fixed-sized then NULL is never returned.
;;;
;;; In the case that value is already in serialised form, this function is O(1).
;;; If the value is not already in serialised form, serialisation occurs
;;; implicitly and is approximately O(n) in the size of the result.
;;;
;;; To deserialise the data returned by this function, in addition to the
;;; serialised data, you must know the type of the GVariant, and (if the machine
;;; might be different) the endianness of the machine that stored it. As a
;;; result, file formats or network messages that incorporate serialised
;;; GVariants must include this information either implicitly (for instance "the
;;; file always contains a G_VARIANT_TYPE_VARIANT and it is always in
;;; little-endian order") or explicitly (by storing the type and/or endianness
;;; in addition to the serialised data).
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     the serialised form of value, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_data_as_bytes ()
;;;
;;; GBytes * g_variant_get_data_as_bytes (GVariant *value);
;;;
;;; Returns a pointer to the serialised form of a GVariant instance. The
;;; semantics of this function are exactly the same as g_variant_get_data(),
;;; except that the returned GBytes holds a reference to the variant data.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     A new GBytes representing the variant data. [transfer full]
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_store ()
;;;
;;; void g_variant_store (GVariant *value, gpointer data);
;;;
;;; Stores the serialised form of value at data. data should be large enough.
;;; See g_variant_get_size().
;;;
;;; The stored data is in machine native byte order but may not be in
;;; fully-normalised form if read from an untrusted source. See
;;; g_variant_get_normal_form() for a solution.
;;;
;;; As with g_variant_get_data(), to be able to deserialise the serialised
;;; variant successfully, its type and (if the destination machine might be
;;; different) its endianness must also be available.
;;;
;;; This function is approximately O(n) in the size of data.
;;;
;;; value :
;;;     the GVariant to store
;;;
;;; data :
;;;     the location to store the serialised data at
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_from_data ()
;;;
;;; GVariant * g_variant_new_from_data (const GVariantType *type,
;;;                                     gconstpointer data,
;;;                                     gsize size,
;;;                                     gboolean trusted,
;;;                                     GDestroyNotify notify,
;;;                                     gpointer user_data);
;;;
;;; Creates a new GVariant instance from serialised data.
;;;
;;; type is the type of GVariant instance that will be constructed. The
;;; interpretation of data depends on knowing the type.
;;;
;;; data is not modified by this function and must remain valid with an
;;; unchanging value until such a time as notify is called with user_data. If
;;; the contents of data change before that time then the result is undefined.
;;;
;;; If data is trusted to be serialised data in normal form then trusted should
;;; be TRUE. This applies to serialised data created within this process or read
;;; from a trusted location on the disk (such as a file installed in /usr/lib
;;; alongside your application). You should set trusted to FALSE if data is read
;;; from the network, a file in the user's home directory, etc.
;;;
;;; If data was not stored in this machine's native endianness, any multi-byte
;;; numeric values in the returned variant will also be in non-native
;;; endianness. g_variant_byteswap() can be used to recover the original values.
;;;
;;; notify will be called with user_data when data is no longer needed. The
;;; exact time of this call is unspecified and might even be before this
;;; function returns.
;;;
;;; Note: data must be backed by memory that is aligned appropriately for the
;;; type being loaded. Otherwise this function will internally create a copy of
;;; the memory (since GLib 2.60) or (in older versions) fail and exit the
;;; process.
;;;
;;; type :
;;;     a definite GVariantType
;;;
;;; data :
;;;     the serialised data
;;;
;;; size :
;;;     the size of data
;;;
;;; trusted :
;;;     TRUE if data is definitely in normal form
;;;
;;; notify :
;;;     function to call when data is no longer needed
;;;
;;; user_data :
;;;     data for notify
;;;
;;; Returns :
;;;     a new floating GVariant of type type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_from_bytes ()
;;;
;;; GVariant * g_variant_new_from_bytes (const GVariantType *type,
;;;                                      GBytes *bytes,
;;;                                      gboolean trusted);
;;;
;;; Constructs a new serialised-mode GVariant instance. This is the inner
;;; interface for creation of new serialised values that gets called from
;;; various functions in gvariant.c.
;;;
;;; A reference is taken on bytes.
;;;
;;; The data in bytes must be aligned appropriately for the type being loaded.
;;; Otherwise this function will internally create a copy of the memory (since
;;; GLib 2.60) or (in older versions) fail and exit the process.
;;;
;;; type :
;;;     a GVariantType
;;;
;;; bytes :
;;;     a GBytes
;;;
;;; trusted :
;;;     if the contents of bytes are trusted
;;;
;;; Returns :
;;;     a new GVariant with a floating reference
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_byteswap ()
;;;
;;; GVariant * g_variant_byteswap (GVariant *value);
;;;
;;; Performs a byteswapping operation on the contents of value. The result is
;;; that all multi-byte numeric data contained in value is byteswapped. That
;;; includes 16, 32, and 64bit signed and unsigned integers as well as file
;;; handles and double precision floating point values.
;;;
;;; This function is an identity mapping on any value that does not contain
;;; multi-byte numeric data. That include strings, booleans, bytes and
;;; containers containing only these things (recursively).
;;;
;;; The returned value is always in normal form and is marked as trusted.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     the byteswapped form of value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_get_normal_form ()
;;;
;;; GVariant * g_variant_get_normal_form (GVariant *value);
;;;
;;; Gets a GVariant instance that has the same value as value and is trusted to
;;; be in normal form.
;;;
;;; If value is already trusted to be in normal form then a new reference to
;;; value is returned.
;;;
;;; If value is not already trusted, then it is scanned to check if it is in
;;; normal form. If it is found to be in normal form then it is marked as
;;; trusted and a new reference to it is returned.
;;;
;;; If value is found not to be in normal form then a new trusted GVariant is
;;; created with the same value as value.
;;;
;;; It makes sense to call this function if you've received GVariant data from
;;; untrusted sources and you want to ensure your serialised output is
;;; definitely in normal form.
;;;
;;; value :
;;;     a GVariant
;;;
;;; Returns :
;;;     a trusted GVariant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_is_normal_form ()
;;;
;;; gboolean g_variant_is_normal_form (GVariant *value);
;;;
;;; Checks if value is in normal form.
;;;
;;; The main reason to do this is to detect if a given chunk of serialised data
;;; is in normal form: load the data into a GVariant using
;;; g_variant_new_from_data() and then use this function to check.
;;;
;;; If value is found to be in normal form then it will be marked as being
;;; trusted. If the value was already marked as being trusted then this function
;;; will immediately return TRUE.
;;;
;;; value :
;;;     a GVariant instance
;;;
;;; Returns :
;;;     TRUE if value is in normal form
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_hash ()
;;;
;;; guint g_variant_hash (gconstpointer value);
;;;
;;; Generates a hash value for a GVariant instance.
;;;
;;; The output of this function is guaranteed to be the same for a given value
;;; only per-process. It may change between different processor architectures or
;;; even different versions of GLib. Do not use this function as a basis for
;;; building protocols or file formats.
;;;
;;; The type of value is gconstpointer only to allow use of this function with
;;; GHashTable. value must be a GVariant.
;;;
;;; value :
;;;     a basic GVariant value as a gconstpointer
;;;
;;; Returns :
;;;     a hash value corresponding to value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_equal" g-variant-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-12-1}
  @argument[value-1]{a @type{g-variant} instance}
  @argument[value-2]{a @type{g-variant} instance}
  @return{@em{True} if @arg{value-1} and @arg{value-2} are equal.}
  @begin{short}
    Checks if @arg{value-1} and @arg{value-2} have the same type and value.
  @end{short}
  @see-type{g-variant}"
  (value-1 (:pointer (:struct g-variant)))
  (value-2 (:pointer (:struct g-variant))))

(export 'g-variant-equal)

;;; ----------------------------------------------------------------------------
;;; g_variant_print ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_print" %g-variant-print) :string
  (value (:pointer (:struct g-variant)))
  (annotate :boolean))

(defun g-variant-print (value &optional (annotate nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-15}
  @argument[value]{a @type{g-variant} instance}
  @argument[annotate]{@em{true} if type information should be included in the
    output}
  @return{A string holding the result.}
  @begin{short}
    Pretty-prints @arg{value} in the format understood by the function
    @fun{g-variant-parse}.
  @end{short}
  If @arg{annotate} is @em{true}, then type information is included in the
  output.
  @see-type{g-variant}
  @see-function{g-variant-parse}"
  (%g-variant-print value annotate))

(export 'g-variant-print)

;;; ----------------------------------------------------------------------------
;;; g_variant_print_string ()
;;;
;;; GString * g_variant_print_string (GVariant *value,
;;;                                   GString *string,
;;;                                   gboolean type_annotate);
;;;
;;; Behaves as g_variant_print(), but operates on a GString.
;;;
;;; If string is non-NULL then it is appended to and returned. Else, a new empty
;;; GString is allocated and it is returned.
;;;
;;; value :
;;;     a GVariant
;;;
;;; string :
;;;     a GString, or NULL
;;;
;;; type_annotate :
;;;     TRUE if type information should be included in the output
;;;
;;; Returns :
;;;     a GString containing the string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_copy ()
;;;
;;; GVariantIter * g_variant_iter_copy (GVariantIter *iter);
;;;
;;; Creates a new heap-allocated GVariantIter to iterate over the container that
;;; was being iterated over by iter. Iteration begins on the new iterator from
;;; the current position of the old iterator but the two copies are independent
;;; past that point.
;;;
;;; Use g_variant_iter_free() to free the return value when you no longer need
;;; it.
;;;
;;; A reference is taken to the container that iter is iterating over and will
;;; be releated only when g_variant_iter_free() is called.
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; Returns :
;;;     a new heap-allocated GVariantIter
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_free ()
;;;
;;; void g_variant_iter_free (GVariantIter *iter);
;;;
;;; Frees a heap-allocated GVariantIter. Only call this function on iterators
;;; that were returned by g_variant_iter_new() or g_variant_iter_copy().
;;;
;;; iter :
;;;     a heap-allocated GVariantIter
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_init ()
;;;
;;; gsize g_variant_iter_init (GVariantIter *iter, GVariant *value);
;;;
;;; Initialises (without allocating) a GVariantIter. iter may be completely
;;; uninitialised prior to this call; its old value is ignored.
;;;
;;; The iterator remains valid for as long as value exists, and need not be
;;; freed in any way.
;;;
;;; iter :
;;;     a pointer to a GVariantIter
;;;
;;; value :
;;;     a container GVariant
;;;
;;; Returns :
;;;     the number of items in value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_n_children ()
;;;
;;; gsize g_variant_iter_n_children (GVariantIter *iter);
;;;
;;; Queries the number of child items in the container that we are iterating
;;; over. This is the total number of items -- not the number of items
;;; remaining.
;;;
;;; This function might be useful for preallocation of arrays.
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; Returns :
;;;     the number of children in the container
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_new ()
;;;
;;; GVariantIter * g_variant_iter_new (GVariant *value);
;;;
;;; Creates a heap-allocated GVariantIter for iterating over the items in value.
;;;
;;; Use g_variant_iter_free() to free the return value when you no longer need
;;; it.
;;;
;;; A reference is taken to value and will be released only when
;;; g_variant_iter_free() is called.
;;;
;;; value :
;;;     a container GVariant
;;;
;;; Returns :
;;;     a new heap-allocated GVariantIter
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_next_value ()
;;;
;;; GVariant * g_variant_iter_next_value (GVariantIter *iter);
;;;
;;; Gets the next item in the container. If no more items remain then NULL is
;;; returned.
;;;
;;; Use g_variant_unref() to drop your reference on the return value when you
;;; no longer need it.
;;;
;;; Example 18. Iterating with g_variant_iter_next_value()
;;;
;;; /* recursively iterate a container */
;;; void
;;; iterate_container_recursive (GVariant *container)
;;; {
;;;   GVariantIter iter;
;;;   GVariant *child;
;;;
;;;   g_variant_iter_init (&iter, container);
;;;   while ((child = g_variant_iter_next_value (&iter)))
;;;     {
;;;       g_print ("type '%s'\n", g_variant_get_type_string (child));
;;;
;;;       if (g_variant_is_container (child))
;;;         iterate_container_recursive (child);
;;;
;;;       g_variant_unref (child);
;;;     }
;;; }
;;;
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; Returns :
;;;     a GVariant, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_next ()
;;;
;;; gboolean g_variant_iter_next (GVariantIter *iter,
;;;                               const gchar *format_string,
;;;                               ...);
;;;
;;; Gets the next item in the container and unpacks it into the variable
;;; argument list according to format_string, returning TRUE.
;;;
;;; If no more items remain then FALSE is returned.
;;;
;;; All of the pointers given on the variable arguments list of this function
;;; are assumed to point at uninitialised memory. It is the responsibility of
;;; the caller to free all of the values returned by the unpacking process.
;;;
;;; See the section on GVariant Format Strings.
;;;
;;; Example 19. Memory management with g_variant_iter_next()
;;;
;;; /* Iterates a dictionary of type 'a{sv}' */
;;; void
;;; iterate_dictionary (GVariant *dictionary)
;;; {
;;;   GVariantIter iter;
;;;   GVariant *value;
;;;   gchar *key;
;;;
;;;   g_variant_iter_init (&iter, dictionary);
;;;   while (g_variant_iter_next (&iter, "{sv}", &key, &value))
;;;     {
;;;       g_print ("Item '%s' has type '%s'\n", key,
;;;                g_variant_get_type_string (value));
;;;
;;;       /* must free data for ourselves */
;;;       g_variant_unref (value);
;;;       g_free (key);
;;;     }
;;; }
;;;
;;;
;;; For a solution that is likely to be more convenient to C programmers when
;;; dealing with loops, see g_variant_iter_loop().
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked, or FALSE if there as no value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_iter_loop ()
;;;
;;; gboolean g_variant_iter_loop (GVariantIter *iter,
;;;                               const gchar *format_string,
;;;                               ...);
;;;
;;; Gets the next item in the container and unpacks it into the variable
;;; argument list according to format_string, returning TRUE.
;;;
;;; If no more items remain then FALSE is returned.
;;;
;;; On the first call to this function, the pointers appearing on the variable
;;; argument list are assumed to point at uninitialised memory. On the second
;;; and later calls, it is assumed that the same pointers will be given and that
;;; they will point to the memory as set by the previous call to this function.
;;; This allows the previous values to be freed, as appropriate.
;;;
;;; This function is intended to be used with a while loop as demonstrated in
;;; the following example. This function can only be used when iterating over
;;; an array. It is only valid to call this function with a string constant for
;;; the format string and the same string constant must be used each time.
;;; Mixing calls to this function and g_variant_iter_next() or
;;; g_variant_iter_next_value() on the same iterator causes undefined behavior.
;;;
;;; If you break out of a such a while loop using g_variant_iter_loop() then you
;;; must free or unreference all the unpacked values as you would with
;;; g_variant_get(). Failure to do so will cause a memory leak.
;;;
;;; See the section on GVariant Format Strings.
;;;
;;; Example 20. Memory management with g_variant_iter_loop()
;;;
;;; /* Iterates a dictionary of type 'a{sv}' */
;;; void
;;; iterate_dictionary (GVariant *dictionary)
;;; {
;;;   GVariantIter iter;
;;;   GVariant *value;
;;;   gchar *key;
;;;
;;;   g_variant_iter_init (&iter, dictionary);
;;;   while (g_variant_iter_loop (&iter, "{sv}", &key, &value))
;;;     {
;;;       g_print ("Item '%s' has type '%s'\n", key,
;;;                g_variant_get_type_string (value));
;;;
;;;       /* no need to free 'key' and 'value' here */
;;;       /* unless breaking out of this loop */
;;;     }
;;; }
;;;
;;;
;;; For most cases you should use g_variant_iter_next().
;;;
;;; This function is really only useful when unpacking into GVariant or
;;; GVariantIter in order to allow you to skip the call to g_variant_unref() or
;;; g_variant_iter_free().
;;;
;;; For example, if you are only looping over simple integer and string types,
;;; g_variant_iter_next() is definitely preferred. For string types, use the '&'
;;; prefix to avoid allocating any memory at all (and thereby avoiding the need
;;; to free anything as well).
;;;
;;; iter :
;;;     a GVariantIter
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked, or FALSE if there was no value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_BUILDER_INIT()
;;;
;;; #define G_VARIANT_BUILDER_INIT(variant_type)
;;;         { { { 2942751021u, variant_type, { 0, } } } }
;;;
;;; A stack-allocated GVariantBuilder must be initialized if it is used together
;;; with g_auto() to avoid warnings or crashes if function returns before
;;; g_variant_builder_init() is called on the builder. This macro can be used
;;; as initializer instead of an explicit zeroing a variable when declaring it
;;; and a following g_variant_builder_init(), but it cannot be assigned to a
;;; variable.
;;;
;;; The passed variant_type should be a static GVariantType to avoid lifetime
;;; issues, as copying the variant_type does not happen in the
;;; G_VARIANT_BUILDER_INIT() call, but rather in functions that make sure that
;;; GVariantBuilder is valid.
;;;
;;; g_auto(GVariantBuilder) builder
;;;     = G_VARIANT_BUILDER_INIT (G_VARIANT_TYPE_BYTESTRING);
;;;
;;; variant_type :
;;;     a const GVariantType*
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_unref ()
;;;
;;; void g_variant_builder_unref (GVariantBuilder *builder);
;;;
;;; Decreases the reference count on builder.
;;;
;;; In the event that there are no more references, releases all memory
;;; associated with the GVariantBuilder.
;;;
;;; Don't call this on stack-allocated GVariantBuilder instances or bad things
;;; will happen.
;;;
;;; builder :
;;;     a GVariantBuilder allocated by g_variant_builder_new()
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_ref ()
;;;
;;; GVariantBuilder * g_variant_builder_ref (GVariantBuilder *builder);
;;;
;;; Increases the reference count on builder.
;;;
;;; Don't call this on stack-allocated GVariantBuilder instances or bad things
;;; will happen.
;;;
;;; builder :
;;;     a GVariantBuilder allocated by g_variant_builder_new()
;;;
;;; Returns :
;;;     a new reference to builder
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_new ()
;;;
;;; GVariantBuilder * g_variant_builder_new (const GVariantType *type);
;;;
;;; Allocates and initialises a new GVariantBuilder.
;;;
;;; You should call g_variant_builder_unref() on the return value when it is no
;;; longer needed. The memory will not be automatically freed by any other call.
;;;
;;; In most cases it is easier to place a GVariantBuilder directly on the stack
;;; of the calling function and initialise it with g_variant_builder_init().
;;;
;;; type :
;;;     a container type
;;;
;;; Returns :
;;;     a GVariantBuilder
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_init ()
;;;
;;; void g_variant_builder_init (GVariantBuilder *builder,
;;;                              const GVariantType *type);
;;;
;;; Initialises a GVariantBuilder structure.
;;;
;;; type must be non-NULL. It specifies the type of container to construct. It
;;; can be an indefinite type such as G_VARIANT_TYPE_ARRAY or a definite type
;;; such as "as" or "(ii)". Maybe, array, tuple, dictionary entry and
;;; variant-typed values may be constructed.
;;;
;;; After the builder is initialised, values are added using
;;; g_variant_builder_add_value() or g_variant_builder_add().
;;;
;;; After all the child values are added, g_variant_builder_end() frees the
;;; memory associated with the builder and returns the GVariant that was
;;; created.
;;;
;;; This function completely ignores the previous contents of builder. On one
;;; hand this means that it is valid to pass in completely uninitialised memory.
;;; On the other hand, this means that if you are initialising over top of an
;;; existing GVariantBuilder you need to first call g_variant_builder_clear()
;;; in order to avoid leaking memory.
;;;
;;; You must not call g_variant_builder_ref() or g_variant_builder_unref() on a
;;; GVariantBuilder that was initialised with this function. If you ever pass a
;;; reference to a GVariantBuilder outside of the control of your own code then
;;; you should assume that the person receiving that reference may try to use
;;; reference counting; you should use g_variant_builder_new() instead of this
;;; function.
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; type :
;;;     a container type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_clear ()
;;;
;;; void g_variant_builder_clear (GVariantBuilder *builder);
;;;
;;; Releases all memory associated with a GVariantBuilder without freeing the
;;; GVariantBuilder structure itself.
;;;
;;; It typically only makes sense to do this on a stack-allocated
;;; GVariantBuilder if you want to abort building the value part-way through.
;;; This function need not be called if you call g_variant_builder_end() and it
;;; also does not need to be called on builders allocated with
;;; g_variant_builder_new (see g_variant_builder_unref() for that).
;;;
;;; This function leaves the GVariantBuilder structure set to all-zeros. It is
;;; valid to call this function on either an initialised GVariantBuilder or one
;;; that is set to all-zeros but it is not valid to call this function on
;;; uninitialised memory.
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_add_value ()
;;;
;;; void g_variant_builder_add_value (GVariantBuilder *builder, GVariant *value)
;;;
;;; Adds value to builder.
;;;
;;; It is an error to call this function in any way that would create an
;;; inconsistent value to be constructed. Some examples of this are putting
;;; different types of items into an array, putting the wrong types or number
;;; of items in a tuple, putting more than one value into a variant, etc.
;;;
;;; If value is a floating reference (see g_variant_ref_sink()), the builder
;;; instance takes ownership of value.
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; value :
;;;     a GVariant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_add ()
;;;
;;; void g_variant_builder_add (GVariantBuilder *builder,
;;;                             const gchar *format_string,
;;;                             ...);
;;;
;;; Adds to a GVariantBuilder.
;;;
;;; This call is a convenience wrapper that is exactly equivalent to calling
;;; g_variant_new() followed by g_variant_builder_add_value().
;;;
;;; This function might be used as follows:
;;;
;;; GVariant *
;;; make_pointless_dictionary (void)
;;; {
;;;   GVariantBuilder *builder;
;;;   int i;
;;;
;;;   builder = g_variant_builder_new (G_VARIANT_TYPE_ARRAY);
;;;   for (i = 0; i < 16; i++)
;;;     {
;;;       gchar buf[3];
;;;
;;;       sprintf (buf, "%d", i);
;;;       g_variant_builder_add (builder, "{is}", i, buf);
;;;     }
;;;
;;;   return g_variant_builder_end (builder);
;;; }
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; format_string :
;;;     a GVariant varargs format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_add_parsed ()
;;;
;;; void g_variant_builder_add_parsed (GVariantBuilder *builder,
;;;                                    const gchar *format,
;;;                                    ...);
;;;
;;; Adds to a GVariantBuilder.
;;;
;;; This call is a convenience wrapper that is exactly equivalent to calling
;;; g_variant_new_parsed() followed by g_variant_builder_add_value().
;;;
;;; This function might be used as follows:
;;;
;;; GVariant *
;;; make_pointless_dictionary (void)
;;; {
;;;   GVariantBuilder *builder;
;;;   int i;
;;;
;;;   builder = g_variant_builder_new (G_VARIANT_TYPE_ARRAY);
;;;   g_variant_builder_add_parsed (builder, "{'width', <%i>}", 600);
;;;   g_variant_builder_add_parsed (builder, "{'title', <%s>}", "foo");
;;;   g_variant_builder_add_parsed (builder, "{'transparency', <0.5>}");
;;;   return g_variant_builder_end (builder);
;;; }
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; format :
;;;     a text format GVariant
;;;
;;; ... :
;;;     arguments as per format
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_end ()
;;;
;;; GVariant * g_variant_builder_end (GVariantBuilder *builder);
;;;
;;; Ends the builder process and returns the constructed value.
;;;
;;; It is not permissible to use builder in any way after this call except for
;;; reference counting operations (in the case of a heap-allocated
;;; GVariantBuilder) or by reinitialising it with g_variant_builder_init() (in
;;; the case of stack-allocated).
;;;
;;; It is an error to call this function in any way that would create an
;;; inconsistent value to be constructed (ie: insufficient number of items
;;; added to a container with a specific number of children required). It is
;;; also an error to call this function if the builder was created with an
;;; indefinite array or maybe type and no children have been added; in this
;;; case it is impossible to infer the type of the empty array.
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; Returns :
;;;     a new, floating, GVariant
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_open ()
;;;
;;; void g_variant_builder_open (GVariantBuilder *builder,
;;;                              const GVariantType *type);
;;;
;;; Opens a subcontainer inside the given builder. When done adding items to
;;; the subcontainer, g_variant_builder_close() must be called.
;;;
;;; It is an error to call this function in any way that would cause an
;;; inconsistent value to be constructed (ie: adding too many values or a value
;;; of an incorrect type).
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; type :
;;;     a GVariantType
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_builder_close ()
;;;
;;; void g_variant_builder_close (GVariantBuilder *builder);
;;;
;;; Closes the subcontainer inside the given builder that was opened by the
;;; most recent call to g_variant_builder_open().
;;;
;;; It is an error to call this function in any way that would create an
;;; inconsistent value to be constructed (ie: too few values added to the
;;; subcontainer).
;;;
;;; builder :
;;;     a GVariantBuilder
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_DICT_INIT()
;;;
;;; #define G_VARIANT_DICT_INIT(asv) { { { asv, 3488698669u, { 0, } } } }
;;;
;;; A stack-allocated GVariantDict must be initialized if it is used together
;;; with g_auto() to avoid warnings or crashes if function returns before
;;; g_variant_dict_init() is called on the builder. This macro can be used as
;;; initializer instead of an explicit zeroing a variable when declaring it and
;;; a following g_variant_dict_init(), but it cannot be assigned to a variable.
;;;
;;; The passed asv has to live long enough for GVariantDict to gather the
;;; entries from, as the gathering does not happen in the G_VARIANT_DICT_INIT()
;;; call, but rather in functions that make sure that GVariantDict is valid. In
;;; context where the initialization value has to be a constant expression, the
;;; only possible value of asv is NULL. It is still possible to call
;;; g_variant_dict_init() safely with a different asv right after the variable
;;; was initialized with G_VARIANT_DICT_INIT().
;;;
;;; g_autoptr(GVariant) variant = get_asv_variant ();
;;; g_auto(GVariantDict) dict = G_VARIANT_DICT_INIT (variant);
;;;
;;; asv :
;;;     a GVariant*.
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_unref ()
;;;
;;; void
;;; g_variant_dict_unref (GVariantDict *dict);
;;;
;;; Decreases the reference count on dict .
;;;
;;; In the event that there are no more references, releases all memory
;;; associated with the GVariantDict.
;;;
;;; Don't call this on stack-allocated GVariantDict instances or bad things
;;; will happen.
;;;
;;; dict :
;;;     a heap-allocated GVariantDict.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_ref ()
;;;
;;; GVariantDict *
;;; g_variant_dict_ref (GVariantDict *dict);
;;;
;;; Increases the reference count on dict .
;;;
;;; Don't call this on stack-allocated GVariantDict instances or bad things
;;; will happen.
;;;
;;; dict :
;;;     a heap-allocated GVariantDict
;;;
;;; Returns :
;;;     a new reference to dict .
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_new ()
;;;
;;; GVariantDict *
;;; g_variant_dict_new (GVariant *from_asv);
;;;
;;; Allocates and initialises a new GVariantDict.
;;;
;;; You should call g_variant_dict_unref() on the return value when it is no
;;; longer needed. The memory will not be automatically freed by any other call.
;;;
;;; In some cases it may be easier to place a GVariantDict directly on the stack
;;; of the calling function and initialise it with g_variant_dict_init(). This
;;; is particularly useful when you are using GVariantDict to construct a
;;; GVariant.
;;;
;;; from_asv :
;;;     the GVariant with which to initialise the dictionary.
;;;
;;; Returns :
;;;     a GVariantDict.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_init ()
;;;
;;; void
;;; g_variant_dict_init (GVariantDict *dict,
;;;                      GVariant *from_asv);
;;;
;;; Initialises a GVariantDict structure.
;;;
;;; If from_asv is given, it is used to initialise the dictionary.
;;;
;;; This function completely ignores the previous contents of dict . On one
;;; hand this means that it is valid to pass in completely uninitialised memory.
;;; On the other hand, this means that if you are initialising over top of an
;;; existing GVariantDict you need to first call g_variant_dict_clear() in
;;; order to avoid leaking memory.
;;;
;;; You must not call g_variant_dict_ref() or g_variant_dict_unref() on a
;;; GVariantDict that was initialised with this function. If you ever pass a
;;; reference to a GVariantDict outside of the control of your own code then
;;; you should assume that the person receiving that reference may try to use
;;; reference counting; you should use g_variant_dict_new() instead of this
;;; function.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; from_asv :
;;;     the initial value for dict .
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_clear ()
;;;
;;; void
;;; g_variant_dict_clear (GVariantDict *dict);
;;;
;;; Releases all memory associated with a GVariantDict without freeing the
;;; GVariantDict structure itself.
;;;
;;; It typically only makes sense to do this on a stack-allocated GVariantDict
;;; if you want to abort building the value part-way through. This function
;;; need not be called if you call g_variant_dict_end() and it also does not
;;; need to be called on dicts allocated with g_variant_dict_new
;;; (see g_variant_dict_unref() for that).
;;;
;;; It is valid to call this function on either an initialised GVariantDict or
;;; one that was previously cleared by an earlier call to g_variant_dict_clear()
;;; but it is not valid to call this function on uninitialised memory.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_contains ()
;;;
;;; gboolean
;;; g_variant_dict_contains (GVariantDict *dict,
;;;                          const gchar *key);
;;;
;;; Checks if key exists in dict .
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to look up in the dictionary
;;;
;;; Returns :
;;;     TRUE if key is in dict
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_dict_contains" g-variant-dict-contains) :boolean
  (dict (gobject::g-boxed-foreign g-variant-dict))
  (key :string))

(export 'g-variant-dict-contains)

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_lookup ()
;;;
;;; gboolean
;;; g_variant_dict_lookup (GVariantDict *dict,
;;;                        const gchar *key,
;;;                        const gchar *format_string,
;;;                        ...);
;;;
;;; Looks up a value in a GVariantDict.
;;;
;;; This function is a wrapper around g_variant_dict_lookup_value() and
;;; g_variant_get(). In the case that NULL would have been returned, this
;;; function returns FALSE. Otherwise, it unpacks the returned value and
;;; returns TRUE.
;;;
;;; format_string determines the C types that are used for unpacking the values
;;; and also determines if the values are copied or borrowed, see the section
;;; on GVariant format strings.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to look up in the dictionary
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     the arguments to unpack the value into
;;;
;;; Returns :
;;;     TRUE if a value was unpacked
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_lookup_value ()
;;;
;;; GVariant *
;;; g_variant_dict_lookup_value (GVariantDict *dict,
;;;                              const gchar *key,
;;;                              const GVariantType *expected_type);
;;;
;;; Looks up a value in a GVariantDict.
;;;
;;; If key is not found in dictionary , NULL is returned.
;;;
;;; The expected_type string specifies what type of value is expected. If the
;;; value associated with key has a different type then NULL is returned.
;;;
;;; If the key is found and the value has the correct type, it is returned. If
;;; expected_type was specified then any non-NULL return value will have this
;;; type.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to look up in the dictionary
;;;
;;; expected_type :
;;;     a GVariantType, or NULL.
;;;
;;; Returns :
;;;     the value of the dictionary key, or NULL.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_insert ()
;;;
;;; void
;;; g_variant_dict_insert (GVariantDict *dict,
;;;                        const gchar *key,
;;;                        const gchar *format_string,
;;;                        ...);
;;;
;;; Inserts a value into a GVariantDict.
;;;
;;; This call is a convenience wrapper that is exactly equivalent to calling
;;; g_variant_new() followed by g_variant_dict_insert_value().
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to insert a value for
;;;
;;; format_string :
;;;     a GVariant varargs format string
;;;
;;; ... :
;;;     arguments, as per format_string
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_insert_value ()
;;;
;;; void
;;; g_variant_dict_insert_value (GVariantDict *dict,
;;;                              const gchar *key,
;;;                              GVariant *value);
;;;
;;; Inserts (or replaces) a key in a GVariantDict.
;;;
;;;  value is consumed if it is floating.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to insert a value for
;;;
;;; value :
;;;     the value to insert
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_remove ()
;;;
;;; gboolean
;;; g_variant_dict_remove (GVariantDict *dict,
;;;                        const gchar *key);
;;;
;;; Removes a key and its associated value from a GVariantDict.
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; key :
;;;     the key to remove
;;;
;;; Returns :
;;;     TRUE if the key was found and removed
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_dict_end ()
;;;
;;; GVariant *
;;; g_variant_dict_end (GVariantDict *dict);
;;;
;;; Returns the current value of dict as a GVariant of type
;;; G_VARIANT_TYPE_VARDICT, clearing it in the process.
;;;
;;; It is not permissible to use dict in any way after this call except for
;;; reference counting operations (in the case of a heap-allocated GVariantDict)
;;; or by reinitialising it with g_variant_dict_init() (in the case of
;;; stack-allocated).
;;;
;;; dict :
;;;     a GVariantDict
;;;
;;; Returns :
;;;     a new, floating, GVariant.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_parse ()
;;; ----------------------------------------------------------------------------

;; FIXME: Does not work for an argument non-nil for VTYPE

(defcfun ("g_variant_parse" %g-variant-parse-1)
    (:pointer (:struct g-variant))
  (vtype :pointer) ; must be the type :pointer
  (text :string)
  (limit :pointer)
  (endptr :pointer)
  (err :pointer))

(defcfun ("g_variant_parse" %g-variant-parse-2)
    (:pointer (:struct g-variant))
  (vtype (gobject:g-boxed-foreign g-variant-type))
  (text :string)
  (limit :pointer)
  (endptr :pointer)
  (err :pointer))

(defun g-variant-parse (vtype text)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-15}
  @argument[vtype]{a @class{g-variant-type} instance, or a valid type string}
  @argument[text]{a string containing a @type{g-variant} in text form}
  @return{A @type{g-variant} instance.}
  @begin{short}
    Parses a @type{g-variant} instance from a text representation.
  @end{short}

  If @arg{vtype} is non-@code{nil} then the value will be parsed to have that
  type. This may result in additional parse errors, in the case that the parsed
  value does not fit the type, but may also result in fewer errors, in the case
  that the type would have been ambiguous, such as with empty arrays.

  In the event that the parsing is successful, the resulting @type{g-variant}
  instance is returned. In case of any error, @code{nil} will be returned.

  Officially, the language understood by the parser is any string produced by
  the function @fun{g-variant-print}.
  @begin[Example]{dictionary}
    @begin{pre}
(g-variant-parse (g-variant-type-new \"b\") \"true\")
=> #.(SB-SYS:INT-SAP #X7F99C4012440)
(g-variant-print *) => \"true\"
(g-variant-parse \"b\" \"false\")
=> #.(SB-SYS:INT-SAP #X564C855E8690)
(g-variant-print *) => \"false\"
(g-variant-parse (g-variant-type-new \"i\") \"100\")
=> #.(SB-SYS:INT-SAP #X7F99C4012CF0)
(g-variant-print * nil) => \"100\"
(g-variant-parse \"d\" \"100\")
=> #.(SB-SYS:INT-SAP #X564C855F9900)
(g-variant-print *) => \"100.0\"
    @end{pre}
  @end{dictionary}
  @see-type{g-variant}
  @see-function{g-variant-print}"
  (with-g-error (err)
    (cond ((stringp vtype)
           (let ((vtype1 (g-variant-type-new vtype)))
             (unwind-protect
               (%g-variant-parse-2 vtype1
                                   text (null-pointer) (null-pointer) err)
               (g-variant-type-free vtype1))))
          ((typep vtype 'g-variant-type)
           (%g-variant-parse-2 vtype text (null-pointer) (null-pointer) err))
          (t
           (%g-variant-parse-1 (null-pointer)
                               text (null-pointer) (null-pointer) err)))))

(export 'g-variant-parse)

;;; ----------------------------------------------------------------------------
;;; g_variant_new_parsed_va ()
;;;
;;; GVariant * g_variant_new_parsed_va (const gchar *format, va_list *app);
;;;
;;; Parses format and returns the result.
;;;
;;; This is the version of g_variant_new_parsed() intended to be used from
;;; libraries.
;;;
;;; The return value will be floating if it was a newly created GVariant
;;; instance. In the case that format simply specified the collection of a
;;; GVariant pointer (eg: format was "%*") then the collected GVariant pointer
;;; will be returned unmodified, without adding any additional references.
;;;
;;; In order to behave correctly in all cases it is necessary for the calling
;;; function to g_variant_ref_sink() the return result before returning control
;;; to the user that originally provided the pointer. At this point, the caller
;;; will have their own full reference to the result. This can also be done by
;;; adding the result to a container, or by passing it to another
;;; g_variant_new() call.
;;;
;;; format :
;;;     a text format GVariant
;;;
;;; app :
;;;     a pointer to a va_list
;;;
;;; Returns :
;;;     a new, usually floating, GVariant
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_new_parsed ()
;;;
;;; GVariant * g_variant_new_parsed (const gchar *format, ...);
;;;
;;; Parses format and returns the result.
;;;
;;; format must be a text format GVariant with one extension: at any point that
;;; a value may appear in the text, a '%' character followed by a GVariant
;;; format string (as per g_variant_new()) may appear. In that case, the same
;;; arguments are collected from the argument list as g_variant_new() would
;;; have collected.
;;;
;;; Consider this simple example:
;;;
;;; g_variant_new_parsed ("[('one', 1), ('two', %i), (%s, 3)]", 2, "three");
;;;
;;; In the example, the variable argument parameters are collected and filled
;;; in as if they were part of the original string to produce the result of
;;; [('one', 1), ('two', 2), ('three', 3)].
;;;
;;; This function is intended only to be used with format as a string literal.
;;; Any parse error is fatal to the calling process. If you want to parse data
;;; from untrusted sources, use g_variant_parse().
;;;
;;; You may not use this function to return, unmodified, a single GVariant
;;; pointer from the argument list. ie: format may not solely be anything along
;;; the lines of "%*", "%?", "%r", or anything starting with "%@".
;;;
;;; format :
;;;     a text format GVariant
;;;
;;; ... :
;;;     arguments as per format
;;;
;;; Returns :
;;;     a new floating GVariant instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_parse_error_print_context ()
;;;
;;; gchar *
;;; g_variant_parse_error_print_context (GError *error,
;;;                                      const gchar *source_str);
;;;
;;; Pretty-prints a message showing the context of a GVariant parse error
;;; within the string for which parsing was attempted.
;;;
;;; The resulting string is suitable for output to the console or other
;;; monospace media where newlines are treated in the usual way.
;;;
;;; The message will typically look something like one of the following:
;;;
;;; unterminated string constant:
;;;   (1, 2, 3, 'abc
;;;             ^^^^
;;; or
;;;
;;; unable to find a common type:
;;;   [1, 2, 3, 'str']
;;;    ^        ^^^^^
;;;
;;; The format of the message may change in a future version.
;;;
;;; error must have come from a failed attempt to g_variant_parse() and
;;; source_str must be exactly the same string that caused the error. If
;;; source_str was not nul-terminated when you passed it to g_variant_parse()
;;; then you must add nul termination before using this function.
;;;
;;; error :
;;;     a GError from the GVariantParseError domain
;;;
;;; source_str :
;;;     the string that was given to the parser
;;;
;;; Returns :
;;;     the printed message.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.variant.lisp ------------------------------------------

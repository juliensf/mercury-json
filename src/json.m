%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2015, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury library for reading and writing JSON.
%
%-----------------------------------------------------------------------------%

:- module json.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module bimap.
:- import_module bitmap.
:- import_module calendar.
:- import_module char.
:- import_module cord.
:- import_module integer.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set_bbbtree.
:- import_module set_ctree234.
:- import_module set_ordlist.
:- import_module set_tree234.
:- import_module set_unordlist.
:- import_module stream.
:- import_module version_array.

%-----------------------------------------------------------------------------%
%
% JSON errors.
%

:- type json.context
    --->    context(
                stream_name   :: string,
                line_number   :: int,
                column_number :: int
            ).

    % This type describes errors that can occur while reading JSON data.
    %
:- type json.error(Error)
    --->    stream_error(Error)
            % An error has occurred in the underlying character stream.

    ;       json_error(
                error_context :: json.context,
                error_desc    :: json.error_desc
            ).

:- type json.error_desc
    --->    unexpected_eof(maybe(string))
            % unexpected end-of-file: Msg

    ;       syntax_error(string, maybe(string))
            % syntax_error(Where, MaybeMsg)
            % syntax error at 'Where': MaybeMsg

    ;       invalid_character_escape(char)

    ;       unexpected_value(string, maybe(string))

    ;       duplicate_object_member(string)

    ;       unterminated_multiline_comment

    ;       invalid_unicode_character(string)

    ;       unpaired_utf16_surrogate

    ;       illegal_start_character(char)
            % A JSON value begins with an illegal character.
            % One of: '}', ']', ',' or ':'.

    ;       illegal_unicode_escape_character(char)
            % A character occurred inside a Unicode escape that is not
            % a hexadecimal digit.

    ;       non_finite_number(string)
            % A number was read but after conversion to a float it was of
            % infinite magnitude.

    ;       illegal_negation(char)
            % In a context where a number is expected, '-' seen but the
            % following character (given by the argument) was not a digit.

    ;       illegal_comment_start(char)
            % We have just seen '/' and are expecting to see either '/'
            % or '*' but instead saw character given in the argument.

    ;       bad_signed_exponent(char, char)
            % bad_signed_exponent(SignChar, Char):
            % We have a signed exponent with the sign given by the SignChar,
            % but the following character, Char, is not a decimal digit.

    ;       bad_exponent(char, char)
            % bad_exponent(ExpChar, Char):
            % We have an exponent with the exponent beginning with ExpChar
            % (either 'e' or 'E'), but the following character, Char, is
            % not '+', '-' or a decimal digit.

    ;       expected_eof(string).

:- instance stream.error(json.error(Error)) <= stream.error(Error).

    % Exceptions of this type are thrown by some procedures in this module if a
    % number of infinite magnitude or non-a-number value is encountered.
    % The argument may contain additional information.
    %
:- type json.non_finite_number_error
    --->    non_finite_number_error(string).

%----------------------------------------------------------------------------%
%
% Wrappers for the standard stream result types.
%

:- type json.res(T, Error) == stream.res(T, json.error(Error)).

:- type json.res(Error) == stream.res(json.error(Error)).

:- type json.result(T, Error) == stream.result(T, json.error(Error)).

:- type json.maybe_partial_res(T, Error) ==
    stream.maybe_partial_res(T, json.error(Error)).

%-----------------------------------------------------------------------------%
%
% Mercury representation of JSON values.
%

:- type json.value
    --->    null
    ;       bool(bool)
    ;       string(string)
    ;       number(float)
    ;       object(json.object)
    ;       array(json.array).

:- type json.object == map(string, json.value).

:- type json.array == list(json.value).

    % int(I) = number(float(I)).
    %
:- func int(int) = json.value.

    % make_object(Members, Value):
    % Value is the JSON object constructed from the name - value pairs in
    % the association list Members.
    % False if a name occurs multiple times in Members.
    %
:- pred make_object(assoc_list(string, json.value)::in, json.value::out)
    is semidet.

    % As above, but throw a software_error/1 exception if a name occurs
    % multiple times.
    %
:- func det_make_object(assoc_list(string, json.value)) = json.value.

%-----------------------------------------------------------------------------%
%
% Procedures for working with JSON values.
%

    % The following are true iff the given value is a JSON value of the
    % type specified by the predicate name.
    %
:- pred is_null(value::in) is semidet.
:- pred is_bool(value::in) is semidet.
:- pred is_string(value::in) is semidet.
:- pred is_number(value::in) is semidet.
:- pred is_object(value::in) is semidet.
:- pred is_array(value::in) is semidet.

    % The following are true iff the given value is a JSON value of the type
    % specified by the predicate name.  They return the underlying value.
    % The are false if the given value is not a JSON value of the type
    % specified by the predicate name.
    %
:- pred get_bool(value::in, bool::out) is semidet.
:- pred get_string(value::in, string::out) is semidet.
:- pred get_number(value::in, float::out) is semidet.
:- pred get_object(value::in, object::out) is semidet.
:- pred get_array(value::in, array::out) is semidet.

    % Return the value of a JSON number as an integer.
    % The fractional part is truncated.
    %
:- pred get_int(value::in, int::out) is semidet.

    % As above, but calls error/1 if the given value is not a JSON value of the
    % type specified by the predicate name.
    %
:- func det_get_bool(value) = bool.
:- func det_get_string(value) = string.
:- func det_get_number(value) = float.
:- func det_get_object(value) = object.
:- func det_get_array(value) = array.

:- func det_get_int(value) = int.

%-----------------------------------------------------------------------------%
%
% Procedures for working with JSON objects.
%

    % lookup_<type>(Object, Member) = Value:
    % Lookup Member in Object and return the underlying value if it is a
    % JSON value of the type specified by the predicate name.
    % Calls error/1 if Member is not a member of Object or if the member value
    % is not a JSON value of the type specified by the predicate name.
    %
:- func lookup_bool(object, string) = bool.
:- func lookup_string(object, string) = string.
:- func lookup_number(object, string) = float.
:- func lookup_object(object, string) = object.
:- func lookup_array(object, string) = array.

:- func lookup_int(object, string) = int.

    % search_<type>(Object, Member, DefaultValue) = Value:
    % Lookup Member in Object and return the underlying value if it is a JSON
    % value of the type specified by the predicate name.  Calls error/1 if the
    % member value is not a JSON value of the type specified by the predicate
    % name.  If Member is not a member of Object, return DefaultValue.
    %
:- func search_bool(object, string, bool) = bool.
:- func search_string(object, string, string) = string.
:- func search_number(object, string, float) = float.
:- func search_object(object, string, object) = object.
:- func search_array(object, string, array) = array.

:- func search_int(object, string, int) = int.

%-----------------------------------------------------------------------------%
%
% JSON reader.
%

    % A JSON reader gets JSON values from an underlying character stream.
    %
:- type json.reader(Stream).

:- type json.reader_params
    --->    reader_params(
                allow_comments         :: allow_comments,
                allow_trailing_commas  :: allow_trailing_commas,
                allow_repeated_members :: allow_repeated_members,
                allow_infinities       :: allow_infinities
            ).

    % Should the extension that allows comments in the JSON being read be
    % enabled?
    %
:- type json.allow_comments
    --->    allow_comments
    ;       do_not_allow_comments.

    % Should the extension that allows trailing commas in JSON objects and
    % arrays be enabled?
    %
:- type json.allow_trailing_commas
    --->    allow_trailing_commas
    ;       do_not_allow_trailing_commas.

    % Should the extension that allows -Infinity and Infinity as numbers
    % be enabled?
    %
:- type json.allow_infinities
    --->    allow_infinities
    ;       do_not_allow_infinities.

    % Should we allow repeated object members in JSON objects?
    % (And if so, how should that situation be handled?)
    %
:- type json.allow_repeated_members
     --->   do_not_allow_repeated_members
            % Return an error if a repeated object member is encountered.

    ;       allow_repeated_members_keep_first
            % If any object members are repeated, keep the first one that we
            % encounter and discard any others.

    ;       allow_repeated_members_keep_last.
            % If any object members are repeated, keep the last one that we
            % encounter and discard any others.

    % init_reader(Stream, Reader, !State):
    % Reader is a new JSON reader using Stream as a character stream and using
    % the default reader parameters.  With the default parameters the reader
    % will conform to the RFC 7159 definition of JSON.
    %
:- pred init_reader(Stream::in, reader(Stream)::out, State::di, State::uo)
        is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % init_reader(Stream, Parameters, Reader, !State):
    % As above, but allow reader parameters to be set by the caller.
    %
:- pred init_reader(Stream::in, reader_params::in, reader(Stream)::out,
        State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Reading JSON values.
%

% The 'read' operations here read a value from the underlying stream using the
% reader.  They return an error if there are any non-whitespace characters in
% the stream after the value that has been read.  (Comments after the value are
% also allowed if the reader allows comments.)

    % read_value(Reader, Result, !State):
    % Read a JSON value from Reader.
    %
:- pred read_value(json.reader(Stream)::in,
    json.result(json.value, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % read_object(Reader, Result, !State):
    % Read a JSON object from Reader.
    %
:- pred read_object(json.reader(Stream)::in,
    json.result(json.object, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % read_array(Reader, Result, !State):
    % Read a JSON array from Reader.
    %
:- pred read_array(json.reader(Stream)::in,
    json.result(json.array, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Getting JSON values.
%

% The 'get' operations here read a value from the underlying stream using the
% reader.  In contrast to the 'read' operations above they do not examine the
% underlying character stream any further once a value has been read.  The
% 'get' operations can be used read a stream of JSON values.

    % get_value(Reader, Result, !State):
    % Get a JSON value from Reader.
    %
:- pred get_value(json.reader(Stream)::in,
    json.result(json.value, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % get_object(Reader, Result, !State):
    % Get a JSON object from Reader.
    %
:- pred get_object(json.reader(Stream)::in,
    json.result(json.object, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % get_array(Reader, Result, !State):
    % Get a JSON array from Reader.
    %
:- pred get_array(json.reader(Stream)::in,
    json.result(json.array, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Folding over object members.
%

    % object_fold(Reader, Pred, InitialAcc, Result, !State):
    %
:- pred object_fold(json.reader(Stream), pred(string, json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode object_fold(in, in(pred(in, in, in, out) is det),
    in, out, di, uo) is det.
:- mode object_fold(in, in(pred(in, in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % object_fold_state(Reader, Pred, InitialAcc, Result, !State):
    %
:- pred object_fold_state(json.reader(Stream),
    pred(string, json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode object_fold_state(in, in(pred(in, in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode object_fold_state(in, in(pred(in, in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

%-----------------------------------------------------------------------------%
%
% Folding over array elements.
%

    % array_fold(Reader, Pred, InitialAcc, Result, !State):
    %
:- pred array_fold(json.reader(Stream), pred(json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode array_fold(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode array_fold(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % array_fold(Reader, Pred, InitialAcc, Result, !State):
    %
:- pred array_fold_state(json.reader(Stream),
    pred(json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode array_fold_state(in, in(pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode array_fold_state(in, in(pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

%-----------------------------------------------------------------------------%
%
% Stream type class instances for JSON readers.
%

% XXX these should be polymorphic in the stream state type, but unfortunately
% restrictions in the type class system mean this is not currently possible.

:- instance stream.stream(json.reader(Stream), io)
    <= stream.stream(Stream, io).

:- instance stream.input(json.reader(Stream), io) <= stream.input(Stream, io).

:- instance stream.reader(json.reader(Stream), json.value, io,
    json.error(Error))
    <= (
        stream.line_oriented(Stream, io),
        stream.putback(Stream, char, io, Error)
    ).

%-----------------------------------------------------------------------------%
%
% JSON writer.
%

    % A JSON writer puts JSON values to an underlying string writer stream.
    %
:- type json.writer(Stream).

:- type json.writer_params
    --->    writer_params(
                output_style            :: output_style,
                output_allow_infinities :: allow_infinities,
                output_member_filter    :: member_filter
            ).

    % This function provides backwards compatibility with older versions of
    % this library.
    %
:- func writer_params(output_style::in, allow_infinities::in) =
    (writer_params::out(writer_params)) is det.

:- inst json.writer_params
    --->    writer_params(ground, ground, member_filter).

:- type json.output_style
    --->    compact
    ;       pretty.

    % A member filter is a semidet predicate whose two arguments are an object
    % member name and its corresponding value.  If the predicate succeeds then
    % the JSON writer will omit that member when the object is written.
    %
:- type json.member_filter
    --->    no_member_filter
    ;       member_filter(pred(string, value)).

:- inst json.member_filter
    --->    no_member_filter
    ;       member_filter(pred(in, in) is semidet).

    % init_writer(Stream, Writer, !State):
    % Writer is a new JSON writer that writes JSON values to Stream.
    %
:- pred init_writer(Stream::in, writer(Stream)::out, State::di, State::uo)
        is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

    % init_writer(Stream, Parameters, Writer, !State):
    %
:- pred init_writer(Stream::in, writer_params::in(writer_params),
        writer(Stream)::out, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

    % put_value(Writer, Value, !State):
    % Write the JSON value Value using the given Writer.
    % Throws an exception if the value is, or contains, a non-finite number.
    %
:- pred put_value(json.writer(Stream)::in, json.value::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- type json.comment
    --->    comment_eol(string)
    ;       comment_multiline(string).

    % put_comment(Writer, Comment, !State):
    %
:- pred put_comment(json.writer(Stream)::in, json.comment::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

%-----------------------------------------------------------------------------%
%
% Writing JSON to file streams.
%

% The following convenience predicate can be used to write JSON values to text
% output streams.

    % Write a JSON value to the current output stream using the compact output
    % style.
    %
:- pred write_compact(value::in, io::di, io::uo) is det.

    % Write a JSON value to the current output stream using the pretty output
    % style.
    %
:- pred write_pretty(value::in, io::di, io::uo) is det.

    % Write a JSON value to the specified output stream using the compact
    % output style.
    %
:- pred write_compact(io.text_output_stream::in, value::in,
    io::di, io::uo) is det.

    % Write a JSON value to the specified output stream using the pretty output
    % style.
    %
:- pred write_pretty(io.text_output_stream::in, value::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% String conversion.
%
    % Convert a JSON value into a string.
    % Throws an exception if the JSON value is, or contains, a non-finite
    % number.
    %
:- func to_string(value) = string.

:- pred from_string(string::in, value::out) is semidet.

:- func det_from_string(string) = value.

%-----------------------------------------------------------------------------%
%
% Marshaling between Mercury types and JSON objects.
%

% Marshaling to and from Mercury types and JSON values is handled by making
% the Mercury types instances of the to_json/1 and from_json/1 type classes.
%
% This module provides instances of these type classes for Mercury's primitive
% types and many of the types defined by the standard library.  The mapping
% between these types and JSON is as follows:
%
% Builtin Types
% -------------
%
%     Mercury Type      JSON
%     ------------      ----
%     int               number (cannot have a fractional part)
%     string            string
%     float##           number (only finite floats can be converted to JSON)
%     char              string (of length 1)
%
% Library Types
% -------------
%
% The following standard library types are handled specially, either to reduce
% the size of their JSON representation or to make it more readable.
%
%     Mercury Type      JSON
%     ------------      ----
%     array/1           array
%     bool/0            Boolean
%     bimap/2           array of objects (pairs -- see below)
%     bitmap/0          string (as per bitmap.to_string/1)
%     cord/1            array
%     date/0            string (as per calendar.date_to_string/1)
%     duration/0        string (as per calendar.duration_to_string/1)
%     integer/0         string (decimal representation)
%     list/1            array
%     map/2             array of objects (pairs)
%     maybe/1           null for 'no' or argument of 'yes'
%     pair/2            object with two members: "fst" and "snd"
%     set/1             array
%     set_bbbtree/1     array
%     set_ctree234/1    array
%     set_tree234/1     array
%     set_unordlist/1   array
%     version_array/1   array
%
% Note that for types that lack a canonical representation, the JSON marshaler
% will not necessarily preserve structural equality.  That is, the following
% may be false:
%
%     J = json.from_type(T),
%     ok(TPrime) = json.to_type(J),
%     T = TPrime
%
%-----------------------------------------------------------------------------%

:- typeclass to_json(T) where [
    func to_json(T) = json.value
].

:- instance to_json(int).
:- instance to_json(float).
:- instance to_json(string).
:- instance to_json(char).
:- instance to_json(bool).
:- instance to_json(integer).
:- instance to_json(date).
:- instance to_json(duration).
:- instance to_json(pair(A, B)) <= (to_json(A), to_json(B)).
:- instance to_json(list(T)) <= to_json(T).
:- instance to_json(cord(T)) <= to_json(T).
:- instance to_json(array(T)) <= to_json(T).
:- instance to_json(version_array(T)) <= to_json(T).
:- instance to_json(bitmap).
:- instance to_json(set_ordlist(T)) <= to_json(T).
:- instance to_json(set_unordlist(T)) <= to_json(T).
:- instance to_json(set_tree234(T)) <= to_json(T).
:- instance to_json(set_ctree234(T)) <= to_json(T).
:- instance to_json(set_bbbtree(T)) <= to_json(T).
:- instance to_json(maybe(T)) <= to_json(T).
:- instance to_json(map(K, V)) <= (to_json(K), to_json(V)).
:- instance to_json(bimap(K, V)) <= (to_json(K), to_json(V)).

    % from_type(Type) = Value:
    %
:- func from_type(T) = json.value <= to_json(T).

%-----------------------------------------------------------------------------%

:- typeclass from_json(T) where [
    func from_json(json.value) = maybe_error(T)
].

:- instance from_json(int).
:- instance from_json(float).
:- instance from_json(char).
:- instance from_json(string).
:- instance from_json(bool).
:- instance from_json(integer).
:- instance from_json(date).
:- instance from_json(duration).
:- instance from_json(pair(A, B)) <= (from_json(A), from_json(B)).
:- instance from_json(list(T)) <= from_json(T).
:- instance from_json(cord(T)) <= from_json(T).
:- instance from_json(array(T)) <= from_json(T).
:- instance from_json(version_array(T)) <= from_json(T).
:- instance from_json(bitmap).
:- instance from_json(set_ordlist(T)) <= from_json(T).
:- instance from_json(set_unordlist(T)) <= from_json(T).
:- instance from_json(set_tree234(T)) <= from_json(T).
:- instance from_json(set_ctree234(T)) <= from_json(T).
:- instance from_json(set_bbbtree(T)) <= from_json(T).
:- instance from_json(maybe(T)) <= from_json(T).
:- instance from_json(map(K, V)) <= (from_json(K), from_json(V)).
:- instance from_json(bimap(K, V)) <= (from_json(K), from_json(V)).

    % to_type(Value) = MaybeType:
    % MaybeType is 'ok(Type)' if Value is a JSON object corresponding
    % to the Mercury value Type and 'error(...)' otherwise.
    %
:- func to_type(json.value) = maybe_error(T) <= from_json(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module json.char_buffer.
:- include_module json.json_lexer.
:- include_module json.json_parser.
:- include_module json.marshal.
:- include_module json.string_reader.
:- include_module json.unmarshal.
:- include_module json.writer.

:- import_module json.char_buffer.
:- import_module json.json_lexer.
:- import_module json.json_parser.
:- import_module json.marshal.
:- import_module json.string_reader.
:- import_module json.unmarshal.
:- import_module json.writer.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module float.
:- import_module mutvar.
:- import_module pair.
:- import_module string.
:- import_module string.builder.
:- import_module require.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%
%
% JSON reader.
%

:- type json.reader(Stream)
    --->    json_reader(
                json_reader_stream    :: Stream,
                json_comments         :: allow_comments,
                json_trailing_commas  :: allow_trailing_commas,
                json_repeated_members :: allow_repeated_members,
                json_infinities       :: allow_infinities,
                json_column_number    :: mutvar(int),
                json_char_buffer      :: char_buffer
            ).

init_reader(Stream, Reader, !State) :-
    promise_pure (
        impure new_mutvar(0, ColNumVar),
        impure char_buffer.init(CharBuffer),
        Reader = json_reader(
            Stream,
            do_not_allow_comments,
            do_not_allow_trailing_commas,
            do_not_allow_repeated_members,
            do_not_allow_infinities,
            ColNumVar,
            CharBuffer
        ),
        !:State = !.State
    ).

init_reader(Stream, Params, Reader, !State) :-
    Params = reader_params(
        AllowComments,
        AllowTrailingCommas,
        RepeatedMembers,
        AllowInfinities
    ),
    promise_pure (
        impure new_mutvar(0, ColNumVar),
        impure char_buffer.init(CharBuffer),
        Reader = json_reader(
            Stream,
            AllowComments,
            AllowTrailingCommas,
            RepeatedMembers,
            AllowInfinities,
            ColNumVar,
            CharBuffer
        ),
        !:State = !.State
    ).

%-----------------------------------------------------------------------------%

read_value(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    do_read_value(Reader, Token, Result, !State).

read_object(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    % Save the context of the beginning of the value.
    make_error_context(Reader, Context, !State),
    do_read_value(Reader, Token, ValueResult, !State),
    (
        ValueResult = ok(Value),
        (
            Value = object(Members),
            Result = ok(Members)
        ;
            ( Value = null
            ; Value = bool(_)
            ; Value = string(_)
            ; Value = number(_)
            ; Value = array(_)
            ),
            Msg = "value must be an object",
            ValueDesc = value_desc(Value),
            ErrorDesc = unexpected_value(ValueDesc, yes(Msg)),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ValueResult = eof,
        Result = eof
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

read_array(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    % Save the context of the beginning of the value.
    make_error_context(Reader, Context, !State),
    do_read_value(Reader, Token, ValueResult, !State),
    (
        ValueResult = ok(Value),
        (
            Value = array(Elements),
            Result = ok(Elements)
        ;
            ( Value = null
            ; Value = bool(_)
            ; Value = string(_)
            ; Value = number(_)
            ; Value = object(_)
            ),
            Msg = "value must be an array",
            ValueDesc = value_desc(Value),
            ErrorDesc = unexpected_value(ValueDesc, yes(Msg)),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ValueResult = eof,
        Result = eof
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------------%

get_value(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    do_get_value(Reader, Token, Result, !State).

get_object(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    % Save the context of the beginning of the value.
    make_error_context(Reader, Context, !State),
    do_get_value(Reader, Token, ValueResult, !State),
    (
        ValueResult = ok(Value),
        (
            Value = object(Members),
            Result = ok(Members)
        ;
            ( Value = null
            ; Value = bool(_)
            ; Value = string(_)
            ; Value = number(_)
            ; Value = array(_)
            ),
            Msg = "value must be an object",
            ValueDesc = value_desc(Value),
            ErrorDesc = unexpected_value(ValueDesc, yes(Msg)),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ValueResult = eof,
        Result = eof
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

get_array(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    % Save the context of the beginning of the value.
    make_error_context(Reader, Context, !State),
    do_get_value(Reader, Token, ValueResult, !State),
    (
        ValueResult = ok(Value),
        (
            Value = array(Elements),
            Result = ok(Elements)
        ;
            ( Value = null
            ; Value = bool(_)
            ; Value = string(_)
            ; Value = number(_)
            ; Value = object(_)
            ),
            Msg = "value must be an array",
            ValueDesc = value_desc(Value),
            ErrorDesc = unexpected_value(ValueDesc, yes(Msg)),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ValueResult = eof,
        Result = eof
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------------%
%
% Folding over object members.
%

object_fold(Reader, Pred, !.Acc, Result, !State) :-
    do_object_fold(Reader, Pred, !.Acc, Result, !State).

object_fold_state(Reader, Pred, !.Acc, Result, !State) :-
    do_object_fold_state(Reader, Pred, !.Acc, Result, !State).

%-----------------------------------------------------------------------------%
%
% Folding over array elements.
%

array_fold(Reader, Pred, !.Acc, Result, !State) :-
    do_array_fold(Reader, Pred, !.Acc, Result, !State).

array_fold_state(Reader, Pred, !.Acc, Result, !State) :-
    do_array_fold_state(Reader, Pred, !.Acc, Result, !State).

%-----------------------------------------------------------------------------%
%
% JSON writer.
%

:- type json.writer(Stream)
    --->    json_writer(
                json_writer_stream     :: Stream,
                json_output_style      :: output_style,
                json_output_infinities :: allow_infinities,
                json_member_filter     :: member_filter
            ).

init_writer(Stream, Writer, !State) :-
    Writer = json_writer(Stream, compact, do_not_allow_infinities,
        no_member_filter).

init_writer(Stream, Parameters, Writer, !State) :-
    Parameters = writer_params(OutputStyle, AllowInfinities, MemberFilter),
    Writer = json_writer(Stream, OutputStyle, AllowInfinities, MemberFilter).

writer_params(OutputStyle, AllowInfinities) =
    writer_params(OutputStyle, AllowInfinities, no_member_filter).

%-----------------------------------------------------------------------------%

put_value(Writer, Value, !State) :-
    Writer = json_writer(Stream, OutputStyle, AllowInfinities,
        MemberFilter0),
    cast_member_filter_to_pred(MemberFilter0, MemberFilter),
    (
        OutputStyle = compact,
        writer.raw_put_value(Stream, AllowInfinities, MemberFilter,
            Value, !State)
    ;
        OutputStyle = pretty,
        writer.pretty_put_value(Stream, AllowInfinities, MemberFilter,
            Value, !State)
    ).

put_comment(Writer, Comment, !State) :-
    Stream = Writer ^ json_writer_stream,
    (
        Comment = comment_eol(String),
        writer.put_eol_comment(Stream, String, !State)
    ;
        Comment = comment_multiline(String),
        writer.put_multiline_comment(Stream, String, !State)
    ).

:- pred cast_member_filter_to_pred(member_filter::in,
    member_filter::out(member_filter)) is det.

:- pragma foreign_proc("C",
    cast_member_filter_to_pred(A::in, B::out(member_filter)),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    B = A;
").

:- pragma foreign_proc("Java",
    cast_member_filter_to_pred(A::in, B::out(member_filter)),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = A;
").

:- pragma foreign_proc("C#",
    cast_member_filter_to_pred(A::in, B::out(member_filter)),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = A;
").

:- pragma foreign_proc("Erlang",
    cast_member_filter_to_pred(A::in, B::out(member_filter)),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = A;
").

%-----------------------------------------------------------------------------%
%
% Writing JSON values to file streams.
%

write_compact(Value, !IO) :-
    io.stdout_stream(Stdout, !IO),
    write_compact(Stdout, Value, !IO).

write_pretty(Value, !IO) :-
    io.stdout_stream(Stdout, !IO),
    write_pretty(Stdout, Value, !IO).

write_compact(File, Value, !IO) :-
    Params = writer_params(compact, do_not_allow_infinities, no_member_filter),
    init_writer(File, Params, Writer, !IO),
    put_value(Writer, Value, !IO).

write_pretty(File, Value, !IO) :-
    Params = writer_params(pretty, do_not_allow_infinities, no_member_filter),
    init_writer(File, Params, Writer, !IO),
    put_value(Writer, Value, !IO).

%-----------------------------------------------------------------------------%
%
% Marshaling / Unmarshaling.
%

from_type(T) = to_json(T).

:- instance to_json(int) where [
    func(to_json/1) is json.marshal.int_to_json
].
:- instance to_json(float) where [
    func(to_json/1) is json.marshal.float_to_json
].
:- instance to_json(char) where [
    func(to_json/1) is json.marshal.char_to_json
].
:- instance to_json(string) where [
    func(to_json/1) is json.marshal.string_to_json
].
:- instance to_json(bool) where [
    func(to_json/1) is json.marshal.bool_to_json
].
:- instance to_json(integer) where [
    func(to_json/1) is json.marshal.integer_to_json
].
:- instance to_json(date) where [
    func(to_json/1) is json.marshal.date_time_to_json
].
:- instance to_json(duration) where [
    func(to_json/1) is json.marshal.duration_to_json
].
:- instance to_json(pair(A, B)) <= (to_json(A), to_json(B)) where [
    func(to_json/1) is json.marshal.pair_to_json
].
:- instance to_json(list(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.list_to_json
].
:- instance to_json(cord(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.cord_to_json
].
:- instance to_json(array(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.array_to_json
].
:- instance to_json(version_array(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.version_array_to_json
].
:- instance to_json(bitmap) where [
    func(to_json/1) is json.marshal.bitmap_to_json
].
:- instance to_json(set_ordlist(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.set_ordlist_to_json
].
:- instance to_json(set_unordlist(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.set_unordlist_to_json
].
:- instance to_json(set_tree234(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.set_tree234_to_json
].
:- instance to_json(set_ctree234(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.set_ctree234_to_json
].
:- instance to_json(set_bbbtree(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.set_bbbtree_to_json
].
:- instance to_json(maybe(T)) <= to_json(T) where [
    func(to_json/1) is json.marshal.maybe_to_json
].
:- instance to_json(map(K, V)) <= (to_json(K), to_json(V)) where [
    func(to_json/1) is json.marshal.map_to_json
].
:- instance to_json(bimap(K, V)) <= (to_json(K), to_json(V)) where [
    func(to_json/1) is json.marshal.bimap_to_json
].

%-----------------------------------------------------------------------------%

:- instance from_json(int) where [
    func(from_json/1) is json.unmarshal.int_from_json
].
:- instance from_json(float) where [
    func(from_json/1) is json.unmarshal.float_from_json
].
:- instance from_json(char) where [
    func(from_json/1) is json.unmarshal.char_from_json
].
:- instance from_json(string) where [
    func(from_json/1) is json.unmarshal.string_from_json
].
:- instance from_json(bool) where [
    func(from_json/1) is json.unmarshal.bool_from_json
].
:- instance from_json(integer) where [
    func(from_json/1) is json.unmarshal.integer_from_json
].
:- instance from_json(date) where [
    func(from_json/1) is json.unmarshal.date_time_from_json
].
:- instance from_json(duration) where [
    func(from_json/1) is json.unmarshal.duration_from_json
].
:- instance from_json(pair(A, B)) <= (from_json(A), from_json(B)) where [
    func(from_json/1) is json.unmarshal.pair_from_json
].
:- instance from_json(list(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.list_from_json
].
:- instance from_json(cord(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.cord_from_json
].
:- instance from_json(array(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.array_from_json
].
:- instance from_json(version_array(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.version_array_from_json
].
:- instance from_json(bitmap) where [
    func(from_json/1) is json.unmarshal.bitmap_from_json
].
:- instance from_json(set_ordlist(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.set_ordlist_from_json
].
:- instance from_json(set_unordlist(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.set_unordlist_from_json
].
:- instance from_json(set_tree234(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.set_tree234_from_json
].
:- instance from_json(set_ctree234(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.set_ctree234_from_json
].
:- instance from_json(set_bbbtree(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.set_bbbtree_from_json
].
:- instance from_json(maybe(T)) <= from_json(T) where [
    func(from_json/1) is json.unmarshal.maybe_from_json
].
:- instance from_json(map(K, V)) <= (from_json(K), from_json(V)) where [
    func(from_json/1) is json.unmarshal.map_from_json
].
:- instance from_json(bimap(K, V)) <= (from_json(K), from_json(V)) where [
    func(from_json/1) is json.unmarshal.bimap_from_json
].

to_type(V) = from_json(V).

%-----------------------------------------------------------------------------%

:- pred make_unexpected_eof_error(json.reader(Stream)::in, maybe(string)::in,
    json.error(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_unexpected_eof_error(Reader, MaybeMsg, Error, !State) :-
    make_error_context(Reader, Context, !State),
    Error = json_error(Context, unexpected_eof(MaybeMsg)).

:- pred make_syntax_error(json.reader(Stream)::in, string::in,
    maybe(string)::in, json.error(Error)::out, State::di, State::uo)
    is det <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_syntax_error(Reader, Where, MaybeMsg, Error, !State) :-
    make_error_context(Reader, Context, !State),
    Error = json_error(Context, syntax_error(Where, MaybeMsg)).

:- pred make_error_context(json.reader(Stream)::in,
    json.context::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_error_context(Reader, Context, !State) :-
    promise_pure (
        Stream = Reader ^ json_reader_stream,
        stream.name(Stream, Name, !State),
        stream.get_line(Stream, LineNo, !State),
        ColNumVar = Reader ^ json_column_number,
        impure get_mutvar(ColNumVar, ColNum),
        Context = context(Name, LineNo, ColNum)
    ).

%-----------------------------------------------------------------------------%

:- instance stream.error(json.error(Error)) <= stream.error(Error) where
[
    func(error_message/1) is make_error_message
].

:- func make_error_message(json.error(Error)) = string <= stream.error(Error).

make_error_message(Error) = Msg :-
    (
        Error = stream_error(StreamError),
        Msg = stream.error_message(StreamError)
    ;
        Error = json_error(Context, ErrorDesc),
        Context = context(StreamName, LineNo, ColNo),
        string.format("%s:%d:%d", [s(StreamName), i(LineNo), i(ColNo)],
            ContextStr),
        (
            ErrorDesc = unexpected_eof(MaybeExtraMsg),
            (
                MaybeExtraMsg = no,
                string.format("%s: error: unexpected end-of-file\n",
                    [s(ContextStr)], Msg)
            ;
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s: error: unexpected end-of-file: %s\n",
                    [s(ContextStr), s(ExtraMsg)], Msg)
            )
        ;
            ErrorDesc = syntax_error(Where, MaybeExtraMsg),
            (
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s: syntax error at '%s': %s\n",
                    [s(ContextStr), s(Where), s(ExtraMsg)], Msg)
            ;
                MaybeExtraMsg = no,
                string.format("%s: syntax error at '%s'\n",
                    [s(ContextStr), s(Where)], Msg)
            )
        ;
            ErrorDesc = invalid_character_escape(What),
            string.format("%s: error: invalid character escape: '\\%c'\n",
                [s(ContextStr), c(What)], Msg)
        ;
            ErrorDesc = unexpected_value(What, MaybeExtraMsg),
            (
                MaybeExtraMsg = no,
                string.format("%s: error: unexpected %s value\n",
                    [s(ContextStr), s(What)], Msg)
            ;
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s: error: unexpected %s value: %s\n",
                    [s(ContextStr), s(What), s(ExtraMsg)], Msg)
            )
        ;
            ErrorDesc = duplicate_object_member(Name),
            string.format(
                "%s: error: object member \"%s\" is not unique\n",
                [s(ContextStr), s(Name)], Msg)
        ;
            ErrorDesc = unterminated_multiline_comment,
            string.format("%s: error: unterminated multiline comment\n",
                [s(ContextStr)], Msg)
        ;
            ErrorDesc = invalid_unicode_character(What),
            string.format("%s: error: invalid Unicode character: \\u%s\n",
                [s(ContextStr), s(What)], Msg)
        ;
            ErrorDesc = unpaired_utf16_surrogate,
            string.format("%s: error: unpaired UTF-16 surrogate\n",
                [s(ContextStr)], Msg)
        ;
            ErrorDesc = illegal_start_character(Char),
            string.format("%s: error: '%c' at start of JSON value\n",
                [s(ContextStr), c(Char)], Msg)
        ;
            ErrorDesc = illegal_unicode_escape_character(Char),
            string.format("%s: error: character" ++
                " in Unicode escape is not a hexadecimal digit: '%c'\n",
                [s(ContextStr), c(Char)], Msg)
        ;
            ErrorDesc = non_finite_number(NumberStr),
            string.format("%s: error: non-finite number: %s\n",
                [s(ContextStr), s(NumberStr)], Msg)

        ;   ErrorDesc = illegal_negation(Char),
            string.format(
                "%s: error: expected a decimal digit after '-', got '%c'\n",
                [s(ContextStr), c(Char)], Msg)
        ;
            ErrorDesc = illegal_comment_start(Char),
            string.format(
                "%s: error: expected '/' or '*' after '/' in" ++
                " comment start, got '%c'\n",
                [s(ContextStr), c(Char)], Msg)
        ;
            ErrorDesc = bad_signed_exponent(SignChar, Char),
            string.format(
                "%s: error: expected a decimal digit " ++
                "after '%c' in exponent, got '%c'\n",
                [s(ContextStr), c(SignChar), c(Char)], Msg)
        ;
            ErrorDesc = bad_exponent(ExpChar, Char),
            string.format(
                "%s: error: expected '+', '-' or a decimal digit after " ++
                "'%c', got '%c'\n",
                [s(ContextStr), c(ExpChar), c(Char)], Msg)
        ;
            ErrorDesc = expected_eof(Found),
            string.format(
                "%s: error: expected end-of-file, got '%s'\n",
                [s(ContextStr), s(Found)], Msg)
        )
    ).

%-----------------------------------------------------------------------------%

:- func value_desc(json.value) = string.

value_desc(null) = "null".
value_desc(bool(_)) = "Boolean".
value_desc(string(_)) = "string".
value_desc(number(_)) = "number".
value_desc(object(_)) = "object".
value_desc(array(_)) = "array".

%-----------------------------------------------------------------------------%
%
% Stream type class instances for JSON readers.
%

% XXX these should be polymorphic in the stream state type, but unfortunately
% restrictions in the type class system mean this is not currently possible.

:- instance stream.stream(json.reader(Stream), io)
    <= stream.stream(Stream, io) where
[
    ( name(Reader, Name, !State) :-
        stream.name(Reader ^ json_reader_stream, Name, !State)
    )
].

:- instance stream.input(json.reader(Stream), io)
    <= stream.input(Stream, io) where [].

:- instance stream.reader(json.reader(Stream), json.value, io,
    json.error(Error))
    <= (
        stream.line_oriented(Stream, io),
        stream.putback(Stream, char, io, Error)
    ) where
[
    ( get(Reader, Result, !State) :-
        json.get_value(Reader, Result, !State)
    )
].

%-----------------------------------------------------------------------------%
%
% Utility predicate for (un)marshaling.
%

:- pred all_functors_have_arity_zero(type_desc::in, functor_number_lex::in,
    functor_number_lex::in) is semidet.

all_functors_have_arity_zero(TypeDesc, I, Limit) :-
    ( if I < Limit then
        get_functor(TypeDesc, I, _Name, 0, []),
        all_functors_have_arity_zero(TypeDesc, I + 1, Limit)
    else
        true
    ).

%-----------------------------------------------------------------------------%

int(I) = number(float(I)).

make_object(Members, Value) :-
    list.foldl(add_member, Members, map.init, Object),
    Value = object(Object).

:- pred add_member(pair(string, json.value)::in,
    json.object::in, json.object::out) is semidet.

add_member(Member, !Object) :-
    Member = Name - Value,
    map.insert(Name, Value, !Object).

det_make_object(Members) = Value :-
    ( if make_object(Members, Value0)
    then Value = Value0
    else error("json.det_make_object: repeated member name")
    ).

%-----------------------------------------------------------------------------%

is_null(null).
is_bool(bool(_)).
is_string(string(_)).
is_number(number(_)).
is_object(object(_)).
is_array(array(_)).

get_bool(bool(Bool), Bool).
get_string(string(String), String).
get_number(number(Number), Number).
get_object(object(Object), Object).
get_array(array(Array), Array).

get_int(number(Number), Int) :-
    Int = truncate_to_int(Number).

det_get_bool(Value) =
    ( if get_bool(Value, Bool)
    then Bool
    else func_error("json.det_get_bool: not a JSON Boolean")
    ).

det_get_string(Value) =
    ( if get_string(Value, String)
    then String
    else func_error("json.det_get_string: not a JSON string")
    ).

det_get_number(Value) =
    ( if get_number(Value, Number)
    then Number
    else func_error("json.det_get_number: not a JSON number")
    ).

det_get_object(Value) =
    ( if get_object(Value, Object)
    then Object
    else func_error("json.det_get_object: not a JSON object")
    ).

det_get_array(Value) =
    ( if get_array(Value, Array)
    then Array
    else func_error("json.get_array: not a JSON array")
    ).

det_get_int(Value) =
    ( if get_int(Value, Int)
    then Int
    else func_error("json.get_int: not a JSON number")
    ).

%-----------------------------------------------------------------------------%

lookup_bool(Object, Member) = Bool :-
    Value = lookup(Object, Member),
    Bool = det_get_bool(Value).

lookup_string(Object, Member) = String :-
    Value = lookup(Object, Member),
    String = det_get_string(Value).

lookup_number(Object, Member) = Number :-
    Value = lookup(Object, Member),
    Number = det_get_number(Value).

lookup_object(Object, Member) = ObjectPrime :-
    Value = lookup(Object, Member),
    ObjectPrime = det_get_object(Value).

lookup_array(Object, Member) = Array :-
    Value = lookup(Object, Member),
    Array = det_get_array(Value).

lookup_int(Object, Member) = Int :-
    Value = lookup(Object, Member),
    Int = det_get_int(Value).

search_bool(Object, Member, Default) = Bool :-
    ( if map.search(Object, Member, Value)
    then Bool = det_get_bool(Value)
    else Bool = Default
    ).

search_string(Object, Member, Default) = String :-
    ( if map.search(Object, Member, Value)
    then String = det_get_string(Value)
    else String = Default
    ).

search_number(Object, Member, Default) = Number :-
    ( if map.search(Object, Member, Value)
    then Number = det_get_number(Value)
    else Number = Default
    ).

search_object(Object, Member, Default) = ObjectPrime :-
    ( if map.search(Object, Member, Value)
    then ObjectPrime = det_get_object(Value)
    else ObjectPrime = Default
    ).

search_array(Object, Member, Default) = Array :-
    ( if map.search(Object, Member, Value)
    then Array = det_get_array(Value)
    else Array = Default
    ).

search_int(Object, Member, Default) = Int :-
    ( if map.search(Object, Member, Value)
    then Int = det_get_int(Value)
    else Int = Default
    ).

%-----------------------------------------------------------------------------%

to_string(Value) = String :-
    some [!State] (
        !:State = builder.init,
        json.init_writer(handle, Writer, !State),
        json.put_value(Writer, Value, !State),
        String = builder.to_string(!.State)
    ).

from_string(String, Value) :-
    some [!State] (
        init_string_state(!:State),
        init_string_reader(no, String, StringReader, !State),
        json.init_reader(StringReader, Reader, !State),
        json.read_value(Reader, Result, !.State, _)
    ),
    require_complete_switch [Result] (
        Result = ok(Value)
    ;
        ( Result = error(_)
        ; Result = eof
        ),
        false
    ).

det_from_string(String) = Value :-
    ( if json.from_string(String, Value0)
    then Value = Value0
    else error("json.det_from_string: from_string failed")
    ).

%-----------------------------------------------------------------------------%
:- end_module json.
%-----------------------------------------------------------------------------%

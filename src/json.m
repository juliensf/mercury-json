%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@opturion.com>
%
% A Mercury library for reading and writing JSON.
%
% TODO:
%    - predicates for folding over array elements and fields
%
% XXX how should we handle repeated object members?
%-----------------------------------------------------------------------------%

:- module json.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module stream.

%-----------------------------------------------------------------------------%
%
% JSON errors.
%

:- type json.context
    --->    context(
                stream_name :: string,
                line_number :: int
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
            % syntax error at `Where': MaybeMsg
            
    ;       invalid_character_escape(char)

    ;       unexpected_value(string, maybe(string))

    ;       duplicate_object_member(string)

    ;       unterminated_multiline_comment

    ;       invalid_unicode_character(string)
    
    ;       other(string).

:- instance stream.error(json.error(Error)) <= stream.error(Error).

%----------------------------------------------------------------------------%
%
% Wrappers for the standard stream result types.
%

:- type json.res(T, Error) == stream.res(T, json.error(Error)).

:- type json.res(Error) == stream.res(json.error(Error)).

:- type json.result(T, Error) == stream.result(T, json.error(Error)).

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

    % A JSON text is a serialized object or array.
    %
:- type json.text
    --->    object(json.object)
    ;       array(json.array).

:- type json.object == map(string, json.value).

:- type json.array == list(json.value).

%-----------------------------------------------------------------------------%
%
% JSON reader.
%

    % A JSON reader gets JSON values from an underlying characet stream.
    %
:- type json.reader(Stream).
    
    % Should the extension that enables support for // style
    % commens in JSON be enabled?
    %
:- type json.allow_comments
    --->    allow_comments
    ;       do_not_allow_comments.

    % Create a new JSON reader using the given character stream.
    % The reader will conform to the RFC 4627 definition of JSON.
    %
:- func json.init_reader(Stream) = json.reader(Stream)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

    % As above, but allow extensions to be enabled or disabled.
    %
:- func json.init_reader(Stream, allow_comments) = json.reader(Stream)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%

:- pred json.get_value(json.reader(Stream)::in,
    json.result(json.value, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

:- pred json.get_text(json.reader(Stream)::in,
    json.result(json.text, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

:- pred json.get_object(json.reader(Stream)::in,
    json.object::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

:- pred json.get_array(json.reader(Stream)::in,
    json.array::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Stream type class instances for JSON readers.
%

% XXX these should be polymorphic in the stream state type, but unfortunately
% restrictions in the type class system mean this is not currently possible.

:- instance stream.stream(json.reader(Stream), io)
    <= stream.stream(Stream, io).

:- instance stream.input(json.reader(Stream), io) <= stream.input(Stream, io).

:- instance stream.reader(json.reader(Stream), json.value, io, json.error(Error))
    <= (
        stream.line_oriented(Stream, io),
        stream.putback(Stream, char, io, Error)
    ).

%-----------------------------------------------------------------------------%
%
% JSON writer.
%

:- type json.writer(Stream).

:- func json.init_writer(Stream) = json.writer(Stream)
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- pred json.put_json(json.writer(Stream)::in, json.value::in,
    State::di, State::uo) is det 
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module json.char_buffer.
:- include_module json.json_lexer.
:- include_module json.json_parser.
:- include_module json.writer.

:- import_module json.char_buffer.
:- import_module json.json_lexer.
:- import_module json.json_parser.
:- import_module json.writer.

:- import_module int.
:- import_module float.
:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------------%
%
% JSON reader.
%

:- type json.reader(Stream)
    --->    json_reader(
                json_stream   :: Stream,
                json_comments :: allow_comments
            ).

json.init_reader(Stream) =
    json_reader(Stream, do_not_allow_comments).

json.init_reader(Stream, AllowComments) =
    json_reader(Stream, AllowComments).

%-----------------------------------------------------------------------------%

get_value(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    do_get_value(Reader, Token, Result, !State). 

get_text(Reader, Result, !State) :-
    get_token(Reader, Token, !State),
    % Save the context of the beginning of the value.
    make_error_context(Reader ^ json_stream, Context, !State),
    do_get_value(Reader, Token, ValueResult, !State),
    (
        ValueResult = ok(Value),
        (
            Value = object(Members),
            Text = object(Members),
            Result = ok(Text)
        ;
            Value = array(Elements),
            Text = array(Elements),
            Result = ok(Text)
        ;
            ( Value = null
            ; Value = bool(_)
            ; Value = string(_)
            ; Value = number(_)
            ),
            Msg = "text must be an array or object",
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

get_object(_, map.init, !IO). % NYI.
get_array(_, [], !IO).        % NYI.

%-----------------------------------------------------------------------------%
%
% JSON writer.
%

:- type json.writer(Stream)
    --->    json_writer(
                json_writer_stream :: Stream
            ).

json.init_writer(Stream) = json_writer(Stream).

%-----------------------------------------------------------------------------%

put_json(Writer, Value, !State) :-
    % XXX currently, we only have a single output format.
    % If we ever support others, e.g. pretty-printing of the JSON, then
    % that should be handled here.
    writer.raw_put_json(Writer ^ json_writer_stream, Value, !State).

%-----------------------------------------------------------------------------%

:- pred make_unexpected_eof_error(Stream::in, maybe(string)::in,
    json.error(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_unexpected_eof_error(Stream, MaybeMsg, Error, !State) :-
    make_error_context(Stream, Context, !State),
    Error = json_error(Context, unexpected_eof(MaybeMsg)). 

:- pred make_json_error(Stream::in, string::in, json.error(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_json_error(Stream, Msg, Error, !State) :-
    make_error_context(Stream, Context, !State),
    Error = json_error(Context, other(Msg)).

:- pred make_syntax_error(Stream::in, string::in, maybe(string)::in,
    json.error(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_syntax_error(Stream, Where, MaybeMsg, Error, !State) :-
    make_error_context(Stream, Context, !State),
    Error = json_error(Context, syntax_error(Where, MaybeMsg)). 

:- pred make_error_context(Stream::in, json.context::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

make_error_context(Stream, Context, !State) :-
    stream.name(Stream, Name, !State),
    stream.get_line(Stream, LineNo, !State),
    Context = context(Name, LineNo).

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
        Context = context(StreamName, LineNo),
        (
            ErrorDesc = unexpected_eof(MaybeExtraMsg),
            (
                MaybeExtraMsg = no,
                string.format("%s:%d: error: unexpected end-of-file\n",
                    [s(StreamName), i(LineNo)], Msg)
            ;
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s:%d: error: unexpected end-of-file: %s\n",
                    [s(StreamName), i(LineNo), s(ExtraMsg)], Msg)
            )
        ; 
            ErrorDesc = syntax_error(Where, MaybeExtraMsg),
            (
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s:%d: syntax error at '%s': %s\n",
                    [s(StreamName), i(LineNo), s(Where), s(ExtraMsg)], Msg)
            ;
                MaybeExtraMsg = no,
                string.format("%s:%d: syntax error at '%s'\n",
                    [s(StreamName), i(LineNo), s(Where)], Msg)
            )
        ;
            ErrorDesc = invalid_character_escape(What),
            string.format("%s:%d: error: invalid character escape: '\\%c'\n",
                [s(StreamName), i(LineNo), c(What)], Msg)
        ;
            ErrorDesc = other(ErrorMsg),
            string.format("%s:%d: error: %s\n", 
                [s(StreamName), i(LineNo), s(ErrorMsg)], Msg)
        ;
            ErrorDesc = unexpected_value(What, MaybeExtraMsg),
            (
                MaybeExtraMsg = no,
                string.format("%s:%d: error: unexpected %s value\n",
                    [s(StreamName), i(LineNo), s(What)], Msg)
            ;
                MaybeExtraMsg = yes(ExtraMsg),
                string.format("%s:%d: error: unexpected %s value: %s\n",
                    [s(StreamName), i(LineNo), s(What), s(ExtraMsg)], Msg)
            )
        ;
            ErrorDesc = duplicate_object_member(Name),
            string.format("%s:%d: error: object member \"%s\" is not unique\n",
                [s(StreamName), i(LineNo), s(Name)], Msg)
        ;
            ErrorDesc = unterminated_multiline_comment,
            string.format("%s:%d: error: unterminated multiline comment\n",
                [s(StreamName), i(LineNo)], Msg)
        ;
            ErrorDesc = invalid_unicode_character(What),
            string.format("%s:%d: error: invalid Unicode character: \\u%s\n",
                [s(StreamName), i(LineNo), s(What)], Msg)
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
        stream.name(Reader ^ json_stream, Name, !State)
    )
].

:- instance stream.input(json.reader(Stream), io)
    <= stream.input(Stream, io) where [].

:- instance stream.reader(json.reader(Stream), json.value, io, json.error(Error))
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
:- end_module json.
%-----------------------------------------------------------------------------%

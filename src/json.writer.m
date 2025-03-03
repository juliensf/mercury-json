%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2016, 2018-2019, 2025 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module implements writing json.value/0 values to string and character
% output streams.
%
%-----------------------------------------------------------------------------%

:- module json.writer.
:- interface.

%-----------------------------------------------------------------------------%

:- pred raw_put_value(json.writer(Stream)::in, json.value::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- pred pretty_put_value(json.writer(Stream)::in, json.value::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

%-----------------------------------------------------------------------------%
%
% Comments.
%

:- pred put_eol_comment(Stream::in, string::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- pred put_multiline_comment(Stream::in, string::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module pair.
:- import_module stream.string_writer.

%-----------------------------------------------------------------------------%
%
% Raw output.
%

raw_put_value(Writer, Value, !State) :-
    (
        Value = null,
        Stream = Writer ^ json_writer_stream,
        put(Stream, "null", !State)
    ;
        Value = bool(Bool),
        Stream = Writer ^ json_writer_stream,
        (
            Bool = yes,
            put(Stream, "true", !State)
        ;
            Bool = no,
            put(Stream, "false", !State)
        )
    ;
        Value = string(String),
        put_string_literal(Writer, String, !State)
    ;
        Value = number(Number),
        put_number(Writer, Number, !State)
    ;
        Value = object(Members),
        raw_put_object(Writer, Members, !State)
    ;
        Value = array(Elements),
        raw_put_array(Writer, Elements, !State)
    ).

%-----------------------------------------------------------------------------%

:- pred raw_put_object(json.writer(Stream)::in, json.object::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_object(Writer, MembersMap, !State) :-
    Stream = Writer ^ json_writer_stream,
    MemberFilter = Writer ^ json_member_filter,
    put(Stream, '{', !State),
    (
        MemberFilter = no_member_filter,
        MembersList = map.to_assoc_list(MembersMap)
    ;
        MemberFilter = member_filter(FilterPred),
        map.foldr(maybe_filter_member(FilterPred), MembersMap, [], MembersList)
    ),
    raw_put_members(Writer, MembersList, !State),
    put(Stream, '}', !State).

:- pred maybe_filter_member(pred(string, value)::in(pred(in, in) is semidet),
    string::in, value::in,
    assoc_list(string, json.value)::in, assoc_list(string, json.value)::out)
    is det.

maybe_filter_member(FilterPred, Name, Value, !MembersList) :-
    ( if FilterPred(Name, Value) then
        true
    else
        !:MembersList = [Name - Value | !.MembersList]
    ).

:- pred raw_put_members(json.writer(Stream)::in,
    assoc_list(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_members(_, [], !State).
raw_put_members(Writer, [Member], !State) :-
    raw_put_member(Writer, Member, !State).
raw_put_members(Writer, [Member, NextMember | Members], !State) :-
    raw_put_member(Writer, Member, !State),
    Stream = Writer ^ json_writer_stream,
    put(Stream, (','), !State),
    raw_put_members(Writer, [NextMember | Members], !State).

:- pred raw_put_member(json.writer(Stream)::in,
    pair(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_member(Writer, Member, !State) :-
    Member = Key - Value,
    put_string_literal(Writer, Key, !State),
    Stream = Writer ^ json_writer_stream,
    put(Stream, ':', !State),
    raw_put_value(Writer, Value, !State).

%-----------------------------------------------------------------------------%

:- pred raw_put_array(json.writer(Stream)::in,
    json.array::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_array(Writer, Elements, !State) :-
    Stream = Writer ^ json_writer_stream,
    put(Stream, '[', !State),
    raw_put_elements(Writer, Elements, !State),
    put(Stream, ']', !State).

:- pred raw_put_elements(json.writer(Stream)::in, json.array::in,
    State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_elements(Writer, Elements, !State) :-
    (
        Elements = []
    ;
        Elements = [Element],
        raw_put_value(Writer, Element, !State)
    ;
        Elements = [Element, NextElement | ElementsPrime],
        raw_put_value(Writer, Element, !State),
        Stream = Writer ^ json_writer_stream,
        put(Stream, (','), !State),
        raw_put_elements(Writer, [NextElement | ElementsPrime], !State)
    ).

%-----------------------------------------------------------------------------%

:- pred put_string_literal(json.writer(Stream)::in, string::in,
    State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_string_literal(Writer, String, !State) :-
    Writer = json_writer(Stream, _, _, EscapeSolidus, _),
    put(Stream, '"', !State),
    string.foldl(escape_and_put_char(Stream, EscapeSolidus), String, !State),
    put(Stream, '"', !State).

:- pred escape_and_put_char(Stream::in, escape_solidus::in, char::in,
    State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

escape_and_put_char(Stream, EscapeSolidus, Char, !State) :-
    ( if
        must_escape_char(Char, EscapedCharStr)
    then
        put(Stream, EscapedCharStr, !State)
    else if
        Char = ('/'),
        EscapeSolidus = escape_solidus
    then
        put(Stream, "\\/", !State)
    else if
        char.to_int(Char, CodePoint),
        CodePoint > 0x001F,
        CodePoint =< 0x007F
    then
        put(Stream, Char, !State)
    else
        put_unicode_escape(Stream, Char, !State)
    ).

:- pred must_escape_char(char::in, string::out) is semidet.

must_escape_char('"', "\\\"").
must_escape_char('\\', "\\\\").
must_escape_char('\b', "\\b").
must_escape_char('\f', "\\f").
must_escape_char('\n', "\\n").
must_escape_char('\r', "\\r").
must_escape_char('\t', "\\t").

:- pred put_unicode_escape(Stream::in, char::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_unicode_escape(Stream, Char, !State) :-
    CodePoint = char.to_int(Char),
    ( if CodePoint > 0xFFFF then
        code_point_to_utf16_surrogates(CodePoint, LS, TS),
        put_hex_digits(Stream, LS, !State),
        put_hex_digits(Stream, TS, !State)
    else
        put_hex_digits(Stream, CodePoint, !State)
    ).

:- pred code_point_to_utf16_surrogates(int::in, int::out, int::out) is det.

code_point_to_utf16_surrogates(CodePoint, LS, TS) :-
    AdjustedCodePoint = CodePoint - 0x10000,
    LS = 0xD800 + (AdjustedCodePoint >> 10),
    TS = 0xDC00 + (AdjustedCodePoint /\ 0x3FF).

:- pred put_hex_digits(Stream::in, int::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_hex_digits(Stream, Int, !State) :-
    string_writer.format(Stream, "\\u%04X", [i(Int)], !State).

%-----------------------------------------------------------------------------%

:- pred put_number(json.writer(Stream)::in, float::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_number(Writer, Number, !State) :-
    ( if is_nan(Number) then
        throw(non_finite_number_error("put_number: nan"))
    else if is_inf(Number) then
        AllowInfinities = Writer ^ json_output_infinities,
        (
            AllowInfinities = allow_infinities,
            NumberStr = ( if Number < 0.0 then "-Infinity" else "Infinity" )
        ;
            AllowInfinities = do_not_allow_infinities,
            throw(non_finite_number_error("put_number: inf or -inf"))
        )
    else if
        % Ensure the call to floor_to_int/1 below won't abort in the C#
        % or Java grades.
        ( Number > float(int.max_int)
        ; Number < float(int.min_int)
        )
    then
        NumberStr = string.from_float(Number)
    else
        Int = floor_to_int(Number),
        ( if Number = float(Int) then
            NumberStr = string.from_int(Int)
        else
            NumberStr = string.from_float(Number)
        )
    ),
    Stream = Writer ^ json_writer_stream,
    put(Stream, NumberStr, !State).

%-----------------------------------------------------------------------------%

pretty_put_value(Writer, Value, !State) :-
    do_pretty_put_json(Writer, 0, Value, !State),
    Stream = Writer ^ json_writer_stream,
    put(Stream, "\n", !State).

:- pred do_pretty_put_json(json.writer(Stream)::in, int::in,
    json.value::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

do_pretty_put_json(Writer, N, Value, !State) :-
    (
        Value = null,
        Stream = Writer ^ json_writer_stream,
        put(Stream, "null", !State)
    ;
        Value = bool(Bool),
        Stream = Writer ^ json_writer_stream,
        (
            Bool = no,
            put(Stream, "false", !State)
        ;
            Bool = yes,
            put(Stream, "true", !State)
        )
    ;
        Value = string(String),
        put_string_literal(Writer, String, !State)
    ;
        Value = number(Number),
        put_number(Writer, Number, !State)
    ;
        Value = object(MembersMap),
        Stream = Writer ^ json_writer_stream,
        ( if map.is_empty(MembersMap) then
            put(Stream, "{}", !State)
        else
            put(Stream, "{\n", !State),
            MemberFilter = Writer ^ json_member_filter,
            (
                MemberFilter = no_member_filter,
                MembersList = map.to_assoc_list(MembersMap)
            ;
                MemberFilter = member_filter(FilterPred),
                map.foldr(maybe_filter_member(FilterPred), MembersMap,
                    [], MembersList)
            ),
            pretty_put_members(Writer, N + 2, MembersList, !State),
            indent(Stream, N, !State),
            put(Stream, "}", !State)
        )
    ;
        Value = array(Elements),
        Stream = Writer ^ json_writer_stream,
        (
            Elements = [],
            put(Stream, "[]", !State)
        ;
            Elements = [_ | _],
            put(Stream, "[\n", !State),
            pretty_put_elements(Writer, N + 2, Elements, !State),
            indent(Stream, N, !State),
            put(Stream, "]", !State)
        )
    ).

:- pred pretty_put_members(json.writer(Stream)::in,
    int::in, assoc_list(string, json.value)::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_members(_, _, [], !State).
pretty_put_members(Writer, N, [Member], !State) :-
    pretty_put_member(Writer, N, Member, "\n", !State).
pretty_put_members(Writer, N, [Member | Members @ [_ | _]], !State) :-
    pretty_put_member(Writer, N, Member, ",\n", !State),
    pretty_put_members(Writer, N, Members, !State).

:- pred pretty_put_member(json.writer(Stream)::in,
    int::in, pair(string, json.value)::in, string::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_member(Writer, N, KeyAndValue, Suffix, !State) :-
    KeyAndValue = Key - Value,
    Stream = Writer ^ json_writer_stream,
    indent(Stream, N, !State),
    put_string_literal(Writer, Key, !State),
    put(Stream, " : ", !State),
    do_pretty_put_json(Writer, N, Value, !State),
    put(Stream, Suffix, !State).

:- pred pretty_put_elements(json.writer(Stream)::in,
    int::in, list(json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_elements(Writer, N, Elements, !State) :-
    (
        Elements = []
    ;
        Elements = [Value],
        Stream = Writer ^ json_writer_stream,
        indent(Stream, N, !State),
        do_pretty_put_json(Writer, N, Value, !State),
        put(Stream, "\n", !State)
    ;
        Elements = [Value | Values @ [_ | _]],
        Stream = Writer ^ json_writer_stream,
        indent(Stream, N, !State),
        do_pretty_put_json(Writer, N, Value, !State),
        put(Stream, ",\n", !State),
        pretty_put_elements(Writer, N, Values, !State)
    ).

:- pred indent(Stream::in, int::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

indent(Stream, N, !State) :-
    ( if N > 0 then
        put(Stream, ' ', !State),
        indent(Stream, N - 1, !State)
    else
        true
    ).

%-----------------------------------------------------------------------------%
%
% Comments.
%

put_eol_comment(Stream, String, !State) :-
    Lines = string.split_at_char('\n', String),
    list.foldl(put_eol_comment_line(Stream), Lines, !State).

:- pred put_eol_comment_line(Stream::in, string::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_eol_comment_line(Stream, Line, !State) :-
    string_writer.format(Stream, "//%s\n", [s(Line)], !State).

put_multiline_comment(Stream, String, !State) :-
    put(Stream, "/*", !State),
    put(Stream, String, !State),
    put(Stream, "*/", !State).

%-----------------------------------------------------------------------------%
:- end_module json.writer.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@www.opturion.com>
%
% This module implements writing json.value/0 values to output streams.
%
%-----------------------------------------------------------------------------%

:- module json.writer.
:- interface.

%-----------------------------------------------------------------------------%

:- pred raw_put_json(Stream::in, json.value::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- pred pretty_put_json(Stream::in, json.value::in, State::di, State::uo) is det
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
:- import_module pair.
:- import_module stream.string_writer.

%-----------------------------------------------------------------------------%
%
% Raw output.
%

raw_put_json(Stream, Value, !State) :-
    (
        Value = null,
        put(Stream, "null", !State)
    ;
        Value = bool(Bool),
        (
            Bool = yes,
            put(Stream, "true", !State)
        ;
            Bool = no,
            put(Stream, "false", !State)
        )
    ;
        Value = string(String),
        put_string_literal(Stream, String, !State)
    ;
        Value = number(Number),
        put_number(Stream, Number, !State)
    ;
        Value = object(Members),
        raw_put_object(Stream, Members, !State)
    ;
        Value = array(Elements),
        raw_put_array(Stream, Elements, !State)
    ).

%-----------------------------------------------------------------------------%

:- pred raw_put_object(Stream::in, json.object::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_object(Stream, MembersMap, !State) :-
    put(Stream, '{', !State),
    MembersList = map.to_assoc_list(MembersMap),
    raw_put_members(Stream, MembersList, !State),
    put(Stream, '}', !State).

:- pred raw_put_members(Stream::in, assoc_list(string, json.value)::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_members(_, [], !State).
raw_put_members(Stream, [Member], !State) :-
    raw_put_member(Stream, Member, !State).
raw_put_members(Stream, [Member, NextMember | Members], !State) :-
    raw_put_member(Stream, Member, !State),
    put(Stream, (','), !State),
    raw_put_members(Stream, [NextMember | Members], !State).

:- pred raw_put_member(Stream::in, pair(string, json.value)::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_member(Stream, Member, !State) :-
    Member = Key - Value,
    put_string_literal(Stream, Key, !State),
    put(Stream, ':', !State),
    raw_put_json(Stream, Value, !State).

%-----------------------------------------------------------------------------%

:- pred raw_put_array(Stream::in, json.array::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_array(Stream, Elements, !State) :-
    put(Stream, '[', !State),
    raw_put_elements(Stream, Elements, !State),
    put(Stream, ']', !State).

:- pred raw_put_elements(Stream::in, json.array::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_elements(_, [], !State).
raw_put_elements(Stream, [Element], !State) :-
    raw_put_json(Stream, Element, !State).
raw_put_elements(Stream, [Element, NextElement | Elements], !State) :-
    raw_put_json(Stream, Element, !State),
    put(Stream, (','), !State),
    raw_put_elements(Stream, [NextElement | Elements], !State).

%-----------------------------------------------------------------------------%

:- pred put_string_literal(Stream::in, string::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).
         
put_string_literal(Stream, String, !State) :-
    put(Stream, '"', !State),
    string.foldl(escape_and_put_char(Stream), String, !State),
    put(Stream, '"', !State).

:- pred escape_and_put_char(Stream::in, char::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).
    
escape_and_put_char(Stream, Char, !State) :-
    ( if escape_char(Char, EscapedCharStr) then
        put(Stream, EscapedCharStr, !State)
    else if char_is_ascii(Char) then
        put(Stream, Char, !State)
    else
        put_unicode_escape(Stream, Char, !State)
    ).

:- pred escape_char(char::in, string::out) is semidet.

escape_char('"', "\\\"").
escape_char('\\', "\\\\").
escape_char('/', "\\/").
escape_char('\b', "\\b").
escape_char('\f', "\\f").
escape_char('\n', "\\n").
escape_char('\r', "\\r").
escape_char('\t', "\\t").

:- pred char_is_ascii(char::in) is semidet.

char_is_ascii(Char) :-
    Code = char.to_int(Char),
    Code >= 0x00,
    Code =< 0x7f.

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
    string_writer.format(Stream, "\\u%04x", [i(Int)], !State).    

%-----------------------------------------------------------------------------%

:- pred put_number(Stream::in, float::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_number(Stream, Number, !State) :-
    Int = floor_to_int(Number),
    ( if Number = float(Int)
    then NumberStr = string.from_int(Int)
    else NumberStr = string.from_float(Number)
    ),
    put(Stream, NumberStr, !State).

%-----------------------------------------------------------------------------%

pretty_put_json(Stream, Value, !State) :-
    do_pretty_put_json(Stream, 0, Value, !State),
    put(Stream, "\n", !State).

:- pred do_pretty_put_json(Stream::in, int::in, json.value::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

do_pretty_put_json(Stream, N, Value, !State) :-
    (
        Value = null,
        put(Stream, "null", !State)
    ;
        Value = bool(Bool),
        (
            Bool = no,
            put(Stream, "false", !State)
        ;
            Bool = yes,
            put(Stream, "true", !State)
        )
    ;
        Value = string(String),
        put_string_literal(Stream, String, !State)
    ;
        Value = number(Number),
        put_number(Stream, Number, !State)
    ;
        Value = object(MembersMap),
        put(Stream, "{\n", !State),
        MembersList = map.to_assoc_list(MembersMap),
        pretty_put_members(Stream, N + 2, MembersList, !State),
        indent(Stream, N, !State),
        put(Stream, "}", !State)
    ;
        Value = array(Elements),
        (
            Elements = [],
            put(Stream, "[]", !State)
        ;
            Elements = [_ | _],
            put(Stream, "[\n", !State),
            pretty_put_elements(Stream, N + 2, Elements, !State),
            indent(Stream, N, !State),
            put(Stream, "]", !State)
        )
    ).

:- pred pretty_put_members(Stream::in, int::in,
    assoc_list(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_members(_, _, [], !State).
pretty_put_members(Stream, N, [Member], !State) :-
    pretty_put_member(Stream, N, Member, "\n", !State).
pretty_put_members(Stream, N, [Member | Members @ [_ | _]], !State) :-
    pretty_put_member(Stream, N, Member, ",\n", !State),
    pretty_put_members(Stream, N, Members, !State).
    
:- pred pretty_put_member(Stream::in, int::in,
    pair(string, json.value)::in, string::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_member(Stream, N, KeyAndValue, Suffix, !State) :-
    KeyAndValue = Key - Value,
    indent(Stream, N, !State),
    put_string_literal(Stream, Key, !State),
    put(Stream, " : ", !State),
    do_pretty_put_json(Stream, N, Value, !State),
    put(Stream, Suffix, !State).

:- pred pretty_put_elements(Stream::in, int::in,
    list(json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_elements(_, _, [], !State).
pretty_put_elements(Stream, N, [Value], !State) :-
    indent(Stream, N, !State),
    do_pretty_put_json(Stream, N, Value, !State),
    put(Stream, "\n", !State).
pretty_put_elements(Stream, N, [Value | Values @ [_ | _]], !State) :-
    indent(Stream, N, !State),
    do_pretty_put_json(Stream, N, Value, !State),
    put(Stream, ",\n", !State),
    pretty_put_elements(Stream, N, Values, !State).
    
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

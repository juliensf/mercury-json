%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2015, Julien Fischer.
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

:- pred raw_put_value(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), json.value::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

:- pred pretty_put_value(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), json.value::in,
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

raw_put_value(Stream, AllowInfinities, MemberFilter, Value, !State) :-
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
        put_number(Stream, AllowInfinities, Number, !State)
    ;
        Value = object(Members),
        raw_put_object(Stream, AllowInfinities, MemberFilter, Members, !State)
    ;
        Value = array(Elements),
        raw_put_array(Stream, AllowInfinities, MemberFilter, Elements, !State)
    ).

%-----------------------------------------------------------------------------%

:- pred raw_put_object(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), json.object::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_object(Stream, AllowInfinities, MemberFilter, MembersMap, !State) :-
    put(Stream, '{', !State),
    (
        MemberFilter = no_member_filter,
        MembersList = map.to_assoc_list(MembersMap)
    ;
        MemberFilter = member_filter(FilterPred),
        map.foldr(maybe_filter_member(FilterPred), MembersMap, [], MembersList)
    ),
    raw_put_members(Stream, AllowInfinities, MemberFilter, MembersList, !State),
    put(Stream, '}', !State).

:- pred maybe_filter_member(pred(string, value)::in(pred(in, in) is semidet),
    string::in, value::in,
    assoc_list(string, json.value)::in, assoc_list(string, json.value)::out) is det.

maybe_filter_member(FilterPred, Name, Value, !MembersList) :-
    ( if FilterPred(Name, Value)
    then true
    else !:MembersList = [Name - Value | !.MembersList]
    ).

:- pred raw_put_members(Stream::in, allow_infinities::in,
    member_filter::in(member_filter),
    assoc_list(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_members(_, _, _, [], !State).
raw_put_members(Stream, AllowInfinities, MemberFilter, [Member], !State) :-
    raw_put_member(Stream, AllowInfinities, MemberFilter, Member, !State).
raw_put_members(Stream, AllowInfinities, MemberFilter,
        [Member, NextMember | Members], !State) :-
    raw_put_member(Stream, AllowInfinities, MemberFilter, Member, !State),
    put(Stream, (','), !State),
    raw_put_members(Stream, AllowInfinities, MemberFilter,
        [NextMember | Members], !State).

:- pred raw_put_member(Stream::in, allow_infinities::in,
    member_filter::in(member_filter),
    pair(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_member(Stream, AllowInfinities, MemberFilter, Member, !State) :-
    Member = Key - Value,
    put_string_literal(Stream, Key, !State),
    put(Stream, ':', !State),
    raw_put_value(Stream, AllowInfinities, MemberFilter, Value, !State).

%-----------------------------------------------------------------------------%

:- pred raw_put_array(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), json.array::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_array(Stream, AllowInfinities, MemberFilter, Elements, !State) :-
    put(Stream, '[', !State),
    raw_put_elements(Stream, AllowInfinities, MemberFilter, Elements, !State),
    put(Stream, ']', !State).

:- pred raw_put_elements(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), json.array::in,
    State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

raw_put_elements(Stream, AllowInfinities, MemberFilter, Elements, !State) :-
    (
        Elements = []
    ;
        Elements = [Element],
        raw_put_value(Stream, AllowInfinities, MemberFilter, Element, !State)
    ;
        Elements = [Element, NextElement | ElementsPrime],
        raw_put_value(Stream, AllowInfinities, MemberFilter, Element, !State),
        put(Stream, (','), !State),
        raw_put_elements(Stream, AllowInfinities, MemberFilter,
            [NextElement | ElementsPrime], !State)
    ).

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
    ( if
        escape_char(Char, EscapedCharStr)
    then
        put(Stream, EscapedCharStr, !State)
    else if
        char.to_int(Char, CodePoint),
        CodePoint > 0x001F,
        CodePoint =< 0x007F
    then
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

:- pred put_number(Stream::in, allow_infinities::in, float::in,
    State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

put_number(Stream, AllowInfinities, Number, !State) :-
    ( if is_nan(Number) then
        throw(non_finite_number_error("put_number: nan"))
    else if is_inf(Number) then
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
        ( if Number = float(Int)
        then NumberStr = string.from_int(Int)
        else NumberStr = string.from_float(Number)
        )
    ),
    put(Stream, NumberStr, !State).

%-----------------------------------------------------------------------------%

pretty_put_value(Stream, AllowInfinities, MemberFilter, Value, !State) :-
    do_pretty_put_json(Stream, AllowInfinities, MemberFilter, 0, Value, !State),
    put(Stream, "\n", !State).

:- pred do_pretty_put_json(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), int::in,
    json.value::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

do_pretty_put_json(Stream, AllowInfinities, MemberFilter, N, Value, !State) :-
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
        put_number(Stream, AllowInfinities, Number, !State)
    ;
        Value = object(MembersMap),
        ( if map.is_empty(MembersMap) then
            put(Stream, "{}", !State)
        else
            put(Stream, "{\n", !State),
            (
                MemberFilter = no_member_filter,
                MembersList = map.to_assoc_list(MembersMap)
            ;
                MemberFilter = member_filter(FilterPred),
                map.foldr(maybe_filter_member(FilterPred), MembersMap,
                    [], MembersList)
            ),
            pretty_put_members(Stream, AllowInfinities, MemberFilter,
                N + 2, MembersList, !State),
            indent(Stream, N, !State),
            put(Stream, "}", !State)
        )
    ;
        Value = array(Elements),
        (
            Elements = [],
            put(Stream, "[]", !State)
        ;
            Elements = [_ | _],
            put(Stream, "[\n", !State),
            pretty_put_elements(Stream, AllowInfinities, MemberFilter,
                N + 2, Elements, !State),
            indent(Stream, N, !State),
            put(Stream, "]", !State)
        )
    ).

:- pred pretty_put_members(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), int::in,
    assoc_list(string, json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_members(_, _, _, _, [], !State).
pretty_put_members(Stream, AllowInfinities, MemberFilter, N, [Member],
        !State) :-
    pretty_put_member(Stream, AllowInfinities, MemberFilter, N, Member,
        "\n", !State).
pretty_put_members(Stream, AllowInfinities, MemberFilter, N,
        [Member | Members @ [_ | _]], !State) :-
    pretty_put_member(Stream, AllowInfinities, MemberFilter, N, Member,
        ",\n", !State),
    pretty_put_members(Stream, AllowInfinities, MemberFilter, N, Members,
        !State).

:- pred pretty_put_member(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), int::in,
    pair(string, json.value)::in, string::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_member(Stream, AllowInfinities, MemberFilter, N, KeyAndValue,
        Suffix, !State) :-
    KeyAndValue = Key - Value,
    indent(Stream, N, !State),
    put_string_literal(Stream, Key, !State),
    put(Stream, " : ", !State),
    do_pretty_put_json(Stream, AllowInfinities, MemberFilter, N, Value,
        !State),
    put(Stream, Suffix, !State).

:- pred pretty_put_elements(Stream::in, allow_infinities::in,
    member_filter::in(member_filter), int::in,
    list(json.value)::in, State::di, State::uo) is det
    <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).

pretty_put_elements(Stream, AllowInfinities, MemberFilter, N,
        Elements, !State) :-
    (
        Elements = []
    ;
        Elements = [Value],
        indent(Stream, N, !State),
        do_pretty_put_json(Stream, AllowInfinities, MemberFilter, N, Value,
            !State),
        put(Stream, "\n", !State)
    ;
        Elements = [Value | Values @ [_ | _]],
        indent(Stream, N, !State),
        do_pretty_put_json(Stream, AllowInfinities, MemberFilter, N, Value,
            !State),
        put(Stream, ",\n", !State),
        pretty_put_elements(Stream, AllowInfinities, MemberFilter, N, Values,
            !State)
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

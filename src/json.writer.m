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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module pair.

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
        % XXX if the number is actually an int, then we should
        % print it as an int.
        NumberStr = string.from_float(Number),
        put(Stream, NumberStr, !State)
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

    % XXX TODO: handle Unicode escapes properly here.
    %
:- pred escape_and_put_char(Stream::in, char::in, State::di, State::uo)
    is det <= (
        stream.writer(Stream, char, State),
        stream.writer(Stream, string, State)
    ).
    
escape_and_put_char(Stream, Char, !State) :-
    ( if escape_char(Char, EscapedCharStr)
    then put(Stream, EscapedCharStr, !State)
    else put(Stream, Char, !State)
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

%-----------------------------------------------------------------------------%
:- end_module json.writer.
%-----------------------------------------------------------------------------%

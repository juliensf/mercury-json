%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%
%
% This submodule implements the conversion of reader and from_json errors into
% human-readable error messages.
%
%-----------------------------------------------------------------------------%

:- module json.error_msg.
:- interface.

%-----------------------------------------------------------------------------%

:- func do_error_context_and_reader_desc_to_string(json.context,
    json.reader_error_desc) = string.

:- func do_from_json_error_to_string(from_json_error) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

do_error_context_and_reader_desc_to_string(Context, ErrorDesc) = Msg :-
    Context = context(StreamName, LineNo, ColNo),
    string.format("%s:%d:%d", [s(StreamName), i(LineNo), i(ColNo)],
        ContextStr),
    (
        ErrorDesc = unexpected_eof(ExtraMsg),
        string.format("%s: error: unexpected end-of-file: %s\n",
            [s(ContextStr), s(ExtraMsg)], Msg)
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
        ErrorDesc = unexpected_value(What, ExtraMsg),
        string.format("%s: error: unexpected %s value: %s\n",
            [s(ContextStr), s(What), s(ExtraMsg)], Msg)
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
        ErrorDesc = unpaired_leading_utf16_surrogate(What),
        string.format("%s: error: unpaired leading UTF-16 surrogate: \\u%s\n",
            [s(ContextStr), s(What)], Msg)
    ;
        ErrorDesc = unpaired_trailing_utf16_surrogate(What),
        string.format("%s: error: unpaired trailing UTF-16 surrogate: \\u%s\n",
            [s(ContextStr), s(What)], Msg)
    ;
        ErrorDesc = invalid_trailing_utf16_surrogate(What),
        string.format("%s: error: invalid trailing UTF-16 surrogate: \\u%s\n",
            [s(ContextStr), s(What)], Msg)
    ;
        ErrorDesc = null_character,
        string.format("%s: error: NULL character encountered\n",
            [s(ContextStr)], Msg)
    ;
        ErrorDesc = string_contains_unescaped_control_character(_StartContext,
            CodePoint),
        ( if to_char_name(CodePoint, CharName0) then
            CharName = " (" ++ CharName0 ++ ")"
        else
            CharName = ""
        ),
        string.format(
            "%s: error: string contains unescaped control character: U+%04X%s\n",
            [s(ContextStr), i(CodePoint), s(CharName)], Msg)
    ;
        ErrorDesc = illegal_start_character(Char),
        string.format("%s: error: '%c' at start of JSON value\n",
            [s(ContextStr), c(Char)], Msg)
    ;
        ErrorDesc = illegal_unicode_escape_character(Char),
        string.format("%s: error: character" ++
            " in Unicode escape is not a hexadecimal digit: %s\n",
            [s(ContextStr), s(describe_char(Char))], Msg)
    ;
        ErrorDesc = non_finite_number(NumberStr),
        string.format("%s: error: non-finite number: %s\n",
            [s(ContextStr), s(NumberStr)], Msg)

    ;   ErrorDesc = illegal_negation(Char),
        string.format(
            "%s: error: expected a decimal digit after '-', got %s\n",
            [s(ContextStr), s(describe_char(Char))], Msg)
    ;
        ErrorDesc = illegal_comment_start(Char),
        string.format(
            "%s: error: expected '/' or '*' after '/' in" ++
            " comment start, got %s\n",
            [s(ContextStr), s(describe_char(Char))], Msg)
    ;
        ErrorDesc = bad_signed_exponent(SignChar, Char),
        string.format(
            "%s: error: expected a decimal digit " ++
            "after '%c' in exponent, got %s\n",
            [s(ContextStr), c(SignChar), s(describe_char(Char))], Msg)
    ;
        ErrorDesc = bad_exponent(ExpChar, Char),
        string.format(
            "%s: error: expected '+', '-' or a decimal digit after " ++
            "'%c' in exponent, got %s\n",
            [s(ContextStr), c(ExpChar), s(describe_char(Char))], Msg)
    ;
        ErrorDesc = expected_eof(Found),
        string.format(
            "%s: error: expected end-of-file, got '%s'\n",
            [s(ContextStr), s(Found)], Msg)
    ;
        ErrorDesc = maximum_nesting_depth_reached,
        string.format(
            "%s: error: maximum nesting depth limit reached\n",
            [s(ContextStr)], Msg)
    ).

:- func describe_char(char) = string.

describe_char(Char) = String :-
    ( if describe_escaped_char(Char, EscapedCharString) then
        String = EscapedCharString
    else if
        char.to_int(Char, CodePoint),
        CodePoint >= 0x0000, CodePoint =< 0x001F
    then
        string.format("U+%04X", [i(CodePoint)], String)
    else
        string.format("'%s'", [s(string.from_char(Char))], String)
    ).

:- pred describe_escaped_char(char::in, string::out) is semidet.

describe_escaped_char('\n', "'\\n'").
describe_escaped_char('\t', "'\\t'").
describe_escaped_char('\r', "'\\r'").

:- pred to_char_name(int::in, string::out) is semidet.

to_char_name(0x0000, "NULL").
to_char_name(0x0001, "START OF HEADING").
to_char_name(0x0002, "START OF TEXT").
to_char_name(0x0003, "END OF TEXT").
to_char_name(0x0004, "END OF TRANSMISSION").
to_char_name(0x0005, "ENQUIRY").
to_char_name(0x0006, "ACKNOWLEDGE").
to_char_name(0x0007, "BELL").
to_char_name(0x0008, "BACKSPACE").
to_char_name(0x0009, "CHARACTER TABULATION").
to_char_name(0x000A, "LINE FEED").
to_char_name(0x000B, "LINE TABULATION").
to_char_name(0x000C, "FORM FEED").
to_char_name(0x000D, "CARRIAGE RETURN").
to_char_name(0x000E, "SHIFT OUT").
to_char_name(0x000F, "SHIFT IN").
to_char_name(0x0010, "DATA LINK ESCAPE").
to_char_name(0x0011, "DEVICE CONTROL ONE").
to_char_name(0x0012, "DEVICE CONTROL TWO").
to_char_name(0x0013, "DEVICE CONTROL THREE").
to_char_name(0x0014, "DEVICE CONTROL FOUR").
to_char_name(0x0015, "NEGATIVE ACKNOWLEDGE").
to_char_name(0x0016, "SYNCHRONOUS IDLE").
to_char_name(0x0017, "END OF TRANSMISSION BLOCK").
to_char_name(0x0018, "CANCEL").
to_char_name(0x0019, "END OF MEDIUM").
to_char_name(0x001A, "SUBSTITUTE").
to_char_name(0x001B, "ESCAPE").
to_char_name(0x001C, "INFORMATION SEPARATOR FOUR").
to_char_name(0x001D, "INFORMATION SEPARATOR THREE").
to_char_name(0x001E, "INFORMATION SEPARATOR TWO").
to_char_name(0x001F, "INFORMATION SEPARATOR ONE").

%-----------------------------------------------------------------------------%

do_from_json_error_to_string(Error) = String :-
    Error = from_json_error(Pointer, TypeDesc, ErrorDesc),
    PointerDesc = describe_pointer(Pointer),
    TypeName = type_name(TypeDesc),
    (
        ErrorDesc = value_type_mismatch(ExpectedDescs, Have),
        ExpectedDescs = one_or_more(FirstExpected, OtherExpected),
        (
            OtherExpected = [],
            string.format(
                "at %s: conversion to %s failed: expected %s, have %s",
                [s(PointerDesc), s(TypeName), s(FirstExpected), s(Have)],
                String)
        ;
            OtherExpected = [_ | _],
            list.det_split_last(OtherExpected, RestExpected, LastExpected),
            ExpectedStart = string.join_list(", ", [FirstExpected | RestExpected]),
            string.format(
                "at %s: conversion to %s failed: expected %s or %s, have %s",
                [s(PointerDesc), s(TypeName), s(ExpectedStart),
                 s(LastExpected), s(Have)], String)
        )
    ;
        ErrorDesc = from_string_failed(FailString),
        string.format(
            "at %s: conversion from string to %s failed: have \"%s\"",
            [s(PointerDesc), s(TypeName), s(FailString)], String)
    ;
        ErrorDesc = missing_members(MissingMemberNames),
        MissingMemberNames = one_or_more(FirstMissing, RestMissing),
        (
            RestMissing = [],
            string.format(
                "at %s: conversion to %s failed: object has no member named \"%s\"",
                [s(PointerDesc), s(TypeName), s(FirstMissing)], String)
        ;
            RestMissing = [_ | _],
            QuotedRestMissing = list.map(add_quotes, RestMissing),
            list.det_split_last(QuotedRestMissing, InitialRestMissing,
                LastMissing),
            MissingStart = string.join_list(", ",
                [add_quotes(FirstMissing) | InitialRestMissing]),
            string.format(
"at %s: conversion to %s failed: object has no members named %s and %s",
                [s(PointerDesc), s(TypeName), s(MissingStart), s(LastMissing)],
                String)
        )
    ;
        ErrorDesc = conflicting_members(Member1, Member2),
        string.format(
"at %s: conversion to %s failed: object cannot have both \"%s\" and  \"%s\" members",
            [s(PointerDesc), s(TypeName), s(Member1), s(Member2)], String)
    ;
        ErrorDesc = out_of_bounds_number,
        string.format(
            "at %s: conversion to %s failed: number is out-of-bounds for type",
            [s(PointerDesc), s(TypeName)], String)
    ;
        ErrorDesc = non_finite_number,
        string.format(
            "at %s: conversion to %s failed: non-finite number",
            [s(PointerDesc), s(TypeName)], String)
    ;
        ErrorDesc = other(ErrorMsg),
        string.format(
            "at %s: conversion to %s failed: %s",
            [s(PointerDesc), s(TypeName), s(ErrorMsg)], String)
    ).

:- func add_quotes(string) = string.

add_quotes(S) = string.format("\"%s\"", [s(S)]).

%-----------------------------------------------------------------------------%
:- end_module json.error_msg.
%-----------------------------------------------------------------------------%

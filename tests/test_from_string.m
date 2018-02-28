%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2018 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test reading JSON from strings.
%
%-----------------------------------------------------------------------------%

:- module test_from_string.
:- interface.

:- import_module io.

:- pred test_from_string(io.text_output_stream::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

test_from_string(File, !IO) :-
    list.foldl(do_from_string_test(File), valid_test_strings, !IO),
    list.foldl(do_from_string_test(File), invalid_test_strings, !IO).

:- pred do_from_string_test(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

do_from_string_test(File, String, !IO) :-
    io.format(File, " Input: \"%s\"\n", [s(String)], !IO),
    Result = maybe_from_string(String),
    io.format(File, "Result: %s\n", [s(string(Result))], !IO).

%-----------------------------------------------------------------------------%

:- func valid_test_strings = list(string).

valid_test_strings = [
   "null",
   "false",
   "true",
   "0",
   "1.5",
   "[]",
   "{}",
   "\"\"",
   "\"foo\"",
   "[\"foo\", \"bar\"]",
   "{\"foo\": 561}",
   "\"a𢭃b\""
].

:- func invalid_test_strings = list(string).

invalid_test_strings = [
    "",
    "\"",
    "[",
    "}"
].

%-----------------------------------------------------------------------------%
:- end_module test_from_string.
%-----------------------------------------------------------------------------%

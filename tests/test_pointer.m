%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015-2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test JSON pointer operations.
%
%-----------------------------------------------------------------------------%

:- module test_pointer.
:- interface.

:- import_module io.

:- pred test_pointer(io.text_output_stream::in, io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_pointer(File, !IO) :-
    io.write_string(File, "Testing JSON pointer resolution using resolve/3:\n", !IO),
    list.foldl(do_resolve_test(File, rfc6901_example), test_pointers, !IO),
    io.nl(File, !IO),
    io.write_string(File, "Testing JSON pointer resolution using det_resolve/2:\n", !IO),
    list.foldl(do_det_resolve_test(File, rfc6901_example), test_pointers, !IO).

%-----------------------------------------------------------------------------%

:- pred do_resolve_test(io.text_output_stream::in, json.value::in,
    string::in, io::di, io::uo) is det.

do_resolve_test(File, Document, PointerStr, !IO) :-
    io.format(File, "TEST POINTER: \"%s\"\n", [s(PointerStr)], !IO),
    ( if string_to_pointer(PointerStr, Pointer) then
        ( if json.resolve(Pointer, Document, Value) then
            io.format(File, "RESULT: %s\n", [s(to_string(Value))], !IO)
        else
            io.write_string(File, "RESULT: <<cannot resolve pointer>>\n", !IO)
        )
    else
        io.write_string(File, "RESULT: invalid pointer\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred do_det_resolve_test(io.text_output_stream::in, json.value::in,
    string::in, io::di, io::uo) is cc_multi.

do_det_resolve_test(File, Document, PointerStr, !IO) :-
    io.format(File, "TEST POINTER: \"%s\"\n", [s(PointerStr)], !IO),
    ( if string_to_pointer(PointerStr, Pointer) then
        ( try [] (
            Value = json.det_resolve(Pointer, Document)
        ) then
            io.format(File, "RESULT: %s\n", [s(to_string(Value))], !IO)
        catch_any Excp ->
            io.format(File, "RESULT: EXCP (%s)\n", [s(string(Excp))], !IO)
        )
    else
        io.write_string(File, "RESULT: invalid pointer\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- func rfc6901_example = json.value.

rfc6901_example = json.det_make_object([
    "foo"  - array([string("bar"), string("baz")]),
    ""     - int(0),
    "a/b"  - int(1),
    "c%d"  - int(2),
    "e^f"  - int(3),
    "g|h"  - int(4),
    "i\\j" - int(5),
    "k\"l" - int(6),
    " "    - int(7),
    "m~n"  - int(8)
]).

%-----------------------------------------------------------------------------%

:- func test_pointers = list(string).

test_pointers = [
    "",
    "/foo",
    "/foo/0",
    "/",
    "/a~1b",
    "/c%d",
    "/e^f",
    "/g|h",
    "/i\\j",
    "/k\"l",
    "/ ",
    "/m~0n",
    "/foo/-",
    "/foo/10",
    "/bar",
    "/foo/bar"
].

%-----------------------------------------------------------------------------%
:- end_module test_pointer.
%-----------------------------------------------------------------------------%

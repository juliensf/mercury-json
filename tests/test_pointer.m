%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test JSON pointer operations.
%
%-----------------------------------------------------------------------------%

:- module test_pointer.
:- interface.

:- import_module io.

:- pred test_pointer(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_pointer(File, !IO) :-
    io.write_string(File, "Testing JSON pointer resolution:\n", !IO),
    TestPointers = [
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
    ],
    list.foldl(do_resolve_test(File, rfc6901_example), TestPointers, !IO).

%-----------------------------------------------------------------------------%

:- pred do_resolve_test(io.text_output_stream::in, json.value::in,
    json.pointer::in, io::di, io::uo) is det.

do_resolve_test(File, Document, Pointer, !IO) :-
    io.format(File, "TEST POINTER: \"%s\"\n", [s(Pointer)], !IO),
    Result = json.resolve(Pointer, Document),
    (
        Result = ok(Value),
        io.format(File, "RESULT: %s\n", [s(to_string(Value))], !IO)
    ;
        Result = cannot_resolve_pointer,
        io.write_string(File, "RESULT: <<cannot resolve pointer>>\n", !IO)
    ;
        Result = error(ErrorDesc),
        io.format(File, "RESULT: <<error: %s>>\n", [s(string(ErrorDesc))], !IO)
    ).

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
:- end_module test_pointer.
%-----------------------------------------------------------------------------%


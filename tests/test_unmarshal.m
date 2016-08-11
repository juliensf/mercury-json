%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Furthers test of JSON->Mercury conversion.
%
%-----------------------------------------------------------------------------%

:- module test_unmarshal.
:- interface.

:- import_module io.

:- pred test_unmarshaling(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module array.
:- import_module array2d.
:- import_module bitmap.
:- import_module bool.
:- import_module digraph.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

test_unmarshaling(File, !IO) :-
    test_unmarshal_ints(File, !IO),
    test_unmarshal_floats(File, !IO),
    test_unmarshal_chars(File, !IO),
    test_unmarshal_strings(File, !IO),
    test_unmarshal_bools(File, !IO),
    test_unmarshal_integers(File, !IO),
    test_unmarshal_bitmaps(File, !IO),
    test_unmarshal_lists(File, !IO),
    test_unmarshal_arrays(File, !IO),
    test_unmarshal_array2ds(File, !IO),
    test_unmarshal_digraphs(File, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_ints(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_ints(File, !IO) :-
    do_unmarshal_test(File, null, _ : int, !IO),
    do_unmarshal_test(File, string("Hello"), _ : int, !IO).

:- pred test_unmarshal_floats(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_floats(File, !IO) :-
    % XXX we should have a test for non-finite floats.
    do_unmarshal_test(File, null, _ : float, !IO),
    do_unmarshal_test(File, string("5.61"), _ : float, !IO).

:- pred test_unmarshal_chars(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_chars(File, !IO) :-
    do_unmarshal_test(File, null, _ : character, !IO),
    do_unmarshal_test(File, string("AA"), _ : character, !IO),
    do_unmarshal_test(File, string(""), _ : character, !IO).

:- pred test_unmarshal_strings(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_strings(File, !IO) :-
    do_unmarshal_test(File, null, _ : string, !IO),
    do_unmarshal_test(File, number(5.61), _ : string, !IO),
    do_unmarshal_test(File, array([]), _ : string, !IO),
    do_unmarshal_test(File, string(""), _ : string, !IO),
    do_unmarshal_test(File, string("Hello"), _ : string, !IO).

:- pred test_unmarshal_bools(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_bools(File, !IO) :-
    do_unmarshal_test(File, null, _ : bool, !IO),
    do_unmarshal_test(File, string("Hello"), _ : bool, !IO),
    do_unmarshal_test(File, number(5.61),  _ : bool, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_integers(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_integers(File, !IO) :-
    do_unmarshal_test(File, null, _ : integer, !IO),
    do_unmarshal_test(File, string("Hello"), _ : integer, !IO),
    do_unmarshal_test(File, string("12345678912345678912346"), _ : integer,
        !IO),
    do_unmarshal_test(File, string("-123456789123456789123456"), _ : integer,
        !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_bitmaps(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_bitmaps(File, !IO) :-
    do_unmarshal_test(File, null, _ : bitmap, !IO),
    do_unmarshal_test(File, string(""), _ : bitmap, !IO),
    do_unmarshal_test(File, string("---"), _ : bitmap, !IO),
    do_unmarshal_test(File, string("<24:10AFBD>"), _ : bitmap, !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_lists(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_lists(File, !IO) :-
    do_unmarshal_test(File, null, _ : list(int), !IO),
    do_unmarshal_test(File, array([]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(1)]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(1), int(2)]), _ : list(int), !IO),
    do_unmarshal_test(File, array([int(10), null]), _ : list(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_arrays(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_arrays(File, !IO) :-

    % Test non-array value.
    %
    do_unmarshal_test(File, null, _ : array(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_array2ds(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_array2ds(File, !IO) :-

    % Test non-array value.
    %
    do_unmarshal_test(File,
        null, _ : array2d(int), !IO),

    % Test no rows.
    %
    do_unmarshal_test(File,
        array([]), _ : array2d(int), !IO),

    % Test two empty rows.
    %
    do_unmarshal_test(File,
        array([array([]), array([])]), _ : array2d(int), !IO),

    % Test first row empty, second row not an array.
    %
    do_unmarshal_test(File,
        array([array([]), null]), _ : array2d(int), !IO),

    % Test first row empty, second row not empty.
    %
    do_unmarshal_test(File,
        array([array([]), array([int(3)])]), _ : array2d(int), !IO),

    % Test first row not an array.
    %
    do_unmarshal_test(File,
        array([bool(no), array([int(3)])]), _ : array2d(int), !IO),

    % Test 3 x 3 array2d.
    %
    do_unmarshal_test(File,
        array([
            array([int(1), int(2), int(3)]),
            array([int(4), int(5), int(6)]),
            array([int(7), int(8), int(9)])
        ]), _ : array2d(int), !IO),

    % Test first and second rows have different (non-zero) lengths.
    %
    do_unmarshal_test(File,
        array([
            array([int(1)]),
            array([int(2), int(3)])
        ]), _ : array2d(int), !IO).

%-----------------------------------------------------------------------------%

:- pred test_unmarshal_digraphs(io.text_output_stream::in, io::di, io::uo)
    is det.

test_unmarshal_digraphs(File, !IO) :-

    % Test empty graph.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([]),
            "edges"    - array([])
        ]), _ : digraph(string), !IO),

    % Edge contains src not in vertex set.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")]),
            "edges"    - array([
                            det_make_object([
                                "source" - string("C"),
                                "dest"   - string("A")
                            ])
                        ])
        ]), _ : digraph(string), !IO),

    % Edge contains dst not in vertex set.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")]),
            "edges"    - array([
                            det_make_object([
                                "source" - string("A"),
                                "dest"   - string("C")
                            ])
                        ])
        ]), _ : digraph(string), !IO),

    % Missing 'edges' member.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([string("A"), string("B")])
        ]), _ : digraph(string), !IO),

    % Missing 'vertices' member.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "edges"    - array([
                            det_make_object([
                                "source" - string("A"),
                                "dest"   - string("C")
                            ])
                        ])
            ]), _ : digraph(string), !IO),

    % Value is not an object.
    %
    do_unmarshal_test(File,
        null, _ : digraph(string), !IO),

    % Vertex value has wrong type.
    %
    do_unmarshal_test(File,
        json.det_make_object([
            "vertices" - array([null, string("B")]),
            "edges"    - array([])
        ]), _ : digraph(string), !IO).

%-----------------------------------------------------------------------------%

:- pred do_unmarshal_test(io.text_output_stream::in, json.value::in,
    T::unused, io::di, io::uo) is det <= from_json(T).

do_unmarshal_test(File, Value, Type, !IO) :-
    io.write_string(File, "JSON: ", !IO),
    json.write_compact(File, Value, !IO),
    io.nl(File, !IO),
    io.write_string(File, "TYPE: ", !IO),
    TypeDesc = type_of(Type),
    io.print(File, TypeDesc, !IO),
    io.nl(File, !IO),
    Result = json.from_json(Value),
    (
        Result = ok(Term : T),
        io.write_string(File, "RESULT: OK: ", !IO),
        io.print(File, Term, !IO),
        io.nl(File, !IO)
    ;
        Result = error(Msg),
        io.write_string(File, "RESULT: ERROR: ", !IO),
        io.write_string(File, Msg, !IO),
        io.nl(File, !IO)
    ),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%
:- end_module test_unmarshal.
%-----------------------------------------------------------------------------%

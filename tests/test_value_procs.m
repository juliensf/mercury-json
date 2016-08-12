%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test procedures for manipulating JSON values.
%
%-----------------------------------------------------------------------------%

:- module test_value_procs.
:- interface.

:- import_module io.

:- pred test_value_procs(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_value_procs(File, !IO) :-
    list.foldl(run_test_value_test(File, "is_null", is_null), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_value_test(File, "is_bool", is_bool), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_value_test(File, "is_string", is_string), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_value_test(File, "is_number", is_number), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_value_test(File, "is_array", is_array), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_value_test(File, "is_object", is_object), test_values,
        !IO).

%-----------------------------------------------------------------------------%

:- pred run_test_value_test(io.text_output_stream::in,
    string::in, pred(value)::in(pred(in) is semidet),
    json.value::in, io::di, io::uo) is det.

run_test_value_test(File, PredName, Pred, Value, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s) ===> ", [s(PredName), s(ValueStr)], !IO),
    ( if Pred(Value)
    then io.write_string(File, "TRUE\n", !IO)
    else io.write_string(File, "FALSE\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- func test_values = list(json.value).

test_values = [
    null,
    bool(no),
    bool(yes),
    string(""),
    string("foo"),
    number(-1.34),
    number(-1.0),
    number(0.0),
    number(1.0),
    number(1.34),
    array([]),
    array([null, bool(yes), bool(no), number(1.0)]),
    det_make_object([]),
    det_make_object(["foo" - bool(yes)]),
    det_make_object(["foo" - bool(yes), "bar" - bool(no)])
].

%-----------------------------------------------------------------------------%
:- end_module test_value_procs.
%-----------------------------------------------------------------------------%

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

:- pred test_value_procs(io.text_output_stream::in, io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_value_procs(File, !IO) :-
    list.foldl(run_test_is_value(File, "is_null", is_null), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_is_value(File, "is_bool", is_bool), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_is_value(File, "is_string", is_string), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_is_value(File, "is_number", is_number), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_is_value(File, "is_array", is_array), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_is_value(File, "is_object", is_object), test_values,
        !IO),
    io.nl(File, !IO),

    list.foldl(run_test_get_value(File, "get_bool", get_bool), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_get_value(File, "get_string", get_string), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_get_value(File, "get_number", get_number), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_get_value(File, "get_int", get_int), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_get_value(File, "get_array", get_array), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_get_value(File, "get_object", get_object), test_values,
        !IO),
    io.nl(File, !IO),

    list.foldl(run_test_det_get_value(File, "det_get_bool", det_get_bool), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_det_get_value(File, "det_get_string", det_get_string), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_det_get_value(File, "det_get_number", det_get_number), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_det_get_value(File, "det_get_int", det_get_int), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_det_get_value(File, "det_get_array", det_get_array), test_values,
        !IO),
    io.nl(File, !IO),
    list.foldl(run_test_det_get_value(File, "det_get_object", det_get_object), test_values,
        !IO).

%-----------------------------------------------------------------------------%

:- pred run_test_is_value(io.text_output_stream::in,
    string::in, pred(value)::in(pred(in) is semidet),
    json.value::in, io::di, io::uo) is det.

run_test_is_value(File, PredName, Pred, Value, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s) ===> ", [s(PredName), s(ValueStr)], !IO),
    ( if Pred(Value)
    then io.write_string(File, "TRUE\n", !IO)
    else io.write_string(File, "FALSE\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred run_test_get_value(io.text_output_stream::in,
    string::in, pred(value, T)::in(pred(in, out) is semidet),
    json.value::in, io::di, io::uo) is det.

run_test_get_value(File, PredName, Pred, Value, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s) ===> ", [s(PredName), s(ValueStr)], !IO),
    ( if Pred(Value, Result) then
        io.format(File, "%s\n", [s(string(Result))], !IO)
    else
        io.write_string(File, "FALSE\n", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred run_test_det_get_value(io.text_output_stream::in,
    string::in, (func(value) =  T)::in,
    json.value::in, io::di, io::uo) is cc_multi.

run_test_det_get_value(File, FuncName, Func, Value, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s) ===> ", [s(FuncName), s(ValueStr)], !IO),
    ( try [] (
        Result = Func(Value)
    ) then
        io.format(File, "%s\n", [s(string(Result))], !IO)
    catch Excp ->
        Excp = software_error(Msg),
        io.format(File, "EXCP (%s)\n", [s(Msg)], !IO)
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

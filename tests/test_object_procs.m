%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test procedures for manipulating JSON objects.
%
%-----------------------------------------------------------------------------%

:- module test_object_procs.
:- interface.

:- import_module io.

:- pred test_object_procs(io.text_output_stream::in, io::di, io::uo)
    is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_object_procs(File, !IO) :-
    list.foldl(
        run_lookup_test(File, "lookup_bool", lookup_bool, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_lookup_test(File, "lookup_string", lookup_string, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_lookup_test(File, "lookup_number", lookup_number, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_lookup_test(File, "lookup_int", lookup_int, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_lookup_test(File, "lookup_object", lookup_object, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_lookup_test(File, "lookup_array", lookup_array, test_members),
        test_objects, !IO),
    io.nl(File, !IO),

    list.foldl(
        run_search_test(File, "search_bool", search_bool, yes, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_string", search_string, "", test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_number", search_number, 561.0, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_int", search_int, 2, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_object", search_object, map.init, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_array", search_array, [], test_members),
        test_objects, !IO),
    io.nl(File, !IO),

    list.foldl(
        run_search_test(File, "search_string_or_null", search_string_or_null, "", test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_object_or_null", search_object_or_null, map.init, test_members),
        test_objects, !IO),
    io.nl(File, !IO),
    list.foldl(
        run_search_test(File, "search_array_or_null", search_array_or_null, [], test_members),
        test_objects, !IO).

%-----------------------------------------------------------------------------%

:- pred run_lookup_test(io.text_output_stream::in, string::in,
    (func(object, string) = T)::in, list(string)::in, json.value::in,
    io::di, io::uo) is cc_multi.

run_lookup_test(File, FuncName, Func, Members, Value, !IO) :-
    list.foldl(run_lookup_test_2(File, FuncName, Func, Value),
        Members, !IO).

:- pred run_lookup_test_2(io.text_output_stream::in, string::in,
    (func(object, string) = T)::in, value::in, string::in,
    io::di, io::uo) is cc_multi.

run_lookup_test_2(File, FuncName, Func, Value, Member, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s, \"%s\") ===> ",
        [s(FuncName), s(ValueStr), s(Member)], !IO),
    Object = det_get_object(Value),
    ( try [] (
        Result = Func(Object, Member)
    ) then
        io.format(File, "%s\n", [s(string(Result))], !IO)
    catch_any Excp ->
        io.format(File, "EXCP (%s)\n", [s(string(Excp))], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred run_search_test(io.text_output_stream::in, string::in,
    (func(object, string, T) = T)::in, T::in, list(string)::in, json.value::in,
    io::di, io::uo) is cc_multi.

run_search_test(File, FuncName, Func, Default, Members, Value, !IO) :-
    list.foldl(run_search_test_2(File, FuncName, Func, Default, Value),
        Members, !IO).

:- pred run_search_test_2(io.text_output_stream::in, string::in,
    (func(object, string, T) = T)::in, T::in, value::in, string::in,
    io::di, io::uo) is cc_multi.

run_search_test_2(File, FuncName, Func, Default, Value, Member, !IO) :-
    ValueStr = json.to_string(Value),
    io.format(File, "%s(%s, \"%s\", %s) ===> ",
        [s(FuncName), s(ValueStr), s(Member), s(string(Default))], !IO),
    Object = det_get_object(Value),
    ( try [] (
        Result = Func(Object, Member, Default)
    ) then
        io.format(File, "%s\n", [s(string(Result))], !IO)
    catch_any Excp ->
        io.format(File, "EXCP (%s)\n", [s(string(Excp))], !IO)
    ).

%-----------------------------------------------------------------------------%

:- func test_objects = list(value).

test_objects = [
    det_make_object([]),
    det_make_object(["foo" - null]),
    det_make_object(["foo" - bool(yes)]),
    det_make_object(["foo" - number(3.141)]),
    det_make_object(["foo" - string("abcde")]),
    det_make_object(["foo" - array([])]),
    det_make_object(["foo" - det_make_object([])])
].

:- func test_members = list(string).

test_members = [
    "foo",
    "bar"
].

%-----------------------------------------------------------------------------%
:- end_module test_object_procs.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test reading a stream of JSON values.
%
%-----------------------------------------------------------------------------%

:- module test_streaming.
:- interface.

:- import_module io.

% get_generic [DONE]
% get_value   [DONE]
% get_object
% get_array

:- pred test_streaming(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module list.
:- import_module stream.
:- import_module string.

%-----------------------------------------------------------------------------%

test_streaming(File, !IO) :-
    Test1 = "values.json_stream",
    io.format(File, "*** Testing get/4 method (%s) ***\n\n", [s(Test1)], !IO),
    do_stream_test(File, Test1, generic_get_until_eof, !IO),
    io.nl(File, !IO),

    Test2 = "bad_values.json_stream",
    io.format(File, "*** Testing get/4 method (%s) ***\n\n", [s(Test2)], !IO),
    do_stream_test(File, Test2, generic_get_until_eof, !IO),
    io.nl(File, !IO),

    Test3 = "values.json_stream",
    io.format(File, "*** Testing get_value/4 method (%s) ***\n\n", [s(Test3)], !IO),
    do_stream_test(File, Test3, get_value_until_eof, !IO),
    io.nl(File, !IO),

    Test4 = "bad_values.json_stream",
    io.format(File, "*** Testing get_value/4 method (%s) ***\n\n", [s(Test4)], !IO),
    do_stream_test(File, Test4, get_value_until_eof, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred do_stream_test(io.text_output_stream::in, string::in,
    pred(io.text_output_stream, json.reader(io.text_input_stream), io, io)
    ::in(pred(in, in, di, uo) is det), io::di, io::uo) is det.

do_stream_test(OutputFile, TestFileName, TestPred, !IO) :-
    io.open_input(TestFileName, OpenTestFileResult, !IO),
    (
        OpenTestFileResult = ok(TestFile),
        json.init_reader(TestFile, Reader, !IO),
        TestPred(OutputFile, Reader, !IO),
        io.close_input(TestFile, !IO)
    ;
        OpenTestFileResult = error(IO_Error),
        Msg = io.error_message(IO_Error),
        io.write_string(OutputFile, Msg, !IO),
        io.nl(OutputFile, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred generic_get_until_eof(io.text_output_stream::in,
    json.reader(io.text_input_stream)::in, io::di, io::uo) is det.

generic_get_until_eof(File, Reader, !IO) :-
    stream.get(Reader, GetResult, !IO),
    (
        GetResult = ok(Value),
        json.write_pretty(File, Value, !IO),
        generic_get_until_eof(File, Reader, !IO)
    ;
        GetResult = eof
    ;
        GetResult = error(Error),
        Msg = stream.error_message(Error),
        io.format(File, "error: %s", [s(Msg)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred get_value_until_eof(io.text_output_stream::in,
    json.reader(io.text_input_stream)::in, io::di, io::uo) is det.

get_value_until_eof(File, Reader, !IO) :-
    json.get_value(Reader, GetResult, !IO),
    (
        GetResult = ok(Value),
        json.write_pretty(File, Value, !IO),
        get_value_until_eof(File, Reader, !IO)
    ;
        GetResult = eof
    ;
        GetResult = error(Error),
        Msg = stream.error_message(Error),
        io.format(File, "error: %s", [s(Msg)], !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module test_streaming.
%-----------------------------------------------------------------------------%

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
% get_value
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
    test_generic_get(File, Test1, !IO),
    io.nl(File, !IO),

    Test2 = "bad_values.json_stream",
    io.format(File, "*** Testing get/4 method (%s) ***\n\n", [s(Test2)], !IO),
    test_generic_get(File, Test2, !IO),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred test_generic_get(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

test_generic_get(OutputFile, TestFileName, !IO) :-
    io.open_input(TestFileName, OpenTestFileResult, !IO),
    (
        OpenTestFileResult = ok(TestFile),
        json.init_reader(TestFile, Reader, !IO),
        generic_get_until_eof(OutputFile, Reader, !IO),
        io.close_input(TestFile, !IO)
    ;
        OpenTestFileResult = error(IO_Error),
        Msg = io.error_message(IO_Error),
        io.write_string(OutputFile, Msg, !IO),
        io.nl(OutputFile, !IO)
    ).
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
:- end_module test_streaming.
%-----------------------------------------------------------------------------%

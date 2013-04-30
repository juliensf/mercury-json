%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013, Julien Fischer.
% All rights reserved.
%
% Author: Julien Fischer <jfischer@opturion.com>
%
% Test harness for the JSON library.
%
%-----------------------------------------------------------------------------%

:- module test_json.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module getopt_io.
:- import_module list.
:- import_module require.
:- import_module stream.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        (pred(C::in, O::out) is semidet :- short_option(C, O)),
        long_option,
        (pred(O::out, D::out) is multi :- option_defaults(O, D))
    ),
    getopt_io.process_options(OptionOps, Args, NonOptionArgs, OptionResult,
        !IO),
    (
        OptionResult = ok(OptionTable),
        getopt_io.lookup_bool_option(OptionTable, help, Help),
        (
            Help = yes,
            help(!IO)
        ;
            Help = no,
            (
                NonOptionArgs = [],
                dir.foldl2(gather_json_file, this_directory, [],
                    MaybeGatherResult, !IO)
            ;
                NonOptionArgs = [_ | _],
                MaybeGatherResult = ok(NonOptionArgs)
            ),
            (
                MaybeGatherResult = ok(TestCases0),
                list.sort(TestCases0, TestCases),
                list.foldl(run_test, TestCases, !IO)
            ;
                MaybeGatherResult = error(_, _IO_Error),
                unexpected($module, $pred, "IO Error")
            )
        )
    ;
        OptionResult = error(Msg),
        bad_cmdline(Msg, !IO)
    ).

:- pred gather_json_file(string::in, string::in, io.file_type::in,
    bool::out, list(string)::in, list(string)::out, io::di, io::uo) is det.

gather_json_file(_Dir, File, Type, Continue, !Files, !IO) :-
    (
        Type = regular_file,
        ( if string.suffix(File, ".json")
        then !:Files = [File | !.Files]
        else true
        )
    ;
        ( Type = directory
        ; Type = symbolic_link
        ; Type = named_pipe
        ; Type = socket
        ; Type = character_device
        ; Type = block_device
        ; Type = message_queue
        ; Type = shared_memory
        ; Type = semaphore
        ; Type = unknown
        )
        % Skip over non-regular files.
    ),
    Continue = yes.

%-----------------------------------------------------------------------------%

:- pred run_test(string::in, io::di, io::uo) is det.

run_test(InputFileName, !IO) :-
    io.open_input(InputFileName, OpenInputResult, !IO),
    (
        OpenInputResult = ok(InputFile),
        BaseFileName = string.det_remove_suffix(InputFileName, ".json"),
        OutputFileName = BaseFileName ++ ".out",
        io.open_output(OutputFileName, OpenOutputResult, !IO),
        (
            OpenOutputResult = ok(OutputFile),
            parse_and_output(InputFile, OutputFile, !IO),
            io.close_input(InputFile, !IO),
            io.close_output(OutputFile, !IO),
            ExpFileName = BaseFileName ++ ".exp",
            ResFileName = BaseFileName ++ ".res",
            string.format("diff -u %s %s > %s",
                [s(ExpFileName), s(OutputFileName), s(ResFileName)],
                DiffCmd),
            io.call_system(DiffCmd, DiffCmdRes, !IO),
            (
                DiffCmdRes = ok(DiffExitStatus),
                ( if DiffExitStatus = 0 then
                    io.format("PASSED: %s\n", [s(BaseFileName)], !IO),
                    io.remove_file(ResFileName, _, !IO),
                    io.remove_file(OutputFileName, _, !IO)
                else
                    io.format("FAILED: %s\n", [s(BaseFileName)], !IO)
                )
            ;
                DiffCmdRes = error(DiffError),
                io.error_message(DiffError, Msg),
                io.format("ABORTED: %s (%s)\n", [s(BaseFileName), s(Msg)], !IO)
            )
        ;
            OpenOutputResult = error(_),
            unexpected($module, $pred, "cannot open output")
        )
    ;
        OpenInputResult = error(_),
        unexpected($module, $pred, "cannot open input")
    ).

:- pred parse_and_output(io.text_input_stream::in, io.text_output_stream::in,
    io::di, io::uo) is det.

parse_and_output(Input, Output, !IO) :-
    Reader = json.init_reader(Input, allow_comments),
    Writer = json.init_writer(Output),
    json.get_value(Reader, Result, !IO),
    (
        Result = ok(Value),
        put_json(Writer, Value, !IO),
        io.nl(Output, !IO)
    ;
        Result = eof,
        io.write_string(Output, "<<empty input stream>>\n", !IO)
    ;
        Result = error(Error),
        Msg = stream.error_message(Error),
        io.write_string(Output, Msg, !IO)
    ). 

%-----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       verbose
    ;       keep_files.

:- pred short_option(char, option).
:- mode short_option(in, out) is semidet.
:- mode short_option(out, in) is det.

short_option('h', help).
short_option('v', verbose).
short_option('k', keep_files).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("verbose", verbose).
long_option("keep-files", keep_files).

:- pred option_defaults(option, option_data).
:- mode option_defaults(in, out) is det.
:- mode option_defaults(out, out) is multi.

option_defaults(help, bool(no)).
option_defaults(verbose, bool(no)).
option_defaults(keep_files, bool(no)).

%-----------------------------------------------------------------------------%

:- pred help(io::di, io::uo) is det.

help(!IO) :-
    io.write_string("Usage: test_json [<options>] [test-case ...]\n", !IO),
    io.write_strings([
        "Options:\n",
        "\t-h, --help\n",
        "\t\tPrint this message.\n",
        "\t-v, -verbose\n",
        "\t\tOutput progress information.\n",
        "\t-k, --keep-files\n",
        "\t\tDo not delete files generated during a test run.\n"
    ], !IO).

%-----------------------------------------------------------------------------%

:- pred bad_cmdline(string::in, io::di, io::uo) is det.

bad_cmdline(Msg, !IO) :-
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "test_json: %s\n", [s(Msg)], !IO),
    io.write_string(Stderr, "test_json: use --help for more information.\n",
        !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
:- end_module test_json.
%-----------------------------------------------------------------------------%


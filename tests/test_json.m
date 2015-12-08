%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2015 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test harness for the JSON library.
%
%-----------------------------------------------------------------------------%

:- module test_json.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.
:- import_module test_marshal.

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
                    MaybeGatherResult, !IO),
                RunMarshalingTests = yes
            ;
                NonOptionArgs = [_ | _],
                ( if list.member("marshal", NonOptionArgs) then
                    list.delete_all(NonOptionArgs, "marshal", NonMarshalTests),
                    MaybeGatherResult = ok(NonMarshalTests),
                    RunMarshalingTests = yes
                else
                    MaybeGatherResult = ok(NonOptionArgs),
                    RunMarshalingTests = no
                )
            ),
            (
                MaybeGatherResult = ok(TestCases0),
                list.sort(TestCases0, TestCases),
                list.foldl(run_test(OptionTable), TestCases, !IO),
                (
                    RunMarshalingTests = yes,
                    run_marshaling_tests(OptionTable, !IO)
                ;
                    RunMarshalingTests = no
                )
            ;
                MaybeGatherResult = error(_, IO_Error),
                io.stderr_stream(Stderr, !IO),
                io.write_string(Stderr, io.error_message(IO_Error), !IO),
                io.set_exit_status(1, !IO)
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

:- pred run_test(option_table(option)::in, string::in, io::di, io::uo) is det.

run_test(OptionTable, InputFileName, !IO) :-
    io.open_input(InputFileName, OpenInputResult, !IO),
    (
        OpenInputResult = ok(InputFile),
        BaseFileName = string.det_remove_suffix(InputFileName, ".json"),
        OutputFileName = BaseFileName ++ ".out",
        io.open_output(OutputFileName, OpenOutputResult, !IO),
        (
            OpenOutputResult = ok(OutputFile),
            parse_and_output(BaseFileName, InputFile, OutputFile, !IO),
            io.close_input(InputFile, !IO),
            io.close_output(OutputFile, !IO),
            check_result(OptionTable, BaseFileName, !IO)
        ;
            OpenOutputResult = error(IO_Error),
            Msg = io.error_message(IO_Error),
            unexpected($file, $pred, "cannot open output: " ++ Msg)
        )
    ;
        OpenInputResult = error(IO_Error),
        Msg = io.error_message(IO_Error),
        unexpected($file, $pred, "cannot open input: " ++ Msg)
    ).

:- pred parse_and_output(string::in,
    io.text_input_stream::in, io.text_output_stream::in,
    io::di, io::uo) is det.

parse_and_output(BaseFileName, Input, Output, !IO) :-
    % Allow the default JSON reader parameters to be overridden for
    % specific tests -- such tests need to be added to the table
    % defined by the override_default_params/4 predicate.
    ( if
        override_default_params(BaseFileName, AllowComments0,
        AllowTrailingCommas0, AllowRepeatedMembers0, AllowInfinities0)
    then
        AllowComments = AllowComments0,
        AllowTrailingCommas = AllowTrailingCommas0,
        AllowRepeatedMembers = AllowRepeatedMembers0,
        AllowInfinities = AllowInfinities0
    else
        % The default JSON reader parameters for the tests.
        AllowComments = allow_comments,
        AllowTrailingCommas = do_not_allow_trailing_commas,
        AllowRepeatedMembers = do_not_allow_repeated_members,
        AllowInfinities = do_not_allow_infinities
    ),
    ReaderParams = reader_params(
        AllowComments,
        AllowTrailingCommas,
        AllowRepeatedMembers,
        AllowInfinities
    ),
    json.init_reader(Input, ReaderParams, Reader, !IO),
    WriterParams = writer_params(
        compact,
        AllowInfinities
    ),
    json.init_writer(Output, WriterParams, Writer, !IO),
    json.read_value(Reader, Result, !IO),
    (
        Result = ok(Value),
        json.put_value(Writer, Value, !IO),
        io.nl(Output, !IO)
    ;
        Result = eof,
        io.write_string(Output, "<<empty input stream>>\n", !IO)
    ;
        Result = error(Error),
        Msg = stream.error_message(Error),
        io.write_string(Output, Msg, !IO)
    ).

:- pred override_default_params(string::in,
    allow_comments::out, allow_trailing_commas::out,
    allow_repeated_members::out,
    allow_infinities::out) is semidet.

override_default_params("repeated_member_first",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_first,
    do_not_allow_infinities).
override_default_params("repeated_member_last",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_last,
    do_not_allow_infinities).
override_default_params("infinity",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_last,
    allow_infinities).

%-----------------------------------------------------------------------------%

:- pred run_marshaling_tests(option_table(option)::in, io::di, io::uo) is cc_multi.

run_marshaling_tests(OptionTable, !IO) :-
    BaseFileName = "marshal",
    OutputFileName = BaseFileName ++ ".out",
    io.open_output(OutputFileName, MaybeOpenResult, !IO),
    (
        MaybeOpenResult = ok(OutputFile),
        test_marshaling(OutputFile, !IO),
        io.close_output(OutputFile, !IO),
        check_result(OptionTable, BaseFileName, !IO)
    ;
        MaybeOpenResult = error(_),
        unexpected($file, $pred, "cannot open output")
    ).

%-----------------------------------------------------------------------------%

:- pred check_result(option_table(option)::in, string::in, io::di, io::uo) is det.

check_result(OptionTable, BaseFileName, !IO) :-
    OutputFileName = BaseFileName ++ ".out",
    ExpFileName = BaseFileName ++ ".exp",
    ResFileName = BaseFileName ++ ".res",
    lookup_accumulating_option(OptionTable, diff_flags, UserDiffFlags),
    DiffFlags = string.join_list(" ", ["-u" | UserDiffFlags]),
    string.format("diff %s %s %s > %s",
        [s(DiffFlags), s(ExpFileName), s(OutputFileName), s(ResFileName)],
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
    ).

%-----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       verbose
    ;       keep_files
    ;       diff_flags.

:- pred short_option(char, option).
:- mode short_option(in, out) is semidet.

short_option('h', help).
short_option('v', verbose).
short_option('k', keep_files).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("verbose", verbose).
long_option("keep-files", keep_files).
long_option("diff-flags", diff_flags).

:- pred option_defaults(option, option_data).
:- mode option_defaults(in, out) is det.
:- mode option_defaults(out, out) is multi.

option_defaults(help, bool(no)).
option_defaults(verbose, bool(no)).
option_defaults(keep_files, bool(no)).
option_defaults(diff_flags, accumulating([])).

%-----------------------------------------------------------------------------%

:- pred help(io::di, io::uo) is det.

help(!IO) :-
    io.write_string("Usage: test_json [<options>] [test-case ...]\n", !IO),
    io.write_strings([
        "Options:\n",
        "\t-h, --help\n",
        "\t\tPrint this message.\n",
        "\t-v, --verbose\n",
        "\t\tOutput progress information.\n",
        "\t-k, --keep-files\n",
        "\t\tDo not delete files generated during a test run.\n",
        "\t--diff-flags\n",
        "\t\tExtra flags to pass to the diff command.\n"
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


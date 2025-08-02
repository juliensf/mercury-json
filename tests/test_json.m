%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2016, 2018, 2022-2025 Julien Fischer.
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
:- import_module test_from_string.
:- import_module test_marshal.
:- import_module test_pointer.
:- import_module test_object_procs.
:- import_module test_streaming.
:- import_module test_unmarshal.
:- import_module test_value_procs.
:- import_module test_writer.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module getopt.
:- import_module io.file.
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
    getopt.process_options_io(OptionOps, Args, NonOptionArgs, OptionResult,
        !IO),
    (
        OptionResult = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        (
            Help = yes,
            help(!IO)
        ;
            Help = no,
            (
                NonOptionArgs = [],
                dir.foldl2(gather_json_file, this_directory, [],
                    MaybeGatherResult, !IO),
                RunMarshalingTests = yes,
                RunPointerTests = yes,
                RunStreamingTests = yes,
                RunUnmarshalingTests = yes,
                RunObjectProcTests = yes,
                RunValueProcTests = yes,
                RunWriterTests = yes,
                RunFromStringTests = yes
            ;
                NonOptionArgs = [_ | _],
                some [!FilteredArgs] (
                    !:FilteredArgs = NonOptionArgs,
                    ( if list.member("marshal", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "marshal", !:FilteredArgs),
                        RunMarshalingTests = yes
                    else
                        RunMarshalingTests = no
                    ),
                    ( if list.member("pointer", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "pointer", !:FilteredArgs),
                        RunPointerTests = yes
                    else
                        RunPointerTests = no
                    ),
                    ( if list.member("unmarshal", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "unmarshal", !:FilteredArgs),
                        RunUnmarshalingTests = yes
                    else
                        RunUnmarshalingTests = no
                    ),
                    ( if list.member("object_procs", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "object_procs", !:FilteredArgs),
                        RunObjectProcTests = yes
                    else
                        RunObjectProcTests = no
                    ),
                    ( if list.member("value_procs", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "value_procs", !:FilteredArgs),
                        RunValueProcTests = yes
                    else
                        RunValueProcTests = no
                    ),
                    ( if list.member("writer", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "writer", !:FilteredArgs),
                        RunWriterTests = yes
                    else
                        RunWriterTests = no
                    ),
                    ( if list.member("streaming", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "streaming", !:FilteredArgs),
                        RunStreamingTests = yes
                    else
                        RunStreamingTests = no
                    ),
                    ( if list.member("from_string", !.FilteredArgs) then
                        list.delete_all(!.FilteredArgs, "from_string", !:FilteredArgs),
                        RunFromStringTests = yes
                    else
                        RunFromStringTests = no
                    ),
                    MaybeGatherResult = ok(!.FilteredArgs)
                )
            ),
            (
                MaybeGatherResult = ok(TestCases0),
                list.sort_and_remove_dups(TestCases0, TestCases),
                some [!NumFailures, !TotalTests] (
                    list.length(TestCases, !:TotalTests),
                    list.foldl2(run_test(OptionTable), TestCases,
                        0, !:NumFailures, !IO),
                    (
                        RunMarshalingTests = yes,
                        run_internal_tests(OptionTable, "marshal", test_marshaling,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunMarshalingTests = no
                    ),
                    (
                        RunPointerTests = yes,
                        run_internal_tests(OptionTable, "pointer", test_pointer,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunPointerTests = no
                    ),
                    (
                        RunStreamingTests = yes,
                        run_internal_tests(OptionTable, "streaming", test_streaming,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunStreamingTests = no
                    ),
                    (
                        RunUnmarshalingTests = yes,
                        run_internal_tests(OptionTable, "unmarshal", test_unmarshaling,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunUnmarshalingTests = no
                    ),
                    (
                        RunObjectProcTests = yes,
                        run_internal_tests(OptionTable, "object_procs", test_object_procs,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunObjectProcTests = no
                    ),
                    (
                        RunValueProcTests = yes,
                        run_internal_tests(OptionTable, "value_procs", test_value_procs,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunValueProcTests = no
                    ),
                    (
                        RunWriterTests = yes,
                        run_internal_tests(OptionTable, "writer", test_writer,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunWriterTests = no
                    ),
                    (
                        RunFromStringTests = yes,
                        run_internal_tests(OptionTable, "from_string", test_from_string,
                            !NumFailures, !IO),
                        !:TotalTests = !.TotalTests + 1
                    ;
                        RunFromStringTests = no
                    ),
                    ( if !.NumFailures = 0 then
                        io.write_string("ALL TESTS PASSED\n", !IO)
                    else
                        io.format("%d / %d TESTS FAILED\n",
                            [i(!.NumFailures), i(!.TotalTests)], !IO)
                    )
                )
            ;
                MaybeGatherResult = error(_, IO_Error),
                io.stderr_stream(Stderr, !IO),
                io.write_string(Stderr, io.error_message(IO_Error), !IO),
                io.set_exit_status(1, !IO)
            )
        )
    ;
        OptionResult = error(OptionError),
        Msg = option_error_to_string(OptionError),
        bad_cmdline(Msg, !IO)
    ).

:- pred gather_json_file(string::in, string::in, io.file_type::in,
    bool::out, list(string)::in, list(string)::out, io::di, io::uo) is det.

gather_json_file(_Dir, File, Type, Continue, !Files, IO, IO) :-
    (
        Type = regular_file,
        ( if string.suffix(File, ".json") then
            !:Files = [File | !.Files]
        else
            true
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

:- pred run_test(option_table(option)::in, string::in, int::in, int::out,
    io::di, io::uo) is det.

run_test(OptionTable, InputFileName, !NumFailures, !IO) :-
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
            check_result(OptionTable, BaseFileName, !NumFailures, !IO)
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
        override_default_params(BaseFileName,
            AllowComments0,
            AllowTrailingCommas0,
            AllowRepeatedMembers0,
            AllowInfinities0,
            AllowSingleQuotedStrings0,
            MaxNestingDepth0,
            AllowHexEscapes0)
    then
        AllowComments = AllowComments0,
        AllowTrailingCommas = AllowTrailingCommas0,
        AllowRepeatedMembers = AllowRepeatedMembers0,
        AllowInfinities = AllowInfinities0,
        AllowSingleQuotedStrings = AllowSingleQuotedStrings0,
        MaxNestingDepth = MaxNestingDepth0,
        AllowHexEscapes = AllowHexEscapes0
    else
        % The default JSON reader parameters for the tests.
        AllowComments = allow_comments,
        AllowTrailingCommas = do_not_allow_trailing_commas,
        AllowRepeatedMembers = do_not_allow_repeated_members,
        AllowInfinities = do_not_allow_infinities,
        AllowSingleQuotedStrings = do_not_allow_single_quoted_strings,
        MaxNestingDepth = no_maximum_nesting_depth,
        AllowHexEscapes = do_not_allow_hex_escapes
    ),
    ReaderParams = reader_params(
        AllowComments,
        AllowTrailingCommas,
        AllowRepeatedMembers,
        AllowInfinities,
        MaxNestingDepth,
        do_not_allow_additional_whitespace,
        AllowSingleQuotedStrings,
        AllowHexEscapes
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
    allow_comments::out,
    allow_trailing_commas::out,
    allow_repeated_members::out,
    allow_infinities::out,
    allow_single_quoted_strings::out,
    maximum_nesting_depth::out,
    allow_hex_escapes::out) is semidet.

override_default_params(TestName, AllowComments, AllowTrailingCommas,
        AllowRepeatedMembers, AllowInfinities, AllowSingleQuotedStrings,
        MaxNestingDepth, AllowHexEscapes) :-
    ( if string.prefix(TestName, "j5_") then
        % Default reader parameters for JSON5 tests.
        AllowComments = allow_comments,
        AllowTrailingCommas = allow_trailing_commas,
        AllowRepeatedMembers = do_not_allow_repeated_members,
        AllowInfinities = allow_infinities,
        AllowSingleQuotedStrings = allow_single_quoted_strings,
        MaxNestingDepth = no_maximum_nesting_depth,
        AllowHexEscapes = allow_hex_escapes
    else
        override_default_params_2(TestName,
            AllowComments, AllowTrailingCommas,
            AllowRepeatedMembers, AllowInfinities,
            AllowSingleQuotedStrings, MaxNestingDepth,
            AllowHexEscapes)
    ).

:- pred override_default_params_2(string::in,
    allow_comments::out,
    allow_trailing_commas::out,
    allow_repeated_members::out,
    allow_infinities::out,
    allow_single_quoted_strings::out,
    maximum_nesting_depth::out,
    allow_hex_escapes::out) is semidet.

override_default_params_2("repeated_member_first",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_first,
    do_not_allow_infinities,
    do_not_allow_single_quoted_strings,
    no_maximum_nesting_depth,
    do_not_allow_hex_escapes).
override_default_params_2("repeated_member_last",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_last,
    do_not_allow_infinities,
    do_not_allow_single_quoted_strings,
    no_maximum_nesting_depth,
    do_not_allow_hex_escapes).
override_default_params_2("infinity",
    allow_comments,
    do_not_allow_trailing_commas,
    allow_repeated_members_keep_last,
    allow_infinities,
    do_not_allow_single_quoted_strings,
    no_maximum_nesting_depth,
    do_not_allow_hex_escapes).

%-----------------------------------------------------------------------------%

:- pred run_internal_tests(option_table(option), string,
    pred(io.text_output_stream, io, io), int, int, io, io).
:- mode run_internal_tests(in, in, pred(in, di, uo) is det,
    in, out, di, uo) is det.
:- mode run_internal_tests(in, in, pred(in, di, uo) is cc_multi,
    in, out, di, uo) is cc_multi.

run_internal_tests(OptionTable, BaseFileName, TestPred, !NumFailures, !IO) :-
    OutputFileName = BaseFileName ++ ".out",
    io.open_output(OutputFileName, MaybeOpenResult, !IO),
    (
        MaybeOpenResult = ok(OutputFile),
        TestPred(OutputFile, !IO),
        io.close_output(OutputFile, !IO),
        check_result(OptionTable, BaseFileName, !NumFailures, !IO)
    ;
        MaybeOpenResult = error(_),
        unexpected($file, $pred, "cannot open output")
    ).

%-----------------------------------------------------------------------------%

:- pred check_result(option_table(option)::in, string::in, int::in, int::out,
    io::di, io::uo) is det.

check_result(OptionTable, BaseFileName, !NumFailures, !IO) :-
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
            io.file.remove_file(ResFileName, _, !IO),
            io.file.remove_file(OutputFileName, _, !IO)
        else
            io.format("FAILED: %s\n", [s(BaseFileName)], !IO),
            !:NumFailures = !.NumFailures + 1
        )
    ;
        DiffCmdRes = error(DiffError),
        io.error_message(DiffError, Msg),
        io.format("ABORTED: %s (%s)\n", [s(BaseFileName), s(Msg)], !IO),
        !:NumFailures = !.NumFailures + 1
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

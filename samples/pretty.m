%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014-2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This program reads a JSON value into memory from a specified file (or from
% the standard input if no file is specified), and, if successful, pretty
% prints the same value to the standard output stream.
%
%-----------------------------------------------------------------------------%

:- module pretty.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module getopt.
:- import_module stream.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ( try [io(!IO)] (
        main_2(!IO)
    )
    then
        true
    catch IO_Error ->
        io.error_message(IO_Error, Msg),
        print_error("error: " ++ Msg ++ "\n", !IO)
    catch_any Other ->
        throw(Other)
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOpts = option_ops_multi(
        short_option,
        long_option,
        option_default
    ),
    getopt.process_options(OptionOpts, Args, NonOptionArgs, MaybeOptionTable),
    (
        MaybeOptionTable = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        (
            Help = yes,
            usage(!IO)
        ;
            Help = no,
            handle_args_and_options(OptionTable, NonOptionArgs, Params,
                MaybeInputFile, Continue, !IO),
            (
                Continue = yes,
                pretty_print_json(Params, MaybeInputFile, !IO)
            ;
                Continue = no
            )
        )
    ;
        MaybeOptionTable = error(OptionErrorMsg),
        string.format("error: %s.\n", [s(OptionErrorMsg)], ErrorMsg),
        print_error(ErrorMsg, !IO)
    ).

:- pred handle_args_and_options(option_table(option)::in, list(string)::in,
    reader_params::out, maybe(io.text_input_stream)::out,
    bool::out, io::di, io::uo) is det.

handle_args_and_options(OptionTable, Args, ReaderParams, MaybeInputFile,
        !:Continue, !IO) :-
    !:Continue = yes,
    (
        Args = [],
        MaybeInputFile = no
    ;
        Args = [InputFileName],
        io.open_input(InputFileName, OpenInputResult, !IO),
        (
            OpenInputResult = ok(InputFile),
            MaybeInputFile = yes(InputFile)
        ;
            OpenInputResult = error(OpenInputError),
            io.error_message(OpenInputError, OpenInputErrorMsg),
            print_error("error: " ++ OpenInputErrorMsg ++ "\n", !IO),
            MaybeInputFile = no,    % Dummy value.
            !:Continue = no
        )
    ;
        Args = [_, _ | _],
        MaybeInputFile = no,
        ArgErrorMsg = "error: more than one argument.\n",
        print_error(ArgErrorMsg, !IO),
        !:Continue = no
    ),
    getopt.lookup_bool_option(OptionTable, allow_infinities,
        AllowInfinitiesOpt),
    (
        AllowInfinitiesOpt = no,
        AllowInfinities = do_not_allow_infinities
    ;
        AllowInfinitiesOpt = yes,
        AllowInfinities = allow_infinities
    ),
    getopt.lookup_bool_option(OptionTable, allow_trailing_commas,
        AllowTrailingCommasOpt),
    (
        AllowTrailingCommasOpt = no,
        AllowTrailingCommas = do_not_allow_trailing_commas
    ;
        AllowTrailingCommasOpt = yes,
        AllowTrailingCommas = allow_trailing_commas
    ),
    getopt.lookup_bool_option(OptionTable, allow_comments,
        AllowCommentsOpt),
    (
        AllowCommentsOpt = no,
        AllowComments = do_not_allow_comments
    ;
        AllowCommentsOpt = yes,
        AllowComments = allow_comments
    ),
    getopt.lookup_string_option(OptionTable, repeated_members,
        RepeatedMembersOpt),
    ( if string_to_repeated_members(RepeatedMembersOpt,  RepeatedMembers0) then
        RepeatedMembers = RepeatedMembers0
    else
        BadValueMsg = "error: value of '--repeated-members' must be one of" ++
            " {none,first,last}.\n",
        print_error(BadValueMsg, !IO),
        RepeatedMembers = do_not_allow_repeated_members,    % Dummy value.
        !:Continue = no
    ),
    getopt.lookup_maybe_int_option(OptionTable, maximum_nesting_depth,
        MaybeMaxNestDepth),
    (
        MaybeMaxNestDepth = no,
        MaxNestDepth = no_maximum_nesting_depth
    ;
        MaybeMaxNestDepth = yes(DepthLimit),
        ( if DepthLimit < 0 then
            BadMaxNestDepthMsg = "error: value of '--maximum-nesting-depth'" ++
                "must be greater than equal to zero.\n",
            print_error(BadMaxNestDepthMsg, !IO),
            MaxNestDepth = maximum_nesting_depth(0), % Dummy value.
            !:Continue = no
        else
            MaxNestDepth = maximum_nesting_depth(DepthLimit)
        )
    ),
    ReaderParams = reader_params(AllowComments, AllowTrailingCommas,
        RepeatedMembers, AllowInfinities, MaxNestDepth).

:- pred pretty_print_json(json.reader_params::in,
    maybe(io.text_input_stream)::in, io::di, io::uo) is det.

pretty_print_json(ReaderParams, MaybeInputFile, !IO) :-
    (
        MaybeInputFile = yes(InputFile)
    ;
        MaybeInputFile = no,
        io.stdin_stream(InputFile, !IO)
    ),

    % Initialise a JSON reader and attach it to the input file stream.
    %
    json.init_reader(InputFile, ReaderParams, Reader, !IO),

    % Ask the reader to get a JSON value from the standard input stream.
    %
    json.read_value(Reader, ValueResult, !IO),
    (
        MaybeInputFile = yes(_),
        io.close_input(InputFile,!IO)
    ;
        MaybeInputFile = no
    ),
    (
        % The reader has returned a JSON value.
        %
        ValueResult = ok(Value),

        % Initialise a JSON writer and attach it to the standard output stream.
        % Set up the writer to pretty print the JSON output.  If the reader
        % was set up to allow infinities in the JSON, then also allow them in
        % the writer.
        %
        io.stdout_stream(Stdout, !IO),
        WriterParams = writer_params(pretty, ReaderParams ^ allow_infinities),
        json.init_writer(Stdout, WriterParams, Writer, !IO),
        json.put_value(Writer, Value, !IO)
    ;
        (
            % The input stream was empty.
            ValueResult = eof,
            ErrorMsg = "error: unexpected end-of-file\n"
        ;
            % An error was detected while reading the JSON value from the input
            % stream.  The error returned by the reader is a value of type
            % 'json.error'/1.  Values of this type are instances of the
            % standard library's 'stream.error'/1 type class, so we can convert
            % them into a human-readable format using the 'error_message'/1
            % function method from that class.
            %
            ValueResult = error(JSON_Error),
            ErrorMsg = stream.error_message(JSON_Error)
        ),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, ErrorMsg, !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       allow_infinities
    ;       allow_trailing_commas
    ;       allow_comments
    ;       repeated_members
    ;       maximum_nesting_depth.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",                  help).
long_option("allow-infinities",      allow_infinities).
long_option("allow-trailing-commas", allow_trailing_commas).
long_option("allow-comments",        allow_comments).
long_option("repeated-members",      repeated_members).
long_option("maximum-nesting-depth", maximum_nesting_depth).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help, bool(no)).
option_default(allow_infinities, bool(no)).
option_default(allow_trailing_commas, bool(no)).
option_default(allow_comments, bool(no)).
option_default(repeated_members, string("none")).
option_default(maximum_nesting_depth, maybe_int(no)).

:- pred string_to_repeated_members(string::in,
    allow_repeated_members::out) is semidet.

string_to_repeated_members("none", do_not_allow_repeated_members).
string_to_repeated_members("first", allow_repeated_members_keep_first).
string_to_repeated_members("last", allow_repeated_members_keep_last).

%-----------------------------------------------------------------------------%

:- pred print_error(string::in, io::di, io::uo) is det.

print_error(Msg, !IO) :-
    io.stderr_stream(Stderr, !IO),
    io.write_string(Stderr, Msg, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.stderr_stream(File, !IO),
    io.write_string(File, "pretty -- pretty print JSON to standard output.\n", !IO),
    io.nl(File, !IO),
    io.write_string(File, "Usage: pretty [<options>] [<arg>]\n", !IO),
    io.nl(File, !IO),
    io.write_string(File, "OPTIONS:\n", !IO),
    write_tabbed_lines(File, [
        "--help",
        "\tPrint usage information.",
        "--repeated-members {none,first,last}",
        "\tSpecifies whether repeated object members are allowed in the",
        "\tinput JSON and, if so, which should be included in the output.",
        "---allow-trailing-commas",
        "\tAllow trailing commas in arrays and objects in the input JSON.",
        "--allow-infinities",
        "\tAllow \"Infinity\" in the input JSON.",
        "--allow-comments",
        "\tAllow comments in the input JSON.",
        "--max-nesting-depth <n>",
        "--maximum-nesting-depth <n>",
        "\tAbort if the nesting depth reaches <n>.",
        "\tDefault is no maximum nesting depth limit."
    ], !IO).

:- pred write_tabbed_lines(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

write_tabbed_lines(_, [], !IO).
write_tabbed_lines(File, [Str | Strs], !IO) :-
    io.write_char(File, '\t', !IO),
    io.write_string(File, Str, !IO),
    io.nl(File, !IO),
    write_tabbed_lines(File, Strs, !IO).

%-----------------------------------------------------------------------------%
:- end_module pretty.
%-----------------------------------------------------------------------------%

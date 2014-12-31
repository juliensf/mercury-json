%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This program reads a JSON value into memory from the standard input stream
% and, if successful, pretty prints the same value to the standard output
% stream.
%
%-----------------------------------------------------------------------------%

:- module pretty.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module stream.

%-----------------------------------------------------------------------------%

main(!IO) :-

    % Initialise a JSON reader and attach it to the standard input stream.
    %
    io.stdin_stream(Stdin, !IO),
    Reader = json.init_reader(Stdin),

    % Ask the reader to get a JSON value from the standard input stream.
    %
    json.read_value(Reader, ValueResult, !IO),
    (
        % The reader has returned a JSON value.
        %
        ValueResult = ok(Value),

        % Initialise a JSON writer and attach it to the standard output stream.
        % Set up the writer to pretty print the JSON output.
        %
        io.stdout_stream(Stdout, !IO),
        WriterParams = writer_params(pretty),
        Writer = json.init_writer(Stdout, WriterParams),
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
:- end_module pretty.
%-----------------------------------------------------------------------------%

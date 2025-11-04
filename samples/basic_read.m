%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2025 Julien Fischer.
% See the file COPYING for license details.
%---------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This program reads some JSON from the file named on the command line
% argument and prints some details about the JSON document it reads to the
% the standard output.
%
%---------------------------------------------------------------------------%

:- module basic_read.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module json.
:- import_module list.
:- import_module map.
:- import_module stream.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [FileName],
        json.read_value_from_named_file(FileName, ReadResult, !IO),
        (
            ReadResult = ok(Document),
            summarize_json_document(FileName, Document, !IO)
        ;
            (
                ReadResult = eof,
                ErrorMsg = "error: input file is empty\n"
            ;
                ReadResult = error(ReaderError),
                (
                    ReaderError = stream_error(IOError),
                    io.error_message(IOError, ErrorMsg0),
                    ErrorMsg = ErrorMsg0 ++ "\n"
                ;
                    ReaderError = json_error(Context, Desc),
                    ErrorMsg = error_context_and_desc_to_string(Context, Desc)
                )
            ),
            io.stderr_stream(Stderr, !IO),
            io.write_string(Stderr, ErrorMsg, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        ( Args = []
        ; Args = [_, _ | _]
        ),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr,
            "error: expected a single argument on the command line.\n", !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred summarize_json_document(string::in, json.value::in,
    io::di, io::uo) is det.

summarize_json_document(FileName, Document, !IO) :-
    (
        Document = null,
        Summary = "the null value"
    ;
        Document = bool(_),
        Summary = "a Boolean"
    ;
        Document = string(_),
        Summary = "a string"
    ;
        Document = number(_),
        Summary = "a number"
    ;
        Document = array(Elems),
        (
            Elems = [],
            Summary = "an empty array"
        ;
            Elems = [_ | _],
            list.length(Elems, NumElems),
            Summary = string.format("an array containing %d elements",
                [i(NumElems)])
        )
    ;
        Document = object(Object),
        % In mercury-json, objects are just maps.
        ( if map.is_empty(Object) then
            Summary = "an empty object"
        else
            map.count(Object, NumMembers),
            ( if NumMembers = 1 then
                Summary = "an object with 1 member"
            else
                Summary = string.format("an object with %d members",
                    [i(NumMembers)])
            )
        )
    ),
    io.format("%s contains %s.\n", [s(FileName), s(Summary)], !IO).

%---------------------------------------------------------------------------%
:- end_module basic_read.
%---------------------------------------------------------------------------%


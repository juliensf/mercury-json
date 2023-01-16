%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014, 2018, 2020, 2023 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%
%
% This module provides string reader streams, which allow Mercury strings to be
% used as a source of characters for character reader streams.
%
%-----------------------------------------------------------------------------%

:- module json.string_reader.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module char.
:- import_module maybe.
:- import_module stream.

%-----------------------------------------------------------------------------%
%
% String readers.
%

    % A string reader: an input stream that is initialised with a Mercury
    % string as a source of characters.
    %
:- type string_reader.

    % The state that is updated by operations on string reader streams.
    %
:- type string_reader_state.

    % An error type for string reader streams.
    % String readers cannot ever return errors, but instances of the stream
    % type classes need to provide one.
    %
:- type string_reader_error
     --->   string_reader_error.

%-----------------------------------------------------------------------------%
%
% String reader creation.
%

    % init_state(State):
    % Create an initial value of a string reader state.
    % Multiple string readers may be attached to this state.
    %
:- pred init_string_state(string_reader_state::uo) is det.

    % init_reader(MaybeName, Src, Reader, !State):
    %
:- pred init_string_reader(maybe(string)::in, string::in,
    string_reader::out,
    string_reader_state::di, string_reader_state::uo) is det.

%-----------------------------------------------------------------------------%
%
% Stream type class instances.
%

:- instance error(string_reader_error).
:- instance stream(string_reader, string_reader_state).
:- instance input(string_reader, string_reader_state).
:- instance reader(string_reader, char, string_reader_state,
    string_reader_error).
:- instance unboxed_reader(string_reader, char, string_reader_state,
    string_reader_error).
:- instance putback(string_reader, char, string_reader_state,
    string_reader_error).
:- instance line_oriented(string_reader, string_reader_state).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module store.

%-----------------------------------------------------------------------------%

:- type string_reader_state
    --->    string_reader_state.

:- instance store(string_reader_state) where [].

%-----------------------------------------------------------------------------%

:- type string_reader
    --->    string_reader(
                reader_maybe_name   :: maybe(string),
                reader_src          :: string,
                reader_src_length   :: int,  % In code units.
                reader_mutable_info
                :: generic_mutvar(reader_mutable_info, string_reader_state)
            ).

:- type reader_mutable_info
    --->    reader_mutable_info(
                rmi_line_number :: int,
                % The current line number.

                rmi_next_index  :: int,
                % The index of the code unit of the next character we read
                % from the source string.

                rmi_putback_char :: int
                % The code point of the putback character or -1 if there
                % is no putback character.  The JSON parser only requires
                % a single character of putback.
            ).

%-----------------------------------------------------------------------------%

init_string_state(string_reader_state).

init_string_reader(MaybeName, Src, Reader, !State) :-
    string.length(Src, SrcLen),
    InitLineNum = 1,
    MutableInfo = reader_mutable_info(InitLineNum, 0, -1),
    store.new_mutvar(MutableInfo, MutableInfoVar, !State),
    Reader = string_reader(MaybeName, Src, SrcLen, MutableInfoVar).

%-----------------------------------------------------------------------------%

:- instance error(string_reader_error) where [
    error_message(_) = "<<string reader error>>"
].

:- instance stream(string_reader, string_reader_state) where [
    ( name(Reader, Name, !State) :-
        MaybeName = Reader ^ reader_maybe_name,
        (
            MaybeName = yes(Name)
        ;
            MaybeName = no,
            Name = "<<string reader>>"
        )
    )
].

:- instance input(string_reader, string_reader_state) where [].

:- instance reader(string_reader, char, string_reader_state,
    string_reader_error)
where [
    ( get(Reader, Result, !State) :-
        Reader = string_reader(_MaybeName, Src, _SrcLen, MutableInfoVar),
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(LineNum, NextIndex, PutbackInt),
        ( if PutbackInt > -1 then
            char.det_from_int(PutbackInt, Char),
            LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
            MutableInfo = reader_mutable_info(LineNumPrime, NextIndex, -1),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State),
            Result = ok(Char)
        else if
            NextIndex < Reader ^ reader_src_length,
            string.unsafe_index_next(Src, NextIndex, NextIndexPrime, Char)
        then
            LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
            MutableInfo = reader_mutable_info(LineNumPrime, NextIndexPrime,
                -1),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State),
            Result = ok(Char)
        else
            Result = eof
        )
    )
].

:- instance unboxed_reader(string_reader, char, string_reader_state,
    string_reader_error)
where [
    ( unboxed_get(Reader, Result, Char, !State) :-
        Reader = string_reader(_MaybeName, Src, _SrcLen, MutableInfoVar),
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(LineNum, NextIndex, PutbackInt),
        ( if PutbackInt > -1 then
            char.det_from_int(PutbackInt, Char),
            LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
            MutableInfo = reader_mutable_info(LineNumPrime, NextIndex, -1),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State),
            Result = ok
        else if
            NextIndex < Reader ^ reader_src_length,
            string.unsafe_index_next(Src, NextIndex, NextIndexPrime, Char0)
        then
            LineNumPrime = ( if Char = '\n' then LineNum + 1 else LineNum ),
            MutableInfo = reader_mutable_info(LineNumPrime, NextIndexPrime,
                -1),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State),
            Char = Char0,
            Result = ok
        else
            Char = '0',
            Result = eof
        )
    )
].

:- instance stream.putback(string_reader, char, string_reader_state,
        string_reader_error)
where [
    ( unget(Reader, Char, !State) :-
        Reader = string_reader(_MaybeName, _Src, _SrcLen, MutableInfoVar),
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(LineNum, NextIndex, PutbackInt),
        %
        % The JSON reader will only try to unget characters that were
        % the result of the last call to get/4.
        %
        ( if PutbackInt < 0 then
            CodePoint = char.to_int(Char),
            LineNumPrime = ( if Char = '\n' then LineNum - 1 else LineNum ),
            MutableInfo = reader_mutable_info(LineNumPrime, NextIndex,
                CodePoint),
            store.set_mutvar(MutableInfoVar, MutableInfo, !State)
        else
            unexpected($file, $pred, "multiple level of putback")
        )
    )
].

:- instance line_oriented(string_reader, string_reader_state) where [
    ( get_line(Reader, LineNo, !State) :-
        MutableInfoVar = Reader ^ reader_mutable_info,
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(LineNo, _, _)
    ),
    ( set_line(Reader, LineNo, !State) :-
        MutableInfoVar = Reader ^ reader_mutable_info,
        store.get_mutvar(MutableInfoVar, MutableInfo0, !State),
        MutableInfo0 = reader_mutable_info(_, NextIndex, Putback),
        MutableInfo = reader_mutable_info(LineNo, NextIndex, Putback),
        store.set_mutvar(MutableInfoVar, MutableInfo, !State)
    )
].

%-----------------------------------------------------------------------------%
:- end_module string_reader.
%-----------------------------------------------------------------------------%

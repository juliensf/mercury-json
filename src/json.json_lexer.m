%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2015 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%

:- module json.json_lexer.
:- interface.

%-----------------------------------------------------------------------------%

:- type token(Error)
    --->    token_left_curly_bracket
    ;       token_right_curly_bracket
    ;       token_left_square_bracket
    ;       token_right_square_bracket
    ;       token_comma
    ;       token_colon
    ;       token_string(string)
    ;       token_number(float)
    ;       token_false
    ;       token_true
    ;       token_null
    ;       token_eof
    ;       token_error(json.error(Error)).

:- pred get_token(json.reader(Stream)::in, token(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

:- func token_to_string(token(Error)) = string.

:- func escape_json_string(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_token(Reader, Token, !State) :-
    stream.get(Reader ^ json_reader_stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if
            json_lexer.is_whitespace(Char)
        then
            get_token(Reader, Token, !State)
        else if
            Char = '{'
        then
            Token = token_left_curly_bracket
        else if
            Char = '}'
        then
            Token = token_right_curly_bracket
        else if
            Char = '['
        then
            Token = token_left_square_bracket
        else if
            Char = ']'
        then
            Token = token_right_square_bracket
        else if
            Char = (':')
        then
            Token = token_colon
        else if
            Char = (',')
        then
            Token = token_comma
        else if
            Char = ('/'),
            Reader ^ json_comments = allow_comments
        then
            make_error_context(Reader, StartCommentContext, !State),
            consume_comment(Reader, StartCommentContext, CommentResult,
                !State),
            (
                CommentResult = ok,
                get_token(Reader, Token, !State)
            ;
                CommentResult = error(Error),
                Token = token_error(Error)
            )
        else if
            Char = '"'
        then
            get_string_literal(Reader, Token, !State)
        else if
            Char = ('-')
        then
            Buffer = Reader ^ json_char_buffer,
            char_buffer.add(Buffer, Char, !State),
            get_negative_number(Reader, Buffer, Token, !State),
            char_buffer.reset(Buffer, !State)
        else if
            char.is_digit(Char)
        then
            Buffer = Reader ^ json_char_buffer,
            char_buffer.add(Buffer, Char, !State),
            get_number(Reader, Buffer, Token, !State),
            char_buffer.reset(Buffer, !State)
        else if
            Char = ('I'),
            Reader ^ json_infinities = allow_infinities
        then
            Buffer = Reader ^ json_char_buffer,
            char_buffer.add(Buffer, Char, !State),
            get_infinity(Reader, Buffer, Token, !State),
            char_buffer.reset(Buffer, !State)
        else if
            ( Char = 'n'
            ; Char = 't'
            ; Char = 'f'
            )
        then
            Buffer = Reader ^ json_char_buffer,
            char_buffer.add(Buffer, Char, !State),
            get_keyword(Reader, Buffer, Token, !State),
            char_buffer.reset(Buffer, !State)
        else if char.to_int(Char, 0) then
            make_error_context(Reader, Context, !State),
            Error = json_error(Context, null_character),
            Token = token_error(Error)
        else
            make_error_context(Reader, Context, !State),
            Error = json_error(Context,
                syntax_error(string.from_char(Char), no)),
            Token = token_error(Error)
        )
    ;
        ReadResult = eof,
        Token = token_eof
    ;
        ReadResult = error(StreamError),
        Token = token_error(stream_error(StreamError))
    ).

:- pred is_whitespace(char::in) is semidet.

is_whitespace('\n').
is_whitespace('\r').
is_whitespace(' ').
is_whitespace('\t').

%-----------------------------------------------------------------------------%
%
% Column numbers.
%

:- pred update_column_number(json.reader(Stream)::in, char::in,
    State::di, State::uo) is det.

update_column_number(Reader, Char, !State) :-
    promise_pure (
        ColMutVar = Reader ^ json_column_number,
        ( if Char = ('\n') then
            impure set_mutvar(ColMutVar, 0)
        else
            impure get_mutvar(ColMutVar, Col),
            impure set_mutvar(ColMutVar, Col + 1)
        ),
        !:State = !.State
    ).

%-----------------------------------------------------------------------------%
%
% String literals.
%

:- pred get_string_literal(json.reader(Stream)::in, token(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_string_literal(Reader, Token, !State) :-
    Buffer = Reader ^ json_char_buffer,
    get_string_chars(Reader, Buffer, Result, !State),
    (
        Result = ok,
        String = char_buffer.to_string(Buffer, !.State),
        Token = token_string(String)
    ;
        Result = error(Error),
        Token = token_error(Error)
    ),
    char_buffer.reset(Buffer, !State).

:- pred get_string_chars(json.reader(Stream)::in,
    char_buffer::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_string_chars(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if
            Char = ('\\')
        then
            get_escaped_char(Reader, Buffer, EscapedCharResult, !State),
            (
                EscapedCharResult = ok,
                get_string_chars(Reader, Buffer, Result, !State)
            ;
                EscapedCharResult = error(Error),
                Result = error(Error)
            )
        else if Char = '"' then
            Result = ok
        else if
            char.to_int(Char, CodePoint),
            CodePoint >= 0x0000, CodePoint =< 0x001F
        then
            make_error_context(Reader, Context, !State),
            ( if CodePoint = 0
            then ErrorDesc = null_character
            else ErrorDesc = unescaped_control_character(CodePoint)
            ),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        else
            char_buffer.add(Buffer, Char, !State),
            get_string_chars(Reader, Buffer, Result, !State)
        )
    ;
        ReadResult = eof,
        Msg = "unterminated string literal",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- pred get_escaped_char(json.reader(Stream)::in,
    char_buffer::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_escaped_char(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if escaped_json_char(Char, EscapedChar) then
            char_buffer.add(Buffer, EscapedChar, !State),
            Result = ok
        else if Char = 'u' then
            get_escaped_unicode_char(Reader, Buffer, Result, !State)
        else
            make_error_context(Reader, Context, !State),
            ErrorDesc = invalid_character_escape(Char),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        Msg = "expected escape character or Unicode escape after '\\'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- pred escaped_json_char(char, char).
:- mode escaped_json_char(in, out) is semidet.
:- mode escaped_json_char(out, in) is semidet.

escaped_json_char('"', '"').
escaped_json_char('\\', '\\').
escaped_json_char('/', '/').
escaped_json_char('b', '\b').
escaped_json_char('f', '\f').
escaped_json_char('n', '\n').
escaped_json_char('r', '\r').
escaped_json_char('t', '\t').

:- pred get_escaped_unicode_char(json.reader(Stream)::in,
    char_buffer::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_escaped_unicode_char(Reader, Buffer, Result, !State) :-
    get_hex_digits(Reader, 4, [], HexDigits, HexDigitsResult, !State),
    (
        HexDigitsResult = ok,
        HexString = string.from_char_list(HexDigits),
        ( if
            string.base_string_to_int(16, HexString, UnicodeCharCode)
        then
            CharCodeClass = classify_code_point(UnicodeCharCode),
            (
                CharCodeClass = code_point_valid,
                UnicodeChar = char.det_from_int(UnicodeCharCode),
                char_buffer.add(Buffer, UnicodeChar, !State),
                Result = ok
            ;
                CharCodeClass = code_point_leading_surrogate,
                get_trailing_surrogate_and_combine(Reader, HexString,
                    UnicodeCharCode, Buffer, Result, !State)
            ;
                CharCodeClass = code_point_trailing_surrogate,
                make_error_context(Reader, Context0, !State),
                Context = Context0 ^ column_number :=
                    Context0 ^ column_number - 5,
                ErrorDesc = unpaired_trailing_utf16_surrogate(HexString),
                Error = json_error(Context, ErrorDesc),
                Result = error(Error)
            ;
                CharCodeClass = code_point_null_char,
                make_error_context(Reader, Context0, !State),
                % Report the column number for the start of the
                % Unicode escape, not the end.
                Context = Context0 ^ column_number :=
                    Context0 ^ column_number - 5,
                ErrorDesc = null_character,
                Error = json_error(Context, ErrorDesc),
                Result = error(Error)
            ;
                CharCodeClass = code_point_invalid,
                make_error_context(Reader, Context, !State),
                ErrorDesc = invalid_unicode_character(HexString),
                Error = json_error(Context, ErrorDesc),
                Result = error(Error)
            )
        else
            make_error_context(Reader, Context, !State),
            ErrorDesc = invalid_unicode_character(HexString),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        HexDigitsResult = error(Error),
        Result = error(Error)
    ).

:- pred get_hex_digits(json.reader(Stream)::in, int::in,
    list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_hex_digits(Reader, !.N, !HexDigits, Result, !State) :-
    ( if !.N > 0 then
        Stream = Reader ^ json_reader_stream,
        stream.get(Stream, ReadResult, !State),
        (
            ReadResult = ok(Char),
            update_column_number(Reader, Char, !State),
            ( if char.is_hex_digit(Char) then
                !:HexDigits = [Char | !.HexDigits],
                !:N = !.N - 1,
                get_hex_digits(Reader, !.N, !HexDigits, Result, !State)
            else
                make_error_context(Reader, Context, !State),
                Error = json_error(Context,
                    illegal_unicode_escape_character(Char)),
                Result = error(Error)
            )
        ;
            ReadResult = eof,
            % XXX it's also an unterminated string literal -- what's
            % the more useful error message?
            Msg = "incomplete Unicode escape",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(Error)
        ;
            ReadResult = error(StreamError),
            Result = error(stream_error(StreamError))
        )
    else
        list.reverse(!HexDigits),
        Result = ok
    ).

:- type code_point_class
    --->    code_point_valid
    ;       code_point_invalid
    ;       code_point_leading_surrogate
    ;       code_point_trailing_surrogate
    ;       code_point_null_char.

:- func classify_code_point(int) = code_point_class.

classify_code_point(Code) =
    ( if is_leading_surrogate(Code) then
        code_point_leading_surrogate
    else if is_trailing_surrogate(Code) then
        code_point_trailing_surrogate
    else if Code > 0, Code =< 0x10FFFF then
        code_point_valid
    else if Code = 0 then
        code_point_null_char
    else
        code_point_invalid
    ).

:- pred is_leading_surrogate(int::in) is semidet.

is_leading_surrogate(Code) :-
    Code >= 0xD800,
    Code =< 0xDBFF.

:- pred is_trailing_surrogate(int::in) is semidet.

is_trailing_surrogate(Code) :-
    Code >= 0xDC00,
    Code =< 0xDFFF.

:- pred get_trailing_surrogate_and_combine(json.reader(Stream)::in,
    string::in, int::in, char_buffer::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_trailing_surrogate_and_combine(Reader, LeadingSurrogateStr,
        LeadingSurrogate, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if Char = ('\\') then
            stream.get(Stream, ReadResultPrime, !State),
            (
                ReadResultPrime = ok(CharPrime),
                update_column_number(Reader, CharPrime, !State),
                ( if CharPrime = 'u' then
                    get_hex_digits(Reader, 4, [], HexDigits, HexDigitsResult,
                        !State),
                    (
                        HexDigitsResult = ok,
                        HexString = string.from_char_list(HexDigits),
                        TrailingSurrogate = string.det_base_string_to_int(16,
                            HexString),
                        ( if
                            is_trailing_surrogate(TrailingSurrogate)
                        then
                            CharCode = combine_utf16_surrogates(
                                LeadingSurrogate, TrailingSurrogate),
                            UnicodeChar = char.det_from_int(CharCode),
                            char_buffer.add(Buffer, UnicodeChar, !State),
                            Result = ok
                        else
                            make_error_context(Reader, Context0, !State),
                            Context = Context0 ^ column_number :=
                                Context0 ^ column_number - 5,
                            ErrorDesc = invalid_trailing_utf16_surrogate(HexString),
                            Error = json_error(Context, ErrorDesc),
                            Result = error(Error)
                        )
                    ;
                        HexDigitsResult = error(Error),
                        Result = error(Error)
                    )
                else
                    make_error_context(Reader, Context, !State),
                    ErrorDesc = invalid_character_escape(Char),
                    Error = json_error(Context, ErrorDesc),
                    Result = error(Error)
                )
            ;
                ReadResultPrime = eof,
                make_error_context(Reader, Context, !State),
                ErrorDesc = invalid_character_escape(Char),
                Error = json_error(Context, ErrorDesc),
                Result = error(Error)
            ;
                ReadResultPrime = error(StreamError),
                Result = error(stream_error(StreamError))
            )
        else
            make_error_context(Reader, Context0, !State),
            Context = Context0 ^ column_number := Context0 ^ column_number - 6,
            ErrorDesc = unpaired_leading_utf16_surrogate(LeadingSurrogateStr),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_error_context(Reader, Context, !State),
        ErrorDesc = unpaired_leading_utf16_surrogate(LeadingSurrogateStr),
        Error = json_error(Context, ErrorDesc),
        Result = error(Error)
    ;
        ReadResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- func combine_utf16_surrogates(int, int) = int.

combine_utf16_surrogates(Lead, Tail) =
    (((Lead - 0xd800) << 10) \/ (Tail - 0xdc00)) + 0x10000.

%-----------------------------------------------------------------------------%
%
% Numeric literals.
%

:- pred get_negative_number(json.reader(Stream)::in, char_buffer::in,
    token(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_negative_number(Reader, Buffer, Token, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if char.is_digit(Char) then
            char_buffer.add(Buffer, Char, !State),
            get_number(Reader, Buffer, Token, !State)
        else if
            Char = ('I'),
            Reader ^ json_infinities = allow_infinities
        then
            char_buffer.add(Buffer, Char, !State),
            get_infinity(Reader, Buffer, Token, !State)
        else
            make_error_context(Reader, Context, !State),
            Error = json_error(Context, illegal_negation(Char)),
            Token = token_error(Error)
        )
    ;
        GetResult = eof,
        Msg = "expected decimal digit after '-'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Token = token_error(Error)
    ;
        GetResult = error(StreamError),
        Token = token_error(stream_error(StreamError))
    ).

:- pred get_number(json.reader(Stream)::in, char_buffer::in,
    token(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_number(Reader, Buffer, Token, !State) :-
    get_number_chars(Reader, Buffer, DigitsResult, !State),
    (
        DigitsResult = ok,
        Number = char_buffer.det_to_float(Buffer, !.State),
        ( if is_inf(Number) then
            NumberStr = char_buffer.to_string(Buffer, !.State),
            string.count_codepoints(NumberStr, NumCodePoints),
            make_error_context(Reader, Context0, !State),
            % Adjust the column number so we are pointing to the beginning
            % of the number.
            Context = Context0 ^ column_number :=
                Context0 ^ column_number - NumCodePoints,
            Error = json_error(Context, non_finite_number(NumberStr)),
            Token = token_error(Error)
        else
            Token = token_number(Number)
        )
    ;
        DigitsResult = error(Error),
        Token = token_error(Error)
    ).

:- pred get_number_chars(json.reader(Stream)::in,
    char_buffer::in, json.res(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_number_chars(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            char.is_digit(Char)
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_number_chars(Reader, Buffer, Result, !State)
        else if
            Char = ('.'),
            char_buffer.last(Buffer, LastChar, !.State),
            char.is_digit(LastChar)
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_frac(Reader, frac_start, Buffer, Result, !State)
        else if
            ( Char = 'e'
            ; Char = 'E'
            )
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_start, Buffer, Result, !State)
        else
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        GetResult = eof,
        ( if
            char_buffer.last(Buffer, LastChar, !.State),
            char.is_digit(LastChar)
        then
            Result = ok
        else
            Msg = "expected '.', 'e', 'E' or decimal digit",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(Error)
        )
    ;
        GetResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- type frac_where
    --->    frac_start  % Have just seen '.'.
    ;       frac_digit. % Have just seen a digit.

:- pred get_frac(json.reader(Stream)::in,
    frac_where::in, char_buffer::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_frac(Reader, Where, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            char.is_digit(Char)
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_frac(Reader, frac_digit, Buffer, Result, !State)
        else if
            ( Char = 'e'
            ; Char = 'E'
            ),
            % There must be a least on digit before the exponent
            % indicator.  We cannot have literals of the form "10.e30".
            Where = frac_digit
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_start, Buffer, Result, !State)
        else
            (
                Where = frac_start,
                Msg = "expected decimal digit after '.'",
                make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                Result = error(Error)
            ;
                Where = frac_digit,
                stream.unget(Stream, Char, !State),
                Result = ok
            )
        )
    ;
        GetResult = eof,
        (
            Where = frac_digit,
            Result = ok
        ;
            Where = frac_start,
            Msg = "expected decimal digit after '.'",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(Error)
        )
    ;
        GetResult = error(StreamError),
        Error = stream_error(StreamError),
        Result = error(Error)
    ).

:- type exp_where
    --->    exp_start    % Have just seen 'e' or 'E'.
    ;       exp_sign     % Have just seen '-' or '+'.
    ;       exp_digit.   % Have just seen a digit.

:- pred get_exp(json.reader(Stream)::in, exp_where::in,
    char_buffer::in, json.res(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_exp(Reader, Where, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            Where = exp_start,
            ( Char = ('-')
            ; Char = ('+')
            )
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_sign, Buffer, Result, !State)
        else if
            char.is_digit(Char)
        then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_digit, Buffer, Result, !State)
        else
            (
                Where = exp_digit,
                stream.unget(Stream, Char, !State),
                Result = ok
            ;
                Where = exp_sign,
                update_column_number(Reader, Char, !State),
                ( if char_buffer.last(Buffer, SignChar, !.State) then
                    make_error_context(Reader, Context, !State),
                    Error = json_error(Context,
                        bad_signed_exponent(SignChar, Char)),
                    Result = error(Error)
                else
                    unexpected($module, $pred, "corrupted buffer (1)")
                )
            ;
                Where = exp_start,
                update_column_number(Reader, Char, !State),
                ( if char_buffer.last(Buffer, ExpChar, !.State) then
                    make_error_context(Reader, Context, !State),
                    Error = json_error(Context, bad_exponent(ExpChar, Char)),
                    Result = error(Error)
                else
                    unexpected($module, $pred, "corrupted buffer (2)")
                )
            )
        )
    ;
        GetResult = eof,
        (
            (
                Where = exp_start,
                Msg = "expected a digit, '+', or '-'"
            ;
                Where = exp_sign,
                Msg = "expected a digit"
            ),
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(Error)
        ;
            Where = exp_digit,
            Result = ok
        )
    ;
        GetResult = error(StreamError),
        Error = stream_error(StreamError),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred get_infinity(json.reader(Stream)::in, char_buffer::in,
    token(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_infinity(Reader, Buffer, Token, !State) :-
    get_keyword_chars(Reader, Buffer, Result, !State),
    (
        Result = ok,
        InfinityStr = char_buffer.to_string(Buffer, !.State),
        ( if InfinityStr = "Infinity" then
            Token = token_number(positive_infinity)
        else if InfinityStr = "-Infinity" then
            Token = token_number(-positive_infinity)
        else
            make_syntax_error(Reader, InfinityStr, no, Error, !State),
            Token = token_error(Error)
        )
    ;
        Result = error(Error),
        Token = token_error(Error)
    ).

    % XXX POST 14.01 -- for compatibility with Mercury 14.01.1 we get infinity
    % this way.  With later versions we would just use float.infinity/0.
    %
:- func positive_infinity = float.

positive_infinity = float.max + float.max.

%-----------------------------------------------------------------------------%

:- pred get_keyword(json.reader(Stream)::in,
    char_buffer::in, token(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_keyword(Reader, Buffer, Token, !State) :-
    get_keyword_chars(Reader, Buffer, Result, !State),
    (
        Result = ok,
        Keyword = char_buffer.to_string(Buffer, !.State),
        ( if is_keyword(Keyword, Token0) then
            Token = Token0
        else
            make_syntax_error(Reader, Keyword, no, Error, !State),
            Token = token_error(Error)
        )
    ;
        Result = error(Error),
        Token = token_error(Error)
    ).

:- pred get_keyword_chars(json.reader(Stream)::in, char_buffer::in,
    stream.res(json.error(Error))::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_keyword_chars(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        ( if char.is_lower(Char) then
            update_column_number(Reader, Char, !State),
            char_buffer.add(Buffer, Char, !State),
            get_keyword_chars(Reader, Buffer, Result, !State)
        else if char.to_int(Char, 0) then
            update_column_number(Reader, Char, !State),
            make_error_context(Reader, Context, !State),
            Error = json_error(Context, null_character),
            Result = error(Error)
        else
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        ReadResult = eof,
        Result = ok
    ;
        ReadResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- pred is_keyword(string::in, token(Error)::out) is semidet.

is_keyword("true", token_true).
is_keyword("false", token_false).
is_keyword("null", token_null).

%-----------------------------------------------------------------------------%

:- pred consume_comment(json.reader(Stream)::in, json.context::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_comment(Reader, StartCommentContext, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if Char = ('/') then
            consume_until_next_nl_or_eof(Reader, Result, !State)
        else if Char = ('*') then
            % The last char kind must be 'other' here since we do not
            % want to accept "/*/" as a multiline comment.
            LastCharKind = char_other,
            consume_multiline_comment(Reader, StartCommentContext,
                LastCharKind, Result, !State)
        else
            make_error_context(Reader, Context, !State),
            Error = json_error(Context, illegal_comment_start(Char)),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- pred consume_until_next_nl_or_eof(json.reader(Stream)::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_until_next_nl_or_eof(Reader, Result, !State) :-
    stream.get(Reader ^ json_reader_stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if Char = ('\n')
        then Result = ok
        else consume_until_next_nl_or_eof(Reader, Result, !State)
        )
    ;
        ReadResult = eof,
        Result = ok
    ;
        ReadResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- type last_multiline_comment_char
    --->    char_star
    ;       char_other.

:- pred consume_multiline_comment(json.reader(Stream)::in,
    json.context::in, last_multiline_comment_char::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_multiline_comment(Reader, StartCommentContext, LastCharKind, Result,
        !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if
            LastCharKind = char_star,
            Char = ('/')
        then
            Result = ok
        else
            ThisCharKind = ( if Char = ('*') then char_star else char_other ),
            consume_multiline_comment(Reader, StartCommentContext,
                ThisCharKind, Result, !State)
        )
    ;
        ReadResult = eof,
        Error = json_error(StartCommentContext,
            unterminated_multiline_comment),
        Result = error(Error)
    ;
        ReadResult = error(Error),
        Result = error(stream_error(Error))
    ).

%-----------------------------------------------------------------------------%

token_to_string(token_left_curly_bracket) = "{".
token_to_string(token_right_curly_bracket) = "}".
token_to_string(token_left_square_bracket) = "[".
token_to_string(token_right_square_bracket) = "]".
token_to_string(token_comma) = ",".
token_to_string(token_colon) = ":".
token_to_string(token_string(String)) =
    "\"" ++ escape_json_string(String) ++ "\"".
token_to_string(token_number(Float)) =
    string.from_float(Float).
token_to_string(token_false) = "false".
token_to_string(token_true) = "true".
token_to_string(token_null) = "null".
token_to_string(token_eof) = "end-of-file".
token_to_string(token_error(_)) = _ :-
    unexpected($module, $pred, "error token encountered").

%-----------------------------------------------------------------------------%

escape_json_string(String) = EscapedString :-
    string.foldl(do_char_escape, String, [], RevChars),
    EscapedString = string.from_rev_char_list(RevChars).

:- pred do_char_escape(char::in, list(char)::in, list(char)::out) is det.

do_char_escape(Char, !RevChars) :-
    ( if escaped_json_char(Escape, Char) then   % Reverse mode.
        !:RevChars = [Escape, '\\'| !.RevChars]
    else if
        char.to_int(Char, CodePoint),
        CodePoint >= 0x0000, CodePoint =< 0x001F
    then
       ( if CodePoint < 0x0010 then
            SecondLastDigit = '0',
            CodePointPrime = CodePoint
        else
            SecondLastDigit = '1',
            CodePointPrime = CodePoint - 16
        ),
        ( if char.int_to_hex_char(CodePointPrime, LastDigit) then
            !:RevChars =
                [LastDigit, SecondLastDigit, '0', '0', 'u', '\\' | !.RevChars]
        else
            % This should not happen as we have checked that the digit is
            % in range above.
            unexpected($file, $pred,
                "cannot escape code point: " ++ int_to_string(CodePoint))
        )
    else
        !:RevChars = [Char | !.RevChars]
    ).

%-----------------------------------------------------------------------------%
:- end_module json.json_lexer.
%-----------------------------------------------------------------------------%

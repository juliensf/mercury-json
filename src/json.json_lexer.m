%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013 Julien Fischer.
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
            char_buffer.init(Buffer, !State),
            char_buffer.add(Buffer, Char, !State),
            get_negative_number(Reader, Buffer, Token, !State)
        else if
            char.is_digit(Char)
        then
            char_buffer.init(Buffer, !State),
            char_buffer.add(Buffer, Char, !State),
            get_number(Reader, Buffer, Token, !State)
        else if
            ( Char = 'n'
            ; Char = 't'
            ; Char = 'f'
            )
        then
            char_buffer.init(Buffer, !State),
            char_buffer.add(Buffer, Char, !State),
            get_keyword(Reader, Buffer, Token, !State)
        else
            string.format("unexpected character '%c'", [c(Char)], Msg),
            make_json_error(Reader, Msg, Error, !State),
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
    get_string_chars(Reader, [], RevChars, Result, !State),
    (
        Result = ok,
        String = string.from_rev_char_list(RevChars),
        Token = token_string(String)
    ;
        Result = error(Error),
        Token = token_error(Error)
    ).

:- pred get_string_chars(json.reader(Stream)::in,
    list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_string_chars(Reader, !Chars, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if Char = ('\\') then
            get_escaped_char(Reader, !Chars, EscapedCharResult, !State),
            (
                EscapedCharResult = ok,
                get_string_chars(Reader, !Chars, Result, !State)
            ;
                EscapedCharResult = error(Error),
                Result = error(Error)
            )
        else if Char = '"' then
            Result = ok
        else
            !:Chars = [Char | !.Chars],
            get_string_chars(Reader, !Chars, Result, !State)
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
    list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_escaped_char(Reader, !Chars, Result, !State) :-
    Stream = Reader ^ json_reader_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        update_column_number(Reader, Char, !State),
        ( if escaped_json_char(Char, EscapedChar) then
            !:Chars = [EscapedChar | !.Chars],
            Result = ok
        else if Char = 'u' then
            get_escaped_unicode_char(Reader, !Chars, Result, !State)
        else
            make_error_context(Reader, Context, !State),
            ErrorDesc = invalid_character_escape(Char),
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- pred escaped_json_char(char::in, char::out) is semidet.

escaped_json_char('"', '"').
escaped_json_char('\\', '\\').
escaped_json_char('/', '/').
escaped_json_char('b', '\b').
escaped_json_char('f', '\f').
escaped_json_char('n', '\n').
escaped_json_char('r', '\r').
escaped_json_char('t', '\t').

:- pred get_escaped_unicode_char(json.reader(Stream)::in,
    list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_escaped_unicode_char(Reader, !Chars, Result, !State) :-
    get_hex_digits(Reader, 4, [], HexDigits, HexDigitsResult, !State),
    (
        HexDigitsResult = ok,
        HexString = string.from_char_list(HexDigits),
        ( if
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            UnicodeCharCode \= 0    % Do not allow the null character.
        then
            CharCodeClass = classify_code_point(UnicodeCharCode),
            (
                CharCodeClass = code_point_valid,
                UnicodeChar = char.det_from_int(UnicodeCharCode),
                !:Chars = [UnicodeChar | !.Chars],
                Result = ok
            ;
                CharCodeClass = code_point_leading_surrogate,
                get_trailing_surrogate_and_combine(Reader, UnicodeCharCode,
                    !Chars, Result, !State)
            ;
                CharCodeClass = code_point_trailing_surrogate,
                unexpected($file, $pred, "unpaired trailing surrogate")
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
                string.format("invalid hex character in Unicode escape: '%c'",
                    [c(Char)], Msg),
                make_json_error(Reader, Msg, Error, !State),
                Result = error(Error)
            )
        ;
            ReadResult = eof,
            make_unexpected_eof_error(Reader, no, Error, !State),
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
    ;       code_point_trailing_surrogate.

:- func classify_code_point(int) = code_point_class.

classify_code_point(Code) =
    ( if is_leading_surrogate(Code) then
        code_point_leading_surrogate
    else if is_trailing_surrogate(Code) then
        code_point_trailing_surrogate
    else if  Code > 0, Code =< 0x10FFFF then
        code_point_valid
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
    int::in, list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_trailing_surrogate_and_combine(Reader, LeadingSurrogate,
        !Chars, Result, !State) :-
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
                        ( if
                            string.base_string_to_int(16, HexString,
                                TrailingSurrogate),
                            is_trailing_surrogate(TrailingSurrogate)
                        then
                            CharCode = combine_utf16_surrogates(
                                LeadingSurrogate, TrailingSurrogate),
                            UnicodeChar = char.det_from_int(CharCode),
                            !:Chars = [UnicodeChar | !.Chars],
                            Result = ok
                        else
                            unexpected($file, $pred, "not a trailing surrogate")
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
            make_error_context(Reader, Context, !State),
            ErrorDesc = unpaired_utf16_surrogate,
            Error = json_error(Context, ErrorDesc),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_error_context(Reader, Context, !State),
        ErrorDesc = unpaired_utf16_surrogate,
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
        else
            Msg = "expected a digit after '-'",
            make_json_error(Reader, Msg, Error, !State),
            Token = token_error(Error)
        )
    ;
        GetResult = eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
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
        NumberStr = char_buffer.to_string(Buffer, !.State),
        Number = string.det_to_float(NumberStr),
        ( if is_inf(Number) then
            Msg = "number is not finite",
            % XXX the context here is the end of the number.
            % Using the start would probably be better.
            make_json_error(Reader, Msg, Error, !State),
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
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(Error)
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
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        GetResult = eof,
        (
            Where = frac_digit,
            Result = ok
        ;
            Where = frac_start,
            make_unexpected_eof_error(Reader, no, Error, !State),
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
                    string.format("expected digit after '%c' in exponent",
                        [c(SignChar)], Msg),
                    make_json_error(Reader, Msg, Error, !State),
                    Result = error(Error)
                else
                    unexpected($module, $pred, "corrupted buffer")
                )
            ;
                Where = exp_start,
                update_column_number(Reader, Char, !State),
                ( if char_buffer.last(Buffer, ExpChar, !.State) then
                    string.format(
                        "expected '+', '-' or digit after '%c' in exponent",
                        [c(ExpChar)], Msg),
                    make_json_error(Reader, Msg, Error, !State),
                    Result = error(Error)
                else
                    unexpected($module, $pred, "corrupted buffer")
                )
            )
        )
    ;
        GetResult = eof,
        (
            ( Where = exp_start
            ; Where = exp_sign
            ),
            make_unexpected_eof_error(Reader, no, Error, !State),
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
        Result = error(StreamError),
        Error = stream_error(StreamError),
        Token = token_error(Error)
    ).

:- pred get_keyword_chars(json.reader(Stream)::in, char_buffer::in,
    stream.res(Error)::out, State::di, State::uo) is det
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
        else
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        ReadResult = eof,
        Result = ok
    ;
        ReadResult = error(Error),
        Result = error(Error)
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
            % The last char kind must be other here since we don't
            % want to accept /*/ as multiline comment.
            LastCharKind = char_other,
            consume_multiline_comment(Reader, StartCommentContext,
                LastCharKind, Result, !State)
        else
            string.format("unexpected character: '%c'", [c(Char)], Msg),
            make_json_error(Reader, Msg, Error, !State),
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
    "\"" ++ String ++ "\"". % XXX Should escape special chars.
token_to_string(token_number(Float)) =
    string.from_float(Float).
token_to_string(token_false) = "false".
token_to_string(token_true) = "true".
token_to_string(token_null) = "null".
token_to_string(token_eof) = "end-of-file".
token_to_string(token_error(_)) = _ :-
    unexpected($module, $pred, "error token encountered").

%-----------------------------------------------------------------------------%
:- end_module json.json_lexer.
%-----------------------------------------------------------------------------%

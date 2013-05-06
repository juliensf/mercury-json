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
    stream.get(Reader ^ json_stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
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
            consume_comment(Reader, CommentResult, !State),
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
            get_string_literal(Reader ^ json_stream, Token, !State)
        else if
            ( Char = ('-')
            ; char.is_digit(Char)
            )
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
            make_json_error(Reader ^ json_stream, Msg, Error, !State),
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
% String literals.
%

:- pred get_string_literal(Stream::in, token(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_string_literal(Stream, Token, !State) :-
    get_string_chars(Stream, [], RevChars, Result, !State),
    (
        Result = ok,
        String = string.from_rev_char_list(RevChars),   
        Token = token_string(String)
    ;
        Result = error(Error),
        Token = token_error(Error)
    ).

:- pred get_string_chars(Stream::in, list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_string_chars(Stream, !Chars, Result, !State) :-
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        ( if Char = ('\\') then
            get_escaped_char(Stream, !Chars, EscapedCharResult, !State),
            (
                EscapedCharResult = ok,
                get_string_chars(Stream, !Chars, Result, !State)
            ;
                EscapedCharResult = error(Error),
                Result = error(Error)
            )
        else if Char = '"' then
            Result = ok
        else
            !:Chars = [Char | !.Chars],
            get_string_chars(Stream, !Chars, Result, !State)
        )
    ;
        ReadResult = eof,
        Msg = "unterminated string literal",
        make_unexpected_eof_error(Stream, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- pred get_escaped_char(Stream::in, list(char)::in, list(char)::out,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_escaped_char(Stream, !Chars, Result, !State) :-
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        ( if escaped_json_char(Char, EscapedChar) then
            !:Chars = [EscapedChar | !.Chars],
            Result = ok
        else if Char = 'u' then
            unexpected($module, $pred, "unicode escapes NYI")
        else
            make_error_context(Stream, Context, !State),
            ErrorDesc = invalid_character_escape(Char),
            Error = json_error(Context, ErrorDesc),  
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_unexpected_eof_error(Stream, no, Error, !State),
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

%-----------------------------------------------------------------------------%
%
% Numeric literals.
%

:- pred get_number(json.reader(Stream)::in, char_buffer::in,
    token(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_number(Stream, Buffer, Token, !State) :-
    get_int(Stream, Buffer, DigitsResult, !State),
    (
        DigitsResult = ok,
        NumberStr = char_buffer.to_string(Buffer, !.State),
        Number = string.det_to_float(NumberStr),
        Token = token_number(Number)
    ;
        DigitsResult = error(Error),
        Token = token_error(Error)
    ).

:- pred get_int(json.reader(Stream)::in,
    char_buffer::in, json.res(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_int(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            char.is_digit(Char)
        then
            char_buffer.add(Buffer, Char, !State),
            get_int(Reader, Buffer, Result, !State)
        else if
            Char = ('.'),
            char_buffer.last(Buffer, LastChar, !.State),
            char.is_digit(LastChar)
        then
            char_buffer.add(Buffer, Char, !State),
            get_frac(Reader, Buffer, Result, !State)
        else if
            ( Char = 'e'
            ; Char = 'E'
            )
        then
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_start, Buffer, Result, !State)
        else
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        GetResult = eof,
        make_unexpected_eof_error(Stream, no, Error, !State),
        Result = error(Error)
    ;
        GetResult = error(StreamError),
        Result = error(stream_error(StreamError))
    ).

:- pred get_frac(json.reader(Stream)::in,
    char_buffer::in, json.res(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

get_frac(Reader, Buffer, Result, !State) :-
    Stream = Reader ^ json_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            char.is_digit(Char)
        then
            char_buffer.add(Buffer, Char, !State),
            get_frac(Reader, Buffer, Result, !State)
        else
            stream.unget(Stream, Char, !State),
            Result = ok
        )
    ;
        GetResult = eof,
        make_unexpected_eof_error(Stream, no, Error, !State),
        Result = error(Error)
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
    Stream = Reader ^ json_stream,
    stream.get(Stream, GetResult, !State),
    (
        GetResult = ok(Char),
        ( if
            Where = exp_start,
            ( Char = ('-')
            ; Char = ('+')
            )
        then
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_sign, Buffer, Result, !State)
        else if
            char.is_digit(Char)
        then
            char_buffer.add(Buffer, Char, !State),
            get_exp(Reader, exp_digit, Buffer, Result, !State)
        else
            (
                Where = exp_digit,
                stream.unget(Stream, Char, !State),
                Result = ok
            ;
                Where = exp_sign,
                ( if char_buffer.last(Buffer, SignChar, !.State) then
                    string.format("expected digit after '%c' in exponent",
                        [c(SignChar)], Msg),
                    make_json_error(Stream, Msg, Error, !State),
                    Result = error(Error)
                else 
                    unexpected($module, $pred, "corrupted buffer")
                )
            ;
                Where = exp_start,
                ( if char_buffer.last(Buffer, ExpChar, !.State) then
                    string.format(
                        "expected '+', '-' or digit after '%c' in exponent",
                        [c(ExpChar)], Msg),
                    make_json_error(Stream, Msg, Error, !State),
                    Result = error(Error)
                else 
                    unexpected($module, $pred, "corrupted buffer")
                )
            )
        )
    ;
        GetResult = eof,
        make_unexpected_eof_error(Stream, no, Error, !State),
        Result = error(Error)
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
            Stream = Reader ^ json_stream,
            make_syntax_error(Stream, Keyword, no, Error, !State),
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
    Stream = Reader ^ json_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        ( if char.is_lower(Char) then
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

:- pred consume_comment(json.reader(Stream)::in, json.res(Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_comment(Reader, Result, !State) :-
    Stream = Reader ^ json_stream,
    stream.get(Stream, ReadResult, !State),
    (
        ReadResult = ok(Char),
        ( if Char = ('/') then
            consume_until_next_nl_or_eof(Stream, Result, !State)
        else if Char = ('*') then
            % The last char kind must be other here since we don't
            % want to accept /*/ as multiline comment.
            LastCharKind = char_other,
            consume_multiline_comment(Stream, LastCharKind, Result, !State)
        else
            string.format("unexpected character: '%c'", [c(Char)], Msg),
            make_json_error(Stream, Msg, Error, !State),
            Result = error(Error)
        )
    ;
        ReadResult = eof,
        make_unexpected_eof_error(Stream, no, Error, !State),
        Result = error(Error)
    ;
        ReadResult = error(Error),
        Result = error(stream_error(Error))
    ).

:- pred consume_until_next_nl_or_eof(Stream::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_until_next_nl_or_eof(Stream, Result, !State) :-
    stream.get(Stream, ReadResult, !State),
    ( 
        ReadResult = ok(Char),
        ( if Char = ('\n')
        then Result = ok
        else consume_until_next_nl_or_eof(Stream, Result, !State)
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

:- pred consume_multiline_comment(Stream::in, last_multiline_comment_char::in,
    json.res(Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

consume_multiline_comment(Stream, LastCharKind, Result, !State) :-
    stream.get(Stream, ReadResult, !State),
    ( 
        ReadResult = ok(Char),
        ( if 
            LastCharKind = char_star,
            Char = ('/')
        then
            Result = ok
        else 
            ThisCharKind = ( if Char = ('*') then char_star else char_other ),
            consume_multiline_comment(Stream, ThisCharKind, Result, !State)
        )
    ;
        ReadResult = eof,
        % XXX we should record the context of the beginning of the comment.
        make_error_context(Stream, Context, !State),
        Error = json_error(Context, unterminated_multiline_comment),
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

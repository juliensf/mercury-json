%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2016 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%

:- module json.json_parser.
:- interface.

%-----------------------------------------------------------------------------%

:- type nesting_depth == int.

:- pred do_read_value(json.reader(Stream)::in,
    token(Error)::in, json.result(json.value, Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

:- pred do_get_value(json.reader(Stream)::in,
    nesting_depth::in, token(Error)::in,
    json.result(json.value, Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Folding over object members.
%

:- pred do_object_fold(json.reader(Stream), pred(string, json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_object_fold(in, in(pred(in, in, in, out) is det),
    in, out, di, uo) is det.
:- mode do_object_fold(in, in(pred(in, in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

:- pred do_object_fold_state(json.reader(Stream),
    pred(string, json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_object_fold_state(in, in(pred(in, in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode do_object_fold_state(in, in(pred(in, in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

%-----------------------------------------------------------------------------%
%
% Folding over array elements.
%

:- pred do_array_fold(json.reader(Stream), pred(json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_array_fold(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode do_array_fold(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

:- pred do_array_fold_state(json.reader(Stream),
    pred(json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_array_fold_state(in, in(pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode do_array_fold_state(in, in(pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

do_read_value(Reader, Token, Result, !State) :-
    NestingDepth = 0,
    do_get_value(Reader, NestingDepth, Token, ValueResult, !State),
    %
    % We now need to check that there is nothing else (except whitespace and
    % possibly comments) in the stream.
    %
    (
        ValueResult = ok(Value),
        get_token(Reader, NextToken, !State),
        (
            % Nothing left to get.
            NextToken = token_eof,
            Result = ok(Value)
        ;
            NextToken = token_error(Error),
            Result = error(Error)
        ;
            ( NextToken = token_left_curly_bracket
            ; NextToken = token_right_curly_bracket
            ; NextToken = token_left_square_bracket
            ; NextToken = token_right_square_bracket
            ; NextToken = token_comma
            ; NextToken = token_colon
            ; NextToken = token_string(_)
            ; NextToken = token_number(_)
            ; NextToken = token_false
            ; NextToken = token_true
            ; NextToken = token_null
            ),
            NextTokenDesc = token_to_string(NextToken),
            make_error_context(Reader, Context, !State),
            Error = json_error(Context, expected_eof(NextTokenDesc)),
            Result = error(Error)
        )
    ;
        ValueResult = eof,
        Result = eof
    ;
        ValueResult = error(Error),
        Result = error(Error)
    ).

do_get_value(Reader, !.NestingDepth, Token, Result, !State) :-
    (
        Token = token_left_curly_bracket,
        do_get_object(Reader, !.NestingDepth, Result, !State)
    ;
        Token = token_left_square_bracket,
        do_get_array(Reader, !.NestingDepth, Result, !State)
    ;
        Token = token_string(String),
        Result = ok(json.string(String))
    ;
        Token = token_number(Number),
        Result = ok(json.number(Number))
    ;
        Token = token_false,
        Result = ok(json.bool(no))
    ;
        Token = token_true,
        Result = ok(json.bool(yes))
    ;
        Token = token_null,
        Result = ok(json.null)
    ;
        (
            Token = token_right_curly_bracket,
            ErrorChar = ('}')
        ;
            Token = token_right_square_bracket,
            ErrorChar = (']')
        ;
            Token = token_comma,
            ErrorChar = (',')
        ;
            Token = token_colon,
            ErrorChar = (':')
        ),
        make_error_context(Reader, Context, !State),
        Error = json_error(Context, illegal_start_character(ErrorChar)),
        Result = error(Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(Error)
    ;
        Token = token_error(TokenError),
        Result = error(TokenError)
    ).

%-----------------------------------------------------------------------------%
%
% Parse objects.
%

:- pred do_get_object(json.reader(Stream)::in,
    nesting_depth::in,
    json.result(json.value, Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

do_get_object(Reader, !.NestingDepth, Result, !State) :-
    !:NestingDepth = !.NestingDepth + 1,
    ( if below_nesting_depth_limit(Reader, !.NestingDepth) then
        do_get_members(Reader, !.NestingDepth, at_start, map.init, MaybeMembers,
            !State),
        (
            MaybeMembers = ok(Members),
            Object = object(Members),
            Result = ok(Object)
        ;
            MaybeMembers = eof,
            Result = eof
        ;
            MaybeMembers = error(Error),
            Result = error(Error)
        )
    else
        make_error_context(Reader, Context, !State),
        ErrorDesc = maximum_nesting_depth_reached,
        Error = json_error(Context, ErrorDesc),
        Result = error(Error)
    ).

:- type object_where
    --->    at_start        % We have just seen '{'.
    ;       after_comma.    % We have just seen ",".

:- pred do_get_members(json.reader(Stream)::in, nesting_depth::in,
    object_where::in,
    map(string, json.value)::in,
    json.result(map(string, json.value), Error)::out,
    State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

do_get_members(Reader, !.NestingDepth, Where, !.Members, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_curly_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Members)
        else
            TokenDesc = token_to_string(Token),
            Msg = "expected a string literal",
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(Error)
        )
    ;
        Token = token_string(FieldName),
        get_token(Reader, ColonToken, !State),
        (
            ColonToken = token_colon,
            get_token(Reader, NextToken, !State),
            (
                ( NextToken = token_left_curly_bracket
                ; NextToken = token_left_square_bracket
                ; NextToken = token_string(_)
                ; NextToken = token_number(_)
                ; NextToken = token_false
                ; NextToken = token_true
                ; NextToken = token_null
                ),
                do_get_value(Reader, !.NestingDepth, NextToken, ValueResult, !State),
                (
                    ValueResult = ok(Value),
                    RepeatedMembers = Reader ^ json_repeated_members,
                    (
                        RepeatedMembers = do_not_allow_repeated_members,
                        ( if map.insert(FieldName, Value, !Members)
                        then RepeatedMemberError = no
                        else RepeatedMemberError = yes
                        )
                    ;
                        RepeatedMembers = allow_repeated_members_keep_first,
                        map.search_insert(FieldName, Value, _, !Members),
                        RepeatedMemberError = no
                    ;
                        RepeatedMembers = allow_repeated_members_keep_last,
                        map.set(FieldName, Value, !Members),
                        RepeatedMemberError = no
                    ),
                    (
                        RepeatedMemberError = no,
                        get_token(Reader, NextNextToken, !State),
                        (
                            NextNextToken = token_right_curly_bracket,
                            Result = ok(!.Members)
                        ;
                            NextNextToken = token_comma,
                            do_get_members(Reader, !.NestingDepth, after_comma,
                                !.Members, Result, !State)
                        ;
                            ( NextNextToken = token_left_curly_bracket
                            ; NextNextToken = token_left_square_bracket
                            ; NextNextToken = token_right_square_bracket
                            ; NextNextToken = token_colon
                            ; NextNextToken = token_string(_)
                            ; NextNextToken = token_number(_)
                            ; NextNextToken = token_false
                            ; NextNextToken = token_true
                            ; NextNextToken = token_null
                            ),
                            Msg = "expected '}' or ','",
                            NextNextTokenDesc = token_to_string(NextNextToken),
                            make_syntax_error(Reader, NextNextTokenDesc,
                                yes(Msg), Error, !State),
                            Result = error(Error)
                        ;
                            NextNextToken = token_eof,
                            Msg = "expected '}' or ','",
                            make_unexpected_eof_error(Reader, yes(Msg),
                                Error, !State),
                            Result = error(Error)
                        ;
                            NextNextToken = token_error(TokenError),
                            Result = error(TokenError)
                        )
                    ;
                        RepeatedMemberError = yes,
                        make_error_context(Reader, Context, !State),
                        ErrorDesc = duplicate_object_member(FieldName),
                        Error = json_error(Context, ErrorDesc),
                        Result = error(Error)
                    )
                ;
                    ValueResult = eof,
                    Result = eof
                ;
                    ValueResult = error(Error),
                    Result = error(Error)
                )
            ;
                NextToken = token_right_curly_bracket,
                EscapedFieldName = escape_json_string(FieldName),
                string.format("missing value for object member \"%s\"",
                    [s(EscapedFieldName)], Msg),
                NextTokenDesc = token_to_string(NextToken),
                make_syntax_error(Reader, NextTokenDesc, yes(Msg), Error,
                    !State),
                Result = error(Error)
            ;
                NextToken = token_colon,
                NextTokenDesc = token_to_string(NextToken),
                Msg = "multiple colons after object member name",
                make_syntax_error(Reader, NextTokenDesc, yes(Msg), Error,
                    !State),
                Result = error(Error)
            ;
                ( NextToken = token_right_square_bracket
                ; NextToken = token_comma
                ),
                NextTokenDesc = token_to_string(NextToken),
                Msg = "expected a value after ':'",
                make_syntax_error(Reader, NextTokenDesc, yes(Msg), Error,
                    !State),
                Result = error(Error)
            ;
                NextToken = token_eof,
                Msg = "expected a value after ':'",
                make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                Result = error(Error)
            ;
                NextToken = token_error(Error),
                Result = error(Error)
            )
        ;
            ( ColonToken = token_left_curly_bracket
            ; ColonToken = token_right_curly_bracket
            ; ColonToken = token_left_square_bracket
            ; ColonToken = token_right_square_bracket
            ; ColonToken = token_comma
            ; ColonToken = token_string(_)
            ; ColonToken = token_number(_)
            ; ColonToken = token_false
            ; ColonToken = token_true
            ; ColonToken = token_null
            ),
            ColonTokenDesc = token_to_string(ColonToken),
            Msg = "expected ':' after object member name",
            make_syntax_error(Reader, ColonTokenDesc, yes(Msg), Error, !State),
            Result = error(Error)
        ;
            ColonToken = token_eof,
            Msg = "expected ':' after object member name",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(Error)
        ;
            ColonToken = token_error(Error),
            Result = error(Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "object member name not a string",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        Token = token_eof,
        Msg = "object missing terminating '}'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        Token = token_error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------------%
%
% Parse arrays.
%

:- pred do_get_array(json.reader(Stream)::in, nesting_depth::in,
    json.result(json.value, Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

do_get_array(Reader, !.NestingDepth, Result, !State) :-
    !:NestingDepth = !.NestingDepth + 1,
    ( if below_nesting_depth_limit(Reader, !.NestingDepth) then
        do_get_array_items(Reader, !.NestingDepth, at_start, [], MaybeRevItems,
            !State),
        (
            MaybeRevItems = ok(RevItems),
            list.reverse(RevItems, Items),
            Result = ok(json.array(Items))
        ;
            MaybeRevItems = eof,
            Result = eof
        ;
            MaybeRevItems = error(Error),
            Result = error(Error)
        )
    else
        make_error_context(Reader, Context, !State),
        ErrorDesc = maximum_nesting_depth_reached,
        Error = json_error(Context, ErrorDesc),
        Result = error(Error)
    ).

:- type array_where
    --->    at_start        % We have just seen '['.
    ;       after_comma.    % We have just seen ",".

:- pred do_get_array_items(json.reader(Stream)::in, nesting_depth::in,
    array_where::in, list(json.value)::in,
    json.result(list(json.value), Error)::out, State::di, State::uo) is det
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).

do_get_array_items(Reader, !.NestingDepth, Where, !.Items, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_square_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Items)
        else
            TokenDesc = token_to_string(Token),
            Msg = "expected a value",
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        do_get_value(Reader, !.NestingDepth, Token, ItemResult, !State),
        (
            ItemResult = ok(Item),
            !:Items = [Item | !.Items],
            get_token(Reader, NextToken, !State),
            (
                NextToken = token_right_square_bracket,
                Result = ok(!.Items)
            ;
                NextToken = token_comma,
                do_get_array_items(Reader, !.NestingDepth, after_comma, !.Items,
                    Result, !State)
            ;
                ( NextToken = token_left_curly_bracket
                ; NextToken = token_left_square_bracket
                ; NextToken = token_string(_)
                ; NextToken = token_number(_)
                ; NextToken = token_false
                ; NextToken = token_true
                ; NextToken = token_null
                ; NextToken = token_right_curly_bracket
                ; NextToken = token_colon
                ),
                TokenDesc = token_to_string(NextToken),
                Msg = "expected ']' or ','",
                make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
                Result = error(Error)
            ;
                NextToken = token_eof,
                Msg = "array missing terminating ']'",
                make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                Result = error(Error)
            ;
                NextToken = token_error(TokenError),
                Result = error(TokenError)
            )
        ;
            ItemResult = eof,
            Result = eof
        ;
            ItemResult = error(Error),
            Result = error(Error)
        )
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_comma
        ; Token = token_colon
        ),
        TokenStr = token_to_string(Token),
        Msg = "expected a value",
        make_syntax_error(Reader, TokenStr, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        Token = token_eof,
        Msg = "array missing terminating ']'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(Error)
    ;
        Token = token_error(TokenError),
        Result = error(TokenError)
    ).

%-----------------------------------------------------------------------------%
%
% Folding over object members.
%

do_object_fold(Reader, Pred, !.Acc, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_left_curly_bracket,
        NestingDepth = 1,
        ( if below_nesting_depth_limit(Reader, NestingDepth) then
            do_object_fold_members(Reader, NestingDepth, at_start, Pred,
                !.Acc, Result, !State)
        else
            make_error_context(Reader, Context, !State),
            ErrorDesc = maximum_nesting_depth_reached,
            Error = json_error(Context, ErrorDesc),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected '{'",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

:- pred do_object_fold_members(json.reader(Stream), nesting_depth,
    object_where, pred(string, json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_object_fold_members(in, in, in, in(pred(in, in, in, out) is det),
    in, out, di, uo) is det.
:- mode do_object_fold_members(in, in, in, in(pred(in, in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

do_object_fold_members(Reader, !.NestingDepth, Where, Pred, !.Acc, Result,
        !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_curly_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Acc)
        else
            Msg = "expected a string literal",
            TokenDesc = token_to_string(Token),
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        )
    ;
        Token = token_string(Key),
        get_token(Reader, ColonToken, !State),
        (
            ColonToken = token_colon,
            get_token(Reader, NextToken, !State),
            do_get_value(Reader, !.NestingDepth, NextToken, ValueResult,
                !State),
            (
                ValueResult = ok(Value),
                Pred(Key, Value, !Acc),
                get_token(Reader, NextNextToken, !State),
                (
                    NextNextToken = token_right_curly_bracket,
                    Result = ok(!.Acc)
                ;
                    NextNextToken = token_comma,
                    do_object_fold_members(Reader, !.NestingDepth, after_comma,
                        Pred, !.Acc, Result, !State)
                ;
                    ( NextNextToken = token_left_curly_bracket
                    ; NextNextToken = token_left_square_bracket
                    ; NextNextToken = token_right_square_bracket
                    ; NextNextToken = token_colon
                    ; NextNextToken = token_string(_)
                    ; NextNextToken = token_number(_)
                    ; NextNextToken = token_false
                    ; NextNextToken = token_true
                    ; NextNextToken = token_null
                    ),
                    Msg = "expected '}' or ','",
                    NextNextTokenDesc = token_to_string(NextNextToken),
                    make_syntax_error(Reader, NextNextTokenDesc, yes(Msg),
                        Error, !State),
                    Result = error(!.Acc, Error)
                ;
                    NextNextToken = token_eof,
                    Msg = "expected '}' or ','",
                    make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                    Result = error(!.Acc, Error)
                ;
                    NextNextToken = token_error(TokenError),
                    Result = error(!.Acc, TokenError)
                )
            ;
                ValueResult = eof,
                unexpected($file, $pred, "unexpected end-of-file")
            ;
                ValueResult = error(Error),
                Result = error(!.Acc, Error)
            )
        ;
            ( ColonToken = token_left_curly_bracket
            ; ColonToken = token_right_curly_bracket
            ; ColonToken = token_left_square_bracket
            ; ColonToken = token_right_square_bracket
            ; ColonToken = token_comma
            ; ColonToken = token_string(_)
            ; ColonToken = token_number(_)
            ; ColonToken = token_false
            ; ColonToken = token_true
            ; ColonToken = token_null
            ),
            Msg = "expected ':'",
            TokenDesc = token_to_string(Token),
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        ;
            ColonToken = token_eof,
            make_unexpected_eof_error(Reader, no, Error, !State),
            Result = error(!.Acc, Error)
        ;
            ColonToken = token_error(Error),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected string literal or '}'",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

do_object_fold_state(Reader, Pred, !.Acc, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_left_curly_bracket,
        NestingDepth = 1,
        do_object_fold_state_members(Reader, NestingDepth, at_start, Pred,
            !.Acc, Result, !State)
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected '{'",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

:- pred do_object_fold_state_members(json.reader(Stream), nesting_depth,
    object_where, pred(string, json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_object_fold_state_members(in, in, in,
    in(pred(in, in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode do_object_fold_state_members(in, in, in,
    in(pred(in, in, in, out, di, uo) is cc_multi), in, out, di, uo)
    is cc_multi.

do_object_fold_state_members(Reader, !.NestingDepth, Where, Pred, !.Acc,
        Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_curly_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Acc)
        else
            Msg = "expected a string literal",
            TokenDesc = token_to_string(Token),
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        )
    ;
        Token = token_string(Key),
        get_token(Reader, ColonToken, !State),
        (
            ColonToken = token_colon,
            get_token(Reader, NextToken, !State),
            do_get_value(Reader, !.NestingDepth, NextToken, ValueResult,
                !State),
            (
                ValueResult = ok(Value),
                Pred(Key, Value, !Acc, !State),
                get_token(Reader, NextNextToken, !State),
                (
                    NextNextToken = token_right_curly_bracket,
                    Result = ok(!.Acc)
                ;
                    NextNextToken = token_comma,
                    do_object_fold_state_members(Reader, !.NestingDepth,
                        after_comma, Pred, !.Acc, Result, !State)
                ;
                    ( NextNextToken = token_left_curly_bracket
                    ; NextNextToken = token_left_square_bracket
                    ; NextNextToken = token_right_square_bracket
                    ; NextNextToken = token_colon
                    ; NextNextToken = token_string(_)
                    ; NextNextToken = token_number(_)
                    ; NextNextToken = token_false
                    ; NextNextToken = token_true
                    ; NextNextToken = token_null
                    ),
                    Msg = "expected '}' or ','",
                    NextNextTokenDesc = token_to_string(NextNextToken),
                    make_syntax_error(Reader, NextNextTokenDesc, yes(Msg),
                        Error, !State),
                    Result = error(!.Acc, Error)
                ;
                    NextNextToken = token_eof,
                    Msg = "expected '}' or ','",
                    make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                    Result = error(!.Acc, Error)
                ;
                    NextNextToken = token_error(TokenError),
                    Result = error(!.Acc, TokenError)
                )
            ;
                ValueResult = eof,
                unexpected($file, $pred, "unexpected end-of-file")
            ;
                ValueResult = error(Error),
                Result = error(!.Acc, Error)
            )
        ;
            ( ColonToken = token_left_curly_bracket
            ; ColonToken = token_right_curly_bracket
            ; ColonToken = token_left_square_bracket
            ; ColonToken = token_right_square_bracket
            ; ColonToken = token_comma
            ; ColonToken = token_string(_)
            ; ColonToken = token_number(_)
            ; ColonToken = token_false
            ; ColonToken = token_true
            ; ColonToken = token_null
            ),
            Msg = "expected ':'",
            TokenDesc = token_to_string(Token),
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        ;
            ColonToken = token_eof,
            make_unexpected_eof_error(Reader, no, Error, !State),
            Result = error(!.Acc, Error)
        ;
            ColonToken = token_error(Error),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected string literal or '}'",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

%-----------------------------------------------------------------------------%
%
% Folding over array elements.
%

do_array_fold(Reader, Pred, !.Acc, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_left_square_bracket,
        NestingDepth = 1,
        ( if below_nesting_depth_limit(Reader, NestingDepth) then
            do_array_fold_elements(Reader, NestingDepth,  at_start, Pred,
                !.Acc, Result, !State)
        else
            make_error_context(Reader, Context, !State),
            ErrorDesc = maximum_nesting_depth_reached,
            Error = json_error(Context, ErrorDesc),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_left_curly_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected '['",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

:- pred do_array_fold_elements(json.reader(Stream), nesting_depth,
    array_where, pred(json.value, A, A),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_array_fold_elements(in, in, in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode do_array_fold_elements(in, in, in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

do_array_fold_elements(Reader, !.NestingDepth, Where, Pred, !.Acc, Result,
        !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_square_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Acc)
        else
            TokenDesc = token_to_string(Token),
            Msg = "expected a value",
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        do_get_value(Reader, !.NestingDepth, Token, ItemResult, !State),
        (
            ItemResult = ok(Item),
            Pred(Item, !Acc),
            get_token(Reader, NextToken, !State),
            (
                NextToken = token_right_square_bracket,
                Result = ok(!.Acc)
            ;
                NextToken = token_comma,
                do_array_fold_elements(Reader, !.NestingDepth, after_comma,
                    Pred, !.Acc, Result, !State)
            ;
                ( NextToken = token_left_curly_bracket
                ; NextToken = token_left_square_bracket
                ; NextToken = token_string(_)
                ; NextToken = token_number(_)
                ; NextToken = token_false
                ; NextToken = token_true
                ; NextToken = token_null
                ; NextToken = token_right_curly_bracket
                ; NextToken = token_colon
                ),
                TokenDesc = token_to_string(NextToken),
                Msg = "expected ']' or ','",
                make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
                Result = error(!.Acc, Error)
            ;
                NextToken = token_eof,
                Msg = "array missing terminating ']'",
                make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                Result = error(!.Acc, Error)
            ;
                NextToken = token_error(TokenError),
                Result = error(!.Acc, TokenError)
            )
        ;
            ItemResult = eof,
            Msg = "expected a value",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        ;
            ItemResult = error(Error),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_comma
        ; Token = token_colon
        ),
        TokenStr = token_to_string(Token),
        Msg = "expected a value",
        make_syntax_error(Reader, TokenStr, yes(Msg),
            Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        Msg = "array missing terminating ']'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(TokenError),
        Result = error(!.Acc, TokenError)
    ).

do_array_fold_state(Reader, Pred, !.Acc, Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_left_square_bracket,
        NestingDepth = 1,
        do_array_fold_state_elements(Reader, NestingDepth, at_start, Pred,
            !.Acc, Result, !State)
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_left_curly_bracket
        ; Token = token_right_square_bracket
        ; Token = token_comma
        ; Token = token_colon
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        Msg = "expected '['",
        TokenDesc = token_to_string(Token),
        make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        make_unexpected_eof_error(Reader, no, Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(Error),
        Result = error(!.Acc, Error)
    ).

:- pred do_array_fold_state_elements(json.reader(Stream), nesting_depth,
    array_where, pred(json.value, A, A, State, State),
    A, json.maybe_partial_res(A, Error), State, State)
    <= (
        stream.line_oriented(Stream, State),
        stream.putback(Stream, char, State, Error)
    ).
:- mode do_array_fold_state_elements(in, in, in,
    in(pred(in, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode do_array_fold_state_elements(in, in, in,
    in(pred(in, in, out, di, uo) is cc_multi), in, out, di, uo) is cc_multi.

do_array_fold_state_elements(Reader, !.NestingDepth, Where, Pred, !.Acc,
        Result, !State) :-
    get_token(Reader, Token, !State),
    (
        Token = token_right_square_bracket,
        ( if
            ( Where = at_start
            ; Reader ^ json_trailing_commas = allow_trailing_commas
            )
        then
            Result = ok(!.Acc)
        else
            TokenDesc = token_to_string(Token),
            Msg = "expected a value",
            make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_left_curly_bracket
        ; Token = token_left_square_bracket
        ; Token = token_string(_)
        ; Token = token_number(_)
        ; Token = token_false
        ; Token = token_true
        ; Token = token_null
        ),
        do_get_value(Reader, !.NestingDepth, Token, ItemResult, !State),
        (
            ItemResult = ok(Item),
            Pred(Item, !Acc, !State),
            get_token(Reader, NextToken, !State),
            (
                NextToken = token_right_square_bracket,
                Result = ok(!.Acc)
            ;
                NextToken = token_comma,
                do_array_fold_state_elements(Reader, !.NestingDepth, after_comma,
                    Pred, !.Acc, Result, !State)
            ;
                ( NextToken = token_left_curly_bracket
                ; NextToken = token_left_square_bracket
                ; NextToken = token_string(_)
                ; NextToken = token_number(_)
                ; NextToken = token_false
                ; NextToken = token_true
                ; NextToken = token_null
                ; NextToken = token_right_curly_bracket
                ; NextToken = token_colon
                ),
                TokenDesc = token_to_string(NextToken),
                Msg = "expected ']' or ','",
                make_syntax_error(Reader, TokenDesc, yes(Msg), Error, !State),
                Result = error(!.Acc, Error)
            ;
                NextToken = token_eof,
                Msg = "array missing terminating ']'",
                make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
                Result = error(!.Acc, Error)
            ;
                NextToken = token_error(TokenError),
                Result = error(!.Acc, TokenError)
            )
        ;
            ItemResult = eof,
            Msg = "expected a value",
            make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
            Result = error(!.Acc, Error)
        ;
            ItemResult = error(Error),
            Result = error(!.Acc, Error)
        )
    ;
        ( Token = token_right_curly_bracket
        ; Token = token_comma
        ; Token = token_colon
        ),
        TokenStr = token_to_string(Token),
        Msg = "expected a value",
        make_syntax_error(Reader, TokenStr, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_eof,
        Msg = "array missing terminating ']'",
        make_unexpected_eof_error(Reader, yes(Msg), Error, !State),
        Result = error(!.Acc, Error)
    ;
        Token = token_error(TokenError),
        Result = error(!.Acc, TokenError)
    ).

%-----------------------------------------------------------------------------%

:- pred below_nesting_depth_limit(json.reader(_)::in, nesting_depth::in)
    is semidet.

below_nesting_depth_limit(Reader, CurrentDepth) :-
    MaxDepthParam = Reader ^ json_maximum_nesting_depth,
    require_complete_switch [MaxDepthParam] (
        MaxDepthParam = no_maximum_nesting_depth
    ;
        MaxDepthParam = maximum_nesting_depth(MaxDepth),
        CurrentDepth < MaxDepth
    ).

%-----------------------------------------------------------------------------%
:- end_module json.json_parser.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test the JSON writer.
%
%-----------------------------------------------------------------------------%

:- module test_writer.
:- interface.

:- import_module io.

:- pred test_writer(io.text_output_stream::in, io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module bool.
:- import_module float.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

test_writer(File, !IO) :-
    test_writers(File, test_writer_params, !IO),
    test_comments(File, !IO),
    test_file_stream_writers(File, !IO).

%-----------------------------------------------------------------------------%

:- pred test_writers(io.text_output_stream::in,
    list(writer_params)::in(list(writer_params)), io::di, io::uo)
    is cc_multi.

test_writers(_, [], !IO).
test_writers(File, [Params | OtherParams], !IO) :-
    Params = writer_params(Style, AllowInfinities, MemberFilter),
    (
        MemberFilter = no_member_filter,
        MemberFilterStr = "no_member_filter"
    ;
        MemberFilter = member_filter(_),
        MemberFilterStr = "member_filter"
    ),
    io.format(File, "*** Testing writer(%s, %s, %s) ***\n",
        [s(string(Style)), s(string(AllowInfinities)), s(MemberFilterStr)], !IO),
    test_writer(File, Params, test_documents, !IO),
    (
        OtherParams = []
    ;
        OtherParams = [_ | _],
        io.nl(File, !IO)
    ),
    test_writers(File, OtherParams, !IO).

:- pred test_writer(io.text_output_stream::in,
    writer_params::in(writer_params), list(json.value)::in,
    io::di, io::uo) is cc_multi.

test_writer(_, _, [], !IO).
test_writer(File, Params, [Doc | Docs], !IO) :-
    test_writer_with_doc(File, Params, Doc, !IO),
    (
        Docs = []
    ;
        Docs = [_ | _],
        io.nl(File, !IO)
    ),
    test_writer(File, Params, Docs, !IO).

:- pred test_writer_with_doc(io.text_output_stream::in,
    writer_params::in(writer_params), json.value::in, io::di, io::uo)
    is cc_multi.

test_writer_with_doc(File, Params, Doc, !IO) :-
    init_writer(File, Params, Writer, !IO),
    ( try [io(!IO)] (
        json.put_value(Writer, Doc, !IO),
        OutputStyle = Params ^ output_style,
        (
            OutputStyle = compact,
            io.nl(File, !IO)
        ;
            OutputStyle = pretty
        )
    ) then
        true
    catch_any Excp ->
        io.format(File, "EXCP (%s)\n", [s(string(Excp))], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred test_comments(io.text_output_stream::in, io::di, io::uo) is det.

test_comments(File, !IO) :-
    init_writer(File, Writer, !IO),
    put_comment(Writer, comment_eol(""), !IO),
    put_comment(Writer, comment_eol("This is an EOL comment"), !IO),
    put_comment(Writer, comment_multiline(""), !IO),
    put_comment(Writer, comment_multiline("This is a mutli\nline comment"),
        !IO),
    io.nl(File, !IO),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%

:- pred test_file_stream_writers(io.text_output_stream::in, io::di, io::uo)
    is det.

test_file_stream_writers(File, !IO) :-
    io.set_output_stream(File, OldOutputFile, !IO),

    io.write_string(File, "*** Testing write_compact/3 ***\n", !IO),
    write_compact(null, !IO),
    io.nl(!IO),
    write_compact(bool(yes), !IO),
    io.nl(!IO),
    io.nl(!IO),

    io.write_string(File, "*** Testing writer_pretty/3 ****\n", !IO),
    write_pretty(null, !IO),
    write_pretty(bool(yes), !IO),
    io.nl(!IO),

    io.set_output_stream(OldOutputFile, _, !IO),

    io.write_string(File, "*** Testing write_compact/4 ***\n", !IO),
    write_compact(File, null, !IO),
    io.nl(File, !IO),
    write_compact(File, bool(yes), !IO),
    io.nl(File, !IO),
    io.nl(File, !IO),

    io.write_string(File, "*** Testing write_pretty/4 ****\n", !IO),
    write_pretty(File, null, !IO),
    write_pretty(File, bool(yes), !IO).

%-----------------------------------------------------------------------------%

:- func test_writer_params = (list(writer_params)::out(list(writer_params))).

test_writer_params = [
    writer_params(compact, do_not_allow_infinities, no_member_filter),
    writer_params(compact, allow_infinities, no_member_filter),
    writer_params(compact, do_not_allow_infinities, member_filter(filter_foo)),
    writer_params(compact, allow_infinities, member_filter(filter_foo)),
    writer_params(pretty, do_not_allow_infinities, no_member_filter),
    writer_params(pretty, allow_infinities, no_member_filter),
    writer_params(pretty, do_not_allow_infinities, member_filter(filter_foo)),
    writer_params(pretty, allow_infinities, member_filter(filter_foo))
].

:- pred filter_foo(string::in, value::in) is semidet.

filter_foo("foo", _).

%-----------------------------------------------------------------------------%

:- func test_documents = list(json.value).

test_documents = [
    null,
    bool(yes),
    bool(no),
    number(3.141),
    number(float.max * float.max), % float.infinity post 14.01.
    string(""),
    string("foo"),
    array([
        number(1.0),
        number(2.0),
        number(3.0),
        number(4.0),
        number(5.0),
        number(6.0)
    ]),
    json.det_make_object([
        "foo" - bool(yes),
        "bar" - bool(no)
    ])
].

%-----------------------------------------------------------------------------%
:- end_module test_writer.
%-----------------------------------------------------------------------------%


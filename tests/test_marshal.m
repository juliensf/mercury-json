%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test marshaling of Mercury -> JSON and unmarshaling of JSON -> Mercury.
%
%-----------------------------------------------------------------------------%

:- module test_marshal.
:- interface.

:- import_module io.

:- pred test_marshaling(io.text_output_stream::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module array.
:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module calendar.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module set_bbbtree.
:- import_module set_unordlist.
:- import_module set_tree234.
:- import_module set_ctree234.
:- import_module stream.
:- import_module string.
:- import_module univ.
:- import_module version_array.

%-----------------------------------------------------------------------------%

test_marshaling(File, !IO) :-

    % Test ints.
    %
    test(File, -561, !IO),
    test(File, 0, !IO),
    test(File, 561, !IO),
    % XXX FIXME: max_int case is not working.
    %test(File, int.max_int : int, !IO),
    % XXX disabled since it's machine specific.
    %test(File, int.min_int : int, !IO),

    % Test strings.
    test(File, "", !IO),
    test(File, "aaabbbccc", !IO),
    test(File, "ûń", !IO),

    % Test floats.
    test(File, 3.141, !IO),
    test(File, infinity, !IO),     % Error.

    % Test chars.
    %
    test(File, 'A', !IO),

    % Test bools.
    test(File, no : bool, !IO),
    test(File, yes : bool, !IO),

    % Test bigints.
    %
    Integer = integer.det_from_string("2365712637126347861237846728356712647827834723847812937812734871293478127"),
    test(File, Integer, !IO),
   
    % Test dates.
    %
    DateA = det_init_date(2000, february, 18, 19, 30, 0, 0),
    test(File, DateA, !IO),
    DateB = det_init_date(2013, march, 2, 20, 30, 11, 123),
    test(File, DateB, !IO),

    % Test durations.
    %
    test(File, zero_duration, !IO),
    
    % Test lists.
    test(File, [1, 2, 3], !IO),

    % Test enumerations.
    %
    test(File, apple, !IO),
    test(File, pear, !IO),
    test(File, [apple, orange, lemon, pear], !IO),

    % Test d.u types.
    %
    test(File, foo(1, 2, 3), !IO),
    test(File, bar([1, 2, 3], 4), !IO),
    test(File, baaz, !IO),

    % Test polymorphic d.u. types.
    %
    test(File, poly1 : poly(fruit), !IO),
    test(File, poly2(lemon), !IO),
    test(File, poly3(apple, orange), !IO),

    % Test univ.
    %
    test(File, univ(561), !IO),
    
    % Test foreign types.
    %
    F = make_foreign,
    test(File, F, !IO),

    test(File, existq1, !IO),
    test(File, existq2(561), !IO),
    test(File, 'new existq3'(561), !IO),

    % Test maybe types.
    %
    test(File, yes("foo"), !IO),
    test(File, no : maybe(string), !IO),

    % Test pairs.
    %
    test(File, apple - orange, !IO),
    test(File, [1, 2, 3] - [apple, orange, pear], !IO),

    % Test maybe_error.
    %
    test(File, ok(pear) : maybe_error(fruit), !IO),
    test(File, error("not fruit") : maybe_error(fruit), !IO),
    test(File, error(apple) : maybe_error(fruit, fruit), !IO),

    % Test sets.
    %
    Set = set.from_list([1, 1, 2, 2, 3, 3, 4, 4]),
    test(File, Set, !IO),

    Set1 = set_unordlist.from_list([1, 1, 2, 2, 3, 3, 4, 4]),
    test(File, Set1, !IO),

    set_tree234.from_list([1, 1, 2, 2, 3, 3, 4, 4], Set2),
    test(File, Set2, !IO),

    Set3 = set_ctree234.from_list([1, 1, 2, 2, 3, 3, 4, 4]),
    test(File, Set3, !IO),

    Set4 = set_bbbtree.from_list([1, 1, 2, 2, 3, 3, 4, 4]),
    test(File, Set4, !IO),

    % Test assoc lists.
    %
    AssocList = [apple - "Apple", orange - "Orange", lemon - "Lemon"],
    test(File, AssocList, !IO),

    % Test maps.
    %
    map.from_assoc_list(AssocList, Map),
    test(File, Map, !IO),

    % Test bimaps.
    %
    bimap.det_from_assoc_list(AssocList, Bimap),
    test(File, Bimap, !IO),

    % Test cords.
    %
    Cord = cord.from_list([1, 2, 3]) ++ cord.from_list([5, 6, 7]),
    test(File, Cord, !IO),

    % Test arrays.
    %
    array.make_empty_array(EmptyArray : array(fruit)),
    test(File, EmptyArray, !IO),
    array.from_list([pear, lemon, lemon, orange, apple], Array),
    test(File, Array, !IO),

    % Test version arrays.
    %
    EmptyVersionArray = version_array.empty : version_array(fruit),
    test(File, EmptyVersionArray, !IO),
    VersionArray = version_array.from_list([pear, lemon, lemon, orange, apple]),
    test(File, VersionArray, !IO).

%-----------------------------------------------------------------------------%

:- type foo
    --->    foo(int, int, int)
    ;       bar(list(int), int)
    ;       baaz.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon
    ;       pear.

:- type poly(T)
    --->    poly1
    ;       poly2(T)
    ;       poly3(T, T).

:- type foreign.

:- pragma foreign_type("C", foreign, "int").
:- pragma foreign_type("Java", foreign, "java.lang.Object").
:- pragma foreign_type("C#", foreign, "object").

:- func make_foreign = foreign.

:- pragma foreign_proc("C",
    make_foreign = (F::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    F = 3;
").

:- pragma foreign_proc("Java",
    make_foreign = (F::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    F = null;
").

:- pragma foreign_proc("C#",
    make_foreign = (F::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    F = null;
").

:- type existq
    --->    existq1
    ;       existq2(int)
    ;       some [T] existq3(T).

%-----------------------------------------------------------------------------%

:- pred test(io.text_output_stream::in, T::in, io::di, io::uo) is det.

test(File, Term, !IO) :-
    io.write_string(File, " Orig. Term: ", !IO),
    io.print(File, Term, !IO),
    io.nl(File, !IO),
    io.write_string(File, "       JSON: ", !IO),
    MaybeJSON = json.from_type(Term),
    (
        MaybeJSON = ok(Value),
        Writer = init_writer(File),
        put_json(Writer, Value, !IO),
        io.nl(File, !IO),
        MaybeTermPrime : maybe_error(T) = json.to_type(Value),
        io.write_string(File, "Result Term: ", !IO),
        (
            MaybeTermPrime = ok(TermPrime),
            io.print(File, TermPrime, !IO),
            io.nl(File, !IO)
        ;
            MaybeTermPrime = error(ResultMsg),
            io.write_string(File, "error: ", !IO),
            io.print(File, ResultMsg, !IO),
            io.nl(File, !IO)
        )
    ;
        MaybeJSON = error(ToJSON_Msg),
        io.write_string(File, "error: ", !IO),
        io.print(File, ToJSON_Msg, !IO),
        io.nl(File, !IO)
    ),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%
:- end_module test_marshal.
%-----------------------------------------------------------------------------%

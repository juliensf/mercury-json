%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014-2016 Julien Fischer.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Test marshaling of Mercury -> JSON and unmarshaling of JSON -> Mercury.
%
%-----------------------------------------------------------------------------%

:- module test_marshal.
:- interface.

:- import_module io.

:- pred test_marshaling(io.text_output_stream::in, io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module json.

:- import_module array.
:- import_module array2d.
:- import_module assoc_list.
:- import_module bag.
:- import_module bimap.
:- import_module bitmap.
:- import_module bool.
:- import_module calendar.
:- import_module cord.
:- import_module digraph.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module pqueue.
:- import_module queue.
:- import_module rational.
:- import_module rbtree.
:- import_module set.
:- import_module set_bbbtree.
:- import_module set_unordlist.
:- import_module set_tree234.
:- import_module set_ctree234.
:- import_module stream.
:- import_module string.
:- import_module unit.
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

    % XXX for compatibility with Mercury 14.01.1 we get infinity this way.
    % With later versions we would just use float.infinity/0.
    Infinity = float.max + float.max,
    test(File, Infinity, !IO),     % Error.

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

    % Test rationals.
    %
    Numerator = integer.det_from_string("2137491723578213491283472187358217348129357812374912873489123"),
    Denominator = integer.det_from_string("19823914719237911293810238918239182984120318293812381"),
    Rational = rational.from_integers(Numerator, Denominator),
    test(File, Rational, !IO),

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

    % Test maybe types.
    %
    test(File, yes("foo"), !IO),
    test(File, no : maybe(string), !IO),

    % Test the type maybe(json.value).
    % The original scheme we used for handling maybe's could not distinguish
    % between 'yes(null)' and 'no'.
    %
    test(File, yes(null), !IO),

    % Test maybe_error/2 types.
    %
    test(File, ok(561) : maybe_error(int), !IO),
    test(File, ok(null) : maybe_error(json.value), !IO),
    test(File, error("this is an error") : maybe_error(string), !IO),
    test(File, error([1, 2, 3, 4]) : maybe_error(string, list(int)), !IO),

    % Test pairs.
    %
    test(File, apple - orange, !IO),
    test(File, [1, 2, 3] - [apple, orange, pear], !IO),

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

    % Test rbtrees.
    %
    some [!RBTree] (
        !:RBTree = rbtree.from_assoc_list(AssocList),
        rbtree.insert_duplicate(apple, "Apple2", !RBTree),
        rbtree.insert_duplicate(apple, "Apple3", !RBTree),
        rbtree.insert_duplicate(apple, "Apple4", !RBTree),
        test(File, !.RBTree, !IO)
    ),

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

    % Test array2ds.
    EmptyArray2d : array2d(fruit) = array2d.from_lists([]),
    test(File, EmptyArray2d, !IO),
    Array2d : array2d(fruit) = array2d.from_lists([
        [apple, pear, apple],
        [orange, apple, pear],
        [pear, apple, orange]
    ]),
    test(File, Array2d, !IO),

    % Test version arrays.
    %
    EmptyVersionArray = version_array.empty : version_array(fruit),
    test(File, EmptyVersionArray, !IO),
    VersionArray = version_array.from_list([pear, lemon, lemon, orange, apple]),
    test(File, VersionArray, !IO),

    % Test bitmaps.
    %
    % XXX the bitmap module should provide det_from_string.
    %
    ( if Bitmap = bitmap.from_string("<24:10AFBD>") then
        test(File, Bitmap, !IO)
    else
        true
    ),

    % Test unit.
    %
    test(File, [unit, unit, unit], !IO),

    % Test queues.
    %
    Queue = queue.from_list([2, 3, 5, 7, 11, 13, 17, 19, 23]),
    test(File, Queue, !IO),

    % Test priority queues.
    %
    assoc_list_to_pqueue([561 - "A", 23 - "B", 491 - "C", 1 - "D"], PQueue),
    test(File, PQueue, !IO),

    % Test directed graphs.
    %
    some [!DG] (
        digraph.init(!:DG),
        digraph.add_vertex("A", AKey, !DG),
        digraph.add_vertex("B", BKey, !DG),
        digraph.add_vertex("C", CKey, !DG),
        digraph.add_vertex("D", _DKey, !DG),
        digraph.add_edge(AKey, BKey, !DG),
        digraph.add_edge(AKey, CKey, !DG),
        digraph.add_edge(CKey, BKey, !DG),
        Digraph = !.DG
    ),
    test(File, Digraph, !IO),

    % Test bags.
    %
    bag.init(EmptyBag : bag(int)),
    test(File, EmptyBag, !IO),

    some [!Bag] (
        bag.init(!:Bag),
        bag.insert(lemon, !Bag),
        bag.insert(lemon, !Bag),
        bag.insert(lemon, !Bag),
        bag.insert(apple, !Bag),
        bag.insert(pear, !Bag),
        bag.insert(pear, !Bag),
        test(File, !.Bag, !IO)
    ),

    % Test JSON.
    %
    test(File, [bool(yes), bool(no), null, number(5.61)], !IO),

    % Test JSON pointer.
    %
    Pointer = json.det_string_to_pointer("/foo/~1bar/12/-/~~~~1"),
    test(File, Pointer, !IO),

    % Test conversion to strings.
    %
    ToStringTests = [
        null,
        bool(no),
        bool(yes),
        number(0.0),
        number(-3.141),
        number(3.141),
        string(""),
        string("foo"),
        array([]),
        array([null, bool(yes), number(1.0), array([number(1.0)])]),
        object(map.init),
        object(map.from_assoc_list(["foo" - null, "bar" - bool(yes), "baaz" - number(5.3)]))
    ],
    list.foldl(do_to_string_test(File), ToStringTests, !IO),
    io.nl(File, !IO),

    % Test conversion from strings.
    %
    ValidFromStringTests = [
        "null",
        "true",
        "false",
        "3e2",
        "3E2",
        "3.141",
        "0.0",
        "-3.141",
        "-3e2",
        "-3E2",
        "\"foo\"",
        "   null",
        "[]",
        "[1.0, 2.0, 3.0]",
        "[null, true, 3.0]",
        "{}",
        "{\"foo\" : 3.0}",
        "{\"foo\" : -3e2}",
        "{\"foo\" : true, \"bar\" : false}"
    ],
    list.foldl(do_from_string_test(File), ValidFromStringTests, !IO),
    io.nl(File, !IO),

    InvalidFromStringTests = [
        "Null",
        "12.",
        "12e",
        "12E",
        ".12",
        "[1.0, 2.0",
        "{\"foo\" : }",
        "{}{}",
        "[1, 2, 3][",
        "[1, 2, 3]]",
        "nullnull",
        "true 1.23",
        "{ \"a\" : 12.3 } true"
    ],
    list.foldl(do_from_string_test(File), InvalidFromStringTests, !IO).

:- pred do_to_string_test(io.text_output_stream::in,
    value::in, io::di, io::uo) is det.

do_to_string_test(File, Value, !IO) :-
    io.format(File, "to_string(%s) = \"%s\"\n",
        [s(string(Value)), s(to_string(Value))], !IO).

:- pred do_from_string_test(io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

do_from_string_test(File, String, !IO) :-
    ( if json.from_string(String, Value) then
        io.format(File, "from_string(\"%s\") = %s\n",
            [s(String), s(string(Value))], !IO)
    else
        io.format(File, "from_string(\"%s\") <<FALSE>>\n",
            [s(String)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- type fruit
    --->    apple
    ;       orange
    ;       lemon
    ;       pear.

:- instance to_json(fruit) where [
    ( to_json(F) = Value :-
        fruit_string(F, S),
        Value = string(S)
    )
].

:- instance from_json(fruit) where [
    ( from_json(V) = Result :-
        ( if
            V = string(S),
            fruit_string(F, S)
        then
            Result = ok(F)
        else
            Result = error("not a fruit")
        )
    )
].

:- pred fruit_string(fruit, string).
:- mode fruit_string(in, out) is det.
:- mode fruit_string(out, in) is semidet.

fruit_string(apple,  "apple").
fruit_string(orange, "orange").
fruit_string(lemon,  "lemon").
fruit_string(pear,   "pear").

%-----------------------------------------------------------------------------%

:- pred test(io.text_output_stream::in, T::in, io::di, io::uo) is cc_multi
    <= (to_json(T), from_json(T)).

test(File, Term, !IO) :-
    io.write_string(File, " Orig. Term: ", !IO),
    io.print(File, Term, !IO),
    io.nl(File, !IO),
    io.write_string(File, "       JSON: ", !IO),
    ( try []
        Value = json.from_type(Term)
    then
        json.init_writer(File, Writer, !IO),
        json.put_value(Writer, Value, !IO),
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
    catch NonFiniteNumberError ->
        NonFiniteNumberError = non_finite_number_error(_),
        io.write_string(File,
            "error: cannot convert non-finite float to JSON\n", !IO)
    ),
    io.nl(File, !IO).

%-----------------------------------------------------------------------------%
:- end_module test_marshal.
%-----------------------------------------------------------------------------%

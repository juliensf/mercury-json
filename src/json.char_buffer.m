%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2013-2014 Julien Fischer.
% See the file COPYING for license details.
%-----------------------------------------------------------------------------%

:- module json.char_buffer.
:- interface.

%-----------------------------------------------------------------------------%

:- type char_buffer.

:- impure pred char_buffer.init(char_buffer::out) is det.

:- pred char_buffer.add(char_buffer::in, char::in, S::di, S::uo) is det.

:- func char_buffer.to_string(char_buffer::in, S::ui) = (string::out) is det.

:- pred char_buffer.last(char_buffer::in, char::out, S::ui) is semidet.

:- pred char_buffer.reset(char_buffer::in, S::di, S::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.

%-----------------------------------------------------------------------------%
%
% C implementation of character buffers.
%

:- pragma foreign_type("C", char_buffer, "MJSON_buffer *",
    [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "

#include ""mercury_memory.h""
#include ""mercury_string.h""

/*
** Defining the macro MJSON_DEBUG_BUFFER will cause some debugging traces
** to be printed to the standard error.
*/
#if defined(MJSON_DEBUG_BUFFER)
    #include <stdio.h>
#endif

/*
** The initial size of the character buffer in bytes.
** You probably don't need to change this, but if you do it must be >= 5.
** (We always reserve space for four bytes -- the maximum required for a
** code point -- plus a nul terminator.
*/
#define MJSON_INITIAL_BUFFER_SIZE 512

/*
** The C version of a character buffer.  It contains the following fields:
**
** last_char    The character code of the last character that was added
**              to the buffer.  If the buffer is empty then this == 0.
**
** num_bytes    The number of bytes used to hold characters in the buffer.
**
** max_bytes    The maximum number of bytes the buffer can currently hold.
**
** contents     The contents of the buffer -- stored as a UTF-8 encoded
**              string.
*/
typedef struct {
    MR_Char     last_char;
    size_t      num_bytes;
    size_t      max_bytes;
    MR_String   contents;
} MJSON_buffer;

").

    % XXX once we drop support for Mercury 14.01 we should use
    % MR_GC_malloc_atomic instead of MR_GC_malloc below.
    %
:- pragma foreign_proc("C",
    init(Buffer::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Buffer = MR_GC_NEW(MJSON_buffer);
    Buffer->last_char = 0;
    Buffer->num_bytes = 0;
    Buffer->max_bytes = MJSON_INITIAL_BUFFER_SIZE;
    Buffer->contents = MR_GC_malloc(sizeof(char) * MJSON_INITIAL_BUFFER_SIZE);
").

:- pragma foreign_proc("C",
    add(Buffer::in, Char::in, State0::di, State::uo),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Buffer->last_char = Char;

    /*
    ** Check whether we need to resize the buffer.
    */
    if (Buffer->num_bytes >= Buffer->max_bytes - 5) {

        MR_String   new_contents;
        size_t      new_size;

        new_size = Buffer->max_bytes * 2;
        new_contents = MR_GC_realloc(Buffer->contents,
            sizeof(char) * new_size);

        Buffer->contents = new_contents;
        Buffer->max_bytes = new_size;

        #if defined(MJSON_DEBUG_BUFFER)
            fprintf(stderr,
                \"Extending char buffer to %d bytes\\n\", new_size);
        #endif
    }

    if (MR_is_ascii(Char)) {
        Buffer->contents[Buffer->num_bytes] = Char;
        Buffer->num_bytes++;
    } else {
        Buffer->num_bytes +=
            MR_utf8_encode(Buffer->contents + Buffer->num_bytes, Char);
    }
    State = State0;
").

:- pragma foreign_proc("C",
    to_string(Buffer::in, _State::ui) = (Str::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Buffer->contents[Buffer->num_bytes] = '\\0';
    MR_make_aligned_string_copy(Str, Buffer->contents);
").

:- pragma foreign_proc("C",
    last(Buffer::in, Char::out, _State::ui),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    if (Buffer->last_char > 0) {
        Char = Buffer->last_char;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- pragma foreign_proc("C",
    reset(Buffer::in, State0::di, State::uo),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    Buffer->last_char = 0;
    Buffer->num_bytes = 0;
    State = State0;
").

%-----------------------------------------------------------------------------%
%
% Mercury implementation of character buffers.
%

    % NOTE: the notag wrapper here is necessary for this type to co-exist
    % with the foreign_type version above.
    %
:- type char_buffer
    --->    char_buffer(mutvar(char_buffer_rep)).

:- type char_buffer_rep
    --->   char_buffer_rep(list(char), int).

init(char_buffer(Buffer)) :-
    BufferRep = char_buffer_rep([], 0),
    impure new_mutvar(BufferRep, Buffer).

add(char_buffer(Buffer), Char, !State) :-
    promise_pure (
        impure get_mutvar(Buffer, BufferRep0),
        BufferRep0 = char_buffer_rep(Chars, NumChars),
        BufferRep = char_buffer_rep([Char | Chars], NumChars + 1),
        impure set_mutvar(Buffer, BufferRep),
        !:State = !.State
    ).

to_string(char_buffer(Buffer), _State) = String :-
    promise_pure (
        impure get_mutvar(Buffer, BufferRep),
        BufferRep = char_buffer_rep(RevChars, _),
        String = string.from_rev_char_list(RevChars)
    ).

last(char_buffer(Buffer), LastChar, _State) :-
    promise_pure (
        impure get_mutvar(Buffer, BufferRep),
        BufferRep = char_buffer_rep(Chars, _),
        Chars = [LastChar | _]
    ).

reset(char_buffer(Buffer), !State) :-
    promise_pure (
        BufferRep = char_buffer_rep([], 0),
        impure set_mutvar(Buffer, BufferRep),
        !:State = !.State
    ).

%-----------------------------------------------------------------------------%
:- end_module char_buffer.
%-----------------------------------------------------------------------------%

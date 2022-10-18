:- module(inline, [
	      backslash_escapes//1,
	      emphasis//1,
	      inline_code//1,
	      inline_image//1,
	      inline_link//1,
	      inline_text/2,
	      backticks//1
]).

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

look_ahead(T), [T] --> [T].

backslash_escapes(Output) --> "\\\"", backslash_escapes(Xs), { append("&quot;", Xs, Output) }.
backslash_escapes(Output) --> "\\&", backslash_escapes(Xs), { append("&amp;", Xs, Output) }.
backslash_escapes(Output) --> "\\<", backslash_escapes(Xs), { append("&lt;", Xs, Output) }.
backslash_escapes(Output) --> "\\>", backslash_escapes(Xs), { append("&gt;", Xs, Output) }.
backslash_escapes([X|Xs]) --> "\\", [X], { member(X, "!#$%'()*+,-./:;=?@[\\]^_`{|}~") }, backslash_escapes(Xs).
backslash_escapes([\,X|Xs]) --> "\\", [X], backslash_escapes(Xs).
backslash_escapes([X|Xs]) --> [X], backslash_escapes(Xs).
backslash_escapes([]) --> [].

emphasis([' ',T,' '|Html0]) -->
    " ",
    [T],
    " ",
    {
	member(T, "*_")
    },
    !,
    emphasis(Html0).

emphasis([\,T|Html0]) -->
    "\\",
    [T],
    {
	member(T, "*_")
    },
    !,
    emphasis(Html0).

emphasis(Html) -->
    [G],
    {
	member(G, "*_")
    },
    look_ahead(T),
    {
	\+ T = G
    },
    emphasis_low_end(G, Html0),
    !,
    emphasis(Html1),
    {
	phrase(format_("<em>~s</em>~s", [Html0, Html1]), Html)
    }.

emphasis(Html) -->
    [T,T],
    {
	member(T, "*_")
    },
    emphasis_high_end(T, Html0),
    !,
    emphasis(Html1),
    {
	phrase(format_("<strong>~s</strong>~s", [Html0, Html1]), Html)
    }.

emphasis([X|Html0]) -->
    [X],
    emphasis(Html0).

emphasis([]) --> [].

emphasis_low_end(G, [' ',T,' '|Html0]) -->
    " ",
    [T],
    " ",
    {
	member(T, "*_")
    },
    !,
    emphasis_low_end(G, Html0).

emphasis_low_end(G, [\,T|Html0]) -->
    "\\",
    [T],
    {
	member(T, "*_")
    },
    !,
    emphasis_low_end(G, Html0).

emphasis_low_end(G, "") -->
    [G],
    look_ahead(T),
    {
	\+ T = G
    },
    !.

emphasis_low_end(G, Html) -->
    [T,T],
    {
	member(T, "*_")
    },
    emphasis_low_high_end(T, Html0),
    !,
    emphasis_low_end(G, Html1),
    {
	phrase(format_("<strong>~s</strong>~s", [Html0, Html1]), Html)
    }.

emphasis_low_end(G, "") -->
    [G].

emphasis_low_end(G, [X|Html0]) -->
    [X],
    emphasis_low_end(G, Html0).

emphasis_high_low_end(G, [' ',T,' '|Html0]) -->
    " ",
    [T],
    " ",
    {
	member(T, "*_")
    },
    !,
    emphasis_high_low_end(G, Html0).

emphasis_high_low_end(G, [\,T|Html0]) -->
    "\\",
    [T],
    {
	member(T, "*_")
    },
    !,
    emphasis_high_low_end(G, Html0).

emphasis_high_low_end(G, "") -->
    [G],
    look_ahead(T),
    {
	\+ T = G
    },
    !.

emphasis_high_low_end(G, "") -->
    [G].

emphasis_high_low_end(G, [X|Html0]) -->
    [X],
    emphasis_high_low_end(G, Html0).

emphasis_high_end(G, [' ',T,' '|Html0]) -->
    " ",
    [T],
    " ",
    {
	member(T, "*_")
    },
    !,
    emphasis_high_end(G, Html0).

emphasis_high_end(G, [\,T|Html0]) -->
    "\\",
    [T],
    {
	member(T, "*_")
    },
    !,
    emphasis_high_end(G, Html0).

emphasis_high_end(G, Html) -->
    [T],
    {
	member(T, "*_")
    },
    look_ahead(U),
    {
	\+ T = U
    },
    emphasis_high_low_end(T, Html0),
    !,
    emphasis_high_end(G, Html1),
    {
	phrase(format_("<em>~s</em>~s", [Html0, Html1]), Html)
    }.

emphasis_high_end(G, "") -->
    [G, G].

emphasis_high_end(G, [X|Html0]) -->
    [X],
    emphasis_high_end(G, Html0).

emphasis_low_high_end(G, [' ',T,' '|Html0]) -->
    " ",
    [T],
    " ",
    {
	member(T, "*_")
    },
    !,
    emphasis_low_high_end(G, Html0).

emphasis_low_high_end(G, [\,T|Html0]) -->
    "\\",
    [T],
    {
	member(T, "*_")
    },
    !,
    emphasis_low_high_end(G, Html0).

emphasis_low_high_end(G, "") -->
    [G, G].

emphasis_low_high_end(G, [X|Html0]) -->
    [X],
    emphasis_low_high_end(G, Html0).

inline_code([\,'`'|Html0]) -->
    "\\`",
    !,
    inline_code(Html0).

inline_code(Html) -->
    backticks(N), { N > 0 },
    inline_code_end(N, Html0),
    !,
    inline_code(Html1),
    {
	phrase(format_("<code>~s</code>~s", [Html0, Html1]), Html)
    }.

inline_code([X|Html0]) -->
    [X],
    inline_code(Html0).

inline_code("") --> [].

backticks(N) -->
    "`",
    backticks(N0),
    { N is N0 + 1 }.

backticks(1) --> "`".

inline_code_end(N, [\,'`'|Html0]) -->
    "\\`",
    !,
    inline_code_end(N, Html0).

inline_code_end(N, "") -->
    backticks(N),!.

inline_code_end(N, Html) -->
    "&",
    inline_code_end(N, Html0),
    { append("&amp;", Html0, Html) }.

inline_code_end(N, Html) -->
    "<",
    inline_code_end(N, Html0),
    { append("&lt;", Html0, Html) }.

inline_code_end(N, Html) -->
    ">",
    inline_code_end(N, Html0),
    { append("&gt;", Html0, Html) }.

inline_code_end(N, [X|Html0]) -->
    [X],
    inline_code_end(N, Html0).

inline_image([\,'!'|Html]) -->
    "\\!",
    !,
    inline_image(Html).

inline_image(Html) -->
    "![",
    seq(AltText),{ \+ member(']', AltText) },
    "](",
    seq(ImgPath),{ \+ member(')', ImgPath) },
    ")",
    !,
    inline_image(Html1),
    {
	phrase(format_("<img alt=\"~s\" src=\"~s\">~s", [AltText, ImgPath, Html1]), Html)
    }.

inline_image([X|Html0]) -->
    [X],
    inline_image(Html0).

inline_image("") --> [].

inline_link([\,'['|Html0]) -->
    "\\[",
    !,
    inline_link(Html0).

inline_link(Html) -->
    "[",
    seq(Text),{ \+ member(']', Text) },
    "](",
    seq(Link),{ \+ member(')', Link) },
    ")",
    !,
    inline_image(Html1),
    {
	phrase(format_("<a href=\"~s\">~s</a>~s", [Link, Text, Html1]), Html)
    }.

inline_link([X|Html0]) -->
    [X],
    inline_link(Html0).

inline_link("") --> [].

inline_text(Md, Html) :-
    phrase(inline_image(Html0), Md),
    phrase(inline_link(Html1), Html0),
    phrase(inline_code(Html2), Html1),
    phrase(emphasis(Html3), Html2),
    phrase(backslash_escapes(Html), Html3).

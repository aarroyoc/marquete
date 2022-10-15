:- module(marquete, [
	      markdown/2,
	      thematic_break//1,
	      atx_heading//1,
	      setext_heading//1,
	      backslash_escapes//1,
	      emphasis//1
]).
	      

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

render(In, Out) :-
    phrase(markdown(Out), In).

% Reference: https://github.github.com/gfm/

char(X) -->
    [X],
    {
	\+ member(X, "\n\r")
    }.
line_ending --> "\n" | "\r" | "\r\n".

line_chars([X|Xs]) -->
    char(X),
    line_chars(Xs).
line_chars([X]) --> char(X).

line([X|Xs]) -->
    char(X),
    line(Xs).

line([]) --> line_ending.

blank_line -->
    " ",
    blank_line.
blank_line -->
    "\t",
    blank_line.
blank_line --> line_ending.

whitespace_char --> " " | "\t" | "\n" | "\r".
whitespace -->
    whitespace_char,
    whitespace.
whitespace -->
    whitespace_char.

space --> " ".
spaces --> space, spaces.
spaces --> [].

% Leaf blocks
zero_to_three_spaces -->
    ( "" | " " | "  " | "   ").

thematic_break("<hr>") -->
    zero_to_three_spaces,
    "*",
    spaces,
    "*",
    spaces,
    "*",
    spaces,
    ( [] | thematic_break_("*") ).

thematic_break("<hr>") -->
    zero_to_three_spaces,
    "-",
    spaces,
    "-",
    spaces,
    "-",
    spaces,
    ( [] | thematic_break_("-") ).

thematic_break("<hr>") -->
    zero_to_three_spaces,
    "_",
    spaces,
    "_",
    spaces,
    "_",
    spaces,
    ( [] | thematic_break_("_") ).

thematic_break_(X) -->
    X,
    spaces,
    ([] | thematic_break_(X) ).

atx_heading(Html) -->
    zero_to_three_spaces,
    "# ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h1>~s</h1>", [Title]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "## ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h2>~s</h2>", [Title]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "### ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h3>~s</h3>", [Title]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "#### ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h4>~s</h4>", [Title]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "##### ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h5>~s</h5>", [Title]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "###### ",
    spaces,
    line_chars(Title),
    {
	phrase(format_("<h6>~s</h6>", [Title]), Html)
    }.

setext_heading("h1") -->
    "=".
setext_heading("h1") -->
    "=",
    setext_heading("h1").

setext_heading("h2") -->
    "-".
setext_heading("h2") -->
    "-",
    setext_heading("h2").

look_ahead(T), [T] --> [T].

backslash_escapes(Output) --> "\\\"", backslash_escapes(Xs), { append("&quot;", Xs, Output) }.
backslash_escapes(Output) --> "\\&", backslash_escapes(Xs), { append("&amp;", Xs, Output) }.
backslash_escapes(Output) --> "\\<", backslash_escapes(Xs), { append("&lt;", Xs, Output) }.
backslash_escapes(Output) --> "\\>", backslash_escapes(Xs), { append("&gt;", Xs, Output) }.
backslash_escapes([X|Xs]) --> "\\", [X], { member(X, "!#$%'()*+,-./:;=?@[\\]^_`{|}~") }, backslash_escapes(Xs).
backslash_escapes([\,X|Xs]) --> "\\", [X], backslash_escapes(Xs).
backslash_escapes([X|Xs]) --> [X], backslash_escapes(Xs).
backslash_escapes([]) --> [].

% TODO: Emphasis with _
% TODO: Tests

emphasis([' ',*,' '|Html0]) -->
    " * ",
    !,
    emphasis(Html0).

emphasis([\,*|Html0]) -->
    "\\*",
    !,
    emphasis(Html0).

emphasis(Html) -->
    "*",
    look_ahead(T),
    {
	\+ T = (*)
    },
    emphasis_low_end(Html0),
    !,
    emphasis(Html1),
    {
	phrase(format_("<em>~s</em>~s", [Html0, Html1]), Html)
    }.

emphasis(Html) -->
    "**",
    emphasis_high_end(Html0),
    !,
    emphasis(Html1),
    {
	phrase(format_("<strong>~s</strong>~s", [Html0, Html1]), Html)
    }.

emphasis([X|Html0]) -->
    [X],
    emphasis(Html0).

emphasis([]) --> [].

emphasis_low_end([' ',*,' '|Html0]) -->
    " * ",
    !,
    emphasis_low_end(Html0).

emphasis_low_end([\,*|Html0]) -->
    "\\*",
    !,
    emphasis_low_end(Html0).

emphasis_low_end("") -->
    "*",
    look_ahead(T),
    {
	\+ T = (*)
    },
    !.

emphasis_low_end(Html) -->
    "**",
    emphasis_low_high_end(Html0),
    !,
    emphasis_low_end(Html1),
    {
	phrase(format_("<strong>~s</strong>~s", [Html0, Html1]), Html)
    }.

emphasis_low_end("") -->
    "*".

emphasis_low_end([X|Html0]) -->
    [X],
    emphasis_low_end(Html0).

emphasis_high_low_end([' ',*,' '|Html0]) -->
    " * ",
    !,
    emphasis_high_low_end(Html0).

emphasis_high_low_end([\,*|Html0]) -->
    "\\*",
    !,
    emphasis_high_low_end(Html0).

emphasis_high_low_end("") -->
    "*",
    look_ahead(T),
    {
	\+ T = (*)
    },
    !.

emphasis_high_low_end("") -->
    "*".

emphasis_high_low_end([X|Html0]) -->
    [X],
    emphasis_high_low_end(Html0).

emphasis_high_end([' ',*,' '|Html0]) -->
    " * ",
    !,
    emphasis_high_end(Html0).

emphasis_high_end([\,*|Html0]) -->
    "\\*",
    !,
    emphasis_high_end(Html0).

emphasis_high_end(Html) -->
    "*",
    look_ahead(T),
    {
	\+ T = (*)
    },
    emphasis_high_low_end(Html0),
    !,
    emphasis_high_end(Html1),
    {
	phrase(format_("<em>~s</em>~s", [Html0, Html1]), Html)
    }.

emphasis_high_end("") -->
    "**".

emphasis_high_end([X|Html0]) -->
    [X],
    emphasis_high_end(Html0).

emphasis_low_high_end([' ',*,' '|Html0]) -->
    " * ",
    !,
    emphasis_low_high_end(Html0).

emphasis_low_high_end([\,*|Html0]) -->
    "\\*",
    !,
    emphasis_low_high_end(Html0).

emphasis_low_high_end("") -->
    "**".

emphasis_low_high_end([X|Html0]) -->
    [X],
    emphasis_low_high_end(Html0).

markdown(Md, Html) :-
    phrase(file_as_lines(MdLines), Md),
    markdown_(MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    (
	phrase(atx_heading(Html0), MdLine)
    ;   phrase(thematic_break(Html0), MdLine)
    ),!,
    markdown_(MdLines,  Html1),
    append(Html0, Html1, Html).

markdown_([MdLine,Heading|MdLines], Html) :-
    phrase(setext_heading(Level), Heading),
    markdown_(MdLines, Html1),
    phrase(format_("<~s>~s</~s>", [Level, MdLine, Level]), Html0),
    append(Html0, Html1, Html).

markdown_([MdLine|MdLines], Html) :-
    MdLine \= [],
    markdown_(MdLine, MdLines, Html).

markdown_([], "").

% for paragraphs
markdown_(Text, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    append(Text, [' '|MdLine], NewText),
    markdown_(NewText, MdLines, Html).

markdown_(Text, [MdLine|MdLines], Html) :-
    MdLine = [],
    phrase(format_("<p>~s</p>", [Text]), Html0),
    markdown_(MdLines, Html1),
    append(Html0, Html1, Html).

markdown_(Text, [], Html) :-
    phrase(format_("<p>~s</p>", [Text]), Html).

file_as_lines([X|Xs]) -->
    line(X),
    file_as_lines(Xs).

file_as_lines([X]) -->
    line_chars(X),
    file_ending.

file_ending --> [].
file_ending --> line_ending.
file_ending --> line_ending, file_ending.    

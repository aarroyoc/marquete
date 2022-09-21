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

markdown_([MdLine|MdLines], Html) :-
    MdLine \= [],
    markdown_(MdLine, MdLines, Html).

markdown_([], "").

% for paragraphs
markdown_(Text, [MdLine|MdLines], Html) :-
    MdLine \= [],
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

test :-
    test_thematic_break,
    test_atx_heading,
    test_markdown.

test_thematic_break :-
    phrase(thematic_break("<hr>"), "  ***"),
    \+ phrase(thematic_break("<hr>"), "+++"),
    phrase(thematic_break("<hr>"), "  - -   -"),
    \+ phrase(thematic_break("<hr>"), "__++"),
    phrase(thematic_break("<hr>"), "***********").

test_atx_heading :-
    phrase(atx_heading("<h1>Título</h1>"), "# Título"),
    \+ phrase(atx_heading("<h1>Título</h1>"), "#Título"),
    phrase(atx_heading("<h3>Título</h3>"), "  ###    Título").   

test_markdown :-
    markdown("# Marquete\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>").

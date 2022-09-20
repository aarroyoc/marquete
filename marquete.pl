:- use_module(library(dcgs)).
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

% Leaf blocks
thematic_break("<hr>") -->
    ( "" | " " | "  " | "   "),
    (line("***") | line("---") | line("___")).

test :-
    test_thematic_break.

test_thematic_break :-
    phrase(thematic_break("<hr>"), "  ***\n").

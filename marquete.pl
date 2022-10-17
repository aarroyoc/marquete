:- module(marquete, [
	      markdown/2,
	      thematic_break//1,
	      atx_heading//1,
	      setext_heading//1,
	      backslash_escapes//1,
	      emphasis//1,
	      inline_code//1,
	      inline_image//1,
	      inline_link//1,
	      inline_text/2
]).
	      

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module(inline).

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
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml), 
	phrase(format_("<h1>~s</h1>", [TitleHtml]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "## ",
    spaces,
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml),
	phrase(format_("<h2>~s</h2>", [TitleHtml]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "### ",
    spaces,
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml),
	phrase(format_("<h3>~s</h3>", [TitleHtml]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "#### ",
    spaces,
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml),
	phrase(format_("<h4>~s</h4>", [TitleHtml]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "##### ",
    spaces,
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml),
	phrase(format_("<h5>~s</h5>", [TitleHtml]), Html)
    }.

atx_heading(Html) -->
    zero_to_three_spaces,
    "###### ",
    spaces,
    line_chars(TitleMd),
    {
	inline_text(TitleMd, TitleHtml),
	phrase(format_("<h6>~s</h6>", [TitleHtml]), Html)
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
    inline_text(MdLine, HtmlLine),
    phrase(format_("<~s>~s</~s>", [Level, HtmlLine, Level]), Html0),
    append(Html0, Html1, Html).

markdown_([MdLine|MdLines], Html) :-
    MdLine \= [],
    markdown_(MdLine, MdLines, Html).

markdown_([], "").

% for paragraphs
markdown_(TextMd, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    append(TextMd, [' '|MdLine], NewText),
    markdown_(NewText, MdLines, Html).

markdown_(TextMd, [MdLine|MdLines], Html) :-
    MdLine = [],
    inline_text(TextMd, TextHtml),
    phrase(format_("<p>~s</p>", [TextHtml]), Html0),
    markdown_(MdLines, Html1),
    append(Html0, Html1, Html).

markdown_(TextMd, [], Html) :-
    inline_text(TextMd, TextHtml),
    phrase(format_("<p>~s</p>", [TextHtml]), Html).

file_as_lines([X|Xs]) -->
    line(X),
    file_as_lines(Xs).

file_as_lines([X]) -->
    line_chars(X),
    file_ending.

file_ending --> [].
file_ending --> line_ending.
file_ending --> line_ending, file_ending.    

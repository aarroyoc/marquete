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
	      

:- use_module(library(charsio)).
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

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

% Leaf blocks
zero_to_three_spaces -->
    spaces(N), { N < 4 }.

spaces(0) --> "".
spaces(N) --> " ", spaces(N0), { N is N0 + 1}.

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

indented_code(Code) -->
    "    ",
    code_line(Code).

indented_code(Code) -->
    "\t",
    code_line(Code).

start_code_fence(N, InfoString) -->
    backticks(N), { N >= 3 },
    seq(InfoString).

ending_code_fence(N) -->
    backticks(N),
    ... .

code_line(Html) -->
    "&",
    code_line(Html0),
    { append("&amp;", Html0, Html) }.

code_line(Html) -->
    "<",
    code_line(Html0),
    { append("&lt;", Html0, Html) }.

code_line(Html) -->
    ">",
    code_line(Html0),
    { append("&gt;", Html0, Html) }.

code_line([X|Html0]) -->
    [X],
    code_line(Html0).

code_line("") --> [].

blockquote_line(Text) -->
    "> ",
    seq(Text).

list_line(unordered, N, X, Text) --> ulist_line(N, X, Text).
list_line(ordered, N, X, Text) --> olist_line(N, X, Text).

ulist_line(N, X, Text) -->
    spaces(N),[X]," ", { N < 4, member(X, "*+-") }, seq(Text).

olist_line(N, X, Text) -->
    spaces(N),number_(_),[X], { N < 4, member(X, ".)") }, seq(Text).

start_html_line -->
    "<", letters(_), ... , ">", ... .

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
    phrase(start_code_fence(N, _InfoString), MdLine),!,
    markdown_code_fence(N, "", MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    phrase(indented_code(Code0), MdLine),!,
    markdown_code_indent(Code0, MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    phrase(blockquote_line(Line), MdLine),!,
    markdown_blockquote([Line], MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    phrase(list_line(Mode, Indent, Char, Text), MdLine),!,
    markdown_list(Mode, Indent, Char, [[Text]], MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    phrase(start_html_line, MdLine),!,
    markdown_html(MdLine, MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    MdLine \= [],
    markdown_(MdLine, MdLines, Html).

markdown_([MdLine|MdLines], Html) :-
    MdLine = [],
    markdown_(MdLines, Html0),
    append("<br>", Html0, Html).

markdown_([], "").

% for block-html
% another line
markdown_html(Html0, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    append(Html0, "\n", Html1),
    append(Html1, MdLine, Html2),
    markdown_html(Html2, MdLines, Html).

% blank line
markdown_html(Html0, [MdLine|MdLines], Html) :-
    MdLine = [],
    markdown_(MdLines, Html1),
    append(Html0, Html1, Html).

% no more lines
markdown_html(Html, [], Html).

% for lists
% new item
markdown_list(Mode, Indent, Char, Items0, [MdLine|MdLines], Html) :-
    phrase(list_line(Mode, Indent, Char, Text), MdLine),!,
    append(Items0, [[Text]], Items),
    markdown_list(Mode, Indent, Char, Items, MdLines, Html).

% add content to current item
markdown_list(Mode, Indent, Char, Items0, [MdLine|MdLines], Html) :-
    phrase((spaces(N), seq(Text)), MdLine),
    N >= Indent + 1,
    !,
    append(Items00, [Item], Items0),
    append(Item, [Text], NewItem),
    append(Items00, [NewItem], Items),
    markdown_list(Mode, Indent, Char, Items, MdLines, Html).

% end of list
markdown_list(ordered, Indent, _Char, Items, [MdLine|MdLines], Html) :-
    phrase((spaces(N), ... ), MdLine), N < Indent + 1,!,
    maplist(markdown_list_items_, Items, ItemsInnerHtml),
    append(ItemsInnerHtml, ItemsHtml),
    markdown_([MdLine|MdLines], Html0),
    phrase(format_("<ol>~s</ol>~s", [ItemsHtml, Html0]), Html).

markdown_list(unordered, Indent, _Char, Items, [MdLine|MdLines], Html) :-
    phrase((spaces(N), ... ), MdLine), N < Indent + 1,!,
    maplist(markdown_list_items_, Items, ItemsInnerHtml),
    append(ItemsInnerHtml, ItemsHtml),
    markdown_([MdLine|MdLines], Html0),
    phrase(format_("<ul>~s</ul>~s", [ItemsHtml, Html0]), Html). 
    

% end of lines
markdown_list(ordered, _Indent, _Char, Items, [], Html) :-
    maplist(markdown_list_items_, Items, ItemsInnerHtml),
    append(ItemsInnerHtml, ItemsHtml),
    phrase(format_("<ol>~s</ol>", [ItemsHtml]), Html).

markdown_list(unordered, _Indent, _Char, Items, [], Html) :-
    maplist(markdown_list_items_, Items, ItemsInnerHtml),
    append(ItemsInnerHtml, ItemsHtml),
    phrase(format_("<ul>~s</ul>", [ItemsHtml]), Html).

markdown_list_items_(Item, Html) :-
    markdown_(Item, InnerHtml),
    phrase(format_("<li>~s</li>", [InnerHtml]), Html).

% for non-lazy blockquotes
% new line
markdown_blockquote(Blockquote0, [MdLine|MdLines], Html) :-
    phrase(blockquote_line(Line), MdLine),!,
    append(Blockquote0, [Line], Blockquote),
    markdown_blockquote(Blockquote, MdLines, Html).

% end of blockquote
markdown_blockquote(Blockquote, MdLines, Html) :-
    markdown_(Blockquote, Html0),
    markdown_(MdLines, Html1),
    phrase(format_("<blockquote>~s</blockquote>~s", [Html0, Html1]), Html).

% end of lines
markdown_blockquote(Blockquote, [], Html) :-
    markdown_(Blockquote, Html0),
    phrase(format_("<blockquote>~s</blockquote>", [Html0]), Html).

% for indented code blocks
markdown_code_indent(Code0, [MdLine|MdLines], Html) :-
    phrase(indented_code(Code1), MdLine),
    append(Code0, ['\n'|Code1], Code),
    markdown_code_indent(Code, MdLines, Html).

markdown_code_indent(Code, [MdLine|MdLines], Html) :-
    \+ phrase(indented_code(_), MdLine),
    phrase(format_("<pre><code>~s</code></pre>", [Code]), Html0),
    markdown_([MdLine|MdLines], Html1),
    append(Html0, Html1, Html).

markdown_code_indent(Code, [], Html) :-
    phrase(format_("<pre><code>~s</code></pre>", [Code]), Html).

% for fenced code blocks
markdown_code_fence(_N, Code, [], Html) :-
    phrase(format_("<pre><code>~s</code></pre>", [Code]), Html).

markdown_code_fence(N, Code, [MdLine|MdLines], Html) :-
    phrase(ending_code_fence(N), MdLine),
    !,
    phrase(format_("<pre><code>~s</code></pre>", [Code]), Html0),
    markdown_(MdLines, Html1),
    append(Html0, Html1, Html).

markdown_code_fence(N, Code0, [MdLine|MdLines], Html) :-
    phrase(code_line(CodeLine), MdLine),
    append(Code0, ['\n'|CodeLine], Code),
    markdown_code_fence(N, Code, MdLines, Html).

% for paragraphs
markdown_(TextMd, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    \+ phrase(start_code_fence(_, _), MdLine),
    \+ phrase(list_line(_,_, _, _), MdLine),
    append(TextMd, [' '|MdLine], NewText),
    markdown_(NewText, MdLines, Html).

% code fence blocks can interrupt a paragraph
markdown_(TextMd, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    phrase(start_code_fence(_, _), MdLine),
    inline_text(TextMd, TextHtml),
    phrase(format_("<p>~s</p>", [TextHtml]), Html0),
    markdown_([MdLine|MdLines], Html1),
    append(Html0, Html1, Html).

% lists can interrupt a paragraph
markdown_(TextMd, [MdLine|MdLines], Html) :-
    MdLine = [_|_],
    phrase(list_line(_, _, _, _), MdLine),
    inline_text(TextMd, TextHtml),
    phrase(format_("<p>~s</p>", [TextHtml]), Html0),
    markdown_([MdLine|MdLines], Html1),
    append(Html0, Html1, Html).

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

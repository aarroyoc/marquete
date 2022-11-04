# Marquete
[Markdown](https://daringfireball.net/projects/markdown/) for Prolog. It implements the basic stuff of Markdown using ISO Prolog predicates and DCGs. It doesn't have the intention to be 100% compatible with `Markdown.pl` or *CommonMark*.

## Usage
```
:- use_module(marquete).

?- markdown(+InputStr, -OutputHtml).
```

where InputStr is a string in Markdown format, and OutputHtml unifies with the output HTML the file has produced.

### Example
Let's render this README with Marquete!
```
:- use_module(marquete).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

?- phrase_from_file(seq(Markdown), "README.md"), markdown(Markdown, HTML), phrase_to_file(seq(HTML), "README.html").
```

Runs on:

* [Scryer Prolog](https://github.com/mthom/scryer-prolog)
* [Trealla Prolog](https://github.com/trealla-prolog/trealla)

Supported stuff:

* Paragraphs
* Thematic breaks
* ATX headings
* Setext headings
* Indented code blocks
* Fenced code blocks
* HTML blocks
* Blank lines
* Blockquotes
* Ordered lists
* Unordered lists
* Backslash escapes
* HTML Entities
* Code spans
* Emphasis and strong emphasis
* Links (only inline)
* Images (only inline)
* Raw HTML


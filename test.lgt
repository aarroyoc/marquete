:- use_module(marquete).

:- object(test, extends(lgtunit)).

test(thematic_break) :-
    phrase(marquete:thematic_break("<hr>"), "  ***"),
    \+ phrase(marquete:thematic_break("<hr>"), "+++"),
    phrase(marquete:thematic_break("<hr>"), "  - -   -"),
    \+ phrase(marquete:thematic_break("<hr>"), "__++"),
    phrase(marquete:thematic_break("<hr>"), "***********").    

test(atx_heading) :-
    phrase(marquete:atx_heading("<h1>Título</h1>"), "# Título"),
    \+ phrase(marquete:atx_heading("<h1>Título</h1>"), "#Título"),
    phrase(marquete:atx_heading("<h3>Título</h3>"), "  ###    Título").

test(setext_heading) :-
    phrase(marquete:setext_heading("h1"), "==="),
    phrase(marquete:setext_heading("h2"), "---").

test(backlash_escapes) :-
    phrase(marquete:backslash_escapes("A!\\27#$"), "A\\!\\27\\#\\$"),
    phrase(marquete:backslash_escapes("!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~"), "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\]\\^\\_\\`\\{\\|\\}\\~").

test(emphasis) :-
    phrase(marquete:emphasis("Hola <em>amigos</em>"), "Hola *amigos*"),
    phrase(marquete:emphasis("Hola *amigos"), "Hola *amigos"),
    phrase(marquete:emphasis("Hola * amigos"), "Hola * amigos"),
    phrase(marquete:emphasis("Hola ami<em>gos</em>"), "Hola ami*gos*"),
    phrase(marquete:emphasis("Hola ami<strong>gos</strong>"), "Hola ami**gos**"),
    phrase(marquete:emphasis("<em>H<strong>o</strong>la</em> <strong><em>amigos</em></strong>"), "*H**o**la* ***amigos***"),
    phrase(marquete:emphasis("<em>H<strong>o</strong>la</em> <em><strong>am</strong>i<strong>gos</strong></em>"), "_H**o**la_ *__am__i__gos__*").

test(inline_code) :-
    phrase(marquete:inline_code("<code>Hola` amigos</code>"), "``Hola` amigos``"),
    phrase(marquete:inline_code("Please don't use any <code>&lt;blink&gt;</code> tags."), "Please don't use any `<blink>` tags.").

test(inline_image) :-
    phrase(marquete:inline_image("Here in Paris <img alt=\"Alt text\" src=\"/path/to/img.jpg\"> so happy!"), "Here in Paris ![Alt text](/path/to/img.jpg) so happy!").

test(inline_link) :-
    phrase(marquete:inline_link("Here in Paris <a href=\"/path/to/img.jpg\">Alt text</a> so happy!"), "Here in Paris [Alt text](/path/to/img.jpg) so happy!").

test(inline_text) :-
    marquete:inline_text("Hi [fr*i*ends](/friends) of _Pro**log**_! Let's go to ![paris](paris.png) \\& have some ``fun``", "Hi <a href=\"/friends\">fr<em>i</em>ends</a> of <em>Pro<strong>log</strong></em>! Let\'s go to <img alt=\"paris\" src=\"paris.png\"> &amp; have some <code>fun</code>").

test(markdown) :-
    marquete:markdown("# Marquete\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>"),
    marquete:markdown("Marquete\n=====\nWelcome to\nMarquete\n\nSay **Hi!** to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say <strong>Hi!</strong> to Marquete</p><hr>"),
    marquete:markdown("# Marquete [Code](/code)\nSee this HTML code:\n```html\n<!DOCTYPE html>\n```\nVery easy!", "<h1>Marquete <a href=\"/code\">Code</a></h1><p>See this HTML code:</p><pre><code>\n&lt;!DOCTYPE html&gt;</code></pre><p>Very easy!</p>"),
    marquete:markdown("# Marquete [Code](/code)\nSee this Bash code:\n\n    cp file.a file.b\n    mv file.b file.a", "<h1>Marquete <a href=\"/code\">Code</a></h1><p>See this Bash code:</p><pre><code>cp file.a file.b\nmv file.b file.a</code></pre>"),
    marquete:markdown("Hola\n\n> Hola\n> \n> > *Adios*\n> Hola\nNo me parece bien", "<p>Hola</p><blockquote><p>Hola</p><blockquote><p><em>Adios</em></p></blockquote><p>Hola</p></blockquote><p>No me parece bien</p>"),
    marquete:markdown("1) 1\n * 2\n  * 3\n* Hola", "<ol><li><p> 1</p><ul><li><p>2</p><ul><li><p>3</p></li></ul></li></ul></li></ol><ul><li><p>Hola</p></li></ul>"),
    marquete:markdown("* 1\n * 2\n  * 3\n* Hola", "<ul><li><p>1</p><ul><li><p>2</p><ul><li><p>3</p></li></ul></li></ul></li><li><p>Hola</p></li></ul>"),
    marquete:markdown("<div>\nAT&T</div>\n\n# Hola AT&T", "<div>\nAT&T</div><h1>Hola AT&amp;T</h1>"),
    marquete:markdown("Prueba [Teruel](https://github.com/aarroyoc/teruel/) y/o [Marquete](https://github.com/aarroyoc/marquete/)", "<p>Prueba <a href=\"https://github.com/aarroyoc/teruel/\">Teruel</a> y/o <a href=\"https://github.com/aarroyoc/marquete/\">Marquete</a></p>").

:- end_object.

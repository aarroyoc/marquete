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
    

test(markdown) :-
    marquete:markdown("# Marquete\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>"),
    marquete:markdown("Marquete\n=====\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>").

:- end_object.
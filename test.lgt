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

test(markdown) :-
    marquete:markdown("# Marquete\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>"),
    marquete:markdown("Marquete\n=====\nWelcome to\nMarquete\n\nSay Hi! to Marquete\n\n***\n", "<h1>Marquete</h1><p>Welcome to Marquete</p><p>Say Hi! to Marquete</p><hr>").

:- end_object.
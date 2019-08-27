# The tricky shit

Write down readme's for yourself asshole so you can remember 
what avenues are exhausted and which aren't yet explored.
And solutions and shit.

## Overview

This is based on a template + re-frame's generated template + other stuff.
When `lein repl / fireplace` doesn't work it's prob just wrong connection
sequence.  Most foolproof is open `lein repl`, require empty core.clj
require core.cljs require what actually working on

Most important don't assume connection is broken just because errors
appear.  Got stuck for hours and hours due to that. Tooling is buggy, try
evaling other forms -like basic println (except right after
``(enable-console-print)`` cause then get errors about that lol

/dev/user.clj has som figwheel-sidecar cmds to start figwheel
also standalone cljs-repl (bit useless)

Nuking profiles.clj (even tho its not looking suspicious... and why do things come and go?)
and killing all running java processes eventually got things up and running
after once more not working for days

ALSO BEWARE NPM
kept `lein repl` from launching despite everything related commented out in project.clj
no proper error msg, just a package.json etc in dir

## License

MY LAW

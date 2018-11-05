#!/bin/sh

which yesod
if test $? -ne 0; then
    stack build yesod-bin
fi

stack clean

PGUSER=hs-editorialcms \
    PGPASS=hs-editorialcms \
    PGDATABASE=hs-editorialcms \
    stack exec -- yesod devel

#!/bin/sh

# which yesod
# if test $? -ne 0; then
#     stack build yesod-bin
# fi

stack clean

PGUSER=editorialcms \
    PGPASS=editorialcms \
    PGDATABASE=editorialcms \
    stack exec -- yesod devel

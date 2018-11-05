#!/bin/sh

uname -a | grep -q Darwin
if test $? -eq 0; then
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hs-editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createdb --encoding=UTF-8 --owner=hs-editorialcms --template=template0 hs-editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/psql -U hs-editorialcms hs-editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/dropdb hs-editorialcms'

    sudo su postgres -c 'EDITOR=emacs /opt/local/lib/postgresql95/bin/psql -U hs-editorialcms hs-editorialcms'
fi


uname -a | grep -q Ubuntu
if test $? -eq 0; then
    # sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hs-editorialcms'
    # sudo su postgres -c 'createdb --encoding=UTF-8 --owner=hs-editorialcms --template=template0 hs-editorialcms'
    sudo su postgres -c 'EDITOR=emacs psql -U hs-editorialcms hs-editorialcms'
fi


uname -a | grep -q NixOS
if test $? -eq 0; then
  # createuser -U postgres --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hs-editorialcms
  # createdb -U postgres --encoding=UTF-8 --owner=hs-editorialcms --template=template0 hs-editorialcms
  psql -U hs-editorialcms hs-editorialcms
fi

#!/bin/sh

uname -a | grep -q Darwin
if test $? -eq 0; then
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createdb --encoding=UTF-8 --owner=editorialcms --template=template0 editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/psql -U editorialcms editorialcms'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/dropdb editorialcms'

    sudo su postgres -c 'EDITOR=emacs /opt/local/lib/postgresql95/bin/psql -U editorialcms editorialcms'
fi


uname -a | grep -q Ubuntu
if test $? -eq 0; then
    # sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication editorialcms'
    # sudo su postgres -c 'createdb --encoding=UTF-8 --owner=editorialcms --template=template0 editorialcms'
    sudo su postgres -c 'EDITOR=emacs psql -U editorialcms editorialcms'
fi


uname -a | grep -q NixOS
if test $? -eq 0; then
  # createuser -U postgres --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication editorialcms
  # createdb -U postgres --encoding=UTF-8 --owner=editorialcms --template=template0 editorialcms
  psql -U editorialcms editorialcms
fi

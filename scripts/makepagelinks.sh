#!/bin/zsh

# Make a list of pagelinks

pagelinksfile=$1

ls pages | grep '.md$' | while read page; do
    name=`echo $page | sed 's:^\([0-9 -]*[0-9\: ]*\)\(.*\)\.md:\2:'`
    link=`echo $name  | tr " " "-" | tr -c -d "[:alnum:]" | tr "[:upper:]" "[:lower:]"`.html
    echo "<li><a href=\"$link\" title=\"$name\">$name</a></li>" >> $pagelinksfile
done

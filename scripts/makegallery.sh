#!/bin/zsh

# Make a screenshot gallery

output=$1
tempfile=`pwd`/$1.tmp
sidebarfile=$2

# Add all computers
pushd screenshots
for computer in *; do
    if [[ -d $computer ]]; then
        echo "<h2 class=\"gallery\">$computer</h2><ol class=\"gallery\">" >> $tempfile
        
        pushd $computer/fullsize
        ls | grep png$ | sed 's/.png//' | sort -r | while read date; do
            echo "<li><a href=\"/screenshots/$computer/$date.html\"><img src=\"/screenshots/$computer/thumb/$date.png\" alt=\"$date\"/></a></li>" >> $tempfile
        done
        popd
        
        echo "</ol>" >> $tempfile
    fi
done
popd

# Make the item template
permalink=`echo $output | sed 's:/:\\\\/:g'`
title="Gallery"
timestamp=`date +%Y-%m-%d`

sed -e "s/{permalink}/$permalink/g" \
    -e "s/{title}/$title/g" \
    -e "s/{timestamp}/$timestamp/g" \
    -e "/{source}/d" \
    -e "/{content}/r $tempfile" \
    -e "/{content}/d" \
    < template/item.html \
    > $tempfile.2

# Make the page template
gpglink=`echo $output.asc | sed 's:/:\\\\/:g'`

sed -e "s/{pagetitle}/$title/g" \
    -e "/{content}/r $tempfile.2" \
    -e "/{content}/d" \
    -e "/{sidebar}/r $sidebarfile" \
    -e "/{sidebar}/d" \
    -e "/{archiveplink}/d" \
    -e "/{archivenlink}/d" \
    -e "s/{gpglink}/$gpglink/g" \
    < template/page.html \
    > $output

rm $tempfile $tempfile.2

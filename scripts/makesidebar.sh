#!/bin/zsh

# Make the sidebar

output=$1
archivelinksfile=`mktemp`
pagelinksfile=`mktemp`

# Archivelinks
first=`ls blog | head -n1 | sed 's/\([0-9]*\)-.*/\1/'`
last=`ls blog | tail -n1 | sed 's/\([0-9]*\)-.*/\1/'`
for archive in `seq $first $last | sort -r`; do
    echo "<li><a href=\"archive/$archive.html\" title=\"$archive\">$archive</a></li>" >> $archivelinksfile
done

# Pagelinks
./scripts/makepagelinks.sh $pagelinksfile

# Build the template
sed -e "/{pagelinks}/r $pagelinksfile" \
    -e "/{pagelinks}/d" \
    -e "/{archivelinks}/r $archivelinksfile" \
    -e "/{archivelinks}/d" \
    < template/sidebar.html \
    > $output

rm $archivelinksfile $pagelinksfile

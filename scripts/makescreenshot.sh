#!/bin/zsh

# Produce a screenshot page

output=$1
tempfile=$1.tmp
computer=$2
date=$3
pagelinksfile=`mktemp`
input=screenshots/$computer/info/$date.md

if [[ ! -e $input ]]; then
    echo "Input file does not exist."
    exit 1
fi

php scripts/markdown.php $input $tempfile

if [[ ! -e $tempfile ]]; then
    echo "Markdown processing failed."
    exit 1
fi

# Add the full size link
echo "<a href=\"/screenshots/$computer/fullsize/$date.png\"><img src=\"/screenshots/$computer/thumb-big/$date.png\" alt=\"Click to view full size.\"/></a>" > $tempfile.2
cat $tempfile >> $tempfile.2
mv $tempfile.2 $tempfile

# Make the item template
permalink=`echo $output | sed 's:/:\\\\/:g'`
title="Screenshot: $computer, $date"
timestamp=$date
source=`echo "$input" | sed -e 's:/:\\\\/:g' -e 's:?:%3f:g'`

sed -e "s/{permalink}/$permalink/g" \
    -e "s/{title}/$title/g" \
    -e "s/{timestamp}/$timestamp/g" \
    -e "s/{source}/$source/g" \
    -e "/{content}/r $tempfile" \
    -e "/{content}/d" \
    < template/item.html \
    > $tempfile.2

# Make the page template
./scripts/makepagelinks.sh $pagelinksfile

sed -e "/{content}/r $tempfile.2" \
    -e "/{content}/d" \
    -e "/{pagelinks}/r $pagelinksfile" \
    -e "/{pagelinks}/d" \
    -e "/{archiveplink}/d" \
    -e "/{archivenlink}/d" \
    < template/page.html \
    > $output

rm $tempfile $tempfile.2 $pagelinksfile

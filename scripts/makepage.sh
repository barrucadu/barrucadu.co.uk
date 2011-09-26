#!/bin/zsh

# Turn a Markdown file into a HTML file, complete with template.

output=$1
tempfile=$1.tmp
input=$2
pagelinksfile=`mktemp`

if [[ ! -e $input ]]; then
    echo "Input file does not exist."
    exit 1
fi

php scripts/markdown.php $input $tempfile

if [[ ! -e $tempfile ]]; then
    echo "Markdown processing failed."
    exit 1
fi

# Make the item template
permalink=`echo $output | sed 's:/:\\\\/:g'`
title=`echo $input | sed 's:^\([a-z]*/\)\([0-9 -]*[0-9]*\:[0-9]*\) \(.*\)\.md:\3:'`
timestamp=`echo $input | sed 's:^\([a-z]*/\)\([0-9 -]*[0-9\: ]*\)\(.*\)\.md:\2:'`
source=`echo $input | sed -e 's:/:\\\\/:g' -e 's:?:%3f:g'`

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

sed -e "s/{pagetitle}/$title/g" \
    -e "/{content}/r $tempfile.2" \
    -e "/{content}/d" \
    -e "/{pagelinks}/r $pagelinksfile" \
    -e "/{pagelinks}/d" \
    -e "/{archiveplink}/d" \
    -e "/{archivenlink}/d" \
    < template/page.html \
    > $output

rm $tempfile $tempfile.2 $pagelinksfile

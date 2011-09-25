#!/bin/zsh

# Make the entire blog - static HTML files and Atom feed.

# Blog entries
echo "Blog entries"
for file in blog/*.md; do
    outfile=`echo $file | sed 's:md$:html:' | tr " " "-" | tr -c -d "[:alnum:]-/.:" | tr "[:upper:]" "[:lower:]"`
    ./scripts/makepage.sh $outfile $file
done

# Pages
echo "Pages"
for file in pages/*.md; do
    outfile=`echo $file | sed -e 's:md$:html:' -e 's:pages/[0-9\: -]*::' | tr " " "-" | tr -c -d "[:alnum:]-." | tr "[:upper:]" "[:lower:]"`
    ./scripts/makepage.sh $outfile $file  
done

# Feed
echo "Feed"
./scripts/makefeed.sh feed.atom blog/*.md

# Archives
echo "Archives"
first=`ls blog | head -n1 | sed 's/\([0-9]*\)-.*/\1/'`
last=`ls blog | tail -n1 | sed 's/\([0-9]*\)-.*/\1/'`
for archive in `seq $first $last`; do
    prev="<a class=\"prev-page\" href=\"archive/$[ $archive - 1 ].html\" title=\"&laquo; Previous Entries\">&laquo; Previous Entries</a>"
    next="<a class=\"next-page\" href=\"archive/$[ $archive + 1].html\" title=\"Next Entries &raquo;\">Next Entries &raquo;</a>"

    if [[ $archive == $first ]]; then
        prev=""
    fi

    if [[ $archive == $last ]]; then
        next="<a class=\"next-page\" href=\"/\" title=\"Next Entries &raquo;\">Next Entries &raquo;</a>"
    fi

    files=()
    ls blog | grep md$ | grep ^$archive | sed 's:^:blog/:' | while read line; do files+=$line; done

    ./scripts/makearchive.sh archive/$archive.html $prev $next $files
done

# Index
echo "Index"
files=()
ls blog | grep md$ | sed 's:^:blog/:' | tail -n5 | while read line; do files+=$line; done
./scripts/makearchive.sh index.html "<a class=\"prev-page\" href=\"archive/$first.html\" title=\"&laquo; Previous Entries\">&laquo; Previous Entries</a>" "" $files

#!/bin/zsh

# Make the entire blog - static HTML files and Atom feed.

INDEXPOSTS=1

# Make sidebar
sidebarfile=`mktemp`
./scripts/makesidebar.sh $sidebarfile

# Get GPG password
GPGPASS=""
echo -n "Enter GPG passphrase: "
stty -echo
read GPGPASS
stty echo
echo

# Sign a file with GPG
function signfile ()
{
    file=$1
    gpg --passphrase $GPGPASS --no-tty --yes -a --detach-sign $file &>/dev/null
}

all=/bin/true

if [[ $1 != "" ]] && [[ $1 != "all" ]]; then
    all=/bin/false
fi

# Blog entries
if $all || [[ $1 == "blog" ]]; then
    [[ $1 == "blog" ]] && shift

    echo "Blog entries"
    for file in blog/*.md; do
        outfile=`echo $file | sed 's:md$:html:' | tr " " "-" | tr -c -d "[:alnum:]-/.:" | tr "[:upper:]" "[:lower:]"`
        ./scripts/makepage.sh $outfile $sidebarfile $file
        signfile $outfile
    done
fi

# Pages
if $all || [[ $1 == "pages" ]]; then
    [[ $1 == "pages" ]] && shift

    echo "Pages"
    for file in pages/*.md; do
        outfile=`echo $file | sed -e 's:md$:html:' -e 's:pages/[0-9\: -]*::' | tr " " "-" | tr -c -d "[:alnum:]-." | tr "[:upper:]" "[:lower:]"`
        ./scripts/makepage.sh $outfile $sidebarfile $file
        signfile $outfile
    done
fi

# Feed
if $all || [[ $1 == "feed" ]]; then
    [[ $1 == "feed" ]] && shift

    echo "Feed"
    ./scripts/makefeed.sh feed.atom blog/*.md
    signfile feed.atom
fi

# Archives
if $all || [[ $1 == "archives" ]]; then
    [[ $1 == "archives" ]] && shift

    echo "Archives"
    first=`ls blog | head -n1 | sed 's/\([0-9]*\)-.*/\1/'`
    last=`ls blog | tail -n1 | sed 's/\([0-9]*\)-.*/\1/'`
    for archive in `seq $first $last`; do
        prev="<a class=\"prev-page\" href=\"archive/$[ $archive - 1 ].html\" title=\"&laquo; Previous Entries\">&laquo; Previous Entries</a>"
        next="<a class=\"next-page\" href=\"archive/$[ $archive + 1].html\" title=\"Next Entries &raquo;\">Next Entries &raquo;</a>"
        
        if [[ $archive == $first ]]; then
            prev=-
        fi
        
        if [[ $archive == $last ]]; then
            next=-
        fi
        
        files=()
        ls blog | grep md$ | grep ^$archive | sed 's:^:blog/:' | while read line; do files+=$line; done
        
        ./scripts/makearchive.sh archive/$archive.html $sidebarfile $prev $next $files
        signfile archive/$archive.html
    done
fi

# Screenshots
if $all || [[ $1 == "screenshots" ]]; then
    [[ $1 == "screenshots" ]] && shift

    echo "Screenshots"
    pushd screenshots
    for computer in *; do
        if [[ -d $computer ]]; then
            pushd $computer/info
            for info in *.md; do
                date=`echo $info | sed 's:.md::'`
                pushd ../../..
                ./scripts/makescreenshot.sh screenshots/$computer/$date.html $sidebarfile $computer $date
                signfile screenshots/$computer/$date.html
                popd
            done
            popd
        fi
    done
    popd
fi

# Gallery
if $all || [[ $1 == "gallery" ]]; then
    [[ $1 == "gallery" ]] && shift

    echo "Gallery"
    ./scripts/makegallery.sh screenshots/index.html $sidebarfile
    signfile screenshots/index.html
fi

# Index
if $all || [[ $1 == "index" ]]; then
    [[ $1 == "index" ]] && shift

    echo "Index"
    files=()
    ls blog | grep md$ | sed 's:^:blog/:' | tail -n$INDEXPOSTS | while read line; do files+=$line; done
    ./scripts/makearchive.sh index.html $sidebarfile - - $files
    signfile index.html
fi

rm $sidebarfile
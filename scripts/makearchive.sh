#!/bin/zsh

# Turn a list of Markdown files into a single HTML file, complete with template.

output=$1
plink=$2
nlink=$3
pagelinksfile=`mktemp`
plinkfile=`mktemp`
nlinkfile=`mktemp`

cp template/page.html $output

shift
shift
shift

inputs=$@

while [[ $1 != "" ]]; do
    input=$1
    tempfile=$1.tmp

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
    permalink=`echo $input | sed 's:md$:html:' | tr " " "-" | tr -c -d "[:alnum:]-/.:" | tr "[:upper:]" "[:lower:]" | sed 's:/:\\\\/:g'`
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


    # Update the page template
    sed -i \
        -e "/{content}/r $tempfile.2" \
        $output

    rm $tempfile $tempfile.2

    shift
done

# Finish the template
./scripts/makepagelinks.sh $pagelinksfile

echo $plink > $plinkfile
echo $nlink > $nlinkfile

if [[ $plink == "-" ]]; then
    echo "" > $plinkfile
fi

if [[ $nlink == "-" ]]; then
    echo "" > $nlinkfile
fi

atitle=Archive

if [[ $output == "index.html" ]]; then
    atitle="Michael Walker"
fi

sed -i \
    -e "s/{pagetitle}/$atitle/g" \
    -e "/{content}/d" \
    -e "/{pagelinks}/r $pagelinksfile" \
    -e "/{pagelinks}/d" \
    -e "/{archiveplink}/r $plinkfile" \
    -e "/{archiveplink}/d" \
    -e "/{archivenlink}/r $nlinkfile" \
    -e "/{archivenlink}/d" \
    $output

rm $pagelinksfile $plinkfile $nlinkfile

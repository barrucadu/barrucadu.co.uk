#!/bin/zsh

# Turn a list of Markdown files into a single Atom file, complete with template.

output=$1

cp template/feed.atom $output

shift

timestamp=""
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

    # Tidy HTML
    sed -i \
        -e 's/\&/\&amp;/g' \
        -e 's/</\&lt;/g' \
        -e 's/>/\&gt;/g' \
        -e 's/"/\&quot;/g' \
        $tempfile

    # Make the item template
    permalink=`echo $input | sed 's:md$:html:' | tr " " "-" | tr -c -d "[:alnum:]-/.:" | tr "[:upper:]" "[:lower:]" | sed 's:/:\\\\/:g'`
    title=`echo $input | sed 's:^\([a-z]*/\)\([0-9 -]*[0-9]*\:[0-9]*\) \(.*\)\.md:\3:'`
    timestamp=`echo $input | sed 's:^[a-z]*/\([0-9-]*\) \([0-9\:]*\) .*:\1T\2Z:'`
    
    sed -e "s/{permalink}/http:\/\/www.barrucadu.co.uk\/$permalink/g" \
        -e "s/{title}/$title/g" \
        -e "s/{timestamp}/$timestamp/g" \
        -e "/{content}/r $tempfile" \
        -e "/{content}/d" \
        < template/item.atom \
        > $tempfile.2


    # Update the feed template
    sed -i \
        -e "/{content}/r $tempfile.2" \
        $output

    rm $tempfile $tempfile.2

    shift
done

# Finish the template
sed -i \
    -e "s/{updated}/$timestamp/" \
    -e "/{content}/d" \
    $output

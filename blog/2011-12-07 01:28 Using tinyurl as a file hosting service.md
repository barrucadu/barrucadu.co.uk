[tinyurl](http://www.tinyurl.com) is a popular URL shortening service with two properties that make it possible to store files on:

1. It allows base64-encoded data URIs
2. It has a very long URI character limit

And so, I have created two scripts, `tinyupload` and `tinydownload` that can store files on tinyurl. Files are stored in chunks of about 4KB (3500 bytes after compression using either gzip, bzip2, or xz), so this isn't very suitable for anything other than rather small files, but it was an interesting little problem to solve. The scripts are available [here](https://github.com/Barrucadu/home/blob/master/bin/tinyupload) and [here](https://github.com/Barrucadu/home/blob/master/bin/tinydownload).

### Usage

The `tinyupload` script takes a filename, and an optional compression method (gzip, bzip2, or xz, default is gzip). The `tinydownload` script takes an index file URL (produced by tinyupload).

    $ tinyupload $file xz
    $ tinydownload $uri

As an example, here is part of a recent formative assessment I did:

    $ tinydownload http://tinyurl.com/6vf5w4j

### tinyupload
    #!/bin/zsh
    
    # Number of bytes for each chunk
    CHUNKSIZE=3500
    
    # Default compression program
    ZIP=gzip
    
    # Mime type for chunks
    CHUNKMIME="application/octet-stream"
    
    # Mime type for index file
    INDEXMIME="text/plain"
    
    # File to upload
    FILE=$1
    
    # Check for the presence of the file
    if [[ ! -e $FILE ]]; then
        echo "$FILE does not exist."
        exit 1
    fi
    
    # Check of an alternative compression program has been given
    if [[ $2 == bzip2 ]] || [[ $2 == xz ]]; then
        ZIP=$2
        echo "Using $ZIP for compression."
    fi
    
    # Make temporary files
    tmpfile=`mktemp`
    indexfile=$tmpfile.index
    
    # Save initial filename and compression method to the index file
    echo $(basename $FILE) > $indexfile
    echo $ZIP >> $indexfile
    
    # Compress the file
    echo "Compressing $FILE"
    $ZIP $FILE -c > $tmpfile
    
    # Split into chunks
    split -b $CHUNKSIZE $tmpfile $tmpfile-
    
    # For each chunk, base64 encode and upload to tinyurl as a data uri
    echo "Uploading $FILE"
    for tfile in $tmpfile-*; do
        base64 -w0 $tfile > $tfile.b64
    
        wget "http://tinyurl.com/api-create.php?url=data:$CHUNKMIME;base64,`cat $tfile.b64`" -q -O - >> $indexfile
        echo >> $indexfile
    done
    
    # Always compress the index file with gzip
    gzip $indexfile
    base64 -w0 $indexfile.gz > $indexfile.gz.b64
    
    # Upload the index file
    echo "Uploading index file"
    url=$(wget "http://tinyurl.com/api-create.php?url=data:$INDEXMIME;base64,`cat $indexfile.gz.b64`" -q -O -)
    
    # Link to the index file
    echo "Uploaded, index file is at $url"
    
    # Clean up
    rm $indexfile* $tmpfile* &>/dev/null

### tinydownload
    #!/bin/zsh
    
    # Mime type for chunks
    CHUNKMIME="application/octet-stream"
    
    # Mime type for index file
    INDEXMIME="text/plain"
    
    # Index file
    INDEX=$1
    
    # Make temporary files
    tmpfile=`mktemp`
    indexfile=$tmpfile.index
    
    # Download index file
    wget $INDEX 2>&1 | \
        grep "data:$INDEXMIME;base64," | \
        sed "s%data:$INDEXMIME;base64,%%" | \
        tail -n1 | \
        sed "s/: Unsupported scheme \`data'.//" > $indexfile.b64
    
    # Decode index file (always using gzip)
    base64 -d $indexfile.b64 > $indexfile.gz
    gunzip $indexfile.gz
    
    # Get the filename and compression program
    file=`head -n1 < $indexfile`
    zip=`head -n2 < $indexfile | tail -n1`
    
    if [[ $zip != gzip ]] && [[ $zip != bzip2 ]] && [[ $zip != xz ]]; then
        echo "Unknown compression method, aborting."
        exit 1
    fi
    
    # Make temporary file
    echo "Downloading $file"
    touch $file.zip
    
    # For every chunk, download, decode, and cat to the temporary file
    sed '1,2d' <$indexfile | \
    while read url; do
        wget $url 2>&1 | \
            grep "data:$CHUNKMIME;base64," | \
            sed "s%data:$CHUNKMIME;base64,%%" | \
            tail -n1 | \
            sed "s/: Unsupported scheme \`data'.//" > $tmpfile-part.b64
        
        base64 -d $tmpfile-part.b64 > $tmpfile-part
        
        cat $tmpfile-part >> $file.zip
        rm $tmpfile-part
    done
    
    # Uncompress the file
    echo "Uncompressing $file"
    [[ $zip == gzip  ]] && gunzip  -c $file.zip > $file
    [[ $zip == bzip2 ]] && bunzip2 -c $file.zip > $file
    [[ $zip == xz    ]] && xz -d   -c $file.zip > $file
    
    # Name the file
    echo "Downloaded to $file"
    
    # Clean up
    rm $indexfile* $tmpfile* $file.zip &>/dev/null

#!/bin/zsh

rsync \
    -axvzc \
    --progress \
    --stats \
    --delete \
    . \
    yuggoth:/srv/http/barrucadu.co.uk/www \
    --exclude-from=rsync.exclude

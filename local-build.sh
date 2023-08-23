#!/bin/sh

ROOT="https://misc.barrucadu.co.uk/_site/"

if [[ "$(hostname)" == "azathoth" ]]; then
  ROOT="file:///home/barrucadu/http/_site/"
fi

if command -v docker &>/dev/null; then
  docker="docker"
else
  docker="podman"
fi

$docker run -it --rm -v $(pwd):/src -v $HOME/http/_site:/build -w /src python:3.9.2 sh -c "
  pip install -r requirements.txt
  ./build --root=$ROOT --out=/build"

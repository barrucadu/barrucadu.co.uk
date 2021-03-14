#!/bin/sh

docker run -it --rm -v $(pwd):/src -v $HOME/http/_site:/build -w /src python:3.8 sh -c "
  pip install -r requirements.txt
  ./build --root=https://misc.barrucadu.co.uk/_site/ --out=/build"

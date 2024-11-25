#!/bin/sh

nix develop --command bash -c "./build --root=https://misc.barrucadu.co.uk/_site/ --out=$HOME/http/_site"

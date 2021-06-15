#!/bin/sh

docker run -v "$PWD":/src --rm -it --entrypoint=swipl swipl -q -f /src/test.pl -g test;
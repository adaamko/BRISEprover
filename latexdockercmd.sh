#!/bin/sh
IMAGE=blang/latex:ubuntu
exec docker run --rm -i --net=none -v "$PWD":/data "$IMAGE" "$@"
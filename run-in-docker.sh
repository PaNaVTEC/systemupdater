#!/bin/bash
set -euox pipefail
IFS=$'\n\t'

DOCKER_TAG='systemupdatertest'

stack build
docker build . -t $DOCKER_TAG
docker run \
       --rm \
       -it \
       $DOCKER_TAG

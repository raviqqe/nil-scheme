#!/bin/sh

set -e

directory=$(dirname $0)/../..

compile() {
  if [ -n "$STAK_HOST_INTERPRETER" ]; then
    $STAK_HOST_INTERPRETER $directory/compile.scm
  else
    stak-compile
  fi
}

while getopts l: option; do
  case $option in
  l)
    libraries="$libraries $OPTARG"
    ;;
  esac
done

shift $(expr $OPTIND - 1)

main=$1

shift 1

export PATH=$directory/target/release_test:$PATH

cat $directory/prelude.scm $libraries $main | compile >main.bc
# TODO Test a decoder.
# stak-decode <main.bc >/dev/null
stak-interpret main.bc "$@"

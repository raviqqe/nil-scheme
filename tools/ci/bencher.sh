#!/bin/sh

set -e

while getopts b:o: option; do
  case $option in
  b)
    branch=$OPTARG
    ;;
  o)
    os=$OPTARG
    ;;
  esac
done

[ -n "$branch" ]
[ -n "$os" ]

shift $(expr $OPTIND - 1)

cd $(dirname $0)/../..

cargo build --release

(
  cd cmd/minimal
  cargo build --release
)

echo PATH=$PWD/target/release:$PWD/cmd/minimal/target/release:$PATH >>$GITHUB_ENV

for directory in bench/*; do
  bencher run \
    --adapter shell_hyperfine \
    --branch $branch \
    --err \
    --file results.json \
    --github-actions $GITHUB_TOKEN \
    --project stak \
    --testbed $os \
    --threshold-max-sample-size 10 \
    --threshold-measure latency \
    --threshold-test t_test \
    --threshold-upper-boundary 0.99 \
    --thresholds-reset \
    --token $BENCHER_TOKEN \
    "hyperfine --export-json results.json -L bin stak,mstak '{bin} $directory/main.scm'"
done

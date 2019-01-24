#! /usr/bin/env bash

set -euxo pipefail

jekyll build
aws --profile camsaul.com --recursive s3 cp _site s3://camsaul.com/

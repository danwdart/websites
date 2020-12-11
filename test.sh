#!/usr/bin/env bash
stack test -j8 --ta="--format=failed-examples" > report
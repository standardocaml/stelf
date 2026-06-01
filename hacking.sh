#!/usr/bin/bash

dune build @doc @doc-private
$1 _build/default/_doc/_html/index.html
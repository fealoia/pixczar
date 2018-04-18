#!/bin/bash
export PATH=$PATH:/usr/local/opt/llvm/bin

ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis pixczar.native

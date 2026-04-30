dune clean
BISECT_FILE=$PWD/bisect dune test --instrument-with bisect_ppx
bisect-ppx-report html --coverage-path $PWD
rm ./*.coverage

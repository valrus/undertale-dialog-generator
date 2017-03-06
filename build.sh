SRC_ROOT=$PWD

pushd src/static/js

elm-make UndertaleDialog.elm --output elm.js
for f in *.js; do
    slimit $f > $SRC_ROOT/gh-pages/src/static/js/${f/js/min.js}
done
rm -f elm.js

popd

pushd src/static/css

for f in *.css; do
    python -m rcssmin < $f > $SRC_ROOT/gh-pages/src/static/css/${f/css/min.css}
done

popd
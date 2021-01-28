elm make src/Main.elm --output=elm.js

cp -rf public docs/public
cp src/*.js docs/
cp src/*.html docs/
cp elm.js docs/

git add -A
git commit -m "pushing to production"

git push -f origin master:gh-pages

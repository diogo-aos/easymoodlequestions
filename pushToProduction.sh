git checkout gh-pages
git add index.html
git commit -m $1
git push origin gh-pages
git checkout master

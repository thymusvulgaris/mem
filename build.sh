elm make src/App.elm --output=pre.js --optimize
uglifyjs pre.js > app.js
cp app.css app.js index.html dist/

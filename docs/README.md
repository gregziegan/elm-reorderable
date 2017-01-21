## Development

To run locally:

static:

`elm-make src/Main.elm --output=elm.js`

hot-reload:

`npm i -g elm-live`

`elm-live src/Main.elm --before_build='./before_build.sh' --output=elm.js --open`

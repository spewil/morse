## morse encoder in haskell

to simply run the binary, just use `./morse`

usage:
- enter lowercase letters and spaces to hear the morse code
- anything else in your input will yield a high pitch tone
- hit enter to exit

#### compile

to compile, you must have some sort of `ghc`. to get that, go [here](https://docs.haskellstack.org/en/stable/README/)

once you've got `stack`, you can simply do

```sh
	stack morse.hs
```

to run the script. this works because of the header in the source file.

#### interpret

if you'd like to make changes and work with the script's functions interactively, use

```bash
	stack ghci morse.hs
```
which will launch the interpreter and let you poke around. running `main` here will simply run the script from its normal entry point.

### ideas

- add in the full set of morse codes (upper case, shortcuts, punctuation)
- i'd really like this to work both ways using the microphone. let me know if you have ideas for this
- i'd like this to handle input more gracefully with some messages and exception handling

#### citations

- thanks to https://github.com/tsoding/haskell-music for the `ffmpeg` skeleton

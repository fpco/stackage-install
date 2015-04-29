# stackage-install

`stackage-install` provides a wrapper around the `cabal install` command, which
will download packages more securely. Initially, this means downloading over an
HTTPS connection from FP Complete's Amazon S3 mirror of Hackage, though more
hardening is planned for the future (see future improvements below).

To install, simply run `cabal update && cabal install stackage-install`. Usage
is intended to overlap well with `cabal install`. Whenever you would have run
`cabal install foo`, you can now run `stackage-install foo` (or `stk install
foo` with [stackage-cli](http://github.com/fpco/stackage-cli) installed), which
will perform the following steps:

1. Run `cabal fetch --dry-run ...` to get cabal's build plan
2. Download the relevant packages from S3, and place them in the locations that `cabal-install` expects
3. Run `cabal install ...`

## Caveats

If you have a modified `remote-repo` in your ~/.cabal/config file, this tool
will not provide proper hardening. Most users do not modify their remote-repo,
so this shouldn't be an issue most of the time.

There are some combinations of `cabal install` arguments which may not
translate well to this tool. One known issue is that passing `--dry-run` is not
supported, but others may apply as well.

This tool necessarily has to call `cabal-install` twice, once to calculate the
dependencies, and then to install them. It's theoretically possible that
`cabal-install` could come up with different build plans between the two calls,
in which case the second call may download some packages insecurely. I've
opened [cabal issue #2566](https://github.com/haskell/cabal/issues/2566) about
disabling downloading in cabal.

## Why not fix cabal?

Hopefully cabal will get fixed soon, the [discussion has already
started](https://mail.haskell.org/pipermail/cabal-devel/2015-April/010124.html).
It's unfortunately unclear how long that discussion will take, and I received a
specific request to write this tool. Since it's a small amount of code, I went
ahead with this as an interim solution.

That said, some of the future enhancements discussed below are not planned for
cabal, in which case this tool will continue to remain relevant for people
looking for additional security beyond transport security.

## Why Stackage?

See [the same question and its answer on stackage-update](https://github.com/fpco/stackage-update#why-stackage).

## Future enhancements

* Check hashes of all packages downloaded against a collection of package hashes
* Verify signatures from authors against the [signature archive](https://github.com/commercialhaskell/sig-archive)

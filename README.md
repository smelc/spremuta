# Setup

First, install [ghcup](https://www.haskell.org/ghcup/) and [direnv](https://direnv.net/)

```shell
for tool in cabal ghc hls
do
  mkdir -p bin/$tool
done

ghcup install cabal --isolate $(pwd)/bin/cabal
ghcup install ghc 9.4.7 --isolate $(pwd)/bin/ghc
ghcup install hls --isolate $(pwd)/bin/hls
```

Install `ormolu` version `0.7.3.0`:

```shell
wget https://github.com/tweag/ormolu/releases/download/0.7.3.0/ormolu-Linux.zip -O ormolu.zip
unzip ormolu.zip
mv ormolu $(pwd)/bin/.
rm ormolu.zip
```

Then `direnv allow`, so that the `.envrc` file gets loaded automatically, and you are all set.

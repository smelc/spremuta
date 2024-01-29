# Spremuta 🍊

## Setup

First, install [ghcup](https://www.haskell.org/ghcup/) and [direnv](https://direnv.net/). Then do:

```shell
for tool in cabal ghc
do
  mkdir -p bin/$tool
done

ghcup install cabal --isolate $(pwd)/bin/cabal
ghcup install ghc 9.4.7 --isolate $(pwd)/bin/ghc
```

Install `ormolu` version `0.7.3.0` (⚠️ version number also mentioned in
[.github/workflows/lint.yml](./github/workflows/lint.yml) and [./hooks/pre_commit.py](./hooks/pre_commit.py)⚠️ ):

```shell
wget https://github.com/tweag/ormolu/releases/download/0.7.3.0/ormolu-Linux.zip -O ormolu.zip
unzip ormolu.zip
mv ormolu $(pwd)/bin/.
rm ormolu.zip
```

Then `direnv allow`, so that the `.envrc` file gets loaded automatically, and you are all set 🎉

### Pre-commit hook

There's a pre-comit hook at [./hooks/pre_commit.py](./hooks/pre_commit.py). Install it as follows:

```shell
ln -sr hooks/pre_commit.py .git/hooks/pre-commit
```

It builds the project, runs tests, and autoformats files.
This allows to avoid running CI pipelines that will fail, by catching failures before pushing.

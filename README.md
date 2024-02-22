# Spremuta 🍊

## What spremuta is about?

spremuta is a tool helping me staying on top of multiple pull requests when I work.
I typically need to look at a PR when its CI is green, not before.
I also want to be notified when something is merged: it's usually time to do work in other repositories
consuming the merged change. This where spremuta enters the picture: it allows to specify simple tasks
regarding pull requests. Here are the working tasks at the moment:

```
notify when https://github.com/IntersectMBO/cardano-node/pull/5681 hasgreenci
notify when https://github.com/IntersectMBO/cardano-node/pull/5574 ismerged
notify when https://github.com/IntersectMBO/cardano-node/pull/5675 hascifinished
```

You can run spremuta in two modes:

1. In `task` mode, as in `spremuta task "notify when https://github.com/IntersectMBO/cardano-node/pull/5681 hasgreenci"`. spremuta will execute the task and exit.
1. In `daemon` mode: `spremuta daemon`

In daemon mode, spremuta reads tasks from the `spremuta.tasks` file which
is a simple lines-oriented text file like this:

```
notify when https://github.com/IntersectMBO/cardano-node/pull/5681 hasgreenci
notify when https://github.com/IntersectMBO/cardano-node/pull/5574 ismerged
```

`spremuta` reads the tasks file every minute and try to perform the first task. If it can,
it comments the task in the file (by prefixing it with `# done:`) and continues with the next file
the next minute. This way it progressively pops the list of tasks. You can edit the file concurrently if you
want to add or remove tasks.

There are options to tune spremuta's behavior which are outside the scope of this README.
Please pass `--help` to the executable to get more details about a command.

## Setup

First, install [ghcup](https://www.haskell.org/ghcup/) and [direnv](https://direnv.net/). Then do:

```shell
for tool in cabal ghc
do
  mkdir -p bin/$tool
done

ghcup install cabal --isolate $(pwd)/bin/cabal
ghcup install ghc 9.8.1 --isolate $(pwd)/bin/ghc
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

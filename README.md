[![Build Status](https://travis-ci.org/Gizra/yesoders.svg?branch=master)](https://travis-ci.org/Gizra/yesoders)

# Yesoders

> A site for [Yesod](http://www.yesodweb.com/) developers and enthusiastics

## Installation

1. Download and install local copies of all the libraries needed by using [the Stack tool](https://github.com/commercialhaskell/stack/):
    ```
    stack install yesod-bin cabal-install --install-ghc && stack build
    ```    
1. create new postgresql database (one for the site, and one for the tests):

    ```
    sudo su - postgres
    psql template1
    CREATE USER root WITH PASSWORD 'root';
    CREATE DATABASE yesoders;
    CREATE DATABASE yesoders_test;
    GRANT ALL PRIVILEGES ON DATABASE yesoders TO root;
    GRANT ALL PRIVILEGES ON DATABASE yesoders_test TO root;
    \q
    ```

## Run

```
stack exec -- yesod devel
```

### Fast Devel

The following is a way to run faster builds while in development:

1. Follow instructions on [yesod-fast-devel](https://github.com/haskellbr/yesod-fast-devel#yesod-fast-devel)
1. Execute `yesod-fast-devel` and open [http://localhost:4000](http://localhost:4000)

## Testing

Execute tests using `stack test`.

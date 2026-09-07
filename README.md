# Table of Contents

* [Plutarch Library for Common Design Patterns in Cardano Smart Contracts](#plutarch-library-for-common-design-patterns-in-cardano-smart-contracts)
  * [How to Use](#how-to-use)
  * [Provided Patterns](#provided-patterns)
    * [Stake Validator](#stake-validator)
      * [Endpoints](#endpoints)
    * [UTxO Indexers](#utxo-indexers)
      * [Singular UTxO Indexer](#singular-utxo-indexer)
        * [One-to-One](#one-to-one)
        * [One-to-Many](#one-to-many)
      * [Multi UTxO Indexer](#multi-utxo-indexer)
    * [Transaction Level Validator Minting Policy](#transaction-level-validator-minting-policy)
    * [Validity Range Normalization](#validity-range-normalization)
    * [Merkelized Validator](#merkelized-validator)

<!-- vim-markdown-toc -->

## Plutarch Library for Common Design Patterns in Cardano Smart Contracts

To help facilitate faster development of Cardano smart contracts, we present a collection of tried and tested modules and functions for implementing common design patterns.

## How to Use

Add this to your project's `cabal.project`, replacing `<commit-hash>` with
your chosen revision:

```cabal
source-repository-package
  type: git
  location: https://github.com/Anastasia-Labs/plutarch-design-patterns.git
  tag: <commit-hash>
```

Then add `plutarch-design-pattern` to `build-depends` in your `.cabal` file.
For the required compiler and dependency settings, see this library's
[cabal.project](cabal.project) at the same revision.

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment. To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

The flake declares optional binary caches in `nixConfig`. On a multi-user
Nix installation, an administrator may still need to trust the substituters
and public keys globally.

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/plutarch-design-patterns.git
```

Navigate to the repository directory:

```sh
cd plutarch-design-patterns
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop
```

The development shell uses GHC 9.6.6 and includes Cabal, HLS, fourmolu,
cabal-fmt, and the configured pre-commit hooks. Run `make help` to list the
available commands.

Build the library and test suite:

```sh
cabal build all
```

Run the tests:

```sh
cabal test --test-show-details=direct
```

Evaluate and build the checks exported by the flake:

```sh
nix flake check
```

This includes the library and test checks as well as cabal-fmt and fourmolu
formatting checks. To apply Haskell formatting locally or run every configured
pre-commit hook directly:

```sh
make format
pre-commit run --all-files
```


## Provided Patterns

### Stake Validator

This module offers two functions meant to be used within a multi-validator for implementing a "coupled" stake validator logic.

The primary application for this is the so-called "withdraw zero trick," which is most effective for validators that need to go over multiple inputs.

With a minimal spending logic (which is executed for each UTxO), and an arbitrary withdrawal logic (which is executed only once), a much more optimized script can be implemented.

#### Endpoints

`spend` merely looks for the presence of a withdrawal (with arbitrary amount) from its own reward address.

`withdraw` takes a custom logic that requires 3 arguments:

  1. Redeemer (arbitrary `PData`)
  2. Script's credential (`PCredential`)
  3. Transaction info (`PTxInfo`)

### UTxO Indexers

The primary purpose of this pattern is to offer a more optimized solution for a unique mapping between one input UTxO to one or many output UTxOs.

#### Singular UTxO Indexer

##### One-to-One

By specifying the input and output indices in the redeemer as a pair of integers, the validator can efficiently pick the input UTxO, match its output reference to make sure it's the one that's getting spent, and similarly pick the corresponding output UTxO in order to perform an arbitrary validation between the two.

##### One-to-Many

Here the validator looks for a set of outputs for the given input, through a redeemer containing an input index and a list of output indices (output indices are required to be in strictly ascending order to disallow duplicates). To make the abstraction as efficient as possible, the provided higher-order function takes 3 validation logics:

1. A function that validates the spending `Input` (single invocation).
2. A function that validates the input UTxO against a corresponding output UTxO. Note that this is executed for each associated output.
3. A function that validates the collective outputs. This also runs only once. The number of outputs is also available for this function (its second argument).

#### Multi UTxO Indexer

While the singular variant of this pattern is primarily meant for the spending endpoint of a contract, a multi UTxO indexer utilizes the stake validator provided by this package. And therefore the spending endpoint can be taken directly from `Plutarch.StakeValidator.spend`.

Subsequently, spend redeemers are irrelevant here. The redeemer of the withdrawal endpoint is expected to be a properly sorted list of pairs of indices (for the one-to-one case), or a list of one-to-many mappings of indices.

Input indices and the sequence of output indices must be strictly ascending, and every matching input must be covered.

It's worth emphasizing that it is necessary for this design to be a
multi-validator as the staking logic filters inputs that are coming from a script address which its validator hash is identical to its own.

The distinction between one-to-one and one-to-many variants here is very similar to the singular case, so please refer to [its section above](#singular-utxo-indexer) for more details.

The primary difference is that here, input indices should be provided for the _filtered_ list of inputs, i.e. only inputs from the same script, unlike the singular variant where the index applies to all the inputs of the transaction. This slight inconvenience is for preventing extra overhead on-chain.

### Transaction Level Validator Minting Policy

Very similar to the [stake validator](#stake-validator), this design pattern utilizes a multi-validator comprising of a spend and a minting endpoint.

The role of the spending input is to ensure the minting endpoint executes. It does so by looking at the mint field and making sure a non-zero amount of its asset (where its policy is the same as the multi-validator's hash, and its name is specified as a parameter) is being minted or burned.

The arbitrary logic is passed to the minting policy so that it can be executed a single time for a given transaction.

### Validity Range Normalization

The datatype that models validity range in Cardano currently allows for values that are either meaningless, or can have more than one representations. For example, since the values are integers, the inclusive flag for each end is redundant and can be omitted in favor of a predefined convention (e.g. a value should always be considered inclusive).

In this module we present a custom datatype that essentially reduces the value domain of the original validity range to a smaller one that eliminates meaningless instances and redundancies.

The datatype is defined as following:

```hs
data NormalizedTimeRange
  = ClosedRange Integer Integer
  | FromNegInf Integer
  | ToPosInf Integer
  | Always
```

The exposed function of the module (`normalizeTimeRange`) takes a
`ValidityRange` and returns this custom datatype.

### Merkelized Validator

Since transaction size is limited in Cardano, some validators benefit from a solution which allows them to delegate parts of their logics. This becomes more prominent in cases where such logics can greatly benefit from optimization solutions that trade computation resources for script sizes (e.g. table lookups can take up more space so that costly computations can be averted).

This design pattern offers an interface for off-loading such logics into an external withdrawal script, so that the size of the validator itself can stay within the limits of Cardano.

> [!NOTE]
> Since Conway, reference scripts contribute an additional fee based on their
> total size across spending and reference inputs. The fee uses tiered pricing,
> starting from a base price per byte set by protocol parameters, and is added
> to the fees for transaction size and script execution.
> See the ledger's [reference-script size calculation](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/UTxO.hs)
> and [fee calculation](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Tx.hs).

The exposed `spend` function from `Plutarch.MerkelizedValidator` expects three arguments:

1. The credential of the withdrawal validator that performs the computation.
2. The list of arguments expected by the underlying logic.
3. The redeemer map from the current transaction information.

This function expects to find a rewarding redeemer for the given credential in
the map. It validates that the redeemer is a `WithdrawRedeemer`, checks that its
input state matches the supplied arguments, and returns its output state for
the spending validator to consume.

For defining withdrawal logic that carries out the computation, use the
exposed `withdraw` function. It takes a computation from a list of generic
inputs to a list of generic outputs and returns a Plutus V3 validator of type
`PScriptContext :--> PUnit`.

The validator checks that the script is running for a rewarding purpose, reads
and validates `WithdrawRedeemer` from the context, and verifies that applying
the computation to `inputState` produces exactly `outputState`.

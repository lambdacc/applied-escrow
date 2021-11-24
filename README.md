# Applied escrow

This repo showcases a use case for escrow between a provider and consumer of a service, using Plutus.

Scenario:
- Alice is (for example) a Plutus expert . Bob needs help with Plutus. Alice is offering 60 minute blocks of time, where she offers 1:1 support on Plutus, at a rate of xx Ada/hour. Bob books an hour with Alice. When booking time, he places xx Ada in this escrow contract.

- From the start time on the contract, Alice can collect payments in tranches. This reduces counterparty risk for Alice.

- On the other hand, Bob can raise dispute at any time after the start time. If disputed, Alice cannot collect funds further. Naturally, Alice would halt the service she is providing as well.

- If disputed Bob can collect back his remaining funds only after yy days.

- If both parties reconcile after a dispute within yy days and Bob unblocks collect, Alice can collect.

- _TODO_ (if useful): Endpoints to withdraw from the contract.


This project gives uses the [Plutus Platform starter project](https://github.com/input-output-hk/plutus-starter) as the template.

### Setting up
Please refer to [Setting up](https://github.com/input-output-hk/plutus-starter#setting-up) section of the plutus starter project.

### Tips
#### To use entr to watch your source file during development
```ls src/*.hs | entr -r cabal build plutus-starter-pab --dependencies-only --disable-documentation```

#### Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc. please do so over on the main [plutus repository](https://github.com/input-output-hk/plutus).


Thanks!

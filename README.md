# Lock and Stake Contracts

A smart contract for locking and staking ADA.

The locking contract allows for a utxo to be unlocked with a correct signature of the wallet. The staking contract is designed for a single specific pool id where the rewards can only be withdrawn to a specific wallet known at compile time. 

It is a simple staking system designed for a single pool, single payout use case. The stake contract can be attached to a wallet or a contract. The example test scripts show how to register, delegator, and withdraw rewards. The contract can not be used as pledge for a pool because the CLI does not allow validation scripts to be used as the reward address yet.

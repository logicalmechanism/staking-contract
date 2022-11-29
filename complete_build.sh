#!/bin/bash
# set -e
# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# get info
poolId=$(cat start_info.json | jq -r .poolId)
rewardPkh=$(cat start_info.json | jq -r .rewardPkh)
rewardSc=$(cat start_info.json | jq -r .rewardSc)

mkdir -p info

# starter nft data
python3 -c "import binascii;a='${poolId}';s=binascii.unhexlify(a);print([x for x in s])"    > info/pool.id
python3 -c "import binascii;a='${rewardPkh}';s=binascii.unhexlify(a);print([x for x in s])" > info/reward.pkh
python3 -c "import binascii;a='${rewardSc}';s=binascii.unhexlify(a);print([x for x in s])"  > info/reward.sc

# change the pool id
python3 -c "from update_contracts import changePoolId;changePoolId('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/pool.id))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout pkh
python3 -c "from update_contracts import changeRewardPkh;changeRewardPkh('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/reward.pkh))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout sc
python3 -c "from update_contracts import changeRewardSc;changeRewardSc('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat info/reward.sc))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# build
cd stake-contract

echo -e "\033[1;35m Building Staking Contract... \033[0m"

# remove old data
rm stake-contract.plutus
rm stake.addr
rm stake.hash
rm stake.bytes
rm stake.cert
rm deleg.cert

# build
cabal build -w ghc-8.10.7
cabal run stake-contract

# get stake data
cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic $(cat ../scripts/testnet.magic) --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus  --out-file stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file deleg.cert

echo -e "\nStake Addr:" $(cat stake.addr)
echo -e "\nStake Hash:" $(cat stake.hash)
echo -e "\nStake Byte:" $(cat stake.bytes)
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq

# update the withdraw and delegate redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/withdraw_redeemer.json > ../scripts/data/withdraw_redeemer-new.json
mv ../scripts/data/withdraw_redeemer-new.json ../scripts/data/withdraw_redeemer.json

# update the register redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/delegate_redeemer.json > ../scripts/data/delegate_redeemer-new.json
mv ../scripts/data/delegate_redeemer-new.json ../scripts/data/delegate_redeemer.json

cd ..

echo -e "\033[1;35m Building Locking Contract... \033[0m"

cd locking-contract

# remove old data
rm locking-contract.plutus
rm validator.addr
rm validator.hash
rm validator.bytes

# build
cabal build -w ghc-8.10.7
cabal run locking-contract

cardano-cli address build --payment-script-file locking-contract.plutus --testnet-magic $(cat ../scripts/testnet.magic) --out-file validator.addr
cardano-cli transaction policyid --script-file locking-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\nValidator Addr:" $(cat validator.addr)
echo -e "\nValidator Hash:" $(cat validator.hash)
echo -e "\nValidator Byte:" $(cat validator.bytes)

cd ..

find ./*-contract/ -name '*.hash' -type f -exec sha256sum {} \; > info/hash.hashes
echo -e "\033[1;36m \nvalidator sha256sum\n\033[0m"
echo -e "\033[1;33m$(cat info/hash.hashes) \033[0m"

find . -name '*.hashes' -type f -exec sha256sum {} \; > info/final.check
echo -e "\033[1;35m \nfinal sha256sum\n\033[0m"
echo -e "\033[1;32m$(sha256sum info/final.check) \033[0m"

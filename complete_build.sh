#!/bin/bash
# set -e
# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# get info
poolId=$(cat start_info.json | jq -r .poolId)
rewardPkh=$(cat start_info.json | jq -r .rewardPkh)
rewardSc=$(cat start_info.json | jq -r .rewardSc)

# starter nft data
python3 -c "import binascii;a='${poolId}';s=binascii.unhexlify(a);print([x for x in s])"    > pool.id
python3 -c "import binascii;a='${rewardPkh}';s=binascii.unhexlify(a);print([x for x in s])" > reward.pkh
python3 -c "import binascii;a='${rewardSc}';s=binascii.unhexlify(a);print([x for x in s])"  > reward.sc

# change the pool id
python3 -c "from update_contracts import changePoolId;changePoolId('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat pool.id))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout pkh
python3 -c "from update_contracts import changeRewardPkh;changeRewardPkh('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat reward.pkh))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# change payout sc
python3 -c "from update_contracts import changeRewardSc;changeRewardSc('./stake-contract/src/StakeContract.hs', './stake-contract/src/StakeContract.hs-new.hs', $(cat reward.sc))"
mv ./stake-contract/src/StakeContract.hs-new.hs ./stake-contract/src/StakeContract.hs

# build
cd stake-contract

# remove old data
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

# update the register redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../scripts/data/reg_redeemer.json > ../scripts/data/reg_redeemer-new.json
mv ../scripts/data/reg_redeemer-new.json ../scripts/data/reg_redeemer.json

cd ..

find ./*-contract/ -name '*.hash' -type f -exec sha256sum {} \; > hash.hashes
echo -e "\033[1;36m \nvalidator sha256sum\n\033[0m"
echo -e "\033[1;33m$(cat hash.hashes) \033[0m"

find . -name '*.hashes' -type f -exec sha256sum {} \; > final.check
echo -e "\033[1;35m \nfinal sha256sum\n\033[0m"
echo -e "\033[1;32m$(sha256sum final.check) \033[0m"

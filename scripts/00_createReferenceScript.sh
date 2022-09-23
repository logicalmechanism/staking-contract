#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)
mkdir -p tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

lock_script_path="../locking-contract/locking-contract.plutus"
stake_script_path="../stake-contract/stake-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)
seller_address=$(cat wallets/reference-wallet/payment.addr)

lock_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "FT Locking Min Fee" ${lock_min_utxo}

stake_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')
echo "FT Minting Min Fee" ${stake_min_utxo}

stake_value=$stake_min_utxo
lock_value=$lock_min_utxo
lock_script_reference_utxo="${reference_address} + ${lock_value}"
stake_script_reference_utxo="${reference_address} + ${stake_value}"

echo -e "\nCreating Locking Reference:\n" ${lock_script_reference_utxo}
echo -e "\nCreating Minting Reference:\n" ${stake_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file tmp/seller_utxo.json

TXNS=$(jq length tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/seller_utxo.json)
HEXTXIN=${TXIN::-8}
# echo $HEXTXIN
# exit

# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_seller_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/seller_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${seller_address} + ${starting_seller_lovelace}" \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --fee 900000
FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
# echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)
# echo $fee
# exit
firstReturn=$((${starting_seller_lovelace} - ${stake_value} - ${lock_value} - ${fee}))
# echo $firstReturn
# exit
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${seller_address} + ${firstReturn}" \
    --tx-out="${lock_script_reference_utxo}" \
    --tx-out-reference-script-file ${lock_script_path} \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-1.signed

cp tmp/tx-1.signed tmp/tx-reference-utxo.signed
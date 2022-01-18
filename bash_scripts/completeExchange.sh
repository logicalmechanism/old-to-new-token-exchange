#!/bin/bash
set -e

CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="data/old_to_new.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
exchanger_address=$(cat buyer-wallet/payment.addr)

policy_id_old="48664e8d76f2b15606677bd117a3eac9929c378ac547ed295518dfd5"
token_name_old="tBigTokenName02"
token_hex_old=$(echo -n ${token_name_old} | xxd -ps)

policy_id_new="16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85"
token_name_new="tBigTokenName12"
token_hex_new=$(echo -n ${token_name_new} | xxd -ps)

SC_ASSET="100 ${policy_id_old}.${token_hex_old} + 700 ${policy_id_new}.${token_hex_new}"

SC_UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/datum.json \
    --tx-out="$script_address $SC_ASSET" | tr -dc '0-9')
SC_UTXO_VALUE="2000000"
sc_address_out="$script_address + $SC_UTXO_VALUE + $SC_ASSET"

exchanger_address_out="$exchanger_address + $SC_UTXO_VALUE + 200 ${policy_id_new}.${token_hex_new}"

echo "OUTPUT: "${sc_address_out}
echo "OUTPUT: "${exchanger_address_out}

# exit

echo -e "\033[0;36m Gathering Buyer UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${exchanger_address} \
    --out-file tmp/exchanger_utxo.json

TXNS=$(jq length tmp/exchanger_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${exchanger_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/exchanger_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/exchanger_utxo.json)
collateral_tx_in=${CTXIN::-19}
collateral_tx_in="f5ba171826d8bbeb134e3f019cbde0d0f2cc6632b500994b5971a6a57abec240#0"
# echo $collateral_tx_in
# exit

exchanger_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}


# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${exchanger_address} \
    --tx-in ${exchanger_tx_in} \
    --tx-in ${script_tx_in} \
    --tx-in-collateral ${collateral_tx_in} \
    --tx-in-datum-file data/datum.json \
    --tx-in-redeemer-file data/redeemer.json \
    --tx-out="${exchanger_address_out}" \
    --tx-out="${sc_address_out}" \
    --tx-out-datum-embed-file data/datum.json \
    --tx-in-script-file ${script_path} \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
# exit
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file buyer-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
# exit
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed
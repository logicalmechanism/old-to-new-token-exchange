#!/bin/bash
set -e

# SET UP VARS HERE
CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

# Addresses
sender_address=$(cat seller-wallet/payment.addr)
receiver_address=$(cat buyer-wallet/payment.addr)
# receiver_address="addr_test1qrxw9ynp9rva8uztyqjgefl8d3k804pwapms4wv3e60ltjn2s7gj5l4pam3pdeckkp7jwx8dsxelvq3ypv2ggzet9wcsq68qvx"

# Define Asset to be printed here
asset="100 48664e8d76f2b15606677bd117a3eac9929c378ac547ed295518dfd5.74426967546f6b656e4e616d653032"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${receiver_address} ${asset}" | tr -dc '0-9')
token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${sender_address} \
    --out-file tmp/sender_utxo.json

TXNS=$(jq length tmp/sender_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-hereafter 99999999 \
    --out-file tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${sender_address} + 2000000 + 900 16af70780a170994e8e5e575f4401b1d89bddf7d1a11d6264e0b0c85.74426967546f6b656e4e616d653132" \
    --tx-out="${token_to_be_traded}" \
    --testnet-magic 1097911063)

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
# exit
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063

echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed
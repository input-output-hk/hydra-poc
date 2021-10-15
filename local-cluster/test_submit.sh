#!/usr/bin/env bash
# a script for testing local cardano-node cluster

set -vx

[ -x $(which cardano-cli) ] || { echo "cardano-cli not found, check PATH environment variable" ; exit 1 ; }

utxo_addr=$(cardano-cli address build --payment-verification-key-file utxo1.vkey --testnet-magic 42)
transfer_amount=100000000

# take first utxo available
utxo=$(cardano-cli query utxo --testnet-magic 42 --address $utxo_addr | tail -n +3 | tr -s ' ' | cut -d ' ' -f1,2 | tr ' ' '#')
amount=$(cardano-cli query utxo --testnet-magic 42 --address $utxo_addr | tail -n +3 | tr -s ' ' | cut -d ' ' -f3)

# dispatch some ADAs to alice
cardano-cli transaction build-raw --tx-in $utxo \
            --tx-out $utxo_addr+$transfer_amount \
            --invalid-hereafter 0 --fee 0 --out-file tx.draft

fees=$(cardano-cli transaction calculate-min-fee --tx-body-file tx.draft --tx-in-count 1 --tx-out-count 2 --witness-count 1 --testnet-magic 42 --genesis genesis-shelley.json | cut -d ' ' -f1)
slot=$(cardano-cli query tip --testnet-magic 42 | jq .slot)

cardano-cli transaction build-raw --tx-in $utxo \
            --tx-out $utxo_addr+$transfer_amount \
            --tx-out $utxo_addr+$(($amount - $transfer_amount - $fees)) \
            --invalid-hereafter $((slot + 100)) --fee $fees --out-file tx.draft

cardano-cli transaction sign --tx-body-file tx.draft --signing-key-file utxo1.skey --testnet-magic 42 --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed --testnet-magic 42

timeout=30

while [ $timeout -gt 0 ]; do
  new_utxo=$(cardano-cli query utxo --testnet-magic 42 --address $utxo_addr | grep $transfer_amount)

  [ ! -z "$new_utxo" ] && { echo "found UTXO: $new_utxo" ; exit 0 ; }

  echo "new UTXO not found, waiting 1s"
  sleep 1
  timeout=$(( $timeout - 1 ))
done

exit 1

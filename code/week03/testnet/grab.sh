cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in e10449f6d63c193d9dcdf00be73b35149355cf4f39c0a933e736027e8105a5ca#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 9e16d784e23dba40d6bbff113d6e48a60a07ad696c83c0fbd77d7bb47d729a05#1 \
    --required-signer-hash d4f5fc7c64f0d0a90cf263b04c6af29c629e1d0674b6f83623800f1f \
    --invalid-before 49044993 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

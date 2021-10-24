#!/bin/bash
printf "\nAn applied escrow.\n"
sleep 1

printf "\n"
export ProviderW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export ConsumerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWB=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWC=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export CheckerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1

export ProviderW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$ProviderW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export ConsumerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$ConsumerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWB_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWC_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export CheckerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "CheckerContract", "caWallet":{"getWalletId": '$CheckerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
printf "\nThere are four wallets belonging to Issuer brand, 2 token holders and 1 Resort\n"
printf "\nThe issuer issued auth NFT to holders after encoding the list of privileges and expiry time in it.\n"
printf "\nThe NFT is linked to the identity of the issuer and holder as its minted using both their pub key hashes\n"

printf "\nThere are 2 levels of loyalty tokens minted GENERAL and SPECIAL\n"
printf "\nEach type comes with its set of privileges and expiration time\n"
printf "\nHolder A is 'GENERAL' member while Holder B is a 'SPECIAL' member\n"

sleep 1
printf "\n1. Log the currency symbol for reference.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '[]' http://localhost:9080/api/contract/instance/$ProviderW_IID/endpoint/logCS
sleep 1

sleep 1
printf "\n2. Mint NFTs and send to A and B wallets. Wallet C does not receive any.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"level":"GENERAL", "destW":{"getWalletId": '$ConsumerW'}}' http://localhost:9080/api/contract/instance/$ProviderW_IID/endpoint/mint
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"level":"SPECIAL", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$ProviderW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find all NFTs for the issuer inside the wallets A B C.\n"
printf "\n3. The output depends on the time elapsed since the tokens were minted\n"

printf "\nPress any key after you wait for a time < 10  or > 10 seconds...\n"
printf "\n"

if [ -t 0 ]; then
  SAVED_STTY="`stty --save`"
  stty -echo -icanon -icrnl time 0 min 0
fi

count=0
keypress=''
while [ "x$keypress" = "x" ]; do
  sleep 1
  let count+=1
  echo -ne $count'\r'
  keypress="`cat -v`"
done

if [ -t 0 ]; then stty "$SAVED_STTY"; fi

echo "You pressed '$keypress' after $count seconds"
printf "\nContinuing ... \n"


curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$ConsumerW'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1



sleep 1
printf "\n Now, we iterate through minting and authorising once more \n"
printf "\n2. Mint NFTs and send to A and B wallets. Wallet C does not receive any.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"level":"GENERAL", "destW":{"getWalletId": '$ConsumerW'}}' http://localhost:9080/api/contract/instance/$ProviderW_IID/endpoint/mint
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"level":"SPECIAL", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$ProviderW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find all NFTs for the issuer inside the wallets A B C.\n"
printf "\n3. The output depends on the time elapsed since the tokens were minted\n"

printf "\nPress any key after you wait for a time < 10  or > 10 seconds...\n"
printf "\n"

if [ -t 0 ]; then
  SAVED_STTY="`stty --save`"
  stty -echo -icanon -icrnl time 0 min 0
fi

count=0
keypress=''
while [ "x$keypress" = "x" ]; do
  sleep 1
  let count+=1
  echo -ne $count'\r'
  keypress="`cat -v`"
done

if [ -t 0 ]; then stty "$SAVED_STTY"; fi

echo "You pressed '$keypress' after $count seconds"
printf "\nContinuing ... \n"


curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$ConsumerW'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"ProviderWallet":{"getWalletId": '$ProviderW'}, "ConsumerWllet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1

printf "\nThank you for your time.\n"
printf "\n"
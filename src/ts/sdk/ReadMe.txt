Interact with contract using Econia Typescript SDK

const {client, account} = ...;

//Load App
const app = new App(client);

const HOST = "0xdad1c1d54fcff3bf0d83b4b0067d7cf0ebdca3ff17556f77115ada2db1ff23fe"  //econia's market host address
const MARKET_ID = 1;

//register market  (SOL vs BTC)

await app.user.register_market_account(account, MARKET_ID, GENERAL_CUSTODIAN_ID,
["0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetSOL",   //BASE_ASSET
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetBTC"]    //QUOTE_ASSET
);

//deposit funds 1000
await app.user.deposit_from_coinstore(account, MARKET_ID, GENERAL_CUSTODIAN_ID, 1000,
["0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetSOL",   //BASE_ASSET
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetBTC"]    //QUOTE_ASSET
);

//place a limit order
await app.market.place_limit_order_user(
 account,
 HOST,
 MARKET_ID,
 true, //ASK
 100,  //size
 100,  //price
 false,
 false,
 false,
 ["0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetSOL",   //BASE_ASSET
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetBTC"]    //QUOTE_ASSET
);

//place a market order

await app.market.place_limit_order_user(
 account,
 HOST,
 MARKET_ID,
 true, //direction
 100, //min_base
 1000,  //max_base
 100, //min_quote
 1000, //max_quote,
 500, //limit_price
 ["0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetSOL",   //BASE_ASSET
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetBTC"]    //QUOTE_ASSET
);

//withdraw funds
await app.user.withdraw_to_coinstore(account, MARKET_ID, 1000,
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetSOL",
"0x498d8926f16eb9ca90cab1b3a26aa6f97a080b3fcbe6e83ae150b7243a00fb68::devnet_coins::DevnetBTC"

);




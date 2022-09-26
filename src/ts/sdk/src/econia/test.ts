import * as $ from "@manahippo/move-to-ts";
import {AptosDataCache, AptosParserRepo, DummyCache, AptosLocalCache} from "@manahippo/move-to-ts";
import {U8, U64, U128} from "@manahippo/move-to-ts";
import {u8, u64, u128} from "@manahippo/move-to-ts";
import {TypeParamDeclType, FieldDeclType} from "@manahippo/move-to-ts";
import {AtomicTypeTag, StructTag, TypeTag, VectorTag, SimpleStructTag} from "@manahippo/move-to-ts";
import {HexString, AptosClient, AptosAccount, TxnBuilderTypes, Types} from "aptos";
import * as Assets from "./assets";
import * as Market from "./market";
import * as Registry from "./registry";
import * as User from "./user";
export const packageName = "Econia";
export const moduleAddress = new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd");
export const moduleName = "test";

export const ASK : boolean = true;
export const ASK_PRICE : U64 = u64("10");
export const ASK_SIZE : U64 = u64("100");
export const CUSTODIAN_ID : U64 = u64("0");
export const LOT_SIZE : U64 = u64("10");
export const MARKET_ID : U64 = u64("0");
export const MINT_AMOUNT : U64 = u64("1000000000");
export const TICK_SIZE : U64 = u64("25");

export function init_module_ (
  econia: HexString,
  $c: AptosDataCache,
): void {
  Registry.init_registry_(econia, $c);
  Assets.init_coin_types(econia, $c);
  Market.register_market_pure_coin(econia, $.copy(LOT_SIZE), $.copy(TICK_SIZE), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "BC", []), new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "QC", [])]);
  User.register_market_account_(econia, $.copy(MARKET_ID), $.copy(CUSTODIAN_ID), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "BC", []), new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "QC", [])]);
  User.deposit_coins_(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), $.copy(MARKET_ID), $.copy(CUSTODIAN_ID), Assets.mint(econia, $.copy(MINT_AMOUNT), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "QC", [])]), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "QC", [])]);
  User.deposit_coins_(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), $.copy(MARKET_ID), $.copy(CUSTODIAN_ID), Assets.mint(econia, $.copy(MINT_AMOUNT), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "BC", [])]), $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "BC", [])]);
  Market.place_limit_order_user_(econia, new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), $.copy(MARKET_ID), $.copy(ASK), $.copy(ASK_SIZE), $.copy(ASK_PRICE), false, false, false, $c, [new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "BC", []), new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "assets", "QC", [])]);
  return;
}

export function loadParsers(repo: AptosParserRepo) {
}
export class App {
  constructor(
    public client: AptosClient,
    public repo: AptosParserRepo,
    public cache: AptosLocalCache,
  ) {
  }
  get moduleAddress() {{ return moduleAddress; }}
  get moduleName() {{ return moduleName; }}
}


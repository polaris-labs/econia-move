import * as $ from "@manahippo/move-to-ts";
import {AptosDataCache, AptosParserRepo, DummyCache, AptosLocalCache} from "@manahippo/move-to-ts";
import {U8, U64, U128} from "@manahippo/move-to-ts";
import {u8, u64, u128} from "@manahippo/move-to-ts";
import {TypeParamDeclType, FieldDeclType} from "@manahippo/move-to-ts";
import {AtomicTypeTag, StructTag, TypeTag, VectorTag, SimpleStructTag} from "@manahippo/move-to-ts";
import {HexString, AptosClient, AptosAccount, TxnBuilderTypes, Types} from "aptos";
import * as Stdlib from "../stdlib";
export const packageName = "Econia";
export const moduleAddress = new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd");
export const moduleName = "critbit";
/// When a char in a bytestring is neither 0 nor 1
export const E_BIT_NOT_0_OR_1 : U64 = u64("0");
/// When attempting to destroy a non-empty tree
export const E_DESTROY_NOT_EMPTY : U64 = u64("1");
/// When an insertion key is already present in a tree
export const E_HAS_KEY : U64 = u64("2");
/// When unable to borrow from empty tree
export const E_BORROW_EMPTY : U64 = u64("3");
/// When no matching key in tree
export const E_NOT_HAS_KEY : U64 = u64("4");
/// When no more keys can be inserted
export const E_INSERT_FULL : U64 = u64("5");
/// When attempting to pop from empty tree
export const E_POP_EMPTY : U64 = u64("6");
/// When attempting to look up on an empty tree
export const E_LOOKUP_EMPTY : U64 = u64("7");
/// `u128` bitmask with all bits set
export const HI_128 : U128 = u128("340282366920938463463374607431768211455");
/// `u64` bitmask with all bits set
export const HI_64 : U64 = u64("18446744073709551615");
/// Node type bit flag indicating inner node
export const INNER : U64 = u64("0");
/// Most significant bit number for a `u128`
export const MSB_u128 : U8 = u8("127");
/// Bit number of node type flag in a `u64` vector index
export const NODE_TYPE : U8 = u8("63");
/// Node type bit flag indicating outer node
export const OUTER : U64 = u64("1");
/// Left direction
export const LEFT : boolean = true;
/// Right direction
export const RIGHT : boolean = false;
/// `u64` bitmask with all bits set, to flag that a node is at root
export const ROOT : U64 = u64("18446744073709551615");

/// A crit-bit tree for key-value pairs with value type `V`
export class CritBitTree
{
  static moduleAddress = moduleAddress;
  static moduleName = moduleName;
  __app: $.AppType | null = null;
  static structName: string = "CritBitTree";
  static typeParameters: TypeParamDeclType[] = [
    { name: "V", isPhantom: false }
  ];
  static fields: FieldDeclType[] = [
  { name: "root", typeTag: AtomicTypeTag.U64 },
  { name: "inner_nodes", typeTag: new VectorTag(new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "critbit", "InnerNode", [])) },
  { name: "outer_nodes", typeTag: new VectorTag(new StructTag(new HexString("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd"), "critbit", "OuterNode", [new $.TypeParamIdx(0)])) }];

  /// Root node index. When bit 63 is set, root node is an outer
  /// node. Otherwise root is an inner node. 0 when tree is empty
  root: U64;
  /// Inner nodes
  inner_nodes: InnerNode[];
  /// Outer nodes
  outer_nodes: OuterNode[];

  constructor(proto: any, public typeTag: TypeTag) {
    this.root = proto['root'] as U64;
    this.inner_nodes = proto['inner_nodes'] as InnerNode[];
    this.outer_nodes = proto['outer_nodes'] as OuterNode[];
  }

  static CritBitTreeParser(data:any, typeTag: TypeTag, repo: AptosParserRepo) : CritBitTree {
    const proto = $.parseStructProto(data, typeTag, repo, CritBitTree);
    return new CritBitTree(proto, typeTag);
  }

  static makeTag($p: TypeTag[]): StructTag {
    return new StructTag(moduleAddress, moduleName, "CritBitTree", $p);
  }
  async loadFullState(app: $.AppType) {
    this.__app = app;
  }

}

/// Inner node
export class InnerNode
{
  static moduleAddress = moduleAddress;
  static moduleName = moduleName;
  __app: $.AppType | null = null;
  static structName: string = "InnerNode";
  static typeParameters: TypeParamDeclType[] = [

  ];
  static fields: FieldDeclType[] = [
  { name: "critical_bit", typeTag: AtomicTypeTag.U8 },
  { name: "parent_index", typeTag: AtomicTypeTag.U64 },
  { name: "left_child_index", typeTag: AtomicTypeTag.U64 },
  { name: "right_child_index", typeTag: AtomicTypeTag.U64 }];
// Documentation comments, specifically for struct fields,
// apparently do not support fenced code blocks unless they are
// preceded by a blank line...
/// Critical bit position. Bit numbers 0-indexed from LSB:
///
/// ```
/// >    11101...1010010101
/// >     bit 5 = 0 -|    |- bit 0 = 1
/// ```
  critical_bit: U8;
  /// Parent node vector index. `ROOT` when node is root,
  /// otherwise corresponds to vector index of an inner node.
  parent_index: U64;
  /// Left child node index. When bit 63 is set, left child is an
  /// outer node. Otherwise left child is an inner node.
  left_child_index: U64;
  /// Right child node index. When bit 63 is set, right child is
  /// an outer node. Otherwise right child is an inner node.
  right_child_index: U64;

  constructor(proto: any, public typeTag: TypeTag) {
    this.critical_bit = proto['critical_bit'] as U8;
    this.parent_index = proto['parent_index'] as U64;
    this.left_child_index = proto['left_child_index'] as U64;
    this.right_child_index = proto['right_child_index'] as U64;
  }

  static InnerNodeParser(data:any, typeTag: TypeTag, repo: AptosParserRepo) : InnerNode {
    const proto = $.parseStructProto(data, typeTag, repo, InnerNode);
    return new InnerNode(proto, typeTag);
  }

  static getTag(): StructTag {
    return new StructTag(moduleAddress, moduleName, "InnerNode", []);
  }
  async loadFullState(app: $.AppType) {
    this.__app = app;
  }

}
/// Outer node with key `k` and value `v`
export class OuterNode
{
  static moduleAddress = moduleAddress;
  static moduleName = moduleName;
  __app: $.AppType | null = null;
  static structName: string = "OuterNode";
  static typeParameters: TypeParamDeclType[] = [
    { name: "V", isPhantom: false }
  ];
  static fields: FieldDeclType[] = [
  { name: "key", typeTag: AtomicTypeTag.U128 },
  { name: "value", typeTag: new $.TypeParamIdx(0) },
  { name: "parent_index", typeTag: AtomicTypeTag.U64 }];
  /// Key, which would preferably be a generic type representing
  /// the union of {`u8`, `u64`, `u128`}. However this kind of
  /// union typing is not supported by Move, so the most general
  /// (and memory intensive) `u128` is instead specified strictly.
  /// Must be an integer for bitwise operations.
  key: U128;
  /// Value from node's key-value pair
  value: any;
  /// Parent node vector index. `ROOT` when node is root,
  /// otherwise corresponds to vector index of an inner node.
  parent_index: U64;

  constructor(proto: any, public typeTag: TypeTag) {
    this.key = proto['key'] as U128;
    this.value = proto['value'] as any;
    this.parent_index = proto['parent_index'] as U64;
  }

  static OuterNodeParser(data:any, typeTag: TypeTag, repo: AptosParserRepo) : OuterNode {
    const proto = $.parseStructProto(data, typeTag, repo, OuterNode);
    return new OuterNode(proto, typeTag);
  }

  static makeTag($p: TypeTag[]): StructTag {
    return new StructTag(moduleAddress, moduleName, "OuterNode", $p);
  }
  async loadFullState(app: $.AppType) {
    if (this.value.typeTag instanceof StructTag) {await this.value.loadFullState(app);}
    this.__app = app;
  }
}
/// Return immutable reference to value corresponding to key `k` in
/// `tree`, aborting if empty tree or no match
export function borrow(
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let closest_outer_node_ref;
  if (is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_BORROW_EMPTY));
  }
  closest_outer_node_ref = borrow_closest_outer_node(tree, $.copy(key), $c, [$p[0]]);
  if (!($.copy(closest_outer_node_ref.key)).eq(($.copy(key)))) {
    throw $.abortCode($.copy(E_NOT_HAS_KEY));
  }
  return closest_outer_node_ref.value;
}

export function borrow_closest_outer_node(
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): OuterNode {
  let child_index, node;
  if (is_outer_node($.copy(tree.root), $c)) {
    return Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index($.copy(tree.root), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  }
  node = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(tree.root), $c, [new SimpleStructTag(InnerNode)]);
  while (true) {
    child_index = is_set($.copy(key), $.copy(node.critical_bit), $c) ?
         $.copy(node.right_child_index) :
         $.copy(node.left_child_index);
    if (is_outer_node($.copy(child_index), $c)) {
      return Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index($.copy(child_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
    }
    node = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_index), $c, [new SimpleStructTag(InnerNode)]);
  }
}

export function borrow_closest_outer_node_mut(
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): OuterNode {
  let child_index, node;
  if (is_outer_node($.copy(tree.root), $c)) {
    return Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(tree.root), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  }
  node = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(tree.root), $c, [new SimpleStructTag(InnerNode)]);
  while (true) {
    child_index = is_set($.copy(key), $.copy(node.critical_bit), $c)?
      $.copy(node.right_child_index):
      $.copy(node.left_child_index);
    if (is_outer_node($.copy(child_index), $c)) {
      return Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(child_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
    }
    node = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_index), $c, [new SimpleStructTag(InnerNode)]);
  }
}

/// Return mutable reference to value corresponding to key `k` in
/// `tree`, aborting if empty tree or no match
export function borrow_mut (
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let closest_outer_node_ref_mut;
  if (is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_BORROW_EMPTY));
  }
  closest_outer_node_ref_mut = borrow_closest_outer_node_mut(tree, $.copy(key), $c, [$p[0]]);
  if (!($.copy(closest_outer_node_ref_mut.key)).eq(($.copy(key)))) {
    throw $.abortCode($.copy(E_NOT_HAS_KEY));
  }
  return closest_outer_node_ref_mut.value;
}

/// Assert that `length` is less than the value indicated by a
/// bitmask where only the 63rd bit is not set (this bitmask
/// corresponds to the maximum number of keys that can be stored in
/// a tree, since the 63rd bit is reserved for the node type bit
/// flag)
export function check_length (
  length: U64
) {
  if (!($.copy(length)).lt(($.copy(HI_64)).xor(($.copy(OUTER)).shl($.copy(NODE_TYPE)))))
    throw $.abortCode($.copy(E_INSERT_FULL));
}

/// Return the number of the most significant bit (0-indexed from
/// LSB) at which two non-identical bitstrings, `s1` and `s2`, vary.
///
/// # XOR/AND method
///
/// To begin with, a bitwise XOR is used to flag all differing bits:
/// ```
/// >           s1: 11110001
/// >           s2: 11011100
/// >  x = s1 ^ s2: 00101101
/// >                 |- critical bit = 5
/// ```
/// Here, the critical bit is equivalent to the bit number of the
/// most significant set bit in XOR result `x = s1 ^ s2`. At this
/// point, [Langley 2012](#References) notes that `x` bitwise AND
/// `x - 1` will be nonzero so long as `x` contains at least some
/// bits set which are of lesser significance than the critical bit:
/// ```
/// >               x: 00101101
/// >           x - 1: 00101100
/// > x = x & (x - 1): 00101100
/// ```
/// Thus he suggests repeating `x & (x - 1)` while the new result
/// `x = x & (x - 1)` is not equal to zero, because such a loop will
/// eventually reduce `x` to a power of two (excepting the trivial
/// case where `x` starts as all 0 except bit 0 set, for which the
/// loop never enters past the initial conditional check). Per this
/// method, using the new `x` value for the current example, the
/// second iteration proceeds as follows:
/// ```
/// >               x: 00101100
/// >           x - 1: 00101011
/// > x = x & (x - 1): 00101000
/// ```
/// The third iteration:
/// ```
/// >               x: 00101000
/// >           x - 1: 00100111
/// > x = x & (x - 1): 00100000
/// ```
/// Now, `x & x - 1` will equal zero and the loop will not begin a
/// fourth iteration:
/// ```
/// >             x: 00100000
/// >         x - 1: 00011111
/// > x AND (x - 1): 00000000
/// ```
/// Thus after three iterations a corresponding critical bit bitmask
/// has been determined. However, in the case where the two input
/// strings vary at all bits of lesser significance than that of the
/// critical bit, there may be required as many as `k - 1`
/// iterations, where `k` is the number of bits in each string under
/// comparison. For instance, consider the case of the two 8-bit
/// strings `s1` and `s2` as follows:
/// ```
/// >              s1: 10101010
/// >              s2: 01010101
/// >     x = s1 ^ s2: 11111111
/// >                  |- critical bit = 7
/// > x = x & (x - 1): 11111110 [iteration 1]
/// > x = x & (x - 1): 11111100 [iteration 2]
/// > x = x & (x - 1): 11111000 [iteration 3]
/// > ...
/// ```
/// Notably, this method is only suggested after already having
/// identified the varying byte between the two strings, thus
/// limiting `x & (x - 1)` operations to at most 7 iterations.
///
/// # Binary search method
///
/// For the present implementation, strings are not partitioned into
/// a multi-byte array, rather, they are stored as `u128` integers,
/// so a binary search is instead proposed. Here, the same
/// `x = s1 ^ s2` operation is first used to identify all differing
/// bits, before iterating on an upper and lower bound for the
/// critical bit number:
/// ```
/// >          s1: 10101010
/// >          s2: 01010101
/// > x = s1 ^ s2: 11111111
/// >       u = 7 -|      |- l = 0
/// ```
/// The upper bound `u` is initialized to the length of the string
/// (7 in this example, but 127 for a `u128`), and the lower bound
/// `l` is initialized to 0. Next the midpoint `m` is calculated as
/// the average of `u` and `l`, in this case `m = (7 + 0) / 2 = 3`,
/// per truncating integer division. Now, the shifted compare value
/// `s = r >> m` is calculated and updates are applied according to
/// three potential outcomes:
///
/// * `s == 1` means that the critical bit `c` is equal to `m`
/// * `s == 0` means that `c < m`, so `u` is set to `m - 1`
/// * `s > 1` means that `c > m`, so `l` us set to `m + 1`
///
/// Hence, continuing the current example:
/// ```
/// >          x: 11111111
/// > s = x >> m: 00011111
/// ```
/// `s > 1`, so `l = m + 1 = 4`, and the search window has shrunk:
/// ```
/// > x = s1 ^ s2: 11111111
/// >       u = 7 -|  |- l = 4
/// ```
/// Updating the midpoint yields `m = (7 + 4) / 2 = 5`:
/// ```
/// >          x: 11111111
/// > s = x >> m: 00000111
/// ```
/// Again `s > 1`, so update `l = m + 1 = 6`, and the window
/// shrinks again:
/// ```
/// > x = s1 ^ s2: 11111111
/// >       u = 7 -||- l = 6
/// > s = x >> m: 00000011
/// ```
/// Again `s > 1`, so update `l = m + 1 = 7`, the final iteration:
/// ```
/// > x = s1 ^ s2: 11111111
/// >       u = 7 -|- l = 7
/// > s = x >> m: 00000001
/// ```
/// Here, `s == 1`, which means that `c = m = 7`. Notably this
/// search has converged after only 3 iterations, as opposed to 7
/// for the linear search proposed above, and in general such a
/// search converges after $log_2(k)$ iterations at most, where $k$
/// is the number of bits in each of the strings `s1` and `s2` under
/// comparison. Hence this search method improves the $O(k)$ search
/// proposed by [Langley 2012](#References) to $O(log_2(k))$, and
/// moreover, determines the actual number of the critical bit,
/// rather than just a bitmask with bit `c` set, as he proposes,
/// which can also be easily generated via `1 << c`.
export function crit_bit (
  s1: U128,
  s2: U128
): U8 {
  let l, m, s, u, x;
  x = ($.copy(s1)).xor($.copy(s2));
  l = u8("0");
  u = $.copy(MSB_u128);
  while (true) {
    m = (($.copy(l)).add($.copy(u))).div(u8("2"));
    s = ($.copy(x)).shr($.copy(m));
    if (($.copy(s)).eq((u128("1")))) {
      return $.copy(m);
    }
    if (($.copy(s)).gt(u128("1"))) {
      l = ($.copy(m)).add(u8("1"));
    }
    else{
      u = ($.copy(m)).sub(u8("1"));
    }
  }
}

/// Destroy empty tree `tree`
export function destroy_empty (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  if (!is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_DESTROY_NOT_EMPTY));
  }
  let { inner_nodes: inner_nodes, outer_nodes: outer_nodes } = tree;
  Stdlib.Vector.destroy_empty_(inner_nodes, $c, [new SimpleStructTag(InnerNode)]);
  Stdlib.Vector.destroy_empty_(outer_nodes, $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  return;
}

/// Return an empty tree
export function empty (
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): CritBitTree {
  return new CritBitTree({ root: u64("0"), inner_nodes: Stdlib.Vector.empty_($c, [new SimpleStructTag(InnerNode)]), outer_nodes: Stdlib.Vector.empty_($c, [new SimpleStructTag(OuterNode, [$p[0]])]) }, new SimpleStructTag(CritBitTree, [$p[0]]));
}

/// Return true if `tree` has `key`
export function has_key (
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): boolean {
  if (is_empty(tree, $c, [$p[0]]))
    return false;
  return ($.copy(borrow_closest_outer_node(tree, $.copy(key), $c, [$p[0]]).key)).eq(($.copy(key)));
}

/// Insert `key` and `value` into `tree`, aborting if `key` already
/// in `tree`
export function insert(
  tree: CritBitTree,
  key: U128,
  value: any,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let _length;
  _length = length(tree, $c, [$p[0]]);
  check_length($.copy(_length));
  if (($.copy(_length)).eq((u64("0")))) {
    insert_empty(tree, $.copy(key), value, $c, [$p[0]]);
  }
  else{
    if (($.copy(_length)).eq((u64("1")))) {
      insert_singleton(tree, $.copy(key), value, $c, [$p[0]]);
    }
    else{
      insert_general(tree, $.copy(key), value, $.copy(_length), $c, [$p[0]]);
    }
  }
  return;
}

/// Decomposed case specified in `insert_general`, walk up tree, for
/// parameters:
/// * `tree`: Tree to insert into
/// * `key` : Key to insert
/// * `value` : Value to insert
/// * `n_outer_nodes` : Number of outer nodes in `tree` pre-insert
/// * `n_inner_nodes` : Number of inner nodes in `tree` pre-insert
///    (index of new inner node)
/// * `search_parent_index`: Index of search parent
/// * `critical_bit`: Critical bit between insertion key and search
///   outer node
export function insert_above (
  tree: CritBitTree,
  key: U128,
  value: any,
  n_outer_nodes: U64,
  n_inner_nodes: U64,
  search_parent_index: U64,
  critical_bit: U8,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let node, node_index;
  node_index = $.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(search_parent_index), $c, [new SimpleStructTag(InnerNode)]).parent_index);
  while (true) {
    if (($.copy(node_index)).eq(($.copy(ROOT))))
      return insert_above_root(tree, $.copy(key), value, $.copy(n_outer_nodes), $.copy(n_inner_nodes), $.copy(critical_bit), $c, [$p[0]]);
    else{
      node = Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(node_index), $c, [new SimpleStructTag(InnerNode)]);
      if (($.copy(critical_bit)).lt($.copy(node.critical_bit)))
        return insert_below_walk(tree, $.copy(key), value, $.copy(n_outer_nodes), $.copy(n_inner_nodes), $.copy(node_index), $.copy(critical_bit), $c, [$p[0]]);
      else
        node_index = $.copy(node.parent_index);
    }
  }
}

/// Decomposed case specified in `insert_general`, insertion above
/// root, for parameters:
/// * `tree`: Tree to insert into
/// * `key` : Key to insert
/// * `value` : Value to insert
/// * `n_outer_nodes` : Number of keys (outer nodes) in `tree`
///   pre-insert
/// * `n_inner_nodes` : Number of inner nodes in `tree` pre-insert
///   (index of new inner node)
/// * `critical_bit`: Critical bit between insertion key and search
///   outer node
export function insert_above_root (
  tree: CritBitTree,
  key: U128,
  value: any,
  n_outer_nodes: U64,
  n_inner_nodes: U64,
  critical_bit: U8,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
) {
  let old_root_index;
  old_root_index = $.copy(tree.root);
  Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(old_root_index), $c, [new SimpleStructTag(InnerNode)]).parent_index = $.copy(n_inner_nodes);
  tree.root = $.copy(n_inner_nodes);
  push_back_insert_nodes(tree, $.copy(key), value, $.copy(n_inner_nodes), $.copy(critical_bit), $.copy(ROOT), is_set($.copy(key), $.copy(critical_bit), $c), $.copy(old_root_index), outer_node_child_index($.copy(n_outer_nodes), $c), $c, [$p[0]]);  
}
/// Decomposed case specified in `insert_general`, insertion below
/// search parent, for parameters:
/// * `tree`: Tree to insert into
/// * `key` : Key to insert
/// * `value` : Value to insert
/// * `n_outer_nodes` : Number of keys (outer nodes) in `tree`
///   pre-insert
/// * `n_inner_nodes` : Number of inner nodes in `tree` pre-insert
///   (index of new inner node)
/// * `search_index`: Child field index of search outer node (with
///   bit flag)
/// * `search_child_side`: Side on which search outer node is child
/// * `search_key`: Key of search outer node
/// * `search_parent_index`: Index of search parent
/// * `critical_bit`: Critical bit between insertion key and search
///   outer node
export function insert_below (
  tree: CritBitTree,
  key: U128,
  value: any,
  n_outer_nodes: U64,
  n_inner_nodes: U64,
  search_index: U64,
  search_child_side: boolean,
  search_key: U128,
  search_parent_index: U64,
  critical_bit: U8,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let search_parent;
  search_parent = Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(search_parent_index), $c, [new SimpleStructTag(InnerNode)]);
  if ((search_child_side == $.copy(LEFT)))
    search_parent.left_child_index = $.copy(n_inner_nodes);
  else
    search_parent.right_child_index = $.copy(n_inner_nodes);

  Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(search_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).parent_index = $.copy(n_inner_nodes);
  push_back_insert_nodes(tree, $.copy(key), value, $.copy(n_inner_nodes), $.copy(critical_bit), $.copy(search_parent_index), ($.copy(key)).lt($.copy(search_key)), outer_node_child_index($.copy(n_outer_nodes), $c), $.copy(search_index), $c, [$p[0]]);
}

/// Decomposed case specified in `insert_general`, insertion below
/// a node encountered during walk, for parameters:
/// * `tree`: Tree to insert into
/// * `key` : Key to insert
/// * `value` : Value to insert
/// * `n_outer_nodes` : Number of keys (outer nodes) in `tree` pre-insert
/// * `n_inner_nodes` : Number of inner nodes in `tree` pre-insert
///   (index of new inner node)
/// * `review_node_index` : Index of node under review from walk
/// * `critical_bit`: Critical bit between insertion key and search
///   outer node

export function insert_below_walk (
  tree: CritBitTree,
  key: U128,
  value: any,
  n_outer_nodes: U64,
  n_inner_nodes: U64,
  review_node_index: U64,
  critical_bit: U8,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let temp$1, temp$2, review_node, walked_child_index, walked_child_side;
  review_node = Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(review_node_index), $c, [new SimpleStructTag(InnerNode)]);
  [walked_child_side, walked_child_index] = is_set($.copy(key), $.copy(review_node.critical_bit), $c)?
      [$.copy(RIGHT), $.copy(review_node.right_child_index)] :
      [$.copy(LEFT), $.copy(review_node.left_child_index)];

  if ((walked_child_side == $.copy(LEFT))) {
    review_node.left_child_index = $.copy(n_inner_nodes);
  }
  else{
    review_node.right_child_index = $.copy(n_inner_nodes);
  }
  Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(walked_child_index), $c, [new SimpleStructTag(InnerNode)]).parent_index = $.copy(n_inner_nodes);
  push_back_insert_nodes(tree, $.copy(key), value, $.copy(n_inner_nodes), $.copy(critical_bit), $.copy(review_node_index), is_set($.copy(key), $.copy(critical_bit), $c), $.copy(walked_child_index), outer_node_child_index($.copy(n_outer_nodes), $c), $c, [$p[0]]);
}

/// Insert key-value pair `key` and `value` into an empty `tree`
export function insert_empty (
  tree: CritBitTree,
  key: U128,
  value: any,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
) {
  Stdlib.Vector.push_back_(tree.outer_nodes, new OuterNode({ key: $.copy(key), value: value, parent_index: $.copy(ROOT) }, new SimpleStructTag(OuterNode, [$p[0]])), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  tree.root = ($.copy(OUTER)).shl($.copy(NODE_TYPE));
}

/// Insert `key` and `value` into `tree` already having
/// `n_outer_nodes` keys for general case where root is an inner
/// node, aborting if `key` is already present. First, perform an
/// outer node search and identify the critical bit of divergence
/// between the search outer node and `k`. Then, if the critical bit
/// is less than that of the search parent (`insert_below()`):
///
/// * Insert a new inner node directly above the search outer node
/// * Update the search outer node to have as its parent the new
///   inner node
/// * Update the search parent to have as its child the new inner
///   node where the search outer node previously was:
/// ```
/// >       2nd
/// >      /   \
/// >    001   1st <- search parent
/// >         /   \
/// >       101   111 <- search outer node
/// >
/// >       Insert 110
/// >       --------->
/// >
/// >                  2nd
/// >                 /   \
/// >               001   1st <- search parent
/// >                    /   \
/// >                  101   0th <- new inner node
/// >                       /   \
/// >   new outer node -> 110   111 <- search outer node
/// ```
/// Otherwise, begin walking back up the tree (`insert_above()`). If
/// walk arrives at the root node, insert a new inner node above the
/// root, updating associated relationships (`insert_above_root()`):
/// ```
/// >          1st
/// >         /   \
/// >       101   0th <- search parent
/// >            /   \
/// >          110    111 <- search outer node
/// >
/// >       Insert 011
/// >       --------->
/// >
/// >                         2nd <- new inner node
/// >                        /   \
/// >    new outer node -> 011   1st
/// >                           /   \
/// >                         101   0th <- search parent
/// >                              /   \
/// >                            110   111 <- search outer node
/// ```
/// Otherwise, if walk arrives at a node indicating a critical bit
/// larger than that between the insertion key and the search node,
/// insert the new inner node below it (`insert_below_walk()`):
/// ```
/// >
/// >           2nd
/// >          /   \
/// >        011   0th <- search parent
/// >             /   \
/// >           101   111 <- search outer node
/// >
/// >       Insert 100
/// >       --------->
/// >
/// >                       2nd
/// >                      /   \
/// >                    001   1st <- new inner node
/// >                         /   \
/// >     new outer node -> 100   0th <- search parent
/// >                            /   \
/// >                          110   111 <- search outer node
/// ```
export function insert_general (
  tree: CritBitTree,
  key: U128,
  value: any,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let critical_bit, n_inner_nodes, search_child_side, search_index, search_key, search_parent_critical_bit, search_parent_index;
  n_inner_nodes = Stdlib.Vector.length_(tree.inner_nodes, $c, [new SimpleStructTag(InnerNode)]);
  [search_index, search_child_side, search_key, search_parent_index, search_parent_critical_bit] = search_outer(tree, $.copy(key), $c, [$p[0]]);
  if (!($.copy(search_key)).neq($.copy(key)))
    throw $.abortCode($.copy(E_HAS_KEY));
  critical_bit = crit_bit($.copy(search_key), $.copy(key));
  if (($.copy(critical_bit)).lt($.copy(search_parent_critical_bit)))
    insert_below(tree, $.copy(key), value, $.copy(n_outer_nodes), $.copy(n_inner_nodes), $.copy(search_index), search_child_side, $.copy(search_key), $.copy(search_parent_index), $.copy(critical_bit), $c, [$p[0]]);
  else
    insert_above(tree, $.copy(key), value, $.copy(n_outer_nodes), $.copy(n_inner_nodes), $.copy(search_parent_index), $.copy(critical_bit), $c, [$p[0]]);
}

/// Insert `key` and `value` into singleton `tree`, aborting if
/// `key` already in `tree`
export function insert_singleton (
  tree: CritBitTree,
  key: U128,
  value: any,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let critical_bit, outer_node;
  outer_node = Stdlib.Vector.borrow_(tree.outer_nodes, u64("0"), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  if (!($.copy(key)).neq($.copy(outer_node.key))) {
    throw $.abortCode($.copy(E_HAS_KEY));
  }
  critical_bit = crit_bit($.copy(outer_node.key), $.copy(key));
  push_back_insert_nodes(tree, $.copy(key), value, u64("0"), $.copy(critical_bit), $.copy(ROOT), ($.copy(key)).gt($.copy(outer_node.key)), outer_node_child_index(u64("0"), $c), outer_node_child_index(u64("1"), $c), $c, [$p[0]]);
  tree.root = u64("0");
  Stdlib.Vector.borrow_mut_(tree.outer_nodes, u64("0"), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).parent_index = u64("0");
  return;
}

export function is_empty (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): boolean {
  return Stdlib.Vector.is_empty_(tree.outer_nodes, $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
}

export function is_outer_node (
  child_field_index: U64,
  $c: AptosDataCache,
): boolean {
  return ((($.copy(child_field_index)).shr($.copy(NODE_TYPE))).and($.copy(OUTER))).eq(($.copy(OUTER)));
}

export function is_set (
  key: U128,
  bit_number: U8,
  $c: AptosDataCache,
): boolean {
  return ((($.copy(key)).shr($.copy(bit_number))).and(u128("1"))).eq((u128("1")));
}

export function length (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U64 {
  return Stdlib.Vector.length_(tree.outer_nodes, $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
}

export function max_key (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U128 {
  if (is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_LOOKUP_EMPTY));
  }
  return $.copy(Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index(max_node_child_index(tree, $c, [$p[0]]), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).key);
}

export function max_node_child_index (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U64 {
  let child_field_index;
  child_field_index = $.copy(tree.root);
  while (true) {
    if (is_outer_node($.copy(child_field_index), $c)) {
      return $.copy(child_field_index);
    }
    else{
    }
    child_field_index = $.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_field_index), $c, [new SimpleStructTag(InnerNode)]).right_child_index);
  }
}
/// Return the minimum key in `tree`, aborting if `tree` is empty
export function min_key(
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U128 {
  if (is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_LOOKUP_EMPTY));
  }
  return $.copy(Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index(min_node_child_index(tree, $c, [$p[0]]), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).key);
}

export function min_node_child_index (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U64 {
  let child_field_index;
  child_field_index = $.copy(tree.root);
  while (true) {
    if (is_outer_node($.copy(child_field_index), $c)) {
      return $.copy(child_field_index);
    }
    else{
    }
    child_field_index = $.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_field_index), $c, [new SimpleStructTag(InnerNode)]).left_child_index);
  }
}

export function outer_node_child_index (
  vector_index: U64,
  $c: AptosDataCache,
): U64 {
  return ($.copy(vector_index)).or(($.copy(OUTER)).shl($.copy(NODE_TYPE)));
}

export function outer_node_vector_index (
  child_field_index: U64,
  $c: AptosDataCache,
): U64 {
  return (($.copy(child_field_index)).and($.copy(HI_64))).xor(($.copy(OUTER)).shl($.copy(NODE_TYPE)));
}

/// Pop from `tree` value corresponding to `key`, aborting if `tree`
/// is empty or does not contain `key`
export function pop (
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let pop_data, _length;
  if (is_empty(tree, $c, [$p[0]])) {
    throw $.abortCode($.copy(E_POP_EMPTY));
  }
  _length = length(tree, $c, [$p[0]]);
  if (($.copy(_length)).eq((u64("1")))) {
    pop_data = pop_singleton(tree, $.copy(key), $c, [$p[0]]);
  }
  else{
    pop_data = pop_general(tree, $.copy(key), $.copy(_length), $c, [$p[0]]);
  }
  return pop_data;
}

export function pop_destroy_nodes (
  tree: CritBitTree,
  inner_index: U64,
  outer_index: U64,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let n_inner_nodes;
  n_inner_nodes = Stdlib.Vector.length_(tree.inner_nodes, $c, [new SimpleStructTag(InnerNode)]);
  Stdlib.Vector.swap_remove_(tree.inner_nodes, $.copy(inner_index), $c, [new SimpleStructTag(InnerNode)]);
  if (($.copy(inner_index)).lt(($.copy(n_inner_nodes)).sub(u64("1")))) {
    stitch_swap_remove(tree, $.copy(inner_index), $.copy(n_inner_nodes), $c, [$p[0]]);
  }
  else{
  }
  let { value: value } = Stdlib.Vector.swap_remove_(tree.outer_nodes, outer_node_vector_index($.copy(outer_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  if ((outer_node_vector_index($.copy(outer_index), $c)).lt(($.copy(n_outer_nodes)).sub(u64("1")))) {
    stitch_swap_remove(tree, $.copy(outer_index), $.copy(n_outer_nodes), $c, [$p[0]]);
  }
  else{
  }
  return value;
}

export function pop_general (
  tree: CritBitTree,
  key: U128,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let temp$1, temp$2, search_child_side, search_index, search_key, search_parent_index;
  [temp$1, temp$2] = [tree, $.copy(key)];
  [search_index, search_child_side, search_key, search_parent_index, ] = search_outer(temp$1, temp$2, $c, [$p[0]]);
  if (!($.copy(search_key)).eq(($.copy(key)))) {
    throw $.abortCode($.copy(E_NOT_HAS_KEY));
  }
  pop_update_relationships(tree, search_child_side, $.copy(search_parent_index), $c, [$p[0]]);
  return pop_destroy_nodes(tree, $.copy(search_parent_index), $.copy(search_index), $.copy(n_outer_nodes), $c, [$p[0]]);
}

export function pop_singleton (
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  if (!($.copy(Stdlib.Vector.borrow_(tree.outer_nodes, u64("0"), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).key)).eq(($.copy(key)))) {
    throw $.abortCode($.copy(E_NOT_HAS_KEY));
  }
  tree.root = u64("0");
  let { value: value } = Stdlib.Vector.pop_back_(tree.outer_nodes, $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  return value;
}

export function pop_update_relationships(
  tree: CritBitTree,
  child_side: boolean,
  parent_index: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let temp$1, grandparent, grandparent_index, parent, sibling_index;
  parent = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(parent_index), $c, [new SimpleStructTag(InnerNode)]);
  if ((child_side == $.copy(LEFT))) {
    temp$1 = $.copy(parent.right_child_index);
  }
  else{
    temp$1 = $.copy(parent.left_child_index);
  }
  sibling_index = temp$1;
  grandparent_index = $.copy(parent.parent_index);
  if (is_outer_node($.copy(sibling_index), $c)) {
    Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(sibling_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).parent_index = $.copy(grandparent_index);
  }
  else{
    Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(sibling_index), $c, [new SimpleStructTag(InnerNode)]).parent_index = $.copy(grandparent_index);
  }
  if (($.copy(grandparent_index)).eq(($.copy(ROOT)))) {
    tree.root = $.copy(sibling_index);
  }
  else{
    grandparent = Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(grandparent_index), $c, [new SimpleStructTag(InnerNode)]);
    if (($.copy(grandparent.left_child_index)).eq(($.copy(parent_index)))) {
      grandparent.left_child_index = $.copy(sibling_index);
    }
    else{
      grandparent.right_child_index = $.copy(sibling_index);
    }
  }
  return;
}
/// Push back a new inner node and outer node into `tree`, where the
  /// new outer node should have key `key`, value `value`, and have as
  /// its parent the new inner node at vector index `inner_index`,
  /// which should have critical bit `critical_bit`, parent field
  /// index `parent_index`, and if `child_polarity` is `true`, left
  /// child field index `child_index_1` and right child field index
  /// `child_index_2`. If `child_polarity` is `false` the polarity of
  /// the children should be flipped
export function push_back_insert_nodes(
  tree: CritBitTree,
  key: U128,
  value: any,
  inner_index: U64,
  critical_bit: U8,
  parent_index: U64,
  child_polarity: boolean,
  child_index_1: U64,
  child_index_2: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): void {
  let left_child_index, right_child_index;
  if (child_polarity) {
    [left_child_index, right_child_index] = [$.copy(child_index_1), $.copy(child_index_2)];
  }
  else{
    [left_child_index, right_child_index] = [$.copy(child_index_2), $.copy(child_index_1)];
  }

  Stdlib.Vector.push_back_(tree.outer_nodes, new OuterNode({ key: $.copy(key), value: value, parent_index: $.copy(inner_index) }, new SimpleStructTag(OuterNode, [$p[0]])), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  Stdlib.Vector.push_back_(tree.inner_nodes, new InnerNode({ critical_bit: $.copy(critical_bit), parent_index: $.copy(parent_index), left_child_index: $.copy(left_child_index), right_child_index: $.copy(right_child_index) }, new SimpleStructTag(InnerNode)), $c, [new SimpleStructTag(InnerNode)]);
  return;
}
  /// Walk from root in a `tree` having an inner node as its root,
  /// branching left or right at each inner node depending on whether
  /// `key` is unset or set, respectively, at the given critical bit.
  /// After arriving at an outer node, then return:
  /// * `u64`: Child field index of search outer node (with node type
  ///   bit flag)
  /// * `bool`: The side, `LEFT` or `RIGHT`, on which the search outer
  ///   node is a child of its parent
  /// * `u128`: Key of search outer node
  /// * `u64`: Vector index of parent of search outer node
  /// * `u8`: Critical bit indicated by parent of search outer node
export function search_outer(
  tree: CritBitTree,
  key: U128,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U64, boolean, U128, U64, U8] {
  let index, node, parent, side;
  parent = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(tree.root), $c, [new SimpleStructTag(InnerNode)]);
  while (true) {
    if (is_set($.copy(key), $.copy(parent.critical_bit), $c)) {
      [index, side] = [$.copy(parent.right_child_index), $.copy(RIGHT)];
    }
    else{
      [index, side] = [$.copy(parent.left_child_index), $.copy(LEFT)];
    }
    if (is_outer_node($.copy(index), $c)) {
      node = Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index($.copy(index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
      return [$.copy(index), side, $.copy(node.key), $.copy(node.parent_index), $.copy(parent.critical_bit)];
    }
    else{
    }
    parent = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(index), $c, [new SimpleStructTag(InnerNode)]);
  }
}

/// Return a tree with one node having `key` and `value`
export function singleton(
  key: U128,
  value: any,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): CritBitTree {
  let tree;
  tree = new CritBitTree({ root: u64("0"), inner_nodes: Stdlib.Vector.empty_($c, [new SimpleStructTag(InnerNode)]), outer_nodes: Stdlib.Vector.empty_($c, [new SimpleStructTag(OuterNode, [$p[0]])]) }, new SimpleStructTag(CritBitTree, [$p[0]]));
  insert_empty(tree, $.copy(key), value, $c, [$p[0]]);
  return tree;
}
/// Update parent node at index `parent_index` in `tree` to reflect
/// as its child a node that has been relocated from old child field
/// index `old_index` to new child field index `new_index`
export function stitch_child_of_parent(
  tree: CritBitTree,
  new_index: U64,
  parent_index: U64,
  old_index: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
){
  let parent;
  parent = Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(parent_index), $c, [new SimpleStructTag(InnerNode)]);
  if (($.copy(parent.left_child_index)).eq(($.copy(old_index)))) {
    parent.left_child_index = $.copy(new_index);
  }
  else{
    parent.right_child_index = $.copy(new_index);
  }
}

export function stitch_parent_of_child(
  tree: CritBitTree,
  new_index: U64,
  child_index: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
){
  if (is_outer_node($.copy(child_index), $c)) {
    Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(child_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).parent_index = $.copy(new_index);
  }
  else{
    Stdlib.Vector.borrow_mut_(tree.inner_nodes, $.copy(child_index), $c, [new SimpleStructTag(InnerNode)]).parent_index = $.copy(new_index);
  }  
}

export function stitch_swap_remove(
  tree: CritBitTree,
  node_index: U64,
  n_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
) {
  let left_child_index, node, parent_index, parent_index__1, right_child_index;
  if (is_outer_node($.copy(node_index), $c)) {
    parent_index = $.copy(Stdlib.Vector.borrow_(tree.outer_nodes, outer_node_vector_index($.copy(node_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]).parent_index);
    if (($.copy(parent_index)).eq(($.copy(ROOT)))) {
      tree.root = $.copy(node_index);
      return;
    }
    else{
    }
    stitch_child_of_parent(tree, $.copy(node_index), $.copy(parent_index), outer_node_child_index(($.copy(n_nodes)).sub(u64("1")), $c), $c, [$p[0]]);
  }
  else{
    node = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(node_index), $c, [new SimpleStructTag(InnerNode)]);
    [parent_index__1, left_child_index, right_child_index] = [$.copy(node.parent_index), $.copy(node.left_child_index), $.copy(node.right_child_index)];
    stitch_parent_of_child(tree, $.copy(node_index), $.copy(left_child_index), $c, [$p[0]]);
    stitch_parent_of_child(tree, $.copy(node_index), $.copy(right_child_index), $c, [$p[0]]);
    if (($.copy(parent_index__1)).eq(($.copy(ROOT)))) {
      tree.root = $.copy(node_index);
      return;
    }
    else{
    }
    stitch_child_of_parent(tree, $.copy(node_index), $.copy(parent_index__1), ($.copy(n_nodes)).sub(u64("1")), $c, [$p[0]]);
  }  
}
/// Terminate iterated traversal by popping the outer node for the
/// current iteration, without traversing further. Implements
/// similar algorithms as `pop_general()`, but without having to
/// do another search from root.
///
/// # Parameters
/// * `tree`: Crit-bit tree containing at least one node
/// * `parent_index`: Node's parent field
/// * `child_index`: Child field index of node
/// * `n_outer_node`: Number of outer nodes in `tree`
///
/// # Returns
/// * `V`: Popped value from outer node
///
/// # Considerations
/// * Takes exposed node indices (`parent_index`, `child_index`) as
///   parameters
/// * Does not calculate number of outer nodes in `tree`, but rather
///   accepts this number as a parameter (`n_outer_nodes`), which
///   should be tracked by the caller and should be nonzero
export function traverse_end_pop (
  tree: CritBitTree,
  parent_index: U64,
  child_index: U64,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): any {
  let pop_data, node_child_side;
  if (($.copy(n_outer_nodes)).eq((u64("1")))) {
    tree.root = u64("0");
    let { value: value } = Stdlib.Vector.pop_back_(tree.outer_nodes, $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
    pop_data = value;
  }
  else{
    node_child_side = ($.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(parent_index), $c, [new SimpleStructTag(InnerNode)]).left_child_index)).eq(($.copy(child_index)));
    pop_update_relationships(tree, node_child_side, $.copy(parent_index), $c, [$p[0]]);
    pop_data = pop_destroy_nodes(tree, $.copy(parent_index), $.copy(child_index), $.copy(n_outer_nodes), $c, [$p[0]]);
  }
  return pop_data;
}

/// Initialize a mutable iterated inorder traversal in a tree having
/// at least one outer node. See [traversal](#Traversal)
///
/// # Parameters
/// * `tree`: A crit-bit tree containing at least one outer node
/// * `direction`: Direction to traverse. If `LEFT`, initialize
///   predecessor traversal, else successor traversal
///
/// # Returns
/// * `u128`: Maximum key in `tree` if `direction` is `LEFT`, else
///    minimum key
/// * `&mut V`: Mutable reference to corresponding node's value
/// * `u64`: Parent field of corresponding node
/// * `u64`: Child field index of corresponding node
///
/// # Considerations
/// * Exposes node indices
/// * Assumes caller has already verified tree is not empty
export function traverse_init_mut(
  tree: CritBitTree,
  direction: boolean,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  let child_field_index, node;
  if ((direction == $.copy(LEFT))) {
    child_field_index = max_node_child_index(tree, $c, [$p[0]]);
  }
  else{
    child_field_index = min_node_child_index(tree, $c, [$p[0]]);
  }

  node = Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(child_field_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  return [$.copy(node.key), node.value, $.copy(node.parent_index), $.copy(child_field_index)];
}

/// Wrapped `traverse_target_child_index()` call for enumerated
/// return extraction. See [traversal](#Traversal)
///
/// # Returns
/// * `u128`: Target key
/// * `&mut V`: Mutable reference to target node's value
/// * `u64`: Target node's parent field
/// * `u64`: Child field index of target node
export function traverse_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  direction: boolean,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  let node, target_child_index;
  target_child_index = traverse_target_child_index(tree, $.copy(key), $.copy(parent_index), direction, $c, [$p[0]]);
  node = Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(target_child_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  return [$.copy(node.key), node.value, $.copy(node.parent_index), $.copy(target_child_index)];
}

/// Traverse in the specified direction from the node containing the
/// specified key (the "start node" containing the "start key") to
/// either the inorder predecessor or the inorder successor to the
/// start key (the "target node" containing the "target key"), then
/// pop the start node and return its value. See
/// [traversal](#Traversal)
///
/// # Parameters
/// * `tree`: Crit-bit tree containing at least two nodes
/// * `key`: Start key. If predecessor traversal, cannot be minimum
///   key in `tree`, since this key does not have a predecessor.
///   Likewise, if successor traversal, cannot be maximum key in
///   `tree`, since this key does not have a successor
/// * `parent_index`: Start node's parent field
/// * `child_index`: Child index of start node
/// * `n_outer_nodes`: Number of outer nodes in `tree`
/// * `direction`: Direction to traverse. If `LEFT`, predecessor
///   traversal, else successor traversal
///
/// # Returns
/// * `u128`: Target key
/// * `&mut V`: Mutable reference to target node's value
/// * `u64`: Target node's parent field
/// * `u64`: Child field index of target node
/// * `V`: Popped start node's value
///
/// # Considerations
/// * Assumes passed start key is not minimum key in tree if
///   predecessor traversal, and that passed start key is not
///   maximum key in tree if successor traversal
/// * Takes exposed node indices (`parent_index`, `child_index`) as
///   parameters
/// * Does not calculate number of outer nodes in `tree`, but rather
///   accepts this number as a parameter (`n_outer_nodes`), which
///   should be tracked by the caller
export function traverse_pop_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  child_index: U64,
  n_outer_nodes: U64,
  direction: boolean,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64, any] {
  let start_child_side, start_value, target_child_index, target_node;
  start_child_side = ($.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(parent_index), $c, [new SimpleStructTag(InnerNode)]).left_child_index)).eq(($.copy(child_index)));
  target_child_index = traverse_target_child_index(tree, $.copy(key), $.copy(parent_index), direction, $c, [$p[0]]);
  pop_update_relationships(tree, start_child_side, $.copy(parent_index), $c, [$p[0]]);
  start_value = pop_destroy_nodes(tree, $.copy(parent_index), $.copy(child_index), $.copy(n_outer_nodes), $c, [$p[0]]);
  if ((outer_node_vector_index($.copy(target_child_index), $c)).eq((($.copy(n_outer_nodes)).sub(u64("1"))))) {
    target_child_index = $.copy(child_index);
  }
  else{
  }
  target_node = Stdlib.Vector.borrow_mut_(tree.outer_nodes, outer_node_vector_index($.copy(target_child_index), $c), $c, [new SimpleStructTag(OuterNode, [$p[0]])]);
  return [$.copy(target_node.key), target_node.value, $.copy(target_node.parent_index), $.copy(target_child_index), start_value];
}

/// Wrapped `traverse_init_mut()` call for predecessor traversal.
/// See [traversal walkthrough](#Walkthrough)
export function traverse_predecessor_init_mut (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  return traverse_init_mut(tree, $.copy(LEFT), $c, [$p[0]]);
}

/// Wrapped `traverse_mut()` call for predecessor traversal. See
/// [traversal walkthrough](#Walkthrough)
export function traverse_predecessor_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  return traverse_mut(tree, $.copy(key), $.copy(parent_index), $.copy(LEFT), $c, [$p[0]]);
}

/// Wrapped `traverse_pop_mut()` call for predecessor traversal. See
/// [traversal walkthrough](#Walkthrough)
export function traverse_predecessor_pop_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  child_index: U64,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64, any] {
  return traverse_pop_mut(tree, $.copy(key), $.copy(parent_index), $.copy(child_index), $.copy(n_outer_nodes), $.copy(LEFT), $c, [$p[0]]);
}

/// Wrapped `traverse_init_mut()` call for successor traversal.
/// See [traversal walkthrough](#Walkthrough)
export function traverse_successor_init_mut (
  tree: CritBitTree,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  return traverse_init_mut(tree, $.copy(RIGHT), $c, [$p[0]]);
}

/// Wrapped `traverse_mut()` call for successor traversal. See
/// [traversal walkthrough](#Walkthrough)
export function traverse_successor_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64] {
  return traverse_mut(tree, $.copy(key), $.copy(parent_index), $.copy(RIGHT), $c, [$p[0]]);
}

/// Wrapped `traverse_pop_mut()` call for successor traversal. See
/// [traversal walkthrough](#Walkthrough)
export function traverse_successor_pop_mut (
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  child_index: U64,
  n_outer_nodes: U64,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): [U128, any, U64, U64, any] {
  return traverse_pop_mut(tree, $.copy(key), $.copy(parent_index), $.copy(child_index), $.copy(n_outer_nodes), $.copy(RIGHT), $c, [$p[0]]);
}

/// Traverse in the specified direction from the node containing the
/// specified key (the "start node" containing the "start key") to
/// either the inorder predecessor or the inorder successor to the
/// start key (the "target node" containing the "target key"), then
/// return the child field index of the target node. See
/// [traversal](#Traversal)
///
/// # Method (predecessor)
/// 1. Walk up from start node until arriving at an inner node that
///    has the start key as the minimum key in its right subtree
///    (the "apex node"): walk up until arriving at a parent that
///    has the last walked node as its right child
/// 2. Walk to maximum key in apex node's left subtree, breaking out
///    at target node (the first outer node): walk to apex node's
///    left child, then walk along right children
///
/// # Method (successor)
/// 1. Walk up from start node until arriving at an inner node that
///    has the start key as the maximum key in its left subtree
///    (the "apex node"): walk up until arriving at a parent that
///    has the last walked node as its left child
/// 2. Walk to minimum key in apex node's right subtree, breaking
///    out at target node (the first outer node): walk to apex
///    node's right child, then walk along left children
///
/// # Parameters
/// * `tree`: Crit-bit tree containing at least two nodes
/// * `key`: Start key. If predecessor traversal, `key` cannot be
///   minimum key in `tree`, since this key does not have a
///   predecessor. Likewise, if successor traversal, `key` cannot be
///   maximum key in `tree`, since this key does not have a
///   successor
/// * `parent_index`: Start node's parent field
/// * `direction`: Direction to traverse. If `LEFT`, predecessor
/// traversal, else successor traversal
///
/// # Returns
/// * `u64`: Child field index of target node
///
/// # Considerations
/// * Assumes passed start key is not minimum key in tree if
///   predecessor traversal, and that passed start key is not
///   maximum key in tree if successor traversal
/// * Takes an exposed vector index (`parent_index`) as a parameter
export function traverse_target_child_index(
  tree: CritBitTree,
  key: U128,
  parent_index: U64,
  direction: boolean,
  $c: AptosDataCache,
  $p: TypeTag[], /* <V>*/
): U64 {
  let temp$2, child_index, parent;
  parent = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(parent_index), $c, [new SimpleStructTag(InnerNode)]);
  while (direction != is_set($.copy(key), $.copy(parent.critical_bit), $c)) {
    parent = Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(parent.parent_index), $c, [new SimpleStructTag(InnerNode)]);
  }
  child_index = direction == $.copy(LEFT) ? $.copy(parent.left_child_index): $.copy(parent.right_child_index);

  while (!is_outer_node($.copy(child_index), $c)) {
      child_index = direction == $.copy(LEFT)?
         $.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_index), $c, [new SimpleStructTag(InnerNode)]).right_child_index) :
         $.copy(Stdlib.Vector.borrow_(tree.inner_nodes, $.copy(child_index), $c, [new SimpleStructTag(InnerNode)]).left_child_index);
  }
  return $.copy(child_index);
}

export function loadParsers(repo: AptosParserRepo) {
  repo.addParser("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd::critbit::CritBitTree", CritBitTree.CritBitTreeParser);
  repo.addParser("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd::critbit::InnerNode", InnerNode.InnerNodeParser);
  repo.addParser("0xc0deb00c9154b6b64db01eeb77d08255300315e1fa35b687d384a703f6034fbd::critbit::OuterNode", OuterNode.OuterNodeParser);
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
  get CritBitTree() { return CritBitTree; }
  get InnerNode() { return InnerNode; }
  get OuterNode() { return OuterNode; }
}


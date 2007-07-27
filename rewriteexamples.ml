open Utility
open List

open Rewriterules


let examples : (string * string) list = [
(*
  From examples/paginate.links.
  
  Not quite there: need to handle concatenation in base_exprs (for
  the like) and arithemetic in base exprs (for the drop).  Also no
  sort.  
*)
"paginate",
"fun(n,c) {
   var db = database \"dictionary\";
   var wordlist = table \"wordlist\" with (
      word: String,
      meaning: String
   ) from db;

   take(results_per_page,
   drop(results_per_page * (n-1),
     for (var r <-- wordlist)
     where (r.word ~ /{c}.*/)
     orderby (r.word)
       [r]))
 }";

(*
  From examples/draggable-db.links.
  
  No sort.
*)
"draggable",
"for (var item <-- itemsTable())
 orderby (item.i)
   [item.name]";

(*
  From examples/factorial.links
  
  No Sort.
*)
"factorial",
"for (var row <-- factorials)
 where (row.i <= n)
 orderby (row.i)
   [(row.i, row.f)]";

(*
  From examples/initialise-list.links
*)
"initialise-list",
"for (var itemEntry <-- itemsTable) [itemEntry.i]";

(*
  From examples/todo-db.links
*)
"todo-db",
"for (var item <-- items)
   <tr><td>{stringToXml(item.name)}</td>
       <td><form l:onsubmit=\"{remove(item.name)}\" method=\"POST\">
            <button type=\"submit\">Done</button>
           </form>
       </td>
   </tr>";

(*
  From examples/wine.links
*)
"wine 1",
"for (var wineType <-- wineTypeTable)
 where (wineType.wine_type_id == wine_type_id)
    stringToXml(wineType.wine_type)";

"wine 2",
"for (var u <-- usersTable)
     [u.cust_id]";

"wine 3",
"for (var u <-- usersTable)
 where (u.user_name == username &&
        u.password == password)
    [u.cust_id]";

"wine 4",
"for (var cust <-- usersTable)
 where (cust.cust_id == cust_id)
   stringToXml(cust.user_name)";

"wine 5",
"for (var item <-- cartItemsTable)
  for (var wine <-- wineTable)
    where (item.wine_id == wine.wine_id && item.order_id == order_id)
      <tr>
        <td> {intToXml(item.qty)} </td>
        <td> {stringToXml(wine.wine_name)} </td>
        <td> {floatToXml(item.price)} </td>
        <td> {floatToXml(item.price *. intToFloat(item.qty))} </td>
      </tr>";

"wine 6",
"for (var item <-- cartItemsTable)
 where (item.cust_id == cust_id
     && item.order_id == order_id)
     [item.price]";

"wine 7",
"for (var x <-- orderTable )
 where (cust_id == x.cust_id && order_id == x.order_id)
   [x]";

"wine 8",
"not here!";


"wine 9",
"for (var wine <-- wineTable)
   where (wine.wine_id == wine_id)
   [wine.wine_name]";

"wine 10",
"for (var region <-- regionTable)
 where (region_id == region.region_id)
    region.region_name";

"wine 11",
"for (var cost_rec <-- inventoryTable)
 where (wine_id == cost_rec.wine_id) 
   [cost_rec.cost])";

"wine 12",
"for (var cart_item <-- cartItemsTable)
 where (cart_item.order_id == order_id &&
        cart_item.cust_id == cust_id)
{
   for (var wine <-- wineTable)
   where (wine.wine_id == cart_item.wine_id)
      for (var cost_rec <-- inventoryTable)
      where (wine.wine_id == cost_rec.wine_id)
          [(cart_item.qty, cost_rec.cost)]
}";

"wine 13",
"for (var cart_item <-- cartItemsTable)
 where (cart_item.order_id == order_id
     && cart_item.cust_id == cust_id)
     [cart_item.item_id]";


"wine 14",
"for (var cart <-- shortOrderTable)
     [cart.order_id]";

"wine 15",
"for (var wine <-- wineTable )
 where (wine_type == 1 || wine.wine_type == wine_type)
 {
   for (var winery <-- wineryTable)
      where (winery.winery_id == wine.winery_id
             && (region_id == 1 || winery.region_id == region_id))
      { 
          for (var cost_rec <-- inventoryTable)
          where (wine.wine_id == cost_rec.wine_id)
            [(wine.wine_id, wine.wine_name, cost_rec.cost,
              wine.year, winery.winery_name)]
      }
 }))";

"wine 16",
"for (var wine <-- wineTable)
 where (wine_type == 1 || wine.wine_type == wine_type)
 {
    for (var winery <-- wineryTable)
    where (winery.winery_id == wine.winery_id
       && (region_id == 1 || winery.region_id == region_id))
   { 
       for (var cost_rec <-- inventoryTable)
       where (wine.wine_id == cost_rec.wine_id)
         [(wine.wine_id, wine.wine_name, cost_rec.cost, wine.year, winery.winery_name)]
   }
 }";

"wine 17",
"for (var region <-- regionTable)
  # (boutrosed)
  if (region_id == region.region_id)
   <option selected=\"SELECTED\" 
       value=\"{intToString(region.region_id)}\">{stringToXml(region.region_name)}</option>
  else
   <option 
      value=\"{intToString(region.region_id)}\">{stringToXml(region.region_name)}</option>";

"wine 18",
"for (var type <-- wineTypeTable)
   # (boutrosed)
   if (wine_type == type.wine_type_id)
     <option selected=\"SELECTED\" 
       value=\"{intToString(type.wine_type_id)}\">{stringToXml(type.wine_type)}</option>
   else
     <option 
       value=\"{intToString(type.wine_type_id)}\">{stringToXml(type.wine_type)}</option>";

(*
  From examples/dictionary/dict-suggest.links
  (same as examples/dictionary/dict-suggest-lite.links)
*)
"dict-suggest",
  "take(10, for (var w <-- wordlist)
            where (w.word ~ /{pre}.*/)
            orderby (w.word)
              [w])";
]
let get = flip List.assoc examples

let compile_largest : Syntax.expression -> (Syntax.expression * expr) list = 
  let tryone default expr = 
    match Compile.compile expr with
      | Some output -> [(expr, output)]
      | None -> default expr in
    Syntax.reduce_expression tryone (snd ->- List.concat)

let library_globals = List.map fst (fst Library.typing_env) 
  
let bind_freevars e : Syntax.untyped_expression = 
  List.fold_right (fun var e -> 
                     if mem var library_globals then e else
                       Syntax.Abstr ([var], e, `U Syntax.dummy_position)) (StringSet.elements(Syntax.freevars e)) e
    
let parse = Parse.parse_string Parse.program ->- Syntax.program_body
let type_e = Inference.type_expression Library.typing_env ->- snd

let run = 
  parse ->- bind_freevars ->- type_e ->- Prepare.normalize ->- compile_largest ->- List.map (fun (x,l) -> (Syntax.strip_data x, l))




let tables code = "{
var usersTable = table \"users\"
                  with (cust_id : Int, user_name : String, 
                        password : String) from db;

var orderTable = table \"orders\"
                  with (cust_id : Int, order_id : Int, date : String,
                        instructions : String, creditcard : String,
                        expirydate : String)
                   from db;

var shortOrderTable = table \"orders\"
                       with (cust_id : Int, order_id : Int)
                   from db;

var cartItemsTable = table \"items\"
                      with (item_id : Int, cust_id : Int, order_id : Int, 
                            price : Float, qty : Int,
                            wine_id : Int)
                      from db;

var wineTable = table \"wine\" 
                 with (wine_id : Int, wine_name : String, wine_type : Int,
                       year : Int, winery_id : Int)
                 from db;

var wineTypeTable = table \"wine_type\" 
                     with (wine_type_id : Int, wine_type : String)
                     from db;

var regionTable = table \"region\"
                  with (region_id : Int, region_name : String)
                  from db;

var inventoryTable = table \"inventory\" with
                     (wine_id : Int, cost : Float)
                     from db;

var wineryTable = table \"winery\" with 
                  (winery_id : Int, winery_name : String,
                  region_id : Int)
                  from db;

var factorials = table \"factorials\" with (i : Int, f : Int) from db;

var wordlist = table \"wordlist\" with (
   word : String, 
   type : String, 
   meaning : String
 ) from (database \"dictionary\");

" ^ code ^ "}"

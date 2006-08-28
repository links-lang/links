var zero = 0;

var db = database (driver="mysql",
                   name="winestore",
                   args="localhost:5432:www-data:");

var usersTable = table "users"
                  with (cust_id : Int, user_name : String, 
                        password : String) from db;

var orderTable = table "orders"
                  with (cust_id : Int, order_id : Int, date : String,
                        instructions : String, creditcard : String,
                        expirydate : String)
                   from db;

var cartItemsTable = table "items"
                      with (cust_id : Int, order_id : Int, 
                            price : Float, qty : Int,
                            wine_id : Int)
                      from db;

var wineTable = table "wine" 
                 with (wine_id : Int, wine_name : String, wine_type : Int,
                       year : Int, winery_id : Int)
                 from db;

var wineTypeTable = table "wine_type" 
                     with (wine_type_id : Int, wine_type : String)
                     from db;

var regionTable = table "region"
                  with (region_id : Int, region_name : String)
                  from db;

var inventoryTable = table "inventory" with
                     (wine_id : Int, cost : Float)
                     from db;

var wineryTable = table "winery" with 
                  (winery_id : Int, winery_name : String,
                  region_id : Int)
                  from db;

fun snd(pair) {
  var (a, b) = pair;
  b
}

fun map(f, l) {
    if (l == []) []
    else [f(hd(l))] ++ map(f, tl(l))
}

fun floatToXml(x) { stringToXml(floatToString(x)) } 

fun sum_float(l) {
  if (l == [])
      0.
  else
      hd(l) +. sum_float(tl(l))
}

fun max(result, l) {
  if (l == [])
    result
  else
    if (result >= hd(l))  
      max(result, tl(l))
    else
      max(hd(l), tl(l))
}

fun assoc(x, l, d) {
    if (l == []) { d }
    else {
        var (k, v) = hd(l);
        if (x == k)
            v
        else assoc(x, tl(l), d)
    }
}

fun wineTypeName(wine_type_id) {
  for (var wineType <-- wineTypeTable)
     where (wineType.wine_type_id == wine_type_id)
       stringToXml(wineType.wine_type)
}

fun cust_id_next() {  # nasty
  var ids = for (var u <-- usersTable)
           [u.cust_id];
  max(0, ids) + 1
}

sig sign_up : (String, String) -> XML

fun sign_up(username, password)  {

  var new_cust_id = cust_id_next();

  insert (usersTable) values
    [( cust_id = new_cust_id, user_name = username, password = password )];
  <div>
    Welcome, {stringToXml(username)}!
    <a l:href="{search_results(new_cust_id, -1, -1, -1)}">Continue shopping</a>
  </div>
}

fun sign_up_form() {
  <html>
    <form l:action="{sign_up(username, password)}">
      <table>
        <tr><td>
          Username:</td><td> <input type="text" l:name="username" />
        </td></tr>
        <tr><td>
          Password:</td><td> <input type="text" l:name="password" />
        </td></tr>
        <tr><td>
          </td><td> <input type="submit" value="Sign Up" />
        </td></tr>
      </table>
    </form>
  </html>
}

fun sign_in(username, password) {

  var cust_id = 
    for (var u <-- usersTable)
    where (u.user_name == username &&
           u.password == password)
      [u.cust_id];    

  if (cust_id == [])
    stringToXml("Incorrect username/password combination")
  else
    search_results(hd(cust_id), -1, -1, -1)
}

fun sign_in_form() {
  <html>
    <form l:action="{sign_in(username, password)}">
      Enter your username and password here:
      <table>
        <tr><td>
          Username:</td><td> <input type="text" l:name="username" />
        </td></tr>
        <tr><td>
          Password:</td><td> <input type="text" l:name="password" />
        </td></tr>
        <tr><td>
        </td> <td> <input type="submit" value="Sign in" />
        </td></tr>

      </table>
    </form>
  </html>
}

fun logout() {
   search_results(-1, -1, -1, -1)
}

sig header : Int -> XML

fun header(cust_id) {
  if (cust_id == -1)
    <div align="right">
      <a l:href="{sign_in_form()}">Sign in</a> or
      <a l:href="{sign_up_form()}">create an account</a>.
    </div>
  else
    <div align="right">Welcome, {for (var cust <-- usersTable)
                                 where (cust.cust_id == cust_id)
                                   stringToXml(cust.user_name)}.
         <a l:href="{logout()}">Logout</a></div>
}

fun footer(cust_id) {
  <div />
}

fun valid_cc_details(card_no, expiry) {
  card_no == "8000000000001001"
}

fun purchase_confirmation(cust_id, order_id, total) {
  debug("arrived at purchase confirmation with " ++ intToString(cust_id)
        ++ " " ++ intToString(order_id) ++ " " ++ floatToString(total));
  <html>
    Your order (reference # {stringToXml(intToString(cust_id))} - {stringToXml(intToString(order_id))}) has been dispatched
    Thank you Mr Claus, your order has been completed and dispatched. 
    <p>Your order reference number is {stringToXml(intToString(cust_id))} - {stringToXml(intToString(order_id))}.
    Please quote this number in any correspondence.</p>

    If it existed, the order would have been shipped to:
    DETAILS

    We have billed your fictional credit card.
    <table>
      <tr>
        <td> Quantity </td>
        <td> Wine </td>
        <td> Unit Price </td>
        <td> Total </td>
      </tr>
      {
        for (var item <-- cartItemsTable)
          for (var wine <-- wineTable)
            where (item.wine_id == wine.wine_id && item.order_id == order_id)
              <tr>
                <td> {intToXml(item.qty)} </td>
                <td> {stringToXml(wine.wine_name)} </td>
                <td> {floatToXml(item.price)} </td>
                <td> {floatToXml(item.price *. intToFloat(item.qty))} </td>
              </tr>
      }
    <tr>
     <td colspan="3">Total of this order</td> 	<td>{floatToXml(total)}</td>
    </tr>
    </table>

    <p>An email confirmation has NOT been sent to you. Thank you for shopping 
    with Linkswine.</p>
    Return to <a l:href="{search_results(cust_id, -1, -1, -1)}">main page</a>.
  </html>
}

sig order_total : (Int, Int) -> Float
fun order_total(cust_id, order_id) {
  sum_float(
    for (var item <-- cartItemsTable)
    where (item.cust_id == cust_id && item.order_id == order_id)
      [item.price]
  )
}

fun checkout(cust_id, order_id, card_no, expiry, instr) {
  var total = order_total(cust_id, order_id);
  if (valid_cc_details(card_no, expiry)) {
    var the_orders = 
      for (var x <-- orderTable )
      where (cust_id == x.cust_id && order_id == x.order_id)
        [x];
    var the_order = hd(the_orders);
    update (var x <-- orderTable)
     where (x.order_id == order_id && cust_id == x.cust_id)
       set (cust_id = the_order.cust_id,
            order_id = the_order.order_id,
            date = the_order.date,
            instructions = instr,
            creditcard = card_no,
            expirydate = expiry);

    debug("successfully updated the order with purchase details.");
    purchase_confirmation(cust_id, order_id, total)
 } else
   deadend()
}

fun begin_checkout(cust_id, order_id) {
  <html>
    <h1>Finalize Your Order</h1>

      <div>Please enter your SurchargeCard details (Try: 8000000000001001 )
        and delivery instructions. Fields shown in red are mandatory.</div>
      <form method="POST" l:action="{checkout(cust_id, order_id, card_no, expiry, instr)}">
      <table>
        <tr><td align="right">
          SurchargeCard: 	
        </td><td>
          <input type="text" l:name="card_no" value="" />
        </td></tr>
        <tr><td align="right">
          Expiry Date (mm/yy): 	
        </td><td>
          <input type="text" l:name="expiry" value="" />
        </td></tr>
        <tr><td align="right">
          Delivery Instructions: 	
        </td><td>
          <input type="text" l:name="instr" value="" />
        </td></tr>
        <tr><td align="right">
        </td><td>
          <input type="submit" value="Purchase" />
        </td></tr>
      </table>
      </form>
  </html>
}

fun cart_itemlist(cust_id, order_id) {
#  wine_costs = for cost_rec <- Table "inventory" with
#                               {wine_id : Int, cost : Float} 
#                                order [wine_id : asc] from db 
#            in [(cost_rec.wine_id, cost_rec.cost)];

  debug("starting cart_itemlist");

  var cart_items = 
    for (var cart_item <-- cartItemsTable)
    where (cart_item.order_id == order_id &&
           cart_item.cust_id == cust_id)
    {
        for (var wine <-- wineTable)
        where (wine.wine_id == cart_item.wine_id)
        {
            for (var cost_rec <-- inventoryTable)
            where (cost_rec.wine_id == wine.wine_id)
              [(wine.wine_name, cost_rec.cost)]
        }
    };
    debug("got results in cart_items");
    if (length(cart_items) == 0)
      <p>Your cart is empty.</p>
    else {
      var total_cost = floatToString(sum_float(map(snd, cart_items)));
      var total_items = intToString(length(cart_items));
      <table width="100%">
           <tr>
             <th> Quantity </th>
             <th> Wine </th>
             <th align="right"> Unit Price </th>
             <th align="right"> Total </th>
           </tr>
        {for (var item <- cart_items)
           <tr>
             <td> 1 </td>
             <td> {stringToXml(item.1)} </td>
             <td align="right"> ${stringToXml(floatToString(item.2))} </td>
             <td align="right"> ${stringToXml(floatToString(item.2))} </td>
           </tr>
        }
        <tr>
          <td></td>
          <td></td>
          <td></td>
          <td align="right"> ${stringToXml(total_cost)} </td>
        </tr>
      </table>
      <form method="POST" l:action="{begin_checkout(cust_id, order_id)}">
        <input type="submit" value="Check out" />
      </form>
    }
}

fun wine_name(wine_id) {
  var matches = 
      for (var wine <-- wineTable)
      where (wine.wine_id == wine_id)
        [wine.wine_name];
    hd(matches)
}

fun get_region_name(region_id) {
  for (var region <-- regionTable)
  where (region_id == region.region_id)
    region.region_name
}

fun get_wine_price(wine_id) {
  hd(for (var cost_rec <-- inventoryTable)
     where (wine_id == cost_rec.wine_id) 
       [cost_rec.cost])
}

fun show_cart(cust_id, order_id, msg) {
  <html>
    <h1>Your Shopping Cart</h1>
    <div>
      {stringToXml(msg)}
    </div>
    <div>
      {cart_itemlist(cust_id, order_id)}
    </div>
    <a l:href="{search_results(cust_id, order_id, -1, -1)}">Continue shopping</a>
  </html>
}

sig cart_total : (Int, Int) -> String

fun cart_total(cust_id, order_id) {
  var cart_items = 
    for (var cart_item <-- cartItemsTable)
    where (cart_item.order_id == order_id &&
           cart_item.cust_id == cust_id)
    {
        for (var wine <-- wineTable)
        where (wine.wine_id == cart_item.wine_id)
          for (var cost_rec <-- inventoryTable)
          where (wine.wine_id == cost_rec.wine_id)
            [cost_rec.cost]
    };
  var total_cost = floatToString(sum_float(cart_items));
  total_cost
}

fun add_to_cart(cust_id, order_id, wine_id) {

  var itemsTable = table "items"
                   with (order_id : Int, cust_id : Int, item_id : Int,
                         wine_id : Int, qty : Int, price : Float)
                   from db;

    var price = get_wine_price(wine_id);
        var max_item_id = max(0, for (var cart_item <-- itemsTable)
                          where (cart_item.order_id == order_id
                                 && cart_item.cust_id == cust_id)
                              [cart_item.item_id]
                          );
        var new_item_id = max_item_id + 1;
        
#        insert into ("items", db) values
#          ( cust_id = cust_id, order_id = order_id, item_id = new_item_id, 
#            wine_id = wine_id, qty = 1, price = price );
      insert (itemsTable) values
          [( cust_id = cust_id, order_id = order_id, item_id = new_item_id, 
             wine_id = wine_id, qty = 1, price = price )];
    debug("finished insert, issuing confirmation page");
    show_cart(cust_id, order_id,
              "Added " ++ wine_name(wine_id) ++ " to your cart.")
}

# create_cart: make a new cart
# quite a hack; doesn't handle multiple carts for one session
fun create_cart(cust_id) {

#    max_cust_id = max(for cust <- (Table "customers" with
#                                        {cust_id : Int} 
#                                        order [cust_id : asc] from db) in
#                            [cust.cust_id]);

  var ordersTable = table "orders"
                     with (cust_id : Int, order_id : Int)
                     from db;
  var os = for (var cart <-- ordersTable)
                where (cart.cust_id == cust_id)
                [cart.order_id];
  var max_order_id = max(0, os);
  var order_id = max_order_id + 1;
  insert (ordersTable) values 
      [(cust_id = cust_id,
        order_id = order_id)];
#  set_cookie("foo")("bar");
  (order_id, cust_id)
}

fun wine_listing(cust_id, order_id, region_id, wine_type) {

  var negone = -1;
  var result = 
    for (var wine <-- wineTable )
    where (wine_type == negone || wine_type == 1
           || wine.wine_type == wine_type)
    {
      for (var winery <-- wineryTable)
         where (winery.winery_id == wine.winery_id
                && (region_id == 1 || region_id == negone
                    || winery.region_id == region_id))
         { 
             for (var cost_rec <-- inventoryTable)
             where (wine.wine_id == cost_rec.wine_id)
               [(wine.wine_id, wine.wine_name, cost_rec.cost,
                 wine.year, winery.winery_name)]
         }
    };

  var (order_id, cust_id) = 
    if (order_id <> -1) (order_id, cust_id)
    else create_cart(cust_id);

  for (var (id, name, cost, year, winery) <- result)
  {
      <li>{stringToXml(intToString(year))} {stringToXml(winery)} {stringToXml(name)} <br/>
          <b>Our price</b>: ${floatToXml(cost)} (${floatToXml(cost *. 12.)} a dozen)
          <a l:href="{add_to_cart(cust_id, order_id, id)}">Add to cart</a>
      </li>
  }
}

fun deadend() {
   <html>deadend</html>
}

sig search_results : (Int, Int, Int, Int) -> XML

fun search_results(cust_id, order_id, region_id, wine_type) {
  <html>
    {header(cust_id)}
    <div>
        <img src="cart_off.jpg"  align="middle" />
      Total in cart: {stringToXml(cart_total(cust_id, order_id))} 
        ({intToXml(length(for (var x <-- cartItemsTable) where (x.cust_id == cust_id && x.order_id == order_id) [x]))} items)
      View <a l:href="{show_cart(cust_id, order_id, "")}">cart</a>.
    </div>
    <form method="POST" l:action="{search_results(cust_id, order_id,
                                                  stringToInt(search_region_id),
                                                  stringToInt(search_wine_type))}">
      <h2>Filter wines by </h2>
      <table>
        <tr><td>Region:</td>
          <td><select l:name="search_region_id">
                {for (var region <-- regionTable)
                   # (boutrosed)
                   if (region_id == region.region_id)
                     <option selected="SELECTED" 
                        value="{intToString(region.region_id)}">{stringToXml(region.region_name)}</option>
                   else
                     <option 
                        value="{intToString(region.region_id)}">{stringToXml(region.region_name)}</option>
                }
              </select>
          </td></tr>
        <tr><td>Wine type:</td>
           <td><select l:name="search_wine_type">
                {for (var type <-- wineTypeTable)
                   # (boutrosed)
                   if (wine_type == type.wine_type_id)
                     <option selected="SELECTED" 
                        value="{intToString(type.wine_type_id)}">{stringToXml(type.wine_type)}</option>
                   else
                     <option 
                        value="{intToString(type.wine_type_id)}">{stringToXml(type.wine_type)}</option>
                }
               </select>
          </td></tr>
       </table>
       <input type="submit" value="Show wines" />
    </form>
    <h1>Wines for region {stringToXml(get_region_name(region_id))},
        {wineTypeName(wine_type)}</h1>
    <ul>
      {wine_listing(cust_id, order_id, region_id, wine_type)}
    </ul>
    {footer(cust_id)}
  </html>
}

sig main : () -> XML

fun main() {
  search_results(-1, -1, -1, -1)
}

main()

#!/home/s0567141/links/links -d -O -w

fun snd(pair) {
  (a, b) = pair;
  b
}

fun map(f, l) {
    if (l == []) []
    else [f(hd(l))] ++ map(f, tl(l));
}

#fun sum_int(l) {
#    if (l == [])
#        0
#    else
#        hd(l) + sum_int(tl(l));
#}

fun count(l) {
  if (l == [])
    0
  else (1 + count(tl(l)))
}

fun sum_float(l) {
    if (l == [])
        0.
    else
        hd(l) +. sum_float(tl(l));
}

fun max(result, l) {
  if (l == [])
    result
  else
    if (result >= hd(l))  
      max(result, tl(l))
    else
      max(hd(l), tl(l));
}

fun assoc(x, l, d) {
    if (l == []) { d }
    else {
        (k, v) = hd(l);
        if (x == k)
            v
        else assoc(x, tl(l), d)
    }
}

fun cust_id_for_order(db, order_id) {
  hd(for row <- (Table "items" with
                   {order_id : Int, cust_id : Int}
                 order [order_id : asc] from db) 
     where (row.order_id == order_id)
     in [row.cust_id]
    );
}

fun cust_id_next(db) {  # nasty
  ids = for u <- (Table "users" with { cust_id : Int }
                         order [ cust_id : desc] from db)
                            in [u.cust_id];
  max(0, ids) + 1;
}
        
fun sign_up(username, password) {
  db = database ("postgresql:winestore:localhost:5432:s0567141:");

  new_cust_id = cust_id_next(db);

  insert into ("users", db) values
    ( cust_id = new_cust_id, user_name = username, password = password );
  <div>
    Welcome, {enxml(username)}, your cust_id is {string_of_int(new_cust_id)}
    <a l:href="{main(new_cust_id, -1, -1, -1)}">Continue shopping</a>
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
  db = database ("postgresql:winestore:localhost:5432:s0567141:");

  cust_id = 
    for u <- (Table "users" with { user_name : String, password : String,
                                   cust_id : Int }
                           order [ cust_id : desc] from db)
                           where (u.user_name == username &&
                                  u.password == password)
                              in [u.cust_id];    

  if (cust_id == [])
    enxml("Incorrect username/password combination")
  else
    main(hd(cust_id), -1, -1, -1);
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
   main(-1, -1, -1, -1);
}

fun header(cust_id) {
  if (cust_id == -1)
    <div align="right">
      <a l:href="{sign_in_form()}">Sign in</a> or
      <a l:href="{sign_up_form()}">create an account</a>.
    </div>
  else
    <div align="right">Welcome, customer {string_of_int(cust_id)}.
         <a l:href="{logout()}">Logout</a></div>
}

fun footer(cust_id) {
  <div />
}

fun valid_cc_details(card_no, expiry) {
  card_no == "8000000000001001"
}

fun purchase_confirmation(cust_id, order_id, total) {
  alert("arrived at purchase confirmation with " ++ string_of_int(cust_id)
        ++ " " ++ string_of_int(order_id) ++ " " ++ string_of_float(total));
  <html>
    Your order (reference # {string_of_int(cust_id)} - {string_of_int(order_id)}) has been dispatched
    Thank you Mr Claus, your order has been completed and dispatched. 
    Your order reference number is {string_of_int(cust_id)} - {string_of_int(order_id)}.
    Please quote this number in any correspondence.

    If it existed, the order would have been shipped to:
    DETAILS

    We have billed your fictional credit card.
    <table>
    Quantity 	Wine 	Unit Price 	Total
    [LOOP OVER CART ITEMS]
    Total of this order 		{string_of_float(total)}
    </table>

    An email confirmation has NOT been sent to you. Thank you for shopping 
    with Linkswine.
    Return to <a l:href="{main(cust_id, -1, -1, -1)}">main page</a>.
  </html>
}

fun order_total(db, cust_id, order_id) {
  sum_float(
    for item <- Table "items"
                 with {cust_id : Int, order_id : Int, price : Float}
                order [cust_id : asc]
                 from db
    where (item.cust_id == cust_id && item.order_id == order_id)
    in {
      [item.price]
    }
  );
}

fun checkout(cust_id, order_id, card_no, expiry, instr) {
  db = database ("postgresql:winestore:localhost:5432:s0567141:xxx");
  total = order_total(db, cust_id, order_id);
  if (valid_cc_details(card_no, expiry)) {
    the_orders = 
    for x <- Table "orders"
              with {cust_id : Int, order_id : Int, date : String,
                    instructions : String, creditcard : String,
                    expirydate : String}
             order [cust_id : asc]
              from db
    where (cust_id == x.cust_id && order_id == x.order_id)
    in [x];
    the_order = hd(the_orders);
    update ("orders", db) by [(the_order,
                               (cust_id = the_order.cust_id,
                                order_id = the_order.order_id,
                                date = the_order.date,
                                instructions = instr,
                                creditcard = card_no,
                                expirydate = expiry))];
    alert("successfully updated the order with purchase details.");
    purchase_confirmation(cust_id, order_id, total)
 } else
   deadend();
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

fun cart_itemlist(db, cust_id, order_id) {
#  wine_costs = for cost_rec <- Table "inventory" with
#                               {wine_id : Int, cost : Float} 
#                                order [wine_id : asc] from db 
#            in [(cost_rec.wine_id, cost_rec.cost)];

  cart_items = 
    for cart_item <- (Table "items" with
                         {order_id : Int, cust_id : Int,
                          item_id : Int, wine_id : Int}
                      order [order_id : asc] from db) 
    where (cart_item.order_id == order_id &&
           cart_item.cust_id == cust_id)
    in {
        for W <- Table "wine" with
                   {wine_id : Int, wine_name : String, wine_type : Int,
                    year : Int, winery_id : Int}
                    order [wine_id : asc]
                    from db
        where (W.wine_id == cart_item.wine_id)
        in {
            for cost_rec <- Table "inventory"
                             with {wine_id : Int, cost : Float} 
                            order [wine_id : asc] from db 
            where (cost_rec.wine_id == W.wine_id)
            in {
                   [(W.wine_name, cost_rec.cost)]
            }
#            [(W.wine_name, (assoc(W.wine_id, wine_costs, 99.44)))]
        }
    };
    total_cost = string_of_float(sum_float(map(snd, cart_items)));
#    total_items = string_of_float(count(cart_items));
    <table width="100%">
         <tr>
           <th> Quantity </th>
           <th> Wine </th>
           <th align="right"> Unit Price </th>
           <th align="right"> Total </th>
         </tr>
      {for W <- cart_items in
         <tr>
           <td> 1 </td>
           <td> {W.1} </td>
           <td align="right"> ${enxml(string_of_float(W.2))} </td>
           <td align="right"> ${enxml(string_of_float(W.2))} </td>
         </tr>
      }
      <tr>
        <td></td>
        <td></td>
        <td></td>
        <td align="right"> ${total_cost} </td>
      </tr>
    </table>
    <form method="POST" l:action="{begin_checkout(cust_id, order_id)}">
      <input type="submit" value="Check out" />
    </form>
}

fun wine_name(db, wine_id) {
    matches = 
      for wine <- (Table "wine" 
                    with {wine_id : Int, wine_name : String} 
                   order [wine_id : asc]
                    from db) 
        where (wine.wine_id == wine_id)
        in
            [wine.wine_name];
    hd(matches);
}

fun get_region_name(db, region_id) {
  for region <- (Table "region"
                  with { region_id : Int, region_name : String }
                 order [ region_id : asc ] from db)
  where (region_id == region.region_id)
  in
    region.region_name
}

fun get_wine_price(db, wine_id) {
    hd(for cost_rec <- Table "inventory" with
                      {wine_id : Int, cost : Float}
                      order [wine_id : asc]
                      from db
       where (wine_id == cost_rec.wine_id) 
       in [cost_rec.cost])
}

fun show_cart(db, cust_id, order_id, msg) {
  <html>
    <h1>Your Shopping Cart</h1>
    <div>
      {enxml(msg)}
    </div>
    <div>
      {cart_itemlist(db, cust_id, order_id)}
    </div>
    <a l:href="{main(cust_id, order_id, -1, -1)}">Continue shopping</a>
  </html>
}

fun cart_total(db, cust_id, order_id) {
  cart_items = 
    for cart_item <- (Table "items" with
                         {order_id : Int, cust_id : Int,
                          item_id : Int, wine_id : Int}
                      order [order_id : asc] from db) 
    where (cart_item.order_id == order_id &&
           cart_item.cust_id == cust_id)
    in {
        for W <- Table "wine" with
                   {wine_id : Int, wine_name : String, wine_type : Int,
                    year : Int, winery_id : Int}
                    order [wine_id : asc]
                    from db
        where (W.wine_id == cart_item.wine_id)
        in
          for cost_rec <- Table "inventory"
                           with {wine_id : Int, cost : Float} 
                          order [wine_id : asc] from db 
          where (W.wine_id == cost_rec.wine_id)
          in
            [cost_rec.cost]
    };
  total_cost = string_of_float(sum_float(cart_items));
  total_cost
}

fun add_to_cart(cust_id, order_id, wine_id) {
    db = database ("postgresql:winestore:localhost:5432:s0567141:xxx");

    price = get_wine_price(db, wine_id);
#    existing_item =
#      for cart_item <- (Table "items" with
#                        {order_id : Int,
#                         cust_id : Int,
#                         wine_id : Int,
#                         item_id : Int}
#                        order [order_id : asc] from db)
#      in
#          if (cart_item.wine_id == wine_id)
#              [cart_item.item_id]
#          else [];
#    existing_item = 
#    hd(for x <- Table "items" with
#                    {order_id : Int, cust_id : Int,
#                     wine_id : Int, item_id : Int}
#                    order [order_id : asc] from db
#       in 
#         if (x.order_id == order_id
#          && x.cust_id == cust_id
#          && x.wine_id == wine_id)
#             [x] else []);
#    if (existing_item) {
#        update("item", db) by [((order_id = order_id, cust_id = cust_id,
#                                 item_id = existing_item.item_id),
#                                (qty = qty + 1))];
#    } else {
        max_item_id = max(0, for cart_item <- (Table "items" with
                                            {order_id : Int,
                                              cust_id : Int,
                                             item_id : Int}
                                       order [order_id : asc] from db)
                          where (cart_item.order_id == order_id
                                 && cart_item.cust_id == cust_id)
                          in
                              [cart_item.item_id]
                          );
        new_item_id = max_item_id + 1;
        
        insert into ("items", db) values
          ( cust_id = cust_id, order_id = order_id, item_id = new_item_id, 
            wine_id = wine_id, qty = 1, price = price );
#    }
    w=alert("finished insert, issuing confirmation page");
    show_cart(db, cust_id, order_id,
              "Added " ++ wine_name(db, wine_id) ++ " to your cart.");
}

# create_cart: make a new cart
# quite a hack; doesn't handle multiple carts for one session
fun create_cart(db, cust_id) {
#    max_cust_id = max(for cust <- (Table "customers" with
#                                        {cust_id : Int} 
#                                        order [cust_id : asc] from db) in
#                            [cust.cust_id]);

  os = for cart <- (Table "orders"
                     with {cust_id : Int, order_id : Int} 
                    order [cust_id : asc] from db) 
                    where (cust_id == cart.cust_id)
       in [cart.order_id];
  max_order_id = max(0, os);
  order_id = max_order_id + 1;
  insert into ("orders", db) values 
      (cust_id = cust_id,
       order_id = order_id);
#  set_cookie("foo")("bar");
  (order_id, cust_id);
}

fun wine_list(cust_id, region_id, wine_type) {
  db = database ("postgresql:winestore:localhost:5432:s0567141:xxx");

  negone = -1;
  result = 
    for W <- Table "wine" with
             {wine_id : Int, wine_name : String, wine_type : Int,
              year : Int, winery_id : Int}
             order [wine_id : asc]
             from db 
    where (wine_type == negone || wine_type == 1 || W.wine_type == wine_type)
    in {
      for winery <- Table "winery" with 
                       {winery_id : Int, winery_name : String,
                        region_id : Int}
                order [winery_id : asc] from db
         where (winery.winery_id == W.winery_id
                && (region_id == 1 || region_id == negone
                    || winery.region_id == region_id))
         in { 
             for cost_rec <- Table "inventory" with
                            {wine_id : Int, cost : Float}
                            order [wine_id : asc]
                            from db
             where (W.wine_id == cost_rec.wine_id)
             in 
                 [(W.wine_id, W.wine_name, cost_rec.cost,
                   W.year, winery.winery_name)]
      };
    };
  (order_id, cust_id) = create_cart(db, cust_id);
  for (id, name, cost, year, winery) <- result in
  {
      <li>{string_of_int(year)} {winery} {name} <br/>
          Our price: ${string_of_float(cost)} (${string_of_float(cost *. 12.)} a dozen)
          <a l:href="{add_to_cart(cust_id, order_id, id)}">Add to cart</a>
      </li>
  }
}

fun deadend() {
   <html>deadend</html>
}

fun main(cust_id, order_id, region_id, wine_type) {
  db = database ("postgresql:winestore:localhost:5432:s0567141:xxx");
  <html>
    {header(cust_id)}
    <div>
      Total in cart: {cart_total(db, cust_id, order_id)} (n items)
      View <a l:href="{show_cart(db, cust_id, order_id, "")}">cart</a>.
    </div>
    <form method="POST" l:action="{main(cust_id, order_id,
                                        int_of_string(search_region_id),
                                        int_of_string(search_wine_type))}">
      <h2>Filter wines by </h2>
      <table>
        <tr><td>Region:</td>
          <td><select l:name="search_region_id">
                {for region <- (Table "region"
                               with { region_id : Int, region_name : String }
                              order [ region_id : asc ] from db)
                                 in
                   # (boutrosed)
                   if (region_id == region.region_id)
                     <option selected="SELECTED" value="{string_of_int(region.region_id)}">{region.region_name}</option>
                   else
                     <option value="{string_of_int(region.region_id)}">{region.region_name}</option>
                }
              </select>
          </td></tr>
        <tr><td>Wine type:</td>
           <td><select l:name="search_wine_type">
                {for type <- (Table "wine_type"
                               with { wine_type_id : Int, wine_type : String }
                              order [ wine_type_id : asc ] from db)
                                 in
                   # (boutrosed)
                   if (wine_type == type.wine_type_id)
                     <option selected="SELECTED" value="{string_of_int(type.wine_type_id)}">{type.wine_type}</option>
                   else
                     <option value="{string_of_int(type.wine_type_id)}">{type.wine_type}</option>
                }
               </select>
          </td></tr>
       </table>
       <input type="submit" value="Show wines" />
    </form>
    Wines for {get_region_name(db, region_id)} {string_of_int(wine_type)}:
    <ul>
      {wine_list(cust_id, region_id, wine_type)}
    </ul>
    {footer(cust_id)}
  </html>
}

main(-1, -1, -1, -1)

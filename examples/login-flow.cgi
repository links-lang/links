
var noMsg = "";

fun login_widget(msg, return) {
    <html>
      <body>
      <div class="error">{stringToXml(msg)}</div>
      <form l:handler="{return}" method="post">
        <table>
          <tr>
            <td>Username:</td>
            <td> <input name="username" value="" /></td>
          </tr>
          <tr>
            <td>Password:</td>
            <td><input type="password" name="userpass" value="" /></td>
          </tr>
        </table>
        <input type="submit" />
      </form>
      <a l:href="{main()}">start again</a>
      </body>
    </html>
}

fun validAuth(name, pass) {
  name == "ezra" && pass == "knock"
}

fun get_user(msg) {
  var current_user = getCookie("loginname");
  if (current_user <> "")     # User is logged in! Return creds.
    current_user
  else {                      # User is not logged in, show login page.
    var (username=name, userpass=pass) = 
        sendSuspend(fun (r){login_widget(msg, r)});
    if (validAuth(name, pass)) {
      # User logged in successfully, set cookie and return creds.
      setCookie("loginname")(name);
      name
    } else
      # User failed to log in, show page again.
      get_user("The password you entered was incorrect")
  }
}

fun logout() {
  setCookie("loginname")("");
}

fun logoutLink(target) {
  <a l:href="{logout(); freshResource(); target()}">Logout</a>
}

fun main() {
 var user = get_user(noMsg);
 <html>
   <body>
     <div>Thanks for logging in, {stringToXml(user)}.</div>
     <div>{logoutLink(main)}</div>
   </body>
 </html>
}

main()

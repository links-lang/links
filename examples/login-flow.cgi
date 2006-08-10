fun login_widget(return, msg) {
    <html>
      {stringToXml(msg)}
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
      <a href="test4.cgi">start again</a>
    </html>
}

fun get_user(msg) {
  var current_user = getCookie("loginname");
  if (current_user <> "")     # User is logged in! Return creds.
    current_user
  else {                      # User is not logged in, show login page.
    var (username=name, userpass=pass) = 
      escape return in {
        toplevel(login_widget(msg, return))
      };
    if (name == "ezra" && pass == "knock") {
      # User logged in successfully, set cookie and return creds.
      setCookie("loginname")(name);
      name
    } else
      # User failed to log in, show page again.
      get_user("The password you entered was incorrect")
  }
}

var noMsg = "";

{
 var user = get_user(noMsg);
 <html>Thanks for logging in, {stringToXml(user)}</html>
}

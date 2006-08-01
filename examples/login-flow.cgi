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
  if (current_user <> "")
    current_user
  else {
    var (username=name, userpass=pass) = 
      escape return in {
        toplevel(login_widget(return, msg))
      };
    if (name == "ezra" && pass == "knock") {
      setCookie("loginname")(name);
      name
    } else
      get_user("The password you entered was incorrect")
  }
}

var noMsg = "";

{
 var user = get_user(noMsg);
 <html>Thanks for logging in, {stringToXml(user)}</html>
}

#!/home/s0567141/links/links -d -w

fun login_widget(gotvals, msg) {
    <html>
      {enxml(msg)}
      <form action="#" method="post">
        <input type="hidden" name="continuation%" 
               value="{string_of_cont(gotvals)}" />
        <table>
          <a href="bad bad
leroy brown" />
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

fun get_user(draw_page, msg) {
      (username=name, userpass=pass) = 
          (escape gotvals in { draw_page(login_widget(gotvals, msg)) });
      if (name == "ezra" && pass == "knock")
        name
      else
        get_user(draw_page, "The password you entered was incorrect");
}

escape draw_page in {
  user = get_user(draw_page, "");
  <html>Thanks, {enxml(user)}</html>
}

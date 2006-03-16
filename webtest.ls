#!/home/s0567141/links/links -w 

fun remove(elem, lyst) {
    if (lyst == []) [] else {
      if (elem == hd(lyst)) 
        remove(elem, tl(lyst))
      else {
         [elem] ++ (remove(elem, tl(lyst)))
      }
    }
}

fun todo(items) {
  <html>
   <body>
    <h1>Items to do</h1>
    <form l:action="{todo( [new] ++ items)}" method="POST">
      <input l:name="new" type="text" size="40"/>
      <input type="submit" value="add new item"/>
    </form>
    <table>{
      for item <- items in
        <tr><td>{[item]}</td>
           <td><form l:action="{todo(remove(item, items))}" method="POST">
                  <input type="submit" value="done"/>
               </form>
           </td>
        </tr> 
    }</table>
   </body>
  </html>
}

todo([])

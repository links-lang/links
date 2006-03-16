#!/home/s0567141/links/links -w 

fun page(str) {
  escape return in
  {
      response =
        escape handler in
      	{
            return(
                <html>
                  <head>
                    <title>{str}</title>
                  </head>
                  <body>
                    {str}
                    {handler}
                  </body>
                </html>
            )
        };
      <html><head></head><i>response</i></html>;
  }
};

page("Abraham Lincoln slept here")



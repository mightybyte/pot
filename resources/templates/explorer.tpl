<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
  </head>
  <body>
    <div id="content">
      <h1>Site Explorer</h1>
      <p>
        This is a simple demo page served using
        <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
        and the <a href="http://snapframework.com/">Snap</a> web framework.
      </p>
      <p>
        Right now the "Tags" section contains all source files, not just tags.
        Not sure how this will end up working out.
      </p>
      <div id="template_explorer">
        <h3>Templates</h3>
        <template_tree/>
        <h3>Tags</h3>
        <tag_tree/>
      </div>
      <content/>
    </div>
  </body>
</html>

// scrape_techstars.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'injury/injury.html'

page.open('http://www.nfl.com/injuries?week=1', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});
var page = require('webpage').create();
page.open('http://www.nfl.com/injuries?week=1', function () {
                   page.evaluate(function(){

                   });
                   fs.write('1.html', page.content, 'w');

                   phantom.exit();
                   });

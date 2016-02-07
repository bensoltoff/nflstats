var url = 'http://www.nfl.com/injuries?week=1';
var page = require('webpage').create();
var fs = require('fs');

page.open(url, function (status) {
    if (status !== 'success') {
        console.log('Fail');
        phantom.exit();
    } else {        
        window.setTimeout(function () {
        fs.write('1.html', page.content, 'w');
        phantom.exit();
        }, 2000); // Change timeout as required to allow sufficient time 
    }
});
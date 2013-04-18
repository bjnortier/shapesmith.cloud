// The main server module.
// The logic ins split into a separate 'server'
// module so that the server can be started independantly for
// functional testing

var server = require('./server'),
    nconf = require('nconf');
   
var port = nconf.get('port');
app.listen(port);
console.info('--------------');
console.info('server started on :' + port + '\n');

var express = require('express');
    fs = require('fs'),
    path = require('path'),
    nconf = require('nconf'),
    app = express(),
    ueberDB = require("ueberDB");

var requirejs = require('requirejs');
var rootDir = path.normalize(path.join(__dirname, '../..'));
var baseUrl = path.join(__dirname, "..");
requirejs.config({
    baseUrl: baseUrl,
    nodeRequire: require,
});

// ---------- Configuration ----------

// Override with command-line arguments
nconf.argv();
nconf.env();

var app_env = nconf.get('app_env') || 'devel';
switch (app_env) {
    case 'functional':
        nconf.file({file: path.join(rootDir, 'config/functional.config.json')});
        break;
    case 'devel':
        nconf.file({file: path.join(rootDir, 'config/devel.config.json')});
        break;
    default:
        throw Error('invalid environment:' + env)
}

var dbType = nconf.get('dbType') || 'sqlite';
var dbArgs = nconf.get('dbArgs') || {};
var useLogger = nconf.get('useLogger') || false;

console.info('\n\nconfiguration:');
console.info('--------------');
console.info('environment: ', app_env);
console.info('port:        ', nconf.get('port'));
console.info('dbtype:      ', dbType);
console.info('dbargs:      ', dbArgs);
console.info('use logger:  ', useLogger);


// ---------- Create db ----------

app.use(express.bodyParser({strict: false}));
app.use(express.cookieParser());
app.use(express.session({secret: 'f8a5125a15a5241cb814649c4d7c16af', key: 'sid', cookie: { maxAge: 60000 }}));
useLogger && app.use(express.logger());

app.set('view engine', 'hbs');
app.set('views', path.join(rootDir, 'templates'));

app.use('/images', express.static(path.join(rootDir, 'static', 'images')));
app.use('/css', express.static(path.join(rootDir, 'static', 'css')));
app.use('/ui/', express.static(path.join(rootDir, 'src')));
app.use('/ui/node_modules', express.static(path.join(rootDir, 'node_modules')));
app.use('/node_modules', express.static(path.join(rootDir, 'node_modules')));
app.use('/lib', express.static(path.join(rootDir, 'src/lib')));

app.set('auth_engine', function(username, req) {
  return (req.session.username === username);
})

app.use(function(err, req, res, next){
  console.error(err.stack);
  res.send(500, 'Oops. An error occurred.');
});

app.get('/', function(req, res) {
  res.redirect('/ui/local/designs');
});

var db = new ueberDB.database(dbType, dbArgs);
db.init(function(err) {
  if (err) {
    console.error(err);
    process.exit(1);
  }

  new requirejs('api/userapi')(app, db);
  new requirejs('api/designapi')(app, db);
  new requirejs('api/objectapi')(app, db);
});
    


// Signup 
app.get(/^\/ui\/signup\/?$/, function(req, res) {
  res.render('signup');
});

// Designs 
app.get(/^\/ui\/([\w%]+)\/designs\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  res.render('designs', {user: user});
});

// Modeller 
app.get(/^\/ui\/([\w%]+)\/([\w%]+)\/modeller$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  res.render('modeller', {user: user, design: design});
});


// For controlling the process (e.g. via Erlang) - stop the server
// when stdin is closed
process.stdin.resume();
process.stdin.on('end', function() {
  process.exit();
});

module.exports = app;
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

var diskDBPath = path.normalize(path.join(rootDir, nconf.get('diskDBPath')));

console.info('\n\nconfiguration:');
console.info('--------------');
console.info('environment: ', app_env);
console.info('port:        ', nconf.get('port'));
console.info('baseUrl:     ', baseUrl);
console.info('disk db path:', diskDBPath);

// ---------- Create db ----------
var DB = requirejs('api/disk_db');
var db = new DB({root: diskDBPath});

app.use(express.bodyParser({strict: false}));
app.use(express.cookieParser());
app.use(express.session({secret: 'f8a5125a15a5241cb814649c4d7c16af', key: 'sid', cookie: { maxAge: 60000 }}));

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

// app.use(express.logger());

app.use(function(err, req, res, next){
  console.error(err.stack);
  res.send(500, 'Oops. An error occurred.');
});

app.get('/', function(req, res) {
  res.redirect('/_ui/local/designs');
});

var db = new ueberDB.database("sqlite");
db.init(function(err) {
  if (err) {
    console.error(err);
    process.exit(1);
  }

  new requirejs('api/userapi')(app, db);
  new requirejs('api/designapi')(app, db);
});


// Designs UI
app.get(/^\/_ui\/([\w%]+)\/designs\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  res.render('designs', {user: user});
});

// Rename design.
// NB! This is not safe if multiple requests change
// the list of designs at the same time!
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  if (!req.body.newName) {
    res.json(400, 'no newName parameter');
  } else if (!/^[a-zA-Z_][a-zA-Z0-9-_\\s]*$/.test(req.body.newName)) {
    res.json(400, 'invalid new name');
  } else {
    var newName = req.body.newName;
    db.renameDesign(user, design, newName, function(err, data) {
      if (err) {
        if (err === 'alreadyExists') {
          res.send(409, 'already exists');
        } else {
          res.send(500, err);
        }
      } else {
        res.json(data);
      }
    });
  }
});

// Delete design
app.delete(/^\/_api\/([\w%]+)\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  db.deleteDesign(user, design, function(err, data) {
    if (err) {
      if (err === 'notFound') {
        res.send(404, 'not found');
      } else {
        res.send(500, err);
      }
    } else {
      res.json(data);
    }
  });
});


// Modeller UI
app.get(/^\/_ui\/([\w%]+)\/([\w%]+)\/modeller$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  res.render('modeller', {user: user, design: design});
});


// Create graph
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/graph\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var graph = req.body;
  db.createGraph(user, design, graph, function(err, sha) {
    if (err) {
      res.send(500, err);
    } else {
      res.json(sha);
    }
  });
});

// Get graph
app.get(/^\/_api\/([\w%]+)\/([\w%]+)\/graph\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var sha = req.params[2];
  db.getGraph(user, design, sha, function(err, data) {
    if (err) {
      res.send(500, err);
    } else {
      return res.json(data);
    }
  });
});

// Create vertex
app.post(/^\/_api\/([\w%]+)\/([\w%]+)\/vertex\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var vertex = req.body;
  db.createVertex(user, design, vertex, function(err, sha) {
    if (err) {
      res.send(500, err);
    } else {
      res.json(sha);
    }
  });
});

// Get vertex
app.get(/^\/_api\/([\w%]+)\/([\w%]+)\/vertex\/([\w%]+)\/?$/, function(req, res) {
  var user = decodeURI(req.params[0]);
  var design = decodeURI(req.params[1]);
  var sha = req.params[2];
  db.getVertex(user, design, sha, function(err, data) {
    if (err) {
      if (err === 'notFound') {
        res.send(404, 'not found');
      } else {
        res.send(500, err);
      }
    } else {
      return res.json(data);
    }
  });
});

// For controlling the process (e.g. via Erlang) - stop the server
// when stdin is closed
process.stdin.resume();
process.stdin.on('end', function() {
  process.exit();
});

module.exports = app;
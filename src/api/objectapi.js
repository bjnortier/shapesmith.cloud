define([
    './objects'
  ],
  function(Objects) {

  var ObjectAPI = function(app, db) {

    // Create graph
    app.post(/^\/api\/([\w%]+)\/(graph|vertex)\/?$/, function(req, res) {
      var username = decodeURI(req.params[0]);
      var graph = req.body;
      Objects.create(db, username, graph, function(err, sha) {
        if (err) {
          res.json(500, err);
        } else {
          res.json(201, sha);
        }
      });
    });

    // Get graph
    app.get(/^\/api\/([\w%]+)\/(graph|vertex)\/([\w%]+)\/?$/, function(req, res) {
      var username = decodeURI(req.params[0]);
      var sha = req.params[2];
      Objects.get(db, username, sha, function(err, object) {
        if (err) {
          res.send(500, err);
        } else if (object === null) {
          res.json(404, 'not found');
        } else {
          return res.json(object);
        }
      
      });
    });

  }

  return ObjectAPI;

});
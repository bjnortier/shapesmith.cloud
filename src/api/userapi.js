define(
  [
  ], 
  function() {

  var UserAPI = function(app, db) {

    // Create a new user
    app.post(/^\/user\/?$/, function(req, res) {
      var username = req.body.username && req.body.username.trim();
      if (username === undefined) {
        res.json(404, {errors: [{missing: 'username'}]});
        return
      }

      if (!/[a-zA-Z][a-zA-Z0-9_]*/.exec(username)) {
        res.json(404, {errors: [{invalid: 'username'}]});
        return
      }

      var key = 'user/' + req.body.username;
      db.get(key, function(err, value) {
        if (err) {
          res.send(500);
        } else if (value !== null) {
          res.json(409, 'user already exists');
        } else {
          db.set(key, {}, function(err, value) {
            if (err) {
              res.send(500);
            } else {
              res.json(201, 'created');
            }
          })
        }
      });

    });

    app.get(/^\/user\/([\w%]+)\/?$/, function(req, res) {
      var user = decodeURI(req.params[0])

      db.get('user/' + user, function(err, value) {
        if (err) {
          res.send(500);
        } else if (value === null) {
          res.json(404, 'not found');
        } else {
          res.json(value);
        }
      });
    });


  }

  return UserAPI;

});
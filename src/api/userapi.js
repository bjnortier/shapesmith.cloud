define(
  [
  ], 
  function() {

  var UserAPI = function(app, db) {

    // Create a new user
    app.post(/^\/user\/?$/, function(req, res) {

      var key = 'user/' + req.body.username;
      db.get(key, function(err, value) {
        if (err) {
          res.send(500);
        } else if (value !== null) {
          res.send(409, 'user already exists');
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
      var user = decodeURI(req.params[0]);
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

    // db.set("valueA", {a:1,b:2});

    //get the object
    

  }

  return UserAPI;

});
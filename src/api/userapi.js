define(
  [
    './users'
  ], 
  function(Users) {

  var UserAPI = function(app, db) {

    // Create a new user
    app.post(/^\/user\/?$/, function(req, res) {
      var username = req.body.username;
      var password = req.body.password;

      var errors = [];
      if (username === undefined) {
        errors.push({'username':'please provide a valid email address'});
      }
      if (password === undefined) {
        errors.push({'password':'please provide a password with a minimum of 6 characters'});
      }

      if (!Users.validateUsername(username)) {
        errors.push({'username':'please provide a valid email address'});
      }

      if (!Users.validatePassword(password)) {
        errors.push({'password':'must be at least 6 characters'});
      }

      if (errors.length) {
        res.json(400, {errors: errors});
        return;
      }

      Users.get(db, username, function(err, userData) {
        if (err) {
          res.json(500, err);
        } else if (userData !== null) {
          res.json(409, 'user already exists');
        } else {
          Users.create(db, username, password, function(err) {
            if (err) {
              res.send(500, err);
            } else {
              req.session.username = username;
              res.json(201, 'created');
            }
          })
        }
      });

    });

    app.get(/^\/user\/([\w%]+)\/?$/, function(req, res) {
      
      var username = decodeURI(req.params[0])
      if (!app.get('auth_engine')(username, req)) {
        res.json(401, 'Unauthorized');
        return
      }

      Users.get(db, username, function(err, userData) {
        if (err) {
          res.json(500);
        } else if (userData === null) {
          res.json(404, 'not found');
        } else {
          res.json(userData);
        }
      });

    });

  }

  return UserAPI;

});
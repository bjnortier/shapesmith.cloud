define([
  ],
  function() {

  var validate = function(username) {
    return !!/[a-zA-Z][a-zA-Z0-9_]*/.exec(username);
  }

  var get = function(db, username, callback) {

    var key = keyFromUsername(username);
    db.get(key, function(err, value) {
      if (err) {
        callback(err);
      } else {
        callback(undefined, value);
      }
    });

  }

  var create = function(db, username, callback) {

    var key = keyFromUsername(username);
    db.set(key, {}, function(err, value) {
      if (err) {
        callback(err);
      } else {
        callback();
      }
    })
    
  }

  var keyFromUsername = function(username) {
    return 'user/' + username;
  }

  return {
    validate: validate,
    get     : get,
    create  : create,
  }

})
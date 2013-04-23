define([
    'bcrypt'
  ],
  function(bcrypt) {

  var validateUsername = function(username) {
    return !!/[a-zA-Z][a-zA-Z0-9_]*/.exec(username);
  }

  var validatePassword = function(password) {
    return !!/^.{6,}$/.exec(password);
  }

  var get = function(db, username, callback) {
    var key = keyFromUsername(username);
    db.get(key, callback);
  }

  var create = function(db, username, password, callback) {
    var key = keyFromUsername(username);

    var userData = {
        username: username,
        password_bcrypt: bcrypt.hashSync(password, 10),
    }
    db.set(key, userData, function(err, value) {
      if (err) {
        callback(err);
      } else {
        callback();
      }
    })

  }

  var keyFromUsername = function(username) {
    return '_user/' + username;
  }

  return {
    validateUsername : validateUsername,
    validatePassword : validatePassword,
    get     : get,
    create  : create,
  }

})
define([
    './graphs'
  ],
  function(Graphs) {

  var validate = function(name) {
    return !!/^[a-zA-Z_][a-zA-Z0-9-_\s]*$/.exec(name);
  }

  var get = function(db, username, design, callback) {

    var key = createDesignKey(username, design);
    db.get(key, function(err, value) {
      if (err) {
        callback(err);
      } else {
        callback(undefined, value);
      }
    });

  }

  var create = function(db, username, design, callback) {

    var designKey = createDesignKey(username, design);

    Graphs.create(db, username, design, function(err, sha) {
      if (err) {
        callback(err);
        return;
      }
      var refs = {
        'heads' : {
          'master': sha
        }
      }
      db.set(designKey, refs, function(err) {
        if (err) {
          callback(err)
        } else {
          callback();
        }
      })
    });

  }

  var updateRef = function(db, username, design, type, ref, newSHA, callback) {
    var key = createDesignKey(username, design);
    db.get(key, function(err, refs) {
      if (err) {
        callback(err);
        return;
      } 
      if (refs === null) {
        callback('notFound');
        return;
      }

      if (refs.hasOwnProperty(type) && (refs[type].hasOwnProperty(ref))) {
        refs[type][ref] = newSHA;
        db.set(key, refs, function(err) {
          if (err) {
            callback(err)
          } else {
            callback();
          }
        });
      } else {
        callback('notFound');
      }
    });
  }

  var createDesignKey = function(username, design) {
    return username + '/design/' + design;
  }

  return {
    validate : validate,
    get      : get,
    create   : create,
    updateRef: updateRef,
  }

});
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

  var del = function(db, username, design, callback) {

    var key = createDesignKey(username, design);
    db.get(key, function(err, value) {
      if (err) {
        callback(err);
      } else if (value === null) {
        callback('notFound');
      } else {
       
        db.remove(key, function(err, value) {
          if (err) {
            callback(err);
          } else {
            callback(undefined, value);
          }
        });
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

  var rename = function(db, username, from, to, callback) {
    var fromKey = createDesignKey(username, from);
    var toKey = createDesignKey(username, to);
    db.get(fromKey, function(err, refs) {
      if (err) {
        callback(err);
        return;
      } 
      if (refs === null) {
        callback('notFound');
        return;
      }
      db.get(toKey, function(err, conflictRefs) {
        if (err) {
          callback(err);
          return;
        } 
        if (conflictRefs !== null) {
          callback('alreadyExists');
          return;
        }

        // Set the new key & remove old one
        db.set(toKey, refs, function(err) {
          if (err) {
            callback(err);
          } else {
            db.remove(fromKey, function(err) {
              if (err) {
                callback(err);
              } else {
                callback();
              }
            });
          }
        })
      });
    });

  }

  var createDesignKey = function(username, design) {
    return username + '/design/' + design;
  }

  return {
    validate : validate,
    get      : get,
    create   : create,
    del      : del,
    updateRef: updateRef,
    rename   : rename,
  }

});
define([
    'graphapi',
  ], function(GraphAPI) {

  var createNewGraph = function(db, username, callback) {
    
    var emptyGraph = {
      vertices: [],
      edges: [],
      metadata: [],
    }
    create(db, username, emptyGraph, callback);

  }

  var create = function(db, username, object, callback) {
    var sha = GraphAPI.hashObject(object);
    var key = createKey(username, sha);
    db.set(key, object, function(err) {
      callback(err, sha);
    })

  }

  var get = function(db, username, sha, callback) {
    var key = createKey(username, sha);
    db.get(key, callback);
  }

  var createKey = function(username, sha) {
    return username + '/object/' + sha;
  }

  return {
    createNewGraph: createNewGraph,
    create        : create,
    get           : get,
  }

});
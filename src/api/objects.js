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

  var create = function(db, username, graph, callback) {
    console.log(graph)
    var sha = GraphAPI.hashObject(graph);
    var key = createKey(username, sha);
    db.set(key, graph, function(err) {
      callback(err, sha);
    })

  }

  var get = function(db, username, sha, callback) {
    var key = createKey(username, sha);
    db.get(key, callback);
  }

  var createKey = function(username, sha) {
    return username + '/graph/' + sha;
  }

  return {
    createNewGraph: createNewGraph,
    create   : create,
    get      : get,
  }

});
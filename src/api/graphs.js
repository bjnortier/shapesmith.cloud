define([
    'graphapi',
  ], function(GraphAPI) {

  var create = function(db, username, graph, callback) {
    
    var emptyGraph = {
      vertices: [],
      edges: [],
      metadata: [],
    }

    var sha = GraphAPI.hashObject(emptyGraph);
    var key = username + '/graph/' + sha;
    db.set(key, {}, function(err, value) {
      if (err) {
        callback(err);
      } else {
        callback(undefined, sha);
      }
    })
  }

  return {
    create: create,
  }

});
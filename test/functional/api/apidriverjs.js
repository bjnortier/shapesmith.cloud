var http = require('http');
    chai = require('chai'),
    assert = chai.assert;
chai.Assertion.includeStack = true;

var Client = function(host, port, prev) {
  this.host = host;
  this.port = port;
  this.prev = prev;
}

Client.prototype.finish = function(callback) {
  this.do = function() {
    callback();
  }

  var sequence = []
  var p = this;
  while (p.prev) {
    sequence.push(p.prev)
    p = p.prev;
  }
  sequence = sequence.reverse();
  sequence[0].do(0, sequence[1]);
}

Client.addCommand = function(name, fn) {
  Client.prototype[name] = function() {
    var that = this;
    var args = arguments;
    this.do = function(state0, next) {
      fn.apply(this, args).call(this, state0, function(state1) {
        that.next.do(state1);
      });
    }
    this.next = new Client(this.host, this.port, this);
    return this.next;
  }
}

Client.addCommand('foo', function(x) {
  return function(state, callback) {
    setTimeout(function() {
      console.log('foo', state);
      callback(state + 1)
    }, 50)
  }
});

Client.addCommand('get', function(path) {

  return function(state0, callback) {

    http.get({
      host: this.host,
      port: this.port || 80, 
      path: path
    }, function(res) {

      res.on('data', function (chunk) {
        var state1 = {
          statusCode: res.statusCode,
          body: JSON.parse(chunk.toString()),
        }
        callback(state1);
      });

    });
  }
});

Client.addCommand('post', function(path, data) {

  return function(state0, callback) {

    var req = http.request({
      method: 'POST',
      host: this.host,
      port: this.port || 80, 
      path: path,
      headers: {'content-type':'application/json'}
    }, function(res) {

      res.on('data', function (chunk) {
        var state1 = {
          statusCode: res.statusCode,
          body: JSON.parse(chunk.toString()),
        }
        callback(state1);
      });
    })
    req.write(JSON.stringify(data))
    req.end();

  }
});

Client.addCommand('assertCode', function(code) {

  return function(state, callback) {
    assert.equal(state.statusCode, code);
    callback(state);
  }

});

Client.addCommand('assertBody', function(body) {

  return function(state, callback) {
    assert.deepEqual(state.body, body);
    callback(state);
  }

});

exports.Client = Client;

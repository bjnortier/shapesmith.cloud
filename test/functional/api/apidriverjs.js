var request = require('request');
    chai = require('chai'),
    assert = chai.assert;
chai.Assertion.includeStack = true;

var Client = function(baseUrl, prev) {
  this.baseUrl = baseUrl; 
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

Client.safeParse = function(body) {
  try {
    return JSON.parse(body);
  } catch (e) {
    return {body: body};
  }
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
    this.next = new Client(this.baseUrl, this);
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

    request.get(this.baseUrl + path, function(err, res, body) {
      var state1 = {
        statusCode: res.statusCode,
        body: Client.safeParse(body),
      }
      callback(state1);

    });
  }
});

Client.addCommand('post', function(path, data) {

  return function(state0, callback) {

    request.post({
      url: this.baseUrl + path,
      headers: {'content-type':'application/json'},
      body: JSON.stringify(data),
    }, function(err, res, body) {
      var state1 = {
        statusCode: res.statusCode,
        body: Client.safeParse(body),
      }
      callback(state1);
    })

  }
});

Client.addCommand('put', function(path, data) {

  return function(state0, callback) {

    request.put({
      url: this.baseUrl + path,
      headers: {'content-type':'application/json'},
      body: JSON.stringify(data),
    }, function(err, res, body) {
      var state1 = {
        statusCode: res.statusCode,
        body: Client.safeParse(body)
      }
      callback(state1);
    })

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
    assert.deepEqual(state.body, body, JSON.stringify(state.body));
    callback(state);
  }

});

exports.Client = Client;

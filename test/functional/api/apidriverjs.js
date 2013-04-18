var http = require('http');
    chai = require('chai'),
    assert = chai.assert;
chai.Assertion.includeStack = true;

var Future = function(host, port) {

  this.addCommand = function(name, command) {
    var that = this;
    this[name] = function() {
      that.next = new Future(host, port);
      command.apply(that, arguments);
      return that.next;
    }
  }
 
  this.addCommand('get', function(path, callback) {

    var that = this;
    this.do = function(state) {

      http.get({
        host: host,
        port: port || 80, 
        path: path
      }, function(res) {

        res.on('data', function (chunk) {
          var state = {
            statusCode: res.statusCode,
            body: JSON.parse(chunk.toString()),
          }
          that.commandDone(state, callback)
        });
      });

    }
  });

  this.addCommand('post', function(path, data, callback) {

    var that = this;
    this.do = function(state) {

      var req = http.request({
        method: 'POST',
        host: host,
        port: port || 80, 
        path: path,
        headers: {'content-type':'application/json'}
      }, function(res) {

        res.on('data', function (chunk) {
          var state = {
            statusCode: res.statusCode,
            body: JSON.parse(chunk.toString()),
          }
          that.commandDone(state, callback)
        });
      })
      req.write(JSON.stringify(data))
      req.end();

    }
  });

  this.addCommand('assertCode', function(code, callback) {

    var that = this;
    this.do = function(state) {
      assert.equal(state.statusCode, code);
      that.commandDone(state, callback);
    }

  });

  this.addCommand('assertBody', function(body, callback) {

    var that = this;
    this.do = function(state) {
      assert.deepEqual(state.body, body);
      that.commandDone(state, callback);
    }

  });

  this.addCommand('create', function() {
    var that = this;
    setTimeout(function() {
      that.commandDone({})
    }, 0)
  });

  this.commandDone = function(state, callback) {
    if (callback) {
      callback();
    } else {
      this.next.do(state);
    }
  }

}

exports.Future = Future;

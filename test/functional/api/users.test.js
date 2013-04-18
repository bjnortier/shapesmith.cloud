var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('Users', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Future('localhost', 9000);
  });

  it('can be created', function(done) {

    client
        .create()
        .get('/user/foo')
        .assertCode(404)
        .post('/user', {username: "foo"})
        .assertCode(201)
        .assertBody('created')
        .get('/user/foo')
        .assertCode(200)
        .assertBody({}, done)

  });

});
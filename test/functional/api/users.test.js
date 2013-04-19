var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('Users', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Client('localhost', 9000);
  });

  it('can be created', function(done) {

    client
        .get('/user/foo')
        .assertCode(404)
        // Missing username
        .post('/user', {})
          .assertCode(404)
          .assertBody({errors: [{missing:'username'}]})
        // Invalid username
        .post('/user', {username: '_'})
          .assertCode(404)
          .assertBody({errors: [{invalid:'username'}]})
        // Valid user
        .post('/user', {username: "foo"})
          .assertCode(201)
          .assertBody('created')
        // user now exists
        .get('/user/foo')
          .assertCode(200)
          .assertBody({})
        // Duplcate
        .post('/user', {username: "foo"})
          .assertCode(409)
          .assertBody('user already exists')
        .finish(done)

  });

});
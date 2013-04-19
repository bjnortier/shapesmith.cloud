var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('User', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Client('http://localhost:9000');
  });

  it('can be created', function(done) {

    client
      .get('/user/foo')
      .assertCode(401)
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

  it('can be authenticated', function(done) {

    var username = 'bob', password = '1234';

    client
      // Not authorized
      .get('/user/' + username)
        .assertCode(401)
      // Create user, creates the session
      .post('/user', {username: username, password: password})
        .assertCode(201)
      // Authorized
      .get('/user/' + username)
        .assertCode(200)
        .assertBody({})
      .finish(done)

  });

});


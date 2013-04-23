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
        .assertCode(400)
        .assertBody({errors: [{'username':'missing'}, {'password':'missing'}]})
      // Invalid username
      .post('/user', {username: '_', password:'123456'})
        .assertCode(400)
        .assertBody({errors: [{'username':'invalid'}]})
      // Invalid password
      .post('/user', {username: 'bob', password:'123'})
        .assertCode(400)
        .assertBody({errors: [{'password':'must be at least 6 characters'}]})
      // Valid user
      .post('/user', {username: 'foo', password:'123456'})
        .assertCode(201)
        .assertBody('created')
      // user now exists
      .get('/user/foo')
        .assertCode(200)
        .assertBody(function(body) {
          assert.equal(body.username, 'foo');
          assert.isString(body.password_bcrypt);
        })
      // Duplcate
      .post('/user', {username: 'foo', password:'123456'})
        .assertCode(409)
        .assertBody('user already exists')
      .finish(done)

  });

  it('can be authenticated', function(done) {

    var username = 'bob', password = '123456';

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
        .assertBody(function(body) {
          assert.equal(body.username, 'bob')
        })
      .finish(done)

  });

});


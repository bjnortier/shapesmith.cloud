var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('Designs', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Client('http://localhost:9000');
  });

  it('can be created', function(done) {

    client
      .post('/api/bob/design', {})
        .assertCode(404)
        .assertBody({errors: [{missing:'name'}]})
      .post('/api/bob/design', {name: 'ab!!'})
        .assertCode(404)
        .assertBody({errors: [{invalid:'name'}]})
      .post('/api/bob/design', {name: ' \t\n'})
        .assertCode(404)
        .assertBody({errors: [{invalid:'name'}]})
      .post('/api/bob/design', {name: 'a b'})
        .assertCode(201)
      .get('/api/bob/design/a b')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .put('/api/bob/design/a b/refs/heads/master', {})
        .assertCode(404)
        .assertBody({errors: ['value must be a JSON string']})
      .put('/api/bob/design/a b/refs/heads/master', 'xxx')
        .assertBody({errors: ['value must be a 160bit (40 character) SHA']})
        .assertCode(404)
      .put('/api/bob/design/a b/refs/heads/foo', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
        .assertBody('not found')
        .assertCode(400)
      .put('/api/bob/design/a b/refs/heads/master', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
        .assertCode(200)
        .assertBody('ok')
      .get('/api/bob/design/a b')
        .assertCode(200)
        .assertBody({ heads: { master: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' } })
      .finish(done)

  });  

});
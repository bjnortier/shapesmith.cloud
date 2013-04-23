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
      .get('/api/bob/designs')
        .assertCode(200)
        .assertBody([])
      .post('/api/bob/design', {})
        .assertCode(400)
        .assertBody({errors: [{missing:'name'}]})
      .post('/api/bob/design', {name: 'ab!!'})
        .assertCode(400)
        .assertBody({errors: [{invalid:'name'}]})
      .post('/api/bob/design', {name: ' \t\n'})
        .assertCode(400)
        .assertBody({errors: [{invalid:'name'}]})
      .post('/api/bob/design', {name: 'a b'})
        .assertCode(201)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .get('/api/bob/design/a b')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .get('/api/bob/designs')
        .assertCode(200)
        .assertBody(['a b'])
      .finish(done)

  });  

  it('can be updated', function(done) {

     client
      .post('/api/jimmy/design', {name: 'dock'})
        .assertCode(201)
      .get('/api/jimmy/design/dock')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .put('/api/jimmy/design/dock/refs/heads/master', {})
        .assertCode(400)
        .assertBody({errors: ['value must be a JSON string']})
      .put('/api/jimmy/design/dock/refs/heads/master', 'xxx')
        .assertCode(400)
        .assertBody({errors: ['value must be a 160bit (40 character) SHA']})
      .put('/api/jimmy/design/dock/refs/heads/foo', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
        .assertCode(404)
        .assertBody('not found')
      .put('/api/jimmy/design/dock/refs/heads/master', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
        .assertCode(200)
        .assertBody('ok')
      .get('/api/jimmy/design/dock')
        .assertCode(200)
        .assertBody({ heads: { master: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' } })
      .finish(done)

  });

  it('can be deleted', function(done) {
     client
      .delete('/api/steve/design/aaa')
        .assertCode(404)
      .post('/api/steve/design', {name: 'aaa'})
        .assertCode(201)
      .get('/api/steve/design/aaa')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .delete('/api/steve/design/aaa')
        .assertCode(200)
        .assertBody('ok')
      .get('/api/steve/design/aaa')
        .assertCode(404)
        .assertBody('not found')
      .get('/api/steve/designs')
        .assertCode(200)
        .assertBody([])
      .finish(done);

  });

  it('can be renamed', function(done) {
    
    client
      .post('/api/woz/design', {})
        .assertCode(400)
        .assertBody({errors: [{missing: 'name'}]})
      .post('/api/woz/design', {name: 'foo'})
        .assertCode(201)
      .get('/api/woz/design/foo')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .post('/api/woz/design/baz', {newName: 'foo'})
        .assertCode(404)
        .assertBody('not found')
      .post('/api/woz/design/foo', {newName: 'foo'})
        .assertCode(409)
        .assertBody('already exists')
      .post('/api/woz/design/foo', {newName: 'foo2'})
        .assertCode(200)
        .assertBody('ok')
      .get('/api/woz/design/foo2')
        .assertCode(200)
        .assertBody({ heads: { master: '51ba39e8fdd07321b190226727022c35649d8da4' } })
      .get('/api/woz/design/foo')
        .assertCode(404) 
      .get('/api/woz/designs')
        .assertCode(200)
        .assertBody(['foo2'])
      .finish(done)

  });

});

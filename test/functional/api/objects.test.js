var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('Objects', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Client('http://localhost:9000');
  });

  describe('Graphs', function() {

    it('can be created and fetched', function(done) {

      client
        .get('/api/bob/graph/7706daff9b8532e7b9294528c73f2e79c4a8e748')
          .assertCode(404)
          .assertBody('not found')
        .post('/api/bob/graph/', {vertices:[], edges:[]})
          .assertCode(201)
          .assertBody('7706daff9b8532e7b9294528c73f2e79c4a8e748')
        .get('/api/bob/graph/7706daff9b8532e7b9294528c73f2e79c4a8e748')
          .assertCode(200)
          .assertBody({vertices:[], edges:[]})
        .finish(done);

    });

  });

  describe('Vertices', function() {

    it('can be created and fetched', function(done) {

      client
        .get('/api/bob/vertex/46f4e8f259f523264d7f32e7120585a0a4af9ff8')
          .assertCode(404)
          .assertBody('not found')
        .post('/api/bob/vertex/', {type:'cube'})
          .assertCode(201)
          .assertBody('46f4e8f259f523264d7f32e7120585a0a4af9ff8')
        .get('/api/bob/vertex/46f4e8f259f523264d7f32e7120585a0a4af9ff8')
          .assertCode(200)
          .assertBody({type:'cube'})
        .finish(done);

    });

  });

});
var chai = require('chai'),
    assert = chai.assert,
    apidriverjs = require('./apidriverjs');
chai.Assertion.includeStack = true;

describe('Users', function() {

  var client;

  beforeEach(function() {
    client = new apidriverjs.Client('localhost', 9000);
  });


});
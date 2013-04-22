define([
    'underscore',      
    './designs'
  ], function(_, Designs) {

  var DesignAPI = function(app, db) {

    // Get the design refs
    app.get(/^\/api\/([\w%]+)\/design\/([\w%]+)\/?$/, function(req, res) {

      var username = decodeURI(req.params[0]);
      var design = decodeURI(req.params[1]);
      Designs.get(db, username, design, function(err, refs) {
        if (err) {
          res.json(500);
        } else if (refs === null) {
          res.json(404, 'not found');
        } else {
          res.json(refs);
        }
      });

    });

    // Create a new design
    app.post(/^\/api\/([\w%]+)\/design\/?$/, function(req, res) {
      var username = decodeURI(req.params[0])
      var design = req.body.name && req.body.name.trim();

      if (design === undefined) {
        res.json(404, {errors: [{missing: 'name'}]});
        return
      }
      if (!Designs.validate(design)) {
        res.json(404, {errors: [{invalid: 'name'}]});
        return
      }

      Designs.get(db, username, design, function(err, value) {
        if (err) {
          res.json(500, err);
          return;
        } 
        if (value !== null) {
          res.json(409, 'design already exists');
          return;
        } 

        Designs.create(db, username, design, function(err) {
          if (err) {
            res.json(500, err);
          } else {
            res.json(201, 'created');
          }
        });

      });

    });

    // Update ref
    app.put(/^\/api\/([\w%]+)\/design\/([\w%]+)\/refs\/(\w+)\/(\w+)\/?$/, function(req, res) {
      var username = decodeURI(req.params[0]);
      var design = decodeURI(req.params[1]);
      var type = req.params[2];
      var ref = req.params[3];
      var newSHA = req.body;

      if (!_.isString(newSHA)) {
        res.json(404, {errors: ['value must be a JSON string']});
        return;
      }

      if (!(newSHA.length === 40)) {
        res.json(404, {errors: ['value must be a 160bit (40 character) SHA']});
        return;
      }

      Designs.updateRef(db, username, design, type, ref, newSHA, function(err) {
        if (err === 'notFound') {
          res.json(400, 'not found');
        } else if (err) {
          res.json(500, err);
        } else {
          res.json('ok');
        }

      });

    });

  }

  return DesignAPI;

});
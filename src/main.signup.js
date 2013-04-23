requirejs.config({
    paths: {
        'underscore': 'node_modules/underscore/underscore',
        'backbone': 'node_modules/backbone/backbone',
    },
    shim: {
        'underscore': {
            exports: '_'
        },
        'backbone': {
            deps: ['underscore', 'jquery'],
            exports: 'Backbone'
        },
    },
});

requirejs([
        'jquery',
        'lib/jquery.mustache',
        'underscore',
        'backbone'
    ], function($, $$, _, Backbone) {

    var Model = Backbone.Model.extend({

        initialize: function() {
            this.view = new View({model: this});
        },

    })

    var View = Backbone.View.extend({

        className: 'signup-form',

        initialize: function() {
            this.render();
            $('.maincolumn').append(this.$el);

            var that = this;
            $(window).resize(function() {
                that.resize();
            });
            this.resize();
        },

        render: function() {
            var template = 
                '<div class="inputs">' +
                    '<div class="email"><input name="email" type="text" placeholder="email address"/><div class="msg"></div></div>' + 
                    '<div class="password"><input name="password" type="password" placeholder="password"/><div class="msg"></div></div>' + 
                '</div>' +
                '<div><button type="submit" class="button">Sign up</button></div>';
            this.$el.html($.mustache(template, {}));
        },

        events: {
            'click .button' : 'signup',
        },

        resize: function() {
            var w = $('.inputs').width();
            $('.inputs input').css('width', (w-20) + 'px');
        },

        signup: function(event) {
            event.stopPropagation();

            var emailAddress = this.$el.find('input[name="email"]').val();
            var password = this.$el.find('input[name="password"]').val();
            console.log(emailAddress, password);
            var that = this;
            $.ajax({
                type: 'POST',
                url: '/user/',
                data: JSON.stringify({username: emailAddress, password: password}),
                dataType: 'json',
                contentType: 'application/json',
                success: function(response) {
                    window.location = '/ui/' + encodeURIComponent(emailAddress) + '/designs' 
                },
                error: function(response) {
                    var parsed = JSON.parse(response.responseText)
                    if (parsed.errors) {
                        parsed.errors.forEach(function(err) {
                            if (err.username) {
                                that.$el.find('div.email').addClass('error');
                                that.$el.find('div.email .msg').html(err.username);
                            }
                            if (err.password) {
                                that.$el.find('.password').addClass('error');   
                                that.$el.find('div.password .msg').html(err.password);

                            }
                        });
                    }
                }
            });

        },

    })


    $(document).ready(function() {
        new Model();
    });

});


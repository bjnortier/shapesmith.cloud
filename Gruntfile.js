var path = require('path');

module.exports = function(grunt) {

  grunt.initConfig({
    simplemocha: {
      options: {
        // globals: ['should'],
        timeout: 3000,
        slow: 5000,
        ignoreLeaks: false,
        ui: 'bdd',
        reporter: 'spec',
        path: 'test'
      },

      unit: { 
        src: 'test/unit.js',
      },
      api: { 
        src: [
            'test/functional/api/*.test.js',
        ],
      },
      ui: { 
        src: [
            'test/functional/ui/points.test.js',
            'test/functional/ui/polylines.test.js',
        ],
      },
    },

    requirejs: {
      compile: {
        options: {
          appDir: ".",
          baseUrl: "src",
          dir: "build",
          optimize: "none",
          mainConfigFile: "src/main.ui.js",
          modules: [
            {
              name: "main.ui"
            }
          ]
        }
      }
    },  

    express: {
        server: {
          options: {
            port: 9000,
            server: path.resolve('./src/api/server.js')
          }
        }
    },

    chmod: {
      options: {
        mode: '755'
      },
      build: {
        src: ['build/bin/start', 'build/node_modules/supervisor/lib/cli-wrapper.js']
      }
    },

    watch: {
      api: {
        files: ['src/api/**/*.js', 'test/functional/api/**/*.js'],
        tasks: ['api']
      }
    },

  });

  grunt.loadNpmTasks('grunt-contrib-requirejs');
  grunt.loadNpmTasks('grunt-simple-mocha');
  grunt.loadNpmTasks('grunt-express');
  grunt.loadNpmTasks('grunt-chmod');
  grunt.loadNpmTasks('grunt-contrib-watch');

  // Unit testing
  grunt.registerTask('unit', ['simplemocha:unit']);
  
  // Functional testing - requires a running server 
  process.env['app_env'] = 'functional';

  grunt.registerTask('api', ['express', 'simplemocha:api']);
  grunt.registerTask('ui', ['express', 'simplemocha:ui']);
  grunt.registerTask('functional', ['express', 'simplemocha:api', 'simplemocha:ui']);

  // Build the single JS file
  grunt.registerTask('build', ['requirejs', 'chmod:build'])

};

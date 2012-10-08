var ws = new Object;  
var values = [];

var readyStateCheckInterval = setInterval(function() {
    if (document.readyState === "complete") {
        init();
        clearInterval(readyStateCheckInterval);
    }
}, 10);


function init() { 
    if (!("WebSocket" in window)) {  
        alert("This browser does not support WebSockets");  
        return;  
    }  
    ws = new WebSocket("ws://localhost:8080/ws");  
    ws.onopen = function() {  
        console.log('Connected');  
    };  
    ws.onmessage = function (evt) {
        var received_msg = evt.data;  
        try {
            var parsed = JSON.parse(received_msg);
            if (parsed['all']) {
                var metrics = parsed['all'];
                var history = metrics[0]['value'];
                values = history.map(function(event) {
                    return {
                        date: new Date(event['timestamp']/1000),
                        price: event['value']
                    }
                },
                history);
            } 
            if (parsed['value']) {
                values.unshift({
                    date: new Date(),
                    price: parsed['value']['value']
                })
            } 
            if (parsed['daily']) {
                var dailystats = parsed['daily'];
                d3.select('#users').select('.history').selectAll('span')
                .data(dailystats).enter().append('span').text(function(daily) {
                    return daily['users'];
                });
                d3.select('#temp-users').select('.history').selectAll('span')
                .data(dailystats).enter().append('span').text(function(daily) {
                    return daily['temp_users'];
                });
                d3.select('#designs').select('.history').selectAll('span')
                .data(dailystats).enter().append('span').text(function(daily) {
                    return daily['models'];
                });
            }
            
        } catch(e) {
            console.error(e);
        }
    };  
    ws.onclose = function() {
        console.log('Connection closed');  
    };  
}  


var m = [10, 30, 10, 0],
w = 600 - m[1] - m[3],
h = 200 - m[0] - m[2];

// Scales and axes. Note the inverted domain for the y-scale: bigger is up!
var x = d3.time.scale().range([0, w]),
y = d3.scale.linear().range([h, 0]),
xAxis = d3.svg.axis().scale(x).tickSize(-h).tickSubdivide(true),
yAxis = d3.svg.axis().scale(y).ticks(4).orient("right");

// An area generator, for the light fill.
var area = d3.svg.area()
    //.interpolate("monotone")
    .x(function(d) { return x(d.date); })
    .y0(h)
    .y1(function(d) { return y(d.price); });

// A line generator, for the dark stroke.
var line = d3.svg.line()
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.price); });

// var data = [2,1,2,2,2,2,1,1,0,2,2,2,0,2,1,1,1,2,1,1,0,2,2,2,0,2,1,1];
// var now = new Date();
// var values = data.map(function(available, i) {
//     return {
//         date: new Date(now.getTime() - i*1000),
//         price: available
//     };
// }).reverse();



function render() {


    // Compute the minimum and maximum date, and the maximum price.
     x.domain([new Date(new Date().getTime() - 5*60*1000), new Date()]);
     y.domain([0, 2]);

      // Add an SVG element with the desired dimensions and margin.
      var svg = d3.select("#available_workers").select(".graph").append("svg:svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", h + m[0] + m[2])
      .append("svg:g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")");

      // Add the clip path.
      svg.append("svg:clipPath")
      .attr("id", "clip")
      .append("svg:rect")
      .attr("width", w)
      .attr("height", h);

      // Add the area path.
      svg.append("svg:path")
      .attr("class", "area")
      .attr("clip-path", "url(#clip)")
      .attr("d", area(values));

      // Add the x-axis.
      svg.append("svg:g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + h + ")")
      .call(xAxis);

      // Add the y-axis.
      svg.append("svg:g")
      .attr("class", "y axis")
      .attr("transform", "translate(" + w + ",0)")
      .call(yAxis);

      // Add the line path.
      svg.append("svg:path")
      .attr("class", "line")
      .attr("clip-path", "url(#clip)")
      .attr("d", line(values));

      // Add a small label for the symbol name.
      // svg.append("svg:text")
      // .attr("x", w - 6)
      // .attr("y", h - 6)
      // .attr("text-anchor", "end")
      // .text(values[0].symbol);

      setTimeout(function() {
        d3.select("svg") .remove();
        if (values.length > 0) {
            values.unshift({
                date: new Date(),
                price: values[0].price
            });
            d3.select('#available_workers').select('.current-value').data([values[0].price]).text(function(d) { return d; });
        }
        render();
    }, 1000);
  }

  render();    
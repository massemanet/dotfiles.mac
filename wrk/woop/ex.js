var scriptHost = "data.php";

var getData = function(url) {
    // cross browser request setup
    var http_request = false;

    if (window.XMLHttpRequest) { // Mozilla, Safari,...
        http_request = new XMLHttpRequest();
        if (http_request.overrideMimeType) {
            http_request.overrideMimeType('text/text');
        }
    } else if (window.ActiveXObject) { // IE
        try {
            http_request = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                http_request = new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e) {}
        }
    }

    // Update chart if data is recieved.
    http_request.onreadystatechange = function() {
        if (http_request.readyState == 4 && http_request.status == 200)
            eval("chart.update(http_request.responseText);");
    };

    http_request.open('GET', url, true);
    http_request.send(null);
};


var Chart = function() {
    // init function for Chart
    this.init = function(id) {
        var canvas = document.getElementById(id);
        this.p = Processing(canvas);

        // Setup function for Processing.js
        this.data = [1,2];
        this.p.setup = function() {
            this.data = [100,150,50,75,125];
            this.size(500,150);
            this.stroke(255);
        };

        /* Drawing function for Processing.js */
        this.p.draw = function() {
            this.background(20);
            var l = this.data.length;
            var h = this.height;

            // Find maximum value.
            var max = this.data[0];
            this.data.forEach(function(x) {
                    if (x > max) max = x;
                });
            // Prevent max value from being on
            // absolute top of canvas.
            max = max * 1.05;

            // Scale vertical position based on maximum value.
            var scaled = this.data.map(function(x) {
                    var ratio = (x * 1.0) / max;
                    return h - (h * ratio);
                });

            // Draw colored shape.
            this.fill(this.color(255,50,50));
            this.beginShape();
            this.vertex(0,h);
            for (var i=0;i<l;i++) {
                var x = this.width * 1.0 * (i/(l-1));
                var y = scaled[i];
                this.vertex(x,y);
            }
            this.vertex(this.width, h);
            this.endShape();

            // Draw horizontal grid lines, and circles
            // at data points.
            this.fill(this.color(255,0,0));
            for (var i=0;i<l;i++) {
                var x = this.width * 1.0 * (i/(l-1));
                var y = scaled[i];
                this.line(x,0,x,h);
                this.ellipse(x,y,10,10);
            }
        };

        this.p.init(); // start Processing.js

        // run this.lookup every 10 seconds
        setInterval(this.lookup, 10000);
    };

  // query php script for new data
  this.lookup = function() {
    getData(scriptHost);
  };

  // Used to update data.
  this.update = function(val) {
    var maxLen = 10;
    //val = this.p.random(50,250); // fake retrieval
    this.p.data.push(val);

    // Retain a maximum of ten pieces of data.
    var len = this.p.data.length;
    if (len > maxLen) {
      this.p.data = this.p.data.slice(len-1-maxLen);
    }
    this.p.draw();
  };
};

var chart = new Chart();

var exchangechart = function(id) {
  chart.init(id);
};

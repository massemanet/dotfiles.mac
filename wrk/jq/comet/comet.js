function make_plot(plot){
  $.plot($("#placeholder"),
         [{label: plot.label,
           data:plot.array}],
         {yaxis: {min: 0,
                  max: 100},
          xaxis: {mode: "time",
                  minTickSize: [1, "minute"]},
          bars: {show: true}});};

function make_ajax(plot){
  $.ajax({url: plot.label,
          dataType: "json",
          success: function(response){successHandler(response,plot);},
          error: function(xhr,status){errorHandler(status,plot);},
          timeout: 2200});};

function successHandler(response,plot){
  make_plot(plot.update(response.data));
  $("#status").text("up :"+response+":"+response.data);
  make_ajax(plot);};

function errorHandler(status,plot){
  make_plot(plot.update(0));
  $("#status").text("down");
  make_ajax(plot);};

function Plot(label,size,tick){
  this.label = label;
  this.size = size;
  this.tick = 2000; // [ms]
  this.array = new Array(size);
  this.index = -1;
  this.update =
     function(value){
       if ( this.size <= ++this.index ) {this.index=0;};
       this.array[this.index] = [(new Date).getTime(),value];
       return this;
     };
  var i;
  var t0 = (new Date).getTime()-(size-1)*tick;
  for (i=0; i<this.size; i++){
    this.array[i] = [t0+this.tick*i,10];};
};

$(function () {
  plot_load = new Plot("load",100,2000);
  make_plot(plot_load);
  make_ajax(plot_load);});

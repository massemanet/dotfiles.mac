/*global $ */
/*jslint
 plusplus: false,
 browser: true,
 forin: true
 undef: true,
 nomen: true,
 eqeqeq: true,
 white: false,
 bitwise: true,
 newcap: true,
 immed: true */
"use strict";

$(function () {
    function Ajax(timeout,renderers){
      var error_handler, success_handler;
      error_handler =
        function (that,status) {
          if ( $("#toggle").text() === "Start" ) {
            $("#status").text("stopped");
          }else{
            if ( status === "error" || status === "parsererror" ) {
              setTimeout(function() { error_handler(that,"disconnected"); },
                         that.timeout);
            }else{
              for (var i in that.renderers) {(that.renderers[i]).update(null);}
              $("#status").text("down: "+status);
              $("#error").text(status);
              $.ajax(that);
            }
          }
        };

      success_handler =
        function(that,response){
          var i;
          if ( $("#toggle").text() === "Start" ) {
            $("#status").text("stopped");
          }else{
            $("#status").text("up :");
            for (i in that.renderers) {(that.renderers[i]).update(response);}
            $.ajax(that);
          }
        };

      this.url = null;
      this.dataType = "json";
      this.timeout = timeout;  // ms
      this.renderers = renderers;
      this.success= function(response)   {success_handler(this,response);};
      this.error  = function(xhr,status) {error_handler(this,status);};
    }

    function find(path,vals) {
      var p, v, i;
      p = path.split(".");
      if ( vals === null ) {
        return 0;
      }else{
        v = vals;
        for (i in p)
        {v = v[p[i]];
         if ( v === undefined ) { return 0;}}
        return v;
      }
    }

    function Plot(title,max,graphs){
      this.title = title;
      this.max = max;
      this.graphs = graphs;

      this.update =
        function(values) {
          var t, i;
          t = (new Date(find("prfSys.now",values))).toLocaleTimeString();
          $("#status").text("up - "+t+" - "+find("prfSys.node",values));
          for ( i in this.graphs ) { this.graphs[i].update(values);}
          $.plot($(this.title),
                 graphs,
                 {yaxis: {min: 0,
                          max: this.max},
                  xaxis: {mode: "time",
                          minTickSize: [1,"minute"]}});
        };
    }

    function Graph(path,opts) {
      this.label = path.split(".").reverse()[0]; // for flot
      this.bars = {show:false};         // for flot
      this.data = [];                   // for flot
      this.scaling = 1;
      this.size = 200;
      var index = -1,                  // private
      i;                             // private

      this.update =
        function(vals){
          var ni, v;
          index++;
          if ( this.size <= index ) {index=0;}
          if ( index === this.size-1 ) {
            ni = 0;
          }else{
            ni=index+1;
          }
          this.data[ni] = null;
          v = this.scaling*find(path,vals);
          this.data[index] = [new Date().getTime(),v];
        };

      for ( i in opts ) { this[i] = opts[i];}
    }

    function Table(containerDiv,dataField,columns) {
      this.id = containerDiv+"-table";
      this.name = this.id.split("#")[1];
      this.update =
        function(values) {
          $(this.id+" tbody tr").remove();
          if ( values !== null ) {
            var i, j, tr, ps;
            ps = find(dataField,values);
            for ( i in ps ) {
              tr = $("<tr>");
              for ( j in columns ) {
                tr.append($("<td>").text(find(columns[j].tag,ps[i])));
              }
              $(this.id+" tbody").append(tr);
            }
            $(this.id).trigger("update").trigger("sorton");
          }
        };

      var j, thr, thead, tbody;
      thr = $("<tr>");
      for ( j in columns ) {
        thr.append($("<th class=\"{sorter: '"+columns[j].parser+"'}\">")
                   .text(columns[j].tag));}
      thead = $("<thead>").append(thr);
      tbody = $("<tbody>");
      $("<table id='"+this.name+"' class='tablesorter'>")
        .append(thead)
        .append(tbody)
        .appendTo(containerDiv)
        .tablesorter({widthFixed: true, widgets: ['zebra']});
    }

    function Column(tag,parser) {
      this.tag = tag;
      this.parser = parser;
    }

    function Ajax_prf() {
      this.channel = "/prf";
      this.ajax =
        new Ajax(3000,
                 [new Plot("#loads",400,
                           [new Graph("prfSys.user",{scaling:100,
                                                     bars:{show:true}}),
                            new Graph("prfSys.iowait",{scaling:100}),
                            new Graph("prfSys.kernel",{scaling:100})]),
                  new Plot("#memory",24,
                           [new Graph("prfSys.beam_vss",{scaling:9.31e-10}),
                            new Graph("prfSys.total",{scaling:9.31e-10}),
                            new Graph("prfSys.processes",{scaling:9.31e-10}),
                            new Graph("prfSys.ets",{scaling:9.31e-10})]),
                  new Table("#procs","procs",
                            [new Column("name"),
                             new Column("pid"),
                             new Column("current_function"),
                             new Column("memory","digit"),
                             new Column("dmemory","digit"),
                             new Column("msgs","digit"),
                             new Column("cpu","digit")])]);
    }

    function click_on_return(e,button) {
      if( (e.keyCode ? e.keyCode : e.which) === 13) {
        $(button).click();
      }
    }

    function toggle(event) {
      var target;
      if ( $("#toggle").text() === "Start" ) {
        $("#target").get(0).disabled = true;
        target = $("#target").val();
        $("#status").text("connecting to "+target);
        event.data.ajax.url = target+event.data.channel;
        $.ajax(event.data.ajax);
        $("#toggle").text("Stop");
      }else{
        $("#target").get(0).disabled = false;
        $("#status").text("stopped");
        $("#toggle").text("Start");
      }
    }

    $("#target").get(0).disabled = false;
    $("#toggle").bind("click",new Ajax_prf(),toggle);
    $("#target").bind("keypress",function(e) { click_on_return(e,"#toggle");});

  });

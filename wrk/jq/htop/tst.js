$(function () {
    // call the tablesorter plugin, the magic happens in the markup
    var happen = function(n){
      var r, c, tr;
      var rows = [3,7,9,12];
      var columns = ["a", "b","c",8,7, 2, 212];
      $("table tbody tr").remove();
      for ( r in rows ) {
        tr = $("<tr>");
        for ( c in columns ) {
          if ( c < 4 ) {
            tr.append($("<td>").text(columns[c]));
          }else{
            tr.append($("<td>").text(n*columns[c]*rows[r]));
          }
        }
        $("table tbody").append(tr);
      }
      $("table").trigger("update").trigger("sorton");
      setTimeout(function(){happen(n+1);},2000);
    };

    var r, tr, thead, tbody, tab;
    var rr = ["Frst", "Scnd", "Thrd", "Frth", "Ffth", "Sxth", "Svnth"];
    tr = $("<tr>");
    for ( r in rr ){
      tr.append($("<th class=\"{sorter: 'digit'}\">").text(rr[r]));
    }
    thead = $("<thead>").append(tr);
    tbody = $("<tbody>");
    tab = $("<table class='tablesorter'>").append(thead).append(tbody);
    tab.appendTo("#mytable").tablesorter();
    happen(2);
    });

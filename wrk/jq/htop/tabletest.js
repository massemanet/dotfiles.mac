$(function() {
    function mk_table(columns) {
      var thr = $("<tr>");
      for ( j in columns ) {
        thr.append($("<th>").text(columns[j]));}
      var thead = $("<thead>").append(thr);
      var tbody = $("<tbody>");
      $("<table id='a-table' class='tablesorter'>")
        .append(thead)
        .append(tbody)
        .appendTo($("#a-div"));
    }
    mk_table(["name","age","status","beard"]);
    $("#a-table").tablesorter();
    $("#append").click(
      function() {
        // add some html
        var html =
          "<tr><td>Tobbe</td><td>67</td><td>Senior</td><td>yes</td></tr>";
        html +=
          "<tr><td>Peter</td><td>12</td><td>Junior</td><td>no</td></tr>";
        html +=
          "<tr><td>Andreas</td><td>17</td><td>Junior</td><td>no</td></tr>";
        html +=
          "<tr><td>Masse</td><td>227</td><td>Senior</td><td>yes</td></tr>";
        // append new html to table body
        $("table tbody").append(html);
        // let the plugin know that we made a update
        $("table").trigger("update");
        // set sorting column and direction, this will sort on the first and third column
        var sorting = [[2,1],[0,0]];
        // sort on the first column
        $("table").trigger("sorton",[sorting]);
        return false;
      });
  });

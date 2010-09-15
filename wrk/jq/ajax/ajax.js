$(function(){
    $("#ajaxButton").click(
      function(){
        $.ajax({url:"someData", timeout:5000});});
    $('#error-div').ajaxError(
        function(info,xhr){
          $(info.target)
            .append("<div>Status: "+xhr.status+" "+xhr.statusText+"</div>");});
    $("#success-div").ajaxSuccess(
      function(info,xhr){
          $(info.target)
            .append("<div>Response: "+xhr.responseText+"</div>");});
  }
);

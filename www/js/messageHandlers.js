Shiny.addCustomMessageHandler("sawkillonly",
  function(message) { 
    var filters = $(".sawkillonly");
    filters.each(function() {
        if(message.location != "SawKill") { $(this).addClass(" hidden") }
        else { $(this).removeClass("hidden") }
    });
  }
)


























// toggle sawkill-only filters
Shiny.addCustomMessageHandler("togglesawkillonlyfilters",
  function(message) { 
    var filters = $(".sawkillonly");
    filters.each(function() {
        if(message.location != "SawKill") { $(this).addClass(" hidden") }
        else { $(this).removeClass("hidden") }
    });
  }
)

// Toggle nutrient filters
Shiny.addCustomMessageHandler("togglenutrientfilters",
  function(message) { 
    var chemistry = $(".chemistry");
    chemistry.each(function() {
        if(message.type == "chemistry") { $(this).removeClass("hidden") }
        else { $(this).addClass(" hidden") }
    });
    var nutrients = $(".nutrients");
    nutrients.each(function() {
        if(message.type == "nutrients") { $(this).removeClass("hidden") }
        else { $(this).addClass(" hidden") }
    });
    var landcover = $(".landcover");
    landcover.each(function() {
        if(message.type == "landuse") { $(this).removeClass("hidden") }
        else { $(this).addClass(" hidden") }
    });
  }
)


//// set location based on URL
//Shiny.addCustomMessageHandler("setlocation",
//  function(message){
//    var location = 'SawKill';
//    var url = document.createElement('a');
//    var urlstr = url.baseURI;
//    var locsearch = urlstr.split('?location=');
//    if(locsearch.length > 1) { location = locsearch[1] }
//    Shiny.setInputValue('location', location, {priority: 'event'});
//  }
//)






















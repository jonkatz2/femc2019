$(document).ready(function() {
  $('[data-toggle="tooltip"]').tooltip();
});

function showmap(event) {
    $("#primarybuttons").find(".redbutton").each(function() {
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
    $("#mapcontainer").removeClass("hidden");
    $("#tablecontainer").addClass(" hidden");
    $("#chartcontainer").addClass(" hidden");
    Shiny.setInputValue("gotobasin", [], {priority: "event"});
};

function showchart(event) {
    $("#primarybuttons").find(".redbutton").each(function(){
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
    $("#mapcontainer").addClass(" hidden");
    $("#tablecontainer").addClass(" hidden");
    $("#chartcontainer").removeClass("hidden");
    Shiny.setInputValue("gotobasin", [], {priority: "event"});
};

function showtable(event) {
    $("#primarybuttons").find(".redbutton").each(function(){
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
    $("#mapcontainer").addClass(" hidden");
    $("#tablecontainer").removeClass("hidden");
    $("#chartcontainer").addClass(" hidden");
};

function showfilters(event) {
    $("#statistics_open").removeClass(" clicked");
    $(event.target).addClass("clicked");
    $("#filterfieldbox").removeClass("hidden");
    $("#statisticsbox").addClass(" hidden");
};

function showstatistics(event) {
    $("#filterfield_open").removeClass(" clicked");
    $(event.target).addClass("clicked");
    $("#filterfieldbox").addClass(" hidden");
    $("#statisticsbox").removeClass("hidden");
};

//make the metric buttons indicate a current state
function clickedmetric(event) {
    $("#metricbuttons").find(".redbutton").each(function() {
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
};

//make the map buttons indicate a current state
function clickedmap(event) {
    $("#mapbuttons").find(".redbutton").each(function() {
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
};


//toggle visibility of filters when clicked
function togglefilter(event) {
    var filter = $(event.target).parent(".filtergroup").find(".filter");
    if(filter.hasClass("hidden")) { filter.removeClass("hidden"); }
    else { filter.addClass(" hidden"); }
    var icon = $(event.target).find("i.fa");
    if(icon.hasClass("fa-caret-right")) { icon.removeClass("fa-caret-right").addClass(" fa-caret-down"); }
    else { icon.removeClass("fa-caret-down").addClass(" fa-caret-right"); }
};

//unhide specified filter
function showfilter(id) {
    var el = $("#"+id);
    var filter = el.parent(".filter");
    if(!filter.length) filter = el.parent("div").parent("div").parent(".filter");
    filter.removeClass("hidden"); 
    var icon = el.parent(".filter").parent(".filtergroup").find("i.fa");
    if(icon.hasClass("fa-caret-right")) { icon.removeClass("fa-caret-right").addClass(" fa-caret-down"); }
};

//Go to a site selected from the map
function gotosite(site) {
    console.log(site);
    $("#tableview_open").click();
    showfilter("site");
//    showfilter("location");
    Shiny.setInputValue("gotosite", site, {priority: "event"});
};

//Go to a basin selected from the map
function gotobasin(basin) {
    console.log(basin);
    $("#tableview_open").click();
    showfilter("subbasin");
//    showfilter("location");
    Shiny.setInputValue("gotobasin", basin, {priority: "event"});
};


//toggle caret only
function togglecaret(event) {
    var icon = $(event.target).find("i.fa");
    if(icon.hasClass("fa-caret-right")) { icon.removeClass("fa-caret-right").addClass(" fa-caret-down"); }
    else { icon.removeClass("fa-caret-down").addClass(" fa-caret-right"); }
};


//// hide some filters for roe jan, show for sawkill
//function sawkillonly() {
//    var loc = [];
//    $("#location").find("input").each(function{
//        if(this.attr("checked").length) loc.push(this.attr("value"))
//    });
//    loc = loc[0]
//    var filters = $(".sawkillonly");
//    filters.each(function() {
//        if(loc != "SawKill") { this.addClass(" hidden") }
//        else { this.removeClass("hidden") }
//    });
//}








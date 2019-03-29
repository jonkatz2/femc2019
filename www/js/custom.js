function showmap(event) {
    $("#primarybuttons").find(".redbutton").each(function() {
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
    $("#mapcontainer").removeClass("hidden");
    $("#tablecontainer").addClass(" hidden");
    $("#chartcontainer").addClass(" hidden");
};

function showchart(event) {
    $("#primarybuttons").find(".redbutton").each(function(){
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
    $("#mapcontainer").addClass(" hidden");
    $("#tablecontainer").addClass(" hidden");
    $("#chartcontainer").removeClass("hidden");
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

function clicked(event) {
    $("#secondarybuttons").find(".redbutton").each(function() {
        $(this).removeClass("clicked")
    });
    $(event.target).addClass(" clicked");
};

function togglefilter(event) {
    console.log("toggle");
    var filter = $(event.target).parent(".filtergroup").find(".filter");
    if(filter.hasClass("hidden")) { filter.removeClass("hidden"); }
    else { filter.addClass(" hidden"); }
    var icon = $(event.target).find("i.fa");
    if(icon.hasClass("fa-caret-right")) { icon.removeClass("fa-caret-right").addClass(" fa-caret-down"); }
    else { icon.removeClass("fa-caret-down").addClass(" fa-caret-right"); }
};











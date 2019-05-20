

// set the dimensions and margins of the graph
var margin = {top: 20, right: 20, bottom: 150, left: 90},
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;

var plotwidth = width * 0.75
var legwidth = width - plotwidth

// parse the date / time
var parseTime = d3.timeParse("%j");

// set the ranges
var x = d3.scaleTime().range([0, plotwidth]);
var y = d3.scaleLinear().range([height, 0]);

// clear the chart before replotting
svg.selectAll("g").remove()
var chartarea = svg.append("g")
    .attr("class", "chart-area")
    .attr("font-size", "1.1em")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// Define the div for the tooltip
var tooltip = d3.select("body").append("div")	
    .attr("class", "chart-tooltip")				
    .style("opacity", 0)
    .style("background-color", "#333")
    .style("color", "#FFF")
    .style("padding", "5px")
    .style("position", "absolute")
    .style("z-index", -1);

var lineOpacity = "0.8";
var lineOpacityHover = "1";
var otherLinesOpacityHover = "0.1";
var lineStroke = "2.5px";
var lineStrokeHover = "6px";

var circleOpacity = '0.85';
var circleOpacityOnLineHover = "0.25"
var circleRadius = 5;
var circleRadiusHover = 7;


// get unique values of models and genes
var allDates = d3.map(data, function(d){return d.day;}).keys();
allDates.forEach(function(d, i) { allDates[i] = parseTime(d); })
var allYears = d3.map(data, function(d){return d.year;}).keys();

var newdata = [];
allYears.forEach(function(d) {
  newdata.push({"year":d, "values":[]}); 
});

data.forEach(function(d) {
   newdata[allYears.indexOf(String(d.year))].values.push({"day":+d.day, "value":d.value});
});

// year colors
var color = d3.scaleOrdinal(d3.schemeCategory10)
  .domain(allYears);

// Scale the range of the data
x.domain([parseTime(0), parseTime(365)]);
y.domain([d3.min(data, function(d) { return +d.value; }), d3.max(data, function(d) { return +d.value * 1.1; })]);
//y.domain(d3.extent(data, function(d) { return +d.value; }));

/* Add line into SVG */
var line = d3.line()
  .x(d => x(parseTime(d.day)))
  .y(d => y(+d.value));

let lines = chartarea.append('g')
  .attr('class', 'lines');

function bolddefinition() {
    $("#thresholddefinition").css("font-weight", "bold")
};

function plaindefinition() {
    $("#thresholddefinition").css("font-weight", "normal")
};

// threshold line first
if(options.hasOwnProperty("threshold")) {
    var tline = d3.line()
      .x(d => d.x)
      .y(d => y(+d.y));

    var threshdata = [{
        "site":"Threshold", 
        "line":[{"x":0, "y":options.threshold}, {"x":plotwidth, "y":options.threshold}]
      }];
    lines.selectAll('.line-group-threshold')
      .data(threshdata).enter()
      .append('g')
      .attr('class', 'line-group-threshold')  
      .on("mouseover", function(d, i) {
          chartarea.append("text")
            .attr("class", "title-text")
            .style("fill", "#333")         
            .text("Threshold: " + options.threshold + " " + options.unit)
            .attr("text-anchor", "middle")
            .attr("x", (width-margin.left-margin.right)/2)
            .attr("y", 5);
          bolddefinition();
        })
      .on("mouseout", function(d) {
          chartarea.select(".title-text").remove();
          plaindefinition();
        })
      .append('path')
      .attr('class', 'line-threshold')  
      .attr('d', d => tline(d.line))
      .style('stroke', 'gray')
      .style('opacity', lineOpacity)
      .style('stroke-width', 2)
      .style('fill', 'none')
      .on("mouseover", function(d) {
          d3.selectAll('.line-threshold')
			      .style('opacity', otherLinesOpacityHover);
          d3.selectAll('.circle')
            .style('stroke', '#333')
			      .style('opacity', circleOpacityOnLineHover);
          d3.select(this)
            .style('opacity', lineOpacityHover)
            .style("stroke-width", lineStrokeHover)
	        .style('stroke', '#333')
            .style("cursor", "pointer");
        })
      .on("mouseout", function(d) {
          d3.selectAll(".line-threshold")
			      .style('opacity', lineOpacity);
          d3.selectAll('.circle')
            .style('stroke', 'gray')
	          .style('opacity', circleOpacity);
          d3.select(this)
            .style("stroke-width", lineStroke)
            .style("cursor", "none")
            .style('stroke', 'gray');
        });
};


lines.selectAll('.line-group')
  .data(newdata).enter()
  .append('g')
  .attr('class', 'line-group')  
  .on("mouseover", function(d, i) {
      chartarea.append("text")
        .attr("class", "title-text")
        .style("fill", "#333")   
//          .style("fill", color(d.year))        
        .text("Year: " + d.year)
        .attr("text-anchor", "middle")
        .attr("x", (width-margin.left-margin.right)/2)
        .attr("y", 5);
    })
  .on("mouseout", function(d) {
      chartarea.select(".title-text").remove();
    })
  .append('path')
  .attr('class', 'line')  
  .attr('d', d => line(d.values))
  .style('stroke', (d) => color(d.year))
  .style('opacity', lineOpacity)
  .style('stroke-width', 2)
  .style('fill', 'none')
  .on("mouseover", function(d) {
      d3.selectAll('.line')
			  .style('opacity', otherLinesOpacityHover);
      d3.selectAll('.circle')
        .style('stroke', '#333')
			  .style('opacity', circleOpacityOnLineHover);
      d3.select(this)
        .style('opacity', lineOpacityHover)
        .style("stroke-width", lineStrokeHover)
	    .style('stroke', '#333')
        .style("cursor", "pointer");
    })
  .on("mouseout", function(d) {
      d3.selectAll(".line")
			  .style('opacity', lineOpacity);
      d3.selectAll('.circle')
        .style('stroke', (d) => color(d.year))
	      .style('opacity', circleOpacity);
      d3.select(this)
        .style("stroke-width", lineStroke)
        .style("cursor", "none")
        .style('stroke', (d) => color(d.year));
    });
  
/* Add circles in the line */
lines.selectAll("circle-group")
  .data(newdata).enter()
  .append("g")
  .style("fill", (d) => color(d.year))
  .selectAll("circle")
  .data(d => d.values).enter()
  .append("g")
  .attr("class", "circle")  
  .on("mouseover", function(d) {
      d3.select(this)     
        .style("cursor", "pointer")
        .append("text")
        .style("fill", "#333")
        .attr("class", "text")
        .text(`${d.value}`)
        .attr("x", d => x(parseTime(d.day)) + 5)
        .attr("y", d => y(+d.value) - 10);
    })
  .on("mouseout", function(d) {
      d3.select(this)
        .style("cursor", "none")  
        .selectAll(".text").remove();
    })
  .append("circle")
  .attr("cx", d => x(parseTime(d.day)))
  .attr("cy", d => y(+d.value))
  .attr("r", circleRadius)
  .style('opacity', circleOpacity)
  .on("mouseover", function(d) {
        d3.select(this)
          .attr("r", circleRadiusHover);
      })
    .on("mouseout", function(d) {
        d3.select(this) 
          .attr("r", circleRadius);  
      });
 

// Add the X Axis
chartarea.append("g")
    .attr("class", "axis")
    .attr("transform", "translate(0," + height + ")")
    .call(d3.axisBottom(x)
            .tickFormat(d3.timeFormat("%b %d")))
    .selectAll("text")	
      .style("text-anchor", "end")
      .attr("dx", "-.8em")
      .attr("dy", ".15em")
      .attr("transform", "rotate(-65)");

// Add the Y Axis
chartarea.append("g")
    .attr("class", "axis")
    .call(d3.axisLeft(y));
  
chartarea.selectAll(".axis")	    
      .style("font-size","1.1em");

// y-label
var ylab = chartarea.append("g")
  .attr("class", "ylab")
  .attr("transform", "translate(-70," + (height-margin.top)/2 + ") rotate(-90)");

ylab.append("text")
  .style("text-anchor", "middle")
  .text(options.ylab);
  
// legend
var legdata = [];
allYears.forEach(function(d, i) {
  legdata.push({
    "year":d, 
    "line":[{"x":20, "y":i*20+20}, {"x":70, "y":i*20+20}], 
    "point":[{"x":45, "y":i*20+20}]
  });
});

var legendline = d3.line()
  .x(d => d.x)
  .y(d => d.y);


var legendarea = svg.append("g")
    .attr("class", "legend-area")
    .attr("font-size", "1.1em")
    .attr("transform",
          "translate(" + (margin.left + plotwidth) + "," + margin.top + ")");

let legendlines = chartarea.append('g')
  .attr('class', 'lines');

/* Add lines to the legend */
legendarea.selectAll('.legendline-group')
  .data(legdata).enter()
  .append('g')
    .attr('class', 'legendline-group')  
    .append('path')
      .attr('class', 'line')  
      .attr('d', d => legendline(d.line) )
      .style('stroke', (d) => color(d.year) )
      .style('opacity', lineOpacity)
      .style('stroke-width', 2)
      .style('fill', 'none');
      
legendarea.selectAll('.legendline-group')
  .append("text")
    .text(d => d.year)
    .attr("x", d => d.line[1].x + 15)
    .attr("y", d => d.line[1].y + 5);
  
  
/* Add circles in the line */
legendarea.selectAll("circle-group")
  .data(legdata).enter()
  .append("g")
    .style("fill", (d) => color(d.year))
    .selectAll("circle")
    .data(d => d.point).enter()
    .append("g")
      .attr("class", "circle")  
      .append("circle")
        .attr("cx", d => d.x)
        .attr("cy", d => d.y)
        .attr("r", circleRadius)
        .style('opacity', circleOpacity);


// threshold line last
if(options.hasOwnProperty("threshold")) {
    var tdata = [{
        "site":"Threshold: " + options.threshold, 
        "line":[{"x":20, "y":allYears.length*20+20}, {"x":70, "y":allYears.length*20+20}]
    }];
    legendarea.selectAll('.legendline-group-threshold')
    .data(tdata).enter()
      .append('g')
        .attr('class', 'legendline-group-threshold')  
        .append('path')
          .attr('class', 'line')  
          .attr('d', d => legendline(d.line) )
          .style('stroke', 'gray')
          .style('opacity', lineOpacity)
          .style('stroke-width', 2)
          .style('fill', 'none');
          
    legendarea.selectAll('.legendline-group-threshold')
      .append("text")
        .text(d => d.site)
        .attr("x", d => d.line[1].x + 15)
        .attr("y", d => d.line[1].y + 5);
}



var filename = options.loc + "_" + options.metric + "_yearplot.png";

// SAVE AS PNG
// FROM http://bl.ocks.org/Rokotyan/0556f8facbaf344507cdc45dc3622177
 // Set-up the export button
d3.select('#downloadd3chart').on('click', function(){
//    Shiny.setInputValue("chartsaveaspng", "true", {priority: "event"});
	var svgString = getSVGString(svg.node());
	svgString2Image( svgString, 2*width, 3*height, 'png', save ); // passes Blob and filesize String to the callback

	function save( dataBlob, filesize ){
		saveAs( dataBlob, filename ); // FileSaver.js function
	}
});

// Below are the functions that handle actual exporting:
// getSVGString ( svgNode ) and svgString2Image( svgString, width, height, format, callback )
function getSVGString( svgNode ) {
	svgNode.setAttribute('xlink', 'http://www.w3.org/1999/xlink');
	var cssStyleText = getCSSStyles( svgNode );
	appendCSS( cssStyleText, svgNode );

	var serializer = new XMLSerializer();
	var svgString = serializer.serializeToString(svgNode);
	svgString = svgString.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink='); // Fix root xlink without namespace
	svgString = svgString.replace(/NS\d+:href/g, 'xlink:href'); // Safari NS namespace fix

	return svgString;

	function getCSSStyles( parentElement ) {
		var selectorTextArr = [];

		// Add Parent element Id and Classes to the list
		selectorTextArr.push( '#'+parentElement.id );
		for (var c = 0; c < parentElement.classList.length; c++)
				if ( !contains('.'+parentElement.classList[c], selectorTextArr) )
					selectorTextArr.push( '.'+parentElement.classList[c] );

		// Add Children element Ids and Classes to the list
		var nodes = parentElement.getElementsByTagName("*");
		for (var i = 0; i < nodes.length; i++) {
			var id = nodes[i].id;
			if ( !contains('#'+id, selectorTextArr) )
				selectorTextArr.push( '#'+id );

			var classes = nodes[i].classList;
			for (var c = 0; c < classes.length; c++)
				if ( !contains('.'+classes[c], selectorTextArr) )
					selectorTextArr.push( '.'+classes[c] );
		}

		// Extract CSS Rules
		var extractedCSSText = "";
		for (var i = 0; i < document.styleSheets.length; i++) {
			var s = document.styleSheets[i];
			
			try {
			    if(!s.cssRules) continue;
			} catch( e ) {
		    		if(e.name !== 'SecurityError') throw e; // for Firefox
		    		continue;
		    	}

			var cssRules = s.cssRules;
			for (var r = 0; r < cssRules.length; r++) {
				if ( contains( cssRules[r].selectorText, selectorTextArr ) )
					extractedCSSText += cssRules[r].cssText;
			}
		}
		

		return extractedCSSText;

		function contains(str,arr) {
			return arr.indexOf( str ) === -1 ? false : true;
		}

	}

	function appendCSS( cssText, element ) {
		var styleElement = document.createElement("style");
		styleElement.setAttribute("type","text/css"); 
		styleElement.innerHTML = cssText;
		var refNode = element.hasChildNodes() ? element.children[0] : null;
		element.insertBefore( styleElement, refNode );
	}
}


function svgString2Image( svgString, width, height, format, callback ) {
	var format = format ? format : 'png';

	var imgsrc = 'data:image/svg+xml;base64,'+ btoa( unescape( encodeURIComponent( svgString ) ) ); // Convert SVG string to data URL

	var canvas = document.createElement("canvas");
	var context = canvas.getContext("2d");

	canvas.width = width;
	canvas.height = height;

	var image = new Image();
	image.onload = function() {
		context.clearRect ( 0, 0, width, height );
		context.drawImage(image, 0, 0, width, height);

		canvas.toBlob( function(blob) {
			var filesize = Math.round( blob.length/1024 ) + ' KB';
			if ( callback ) callback( blob, filesize );
		});

		
	};

	image.src = imgsrc;
}





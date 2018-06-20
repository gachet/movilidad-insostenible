//preproceso
/*d3.dsv(',' , '../residencia_trabajo/data/locations.csv',
function(d) {
    return d;
  }).then(function(data) {
      console.log(data)

  }); */
window.onload = function() {


  loadData();
  initCanvas();
}

names = ["locations", "routes"]
files = ["../residencia_trabajo/data/locations.csv", "../residencia_trabajo/data/routes.csv"]
separators = [",", ","]
globalData = {}

function loadData() {
  d3.dsv(separators[0], files[0], function(d) {
    return d
  }).then(function(data) {
    globalData.locations = data;

  });

  d3.dsv(separators[1], files[1], function(d) {
    d.workers_count=parseInt(d.workers_count);
    d.duration=parseInt(d.duration);
    d.distance=parseInt(d.distance);
    return d;
  }).then(function(data) {
    globalData.routes = data;
    nestedRoutes = d3.nest()
      .key(function(d) {
        return d.origin;
      })
    /*  .key(function(d) {
        return d.destination;
      })
      .key(function(d) {
        return d.mode;
      })*/
      //.sortKeys(function(a,b) { return parseInt(a.workers_count) - parseFloat(b.workers_count); })
      .object(data);
    poblaciones = Object.keys(nestedRoutes)
    createForms()
    initViz("Alcobendas")
  });

}

function createForms() {
  var select = d3.select('#forms')
    .append('select')
    .attr('class', 'select')
    .on('change', onchange)

  var options = select
    .selectAll('option')
    .data(poblaciones).enter()
    .append('option')
    .text(function(d) {
      return d;
    });

  function onchange() {
    selectValue = d3.select('select').property('value')
    //aqui genero visualizacion
    initViz(selectValue)
    d3.select('body')
      .append('p')
      .text(selectValue + ' is the last selected option.')
  };
}


function initViz(selectedCityName) {
  svg.selectAll("*").remove();
   dataCity = nestedRoutes[selectedCityName]
   //dataTransport=dataCity.filter(function(d){return d.mode=="bicycling"})
   dataTransport=dataCity
     .filter(function(d){return d.mode === "bicycling"})
    // .filter(function(d){return ! (d.destination === selectedCityName) })
     .sort(function(a,b) { return parseInt(b.workers_count) - parseFloat(a.workers_count); })
     .slice(0,20)

    createPolarLayout(dataTransport);

  var groupEnter =
    svg.selectAll("g.city")
    .data(dataTransport)
    .enter()
    .append("g")
    .attr("transform", function(d){ return "translate(" + d.x+"," +d.y +")"})
    .attr("class", ()=> {return "city"})


  circlesEnter = groupEnter
    .append("circle")
    // .attr("cx", function(d) {    //   return d.x;    // })    // .attr("cy", function (d) { return d.x; })
    .attr("r", function(d) {
      return d.r
    })
    .style("fill", function(d) {
      //return "#333";
      return d3.interpolateOrRd(d.Magnitude)
    });
    groupEnter.append("text")
    .attr("dx", function(d){return 5})
    .text((d)=> {return d.destination})

    svg.append("circle")
    .style("fill","transparent")
    .attr("stroke","black")
    .attr('stroke-width', 1)
    .attr("r",distanceRadius(30*60))

    svg.append("circle")
    .style("fill","transparent")
    .attr("stroke","black")
    .attr('stroke-width', 1)
    .attr("r",distanceRadius(60*60))

}



function createPolarLayout(dataset){
  var dataSetCapado=dataset.filter((d)=>{return ! (d.origin===d.destination) })
  var maxTime=d3.max(dataSetCapado,(d)=>{return parseInt(d.duration) })
  var minTime=d3.min(dataSetCapado,(d)=>{return parseInt(d.duration) })
   maxWorkers=d3.max(dataSetCapado,(d)=>{return parseInt(d.workers_count) })
   minWorkers=d3.min(dataSetCapado,(d)=>{return parseInt(d.workers_count) })

  var maxRadius=500
   distanceRadius= d3.scaleLinear()
    .domain([minTime, maxTime])
    .range([0, maxRadius])

    var circleRadius= d3.scaleLinear()
      .domain([minWorkers, maxWorkers])
      .range([4, 40])


  var step=(2*Math.PI)/dataset.length
  dataset.forEach(function(dstCity,indx,arr){

    if( dstCity.origin === dstCity.destination) {
      dstCity.x =dstCity.xy=0;
      dstCity.r=4;
      dstCity.Magnitude=1;
    }
    else{
      dstCity.x = distanceRadius(dstCity.duration)*Math.sin(indx*step)
      dstCity.y = distanceRadius(dstCity.duration)*Math.cos(indx*step)
      dstCity.r=circleRadius(dstCity.workers_count)
      dstCity.Magnitude=(dstCity.workers_count/maxWorkers)
    }
  })
}


function initCanvas() {
  available_width = $(window).width();
  available_height = $(window).height(); //-$('nav.navbar').height()
  circle_margin = 0;

  w = available_width - 100;
  h = available_height - 100;
  min_dimension = Math.min(available_width, available_height);
  radius = min_dimension/2;
  rx = radius;
  ry = radius;

  if ($('#canvas-div')[0] == undefined) {
    var div = d3.select("#graph").insert("div", "#extra")
      // .style("top", "0px")
      .style("width", w + "px")
      .style("height", h + "px") //TODO
      //.style("position", "absolute")
      //.style("-webkit-backface-visibility", "hidden")
      .style("margin-left", circle_margin + "px")
      .attr("id", 'canvas-div');

    svg0 = div.append("svg:svg")
      .attr("width", w)
      .attr("height", h);
    svg = svg0.append("svg:g")
      .attr("transform", "translate(" + rx + "," + ry + ")")
      .classed("svgcanvas", true)
  }

}

// Constant values
const opacity = 0.8;

let travelTime = 30 * 60; 

const transportModes = {
	'walking': 'Caminando',
	'bicycling': 'Bicicleta',
	'transit': 'Transporte Público',
	'driving': 'Coche',
	'no mode': 'No hay modo'
}


// Width and height (for the three charts)
const contWidth = d3.select('#chart').node().clientWidth;

const margin = { top: 30, right: 50, bottom: 50, left: 180 },
		width = contWidth - margin.left - margin.right,
		height = contWidth / 1.2 - margin.top - margin.bottom;	

// Append svg
const svg = d3.select('#chart')
	.append('svg')
		.attr('width', width + margin.left + margin.right)
		.attr('height', height + margin.top + margin.bottom)
	.append('g')
		.attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

// Scales
const xScale = d3.scaleLinear()
	.range([0, width]);

const yScale = d3.scaleBand().padding(0)
	.range([0, height]);

const yDensity = d3.scaleLinear();

const cScale = d3.scaleOrdinal()
	.range(['#1a9641', '#a6d96a', '#fdae61', '#d7191c', '#cccccc']);

const violin = d3.area()
	.curve(d3.curveBasis)
  .x(d => xScale(d[0]))
  .y0(d => {return yDensity(d[1]/2)})
  .y1(d => yDensity(-d[1]/2));

// Formatters
const formatTime = d3.timeFormat("%H:%M"),
    	formatMinutes = function(d) { return formatTime(new Date(2012, 0, 1, 0, 0, d)); };


// Load the data
d3.csv('../data/routes_d3.csv', function(error, mydata) {
	if (error) throw error;

	// ------ Data & scales------ // 
	mydata.forEach(d => {
		d.workers = +d.workers;
		d.distance = +d.distance;
		d.duration = +d.duration;
	})

	mydata = mydata.filter(d => d.origin != d.destination);

	mydata = bestMode(mydata, travelTime);
	xScale.domain(d3.extent(mydata, d => d.duration));
	

	const thresholds = d3.range(0, d3.max(mydata, d => d.duration), 180),
				total_workers = d3.sum(mydata, d => d.workers);

	const nestedData = d3.nest()
	  .key(d => d.mode_b)
	  .entries(mydata)
	  // .filter(d => d.key == 'no mode')
	  .map(d => {
	  	const serie_w = d.values.length / bestMode(mydata, travelTime).length;
		 	d.density = kernelDensityEstimator(kernelEpanechnikov(7), thresholds, total_workers)(d.values)
	  		.filter(v => d.key != 'no mode' ? v[0] <= travelTime : v[0] >= travelTime);

	  	return d;
	  });
	
	

	yScale.domain(Object.keys(transportModes));
	cScale.domain(Object.keys(transportModes));
	
	maxDensity = d3.max(nestedData, d => d3.max(d.density, v => v[1]))
	yDensity
  	.range([yScale.bandwidth(), 0])
  	.domain([-maxDensity / 2, maxDensity/2]);
	


	

	// ------ Axis ------ //

	const xAxis = d3.axisBottom(xScale)
		.tickFormat(formatMinutes)
		.tickValues([0, 600, 1200, 1800, 3600, 5400]);
		

	
	svg.append('g')
		.attr('class', 'x axis')
		.attr('transform', `translate(0, ${height})`)
		.call(xAxis);

	const yAxis = d3.axisLeft(yScale)
		.tickSizeInner(-width)
		.tickPadding(20);
	
	svg.append('g')
		.attr('class', 'y axis')
		.call(yAxis);

	// Delete path
	svg.selectAll('.domain').remove();
	svg.select('.y.axis').selectAll('line').attr('stroke', '#ccc')


	
	// ------ render ------ // 

	var modes = svg.selectAll('g.mode')
		.data(nestedData)
		.enter()
		.append('g')
		.attr('class', d => `mode ${d.key}`)
		.attr('transform', d => `translate(0, ${yScale(d.key)})`);

	modes.selectAll('path')
		.data(d => [d])
		.enter()
		.append('path')
		.attr('d', d => violin(d.density))
		.attr('fill', d => cScale(d.key))

	svg.append('line')
	  .attr('x1', xScale(travelTime))
	  .attr('y1', 0)
	  .attr('x2', xScale(travelTime))
	  .attr('y2', height)
	  .style('stroke', '#000')

});

function bestMode (data, travelTime) {
	return d3.nest()
  	.key(d => d.route)
  	.entries(data)
  	.map((d, i) => {
			v = d.values.filter(v => +v.duration <= travelTime)
			
			if (v.length == 1) {
				v = v[0];
				v.mode_b = v.mode;
			} else if (v.length == 0) {
				v = d.values.find(v => v.mode == 'driving')
				v.mode_b = 'no mode';
			} else {
				v = v.sort((a, b) => Object.keys(transportModes).indexOf(a.mode) - Object.keys(transportModes).indexOf(b.mode))[0];
				v.mode_b = v.mode;
			}
			return v;
			
		}).filter(d => d !== undefined)
}



function kernelDensityEstimator(kernel, X, total_workers) {
  return function(V) {
    return X.map(function(x) {
    	// X = thresholds array
    	// V = durations array
      return [x, d3.sum(V, function(v) { 
      	return kernel(x - v.duration) * v.workers; }) / total_workers];
    });
    
  };
}

function kernelEpanechnikov(k) {
  return function(v) {
    return Math.abs(v /= k) <= 1 ? 0.75 * (1 - v * v) / k : 0;
  };
}




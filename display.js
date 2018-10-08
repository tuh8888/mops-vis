/*
Copyright (c) 2013-2018 Ross Kirsling

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// set up SVG for D3
const width = 960;
const height = 500;
const colors = d3.scaleOrdinal(d3.schemeCategory10);

const svg = d3.select('body')
    .append('svg')
    .attr('oncontextmenu', 'return false;')
    .attr('width', width)
    .attr('height', height);

// set up initial nodes and links
//  - nodes are known by 'id', not by index in array.
//  - reflexive edges are indicated on the node (as a bold black circle).
//  - links are always source < target; edge directions are set by 'left' and 'right'.
// const nodes = [
//     { id: 0, reflexive: false },
//     { id: 1, reflexive: true },
//     { id: 2, reflexive: false }
// ];
// const links = [
//     { source: nodes[0], target: nodes[1], left: false, right: true },
//     { source: nodes[1], target: nodes[2], left: false, right: true }
// ];

let nodes = [];
let links = [];

function addGraphData(graphData) {
    console.log("graph received");
    nodes = nodes.concat(graphData.nodes);
    links = links.concat(graphData.links);
    console.log(graphData)
}

function getInitialGraph() {
    console.log("graph requested");
    smackjack.getInitialGraph(addGraphData);
    restart();
}

//TODO Either send nodes that are known or process what is rerturned
function getNode(nodeId) {
    smackjack.getNode(nodeId, addGraphData);
    restart()
}

// init D3 force layout
const force = d3.forceSimulation()
    .force('link', d3.forceLink().id((d) => d.id).distance(150))
    .force('charge', d3.forceManyBody().strength(-500))
    .force('x', d3.forceX(width / 2))
    .force('y', d3.forceY(height / 2))
    .on('tick', tick);

// init D3 drag support
const drag = d3.drag()
    .on('start', (d) => {
        if (!d3.event.active) force.alphaTarget(0.3).restart();

        d.fx = d.x;
        d.fy = d.y;
    })
    .on('drag', (d) => {
        d.fx = d3.event.x;
        d.fy = d3.event.y;
    })
    .on('end', (d) => {
        if (!d3.event.active) force.alphaTarget(0);

        d.fx = null;
        d.fy = null;
    });

// define arrow markers for graph links
svg.append('svg:defs').append('svg:marker')
    .attr('id', 'end-arrow')
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 6)
    .attr('markerWidth', 3)
    .attr('markerHeight', 3)
    .attr('orient', 'auto')
    .append('svg:path')
    .attr('d', 'M0,-5L10,0L0,5')
    .attr('fill', '#000');

svg.append('svg:defs').append('svg:marker')
    .attr('id', 'start-arrow')
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 4)
    .attr('markerWidth', 3)
    .attr('markerHeight', 3)
    .attr('orient', 'auto')
    .append('svg:path')
    .attr('d', 'M10,-5L0,0L10,5')
    .attr('fill', '#000');

// handles to link and node element groups
let path = svg.append('svg:g').selectAll('path');
let circle = svg.append('svg:g').selectAll('g');

// mouse event vars
let selectedNode = null;
let selectedLink = null;
let mousedownLink = null;
let mousedownNode = null;
let mouseupNode = null;


function resetMouseVars() {
    mousedownNode = null;
    mouseupNode = null;
    mousedownLink = null;
}

// update force layout (called automatically each iteration)
function tick() {
    // draw directed edges with proper padding from node centers
    path.attr('d', (d) => {
        const deltaX = d.target.x - d.source.x;
        const deltaY = d.target.y - d.source.y;
        const dist = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
        const normX = deltaX / dist;
        const normY = deltaY / dist;
        const sourcePadding = d.left ? 17 : 12;
        const targetPadding = d.right ? 17 : 12;
        const sourceX = d.source.x + (sourcePadding * normX);
        const sourceY = d.source.y + (sourcePadding * normY);
        const targetX = d.target.x - (targetPadding * normX);
        const targetY = d.target.y - (targetPadding * normY);

        return `M${sourceX},${sourceY}L${targetX},${targetY}`;
    });

    circle.attr('transform', (d) => `translate(${d.x},${d.y})`);
}

// update graph (called when needed)
function restart() {
    if (selectedNode) {
        getNode(selectedNode.id)
    }

    circle.call(drag);

    // path (link) group
    path = path.data(links);

    // update existing links
    path.classed('selected', (d) => d === selectedLink)
        .style('marker-start', (d) => d.left ? 'url(#start-arrow)' : '')
        .style('marker-end', (d) => d.right ? 'url(#end-arrow)' : '');

    // remove old links
    path.exit().remove();

    // add new links
    path = path.enter().append('svg:path')
        .attr('class', 'link')
        .classed('selected', (d) => d === selectedLink)
        .style('marker-start', (d) => d.left ? 'url(#start-arrow)' : '')
        .style('marker-end', (d) => d.right ? 'url(#end-arrow)' : '')
        .on('mousedown', (d) => {
            if (d3.event.ctrlKey) return;

            // select link
            mousedownLink = d;
            selectedLink = (mousedownLink === selectedLink) ? null : mousedownLink;
            selectedNode = null;
            restart();
        })
        .merge(path);

    // circle (node) group
    // NB: the function arg is crucial here! nodes are known by id, not by index!
    circle = circle.data(nodes, (d) => d.id);

    // update existing nodes (reflexive & selected visual states)
    circle.selectAll('circle')
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .classed('reflexive', (d) => d.reflexive);

    // remove old nodes
    circle.exit().remove();

    // add new nodes
    const g = circle.enter().append('svg:g');

    g.append('svg:circle')
        .attr('class', 'node')
        .attr('r', 12)
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .style('stroke', (d) => d3.rgb(colors(d.id)).darker().toString())
        .classed('reflexive', (d) => d.reflexive)
        .on('mouseover', function (d) {
            if (!mousedownNode || d === mousedownNode) return;
            // enlarge target node
            d3.select(this).attr('transform', 'scale(1.1)');
        })
        .on('mouseout', function (d) {
            if (!mousedownNode || d === mousedownNode) return;
            // unenlarge target node
            d3.select(this).attr('transform', '');
        })
        .on('mousedown', (d) => {
            if (d3.event.ctrlKey) return;

            // select node
            mousedownNode = d;
            selectedNode = (mousedownNode === selectedNode) ? null : mousedownNode;
            selectedLink = null;

            restart();
        })
        .on('mouseup', function (d) {
            if (!mousedownNode) return;

            // check for drag-to-self
            mouseupNode = d;
            if (mouseupNode === mousedownNode) {
                resetMouseVars();
                return;
            }

            // unenlarge target node
            d3.select(this).attr('transform', '');

            restart();
        });

    // show node IDs
    g.append('svg:text')
        .attr('x', 0)
        .attr('y', 4)
        .attr('class', 'id')
        .text((d) => d.id);

    circle = g.merge(circle);

    // set the graph in motion
    force
        .nodes(nodes)
        .force('link').links(links);

    force.alphaTarget(0.3).restart();
}

function mousedown() {
    // because :active only works in WebKit?
    svg.classed('active', true);

    if (d3.event.ctrlKey || mousedownNode || mousedownLink) return;

    restart();
}

function mousemove() {
    if (!mousedownNode) return;
    restart();
}

function mouseup() {

    // because :active only works in WebKit?
    svg.classed('active', false);

    // clear mouse event vars
    resetMouseVars();
}

// app starts here
svg.on('mousedown', mousedown)
    .on('mousemove', mousemove)
    .on('mouseup', mouseup);
restart();
getInitialGraph();

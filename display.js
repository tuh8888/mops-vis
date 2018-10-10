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
let fullGraph = {};
let smackjackExists = false;

// Make some extensions to Array to prevent adding if items are already in the graph
Array.prototype.inArray = function (comparer) {
    for (let i = 0; i < this.length; i++) {
        if (comparer(this[i])) return true;
    }
    return false;
};
Array.prototype.pushIfDoesNotExist = function (element, comparer) {
    if (!this.inArray(comparer)) {
        this.push(element);
    }
};

function checkIfSmackjackExists() {
    const request = new XMLHttpRequest();
    request.open('GET', "http://" + location.hostname + ":" + location.port + "/ajax-process/GET-INITIAL-GRAPH", true);
    request.onreadystatechange = function () {
        if (request.readyState === 4) {
            smackjackExists = request.status !== 404;
        }
    };
    request.send();
}

function addGraphData(newGraphData) {
    console.log("graph received");
    if (newGraphData.nodes && newGraphData.links) {
        let newNode;
        for (let i = 0; i < newGraphData.nodes.length; i++) {
            newNode = newGraphData.nodes[i];
            nodes.pushIfDoesNotExist(newNode, function (e) {
                return e.id === newNode.id;
            })
        }
        let newLink;
        for (let i = 0; i < newGraphData.links.length; i++) {
            newLink = newGraphData.links[i];
            newLink.source = typeof newLink.source === 'string' ? nodes.find(node => node.id === newLink.source) : newLink.source;
            newLink.target = typeof newLink.target === 'string' ? nodes.find(node => node.id === newLink.target) : newLink.target;
            links.pushIfDoesNotExist(newLink, function (e) {
                return (e.source === newLink.source && e.target === newLink.target) || (e.source.id === newLink.source && e.target.id === newLink.target);
            })
        }
        console.log(newGraphData);
        restart();
    }
}

function getInitialGraph() {
    console.log("Graph requested");
    checkIfSmackjackExists();
    setTimeout(function () {
        if (smackjackExists) {
            console.log("Using server data");
            smackjack.getInitialGraph(addGraphData);
        } else {
            console.log("Using default data");
            nodes = [
                {"id": "1"},
                {"id": "2"},
                {"id": "3"},
                {"id": "4"},
                {"id": "5"}
            ];
            links = [
                {"source": "1", "target": "2", "label": "A"},
                {"source": "1", "target": "3", "label": "B"},
                {"source": "2", "target": "4", "label": "C"},
                {"source": "2", "target": "5", "label": "D"},
                {"source": "5", "target": "1", "label": "E"}
            ];
            fullGraph = {
                "nodes": [
                    {"id": "1"},
                    {"id": "2"},
                    {"id": "3"},
                    {"id": "4"},
                    {"id": "5"},
                    {"id": "6"},
                    {"id": "7"}
                ],
                "links": [
                    {"source": "1", "target": "2", "label": "A"},
                    {"source": "1", "target": "3", "label": "B"},
                    {"source": "2", "target": "4", "label": "C"},
                    {"source": "2", "target": "5", "label": "D"},
                    {"source": "5", "target": "1", "label": "E"},
                    {"source": "5", "target": "6", "label": "F"},
                    {"source": "6", "target": "7", "label": "G"}
                ]
            }
        }
        restart();
    }, 2000)

}

//TODO Either send nodes that are known or process what is rerturned
function getNode(nodeId) {
    if (smackjackExists) {
        smackjack.getNode(nodeId, addGraphData);
    } else {
        const linksToReturn = fullGraph.links.filter(link => link.source === nodeId || link.target === nodeId || link.target.id === nodeId || link.source.id === nodeId);

        class Node {
            constructor(id) {
                this.id = id;
            }
        }

        const nodesToReturn = [];
        for (let i = 0; i < linksToReturn.length; i++) {
            let link = linksToReturn[i];
            nodesToReturn.push(typeof link.source === 'string' ? new Node(link.source) : link.source);
            nodesToReturn.push(typeof link.target === 'string' ? new Node(link.target) : link.target);
        }

        const graphToReturn = {
            "links": linksToReturn,
            "nodes": nodesToReturn
        };
        addGraphData(graphToReturn);
    }
}

function removeNode(nodeId) {
    nodes = nodes.filter(node => node.id !== nodeId);
    links = links.filter(link => link.source.id !== nodeId && link.target.id !== nodeId);
    restart();
}

function searchNode() {
    const idToSearchFor = document.getElementById("search-text-field").value;
    console.log(idToSearchFor);
    getNode(idToSearchFor);
    restart();
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

svg.append("defs").append("path")
    .attr("id", "textPath")
    .attr("d", 'M10,-5L0,0L10,5');

// handles to link and node element groups
let path = svg.append('svg:g').selectAll('path');
let node = svg.append('svg:g').selectAll('g.node');

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

    node.attr('transform', (d) => `translate(${d.x},${d.y})`);
}

// update graph (called when needed)
function restart() {
    // path (link) group
    path = path.data(links);

    // update existing links
    path.classed('selected', (d) => d === selectedLink)
        .style('marker-start', '')
        .style('marker-end', 'url(#end-arrow)');

    // remove old links
    path.exit().remove();

    // add new links
    const gLink = path.enter().append('svg:g')
        .attr('class', 'link');

    const paths = gLink.append('path')
        // .attr('class', 'link')
        .classed('selected', (d) => d === selectedLink)
        .style('marker-start', '')
        .attr('marker-end','url(#end-arrow)')
        .on('mousedown', (d) => {
            if (d3.event.ctrlKey) return;

            // select link
            mousedownLink = d;
            selectedLink = (mousedownLink === selectedLink) ? null : mousedownLink;
            selectedNode = null;
            restart();
        });

    //TODO: figure out why paths aren't showing
    gLink.append("text").append("textPath")
        .attr("xlink:href", "#textPath")
        .text((d) => d.id);

    path = paths.merge(path);



    // circle (node) group
    // NB: the function arg is crucial here! nodes are known by id, not by index!
    node = node.data(nodes, (d) => d.id);

    // update existing nodes (reflexive & selected visual states)
    node.selectAll('circle')
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .classed('reflexive', (d) => d.reflexive)
        .classed('selected', (d) => d === selectedNode);

    node.selectAll('text')
        .classed('display', (d) => d.display);

    // remove old nodes
    node.exit().remove();

    // add new nodes
    const g = node.enter().append('svg:g')
        .attr('class', 'node');

    g.append('svg:circle')
        .attr('r', 12)
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .style('stroke', (d) => d3.rgb(colors(d.id)).darker().toString())
        .classed('reflexive', (d) => d.reflexive)
        .on('mouseover', function (d) {
            // if (!mousedownNode || d === mousedownNode) return;
            // // enlarge target node
            // d3.select(this).attr('transform', 'scale(1.1)');
        })
        .on('mouseout', function (d) {
            // if (!mousedownNode || d === mousedownNode) return;
            // // unenlarge target node
            // d3.select(this).attr('transform', '');
        })
        .on('mousedown', (d) => {
            if (!d3.event.ctrlKey) getNode(d.id);

            // select node
            mousedownNode = d;
            d.display = !d.display;
            selectedNode = (mousedownNode === selectedNode) ? null : mousedownNode;

            selectedLink = null;

            restart();
        })
        .on('mouseup', function (d) {
            if (!mousedownNode) return;

            // check for drag-to-self
            mouseupNode = d;
            if (mouseupNode === mousedownNode) {
                if (mouseupNode) {
                    getNode(mouseupNode.id);
                }
                resetMouseVars();
                return;
            }

            // unenlarge target node
            d3.select(this).attr('transform', '');

            restart();
        });

    // show node IDs
    g.append('text')
        .attr('x', 0)
        .attr('y', 4)
        .text((d) => {
            return d.id;
        });

    node = g.merge(node);

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
d3.select(window)
    .on('keydown', keydown)
    .on('keyup', keyup);


let lastKeyDown = -1;
function keydown() {
    d3.event.preventDefault();

    if (lastKeyDown !== -1) return;
    lastKeyDown = d3.event.key;
    if (d3.event.key === "Delete") {
        if (selectedNode) {
            removeNode(selectedNode.id)
        } else if (selectedLink) {
            // TODO: Delete links
            // links.splice(links.indexOf(selectedLink), 1);
        }
    }
    if (d3.event.ctrlKey) {
        node.call(drag);
        svg.classed('ctrl', true)
    }
}


function keyup() {
    lastKeyDown = -1;

    // ctrl
    if (d3.event.key === "Control") {
        node.on('.drag', null);
        svg.classed('ctrl', false);
    }
}

getInitialGraph();



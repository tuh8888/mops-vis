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

/*
author: Harrison Pielke-Lombardo
 */

let selectedNode;
let selectedLink;
let mousedownLink;
let mousedownNode;
let mouseupNode;
let lastKeyDown;

let path, node, graph;
let svg, colors;
let force, drag;

/**
 * @global
 * @param layoutConfig          Parameters about the initial state of the force layout and display area.
 * @param layoutConfig.width:int   Width.
 * @param layoutConfig.height:int
 * @param layoutConfig.link:object
 * @param layoutConfig.link.func:string
 * @param layoutConfig.link.distance:int
 * @param layoutConfig.charge:object
 * @param layoutConfig.charge.func:string
 * @param layoutConfig.charge.strength:int
 * @param layoutConfig.x:object
 * @param layoutConfig.x.func:string
 * @param layoutConfig.x.scale:double
 * @param layoutConfig.y:object
 * @param layoutConfig.y.func:string
 * @param layoutConfig.y.scale:double
 * @param layoutConfig.alphaTarget.hot:string   Hottest state of layout
 * @param layoutConfig.alphaTarget.cool:string   Coldest state of layout
 */
let layoutConfig;

function setupInteractionVars() {
    // mouse event vars
    selectedNode = null;
    selectedLink = null;
    mousedownLink = null;
    mousedownNode = null;
    mouseupNode = null;
    lastKeyDown = -1;
}

function resetMouseVars() {
    mousedownNode = null;
    mouseupNode = null;
    mousedownLink = null;
}

function setupForceLayout() {
    // update force layout (called automatically each iteration)
    const tick = function () {
        // draw directed edges with proper padding from node centers
        path.attr('d', (d) => {
            const deltaX = d.target.x - d.source.x;
            const deltaY = d.target.y - d.source.y;
            const dist = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
            const normX = deltaX / dist;
            const normY = deltaY / dist;
            const sourcePadding = 12;
            const targetPadding = 12;
            const sourceX = d.source.x + (sourcePadding * normX);
            const sourceY = d.source.y + (sourcePadding * normY);
            const targetX = d.target.x - (targetPadding * normX);
            const targetY = d.target.y - (targetPadding * normY);

            return `M${sourceX},${sourceY}L${targetX},${targetY}`;
        });

        node.attr('transform', (d) => `translate(${d.x},${d.y})`);
    };

    // init D3 force layout
    return d3.forceSimulation()
        .force('link', d3.forceLink().id((d) => d.id).distance(layoutConfig.link.distance))
        .force('charge', d3.forceManyBody().strength(layoutConfig.charge.strength))
        .force('x', d3.forceX(layoutConfig.width * layoutConfig.x.scale))
        .force('y', d3.forceY(layoutConfig.height * layoutConfig.y.scale))
        .on('tick', tick);
}

function setupDrag() {
    // init D3 drag support
    return d3.drag()
        .on('start', (d) => {
            if (!d3.event.active) force.alphaTarget(layoutConfig.alphaTarget.hot).restart();
            d.fx = d.x;
            d.fy = d.y;
        })
        .on('drag', (d) => {
            d.fx = d3.event.x;
            d.fy = d3.event.y;
        })
        .on('end', (d) => {
            if (!d3.event.active) force.alphaTarget(layoutConfig.alphaTarget.cool);
            d.fx = null;
            d.fy = null;
        });
}

function setupSVG() {
    const svg = d3.select('body')
        .append('svg')
        .attr('oncontextmenu', 'return false;')
        .attr('width', layoutConfig.width)
        .attr('height', layoutConfig.height);

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

    function keydown() {
        d3.event.preventDefault();

        if (lastKeyDown !== -1) return;
        lastKeyDown = d3.event.key;
        if (d3.event.key === "Delete") {
            if (selectedNode) {
                graph.removeNode(selectedNode.id)
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

    svg.on('mousedown', mousedown)
        .on('mousemove', mousemove)
        .on('mouseup', mouseup);
    d3.select(window)
        .on('keydown', keydown)
        .on('keyup', keyup);

    return svg;
}

function updateLinks() {
    // path (link) group
    path = path.data(graph.links);

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
        .attr('marker-end', 'url(#end-arrow)')
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
}

function updateNodes() {
    // circle (node) group
    // NB: the function arg is crucial here! nodes are known by id, not by index!
    node = node.data(graph.nodes, (d) => d.id);

    // update existing nodes (reflexive & selected visual states)
    node.selectAll('circle')
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .classed('selected', (d) => d === selectedNode);

    node.selectAll('text')
        .classed('display', (d) => d.display);

    // remove old nodes
    node.exit().remove();

    // add new nodes
    const g = node.enter().append('svg:g')
        .attr('class', 'node');

    function mouseout(d) {
        // if (!mousedownNode || d === mousedownNode) return;
        // // unenlarge target node
        // d3.select(this).attr('transform', '');
    }

    function mousedown(d) {
        if (!d3.event.ctrlKey) graph.getNode(d.id);

        // select node
        mousedownNode = d;
        d.display = !d.display;
        selectedNode = (mousedownNode === selectedNode) ? null : mousedownNode;

        selectedLink = null;

        restart();
    }

    function mouseup(d) {
        if (!mousedownNode) return;

        // check for drag-to-self
        mouseupNode = d;
        if (mouseupNode === mousedownNode) {
            if (mouseupNode) {
                graph.getNode(mouseupNode.id);
            }
            resetMouseVars();
            return;
        }

        // unenlarge target node
        d3.select(this).attr('transform', '');

        restart();
    }

    g.append('svg:circle')
        .attr('r', 12)
        .style('fill', (d) => (d === selectedNode) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .style('stroke', (d) => d3.rgb(colors(d.id)).darker().toString())
        .on('mouseover', mouseover)
        .on('mouseout', mouseout)
        .on('mousedown', mousedown)
        .on('mouseup', mouseup);

    // show node IDs
    g.append('text')
        .attr('x', 0)
        .attr('y', 4)
        .text((d) => {
            return d.id;
        });

    node = g.merge(node);

    function mouseover(d) {
        // if (!mousedownNode || d === mousedownNode) return;
        // // enlarge target node
        // d3.select(this).attr('transform', 'scale(1.1)');
    }
}

// update graph (called when needed)
function restart() {
    updateLinks();
    updateNodes();

    // set the graph in motion
    force
        .nodes(graph.nodes)
        .force('link').links(graph.links);

    force.alphaTarget(layoutConfig.alphaTarget.hot).restart();
}


function initialSetup() {
    // set up SVG for D3
    colors = d3.scaleOrdinal(d3.schemeCategory10);
    svg = setupSVG();

    // handles to link and node element groups
    graph = new Graph();
    path = svg.append('svg:g').selectAll('path');
    node = svg.append('svg:g').selectAll('g.node');

    force = setupForceLayout();
    drag = setupDrag();


    setupInteractionVars();

    graph.getInitialGraph();
    restart();
}

function main() {
    const xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function () {
        if (this.readyState === 4 && this.status === 200) {
            layoutConfig = JSON.parse(this.responseText);
            initialSetup();
        }
    };
    xmlhttp.open("GET", "layout_config.json", true);
    xmlhttp.send();
}

main();
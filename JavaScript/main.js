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

/*
GLOBAL variables
 */
let graph;
let interactor, colors;
let useWebGL;


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
function initialSetup(layoutConfig) {
    useWebGL = false;

    graph = new Graph();


    interactor = new Interactor(restart);

    // set up SVG for D3
    colors = d3.scaleOrdinal(d3.schemeCategory10);

    setupDisplay();

    //Setup D3
    setupForceLayout(layoutConfig);
    setupDrag(force);
    setupZoom();

    if (useWebGL) {
        setupWebGL();
    } else {
        setupSVG(layoutConfig, interactor);
        // handles to link and node element groups
        setupPath();
        setupNode();
    }

    setupSliders(layoutConfig);

    // handle key events
    d3.select(window)
        .on('keydown', () => interactor.bodyKeyDown(svg))
        .on('keyup', () => interactor.bodyKeyUp(svg));

    window.addEventListener("resize", redraw);

    graph.getInitialGraph();


}

function redraw() {
    if (useWebGL) {
        canvas.attr('width', canvas.node().getBoundingClientRect().width);
        canvas.attr('height', canvas.node().getBoundingClientRect().height);
    } else {
        svg.attr('width', svg.node().getBoundingClientRect().width);
        svg.attr('height', svg.node().getBoundingClientRect().height);
    }
    force.force('x', d3.forceX(svg.node().getBoundingClientRect().width * 0.5));
    force.force('y', d3.forceY(svg.node().getBoundingClientRect().height * 0.5));
}

// update graph (called when needed)
function restart() {
    updateLinks();
    updateNodes();

    // set the graph in motion
    force
        .nodes(graph.nodes)
        .force('link')
        .links(graph.links);

    force.alphaTarget(alphaTargetHot).restart();
}

function main() {
    const xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function () {
        if (this.readyState === 4 && this.status === 200) {
            initialSetup(JSON.parse(this.responseText));
        }
    };
    xmlhttp.open("GET", "resources/layout_config.json", true);
    xmlhttp.send();
}

main();
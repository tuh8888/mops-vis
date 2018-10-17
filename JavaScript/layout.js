let force, drag, zoom;
const nominal_stroke = 1.5;
const max_stroke = 4.5;
const max_base_node_size = 36;
const min_zoom = 0.1;
const max_zoom = 7;
const nominal_base_node_size = 8;
const nominal_text_size = 10;
const max_text_size = 24;
const text_center = false;
let stroke = nominal_stroke;

function setupForceLayout(layoutConfig) {
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

        pathText.attr("transform", (d) => {
            return "translate(" + ((d.source.x + d.target.x) / 2) + "," + ((d.source.y + d.target.y) / 2) + ")";
        });


        node.attr('transform', (d) => `translate(${d.x},${d.y})`);
    };

    // init D3 force layout
    force = d3.forceSimulation()
        .force('link', d3.forceLink().id((d) => d.id).distance(layoutConfig.link.distance))
        .force('charge', d3.forceManyBody().strength(layoutConfig.charge.strength))
        .force('x', d3.forceX(display.node().getBoundingClientRect().width * 0.5))
        .force('y', d3.forceY(display.node().getBoundingClientRect().height * 0.5))
        .on('tick', tick);
}

function setupDrag(layoutConfig, force) {
    // init D3 drag support
    drag = d3.drag()
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

function setupZoom() {
    zoom = d3.zoom().scaleExtent([min_zoom, max_zoom])
        .on("zoom", function () {

            node.attr("transform", d3.zoomTransform(this));
        });
    svg.call(zoom);
}

function setupSlider() {
    const slider1 = d3.sliderHorizontal()
        .min(0)
        .max(1)
        .width(100)
        .ticks(2)
        .default(0.015)
        .on('onchange', val => {
            d3.select("p#value1").text(val);
        });

    const group1 = d3.select("div#slider1").append("svg")
        .append("g")
        .attr("transform", "translate(30,30)");

    group1.call(slider1);

    d3.select("p#value1").text(d3.format('.2%')(slider1.value()));
    d3.select("a#setValue1").on("click", () => {
        slider1.value(0.015);
        d3.event.preventDefault();
    });
}
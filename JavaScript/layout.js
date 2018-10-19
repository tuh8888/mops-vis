let force, drag, zoom;
let alphaTargetHot, alphaTargetCool;

function setupForceLayout(layoutConfig) {
    alphaTargetHot = layoutConfig.alphaTarget.hot;
    alphaTargetCool = layoutConfig.alphaTarget.cool;

    // update force layout (called automatically each iteration)
    const tick = function () {
        if (useWebGL) {
            graph.nodes.forEach((node) => {
                const {x, y, circle} = node;
                circle.position.set(x-300, y - 300, 0);
            });

            graph.links.forEach((link) => {
                const {source, target, line} = link;
                line.geometry.verticesNeedUpdate = true;
                line.geometry.vertices[0] = new THREE.Vector3(source.x-300, source.y-300, -1);
                line.geometry.vertices[1] = new THREE.Vector3(target.x-300, target.y-300, -1)
            });
            posBuff.needsUpdate = true; // Important!
            renderer.render(scene, camera);
        } else {
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
        }
    };

    // init D3 force layout
    force = d3.forceSimulation()
        .force('link', d3.forceLink().id((d) => d.id).distance(layoutConfig.link.distance).strength(layoutConfig.link.strength))
        .force('charge', d3.forceManyBody().strength(layoutConfig.charge.strength))
        .force('x', d3.forceX(display.node().getBoundingClientRect().width * 0.5))
        .force('y', d3.forceY(display.node().getBoundingClientRect().height * 0.5))
        .on('tick', tick);
}

function setupDrag(force) {
    // init D3 drag support
    drag = d3.drag()
        .on('start', (d) => {
            if (!d3.event.active) force.alphaTarget(alphaTargetHot).restart();
            d.fx = d.x;
            d.fy = d.y;
        })
        .on('drag', (d) => {
            d.fx = d3.event.x;
            d.fy = d3.event.y;
        })
        .on('end', (d) => {
            if (!d3.event.active) force.alphaTarget(alphaTargetCool);
            d.fx = null;
            d.fy = null;
        });
}

function setupZoom() {
    zoom = d3.zoom().scaleExtent([0.1, 7])
        .on("zoom", function () {

            const transform = d3.zoomTransform(this);
            if (useWebGL) {
                const context = canvas.getContext("2d");
                context.translate(transform.x, transform.y);
                context.scale(transform.k, transform.k);
            } else {
                g.attr("transform", transform);
            }
        });
}

function makeSlider(name, min, max, start, func) {
    const div = d3.select("#layout").append("div")
        .attr("height", 100);
    div.append("p")
        .text(name);
    const sliderSVG = div.append("svg")
        .attr("class", "slider")
        .append("g")
        .attr("transform", "translate(30,30)");
    const slider = d3.sliderHorizontal()
        .min(min)
        .max(max)
        .ticks(0)
        .width(100)
        .default(start)
        .on('onchange', func);
    sliderSVG.call(slider);
}

function setupSliders(layoutConfig) {
    makeSlider("Alpha Target", 0, 1, layoutConfig.alphaTarget.hot, (val) => {
        alphaTargetHot = val;
        force.alphaTarget(alphaTargetHot).restart();
        restart();
    });

    makeSlider("Charge Strength", -10000, 0, layoutConfig.charge.strength, val => {
        force.force('charge', d3.forceManyBody().strength(val));
        restart()
    });

    makeSlider("Link Distance", 0, 1000, layoutConfig.link.distance, (val) => {
        force.force('link', d3.forceLink().id((d) => d.id).distance(val));
        restart()
    });
    makeSlider("Link Strength", 0, 1, layoutConfig.link.strength, (val) => {
        force.force('link', d3.forceLink().id((d) => d.id).strength(val));
        restart()
    });
}
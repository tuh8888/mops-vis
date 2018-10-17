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
            return "translate(" + ((d.source.x + d.target.x)/2) + "," + ((d.source.y + d.target.y)/2) + ")"; });


        node.attr('transform', (d) => `translate(${d.x},${d.y})`);
    };

    // init D3 force layout
    force = d3.forceSimulation()
        .force('link', d3.forceLink().id((d) => d.id).distance(layoutConfig.link.distance))
        .force('charge', d3.forceManyBody().strength(layoutConfig.charge.strength))
        .force('x', d3.forceX(layoutConfig.width * layoutConfig.x.scale))
        .force('y', d3.forceY(layoutConfig.height * layoutConfig.y.scale))
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
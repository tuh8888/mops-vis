/**
 * @param layoutConfig
 * @param {!Interactor} interactor
 * @returns {*}
 */
function setupSVG(layoutConfig, interactor) {
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



    svg.on('mousedown', () => interactor.svgMouseDown(svg))
        .on('mousemove', () => interactor.svgMouseMove(svg))
        .on('mouseup', () =>interactor.svgMouseUp(svg));

    return svg;
}
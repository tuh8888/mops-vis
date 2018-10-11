function setupPath(svg) {
    return svg.append('svg:g').selectAll('path');
}

function setupNode(svg) {
    return svg.append('svg:g').selectAll('g.node');
}

/**
 *
 * @returns {*}
 */
function updateLinks() {
    // path (link) group
    // noinspection ES6ModulesDependencies
    path = path.data(graph.links);

    // update existing links
    path.classed('selected', interactor.isSelectedLink)
        .style('marker-start', '')
        .style('marker-end', 'url(#end-arrow)');

    // remove old links
    path.exit().remove();

    // add new links
    const gLink = path.enter().append('svg:g')
        .attr('class', 'link');

    const paths = gLink.append('path')
    // .attr('class', 'link')
        .classed('selected', interactor.isSelectedLink)
        .style('marker-start', '')
        .attr('marker-end', 'url(#end-arrow)')
        .on('mousedown', interactor.pathMouseDown);

    //TODO: figure out why paths aren't showing
    gLink.append("text").append("textPath")
        .attr("xlink:href", "#textPath")
        .text((d) => d.id);

    return paths.merge(path);
}

/**
 *
 * @returns {*}
 */
function updateNodes() {
    // circle (node) group
    // NB: the function arg is crucial here! nodes are known by id, not by index!
    node = node.data(graph.nodes, (d) => d.id);

    // update existing nodes (reflexive & selected visual states)
    node.selectAll('circle')
        .style('fill', (d) => (interactor.isSelectedNode(d)) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .classed('selected', interactor.isSelectedNode);

    node.selectAll('text')
        .classed('display', (d) => d.display);

    // remove old nodes
    node.exit().remove();

    // add new nodes
    const g = node.enter().append('svg:g')
        .attr('class', 'node');

    g.append('svg:circle')
        .attr('r', 12)
        .style('fill', (d) => (interactor.isSelectedNode(d)) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
        .style('stroke', (d) => d3.rgb(colors(d.id)).darker().toString())
        .on('mouseover', (d) => interactor.nodeMouseOver.call(interactor, d))
        .on('mouseout', (d) => interactor.nodeMouseOut.call(interactor, d))
        .on('mousedown', (d) => interactor.nodeMouseDown.call(interactor, d))
        .on('mouseup', (d) => interactor.nodeMouseUp.call(interactor, d));

    // show node IDs
    g.append('text')
        .attr('x', 0)
        .attr('y', 4)
        .text((d) => {
            return d.id;
        });

    return g.merge(node);

}
let path, node, pathText;

function setupPath() {
    path = svg.append('svg:g').selectAll('path');

    // Get text elements to use as path labels
    pathText = path.selectAll('text');
}

function setupNode() {
    node = svg.append('svg:g').selectAll('g.node');
}

/**
 *
 * @returns {*}
 */
function updateLinks() {
    // path (link) group
    // noinspection ES6ModulesDependencies
    path = path.data(graph.links);
    pathText = pathText.data(graph.links);

    // update existing links
    path.classed('selected', interactor.isSelectedLink)
        .style('marker-end', "url(#end-arrow)");

    // remove old links
    path.exit().remove();
    pathText.exit().remove();

    // add new links
    const gLink = path.enter().append('svg:g')
        .attr('class', 'link');

    const paths = gLink.append('path')
    // .attr('class', 'link')
        .classed('selected', interactor.isSelectedLink)
        .style('marker-end', "url(#end-arrow)")
        .on('mousedown', interactor.pathMouseDown);

    //TODO: figure out why paths aren't showing
    const pathTexts = gLink.append("text")
        .text((d) => d.label);


    path = paths.merge(path);
    pathText = pathTexts.merge(pathText);
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
        .style('stroke-width', (d) => (interactor.isSelectedNode(d)) ? 2 * stroke : stroke)
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
        .on('mouseup', (d) => interactor.nodeMouseUp.call(interactor, d))
        .on("dblclick.zoom", (d) => interactor.centerNode.call(interactor, d));

    // show node IDs
    g.append('text')
        .attr('x', 0)
        .attr('y', 4)
        .text((d) => {
            return d.id;
        });

    node = g.merge(node);
}
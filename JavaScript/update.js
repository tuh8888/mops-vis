let path, node, pathText;
const rectHeight = 16;
const charWidth = 10;
const textYOffset = 13;

function setupPath() {
    path = g.append('svg:g').selectAll('path');

    // Get text elements to use as path labels
    pathText = path.selectAll('text');
}

function setupNode() {
    node = g.append('svg:g').selectAll('g.node');
}

/**
 *
 * @returns {*}
 */
function updateLinks() {
    if (useWebGL) {
        graph.links.forEach((link) => {
            if (!link.line) {
                link.material = new THREE.LineBasicMaterial({color: 0xAAAAAA});
                link.geometry = new THREE.Geometry();
                link.line = new THREE.Line(link.geometry, link.material);
                scene.add(link.line)
            }
        });
    } else {
        // path (link) group
        // noinspection ES6ModulesDependencies
        path = path.data(graph.links);
        pathText = pathText.data(graph.links);

        // update existing links
        path.classed('selected', interactor.isSelectedLink)
            .style('marker-start', '')
            .style('marker-end', 'url(#end-arrow)')
            .style('display', (d) => {
                if (!displayAbstractions && Interactor.isSubClassOf(d)) {
                    return "none";
                } else {
                    return "inline";
                }
            });

        pathText.style('display', (d) => {
            if (!displayAbstractions && Interactor.isSubClassOf(d)) {
                return "none";
            } else {
                return "inline";
            }
        });

        // remove old links
        path.exit().remove();
        pathText.exit().remove();

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
        const pathTexts = gLink.append("text")
            .text((d) => d.label);

        path = paths.merge(path);
        pathText = pathTexts.merge(pathText);
    }
}

/**
 *
 * @returns {*}
 */
function updateNodes() {
    if (useWebGL) {
        graph.nodes.forEach((node) => {
            if (! node.circle) {
                node.geometry = new THREE.CircleGeometry(5, 32);
                node.material = new THREE.MeshBasicMaterial({color: colors(node.id)});
                node.circle = new THREE.Mesh(node.geometry, node.material);
                scene.add(node.circle)
            }
        });
    } else {
        // circle (node) group
        // NB: the function arg is crucial here! nodes are known by id, not by index!
        node = node.data(graph.nodes, (d) => d.id);

        // update existing nodes (reflexive & selected visual states)
        node.selectAll('rect')
            .style('fill', (d) => (interactor.isSelectedNode(d)) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
            .classed('selected', interactor.isSelectedNode);

        node.selectAll('text')
            .classed('display', (d) => d.display);

        // remove old nodes
        node.exit().remove();

        // add new nodes
        const g = node.enter().append('svg:g')
            .attr('class', 'node');



        g.append('svg:rect')
            .attr('width', (d) => {
                const width = d.id.length * charWidth;
                d.width = width;
                return width;
            })
            .attr('height', (d) => {
                const height = rectHeight;
                d.height = height;
                return height;
            })
            .style('fill', (d) => (interactor.isSelectedNode(d)) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id))
            .style('stroke', (d) => d3.rgb(colors(d.id)).darker().toString())
            .on('mouseover', (d) => interactor.nodeMouseOver.call(interactor, d))
            .on('mouseout', (d) => interactor.nodeMouseOut.call(interactor, d))
            .on('mousedown', (d) => interactor.nodeMouseDown.call(interactor, d))
            .on('mouseup', (d) => interactor.nodeMouseUp.call(interactor, d));

        // show node IDs
        g.append('text')
            .attr('x', (d) => d.id.length * charWidth / 2)
            .attr('y', textYOffset)
            .text((d) => { return d.id; });



        node = g.merge(node);
    }
}
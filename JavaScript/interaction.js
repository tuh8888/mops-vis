class Interactor {
    constructor() {
        this.selectedNode = null;
        this.mousedownNode = null;
        this.mouseupNode = null;
        this.selectedLink = null;
        this.mousedownLink = null;
        this.lastKeyDown = -1;
    }

    resetMouseVars() {
        this.mousedownNode = null;
        this.mouseupNode = null;
        this.mousedownLink = null;
    }

    pathMouseDown(d) {
        if (d3.event.ctrlKey) return;

        // select link
        this.mousedownLink = d;
        this.selectedLink = (this.mousedownLink === this.selectedLink) ? null : this.mousedownLink;
        this.selectedNode = null;
        restart();
    }

    isSelectedLink (d) {
        return d === this.selectedLink;
    }

    isSelectedNode (d) {
        return d === this.selectedNode;
    }

    nodeMouseOut(d) {
        // if (!mousedownNode || d === mousedownNode) return;
        // // unenlarge target node
        // d3.select(this).attr('transform', '');
    }

    nodeMouseDown(d) {
        if (d3.event.ctrlKey) {
            graph.getNode(d.id);
        } else {
            node.call(drag);
        }

        // select node
        this.mousedownNode = d;
        d.display = !d.display;
        this.selectedNode = (this.mousedownNode === this.selectedNode) ? null : this.mousedownNode;

        this.selectedLink = null;

        restart();
    }

    nodeMouseUp(d) {
        'use strict';
        if (!this.mousedownNode) return;

        // check for drag-to-self
        this.mouseupNode = d;
        if (this.mouseupNode === this.mousedownNode) {
            if (this.mouseupNode) {
                graph.getNode(this.mouseupNode.id);
            }
            this.resetMouseVars();
            return;
        }

        // unenlarge target node
        d3.select(this).attr('transform', '');

        restart();
    }

    nodeMouseOver(d) {
        // if (!mousedownNode || d === mousedownNode) return;
        // // enlarge target node
        // d3.select(this).attr('transform', 'scale(1.1)');
    }

    svgMouseDown(svg) {
        // because :active only works in WebKit?
        svg.classed('active', true);
        if (d3.event.ctrlKey || this.mousedownNode || this.mousedownLink) return;
        restart();
    }

    // noinspection JSUnusedLocalSymbols
    svgMouseMove(svg) {
        if (!this.mousedownNode) return;
        restart();
    }

    svgMouseUp(svg) {
        // because :active only works in WebKit?
        svg.classed('active', false);
        // clear mouse event vars
        this.resetMouseVars();
    }

    // noinspection JSUnusedLocalSymbols
    bodyKeyDown(svg) {
        // d3.event.preventDefault();

        if (this.lastKeyDown !== -1) return;
        this.lastKeyDown = d3.event.key;
        if (d3.event.key === "Delete") {
            if (this.selectedNode) {
                graph.removeNode(this.selectedNode.id)
            } else if (this.selectedLink) {
                // TODO: Delete links
                // links.splice(links.indexOf(selectedLink), 1);
            }
        }
        if (d3.event.ctrlKey) {
            node.on('.drag', null);
            svg.classed('ctrl', false);
        }
    }

    bodyKeyUp(svg) {
        this.lastKeyDown = -1;
        // ctrl
        if (d3.event.key === "Control") {
            node.call(drag);
            svg.classed('ctrl', true)
        }
    }
}



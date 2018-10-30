let inherited = false;
let displayAbstractions = false;
let displayAllNodeLabels = false;
let displayAllEdgeLabels = false;

// Make some extensions to Array to prevent adding if items are already in the graph
Array.prototype.inArray = function (comparer) {
    for (let i = 0; i < this.length; i++) {
        if (comparer(this[i])) return true;
    }
    return false;
};
Array.prototype.pushIfDoesNotExist = function (element, comparer) {
    if (!this.inArray(comparer)) {
        this.push(element);
    }
};

class Node {
    constructor(id) {
        this.id = id;
    }
}

class Link {
    constructor(source, target, label) {
        this.source = source;
        this.target = target;
        this.label = label;

    }
}

class Graph {

    constructor() {
        this.nodes = [];
        this.links = [];
        this.defaultData = null;
    }

    addGraphData(newGraphData) {
        console.log("graph received");
        if (newGraphData.nodes && newGraphData.links) {
            let newNode;
            for (let i = 0; i < newGraphData.nodes.length; i++) {
                newNode = newGraphData.nodes[i];
                graph.nodes.pushIfDoesNotExist(newNode, function (e) {
                    return e.id === newNode.id;
                })
            }
            let newLink;
            for (let i = 0; i < newGraphData.links.length; i++) {
                newLink = newGraphData.links[i];
                newLink.source = typeof newLink.source === 'string' ? graph.nodes.find(node => node.id === newLink.source) : newLink.source;
                newLink.target = typeof newLink.target === 'string' ? graph.nodes.find(node => node.id === newLink.target) : newLink.target;
                graph.links.pushIfDoesNotExist(newLink, function (e) {
                    return (e.source === newLink.source && e.target === newLink.target) || (e.source.id === newLink.source && e.target.id === newLink.target);
                })
            }
            console.log(newGraphData);
            restart();
        }
    };

    static getInitialGraph() {
        getP53()
    };

    getNode(nodeId) {
        if (smackjack.exists) {
            smackjack.getNode(nodeId, {"inherited": inherited}, this.addGraphData);
        } else {
            const linksToReturn = this.defaultData.links.filter(link => link.source === nodeId || link.target === nodeId || link.target.id === nodeId || link.source.id === nodeId);

            const nodesToReturn = [];
            for (let i = 0; i < linksToReturn.length; i++) {
                let link = linksToReturn[i];
                nodesToReturn.push(typeof link.source === 'string' ? new Node(link.source) : link.source);
                nodesToReturn.push(typeof link.target === 'string' ? new Node(link.target) : link.target);
            }

            const graphToReturn = {
                "links": linksToReturn,
                "nodes": nodesToReturn
            };
            this.addGraphData(graphToReturn);
        }
    };

    removeNode(nodeId) {
        this.nodes = this.nodes.filter(node => node.id !== nodeId);
        this.links = this.links.filter(link => link.source.id !== nodeId && link.target.id !== nodeId);
        restart();
    };

    searchNode() {
        const idToSearchFor = document.getElementById("search-text-field").value;
        console.log(idToSearchFor);
        this.getNode(idToSearchFor);
        restart();
    };

    //TODO: Finish intersection search on lisp side
    intersectionSearch() {
        const ids = [].map.call(document.getElementsByClassName("intersection-search-text-field"), (textField) => textField.value);
        const maxDepth = document.getElementById("max-depth-text-field").value;
        const maxFanout = document.getElementById("max-fanout-text-field").value;

        if (smackjack.exists) {
            smackjack.getSearchResults(ids, "intersection search", {"maxDepth": maxDepth, "maxFanout":maxFanout}, this.addGraphData);
        }
        restart();
    }
}

class BigDefaultGraph extends Graph {
    constructor(size) {
        super();
        this.nodes = d3.range(size).map(function (i) {
            return {
                id: i.toString()
            };
        });
        this.links = d3.range(this.nodes.length - 1).map(function (i) {
            return {
                source: Math.floor(Math.sqrt(i)).toString(),
                target: (i + 1).toString(),
                label: i.toString()
            };
        });

        this.defaultData = {
            "nodes": this.nodes,
            "links": this.links
        };
        console.log(this);
    }

    static getInitialGraph() {
        restart();
    }

}

function setInherited() {
    inherited = document.getElementById("inheritedCheckBox").checked;
}

function setDisplayAbstractions() {
    displayAbstractions = document.getElementById("abstractionsCheckBox").checked;
    restart();
}

function setNodeLabelsAbstractions() {
    displayAllNodeLabels = document.getElementById("nodeLabelsCheckBox").checked;
    restart();
}

function setEdgeLabelsAbstractions() {
    displayAllEdgeLabels = document.getElementById("edgeLabelsCheckBox").checked;
    restart();
}

function getP53() {
    smackjack.checkIfSmackjackExists();

    // Give server a chance to respond and let head load properly
    setTimeout(function () {
        if (smackjack.exists) {
            console.log("Using server data");
            graph.getNode("cellular tumor antigen p53 (human)");
        } else {
            console.log("Using default data");
            const xmlhttp = new XMLHttpRequest();
            xmlhttp.onreadystatechange = function () {
                if (this.readyState === 4 && this.status === 200) {
                    graph.defaultData = JSON.parse(this.responseText);
                    graph.addGraphData(graph.defaultData);
                }
            };
            xmlhttp.open("GET", "resources/graph.json", true);
            xmlhttp.send();
        }
        restart();
    }, 2000);
}
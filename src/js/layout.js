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
                [p1, p2] = getPath(d);
                return `M${p1[0]},${p1[1]}L${p2[0]},${p2[1]}`;
            });

            pathText.attr("transform", (d) => {
                [p1, p2] = getPath(d);
                return "translate(" + ((p1[0] + p2[0]) / 2) + "," + ((p1[1] + p2[1]) / 2) + ")";
            });


            node.attr('transform', (d) => `translate(${d.x},${d.y})`);
        }
    };

    function getIntersection(dx, dy, cx, cy, w, h) {
        if (Math.abs(dy / dx) < h / w) {
            // Hit vertical edge of box1
            return [cx + (dx > 0 ? w : -w), cy + dy * w / Math.abs(dx)];
        } else {
            // Hit horizontal edge of box1
            return [cx + dx * h / Math.abs(dy), cy + (dy > 0 ? h : -h)];
        }
    }

    function getPath(d) {
        const w1 = d.source.width / 2;
        const w2 = d.target.width / 2;
        const h1 = d.source.height / 2;
        const h2 = d.target.height / 2;

        const x1 = d.source.x;
        const y1 = d.source.y;
        const x2 = d.target.x;
        const y2 = d.target.y;

        // Center coordinates
        const cx1 = x1 + w1;
        const cy1 = y1 + h1;
        const cx2 = x2 + w2;
        const cy2 = y2 + h2;

        // Distance between centers
        const dx = cx2 - cx1;
        const dy = cy2 - cy1;

        let p1, p2;
        if (!dx) {
            p1 = [cx1, y1 + h2 * 2];
            p2 = [cx1, y2];
        } else {
            p1 = getIntersection(dx, dy, cx1, cy1, w1, h1);
            p2 = getIntersection(-dx, -dy, cx2, cy2, w2, h2);
        }
        return [p1, p2]
    }


    // init D3 force layout
    force = d3.forceSimulation()
        .force('link', d3.forceLink().id((d) => d.id).distance(layoutConfig.link.distance).strength(layoutConfig.link.strength))
        .force('charge', d3.forceManyBody().strength(layoutConfig.charge.strength))
        .force('collide', rectCollide().size((d) => {return [d.width, d.height]}))
        .force('x', d3.forceX(display.node().getBoundingClientRect().width * 0.5))
        .force('y', d3.forceY(display.node().getBoundingClientRect().height * 0.5))
        .on('tick', tick);

    function constant(_) {
        return function () { return _ }
    }
    function rectCollide() {
        let nodes, sizes, masses;
        let size = constant([0, 0]);
        let strength = 1;
        let iterations = 1;

        function force() {
            let node, size, mass, xi, yi;
            let i = -1;
            while (++i < iterations) { iterate() }

            function iterate() {
                let j = -1;
                const tree = d3.quadtree(nodes, xCenter, yCenter).visitAfter(prepare);

                while (++j < nodes.length) {
                    node = nodes[j];
                    size = sizes[j];
                    mass = masses[j];
                    xi = xCenter(node);
                    yi = yCenter(node);

                    tree.visit(apply)
                }
            }

            function apply(quad, x0, y0, x1, y1) {
                const data = quad.data;
                const xSize = (size[0] + quad.size[0]) / 2;
                const ySize = (size[1] + quad.size[1]) / 2;
                if (data) {
                    if (data.index <= node.index) { return }

                    let x = xi - xCenter(data);
                    let y = yi - yCenter(data);
                    const xd = Math.abs(x) - xSize;
                    // noinspection JSSuspiciousNameCombination
                    const yd = Math.abs(y) - ySize;

                    if (xd < 0 && yd < 0) {
                        const l = Math.sqrt(x * x + y * y);
                        const m = masses[data.index] / (mass + masses[data.index]);

                        if (Math.abs(xd) < Math.abs(yd)) {
                            node.vx -= (x *= xd / l * strength) * m;
                            data.vx += x * (1 - m)
                        } else {
                            node.vy -= (y *= yd / l * strength) * m;
                            data.vy += y * (1 - m)
                        }
                    }
                }

                return x0 > xi + xSize || y0 > yi + ySize ||
                    x1 < xi - xSize || y1 < yi - ySize
            }

            function prepare(quad) {
                if (quad.data) {
                    quad.size = sizes[quad.data.index]
                } else {
                    quad.size = [0, 0];
                    let i = -1;
                    while (++i < 4) {
                        if (quad[i] && quad[i].size) {
                            quad.size[0] = Math.max(quad.size[0], quad[i].size[0]);
                            quad.size[1] = Math.max(quad.size[1], quad[i].size[1])
                        }
                    }
                }
            }
        }

        function xCenter(d) { return d.x + d.vx + sizes[d.index][0] / 2 }
        function yCenter(d) { return d.y + d.vy + sizes[d.index][1] / 2 }

        force.initialize = function (_) {
            sizes = (nodes = _).map(size);
            masses = sizes.map(function (d) { return d[0] * d[1] })
        };

        force.size = function (_) {
            // noinspection CommaExpressionJS
            return (arguments.length
                ? (size = typeof _ === 'function' ? _ : constant(_), force)
                : size)
        };

        force.strength = function (_) {
            // noinspection CommaExpressionJS
            return (arguments.length ? (strength = +_, force) : strength)
        };

        force.iterations = function (_) {
            // noinspection CommaExpressionJS
            return (arguments.length ? (iterations = +_, force) : iterations)
        };

        return force
    }
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
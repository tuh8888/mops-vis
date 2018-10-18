let display, svg, g;
let positions, renderer, scene, camera, posBuff, canvas;

function setupDisplay() {
    display = d3.select('#display');
}

/**
 * @param layoutConfig
 * @param {!Interactor} interactor
 * @returns {*}
 */
function setupSVG(layoutConfig, interactor) {
    svg = display
        .append('svg')
        .attr('oncontextmenu', 'return false;')
        .attr('height', window.height)
        .attr('width', window.width);
    g = svg.append("svg:g");

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

    // svg.append("defs").append("path")
    //     .attr("id", "textPath")
    //     .attr("d", 'M10,-5L0,0L10,5');


    svg.on('mousedown', () => interactor.svgMouseDown(svg))
        .on('mousemove', () => interactor.svgMouseMove(svg))
        .on('mouseup', () => interactor.svgMouseUp(svg));

    svg.call(zoom);
}

function setupWebGL() {
    const width = d3.select('#display').node().getBoundingClientRect().width;
    const height = d3.select('#display').node().getBoundingClientRect().height;

    scene = new THREE.Scene();
    camera = new THREE.OrthographicCamera(0, width / 2, height / 2, height / -2, 1, 10000);
    renderer = new THREE.WebGLRenderer({alpha: true});
    renderer.setSize(width, height);
    scene.add(camera);
    canvas = renderer.domElement;
    d3.select('#display').node().append(canvas);
    camera.position.z = 1000;

    const attributes = {
        size: {type: 'f', value: []}
    };

    const cloudMat = new THREE.ShaderMaterial({
        uniforms: uniforms(),
        attributes: attributes,
        vertexShader: d3.select('#vertexshader').node().textContent,
        fragmentShader: d3.select('#fragmentshader').node().textContent,
        transparent: true,
        setDepthTest: false,
        // blending: THREE.CustomBlending,
        // blendEquation: THREE.AddEquation,
        // blendSrc: THREE.SrcAlphaSaturate,
        // blendDst: THREE.OneMinusSrcAlphaFactor,
    });

    positions = [];

    const cloudGeom = new THREE.BufferGeometry();
    posBuff = new THREE.BufferAttribute(positions, 3);
    cloudGeom.addAttribute('position', posBuff);
    // cloudGeom.addAttribute('size', new THREE.BufferAttribute(sizes, 1));
    cloudGeom.computeBoundingSphere();

    const pointCloud = new THREE.PointCloud(cloudGeom, cloudMat);
    scene.add(pointCloud);
}

function uniforms(opts) {
    opts = opts || {};
    return {
        color: {
            type: 'c',
            value: new THREE.Color(0x3498db)
        },
        alpha: {type: 'f', value: 0.7},
        pointSize: {type: 'f', value: 10},
        shouldResize: {type: '1i', value: opts.shouldResize ? 1 : 0}
    }
}
const smackjack = {"exists": false};
(function () {
    let httpFactory = null;
    const httpFactories = [function () {
        return new XMLHttpRequest();
    }, function () {
        return new ActiveXObject('Msxml2.XMLHTTP');
    }, function () {
        return new ActiveXObject('Microsoft.XMLHTTP');
    }];

    function httpNewRequest() {
        if (httpFactory) {
            return httpFactory();
        } else {
            let request = null;
            let i = 0, l = httpFactories.length, factory = httpFactories[i];
            for (; !(request !== null || i >= l); i += 1, factory = httpFactories[i]) {
                try {
                    request = factory();
                } catch (e) {
                }
            }
            if (request === null) {
                httpFactory = function () {
                    throw new Error('XMLHttpRequest not supported');
                };
                return httpFactory();
            } else {
                return request;
            }
        }
    }

    // noinspection JSUnusedLocalSymbols
    function identity(x) {
        return x;
    }

    // noinspection JSUnusedLocalSymbols
    function responseXml(request) {
        return request.responseXML;
    }

    // noinspection JSUnusedLocalSymbols
    function responseText(request) {
        return request.responseText;
    }

    // noinspection JSUnusedLocalSymbols
    function responseXmlText(request) {
        let result = '';
        let n = request.responseXML.firstChild;
        if (n) {
            n = n.firstChild;
            if (n) {
                result = n.nodeValue;
            }
        }
        return result;
    }

    function responseJson(request) {
        return JSON.parse(request.responseText);
    }

    function fetchUri(uri, callback, method, body, errorHandler, process) {
        if (method === undefined) {
            method = 'GET';
        }
        let request = httpNewRequest();
        if (!request) {
            console.log('Browser couldn\'t make a request object.');
        }
        request.open(method, uri, true);
        request.onreadystatechange = function () {
            if (4 === request.readyState) {
                if (request.status >= 200 && request.status < 300 || request.status === 304) {
                    if (callback != null) {
                        callback(process(request));
                    }
                } else {
                    if (errorHandler == null) {
                        console.log('Error while fetching URI ' + uri + ' ' + request.status + ' ' + request.statusText);
                    } else {
                        errorHandler(request);
                    }
                }
            }
            return null;
        };
        if (method === 'POST') {
            request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        }
        request.send(body);
        return null;
    }

    function ajaxEncodeArgs(args) {
        let s = '';
        for (let i = 0; i < args.length; i += 1) {
            if (i > 0) {
                s += '&';
            }
            s += 'arg' + i + '=' + encodeURIComponent(JSON.stringify(args[i]));
        }
        return s;
    }

    function ajaxCall(func, args, method, callback, errorHandler, process) {
        if (method === undefined) {
            method = 'GET';
        }
        let uri = '/ajax-process' + '/' + encodeURIComponent(func) + '/';
        const ajaxArgs = ajaxEncodeArgs(args);
        let body = null;
        if (method === 'GET' && args.length > 0) {
            uri += '?' + ajaxArgs;
        }
        if (method === 'POST') {
            body = ajaxArgs;
        }
        return fetchUri(uri, callback, method, body, errorHandler, process);
    }

    //TODO edit these according to the corresponding ajax calls under "initialize-ajax" in network-visualization-website.lisp
    function getNode(nodeName, callback, errorHandler) {
        return ajaxCall('GET-NODE', [nodeName, inherited], 'GET', callback, errorHandler, responseJson);  //arguments go in the brackets
    }

    smackjack.checkIfSmackjackExists = function() {
        const request = new XMLHttpRequest();
        request.onreadystatechange = function () {
            if (request.readyState === 4) {
                smackjack.exists = request.status !== 404;
            }
        };
        request.open('GET', "http://" + location.hostname + ":" + location.port + "/ajax-process/GET-NODE", true);
        request.send();
    };

    smackjack.ajaxCall = ajaxCall;
    smackjack.getNode = getNode;
    return null;
})();
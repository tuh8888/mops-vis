# mops-vis
Visualization for KaBOB Mops interface

## Getting started:
1. Start an mlisp on the server
2. Install cl-who, hunchentoot, cl-json, and smackjack from quicklisp.
 ```lisp
 (ql:quickload '("hunchentoot" "cl-who" "cl-json" "smackjack"))
 ```
 This will fail on Allegro CL. If it does, abort and do the following:
  - Go into quicklisp/dists/quicklisp/software
  - Make these changes to cl+ssl/src/ffi.lisp
    - +V-ASN1-UTF8STRING+ => +v-asn1-utf8string+
    - +V-ASN1-PRINTABLESTRING+ => +v-asn1-printablestring+
    - +V-ASN1-TELETEXSTRING+ => +v-asn1-teletexstring+
    - +V-ASN1-IASTRING+ => +v-asn1-iastring+
    - +V-ASN1-UNIVERSALSTRING+ => +v-asn1-universalstring+
    - +V-ASN1-BMPSTRING+ => +v-asn1-bmpstring+
  
  - Make this change to Parenscript/src/package.lisp
    - remove the statement (:case :invert) from line 9 in src/package.lisp
  - Now reattempt the installations
3. Set the variable `local-dir` in mops-vis.lisp to the base directory where mops-vis was installed to
4. load net-vis.lisp and mops-vis.lisp
5. Execute `(net-vis:start-website :port port-number)` with an open port number (8080 or 8081 are good choices)
6. Open the display by going to hostname:port/index in a browser


## TODO
- Expand all nodes on click (for some reason not all of the mop IDs are cooperative)
- Get inverse slots

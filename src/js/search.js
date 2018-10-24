const n = 5;
/*number of completions to display*/
const test = ["hello", "there", "appletree", "how", "are", "the", "ants", "and", "their", "apples", "there", "?"];
let autocompleteData;
smackjack.checkIfSmackjackExists();
setTimeout(function () {
    if (smackjack.exists) {
        smackjack.getAutocomplete(setAutocompleteData);
    } else {
        setAutocompleteData({
            "?": {"stop": null},
            "h": {"o": {"w": {"stop": null}}, "e": {"l": {"l": {"o": {"stop": null}}}}},
            "t": {"h": {"e": {"stop": null, "r": {"e": {"stop": null}}, "i": {"r": {"stop": null}}}}},
            "a": {
                "n": {"t": {"s": {"stop": null}}, "d": {"stop": null}},
                "p": {"p": {"l": {"e": {"s": {"stop": null}, "t": {"r": {"e": {"e": {"stop": null}}}}}}}},
                "r": {"e": {"stop": null}}
            }
        })
    }
}, 2000);

function setAutocompleteData(data) {
    autocompleteData = data;
    autocomplete(document.getElementById("search-text-field"), autocompleteData);
    addSearchTextField();
    addSearchTextField();
}

function autocomplete(inp, data) {
    /*the autocomplete function takes two arguments,
    the text field element and an array of possible autocompleted values:*/
    let currentFocus;
    /*execute a function when someone writes in the text field:*/
    // noinspection JSUnusedLocalSymbols
    inp.addEventListener("input", function (e) {
        let a, b, stringInput = this.value;
        /*close any already open lists of autocompleted values*/
        closeAllLists();
        if (!stringInput) {
            return false;
        }
        currentFocus = -1;
        /*create a DIV element that will contain the items (values):*/
        a = document.createElement("DIV");
        a.setAttribute("id", this.id + "autocomplete-list");
        a.setAttribute("class", "autocomplete-items");
        /*append the DIV element as a child of the autocomplete container:*/
        this.parentNode.appendChild(a);


        /*Get first completions*/
        const completions = findCompletetions(stringInput, data);
        if (completions) {
            completions.forEach(completion => {
                /*create a DIV element for each matching element:*/
                b = document.createElement("DIV");
                /*make the matching letters bold:*/
                b.innerHTML = "<strong>" + completion.substr(0, stringInput.length) + "</strong>";
                b.innerHTML += completion.substr(stringInput.length);
                /*insert a input field that will hold the current array item's value:*/
                b.innerHTML += "<input type='hidden' value='" + completion + "'>";
                /*execute a function when someone clicks on the item value (DIV element):*/
                // noinspection JSUnusedLocalSymbols
                b.addEventListener("click", function (e) {
                    /*insert the value for the autocomplete text field:*/
                    inp.value = this.getElementsByTagName("input")[0].value;
                    /*close the list of autocompleted values,
                    (or any other open lists of autocompleted values:*/
                    closeAllLists();
                });
                a.appendChild(b);
            });
        }
    });
    /*execute a function presses a key on the keyboard:*/
    inp.addEventListener("keydown", function (e) {
        let x = document.getElementById(this.id + "autocomplete-list");
        if (x) x = x.getElementsByTagName("div");
        if (e.key === "ArrowDown") {
            /*If the arrow DOWN key is pressed,
            increase the currentFocus variable:*/
            currentFocus++;
            /*and and make the current item more visible:*/
            addActive(x);
        } else if (e.key === "ArrowUp") { //up
            /*If the arrow UP key is pressed,
            decrease the currentFocus variable:*/
            currentFocus--;
            /*and and make the current item more visible:*/
            addActive(x);
        } else if (e.key === "Enter") {
            /*If the ENTER key is pressed, prevent the form from being submitted,*/
            e.preventDefault();
            if (currentFocus > -1) {
                /*and simulate a click on the "active" item:*/
                if (x) x[currentFocus].click();
            }
        }
    });

    function addActive(x) {
        /*a function to classify an item as "active":*/
        if (!x) return false;
        /*start by removing the "active" class on all items:*/
        removeActive(x);
        if (currentFocus >= x.length) currentFocus = 0;
        if (currentFocus < 0) currentFocus = (x.length - 1);
        /*add class "autocomplete-active":*/
        x[currentFocus].classList.add("autocomplete-active");
    }

    function removeActive(x) {
        /*a function to remove the "active" class from all autocomplete items:*/
        for (let i = 0; i < x.length; i++) {
            x[i].classList.remove("autocomplete-active");
        }
    }

    function closeAllLists(elmnt) {
        /*close all autocomplete lists in the document,
        except the one passed as an argument:*/
        let x = document.getElementsByClassName("autocomplete-items");
        for (let i = 0; i < x.length; i++) {
            if (elmnt !== x[i] && elmnt !== inp) {
                x[i].parentNode.removeChild(x[i]);
            }
        }
    }

    /*execute a function when someone clicks in the document:*/
    document.addEventListener("click", function (e) {
        closeAllLists(e.target);
    });
}

function findCompletetions(stringInput, data) {
    let char, value;
    let node = data;
    /*traverse data to deepest node in data*/
    for (let i = 0; i < stringInput.length; i++) {
        char = stringInput[i];
        value = node[char];
        if (value) {
            node = value;
        } else {
            return;
        }
    }
    /*Get first completions*/
    const completions = [];
    traverseNode(node, stringInput);

    function traverseNode(node, completion) {
        if (completions.length < n) {
            if (node) {
                for (let key in node) {
                    if (node.hasOwnProperty(key)) {
                        if (key === "stop") {
                            completions.push(completion);
                        } else if(key === "bar"){

                        } else {
                            traverseNode(node[key], completion + key);
                        }
                    }
                }
            } else {
                completions.push(completion);
            }
        }
    }

    return completions;
}

function addSearchTextField() {
    const div = document.getElementById("intersection-search-text-fields");
    const autocompleteDiv = document.createElement("div");
    autocompleteDiv.setAttribute("class", "autocomplete");
    const input = document.createElement("input");
    input.setAttribute("type", "text");
    input.setAttribute("class", "intersection-search-text-field");
    input.setAttribute("title", "intersection-search");
    autocomplete(input, autocompleteData);
    autocompleteDiv.appendChild(input);
    autocompleteDiv.appendChild(document.createElement("br"));
    div.appendChild(autocompleteDiv);
}

function removeSearchTextField() {
    const div = document.getElementById("intersection-search-text-fields");
    const childDivs = div.getElementsByClassName("autocomplete");
    // Don't remove first 2 text fields
    if (childDivs.length > 2) {
        //Remove text field
        div.removeChild(childDivs.item(childDivs.length-1));
    }
}
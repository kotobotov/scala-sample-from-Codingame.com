const expression = readline();

let start_elements = ["{", "(", "["]
let closed_to_start_convert = new Map([
    ["}", "{"],
    ["]", "["],
    [")", "("]
])
let valid_elements = ["{", "(", "[", ")", "}", "]"]

function rec_eval(input_data, out) {
    if (input_data !== undefined && input_data !== "") {
        let elem = input_data[0]
        if (valid_elements.includes(elem)) {
            if (start_elements.includes(elem)) return rec_eval(input_data.substring(1), elem + out)
            else if (closed_to_start_convert.has(elem) && out !== "" && closed_to_start_convert.get(elem) === out[0]) return rec_eval(input_data.substring(1), out.substring(1))
            else return false
        } else return rec_eval(input_data.substring(1), out)
    } else if (out === "") return true
    else return false
}

console.log(rec_eval(expression, ""));

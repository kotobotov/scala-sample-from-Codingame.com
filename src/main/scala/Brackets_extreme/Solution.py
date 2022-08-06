import sys
import math

# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.

my_input = input()
start_elements = ["{", "(", "["]
closed_to_start_convert = {"}": "{", "]": "[", ")": "("}
valid_elements = ["{", "(", "[", ")", "}", "]"]


def rec_eval(input_data, out):
    if input_data != "":
        elem = input_data[0]
        if elem in valid_elements:
            if elem in start_elements:
                return rec_eval(input_data[1:], elem + out)
            elif out != "" and elem in closed_to_start_convert and closed_to_start_convert.get(elem) == out[0]:
                return rec_eval(input_data[1:], out[1:])
            else:
                return False
        else:
            return rec_eval(input_data[1:], out)
    elif out == "":
        return True
    else:
        return False

print(str(rec_eval(my_input, "")).lower())
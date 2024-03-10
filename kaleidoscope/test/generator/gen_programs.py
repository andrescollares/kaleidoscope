import sys

iterations = int(sys.argv[1])
program = ""
var_list = []

for i in range(iterations):
    var_name = f"var_{i}"
    let_in = f"let int var_{i} = {i} in\n"
    program += let_in
    var_list.append(var_name)

program += " + ".join(var_list) + ";\n"

with open("/kaleidoscope/test/generator/out.k", "w") as f:
    f.write(program)


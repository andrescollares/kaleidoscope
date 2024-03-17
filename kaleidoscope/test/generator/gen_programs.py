import sys

iterations = int(sys.argv[1])
program = ""
var_list = []

for i in range(iterations):
    func_def = f"def func_{i}(int x) -> int: x;\n"
    program += func_def

for i in range(iterations):
    var_name = f"var_{i}"
    let_in = f"let int var_{i} = func_{i}({i}) in\n"
    program += let_in
    var_list.append(var_name)

program += " + ".join(var_list) + ";\n"

with open("/kaleidoscope/test/generator/out.k", "w") as f:
    f.write(program)


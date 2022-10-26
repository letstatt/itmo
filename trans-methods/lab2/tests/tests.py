import subprocess
import sys
import os

success = [
    # function name
    "fun _()",
    "fun a_b()",
    "fun a_z123()",
    "fun A_Z123()",
    # arguments
    "fun f(var: Type)",
    "fun f(var: Type, )",
    "fun f(var: Type, var: Type)",
    "fun f(var: Type, var: Type2)",
    "fun f(var: A, var2: B, var3: C, var4: D, var5: E)",
    # return type
    "fun f():Int",
    "fun f(): Int",
    "fun f(a: aboba): int",
    "fun f(): __type_with_underscores",
    # spaces
    " fun      f  (    abb:    bba)    :    kek  ",
    # typed types
    "fun f(var: Array<Int>)",
    "fun f(var: Array<Int>, var2: Array<Array<Int>>)",
    "fun f(): Array<Int>",
    "fun f(): Array<Array<Int>>",
]

failures = [
    # invalid function name
    "fun ()",
    "fun()",
    "fun 123()",
    "fun fun()",
    "fun f!()",
    "fun @f()",
    "fun русский_язык()",
    "fun :()",
    "fun )()",
    # no fun keyword
    "func f()",
    "abc()",
    "fun(): type",
    # broken brackets
    "fun a(: ret_type",
    "fun a[]: ret_type",
    "fun a\{\}: ret_type",
    "fun a: ret_type",
    "fun a",
    # invalid arguments list
    "fun f(a,)",
    "fun f(a, b)",
    "fun f(a: )",
    "fun f(a: , b)",
    # end of function declaration
    "fun f(): ",
    "fun f();",
    "fun f() fun (a: b)",
    "fun f() a: A",
    # typed types
    "fun f(var: Map<Int, Int>)",
    "fun f(var: Map<Int)",
    "fun f(var: MapInt>)",
    "fun f(var: Map<<Int>)",
    "fun f(var: Map<<>>)",
    "fun f(var: Map<<>>)",
    "fun f(): Map<Int, Int>",
    "fun f(): Map<Int><Int>",
]

# start-up

if len(sys.argv) < 2:
    print("usage: python3 tests.py path-to-parser")

assert os.path.isfile(sys.argv[1]), "expected a file"

# set variables

path_to_parser = sys.argv[1]
test_file = "input.txt"

def launch(cases, expected_return_code):
    args = [path_to_parser, test_file]
    for case in cases:
        with open(test_file, 'w') as f:
            f.write(case)
        p = subprocess.run(args)
        
        assert p.returncode == expected_return_code, \
            "test: {}\n, expected return code: {}, got: {}" \
                .format(case, expected_return_code, p.returncode)


launch(success, 0)
launch(failures, 1)

print("Tests passed!")

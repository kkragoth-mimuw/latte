# PATH=$PATH:/usr/local/opt/llvm/bin/
import os
import subprocess
from enum import Enum

from box import Box
import glob
from colorama import Fore, Back, Style

config = Box({
    # "WHITELIST_TESTS": [],
    # "BLACKLIST_TESTS": [],
    "PATH": "tests/benkegood/"
})

class TestResult(Enum):
    OK = 1
    COMPILATION_ERROR = 2
    LLI_ERROR = 3
    OUTPUT_ERROR = 4

def run_tests_good():
    correctTests = []
    incorrectTests = Box({
        "compilation_error": [],
        "lli_error": [],
        "output_error": []
    })

    tests = []
    for file in os.listdir(config.PATH):
        if file.endswith(".lat"):
            tests.append(file)

    tests = sorted(tests, key=lambda x: os.path.splitext(x)[0])

    # tests = ([f for f in glob.glob(config.PATH + "core***.lat")])

    print(tests)

    for test in tests:
        res = run_test_good(test)
        if res == TestResult.OK:
            correctTests.append(test)
        else:
            if res == TestResult.COMPILATION_ERROR:
                incorrectTests.compilation_error.append(test)
            elif res == TestResult.LLI_ERROR:
                incorrectTests.lli_error.append(test)
            elif res == TestResult.OUTPUT_ERROR:
                incorrectTests.output_error.append(test)

    print(f'{Fore.YELLOW}Tests summary{Style.RESET_ALL}')
    print(f'Correct tests: {Fore.GREEN} {len(correctTests)} {Style.RESET_ALL}')
    print(f'Incorrect tests: {Fore.RED} {len(incorrectTests.compilation_error) + len(incorrectTests.lli_error) + len(incorrectTests.output_error)} {Style.RESET_ALL}')
    print(f'problematic tests:')
    if (len(incorrectTests.compilation_error) > 0):
        print(f'compilation errors: {incorrectTests.compilation_error}')
    if (len(incorrectTests.lli_error) > 0):
        print(f'lli errors: {incorrectTests.lli_error}')
    if (len(incorrectTests.output_error) > 0):
        print(f'output errors: {incorrectTests.output_error}')

def run_test_good(test) -> TestResult:
    print(f'Running test {test}..')
    result = subprocess.run(["./latte", config.PATH + test])
    if result.returncode != 0:
        return TestResult.COMPILATION_ERROR

    test_basename = test.partition(".")[0]
    test_bytecode = config.PATH + f'{test_basename}.bc'
    test_myoutput = config.PATH + f'{test_basename}.my_output'
    test_output = config.PATH + f'{test_basename}.output'

    with open(test_myoutput, 'w') as my_output_file:
        result = subprocess.run(["lli", test_bytecode], stdout=my_output_file)   
        if result.returncode != 0:
            return TestResult.LLI_ERROR

    result = subprocess.run(["diff", "-q", test_myoutput, test_output])

    if result.returncode == 0:
        return TestResult.OK
    else:
        return TestResult.OUTPUT_ERROR

if __name__ == "__main__":
    run_tests_good()
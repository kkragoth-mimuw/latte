# PATH=$PATH:/usr/local/opt/llvm/bin/
import os
import subprocess
import glob
from enum import Enum

from box import Box
from colorama import Fore, Back, Style


config = Box({
    "TEST_SETS": [
        {
            "NAME": "BENCORE",
            "WHITELIST_TESTS": [
            ],
            "PATH": "tests/benkegood/"
        },
        {
            "NAME": "STUDENTS",
            "WHITELIST_TESTS": [
            ],
            "PATH": "tests/basic/"
        },
        {
            "NAME": "MY",
            "WHITELIST_TESTS": [
            ],
            "PATH": "tests/my/"
        },
        {
            "NAME": "STRUCTS",
            "WHITELIST_TESTS": [
            ],
            "PATH": "tests/structs/"
        },
        {
            "NAME": "STRINGS_COMPARISONS",
            "WHITELIST_TESTS": [

            ],
            "PATH": "tests/stringsmy/"
        },
        {
            "NAME": "MY_OBJECTS",
            "WHITELIST_TESTS": [

            ],
            "PATH": "tests/myobjects/"
        },
        {
            "NAME": "C_TESTS",
            "WHITELIST_TESTS": [

            ],
            "PATH": "tests/cTests/"
        }
    ]
})


class TestResult(Enum):
    OK = 1
    COMPILATION_ERROR = 2
    LLI_ERROR = 3
    OUTPUT_ERROR = 4

def gather_all_tests_in_directory(dir):
    tests = []
    for file in os.listdir(dir):
        if file.endswith(".lat"):
            tests.append(file)

    tests = sorted(tests, key=lambda x: os.path.splitext(x)[0])

    return tests

def run_tests_good(dir, tests):
    correctTests = []
    incorrectTests = Box({
        "compilation_error": [],
        "lli_error": [],
        "output_error": []
    })

    for test in tests:
        res = run_test_good(dir, test)
        if res == TestResult.OK:
            correctTests.append(test)
        else:
            if res == TestResult.COMPILATION_ERROR:
                incorrectTests.compilation_error.append(test)
            elif res == TestResult.LLI_ERROR:
                incorrectTests.lli_error.append(test)
            elif res == TestResult.OUTPUT_ERROR:
                incorrectTests.output_error.append(test)
    
    return (correctTests, incorrectTests)

def print_summary(testSetName, correctTests, incorrectTests):
    incorrectTestsCount = len(incorrectTests.compilation_error) + len(incorrectTests.lli_error) + len(incorrectTests.output_error)
    print(f'{Fore.YELLOW}Tests summary for {testSetName}{Style.RESET_ALL}')
    print(f'Correct tests: {Fore.GREEN} {len(correctTests)} {Style.RESET_ALL}')
    print(f'Incorrect tests: {Fore.RED} {incorrectTestsCount} {Style.RESET_ALL}')
    if incorrectTestsCount > 0:
        print(f'List of problematic tests:')
        if (len(incorrectTests.compilation_error) > 0):
            print(f'compilation errors: {incorrectTests.compilation_error}')
        if (len(incorrectTests.lli_error) > 0):
            print(f'lli errors: {incorrectTests.lli_error}')
        if (len(incorrectTests.output_error) > 0):
            print(f'output errors: {incorrectTests.output_error}')

def run_test_good(dir, test) -> TestResult:
    print(f'Running test {test}..')
    result = subprocess.run(["./latc_llvm", dir + test])
    if result.returncode != 0:
        return TestResult.COMPILATION_ERROR

    test_basename = test.partition(".")[0]
    test_bytecode = dir + f'{test_basename}.bc'
    test_myoutput = dir + f'{test_basename}.my_output'
    test_output = dir + f'{test_basename}.output'
    test_input = dir + f'{test_basename}.input'

    with open(test_myoutput, 'w') as my_output_file:
        if os.path.exists(test_input):
            with open(test_input, 'r') as input_file:
                result = subprocess.run(["lli", test_bytecode], stdout=my_output_file, stdin=input_file)   
                if result.returncode != 0:
                    return TestResult.LLI_ERROR
        else:
            result = subprocess.run(["lli", test_bytecode], stdout=my_output_file)   
            if result.returncode != 0:
                return TestResult.LLI_ERROR

    result = subprocess.run(["diff", "-q", test_myoutput, test_output])

    if result.returncode == 0:
        return TestResult.OK
    else:
        return TestResult.OUTPUT_ERROR

if __name__ == "__main__":
    test_results = {}
    for test_set in config.TEST_SETS:
        tests = []
        if len(test_set.WHITELIST_TESTS) > 0:
            tests = test_set.WHITELIST_TESTS
        else:
            tests = gather_all_tests_in_directory(test_set.PATH)
        test_results[test_set.NAME] = run_tests_good(test_set.PATH, tests)

    for k, v in test_results.items():
        print_summary(k, v[0], v[1])

        # print(f'Running test set: {test_set.NAME}')
        # run_tests_good(tests)
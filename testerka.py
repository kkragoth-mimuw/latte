# PATH=$PATH:/usr/local/opt/llvm/bin/
import os
import subprocess
from box import Box
import glob
from colorama import Fore, Back, Style

config = Box({
    # "WHITELIST_TESTS": [],
    # "BLACKLIST_TESTS": [],
    "PATH": "tests/benkegood/"
})

def run_tests_good():
    correctTests = []
    incorrectTests = []

    tests = []
    for file in os.listdir(config.PATH):
        if file.endswith(".lat"):
            tests.append(file)

    tests = sorted(tests, key=lambda x: os.path.splitext(x)[0])

    # tests = ([f for f in glob.glob(config.PATH + "core***.lat")])

    print(tests)

    for test in tests:
        if run_test_good(test):
            correctTests.append(test)
        else:
            incorrectTests.append(test)

    print(f'{Fore.YELLOW}Tests summary{Style.RESET_ALL}')
    print(f'Correct tests: {Fore.GREEN} {len(correctTests)} {Style.RESET_ALL}')
    print(f'Incorrect tests: {Fore.RED} {len(incorrectTests)} {Style.RESET_ALL}')
    print(f'problematic tests: {incorrectTests}')

def run_test_good(test) -> bool:
    print(f'Running test {test}..')
    result = subprocess.run(["./latte", config.PATH + test])
    return result.returncode == 0

if __name__ == "__main__":
    run_tests_good()
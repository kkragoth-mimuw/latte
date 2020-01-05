import os
import subprocess
from box import Box
import glob

config = Box({
    # "WHITELIST_TESTS": [],
    # "BLACKLIST_TESTS": [],
    "PATH": "good/"
})

def run_tests_good():
    correctTests = []
    incorrectTests = []

    tests = ([f for f in glob.glob(config.PATH + "*.lat")])

    for test in tests:
        if run_test_good(test):
            correctTests.append(test)
        else:
            incorrectTests.append(test)

def run_test_good(test) -> bool:
    print(f'Running test {test}..')
    result = subprocess.Popen(f'../latte {test}')
    return result.returncode

if __name__ == "__main__":
    run_tests_good()
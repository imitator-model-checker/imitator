#!/usr/bin/env python3

import argparse
import re
import sys
from pathlib import Path


def parse_args():
    parser = argparse.ArgumentParser(
        description="Fail when test results exceed the allowed failure threshold."
    )
    parser.add_argument(
        "--log",
        type=Path,
        default=Path("tests/tests.log"),
        help="Path to the test log (default: tests/tests.log).",
    )
    parser.add_argument(
        "--max-failures",
        type=int,
        required=True,
        help="Maximum allowed number of failed benchmarks or test cases.",
    )
    return parser.parse_args()


def failed_count(log, name, all_passed_pattern):
    failed = re.search(
        r"^(\d+)/(\d+) {} failed\.$".format(name),
        log,
        re.MULTILINE,
    )
    if failed:
        return int(failed.group(1)), int(failed.group(2))

    all_passed = re.search(all_passed_pattern, log, re.MULTILINE)
    if all_passed:
        return 0, int(all_passed.group(2))

    raise ValueError("Could not find the {} summary".format(name))


def main():
    args = parse_args()

    if args.max_failures < 0:
        sys.exit("--max-failures must be zero or greater")

    if not args.log.is_file():
        sys.exit("Test log was not generated: {}".format(args.log))

    log = args.log.read_text(encoding="utf-8", errors="replace")

    try:
        failed_benchmarks, total_benchmarks = failed_count(
            log,
            "benchmarks",
            r"^.*All benchmarks \((\d+)/(\d+)\) passed successfully\.$",
        )
        failed_test_cases, total_test_cases = failed_count(
            log,
            "test cases",
            r"^All test cases \((\d+)/(\d+)\) passed successfully\.$",
        )
    except ValueError as error:
        sys.exit("{} in {}".format(error, args.log))

    print(
        "Failed benchmarks: {}/{}; failed test cases: {}/{}; "
        "maximum allowed: {}".format(
            failed_benchmarks,
            total_benchmarks,
            failed_test_cases,
            total_test_cases,
            args.max_failures,
        )
    )

    if (
        failed_benchmarks > args.max_failures
        or failed_test_cases > args.max_failures
    ):
        sys.exit("Test failure threshold exceeded")


if __name__ == "__main__":
    main()

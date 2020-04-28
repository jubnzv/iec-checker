import argparse
import os
import sys
from typing import List

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "src"))
from python.core import run_checker  # noqa
from python.dump import process_dump, remove_dump  # noqa


def main(files: List[str]):
    for f in files:
        checker_warnings, errors, rc = run_checker(f)
        if rc != 0:
            for w in checker_warnings:
                print(f'{w}')
            continue

        dump_name = f'{f}.dump.json'
        plugins_warnings = process_dump(dump_name)
        remove_dump(dump_name)

        print(f'Report for {f}:')
        if checker_warnings or plugins_warnings:
            for w in checker_warnings:
                print(f'{w}')
            for p in plugins_warnings:
                print(f'{w}')
        else:
            print('No errors found!')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs='*', help="Path to IEC source files")
    args = parser.parse_args()
    sys.exit(main(args.files))

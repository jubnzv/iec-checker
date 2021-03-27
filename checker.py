import argparse
import os
import sys
from typing import List

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "src"))
from python.core import run_checker  # noqa
from python.dump import DumpManager  # noqa
from python.plugins.cfg_plotter import CFGPlotter  # noqa


def main(files: List[str], draw_cfg: str = "",
         binary: str = "../output/bin/iec_checker"):
    for f in files:
        if not os.path.isfile(f):
            continue
        checker_warnings, rc = run_checker(f, binary)
        if rc != 0:
            print(f'Report for {f}:')
            for w in checker_warnings:
                print(f'{w}')
            continue

        dump_name = f'{f}.dump.json'
        plugins_warnings = []
        with DumpManager(dump_name) as dm:
            plugins_warnings = dm.run_all_inspections()
            if draw_cfg:
                cfg_plotter = CFGPlotter(dm.scheme.cfgs)
                cfg_plotter.save_file(draw_cfg)

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
    parser.add_argument("--draw-cfg", type=str,
                        help="Save control flow graph image at the selected path")
    parser.add_argument("-b","--binary", default="../output/bin/iec_checker",
                        help="File path to the OCaml binary")
    args = parser.parse_args()
    sys.exit(main(args.files, args.draw_cfg, args.binary))

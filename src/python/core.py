"""
Module to communicate with OCaml core.
"""
from typing import List, Tuple
import io
import subprocess
import ijson

from .om import Error, Warning


def process_output(json_out: str) -> List[Warning]:
    warnings = []
    for item in ijson.items(io.BytesIO(json_out), ""):
        if item:
            warnings.append(Warning.from_dict(item.pop()))
    return warnings


def run_checker(file_path: str) -> Tuple[List[Warning], List[Error], int]:
    """Run iec-checker core for a given file.

    This will execute core inspections and generate JSON dump processed with
    plugins."""
    p = subprocess.Popen(["output/bin/iec_checker",
                          "-output-format", "json",
                          "-quiet", "true",
                          "-dump", "true",
                          file_path],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    p.wait()
    out, err = p.communicate()
    warnings = process_output(out)
    return (warnings, [], p.returncode)

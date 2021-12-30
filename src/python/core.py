"""
Module to communicate with OCaml core.
"""
from typing import List, Tuple
import io
import os
import subprocess
import ijson

from .om import Warning

binary_default = os.path.join("..", "output", "bin", "iec_checker")


def process_output(json_out: bytes) -> List[Warning]:
    warnings = []
    for warns in ijson.items(io.BytesIO(json_out), ""):
        for item in warns:
            if item:
                warnings.append(Warning.from_dict(item))
    return warnings


def check_program(program: str,
                  binary: str = binary_default) -> Tuple[List[Warning], int]:
    """Run iec-checker core and send given program source in stdin.
    This will create 'stdin.dump.json' dump file in a current directory.
    """
    p = subprocess.Popen([binary, "-o", "json", "-q", "-d", "-"],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT,
                         stdin=subprocess.PIPE,
                         encoding='utf8')
    out, err = p.communicate(f'{program}\n')
    p.wait()
    warnings = process_output(out.encode())
    return (warnings, p.returncode)


def run_checker(file_path: str, binary: str = binary_default,
                *args) -> Tuple[List[Warning], int]:
    """Run iec-checker core for a given file.
    This will execute core inspections and generate JSON dump processed with
    plugins."""
    p = subprocess.Popen([binary, "-o", "json", "-q", "-d", *args, file_path],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    p.wait()
    out, err = p.communicate()
    warnings = process_output(out)
    return (warnings, p.returncode)


def run_checker_full_out(file_path: str, binary: str = binary_default,
                *args) -> Tuple[int, str]:
    """Run iec-checker core for a given file and capture the whole output.
    No extra options will be set by default."""
    p = subprocess.Popen([binary, *args, file_path],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    p.wait()
    out, err = p.communicate()
    return (p.returncode, '\n'.join([str(out), str(err)]))


def filter_warns(warns: List[Warning], warn_id: str) -> List[Warning]:
    """Filter warning by identifier."""
    return list(filter(lambda w: w.id == warn_id, warns))

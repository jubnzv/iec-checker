"""
Module to communicate with OCaml core.
"""
from dataclasses import dataclass
from typing import List, Tuple
import io
import subprocess
import ijson


@dataclass
class Warning:
    """Warning found by OCaml core."""
    linenr: int
    column: int
    id: str
    msg: str

    @classmethod
    def from_dict(cls, values):
        args = {}
        args['linenr'] = values.get('linenr', -1)
        args['column'] = values.get('column', -1)
        args['id'] = values.get('id', -1)
        args['msg'] = values.get('msg', '')
        return Warning(**args)

    def __str__(self):
        return f"{self.linenr}:{self.column}: {self.id}: {self.msg}"


@dataclass
class Error:
    """Error produced when OCaml core fails."""
    msg: str


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
    # TODO: Errors not in json
    warnings = process_output(out)
    return (warnings, [], p.returncode)

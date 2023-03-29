"""
Routines to work with dump files generated by the ``iec-checker`` binary.
"""
import os
import logging
from dataclasses import dataclass
from typing import List, Optional
import ijson

from .om import Scheme


log = logging.getLogger('plugins')
log.setLevel(logging.DEBUG)


@dataclass
class PluginWarning:
    """Inspection message generated by the Python plugin."""
    msg: str


class CheckerError(Exception):
    """Internal exception generated by the checker."""


class DumpManager:
    """Class that incapsulates logic over ObjectModel unmarshalled from the
    generated dump files."""

    def __init__(self, dump_path: str):
        self.dump_path: str = dump_path
        self.scheme: Optional[Scheme] = None

    def __enter__(self):
        self.scheme = self.mk_scheme()
        if not self.scheme:
            raise CheckerError(
                f'Can\'t extract dump scheme from {self.dump_path}!')
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.remove_dump()

    def run_all_inspections(self) -> List[PluginWarning]:
        """Run all inspections implemented as Python plugins."""
        return []

    def mk_scheme(self) -> Scheme:
        scheme = None
        with open(self.dump_path, 'rb') as f:
            for item in ijson.items(f, ""):
                scheme = Scheme.from_dict(item)
        if not scheme:
            raise Exception(f"Cannot parse JSON scheme from {self.dump_path}")
        return scheme

    def remove_dump(self):
        """Remove processed dump file."""
        try:
            os.remove(self.dump_path)
        except OSError as e:
            raise CheckerError(f'Can\'t remove {self.dump_path}: {str(e)}')

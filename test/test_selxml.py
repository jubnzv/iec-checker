import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import (run_checker_full_out,
                         binary_default,
                         filter_warns)  # noqa
from python.dump import DumpManager  # noqa


def test_sel_rtac():
    rc, out = run_checker_full_out('selxml/SEL_RTAC/', binary_default, "-v", "-i", "selxml")
    assert rc == 0
    assert("Parsing selxml/SEL_RTAC/ProjSpace_MAIN_POU.xml" in out)
    assert("Parsing selxml/SEL_RTAC/Tag Processor.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Minimal.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_GVL1.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Simple.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Example.xml" in out)


def test_recurse_dirs():
    """Test whether the checker will recursively looking up for files in the
    nested directories."""
    rc, out = run_checker_full_out('selxml', binary_default, "-v", "-i", "selxml")
    assert rc == 0
    assert("Parsing selxml/SEL_RTAC/ProjSpace_MAIN_POU.xml" in out)
    assert("Parsing selxml/SEL_RTAC/Tag Processor.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Minimal.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_GVL1.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Simple.xml" in out)
    assert("Parsing selxml/SEL_RTAC/ProjSpace_Example.xml" in out)

import os

import lit.formats

config.name = "temp-opt"
config.test_format = lit.formats.ShTest(execute_external=True)

this_dir = os.path.dirname(__file__)

# Source tree root is the parent of either:
# - the source test directory, or
# - the build test directory.
src_root = os.path.dirname(os.path.dirname(this_dir))

config.test_source_root = os.path.join(src_root, "test")
config.test_exec_root = this_dir

config.suffixes = [".cpp"]

# Do not descend into Inputs/ when discovering tests.
config.excludes = ["Inputs"]

temp_opt_path = os.path.join(src_root, "build", "temp-opt")
config.substitutions.append(("%temp_opt", temp_opt_path))

config.substitutions.append(("%FileCheck", "FileCheck"))

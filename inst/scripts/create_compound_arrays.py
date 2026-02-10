# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "anndata",
#     "h5py",
#     "numpy"
# ]
# ///

# Related issues:
# - https://github.com/Huber-group-EMBL/rhdf5/issues/192

import numpy as np
import h5py
import anndata as ad

h5_file = h5py.File("inst/testfiles/compound_examples.h5", "a")

homogeneous_compound_array = np.array([(1, 2, 3), (9, 10, 11)], dtype=[("col_1", "i4"), ("col_2", "i4"), ("col_3", "i4")])
ad.io.write_elem(h5_file, "homogeneous_compound_array", homogeneous_compound_array)

heterogeneous_compound_array = np.array([(1, 2.0, "a"), (9, 10.0, "b")], dtype=[("col_1", "i4"), ("col_2", "f4"), ("col_3", "S1")])
ad.io.write_elem(h5_file, "heterogeneous_compound_array", heterogeneous_compound_array)

h5_file.close()


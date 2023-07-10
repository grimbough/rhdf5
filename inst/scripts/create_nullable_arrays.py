import numpy as np
import pandas as pd
import anndata as ad
import h5py

h5_file = h5py.File("/data/nullable_examples.h5", "a")
int_array = pd.array([1, None, 3, 4], dtype="Int32")
ad.experimental.write_elem(h5_file, "nullable_integer", int_array)

bool_array = pd.array([True, False, pd.NA], dtype="boolean")
ad.experimental.write_elem(h5_file, "nullable_boolean", bool_array)

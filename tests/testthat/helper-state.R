# Ensure no open HDF5 objects remain after each test
set_state_inspector(function() {
  list(
    open_h5_connections = h5validObjects()
  )
})

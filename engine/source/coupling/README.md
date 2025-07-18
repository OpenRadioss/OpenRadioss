# Coupling Adapter Infrastructure (preCICE & CWIPI)

This directory implements the coupling infrastructure for OpenRadioss, enabling data exchange between OpenRadioss and external solvers via the preCICE and CWIPI libraries. The design supports both C++ and Fortran integration, with a separation between the C interface, C++ adapters, and Fortran bindings.


## Architecture Overview
We are using preCICE v3 API, and CWIPI default (legacy) API for coupling. The architecture consists of:

- **C Interface (`coupling_c_interface.cpp`/`.h`)**: Exposes a C API for use by Fortran and other languages. All coupling operations (create, configure, initialize, data exchange, advance, finalize) are routed through this interface.
- **C++ Adapters**:
  - `PreciceCouplingAdapter`: Implements coupling using the preCICE library.
  - `CwipiCouplingAdapter`: Implements coupling using the CWIPI library.
  - `DummyCouplingAdapter`: Fallback for builds without preCICE or CWIPI.
- **Factory (`coupling_factory.cpp`)**: Instantiates the correct adapter at runtime based on build flags.
- **Fortran Interface (`coupling_adapter.F90`)**: Fortran module that calls the C interface for coupling operations.

## Main Data Flow

1. **Creation**: Fortran or C code calls `coupling_adapter_create()` to obtain a new adapter instance.
2. **Configuration**: `coupling_adapter_configure()` loads a config file and sets up the adapter (participant names, mesh, data types, etc.).
3. **Mesh/Node Setup**: `coupling_adapter_set_nodes()` and (for CWIPI) `coupling_adapter_set_mesh()` define the mesh and coupling nodes.
4. **Initialization**: `coupling_adapter_initialize()` sets up the coupling, mesh vertices, and communication.
5. **Data Exchange**: `coupling_adapter_write_data()` and `coupling_adapter_read_data()` transfer data (displacements, forces, positions) between OpenRadioss and the coupled solver.
6. **Advance**: `coupling_adapter_advance()` advances the coupling by one time step.
7. **Finalization**: `coupling_adapter_finalize()` cleans up resources.

## Adding a New Adapter

1. Implement a new subclass of `CouplingAdapter` in C++.
2. Implement all required virtual methods (see `coupling.h`).
3. Add the new adapter to `coupling_factory.cpp`.
4. Update the C interface if new functionality is needed.
5. Document the new adapter in this README.

## Key Files

- `coupling.h`: Abstract base class for all adapters.
- `precice_coupling_adapter.h/cpp`: preCICE adapter implementation.
- `cwipi_coupling_adapter.h/cpp`: CWIPI adapter implementation.
- `coupling_c_interface.h/cpp`: C interface for Fortran and C++.
- `coupling_factory.cpp`: Adapter instantiation logic.
- `coupling_adapter.F90`: Fortran interface.

## Function Mapping Table

| C Interface Function                | preCICE Adapter Method                | CWIPI Adapter Method                  | Underlying Library Calls           |
|-------------------------------------|---------------------------------------|--------------------------------------|------------------------------------|
| `coupling_adapter_create`           | `PreciceCouplingAdapter()`            | `CwipiCouplingAdapter()`              | -                                  |
| `coupling_adapter_destroy`          | Destructor                            | Destructor                           | finalize(), `cwipi_finalize()`       |
| `coupling_adapter_configure`        | `configure()`                         | `configure()`                        | `precice::Participant`, `cwipi_init`   |
| `coupling_adapter_set_nodes`        | `setNodes()`                          | `setNodes()`                         | -                                  |
| `coupling_adapter_set_mesh`         | -                                     | `setMesh()`                          | -                                  |
| `coupling_adapter_initialize`       | `initialize()`                        | `initialize()`                       | `precice::Participant`, `cwipi_create_coupling`, `cwipi_define_mesh`, `cwipi_locate` |
| `coupling_adapter_write_data`       | `writeData()`                         | `writeData()`                        | `precice::writeData`, `cwipi_issend`   |
| `coupling_adapter_read_data`        | `readData()`                          | `readData()`                         | `precice::readData`, `cwipi_irecv`     |
| `coupling_adapter_advance`          | `advance()`                           | `advance()`                          | `precice::advance`, `cwipi_locate`     |
| `coupling_adapter_finalize`         | `finalize()`                          | `finalize()`                         | `precice::finalize`, `cwipi_finalize`  |

## Build Flags

- run `./build_script.sh -arch=linux64_gf -mpi=ompi -cwipi=/absolute/path/to/cwipi`. It will define `WITH_CWIPI` that enables  adapter.
- run `./build_script.sh -arch=linux64_gf -mpi=ompi -preCICE`. It will define `WITH_PRECICE` that enables preCICE adapter.

## References
- [preCICE Documentation](https://precice.org/)
- [CWIPI Github](https://github.com/onera/cwipi)

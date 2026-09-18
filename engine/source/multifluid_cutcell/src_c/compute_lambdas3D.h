#ifndef COMPUTE_LAMBDAS3D_H
#define COMPUTE_LAMBDAS3D_H

#include "general_clipping.h"
#include "vector_double.h"
#include "array_double.h"
#include "array_Points.h"
#include "my_real_c.inc"

Polyhedron3D* clip3D(const Polyhedron3D *clipper, const Polyhedron3D *clipped);
void compute_lambdas2D_time(const Polyhedron3D* grid, const Polyhedron3D *initial_p, \
                            Vector_points3D **lambdas, Vector_points3D **normals_ptr, Vector_int64 **edge_indices, bool *is_narrowband);
void compute_lambdas2D(const Polygon2D* grid, const Polyhedron3D *clipped, const my_real_c dt, \
                        Array_double **lambdas_arr, Vector_double** big_lambda_n, Vector_double** big_lambda_np1, \
                        Vector_points3D **normals_ptr, Vector_int64 **edge_indices, bool *is_narrowband);

#endif
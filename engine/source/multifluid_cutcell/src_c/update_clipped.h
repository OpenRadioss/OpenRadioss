#ifndef UPDATE_CLIPPED_H
#define UPDATE_CLIPPED_H

#include "my_real_c.inc"
#include "Polygon2D.h"
#include "Polyhedron3D.h"
#include "array_int.h"
#include <stdint.h>

Polyhedron3D* build_space2D_time_cell(const Polygon2D *fn, const my_real_c *vsx, const my_real_c* vsy, uint64_t size_vs, const my_real_c dt, bool split, Array_int *list_del_pts);
void update_solid(Polygon2D **solid, Polyhedron3D** solid3D, const my_real_c* vec_move_solidx, const my_real_c* vec_move_solidy, my_real_c dt, \
                    my_real_c minimal_length, my_real_c maximal_length, my_real_c minimal_angle);
#endif
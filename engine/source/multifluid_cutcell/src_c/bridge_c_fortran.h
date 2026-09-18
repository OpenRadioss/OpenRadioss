#ifndef BRIDGE_C_FORTRAN_H
#define BRIDGE_C_FORTRAN_H

#include "my_real_c.inc"
#include "Polygon2D.h"


void launch_grb_();
void end_grb_();

void build_grid_from_points_fortran_(const my_real_c* x_v, const my_real_c* y_v, long long int* pt_indices, const long long int *signed_nb_pts);
void build_clipped_from_pts_fortran_(const my_real_c* x_v, const my_real_c* y_v, const long long int* limits, const long long *nb_polygons);

void compute_lambdas2d_fortran_(const my_real_c *dt, \
                        my_real_c *ptr_lambdas_arr,    \
                        my_real_c *ptr_big_lambda_n,   \
                        my_real_c *ptr_big_lambda_np1, \
                        my_real_c *normals_x, my_real_c *normals_y, my_real_c *normals_t,\
                        long long* edge_indices, long long* max_length_array, long long* nb_normals, \
                        long long int *is_narrowband_ptr);
void nb_pts_clipped_fortran_(long long int* signed_nb_pts_solid);
void nb_edge_clipped_fortran_(long long int* signed_nb_edges_solid);
void compute_normals_clipped_fortran_(my_real_c* normalVecx, my_real_c* normalVecy, \
                                        my_real_c* min_pos_Se);
void smooth_vel_clipped_fortran_(my_real_c* vec_move_clippedx, my_real_c* vec_move_clippedy, my_real_c* min_pos_Se, my_real_c *dt);
void get_clipped_ith_vertex_fortran_(long long int *k, Point2D *pt);
void get_clipped_edges_ith_vertex_fortran_(long long int *k, long long int *signed_eR, long long int *signed_eL);
void update_clipped_fortran_(const my_real_c* vec_move_clippedy, const my_real_c* vec_move_clippedz, const my_real_c* dt,\
                            long long* new_nb_edges, \
                            my_real_c *minimal_length, my_real_c *maximal_length, my_real_c *minimal_angle);
/// @brief List all points of the clipped polygon into x_v_clipped and y_v_clipped arrays.
/// @details The limits_polygons array gives the start index of each polygon in the clipped solid. 
////          Its size is nb_polygons+1, with limits_polygons[0]=0 and limits_polygons[nb_polygons]=total number of points in the clipped solid.
////          For polygon i, its points are stored from index limits_polygons[i] to limits_polygons[i+1]-1 in the x_v_clipped and y_v_clipped arrays.
/// @param x_v_clipped x coordinates of clipped polygon points
/// @param y_v_clipped y coordinates of clipped polygon points
/// @param limits_polygons Index limits of each polygon in the clipped solid
void output_clipped_fortran_(my_real_c* x_v_clipped, my_real_c* y_v_clipped, long long* limits_polygons);

#endif
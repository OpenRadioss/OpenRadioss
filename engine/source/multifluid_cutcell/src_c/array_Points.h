#ifndef ARRAY_POINTS_H
#define ARRAY_POINTS_H

#include "Point.h"
#include <stdint.h>

/*
Module defining dynamic 2D array of Point2D structure
*/

typedef struct {
    Point2D* points;
    uint64_t ncols, nrows, capacity;
} Array_points2D;

void double_capacity_arr_pts2D(Array_points2D* v);
Array_points2D* alloc_empty_arr_pts2D();
Array_points2D* alloc_with_init_arr_pts2D(const Point2D* points, const uint64_t nrows, const uint64_t ncols);
Array_points2D* alloc_with_capacity_arr_pts2D(const uint64_t nrows, const uint64_t ncols);
void dealloc_arr_pts2D(Array_points2D* v);
Point2D* get_ijth_elem_arr_pts2D(const Array_points2D* v, const uint64_t i, const uint64_t j);
void set_ijth_elem_arr_pts2D(Array_points2D* v, const uint64_t i, const uint64_t j, const Point2D* p);
void copy_arr_pts2D(const Array_points2D* src, Array_points2D* dest);
void print_arr_pt2D(const Array_points2D *p);

typedef struct {
    Point3D* points;
    uint64_t ncols, nrows, capacity;
} Array_points3D;

void double_capacity_arr_pts3D(Array_points3D* v);
Array_points3D* alloc_empty_arr_pts3D();
Array_points3D* alloc_with_init_arr_pts3D(const Point3D* points, const uint64_t nrows, const uint64_t ncols);
Array_points3D* alloc_with_capacity_arr_pts3D(const uint64_t nrows, const uint64_t ncols);
void dealloc_arr_pts3D(Array_points3D* v);
Point3D* get_ijth_elem_arr_pts3D(const Array_points3D* v, const uint64_t i, const uint64_t j);
void set_ijth_elem_arr_pts3D(Array_points3D* v, const uint64_t i, const uint64_t j, const Point3D* p);
void copy_arr_pts3D(const Array_points3D* src, Array_points3D* dest);
void print_arr_pt3D(const Array_points3D *p);

typedef struct {
    Point4D* points;
    uint64_t ncols, nrows, capacity;
} Array_points4D;

void double_capacity_arr_pts4D(Array_points4D* v);
Array_points4D* alloc_empty_arr_pts4D();
Array_points4D* alloc_with_init_arr_pts4D(const Point4D* points, const uint64_t nrows, const uint64_t ncols);
Array_points4D* alloc_with_capacity_arr_pts4D(const uint64_t nrows, const uint64_t ncols);
void dealloc_arr_pts4D(Array_points4D* v);
Point4D* get_ijth_elem_arr_pts4D(const Array_points4D* v, const uint64_t i, const uint64_t j);
void set_ijth_elem_arr_pts4D(Array_points4D* v, const uint64_t i, const uint64_t j, const Point4D* p);
void copy_arr_pts4D(const Array_points4D* src, Array_points4D* dest);
void print_arr_pt4D(const Array_points4D *p);


#endif
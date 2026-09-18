#ifndef VECTOR_POINTS_H
#define VECTOR_POINTS_H

#include "Point.h"
#include <stdint.h>

/*
Structure of dynamical 1D array of Point2D (mimic std::Vector<Point2D>).
*/
typedef struct {
    Point2D* points;
    uint64_t size, capacity;
} Vector_points2D;

void push_back_vec_pts2D(Vector_points2D** v_ptr, const Point2D* point);
void double_capacity_vec_pts2D(Vector_points2D* v);
Vector_points2D* alloc_empty_vec_pts2D();
Vector_points2D* alloc_with_init_vec_pts2D(const Point2D* points, uint64_t size);
Vector_points2D* alloc_with_capacity_vec_pts2D(uint64_t size);
void dealloc_vec_pts2D(Vector_points2D* v);
Point2D* get_ith_elem_vec_pts2D(const Vector_points2D* v, uint64_t i);
void set_ith_elem_vec_pts2D(Vector_points2D* v, uint64_t i, const Point2D* p);
void copy_vec_pts2D(const Vector_points2D* src, Vector_points2D* dest);
void print_vec_pt2D(const Vector_points2D p);
Vector_points2D* cat_vec_pts2D(const Vector_points2D* v1, const Vector_points2D* v2);

typedef struct {
    Point3D* points;
    uint64_t size, capacity;
} Vector_points3D;

void push_back_vec_pts3D(Vector_points3D** v_ptr, const Point3D* point);
void double_capacity_vec_pts3D(Vector_points3D* v);
Vector_points3D* alloc_empty_vec_pts3D();
Vector_points3D* alloc_with_init_vec_pts3D(const Point3D* points, uint64_t size);
Vector_points3D* alloc_with_capacity_vec_pts3D(uint64_t size);
void dealloc_vec_pts3D(Vector_points3D* v);
Point3D* get_ith_elem_vec_pts3D(const Vector_points3D* v, uint64_t i);
void set_ith_elem_vec_pts3D(Vector_points3D* v, uint64_t i, const Point3D* p);
void copy_vec_pts3D(const Vector_points3D* src, Vector_points3D* dest);
void print_vec_pt3D(const Vector_points3D p);
Vector_points3D* cat_vec_pts3D(const Vector_points3D* v1, const Vector_points3D* v2);

typedef struct {
    Point4D* points;
    uint64_t size, capacity;
} Vector_points4D;

void push_back_vec_pts4D(Vector_points4D** v_ptr, const Point4D* point);
void double_capacity_vec_pts4D(Vector_points4D* v);
Vector_points4D* alloc_empty_vec_pts4D();
Vector_points4D* alloc_with_init_vec_pts4D(const Point4D* points, uint64_t size);
Vector_points4D* alloc_with_capacity_vec_pts4D(uint64_t size);
void dealloc_vec_pts4D(Vector_points4D* v);
Point4D* get_ith_elem_vec_pts4D(const Vector_points4D* v, uint64_t i);
void set_ith_elem_vec_pts4D(Vector_points4D* v, uint64_t i, const Point4D* p);
void copy_vec_pts4D(const Vector_points4D* src, Vector_points4D* dest);
void print_vec_pt4D(const Vector_points4D p);
Vector_points4D* cat_vec_pts4D(const Vector_points4D* v1, const Vector_points4D* v2);


#endif
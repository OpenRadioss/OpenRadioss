#ifndef VECTOR_INDICES_INTERSECTION_H
#define VECTOR_INDICES_INTERSECTION_H

#include "stdint.h"

/*
Structure used for edges intersection (index of edge intersecting and point associated).
Also, mimic of std::Vector<Indices_intersection> defined hereafter.
*/
typedef struct {
    uint64_t i_e;
    uint64_t i_pt;
} Indices_intersection;

typedef struct {
    Indices_intersection* data;
    uint64_t size, capacity;
} Vector_indices_intersec;

void push_back_vec_indices_intersec(Vector_indices_intersec** v_ptr, const Indices_intersection* point);
void push_back_unique_vec_indices_intersec(Vector_indices_intersec** v_ptr, const Indices_intersection* point);
void double_capacity_vec_indices_intersec(Vector_indices_intersec* v);
Vector_indices_intersec* alloc_empty_vec_indices_intersec();
Vector_indices_intersec* alloc_with_init_vec_indices_intersec(const Indices_intersection* points, uint64_t size);
Vector_indices_intersec* alloc_with_capacity_vec_indices_intersec(uint64_t size);
void dealloc_vec_indices_intersec(Vector_indices_intersec* v);
Indices_intersection* get_ith_elem_vec_indices_intersec(const Vector_indices_intersec* v, uint64_t i);
void set_ith_elem_vec_indices_intersec(Vector_indices_intersec* v, uint64_t i, Indices_intersection* d);
void copy_vec_indices_intersec(const Vector_indices_intersec* src, Vector_indices_intersec* dest);
Vector_indices_intersec* cat_vec_indices_intersec(const Vector_indices_intersec* v1, const Vector_indices_intersec* v2);


#endif
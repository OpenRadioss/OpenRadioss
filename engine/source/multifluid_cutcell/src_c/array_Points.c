#include "array_Points.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

//----------------------------------------------------------------------------------------
//                                 2D POINTS
//----------------------------------------------------------------------------------------
Array_points2D* alloc_empty_arr_pts2D(){
    Array_points2D* v = (Array_points2D*)malloc(sizeof(Array_points2D));
    v->points = (Point2D*)malloc(sizeof(Point2D));
    v->nrows = 0;
    v->ncols = 0;
    v->capacity = 1;

    return v;
}

//Allocated with all data = 0
Array_points2D* alloc_with_capacity_arr_pts2D(const uint64_t nrows, const uint64_t ncols){
    Array_points2D* v = (Array_points2D*)malloc(sizeof(Array_points2D));
    v->points = (Point2D*)calloc(nrows*ncols, sizeof(Point2D));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

Array_points2D* alloc_with_init_arr_pts2D(const Point2D* points, const uint64_t nrows, const uint64_t ncols){
    Array_points2D* v;
    if (points!=NULL){
        v = (Array_points2D*)malloc(sizeof(Array_points2D));
        v->points = (Point2D*)malloc(nrows*ncols*sizeof(Point2D));
        memcpy(v->points, points, nrows*ncols*sizeof(Point2D));
        v->nrows = nrows;
        v->ncols = ncols;
        v->capacity = nrows*ncols;
    } else {
        v = NULL;
    }

    return v;
}

void double_capacity_arr_pts2D(Array_points2D* v){
    v->capacity *= 2;
    v->points = (Point2D*) realloc(v->points, v->capacity*sizeof(Point2D));
}

void dealloc_arr_pts2D(Array_points2D* v){
    if (v!=NULL){
        free(v->points);
        v->points = NULL;
        v->capacity = 0;
        v->nrows = 0;
        v->ncols = 0;
    }
}

Point2D* get_ijth_elem_arr_pts2D(const Array_points2D* v, const uint64_t i, const uint64_t j){
    Point2D* p;
    if ((i<v->nrows) && (j<v->ncols)) {
        return v->points + i*v->ncols + j;
    } else {
        p = (Point2D*) malloc(sizeof(Point2D));
        p->x = nan("");
        p->y = nan("");
        return p;
    }
}

void set_ijth_elem_arr_pts2D(Array_points2D* v, const uint64_t i, const uint64_t j, const Point2D* p){
    if (j >= v->ncols){
        printf("ERROR in set set_ijth_elem_arr_pts2D : j bigger than number of columns!");
    }
    while(i*v->ncols+j >= v->capacity) double_capacity_arr_pts2D(v);
    if (i >= v->nrows) v->nrows = i+1;
    
    v->points[i*v->ncols+j] = *p;
}

void copy_arr_pts2D(const Array_points2D* src, Array_points2D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->nrows = 0;
            dest->ncols = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point2D*)malloc(src->capacity*sizeof(Point2D));
            memcpy(dest->points, src->points, src->nrows*src->ncols*sizeof(Point2D));
            dest->nrows = src->nrows;
            dest->ncols = src->ncols;
            dest->capacity = src->capacity;
        }
    }
}

void print_arr_pt2D(const Array_points2D *p){
    unsigned long int i, j;
    Point2D* pt;

    printf("vector p = [");
    for (i = 0; i<p->nrows-1; i++){
        for (j = 0; j<p->ncols-1; j++){
            pt = get_ijth_elem_arr_pts2D(p, i, j);
            print_pt2D(pt);
        }
        pt = get_ijth_elem_arr_pts2D(p, i, p->ncols-1);
        print_pt2D(pt);
    }
    for (j = 0; j<p->ncols-1; j++){
        pt = get_ijth_elem_arr_pts2D(p, p->nrows-1, j);
        print_pt2D(pt);
    }
    pt = get_ijth_elem_arr_pts2D(p, p->nrows-1, p->ncols-1);
    print_pt2D(pt);
    printf("]\n");
}

//----------------------------------------------------------------------------------------
//                                 3D POINTS
//----------------------------------------------------------------------------------------
Array_points3D* alloc_empty_arr_pts3D(){
    Array_points3D* v = (Array_points3D*)malloc(sizeof(Array_points3D));
    v->points = (Point3D*)malloc(sizeof(Point3D));
    v->nrows = 0;
    v->ncols = 0;
    v->capacity = 1;

    return v;
}

Array_points3D* alloc_with_capacity_arr_pts3D(const uint64_t nrows, const uint64_t ncols){
    Array_points3D* v = (Array_points3D*)malloc(sizeof(Array_points3D));
    v->points = (Point3D*)calloc(nrows*ncols, sizeof(Point3D));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

Array_points3D* alloc_with_init_arr_pts3D(const Point3D* points, const uint64_t nrows, const uint64_t ncols){
    Array_points3D* v = (Array_points3D*)malloc(sizeof(Array_points3D));
    v->points = (Point3D*)malloc(nrows*ncols*sizeof(Point3D));
    memcpy(v->points, points, nrows*ncols*sizeof(Point3D));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

void double_capacity_arr_pts3D(Array_points3D* v){
    v->capacity *= 2;
    v->points = (Point3D*) realloc(v->points, v->capacity*sizeof(Point3D));
}

void dealloc_arr_pts3D(Array_points3D* v){
    if (v!=NULL){
        free(v->points);
        v->points = NULL;
        v->capacity = 0;
        v->nrows = 0;
        v->ncols = 0;
    }
}

Point3D* get_ijth_elem_arr_pts3D(const Array_points3D* v, const uint64_t i, const uint64_t j){
    Point3D* p;
    if ((i<v->nrows) && (j<v->ncols)) {
        return v->points + i*v->ncols + j;
    } else {
        p = (Point3D*) malloc(sizeof(Point3D));
        p->x = nan("");
        p->y = nan("");
        p->t = nan("");
        return p;
    }
}

void set_ijth_elem_arr_pts3D(Array_points3D* v, const uint64_t i, const uint64_t j, const Point3D* p){
    if (j >= v->ncols){
        printf("ERROR in set set_ijth_elem_arr_pts3D : j bigger than number of columns!");
    }
    while(i*v->ncols+j >= v->capacity) double_capacity_arr_pts3D(v);
    if (i >= v->nrows) v->nrows = i+1;
    
    v->points[i*v->ncols+j] = *p;
}

void copy_arr_pts3D(const Array_points3D* src, Array_points3D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->nrows = 0;
            dest->ncols = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point3D*)malloc(src->capacity*sizeof(Point3D));
            memcpy(dest->points, src->points, src->nrows*src->ncols*sizeof(Point3D));
            dest->nrows = src->nrows;
            dest->ncols = src->ncols;
            dest->capacity = src->capacity;
        }
    }
}

void print_arr_pt3D(const Array_points3D *p){
    unsigned long int i, j;
    Point3D* pt;

    printf("vector p = [");
    for (i = 0; i<p->nrows-1; i++){
        for (j = 0; j<p->ncols-1; j++){
            pt = get_ijth_elem_arr_pts3D(p, i, j);
            print_pt3D(pt);
        }
        pt = get_ijth_elem_arr_pts3D(p, i, p->ncols-1);
        print_pt3D(pt);
    }
    for (j = 0; j<p->ncols-1; j++){
        pt = get_ijth_elem_arr_pts3D(p, p->nrows-1, j);
        print_pt3D(pt);
    }
    pt = get_ijth_elem_arr_pts3D(p, p->nrows-1, p->ncols-1);
    print_pt3D(pt);
    printf("]\n");
}

//----------------------------------------------------------------------------------------
//                                 4D POINTS
//----------------------------------------------------------------------------------------
Array_points4D* alloc_empty_arr_pts4D(){
    Array_points4D* v = (Array_points4D*)malloc(sizeof(Array_points4D));
    v->points = (Point4D*)malloc(sizeof(Point4D));
    v->nrows = 0;
    v->ncols = 0;
    v->capacity = 1;

    return v;
}

Array_points4D* alloc_with_capacity_arr_pts4D(const uint64_t nrows, const uint64_t ncols){
    Array_points4D* v = (Array_points4D*)malloc(sizeof(Array_points4D));
    v->points = (Point4D*)calloc(nrows*ncols, sizeof(Point4D));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

Array_points4D* alloc_with_init_arr_pts4D(const Point4D* points, const uint64_t nrows, const uint64_t ncols){
    Array_points4D* v = (Array_points4D*)malloc(sizeof(Array_points4D));
    v->points = (Point4D*)malloc(nrows*ncols*sizeof(Point4D));
    memcpy(v->points, points, nrows*ncols*sizeof(Point4D));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

void double_capacity_arr_pts4D(Array_points4D* v){
    v->capacity *= 2;
    v->points = (Point4D*) realloc(v->points, v->capacity*sizeof(Point4D));
}

void dealloc_arr_pts4D(Array_points4D* v){
    if (v!=NULL){
        free(v->points);
        v->points = NULL;
        v->capacity = 0;
        v->nrows = 0;
        v->ncols = 0;
    }
}

Point4D* get_ijth_elem_arr_pts4D(const Array_points4D* v, const uint64_t i, const uint64_t j){
    Point4D* p;
    if ((i<v->nrows) && (j<v->ncols)) {
        return v->points + i*v->ncols + j;
    } else {
        p = (Point4D*) malloc(sizeof(Point4D));
        p->x = nan("");
        p->y = nan("");
        p->z = nan("");
        p->t = nan("");
        return p;
    }
}

void set_ijth_elem_arr_pts4D(Array_points4D* v, const uint64_t i, const uint64_t j, const Point4D* p){
    if (j >= v->ncols){
        printf("ERROR in set set_ijth_elem_arr_pts4D : j bigger than number of columns!");
    }
    while(i*v->ncols+j >= v->capacity) double_capacity_arr_pts4D(v);
    if (i >= v->nrows) v->nrows = i+1;
    
    v->points[i*v->ncols+j] = *p;
}

void copy_arr_pts4D(const Array_points4D* src, Array_points4D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->nrows = 0;
            dest->ncols = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point4D*)malloc(src->capacity*sizeof(Point4D));
            memcpy(dest->points, src->points, src->nrows*src->ncols*sizeof(Point4D));
            dest->nrows = src->nrows;
            dest->ncols = src->ncols;
            dest->capacity = src->capacity;
        }
    }
}

void print_arr_pt4D(const Array_points4D *p){
    unsigned long int i, j;
    Point4D* pt;

    printf("vector p = [");
    for (i = 0; i<p->nrows-1; i++){
        for (j = 0; j<p->ncols-1; j++){
            pt = get_ijth_elem_arr_pts4D(p, i, j);
            print_pt4D(pt);
        }
        pt = get_ijth_elem_arr_pts4D(p, i, p->ncols-1);
        print_pt4D(pt);
    }
    for (j = 0; j<p->ncols-1; j++){
        pt = get_ijth_elem_arr_pts4D(p, p->nrows-1, j);
        print_pt4D(pt);
    }
    pt = get_ijth_elem_arr_pts4D(p, p->nrows-1, p->ncols-1);
    print_pt4D(pt);
    printf("]\n");
}

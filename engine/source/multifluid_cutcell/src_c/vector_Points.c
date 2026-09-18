#include "vector_Points.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

//----------------------------------------------------------------------------------------
//                                 2D POINTS
//----------------------------------------------------------------------------------------
Vector_points2D* alloc_empty_vec_pts2D(){
    Vector_points2D* v = (Vector_points2D*)malloc(sizeof(Vector_points2D));
    v->points = (Point2D*)malloc(sizeof(Point2D));
    v->size = 0;
    v->capacity = 1;

    return v;
}

Vector_points2D* alloc_with_capacity_vec_pts2D(uint64_t size){
    Vector_points2D* v = (Vector_points2D*)malloc(sizeof(Vector_points2D));
    v->points = (Point2D*)malloc(size*sizeof(Point2D));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_points2D* alloc_with_init_vec_pts2D(const Point2D* points, uint64_t size){
    Vector_points2D* v;
    if (points!=NULL){
        v = (Vector_points2D*)malloc(sizeof(Vector_points2D));
        v->points = (Point2D*)malloc(size*sizeof(Point2D));
        memcpy(v->points, points, size*sizeof(Point2D));
        v->size = size;
        v->capacity = size;
    } else {
        v = NULL;
    }

    return v;
}

void double_capacity_vec_pts2D(Vector_points2D* v){
    v->capacity *= 2;
    v->points = (Point2D*) realloc(v->points, v->capacity*sizeof(Point2D));
}

void push_back_vec_pts2D(Vector_points2D** v_ptr, const Point2D* point){
    Vector_points2D* v = *v_ptr;
    if ((v->capacity == 0) || (v->points == NULL)){
        *v_ptr = alloc_with_init_vec_pts2D(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_pts2D(v);
        v->points[v->size] = *point;
        v->size += 1;
    }
    else {
        v->points[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_pts2D(Vector_points2D* v){
    if (v!=NULL){
        if(v->points){
            free(v->points);
            v->points = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

Point2D* get_ith_elem_vec_pts2D(const Vector_points2D* v, uint64_t i){
    Point2D* p;
    if (i<v->size) {
        return v->points + i;
    } else {
        p = (Point2D*) malloc(sizeof(Point2D));
        p->x = nan("");
        p->y = nan("");
        return p;
    }
}

void set_ith_elem_vec_pts2D(Vector_points2D* v, uint64_t i, const Point2D* p){
    while(i >= v->capacity) double_capacity_vec_pts2D(v);
    if (i >= v->size) v->size = i+1;
    
    v->points[i] = *p;
}

void copy_vec_pts2D(const Vector_points2D* src, Vector_points2D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point2D*)malloc(src->capacity*sizeof(Point2D));
            memcpy(dest->points, src->points, src->size*sizeof(Point2D));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_pt2D(const Vector_points2D p){
    unsigned long int i;
    Point2D* pt;

    printf("vector p = [");
    for (i=0; i<p.size; i++){
        pt = get_ith_elem_vec_pts2D(&p, i);
        print_pt2D(pt);
    }
    printf("]\n");
}

Vector_points2D* cat_vec_pts2D(const Vector_points2D* v1, const Vector_points2D* v2){
    Vector_points2D *catv = alloc_with_capacity_vec_pts2D(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->points, v1->points, v1->size*sizeof(Point2D));
    memcpy(catv->points+v1->size, v2->points, v2->size*sizeof(Point2D));
    return catv;
}

//----------------------------------------------------------------------------------------
//                                 3D POINTS
//----------------------------------------------------------------------------------------
Vector_points3D* alloc_empty_vec_pts3D(){
    Vector_points3D* v = (Vector_points3D*)malloc(sizeof(Vector_points3D));
    v->points = (Point3D*)malloc(sizeof(Point3D));
    v->size = 0;
    v->capacity = 1;

    return v;
}

Vector_points3D* alloc_with_capacity_vec_pts3D(uint64_t size){
    Vector_points3D* v = (Vector_points3D*)malloc(sizeof(Vector_points3D));
    v->points = (Point3D*)malloc(size*sizeof(Point3D));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_points3D* alloc_with_init_vec_pts3D(const Point3D* points, uint64_t size){
    Vector_points3D* v = (Vector_points3D*)malloc(sizeof(Vector_points3D));
    v->points = (Point3D*)malloc(size*sizeof(Point3D));
    memcpy(v->points, points, size*sizeof(Point3D));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_pts3D(Vector_points3D* v){
    v->capacity *= 2;
    v->points = (Point3D*) realloc(v->points, v->capacity*sizeof(Point3D));
}

void push_back_vec_pts3D(Vector_points3D** v_ptr, const Point3D* point){
    Vector_points3D* v = *v_ptr;
    if ((v->capacity == 0) || (v->points == NULL)){
        *v_ptr = alloc_with_init_vec_pts3D(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_pts3D(v);
        v->points[v->size] = *point;
        v->size += 1;
    }
    else {
        v->points[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_pts3D(Vector_points3D* v){
    if (v!=NULL){
        if(v->points){
            free(v->points);
            v->points = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

Point3D* get_ith_elem_vec_pts3D(const Vector_points3D* v, uint64_t i){
    Point3D* p;
    if (i<v->size) {
        return v->points + i;
    } else {
        p = (Point3D*) malloc(sizeof(Point3D));
        p->x = nan("");
        p->y = nan("");
        p->t = nan("");
        return p;
    }
}

void set_ith_elem_vec_pts3D(Vector_points3D* v, uint64_t i, const Point3D* p){
    while(i >= v->capacity) double_capacity_vec_pts3D(v);
    if (i >= v->size) v->size = i+1;
    
    v->points[i] = *p;
}

void copy_vec_pts3D(const Vector_points3D* src, Vector_points3D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point3D*)malloc(src->capacity*sizeof(Point3D));
            memcpy(dest->points, src->points, src->size*sizeof(Point3D));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_pt3D(const Vector_points3D p){
    unsigned long int i;
    Point3D* pt;

    printf("vector p = [");
    for (i=0; i<p.size; i++){
        pt = get_ith_elem_vec_pts3D(&p, i);
        print_pt3D(pt);
    }
    printf("]\n");
}

Vector_points3D* cat_vec_pts3D(const Vector_points3D* v1, const Vector_points3D* v2){
    Vector_points3D *catv = alloc_with_capacity_vec_pts3D(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->points, v1->points, v1->size*sizeof(Point3D));
    memcpy(catv->points+v1->size, v2->points, v2->size*sizeof(Point3D));
    return catv;
}


//----------------------------------------------------------------------------------------
//                                 4D POINTS
//----------------------------------------------------------------------------------------
Vector_points4D* alloc_empty_vec_pts4D(){
    Vector_points4D* v = (Vector_points4D*)malloc(sizeof(Vector_points4D));
    v->points = (Point4D*)malloc(sizeof(Point4D));
    v->size = 0;
    v->capacity = 1;

    return v;
}

Vector_points4D* alloc_with_capacity_vec_pts4D(uint64_t size){
    Vector_points4D* v = (Vector_points4D*)malloc(sizeof(Vector_points4D));
    v->points = (Point4D*)malloc(size*sizeof(Point4D));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_points4D* alloc_with_init_vec_pts4D(const Point4D* points, uint64_t size){
    Vector_points4D* v = (Vector_points4D*)malloc(sizeof(Vector_points4D));
    v->points = (Point4D*)malloc(size*sizeof(Point4D));
    memcpy(v->points, points, size*sizeof(Point4D));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_pts4D(Vector_points4D* v){
    v->capacity *= 2;
    v->points = (Point4D*) realloc(v->points, v->capacity*sizeof(Point4D));
}

void push_back_vec_pts4D(Vector_points4D** v_ptr, const Point4D* point){
    Vector_points4D* v = *v_ptr;
    if ((v->capacity == 0) || (v->points == NULL)){
        free(v->points);
        *v_ptr = alloc_with_init_vec_pts4D(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_pts4D(v);
        v->points[v->size] = *point;
        v->size += 1;
    }
    else {
        v->points[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_pts4D(Vector_points4D* v){
    if (v!=NULL){
        if(v->points){
            free(v->points);
            v->points = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

Point4D* get_ith_elem_vec_pts4D(const Vector_points4D* v, uint64_t i){
    Point4D* p;
    if (i<v->size) {
        return v->points + i;
    } else {
        p = (Point4D*) malloc(sizeof(Point4D));
        p->x = nan("");
        p->y = nan("");
        p->z = nan("");
        p->t = nan("");
        return p;
    }
}

void set_ith_elem_vec_pts4D(Vector_points4D* v, uint64_t i, const Point4D* p){
    while(i >= v->capacity) double_capacity_vec_pts4D(v);
    if (i >= v->size) v->size = i+1;
    
    v->points[i] = *p;
}

void copy_vec_pts4D(const Vector_points4D* src, Vector_points4D* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->points);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->points);
            dest->points = (Point4D*)malloc(src->capacity*sizeof(Point4D));
            memcpy(dest->points, src->points, src->size*sizeof(Point4D));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_pt4D(const Vector_points4D p){
    unsigned long int i;
    Point4D* pt;

    printf("vector p = [");
    for (i=0; i<p.size; i++){
        pt = get_ith_elem_vec_pts4D(&p, i);
        print_pt4D(pt);
    }
    printf("]\n");
}

Vector_points4D* cat_vec_pts4D(const Vector_points4D* v1, const Vector_points4D* v2){
    Vector_points4D *catv = alloc_with_capacity_vec_pts4D(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->points, v1->points, v1->size*sizeof(Point4D));
    memcpy(catv->points+v1->size, v2->points, v2->size*sizeof(Point4D));
    return catv;
}

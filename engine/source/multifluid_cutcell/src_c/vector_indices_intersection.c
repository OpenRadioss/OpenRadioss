#include "vector_indices_intersection.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

Vector_indices_intersec* alloc_empty_vec_indices_intersec(){
    return alloc_with_capacity_vec_indices_intersec(1);
}

Vector_indices_intersec* alloc_with_capacity_vec_indices_intersec(uint64_t size){
    Vector_indices_intersec* v = (Vector_indices_intersec*)malloc(sizeof(Vector_indices_intersec));
    v->data = (Indices_intersection*)malloc(size*sizeof(Indices_intersection));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_indices_intersec* alloc_with_init_vec_indices_intersec(const Indices_intersection* data, uint64_t size){
    Vector_indices_intersec* v = (Vector_indices_intersec*)malloc(sizeof(Vector_indices_intersec));
    v->data = (Indices_intersection*)malloc(size*sizeof(Indices_intersection));
    memcpy(v->data, data, size*sizeof(Indices_intersection));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_indices_intersec(Vector_indices_intersec* v){
    v->capacity *= 2;
    v->data = (Indices_intersection*) realloc(v->data, v->capacity*sizeof(Indices_intersection));
}

void push_back_vec_indices_intersec(Vector_indices_intersec** v_ptr, const Indices_intersection* point){
    Vector_indices_intersec* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_indices_intersec(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_indices_intersec(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void push_back_unique_vec_indices_intersec(Vector_indices_intersec** v_ptr, const Indices_intersection* point){
    uint64_t i;
    int8_t isin;
    Vector_indices_intersec* v = *v_ptr;

    if ((v->size == 0) || (v->data == NULL)){
        free(v->data);
        *v_ptr = alloc_with_init_vec_indices_intersec(point, 1);
        return;
    }
    if (v->size >= v->capacity)
    {
        double_capacity_vec_indices_intersec(v);
    }

    isin = 0;
    for(i = 0; i<v->size; i++){
        if ((v->data[i].i_e == point->i_e) && (v->data[i].i_pt == point->i_pt)){
            isin = 1;
            break;
        }
    }
    if (!isin) {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_indices_intersec(Vector_indices_intersec* v){
    if(v!=NULL){
        if(v->data){
            free(v->data);
            v->data = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

Indices_intersection* get_ith_elem_vec_indices_intersec(const Vector_indices_intersec* v, uint64_t i){
    Indices_intersection* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (Indices_intersection*) malloc(sizeof(Indices_intersection));
        p->i_e = -1;
        p->i_pt = -1;
        return p;
    }
}

void set_ith_elem_vec_indices_intersec(Vector_indices_intersec* v, uint64_t i, Indices_intersection* d){
    while(i >= v->capacity) double_capacity_vec_indices_intersec(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_indices_intersec(const Vector_indices_intersec* src, Vector_indices_intersec* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (Indices_intersection*)malloc(src->capacity*sizeof(Indices_intersection));
            memcpy(dest->data, src->data, src->size*sizeof(Indices_intersection));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

Vector_indices_intersec* cat_vec_indices_intersec(const Vector_indices_intersec* v1, const Vector_indices_intersec* v2){
    Vector_indices_intersec *catv = alloc_with_capacity_vec_indices_intersec(v1->size + v2->size);
    memcpy(catv->data, v1->data, v1->size*sizeof(Indices_intersection));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(Indices_intersection));
    return catv;
}
#include "Point.h"
#include <stdlib.h>
#include <math.h>
#include <stdio.h>

my_real_c scalProd2D(const Point2D v1, const Point2D v2){
    return v1.x*v2.x + v1.y*v2.y;
}

my_real_c compute_distance2D(const Point2D v, const Point2D n, const Point2D pt){
    Point2D diff = (Point2D){v.x-pt.x, v.y-pt.y};
    return scalProd2D(n, diff);
}

Point2D find_intersection2D(const Point2D v1, const my_real_c d1, const Point2D v2, const my_real_c d2){
        my_real_c r;
        Point2D inter;
        if (fabs(d1-d2)<1e-10)
            return v1;
        else{
            r = d2/(d2-d1);
            inter.x = r*v1.x + (1.0-r)*v2.x;
            inter.y = r*v1.y + (1.0-r)*v2.y;
            return inter;
        }
}

void sum_points2D(void* out_void, const void* v1_void, const void* v2_void){
    Point2D* out = (Point2D*)out_void;
    Point2D* v1 = (Point2D*)v1_void;
    Point2D* v2 = (Point2D*)v2_void;
    *out = (Point2D){v1->x + v2->x, v1->y + v2->y};
}

void scale_points2D(void* out_void, const void* s_void, const void* v_void){
    Point2D* out = (Point2D*)out_void;
    my_real_c* s = (my_real_c*)s_void;
    Point2D* v = (Point2D*)v_void;
    *out = (Point2D){(*s) * v->x, (*s) * v->y};
}

void inplace_axpy_points2D(Point2D* v1, const my_real_c a, const Point2D* v2){
    *v1 = (Point2D){a*v1->x + v2->x, a*v1->y + v2->y};
}

void inplace_xpay_points2D(Point2D* v1, const my_real_c a, const Point2D* v2){
    *v1 = (Point2D){v1->x + a*v2->x, v1->y + a*v2->y};
}

void print_pt2D(const Point2D *p){
    printf("(%.3e, %.3e) ", p->x, p->y);
}

my_real_c norm_pt2D(const Point2D p){
    return sqrt(p.x*p.x + p.y*p.y);
}

my_real_c scalProd3D(const Point3D v1, const Point3D v2){
    return v1.x*v2.x + v1.y*v2.y + v1.t*v2.t;
}

my_real_c compute_distance3D(const Point3D v, const Point3D n, const Point3D pt){
    Point3D diff = (Point3D){v.x-pt.x, v.y-pt.y, v.t-pt.t};
    return scalProd3D(n, diff);
}

Point3D find_intersection3D(const Point3D v1, const my_real_c d1, const Point3D v2, const my_real_c d2){
        my_real_c r;
        Point3D inter;
        if (fabs(d1-d2)<1e-15)
            return v1;
        else{
            r = d2/(d2-d1);
            inter.x = r*v1.x + (1.0-r)*v2.x;
            inter.y = r*v1.y + (1.0-r)*v2.y;
            inter.t = r*v1.t + (1.0-r)*v2.t;
            return inter;
        }
}

void sum_points3D(void* out_void, const void* v1_void, const void* v2_void){
    Point3D* out = (Point3D*)out_void;
    Point3D* v1 = (Point3D*)v1_void;
    Point3D* v2 = (Point3D*)v2_void;
    *out = (Point3D){v1->x + v2->x, v1->y + v2->y, v1->t + v2->t};
}

void scale_points3D(void* out_void, const void* s_void, const void* v_void){
    Point3D* out = (Point3D*)out_void;
    my_real_c* s = (my_real_c*)s_void;
    Point3D* v = (Point3D*)v_void;
    *out = (Point3D){(*s) * v->x, (*s) * v->y, (*s) * v->t};
}

void inplace_axpy_points3D(Point3D* v1, const my_real_c a, const Point3D* v2){
    *v1 = (Point3D){a*v1->x + v2->x, a*v1->y + v2->y, a*v1->t + v2->t};
}

void inplace_xpay_points3D(Point3D* v1, const my_real_c a, const Point3D* v2){
    *v1 = (Point3D){v1->x + a*v2->x, v1->y + a*v2->y, v1->t + a*v2->t};
}

void print_pt3D(const Point3D *p){
    printf("(%.3e, %.3e, %.3e) ", p->x, p->y, p->t);
}

my_real_c norm_pt3D(const Point3D p){
    return sqrt(p.x*p.x + p.y*p.y + p.t*p.t);
}

my_real_c scalProd4D(const Point4D v1, const Point4D v2){
    return v1.x*v2.x + v1.y*v2.y  + v1.z*v2.z + v1.t*v2.t;
}

my_real_c compute_distance4D(const Point4D v, const Point4D n, const Point4D pt){
    Point4D diff = (Point4D){v.x-pt.x, v.y-pt.y, v.z-pt.z, v.t-pt.t};
    return scalProd4D(n, diff);
}

Point4D find_intersection4D(const Point4D v1, const my_real_c d1, const Point4D v2, const my_real_c d2){
        my_real_c r;
        Point4D inter;
        if (fabs(d1-d2)<1e-10)
            return v1;
        else{
            r = d2/(d2-d1);
            inter.x = r*v1.x + (1.0-r)*v2.x;
            inter.y = r*v1.y + (1.0-r)*v2.y;
            inter.z = r*v1.z + (1.0-r)*v2.z;
            inter.t = r*v1.t + (1.0-r)*v2.t;
            return inter;
        }
}

void sum_points4D(void* out_void, const void* v1_void, const void* v2_void){
    Point4D* out = (Point4D*)out_void;
    Point4D* v1 = (Point4D*)v1_void;
    Point4D* v2 = (Point4D*)v2_void;
    *out = (Point4D){v1->x + v2->x, v1->y + v2->y, v1->z + v2->z, v1->t + v2->t};
}

void scale_points4D(void* out_void, const void* s_void, const void* v_void){
    Point4D* out = (Point4D*)out_void;
    my_real_c* s = (my_real_c*)s_void;
    Point4D* v = (Point4D*)v_void;
    *out = (Point4D){(*s) * v->x, (*s) * v->y, (*s) * v->z, (*s) * v->t};
}

void inplace_axpy_points4D(Point4D* v1, const my_real_c a, const Point4D* v2){
    *v1 = (Point4D){a*v1->x + v2->x, a*v1->y + v2->y, a*v1->z + v2->z, a*v1->t + v2->t};
}

void inplace_xpay_points4D(Point4D* v1, const my_real_c a, const Point4D* v2){
    *v1 = (Point4D){v1->x + a*v2->x, v1->y + a*v2->y, v1->z + a*v2->z, v1->t + a*v2->t};
}

void print_pt4D(const Point4D *p){
    printf("(%.3e, %.3e, %.3e, %.3e) ", p->x, p->y, p->z, p->t);
}

my_real_c norm_pt4D(const Point4D p){
    return sqrt(p.x*p.x + p.y*p.y + p.z*p.z + p.t*p.t);
}

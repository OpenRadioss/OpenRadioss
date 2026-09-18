#include "Polyhedron3D.h"

/// @brief Polyhedron3D constructor
Polyhedron3D* new_Polyhedron3D(){
    GrB_Info infogrb;
    Polyhedron3D* p = (Polyhedron3D*) malloc(sizeof(Polyhedron3D));

    p->vertices = alloc_empty_vec_pts3D();
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    p->volumes = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    infogrb = GrB_Matrix_new(p->edges, GrB_INT8, 0,0);
    infogrb = GrB_Matrix_new(p->faces, GrB_INT8, 0,0);
    infogrb = GrB_Matrix_new(p->volumes, GrB_INT8, 0,0);
    p->status_face = alloc_empty_vec_int();
    
    return p;
}

/// @brief Polyhedron3D constructor
Polyhedron3D* new_Polyhedron3D_vefs(const Vector_points3D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces, const Vector_int* status_face){
    GrB_Info infogrb;
    GrB_Index nb_rows;
    Polyhedron3D* p = (Polyhedron3D*) malloc(sizeof(Polyhedron3D));

    p->vertices = alloc_empty_vec_pts3D();
    copy_vec_pts3D(vertices, p->vertices);
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->edges, *edges);
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->faces, *faces);

    p->volumes = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    infogrb = GrB_Matrix_ncols(&nb_rows, *(p->faces));
    infogrb = GrB_Matrix_new(p->volumes, GrB_INT8, nb_rows, 1);

    p->status_face = alloc_empty_vec_int();
    copy_vec_int(status_face, p->status_face);

    return p;
}

/// @brief Polyhedron3D constructor
Polyhedron3D* new_Polyhedron3D_vefvs(const Vector_points3D* vertices, const GrB_Matrix* edges, const GrB_Matrix* faces, const GrB_Matrix* volumes, const Vector_int* status_face){
    Polyhedron3D* p = (Polyhedron3D*) malloc(sizeof(Polyhedron3D));

    p->vertices = alloc_empty_vec_pts3D();
    copy_vec_pts3D(vertices, p->vertices);
    p->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->edges, *edges);
    p->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->faces, *faces);
    p->volumes = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    GrB_Matrix_dup(p->volumes, *volumes);
    p->status_face = alloc_empty_vec_int();
    copy_vec_int(status_face, p->status_face);
    
    return p;
}

/// @brief Polyhedron3D constructor using points forming a grid
Polyhedron3D* Polyhedron3D_from_vertices(const my_real_c* x_v, unsigned long int n_x, const my_real_c* y_v, unsigned long int n_y, const my_real_c* z_v, unsigned long int n_z){
    GrB_Info infogrb;
    GrB_Index nb_pts, nb_edges, nb_faces, nb_volumes;
    uint64_t curr_face, curr_pt, curr_edge, curr_vol, iy, ix, iz;
    uint64_t ind_pt_SW, ind_pt_SE, ind_pt_NW;
    uint64_t ind_pt_SW_up;//, ind_pt_SE_up, ind_pt_NW_up;
    uint64_t ind_e_W, ind_e_S, ind_e_E, ind_e_N;
    uint64_t ind_e_W_up, ind_e_S_up;
    uint64_t ind_e_SW_up, ind_e_SE_up, ind_e_NW_up;
    uint64_t face_W, face_B, face_S, face_E, face_N, face_U;
    my_real_c yS, xW, zB;
    Point3D ptSWB;
    Vector_points3D* vertices;
    Vector_int* status_face;
    GrB_Matrix* edges;
    GrB_Matrix* faces;
    GrB_Matrix* volumes;
    Polyhedron3D* p;

    if (n_x<2 || n_y<2 || n_z<2)
        return new_Polyhedron3D();

    edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
    volumes = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));

    nb_pts = n_x*n_y*n_z;
    nb_edges = ((3*(n_x-1) + 2)*(n_y-1) + 2*(n_y-1) + 1)*(n_z-1) + (2*(n_x-1) + 1)*(n_y-1) + (n_y-1);
    nb_faces = ((3*(n_x-1) + 1)*(n_y-1) + (n_y-1))*(n_z-1) + (n_x-1)*(n_y-1);
    nb_volumes = (n_x-1)*(n_y-1)*(n_z-1);

    vertices = alloc_with_capacity_vec_pts3D(nb_pts);
    infogrb = GrB_Matrix_new(edges, GrB_INT8, nb_pts, nb_edges);
    infogrb = GrB_Matrix_new(faces, GrB_INT8, nb_edges, nb_faces);
    infogrb = GrB_Matrix_new(volumes, GrB_INT8, nb_faces, nb_volumes);

    ptSWB = (Point3D){x_v[0], y_v[0], z_v[0]};
    push_back_vec_pts3D(&vertices, &ptSWB);
    curr_face = 0;
    curr_pt = 0;
    curr_edge = 0;
    curr_vol = 0;
    for (iz = 0; iz < n_z-2; iz++){
        zB = z_v[iz];
        for (iy = 0; iy < n_y-2; iy++){
            yS = y_v[iy];
            for (ix = 0; ix < n_x-1; ix++){
                xW = x_v[ix];

                ind_pt_SW = curr_pt;
                ind_pt_SE = curr_pt + 1;
                ind_pt_NW = curr_pt + n_x;
                ind_pt_SW_up = curr_pt + n_x*n_y;

                ind_e_W = curr_edge;
                ind_e_SW_up = curr_edge + 1;
                ind_e_S = curr_edge + 2;
                ind_e_E = curr_edge + 3;
                ind_e_SE_up = curr_edge + 4;
                ind_e_NW_up = curr_edge + (3*(n_x-1) + 3);
                ind_e_N = curr_edge + (3*(n_x-1) + 4);
                ind_e_W_up = ind_e_W + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;
                ind_e_S_up = ind_e_S + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;

                face_W = curr_face;
                face_B = curr_face + 1;
                face_S = curr_face + 2;
                face_E = curr_face + 3;
                face_N = face_S + (3*(n_x-1) + 1);
                face_U = face_B + (3*(n_x-1) + 1)*(n_y-1) + (n_y-1);

                ptSWB = (Point3D){xW,yS,zB};

                set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

                infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
                infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
                infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
                infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
                infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
                infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, face_W);
                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , face_W);
                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, face_W);
                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , face_W);

                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , face_S);
                infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, face_S);
                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , face_S);
                infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, face_S);

                infogrb = GrB_Matrix_setElement(*volumes,  1, face_E, curr_vol);
                infogrb = GrB_Matrix_setElement(*volumes, -1, face_B, curr_vol);
                infogrb = GrB_Matrix_setElement(*volumes, -1, face_S, curr_vol);
                infogrb = GrB_Matrix_setElement(*volumes, -1, face_W, curr_vol);
                infogrb = GrB_Matrix_setElement(*volumes,  1, face_N, curr_vol);
                infogrb = GrB_Matrix_setElement(*volumes,  1, face_U, curr_vol);

                curr_pt += 1;
                curr_edge += 3;
                curr_face += 3;
                curr_vol += 1;
            }
            //Treat east-most point and edge
            xW = x_v[n_x-1];
            ptSWB = (Point3D){xW,yS,zB};
            ind_pt_SW = curr_pt;
            ind_pt_NW = curr_pt + n_x;
            ind_pt_SW_up = curr_pt + n_x*n_y;

            ind_e_W = curr_edge;
            ind_e_SW_up = curr_edge + 1;
            ind_e_NW_up = curr_edge +  + (3*n_x);
            ind_e_W_up = ind_e_W + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;

            set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);

            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , curr_face);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, curr_face);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, curr_face);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , curr_face);

            curr_pt += 1;
            curr_edge += 2;
            curr_face += 1;
        }

        //Treat north-most cells
        iy = n_y-2;
        yS = y_v[iy];
        ind_e_NW_up = curr_edge + (3*(n_x-1));
        face_N = curr_face + 3*(n_x-1);
        for (ix = 0; ix < n_x-1; ix++){
            ind_pt_SW = curr_pt;
            ind_pt_SE = curr_pt + 1;
            ind_pt_NW = curr_pt + n_x;
            ind_pt_SW_up = curr_pt + n_x*n_y;

            ind_e_W = curr_edge;
            ind_e_SW_up = curr_edge + 1;
            ind_e_S = curr_edge + 2;
            ind_e_E = curr_edge + 3;
            ind_e_SE_up = curr_edge + 4;
            ind_e_NW_up += 2;
            ind_e_N = ind_e_NW_up + 1;
            ind_e_W_up = ind_e_W + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;
            ind_e_S_up = ind_e_S + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;

            face_W = curr_face;
            face_B = curr_face + 1;
            face_S = curr_face + 2;
            face_E = curr_face + 3;
            face_N += 1;
            face_U = face_B + (3*(n_x-1) + 1)*(n_y-1) + (n_y-1);
            
            xW = x_v[ix];
            ptSWB = (Point3D){xW,yS,zB};

            set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, face_W);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , face_W);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, face_W);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , face_W);

            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , face_S);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, face_S);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , face_S);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, face_S);

            infogrb = GrB_Matrix_setElement(*volumes,  1, face_E, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_B, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_S, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_W, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes,  1, face_N, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes,  1, face_U, curr_vol);

            curr_pt += 1;
            curr_edge += 3;
            curr_face += 3;
            curr_vol += 1;
        }
        //Treat east-most point and edge
        xW = x_v[n_x-1];
        ptSWB = (Point3D){xW,yS,zB};
        ind_pt_SW = curr_pt;
        ind_pt_NW = curr_pt + n_x;
        ind_pt_SW_up = curr_pt + n_x*n_y;

        ind_e_W = curr_edge;
        ind_e_SW_up = curr_edge + 1;
        ind_e_NW_up += 2;
        ind_e_W_up = ind_e_W + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);

        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , curr_face);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, curr_face);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, curr_face);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , curr_face);

        curr_pt += 1;
        curr_edge += 2;
        curr_face += 1;
    

        //Treat north-most points and edges
        yS = y_v[n_y-1];
        for (ix = 0; ix < n_x-1; ix++){
            ind_pt_SW = curr_pt;
            ind_pt_SE = curr_pt + 1;
            ind_pt_SW_up = curr_pt + n_x*n_y;

            ind_e_SW_up = curr_edge;
            ind_e_S = curr_edge + 1;
            ind_e_SE_up = curr_edge + 2;
            ind_e_S_up = ind_e_S + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;

            xW = x_v[ix];
            ptSWB = (Point3D){xW,yS,zB};

            set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , curr_face);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, curr_face);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , curr_face);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, curr_face);

            curr_pt += 1;
            curr_edge += 2;
            curr_face += 1;
        }
        xW = x_v[n_x-1];
        ptSWB = (Point3D){xW,yS,zB};
        ind_pt_SW = curr_pt;
        ind_pt_SW_up = curr_pt + n_x*n_y;
        
        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);
        
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , curr_edge);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, curr_edge);        

        curr_pt += 1;
        curr_edge += 1;
    }

    //Make the top-most cubes of the grid 
    zB = z_v[n_z-2];
    ind_e_W_up = curr_edge + (3*(n_x-1)+2)*(n_y-1) +  + 2*(n_y-1) + 1;
    face_U = curr_face + (3*(n_x-1) + 1)*(n_y-1) + (n_y-1);
    for (iy = 0; iy < n_y-2; iy++){
        yS = y_v[iy];
        for (ix = 0; ix < n_x-1; ix++){
            xW = x_v[ix];

            ind_pt_SW = curr_pt;
            ind_pt_SE = curr_pt + 1;
            ind_pt_NW = curr_pt + n_x;
            ind_pt_SW_up = curr_pt + n_x*n_y;

            ind_e_W = curr_edge;
            ind_e_SW_up = curr_edge + 1;
            ind_e_S = curr_edge + 2;
            ind_e_E = curr_edge + 3;
            ind_e_SE_up = curr_edge + 4;
            ind_e_NW_up = curr_edge + 3*n_x;
            ind_e_N = ind_e_NW_up + 1;
            ind_e_S_up = ind_e_W_up + 1;

            face_W = curr_face;
            face_B = curr_face + 1;
            face_S = curr_face + 2;
            face_E = curr_face + 3;
            face_N = face_S + (3*(n_x-1) + 1);

            ptSWB = (Point3D){xW,yS,zB};

            set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, face_W);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , face_W);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, face_W);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , face_W);

            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , face_S);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, face_S);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , face_S);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, face_S);

            infogrb = GrB_Matrix_setElement(*volumes,  1, face_E, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_B, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_S, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes, -1, face_W, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes,  1, face_N, curr_vol);
            infogrb = GrB_Matrix_setElement(*volumes,  1, face_U, curr_vol);

            curr_pt += 1;
            curr_edge += 3;
            curr_face += 3;
            curr_vol += 1;
            ind_e_W_up += 2;
            face_U += 1;
        }
        //Treat east-most point and edge
        xW = x_v[n_x-1];
        ptSWB = (Point3D){xW,yS,zB};
        ind_pt_SW = curr_pt;
        ind_pt_NW = curr_pt + n_x;
        ind_pt_SW_up = curr_pt + n_x*n_y;

        ind_e_W = curr_edge;
        ind_e_SW_up = curr_edge + 1;
        ind_e_NW_up = curr_edge +  + 3*n_x;

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);

        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , curr_face);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, curr_face);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, curr_face);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , curr_face);

        curr_pt += 1;
        curr_edge += 2;
        curr_face += 1;
        ind_e_W_up += 1;
    }

    //Treat north-most cells
    iy = n_y-2;
    yS = y_v[iy];
    ind_e_NW_up = curr_edge + (3*(n_x-1));
    face_N = curr_face + 3*(n_x-1);
    ind_e_S_up = curr_edge + (3*(n_x-1)+2) + (2*(n_x-1)+1)*(n_y-1) - 1;
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;
        ind_pt_NW = curr_pt + n_x;
        ind_pt_SW_up = curr_pt + n_x*n_y;

        ind_e_W = curr_edge;
        ind_e_SW_up = curr_edge + 1;
        ind_e_S = curr_edge + 2;
        ind_e_E = curr_edge + 3;
        ind_e_SE_up = curr_edge + 4;
        ind_e_NW_up += 2;
        ind_e_N = ind_e_NW_up + 1;
        ind_e_S_up += 2;

        face_W = curr_face;
        face_B = curr_face + 1;
        face_S = curr_face + 2;
        face_E = curr_face + 3;
        face_N += 1;

        xW = x_v[ix];
        ptSWB = (Point3D){xW,yS,zB};

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, face_W);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , face_W);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, face_W);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , face_W);

        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , face_S);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, face_S);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , face_S);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, face_S);

        infogrb = GrB_Matrix_setElement(*volumes,  1, face_E, curr_vol);
        infogrb = GrB_Matrix_setElement(*volumes, -1, face_B, curr_vol);
        infogrb = GrB_Matrix_setElement(*volumes, -1, face_S, curr_vol);
        infogrb = GrB_Matrix_setElement(*volumes, -1, face_W, curr_vol);
        infogrb = GrB_Matrix_setElement(*volumes,  1, face_N, curr_vol);
        infogrb = GrB_Matrix_setElement(*volumes,  1, face_U, curr_vol);

        curr_pt += 1;
        curr_edge += 3;
        curr_face += 3;
        curr_vol += 1;
        ind_e_W_up += 2;
        face_U += 1;
    }
    //Treat east-most point and edge
    xW = x_v[n_x-1];
    ptSWB = (Point3D){xW,yS,zB};
    ind_pt_SW = curr_pt;
    ind_pt_NW = curr_pt + n_x;
    ind_pt_SW_up = curr_pt + n_x*n_y;

    ind_e_W = curr_edge;
    ind_e_SW_up = curr_edge + 1;
    ind_e_NW_up += 2;

    set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

    infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_W    );
    infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW   , ind_e_W    );
    infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
    infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);

    infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_W    , curr_face);
    infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SW_up, curr_face);
    infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_NW_up, curr_face);
    infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W_up , curr_face);

    curr_pt += 1;
    curr_edge += 2;
    curr_face += 1;
    ind_e_W_up += 1;
    

    //Treat north-most points and edges
    yS = y_v[n_y-1];
    ind_e_S_up = curr_edge + (2*(n_x-1)+1) + (2*(n_x-1)+1)*(n_y-1) - 1;
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;
        ind_pt_SW_up = curr_pt + n_x*n_y;

        ind_e_SW_up = curr_edge;
        ind_e_S = curr_edge + 1;
        ind_e_SE_up = curr_edge + 2;
        ind_e_S_up += 1;

        xW = x_v[ix];
        ptSWB = (Point3D){xW,yS,zB};
        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, ind_e_SW_up);
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , ind_e_S    );
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE   , ind_e_S    );

        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_S    , curr_face);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_SE_up, curr_face);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S_up , curr_face);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_SW_up, curr_face);

        curr_pt += 1;
        curr_edge += 2;
        curr_face += 1;
    }
    xW = x_v[n_x-1];
    ptSWB = (Point3D){xW,yS,zB};
    ind_pt_SW = curr_pt;
    ind_pt_SW_up = curr_pt + n_x*n_y;

    set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);
    
    infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW   , curr_edge);
    infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SW_up, curr_edge);        

    curr_pt += 1;
    curr_edge += 1;

    //Finally, deal with the top-most faces, edges and points
    zB = z_v[n_z-1];
    for (iy = 0; iy < n_y-2; iy++){
        yS = y_v[iy];
        for (ix = 0; ix < n_x-1; ix++){
            xW = x_v[ix];

            ind_pt_SW = curr_pt;
            ind_pt_SE = curr_pt + 1;
            ind_pt_NW = curr_pt + n_x;

            ind_e_W = curr_edge;
            ind_e_S = curr_edge + 1;
            ind_e_E = curr_edge + 2;
            ind_e_N = curr_edge + (2*n_x);

            face_B = curr_face;

            ptSWB = (Point3D){xW,yS,zB};

            set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W);
            infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S);
            infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S);

            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
            infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
            infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

            curr_pt += 1;
            curr_edge += 2;
            curr_face += 1;
        }
        //Treat east-most point and edge
        xW = x_v[n_x-1];
        ptSWB = (Point3D){xW,yS,zB};
        ind_pt_SW = curr_pt;
        ind_pt_NW = curr_pt + n_x;

        ind_e_W = curr_edge;

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W);        

        curr_pt += 1;
        curr_edge += 1;
    }

    //Treat north-most cells
    yS = y_v[n_y-2];
    ind_e_N = curr_edge + (2*(n_x-1));
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;
        ind_pt_NW = curr_pt + n_x;

        ind_e_W = curr_edge;
        ind_e_S = curr_edge + 1;
        ind_e_E = curr_edge + 2;
        ind_e_N += 1;

        face_B = curr_face ;
            
        xW = x_v[ix];
        ptSWB = (Point3D){xW,yS,zB};

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W);
        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S);

        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_S, face_B);
        infogrb = GrB_Matrix_setElement(*faces,  1, ind_e_E, face_B);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_N, face_B);
        infogrb = GrB_Matrix_setElement(*faces, -1, ind_e_W, face_B);

        curr_pt += 1;
        curr_edge += 2;
        curr_face += 1;
    }
    //Treat east-most point and edge
    xW = x_v[n_x-1];
    ptSWB = (Point3D){xW,yS,zB};
    ind_pt_SW = curr_pt;
    ind_pt_NW = curr_pt + n_x;

    ind_e_W = curr_edge;
    set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

    infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_W);
    infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_NW, ind_e_W);        

    curr_pt += 1;
    curr_edge += 1;
    

    //Treat north-most points and edges
    yS = y_v[n_y-1];
    for (ix = 0; ix < n_x-1; ix++){
        ind_pt_SW = curr_pt;
        ind_pt_SE = curr_pt + 1;

        ind_e_S = curr_edge;

        xW = x_v[ix];
        ptSWB = (Point3D){xW,yS,zB};

        set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);

        infogrb = GrB_Matrix_setElement(*edges, -1, ind_pt_SW, ind_e_S);
        infogrb = GrB_Matrix_setElement(*edges,  1, ind_pt_SE, ind_e_S);        

        curr_pt += 1;
        curr_edge += 1;
    }
    xW = x_v[n_x-1];
    ptSWB = (Point3D){xW,yS,zB};
    ind_pt_SW = curr_pt;
    set_ith_elem_vec_pts3D(vertices, ind_pt_SW, &ptSWB);
            

    status_face = alloc_empty_vec_int();
    status_face->data = (long int*) calloc(nb_faces, sizeof(long int));
    status_face->capacity = nb_faces;
    status_face->size = nb_faces;

    p = new_Polyhedron3D_vefvs(vertices, edges, faces, volumes, status_face);

    dealloc_vec_int(status_face); free(status_face);
    dealloc_vec_pts3D(vertices); free(vertices);
    GrB_free(edges); free(edges);
    GrB_free(faces); free(faces);
    GrB_free(volumes); free(volumes);

    return p;
}

void copy_Polyhedron3D(const Polyhedron3D *src, Polyhedron3D *dest){
    GrB_Index nb_pts, nb_edges, nb_faces, nb_volumes;

    if (src == NULL){
        if (dest != NULL) {
            if(dest->vertices){
                dealloc_vec_pts3D(dest->vertices); free(dest->vertices);
            }
            GrB_free(dest->edges);
            GrB_free(dest->faces);
            GrB_free(dest->volumes);
            if(dest->status_face){
                dealloc_vec_int(dest->status_face); free(dest->status_face);
            }
            dest->vertices = NULL;
            dest->edges = NULL;
            dest->faces = NULL;
            dest->volumes = NULL;
            dest->status_face = NULL;
        }
    } else {
        if (dest != NULL){
            //GrB_free(dest->edges);
            //GrB_free(dest->faces);
            //GrB_free(dest->volumes);
            GrB_Matrix_nrows(&nb_pts, *(src->edges));
            GrB_Matrix_ncols(&nb_edges, *(src->edges));
            GrB_Matrix_ncols(&nb_faces, *(src->faces));
            GrB_Matrix_ncols(&nb_volumes, *(src->volumes));
            if (dest->edges) {
                GrB_Matrix_resize(*(dest->edges), nb_pts, nb_edges);
            } else {
                dest->edges = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
                GrB_Matrix_new(dest->edges, GrB_INT8, nb_pts, nb_edges);
            }
            if (dest->faces){
                GrB_Matrix_resize(*(dest->faces), nb_edges, nb_faces);
            } else {
                dest->faces = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
                GrB_Matrix_new(dest->faces, GrB_INT8, nb_edges, nb_faces);
            }
            if (dest->volumes){
                GrB_Matrix_resize(*(dest->volumes), nb_faces, nb_volumes);
            } else {
                dest->volumes = (GrB_Matrix*) malloc(sizeof(GrB_Matrix));
                GrB_Matrix_new(dest->volumes, GrB_INT8, nb_faces, nb_volumes);
            }
            if (!dest->vertices){
                dest->vertices = alloc_empty_vec_pts3D();
            }

            copy_vec_pts3D(src->vertices, dest->vertices);
            GrB_Matrix_dup(dest->edges, *(src->edges));
            GrB_Matrix_dup(dest->faces, *(src->faces));
            GrB_Matrix_dup(dest->volumes, *(src->volumes));
            if (!dest->status_face){
                dest->status_face = alloc_empty_vec_int();
            }
            copy_vec_int(src->status_face, dest->status_face);
        }
    }
}

void dealloc_Polyhedron3D(Polyhedron3D* p){
    if(p){
        if(p->vertices){
            dealloc_vec_pts3D(p->vertices); free(p->vertices);
        }
        GrB_free(p->edges);
        if(p->edges) free(p->edges);
        GrB_free(p->faces);
        if(p->faces) free(p->faces);
        GrB_free(p->volumes);
        if(p->volumes) free(p->volumes);
        if(p->status_face){
            dealloc_vec_int(p->status_face); free(p->status_face);
        }
    }
}

/// @brief Fuse two Polyhedron3D into a single structure
Polyhedron3D* fuse_polyhedrons(const Polyhedron3D* p1, const Polyhedron3D* p2){
    Vector_points3D* fused_vertices;
    GrB_Matrix *fused_edges = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *fused_faces = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    GrB_Matrix *fused_volumes = (GrB_Matrix*)malloc(sizeof(GrB_Matrix));
    Vector_int *fused_status;
    GrB_Index nrows1, ncols1, nrows2, ncols2;
    GrB_Matrix zeros1, zeros2;
    Polyhedron3D* res_p;

    fused_vertices = cat_vec_pts3D(p1->vertices, p2->vertices);
    fused_status = cat_vec_int(p1->status_face, p2->status_face);

    GrB_Matrix_nrows(&nrows1, *(p1->edges));
    GrB_Matrix_ncols(&ncols1, *(p1->edges));
    GrB_Matrix_nrows(&nrows2, *(p2->edges));
    GrB_Matrix_ncols(&ncols2, *(p2->edges));
    GrB_Matrix_new(&zeros1, GrB_INT8, nrows1, ncols2);
    GrB_Matrix_new(&zeros2, GrB_INT8, nrows2, ncols1);
    GrB_Matrix_new(fused_edges, GrB_INT8, nrows1+nrows2, ncols1+ncols2);
    GxB_Matrix_concat(*fused_edges, (GrB_Matrix[]){*(p1->edges), zeros1, zeros2, *(p2->edges)}, 2, 2, GrB_NULL);
    //fused_edges = [[p1.edges spzeros(Int8, size(p1.edges, 1), size(p2.edges, 2))];
    //                [spzeros(Int8, size(p2.edges, 1), size(p1.edges, 2)) p2.edges]];

    GrB_Matrix_nrows(&nrows1, *(p1->faces));
    GrB_Matrix_ncols(&ncols1, *(p1->faces));
    GrB_Matrix_nrows(&nrows2, *(p2->faces));
    GrB_Matrix_ncols(&ncols2, *(p2->faces));
    GrB_Matrix_resize(zeros1, nrows1, ncols2);
    GrB_Matrix_resize(zeros2, nrows2, ncols1);
    GrB_Matrix_new(fused_faces, GrB_INT8, nrows1+nrows2, ncols1+ncols2);
    GxB_Matrix_concat(*fused_faces, (GrB_Matrix[]){*(p1->faces), zeros1, zeros2, *(p2->faces)}, 2, 2, GrB_NULL);
    //fused_faces = [[p1.faces  spzeros(Int8, size(p1.faces, 1), size(p2.faces, 2))];
    //                [spzeros(Int8, size(p2.faces, 1), size(p1.faces, 2)) p2.faces]];

    GrB_Matrix_nrows(&nrows1, *(p1->volumes));
    GrB_Matrix_ncols(&ncols1, *(p1->volumes));
    GrB_Matrix_nrows(&nrows2, *(p2->volumes));
    GrB_Matrix_ncols(&ncols2, *(p2->volumes));
    GrB_Matrix_resize(zeros1, nrows1, ncols2);
    GrB_Matrix_resize(zeros2, nrows2, ncols1);
    GrB_Matrix_new(fused_volumes, GrB_INT8, nrows1+nrows2, ncols1+ncols2);
    GxB_Matrix_concat(*fused_volumes, (GrB_Matrix[]){*(p1->volumes), zeros1, zeros2, *(p2->volumes)}, 2, 2, GrB_NULL);

    res_p = new_Polyhedron3D_vefvs(fused_vertices, fused_edges, fused_faces, fused_volumes, fused_status);
    
    GrB_free(&zeros1);
    GrB_free(&zeros2);
    dealloc_vec_pts3D(fused_vertices); free(fused_vertices);
    GrB_free(fused_edges); free(fused_edges);
    GrB_free(fused_faces); free(fused_faces);
    GrB_free(fused_volumes); free(fused_volumes);
    dealloc_vec_int(fused_status); free(fused_status);
    
    return res_p;
}

/// @brief Deletes all empty volumes, faces and edges.
void clean_Polyhedron3D(const Polyhedron3D* p, Polyhedron3D** res_p){
    GrB_Index k, i_v, i, j, j_f;
    GrB_Vector vj, extr_vals_vj, face_indices;
    GrB_Vector fj, extr_vals_fj, grb_edge_indices;
    GrB_Vector ej, extr_vals_ej, pt_indices;
    Vector_uint* ind_kept_pts, *edge_indices;
    Vector_points3D* new_vertices;
    GrB_Matrix new_edges, new_faces, new_volumes;
    Vector_int *new_status_face;
    GrB_Vector grb_ind_kept_pts, justone;
    GrB_Info infogrb;
    GrB_Index nb_edges, nb_faces, nb_pts, nb_volumes;
    GrB_Index size_face_indices, size_grb_edge_indices, size_pt_indices, val;
    uint64_t ind_pt;
    GrB_Index ncols_new_edges, ncols_new_faces, ell;
    uint64_t nrows_new_edges;
    Polyhedron3D* new_p = NULL;
    Polyhedron3D* copy_p = new_Polyhedron3D();

    GrB_Matrix_ncols(&nb_volumes, *(p->volumes));
    GrB_Matrix_ncols(&nb_faces, *(p->faces));
    GrB_Matrix_ncols(&nb_edges, *(p->edges));
    GrB_Matrix_nrows(&nb_pts, *(p->edges));
    ind_kept_pts = alloc_empty_vec_uint();
    new_vertices = alloc_empty_vec_pts3D();
    edge_indices = alloc_empty_vec_uint();
    GrB_Matrix_new(&new_edges, GrB_INT8, 1, 1);
    GrB_Vector_new(&grb_ind_kept_pts, GrB_UINT64, 1);
    GrB_Matrix_new(&new_faces, GrB_INT8, 1, 1);
    GrB_Vector_new(&justone, GrB_UINT64, 1);
    GrB_Matrix_new(&new_volumes, GrB_INT8, 1, 1);
    new_status_face = alloc_with_capacity_vec_int(1);
    GrB_Vector_new(&vj, GrB_INT8, nb_faces);
    GrB_Vector_new(&face_indices, GrB_UINT64, nb_faces);
    GrB_Vector_new(&extr_vals_vj, GrB_INT8, nb_faces);
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&grb_edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);

    for(k=0; k<nb_volumes; k++){
        infogrb = GrB_extract(vj, GrB_NULL, GrB_NULL, *(p->volumes), GrB_ALL, 1, k, GrB_NULL); //Get indices of faces composing volume k
        infogrb = GxB_Vector_extractTuples_Vector(face_indices, extr_vals_vj, vj, GrB_NULL);
        infogrb = GrB_Vector_size(&size_face_indices, face_indices);
        ind_kept_pts->size = 0;
        for (i_v=0; i_v<size_face_indices; i_v++){
            infogrb = GrB_Vector_extractElement(&i, face_indices, i_v);

            infogrb = GrB_extract(fj, GrB_NULL, GrB_NULL, *(p->faces), GrB_ALL, 1, i, GrB_NULL); //Get indices of edges composing face i
            infogrb = GxB_Vector_extractTuples_Vector(grb_edge_indices, extr_vals_fj, fj, GrB_NULL);
            infogrb = GrB_Vector_size(&size_grb_edge_indices, grb_edge_indices);

            for(j_f=0; j_f<size_grb_edge_indices; j_f++){
                infogrb = GrB_Vector_extractElement(&j, grb_edge_indices, j_f);
                push_back_unique_vec_uint(&edge_indices, &j);
                infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *(p->edges), GrB_ALL, 1, j, GrB_NULL); //Get indices of points composing edge j
                infogrb = GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL);
                infogrb = GrB_Vector_size(&size_pt_indices, pt_indices);
                for(ell=0; ell<size_pt_indices; ell++){
                    infogrb = GrB_Vector_extractElement(&val, pt_indices, ell);
                    push_back_unique_vec_uint(&ind_kept_pts, &val);
                }
            }
        }

        sort_vec_uint(ind_kept_pts);
        sort_vec_uint(edge_indices);
        if (ind_kept_pts->size > 0){
            ind_pt = *get_ith_elem_vec_uint(ind_kept_pts, 0);
            set_ith_elem_vec_pts3D(new_vertices, 0, get_ith_elem_vec_pts3D(p->vertices, ind_pt));
            for (j_f=1; j_f<ind_kept_pts->size; j_f++){
                ind_pt = *get_ith_elem_vec_uint(ind_kept_pts, j_f);
                set_ith_elem_vec_pts3D(new_vertices, j_f, get_ith_elem_vec_pts3D(p->vertices, ind_pt));
            }
        }
        
        //new_edges = p.edges[ind_kept_pts, grb_edge_indices]
        if (ind_kept_pts->size>1){
            ncols_new_edges = edge_indices->size;
            ncols_new_faces = size_face_indices;
            nrows_new_edges = ind_kept_pts->size;
            GrB_Vector_resize(grb_ind_kept_pts, ind_kept_pts->size);
            for(ell=0; ell<ind_kept_pts->size; ell++){
                infogrb = GrB_Vector_setElement(grb_ind_kept_pts, *get_ith_elem_vec_uint(ind_kept_pts, ell), ell);
            }
            GrB_Vector_resize(grb_edge_indices, edge_indices->size);
            for (j_f=0; j_f<edge_indices->size; j_f++){
                infogrb = GrB_Vector_setElement(grb_edge_indices, *get_ith_elem_vec_uint(edge_indices, j_f), j_f);
            }
            GrB_Matrix_resize(new_edges, nrows_new_edges, ncols_new_edges);
            infogrb = GrB_extract(new_edges, GrB_NULL, GrB_NULL, *(p->edges), grb_ind_kept_pts, grb_edge_indices, GrB_NULL);

            //new_faces = p.faces[grb_edge_indices, i]
            GrB_Matrix_resize(new_faces, ncols_new_edges, ncols_new_faces);
            infogrb = GrB_extract(new_faces, GrB_NULL, GrB_NULL, *(p->faces), grb_edge_indices, face_indices, GrB_NULL);
            GrB_Matrix_resize(new_volumes, ncols_new_faces, 1);
            GrB_Vector_setElement(justone, 0, k);
            infogrb = GrB_extract(new_volumes, GrB_NULL, GrB_NULL, *(p->volumes), face_indices, justone, GrB_NULL);
        

            if (new_status_face){
                dealloc_vec_int(new_status_face);
                free(new_status_face);
            }
            new_status_face = alloc_with_capacity_vec_int(ncols_new_faces);
            for(j_f = 0; j_f < ncols_new_faces; j_f++){
                GrB_Vector_extractElement(&j, face_indices, j_f);
                set_ith_elem_vec_int(new_status_face, j_f, get_ith_elem_vec_int(p->status_face, j));
            }

            if (new_p){ //Faces already exist
                copy_Polyhedron3D(new_p, copy_p);
                dealloc_Polyhedron3D(new_p);
                new_p = fuse_polyhedrons(copy_p, new_Polyhedron3D_vefvs(new_vertices, &new_edges, &new_faces, &new_volumes, new_status_face));
            } else { //First face created
                new_p = new_Polyhedron3D_vefvs(new_vertices, &new_edges, &new_faces, &new_volumes, new_status_face);
            }
        }
    }

    if(*res_p){
        dealloc_Polyhedron3D(*res_p);
        free(*res_p);
    }
    *res_p = new_p;

    dealloc_Polyhedron3D(copy_p);free(copy_p);
    dealloc_vec_uint(edge_indices);free(edge_indices);
    dealloc_vec_uint(ind_kept_pts);free(ind_kept_pts);
    dealloc_vec_int(new_status_face);free(new_status_face);
    dealloc_vec_pts3D(new_vertices);free(new_vertices);
    GrB_free(&new_edges);
    GrB_free(&new_faces);
    GrB_free(&grb_ind_kept_pts);
    GrB_free(&justone);
    GrB_free(&fj);
    GrB_free(&extr_vals_fj);
    GrB_free(&grb_edge_indices);
    GrB_free(&ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&pt_indices);
    GrB_free(&vj);
    GrB_free(&face_indices);
    GrB_free(&extr_vals_vj);
}

void print_polyhedron3D(const Polyhedron3D* src, const char* file){
    FILE* output = fopen(file, "w");
    GrB_Info infogrb;
    GrB_Vector vj, extr_vals_vj, face_indices;
    GrB_Vector fj, extr_vals_fj, edge_indices;
    GrB_Vector ej, extr_vals_ej, pt_indices;
    GrB_Index nb_edges, nb_faces, nb_volumes, size_edge_indices, size_face_indices, size_pt_indices, val, nb_pts;
    GrB_Index i, j_f, j, k, i_v, ell;
    Point3D* pt;

    GrB_Matrix_ncols(&nb_faces, *(src->faces));
    GrB_Matrix_ncols(&nb_edges, *(src->edges));
    GrB_Matrix_nrows(&nb_pts, *(src->edges));
    GrB_Matrix_ncols(&nb_volumes, *(src->volumes));
    GrB_Vector_new(&vj, GrB_INT8, nb_faces);
    GrB_Vector_new(&face_indices, GrB_UINT64, nb_faces);
    GrB_Vector_new(&extr_vals_vj, GrB_INT8, nb_faces);
    GrB_Vector_new(&fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&edge_indices, GrB_UINT64, nb_edges);
    GrB_Vector_new(&extr_vals_fj, GrB_INT8, nb_edges);
    GrB_Vector_new(&ej, GrB_INT8, nb_pts);
    GrB_Vector_new(&pt_indices, GrB_UINT64, nb_pts);
    GrB_Vector_new(&extr_vals_ej, GrB_INT8, nb_pts);

    for(k=0; k<nb_volumes; k++){
        infogrb = GrB_extract(vj, GrB_NULL, GrB_NULL, *(src->volumes), GrB_ALL, 1, k, GrB_NULL); //Get indices of faces composing volume k
        infogrb = GxB_Vector_extractTuples_Vector(face_indices, extr_vals_vj, vj, GrB_NULL);
        infogrb = GrB_Vector_size(&size_face_indices, face_indices);
        for (i_v=0; i_v<size_face_indices; i_v++){
            infogrb = GrB_Vector_extractElement(&i, face_indices, i_v);

            infogrb = GrB_extract(fj, GrB_NULL, GrB_NULL, *(src->faces), GrB_ALL, 1, i, GrB_NULL); //Get indices of edges composing face i
            infogrb = GxB_Vector_extractTuples_Vector(edge_indices, extr_vals_fj, fj, GrB_NULL);
            infogrb = GrB_Vector_size(&size_edge_indices, edge_indices);

            for(j_f=0; j_f<size_edge_indices; j_f++){
                infogrb = GrB_Vector_extractElement(&j, edge_indices, j_f);
                infogrb = GrB_extract(ej, GrB_NULL, GrB_NULL, *(src->edges), GrB_ALL, 1, j, GrB_NULL); //Get indices of points composing edge j
                infogrb = GxB_Vector_extractTuples_Vector(pt_indices, extr_vals_ej, ej, GrB_NULL);
                infogrb = GrB_Vector_size(&size_pt_indices, pt_indices);
                //if(size_pt_indices>0){
                for(ell=0; ell<size_pt_indices; ell+=2){
                    infogrb = GrB_Vector_extractElement(&val, pt_indices, ell);
                    pt = get_ith_elem_vec_pts3D(src->vertices, val);
                    fprintf(output, "%lf %lf %lf\n", pt->x, pt->y, pt->t);

                    infogrb = GrB_Vector_extractElement(&val, pt_indices, ell+1);
                    pt = get_ith_elem_vec_pts3D(src->vertices, val);
                    fprintf(output, "%lf %lf %lf\n", pt->x, pt->y, pt->t);

                    fprintf(output, "\n");
                    fprintf(output, "\n");
                }
            }
        }
    }
    fclose(output);

    GrB_free(&fj);
    GrB_free(&extr_vals_fj);
    GrB_free(&edge_indices);
    GrB_free(&ej);
    GrB_free(&extr_vals_ej);
    GrB_free(&pt_indices);
    GrB_free(&vj);
    GrB_free(&face_indices);
    GrB_free(&extr_vals_vj);
}
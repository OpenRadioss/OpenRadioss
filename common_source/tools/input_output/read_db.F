Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    read_db              ../common_source/tools/input_output/read_db.F
      !||--- called by ------------------------------------------------------
      !||    alelag_rrest         ../engine/source/output/restart/rdresb.F
      !||    allocbuf_auto        ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    cfieldrest           ../engine/source/output/restart/rdresb.F
      !||    convrrest            ../engine/source/output/restart/rdresb.F
      !||    crkxfem_rrestanim    ../engine/source/output/restart/rdresb.F
      !||    decompress_r_nnz     ../common_source/comm/decompress_nnz.F
      !||    drape_rrest          ../engine/source/output/restart/rdresb.F
      !||    dsrrest              ../engine/source/output/restart/rdresb.F
      !||    eigrrest             ../engine/source/output/restart/rdresb.F
      !||    fvrrest              ../engine/source/output/restart/rdresb.F
      !||    fxbresb              ../engine/source/output/restart/rdresb.F
      !||    fxfluxrrest          ../engine/source/constraints/thermic/fxfluxrrest.F
      !||    fxtemprrest          ../engine/source/output/restart/rdresb.F
      !||    imprrest             ../engine/source/output/restart/rdresb.F
      !||    intbuf_ini           ../common_source/interf/intbuf_ini.F
      !||    intfric_rrestr       ../common_source/modules/interfaces/intbuf_fric_mod.F90
      !||    intstamp_rrestr      ../engine/share/modules/intstamp_mod.F
      !||    knotlocelrest        ../engine/source/output/restart/rdresb.F
      !||    knotlocpcrest        ../engine/source/output/restart/rdresb.F
      !||    knotrest             ../engine/source/output/restart/rdresb.F
      !||    lec_inimap1d_file    ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lec_inimap2d_file    ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    loadprest            ../engine/source/output/restart/rdresb.F
      !||    nfrrest              ../engine/source/output/restart/rdresb.F
      !||    nitscherrest         ../engine/source/output/restart/rdresb.F
      !||    plyxfem_ravuply      ../engine/source/output/restart/rdresb.F
      !||    plyxfem_rrest        ../engine/source/output/restart/rdresb.F
      !||    r_bufbric_22         ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    radiarrest           ../engine/source/output/restart/rdresb.F
      !||    rdresb               ../engine/source/output/restart/rdresb.F
      !||    read_ale_grid        ../engine/source/output/restart/read_ale_grid.F90
      !||    read_bcs_wall        ../engine/source/output/restart/read_bcs_wall.F90
      !||    read_cluster         ../engine/source/output/cluster/read_cluster.F
      !||    read_elgroup_param   ../engine/source/output/restart/read_elgroup_param.F
      !||    read_eosparam        ../engine/source/output/restart/read_eosparam.F90
      !||    read_failparam       ../engine/source/output/restart/read_failparam.F
      !||    read_inivel          ../engine/source/output/restart/read_inivel.F90
      !||    read_mat_table       ../engine/source/materials/tools/read_mat_table.F
      !||    read_matparam        ../engine/source/output/restart/read_matparam.F
      !||    read_nloc_struct     ../engine/source/output/restart/read_nloc_struct.F
      !||    read_pcyl            ../engine/source/output/restart/read_pcyl.F
      !||    read_sensors         ../engine/source/output/restart/read_sensors.F
      !||    read_thermparam      ../engine/source/output/restart/read_thermparam.F90
      !||    read_units           ../common_source/comm/write_units.F
      !||    read_viscparam       ../engine/source/output/restart/read_viscparam.F
      !||    rmatrrest            ../engine/source/output/restart/rdresb.F
      !||    rthbufrest           ../engine/source/output/restart/rdresb.F
      !||    spmd_inisph          ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_initfi          ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    stack_rrest          ../engine/source/output/restart/rdresb.F
      !||    table_rrestr         ../engine/source/tools/curve/table_tools.F
      !||    thcrrest             ../engine/source/output/restart/rdresb.F
      !||    userwi_read          ../engine/source/user_interface/userwindow_interface_routines.F
      !||    wigerest             ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    read_db_c            ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE READ_DB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
         CALL READ_DB_C(R4,1000)
         DO J = 1, 1000
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
         CALL READ_DB_C(R4,N-N2)
         DO J = 1, N-N2
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    read_db_array     ../common_source/tools/input_output/read_db.F
      !||--- called by ------------------------------------------------------
      !||    intfric_rrestr    ../common_source/modules/interfaces/intbuf_fric_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE READ_DB_ARRAY(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
            INTEGER N
C     REAL
            my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
            INTEGER I,J,K,N1,N2
            DOUBLE PRECISION R8(1000)
            REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
            K=0
            N1=N/1000
            IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
             DO I = 1, N1
               CALL READ_DB_ARRAY_C(R4,1000)
               DO J = 1, 1000
                 K=K+1
                 A(K) = R4(J)
               ENDDO
             ENDDO
             N2=N1*1000
             IF(N2/=N)THEN
               CALL READ_DB_ARRAY_C(R4,N-N2)
               DO J = 1, N-N2
                 K=K+1
                 A(K) = R4(J)
               ENDDO
             ENDIF
            ELSE
C fortran double precision : 64 bits C double : 64 bits
             DO I = 1, N1
              CALL READ_DB_ARRAY_C(R8,1000)
              DO J = 1, 1000
                K=K+1
                A(K) = R8(J)
              ENDDO
             ENDDO
             N2=N1*1000
             IF(N2/=N)THEN
              CALL READ_DB_ARRAY_C(R8,N-N2)
              DO J = 1, N-N2
                K=K+1
                A(K) = R8(J)
              ENDDO
             ENDIF
            ENDIF
C
            RETURN
            END    
      !||====================================================================
      !||    read_dpdb         ../common_source/tools/input_output/read_db.F
      !||--- called by ------------------------------------------------------
      !||    allocbuf_auto     ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    intstamp_rrestr   ../engine/share/modules/intstamp_mod.F
      !||    rdresb            ../engine/source/output/restart/rdresb.F
      !||    read_sensors      ../engine/source/output/restart/read_sensors.F
      !||--- calls      -----------------------------------------------------
      !||    read_db_c         ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE READ_DPDB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      DOUBLE PRECISION A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
C
      RETURN
      END

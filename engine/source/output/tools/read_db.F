Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  READ_DB                       source/output/tools/read_db.F 
Chd|-- called by -----------
Chd|        DECOMPRESS_R_NNZ              ../common_source/comm/decompress_nnz.F
Chd|        INTBUF_INI                    ../common_source/interf/intbuf_ini.F
Chd|        PBLAST_LOAD                   ../common_source/modules/loads/pblast_mod.F
Chd|        READ_UNITS                    ../common_source/comm/write_units.F
Chd|        ALELAG_RREST                  source/output/restart/rdresb.F
Chd|        ALLOCBUF_AUTO                 source/elements/elbuf/allocbuf_auto.F
Chd|        CFIELDREST                    source/output/restart/rdresb.F
Chd|        CONVRREST                     source/output/restart/rdresb.F
Chd|        CRKXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        DRAPE_RREST                   source/output/restart/rdresb.F
Chd|        DSRREST                       source/output/restart/rdresb.F
Chd|        EIGRREST                      source/output/restart/rdresb.F
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|        FXBRESB                       source/output/restart/rdresb.F
Chd|        FXFLUXRREST                   source/constraints/thermic/fxfluxrrest.F
Chd|        FXTEMPRREST                   source/output/restart/rdresb.F
Chd|        IMPRREST                      source/output/restart/rdresb.F
Chd|        INTFRIC_RRESTR                share/modules/intbuf_fric_mod.F
Chd|        INTSTAMP_RRESTR               share/modules/intstamp_mod.F  
Chd|        KNOTLOCELREST                 source/output/restart/rdresb.F
Chd|        KNOTLOCPCREST                 source/output/restart/rdresb.F
Chd|        KNOTREST                      source/output/restart/rdresb.F
Chd|        LOADPREST                     source/output/restart/rdresb.F
Chd|        NFRREST                       source/output/restart/rdresb.F
Chd|        NITSCHERREST                  source/output/restart/rdresb.F
Chd|        PLYXFEM_RAVUPLY               source/output/restart/rdresb.F
Chd|        PLYXFEM_RREST                 source/output/restart/rdresb.F
Chd|        RADIARREST                    source/output/restart/rdresb.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_CLUSTER                  source/output/cluster/read_cluster.F
Chd|        READ_ELGROUP_PARAM            source/output/restart/read_elgroup_param.F
Chd|        READ_FAILPARAM                source/output/restart/read_failparam.F
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|        READ_MAT_TABLE                source/materials/tools/read_mat_table.F
Chd|        READ_NLOC_STRUCT              source/output/restart/read_nloc_struct.F
Chd|        READ_PCYL                     source/output/restart/read_pcyl.F
Chd|        READ_SENSORS                  source/output/restart/read_sensors.F
Chd|        READ_VISCPARAM                source/output/restart/read_viscparam.F
Chd|        RMATRREST                     source/output/restart/rdresb.F
Chd|        RTHBUFREST                    source/output/restart/rdresb.F
Chd|        R_BUFBRIC_22                  source/interfaces/int22/r_bufbric_22.F
Chd|        SPMD_INISPH                   source/mpi/sph/spmd_sptool.F  
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        STACK_RREST                   source/output/restart/rdresb.F
Chd|        TABLE_RRESTR                  source/tools/curve/table_tools.F
Chd|        THCRREST                      source/output/restart/rdresb.F
Chd|        USERWI_READ                   source/user_interface/userwindow_interface_routines.F
Chd|        WIGEREST                      source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB_C                     source/output/tools/sortie_c.c
Chd|====================================================================
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

Chd|====================================================================
Chd|  READ_DPDB                     source/output/tools/read_db.F 
Chd|-- called by -----------
Chd|        ALLOCBUF_AUTO                 source/elements/elbuf/allocbuf_auto.F
Chd|        INTSTAMP_RRESTR               share/modules/intstamp_mod.F  
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_SENSORS                  source/output/restart/read_sensors.F
Chd|-- calls ---------------
Chd|        READ_DB_C                     source/output/tools/sortie_c.c
Chd|====================================================================
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

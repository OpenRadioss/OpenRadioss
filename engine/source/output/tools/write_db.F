Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  WRITE_DB                      source/output/tools/write_db.F
Chd|-- called by -----------
Chd|        COMPRESS_R_NNZ                ../common_source/comm/compress_nnz.F
Chd|        PBLAST_WRITE                  ../common_source/modules/pblast_mod.F
Chd|        WRITE_UNITS                   ../common_source/comm/write_units.F
Chd|        ALELAG_WREST                  source/output/restart/wrrest.F
Chd|        CONVWREST                     source/output/restart/wrrest.F
Chd|        CRKXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        DRAPE_WREST                   source/output/restart/wrrest.F
Chd|        DSWREST                       source/output/restart/wrrest.F
Chd|        EIGWREST                      source/output/restart/wrrest.F
Chd|        FVWREST                       source/output/restart/wrrest.F
Chd|        FXBWREST                      source/output/restart/wrrest.F
Chd|        FXFLUXWREST                   source/constraints/thermic/fxfluxwrest.F
Chd|        FXTEMPWREST                   source/output/restart/wrrest.F
Chd|        IMPWREST                      source/output/restart/wrrest.F
Chd|        INTFRIC_WRESTR                share/modules/intbuf_fric_mod.F
Chd|        INTSTAMP_WRESTR               share/modules/intstamp_mod.F  
Chd|        NFWREST                       source/output/restart/wrrest.F
Chd|        NITSCHEWREST                  source/output/restart/wrrest.F
Chd|        PLYXFEM_WRAVUPLY              source/output/restart/wrrest.F
Chd|        PLYXFEM_WREST                 source/output/restart/wrrest.F
Chd|        RADIAWREST                    source/output/restart/wrrest.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RIGMATWREST                   source/output/restart/wrrest.F
Chd|        RTHBUFWREST                   source/output/restart/wrrest.F
Chd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SAVESPH                  source/mpi/sph/spmd_sptool.F  
Chd|        STACK_WREST                   source/output/restart/wrrest.F
Chd|        STAT_INIMAP1D_FILE_SPMD       source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       source/output/sta/stat_inimap2d_file_spmd.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|        THCWREST                      source/output/restart/wrrest.F
Chd|        WRITE_ELGROUP_PARAM           source/output/restart/write_elgroup_param.F
Chd|        WRITE_INTBUF                  source/output/restart/write_intbuf.F
Chd|        WRITE_MATPARAM                source/output/restart/write_matparam.F
Chd|        WRITE_NLOC_STRUCT             source/output/restart/write_nloc_struct.F
Chd|        WRITE_SENSOR_TAB              source/output/restart/write_sensor_tab.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        W_BUFBRIC_22                  source/interfaces/int22/w_bufbric_22.F
Chd|        W_CLUSTER                     source/output/cluster/w_cluster.F
Chd|        W_ELBUF_STR                   source/elements/elbuf/w_elbuf_str.F
Chd|-- calls ---------------
Chd|        WRITE_DB_C                    source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE WRITE_DB(A,N)
C ecriture nombre reel : routine tampon
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
      IF(ICRAY.EQ.1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,1000)
       ENDDO
       N2=N1*1000
       IF(N2.NE.N)THEN
        DO J = 1, N-N2
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,N-N2)
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2.NE.N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  WRITE_DPDB                    source/output/tools/write_db.F
Chd|-- called by -----------
Chd|        ADD_ELAPSED_TIME_MON_OFF      source/system/timer.F         
Chd|        INTSTAMP_WRESTR               share/modules/intstamp_mod.F  
Chd|        PRINTIME                      source/system/timer.F         
Chd|        RESOL                         source/engine/resol.F         
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        W_ELBUF_STR                   source/elements/elbuf/w_elbuf_str.F
Chd|-- calls ---------------
Chd|        WRITE_DB_C                    source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE WRITE_DPDB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      DOUBLE PRECISION
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
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2.NE.N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
C
      RETURN
      END

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
#include "hardware.inc"
Chd|====================================================================
Chd|  SPMD_ALL_DMIN                 source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        SENSOR0                       source/tools/sensor/sensor0.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_ALL_DMIN(V,LEN)
C min tableau V de taille LEN de type my_real
C resultat dans VTMP de taille LEN
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE), I, IERROR
      my_real
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_ALLREDUCE(V,VTMP,LEN,REAL,MPI_MIN,
     .                     MPI_COMM_WORLD,IERROR)
        IF(ISPMD==0)THEN
          DO I = 1, LEN
            V(I) = VTMP(I)
          END DO
        END IF
      ENDIF
C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        GET_U_NOD_A                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_D                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_V                   source/user_interface/uaccess.F
Chd|        GET_U_NOD_X                   source/user_interface/uaccess.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_DSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type my_real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real V(LEN),VTMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  REAL,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,ierror)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_DPSUM               source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_DPSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type my_dp
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"

C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      DOUBLE PRECISION V(LEN),VTMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_DOUBLE_PRECISION,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,ierror)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_FSUM                source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_FSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type REAL*4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      REAL*4 V(LEN),VTMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_REAL4,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,ierror)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_PART_COM                 source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_PART_COM(TAG,MAIN,ICOMV)
C rempli sur pmain le tableau ICOMV suivant la valeur de TAG locale
C ie sur main : ICOMV(P) = "TAG sur p"
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAG, MAIN, ICOMV(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,K,LOC_PROC
      DATA MSGOFF/8001/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD+1
      IF (LOC_PROC/=MAIN) THEN
        MSGTYP=MSGOFF
            CALL MPI_SEND(TAG,1,MPI_INTEGER,IT_SPMD(MAIN),
     .                    MSGTYP,MPI_COMM_WORLD,IERROR)
      ELSE
        ICOMV(MAIN) = TAG
        DO K=1,NSPMD
          IF(K/=MAIN) THEN
            MSGTYP=MSGOFF
            CALL MPI_RECV(TAG,1,MPI_INTEGER,IT_SPMD(K),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
            ICOMV(K) = TAG
          END IF
        END DO
      ENDIF
C
#endif
      RETURN
      END
C

Chd|====================================================================
Chd|  SPMD_GLOB_FSUM9               source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        VELVECC                       source/output/anim/generate/velvec.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_FSUM9(V,LEN)
C gather tableau V de taille LEN de type real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      REAL
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IERROR,
     .        STATUS(MPI_STATUS_SIZE)
      REAL
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_REAL,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        ALELIN                        source/ale/grid/alelin.F      
Chd|        ANI_PCONT21                   source/output/anim/generate/ani_pcont.F
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|        OUTP_MT                       source/output/sty/outp_mt.F   
Chd|        OUTP_N_V2                     source/output/sty/outp_n_v.F  
Chd|        OUTP_N_VC                     source/output/sty/outp_n_v.F  
Chd|        PNOISE                        source/general_controls/computation/pnoise.F
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|        SENSOR0                       source/tools/sensor/sensor0.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|        SORTIE_ERROR                  source/output/sortie_error.F  
Chd|        THCLUSTER                     source/output/th/thcluster.F  
Chd|        VELVEC2                       source/output/anim/generate/velvec.F
Chd|        VELVECC21                     source/output/anim/generate/velvec.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        WRTDES0                       source/output/th/wrtdes0.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_DSUM9(V,LEN)
C gather tableau V de taille LEN de type my_real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IERROR,
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  REAL,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_GLOB_DPSUM9              source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_DPSUM9(V,LEN)
C gather tableau V de taille LEN de type double precision
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      DOUBLE PRECISION
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IERROR,
     .        STATUS(MPI_STATUS_SIZE)
      DOUBLE PRECISION
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_DOUBLE_PRECISION,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|        CNTSKEW                       source/output/anim/generate/aniskewf.F
Chd|        DONESEC                       source/output/anim/generate/donesec.F
Chd|        DRBE2CNT                      source/output/anim/generate/drbe2cnt.F
Chd|        DRBE3CNT                      source/output/anim/generate/drbe3cnt.F
Chd|        DRBYCNT                       source/output/anim/generate/drbycnt.F
Chd|        DSECCNT                       source/output/anim/generate/dseccnt.F
Chd|        DSPHCNT                       source/output/anim/generate/dsphcnt.F
Chd|        DYNAIN_SIZE_C                 source/output/dynain/dynain_size.F
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|        FR_RLALE                      source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FR_RLINK1                     source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_CREATE_RBE2_IMPI          source/output/h3d/h3d_build_fortran/h3d_create_rbe2_impi.F
Chd|        H3D_CREATE_RBE3_IMPI          source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
Chd|        H3D_CREATE_RBODIES_IMPI       source/output/h3d/h3d_build_fortran/h3d_create_rbodies_impi.F
Chd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        LCBCSF                        source/constraints/general/bcs/lcbcsf.F
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        LECTUR                        source/input/lectur.F         
Chd|        PNOISE                        source/general_controls/computation/pnoise.F
Chd|        PRINTIME                      source/system/timer.F         
Chd|        RBYONF                        source/constraints/general/rbody/rbyonf.F
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SENSOR0                       source/tools/sensor/sensor0.F 
Chd|        SPMD_DPARRBE2                 source/mpi/anim/spmd_dparrbe2.F
Chd|        SPMD_DPARRBE3                 source/mpi/anim/spmd_dparrbe3.F
Chd|        SPMD_DPARRBY                  source/mpi/anim/spmd_dparrby.F
Chd|        STAT_SIZE_C                   source/output/sta/stat_size.F 
Chd|        SZ_PRINT                      source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_ISUM9(V,LEN)
C gather tableau V de taille LEN de type integer
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,IERROR,
     .        VTMP(LEN),STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_MIN                 source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_MIN(V,LEN)
C gather tableau V de taille LEN de type reel
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .  V(LEN),VTMP(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,IERROR,
     .        STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  REAL,MPI_MIN,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_GLOB_MAX                 source/mpi/interfaces/spmd_th.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_MAX(V,LEN)
C gather tableau V de taille LEN de type reel
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .  V(LEN),VTMP(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,IERROR,
     .        STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  REAL,MPI_MAX,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
C

Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    spmd_all_dmin   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_ALL_DMIN(V,LEN)
C min tableau V de taille LEN de type my_real
C resultat dans VTMP de taille LEN
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                     SPMD_COMM_WORLD,IERROR)
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

      !||====================================================================
      !||    spmd_glob_dsum   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    get_u_nod_a      ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_d      ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_v      ../engine/source/user_interface/uaccess.F
      !||    get_u_nod_x      ../engine/source/user_interface/uaccess.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_DSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type my_real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,ierror)
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
      !||====================================================================
      !||    spmd_glob_dpsum   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_DPSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type my_dp
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"

C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,ierror)
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
      !||====================================================================
      !||    spmd_glob_fsum   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_FSUM(V,LEN,VTMP)
C gather tableau V de taille LEN de type REAL*4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,ierror)
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
      !||====================================================================
      !||    spmd_part_com   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    rbypid          ../engine/source/constraints/general/rbody/rbypid.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_PART_COM(TAG,MAIN,ICOMV)
C rempli sur pmain le tableau ICOMV suivant la valeur de TAG locale
C ie sur main : ICOMV(P) = "TAG sur p"
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
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
     .                    MSGTYP,SPMD_COMM_WORLD,IERROR)
      ELSE
        ICOMV(MAIN) = TAG
        DO K=1,NSPMD
          IF(K/=MAIN) THEN
            MSGTYP=MSGOFF
            CALL MPI_RECV(TAG,1,MPI_INTEGER,IT_SPMD(K),
     .                    MSGTYP,SPMD_COMM_WORLD,STATUS,IERROR)
            ICOMV(K) = TAG
          END IF
        END DO
      ENDIF
C
#endif
      RETURN
      END
C

      !||====================================================================
      !||    spmd_glob_fsum9   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    velvecc           ../engine/source/output/anim/generate/velvec.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    my_alloc_mod      ../common_source/tools/memory/my_alloc.F90
      !||====================================================================
      SUBROUTINE SPMD_GLOB_FSUM9(V,LEN)
C gather tableau V de taille LEN de type real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
      REAL, DIMENSION(:),ALLOCATABLE :: VTMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MY_ALLOC(VTMP,LEN)
      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_REAL,MPI_SUM,IT_SPMD(1),
     .                  SPMD_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD==0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
      DEALLOCATE(VTMP)
#endif
      RETURN
      END
C
      !||====================================================================
      !||    spmd_glob_dsum9   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    alelin            ../engine/source/ale/grid/alelin.F
      !||    ani_pcont21       ../engine/source/output/anim/generate/ani_pcont.F
      !||    ecrit             ../engine/source/output/ecrit.F
      !||    hist2             ../engine/source/output/th/hist2.F
      !||    i9wal2            ../engine/source/interfaces/int09/i9wal2.F
      !||    i9wal3            ../engine/source/interfaces/int09/i9wal3.F
      !||    intstamp_ass      ../engine/source/interfaces/int21/intstamp_ass.F
      !||    noise             ../engine/source/general_controls/computation/noise.F
      !||    outp_mt           ../engine/source/output/sty/outp_mt.F
      !||    outp_n_v2         ../engine/source/output/sty/outp_n_v.F
      !||    outp_n_vc         ../engine/source/output/sty/outp_n_v.F
      !||    pnoise            ../engine/source/general_controls/computation/pnoise.F
      !||    section_io        ../engine/source/tools/sect/section_io.F
      !||    sensor_spmd       ../engine/source/tools/sensor/sensor_spmd.F
      !||    sms_pcg           ../engine/source/ams/sms_pcg.F
      !||    sms_produt3       ../engine/source/ams/sms_proj.F
      !||    sortie_error      ../engine/source/output/sortie_error.F
      !||    thcluster         ../engine/source/output/th/thcluster.F
      !||    velvec2           ../engine/source/output/anim/generate/velvec.F
      !||    velvecc21         ../engine/source/output/anim/generate/velvec.F
      !||    wrrestp           ../engine/source/output/restart/wrrestp.F
      !||    wrtdes0           ../engine/source/output/th/wrtdes0.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_DSUM9(V,LEN)
C gather tableau V de taille LEN de type my_real
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,IERROR)
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
      !||====================================================================
      !||    spmd_glob_dpsum9   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    intstamp_ass       ../engine/source/interfaces/int21/intstamp_ass.F
      !||    sms_pcg            ../engine/source/ams/sms_pcg.F
      !||    sms_produt3        ../engine/source/ams/sms_proj.F
      !||    sms_produt_h       ../engine/source/ams/sms_proj.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_DPSUM9(V,LEN)
C gather tableau V de taille LEN de type double precision
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,IERROR)
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
      !||====================================================================
      !||    spmd_glob_isum9                   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- called by ------------------------------------------------------
      !||    cntskew                           ../engine/source/output/anim/generate/aniskewf.F
      !||    donesec                           ../engine/source/output/anim/generate/donesec.F
      !||    drbe2cnt                          ../engine/source/output/anim/generate/drbe2cnt.F
      !||    drbe3cnt                          ../engine/source/output/anim/generate/drbe3cnt.F
      !||    drbycnt                           ../engine/source/output/anim/generate/drbycnt.F
      !||    dseccnt                           ../engine/source/output/anim/generate/dseccnt.F
      !||    dsphcnt                           ../engine/source/output/anim/generate/dsphcnt.F
      !||    dynain_size_c                     ../engine/source/output/dynain/dynain_size.F
      !||    find_dt_for_targeted_added_mass   ../engine/source/time_step/find_dt_for_targeted_added_mass.F
      !||    fr_rlale                          ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fr_rlink1                         ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fxbypid                           ../engine/source/constraints/fxbody/fxbypid.F
      !||    genani                            ../engine/source/output/anim/generate/genani.F
      !||    h3d_create_rbe2_impi              ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe2_impi.F
      !||    h3d_create_rbe3_impi              ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
      !||    h3d_create_rbodies_impi           ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbodies_impi.F
      !||    i21_icrit                         ../engine/source/interfaces/intsort/i21_icrit.F
      !||    i9wal2                            ../engine/source/interfaces/int09/i9wal2.F
      !||    i9wal3                            ../engine/source/interfaces/int09/i9wal3.F
      !||    lcbcsf                            ../engine/source/constraints/general/bcs/lcbcsf.F
      !||    lecnoise                          ../engine/source/general_controls/computation/lecnoise.F
      !||    lectur                            ../engine/source/input/lectur.F
      !||    pnoise                            ../engine/source/general_controls/computation/pnoise.F
      !||    printime                          ../engine/source/system/timer.F
      !||    rbyonf                            ../engine/source/constraints/general/rbody/rbyonf.F
      !||    rbypid                            ../engine/source/constraints/general/rbody/rbypid.F
      !||    resol                             ../engine/source/engine/resol.F
      !||    sensor_spmd                       ../engine/source/tools/sensor/sensor_spmd.F
      !||    spmd_dparrbe2                     ../engine/source/mpi/anim/spmd_dparrbe2.F
      !||    spmd_dparrbe3                     ../engine/source/mpi/anim/spmd_dparrbe3.F
      !||    spmd_dparrby                      ../engine/source/mpi/anim/spmd_dparrby.F
      !||    stat_size_c                       ../engine/source/output/sta/stat_size.F
      !||    sz_print                          ../engine/source/output/restart/arralloc.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_ISUM9(V,LEN)
C gather tableau V de taille LEN de type integer
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,IERROR)
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
      !||====================================================================
      !||    spmd_glob_min   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_MIN(V,LEN)
C gather tableau V de taille LEN de type reel
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,IERROR)
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
      !||====================================================================
      !||    spmd_glob_max   ../engine/source/mpi/interfaces/spmd_th.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_GLOB_MAX(V,LEN)
C gather tableau V de taille LEN de type reel
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
     .                  SPMD_COMM_WORLD,IERROR)
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

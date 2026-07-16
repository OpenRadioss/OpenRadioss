Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    stat_n_temp       ../engine/source/output/sta/stat_n_temp.F
!||--- called by ------------------------------------------------------
!||    genstat           ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||    spmd_dstat_gath   ../engine/source/mpi/output/spmd_stat.F
!||    strs_txt50        ../engine/source/output/sta/sta_txt.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod      ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE STAT_N_TEMP(X,NUMNOD,ITAB,ITABG,LENG,
     .                    NODGLOB,WEIGHT,NODTAG,TEMP,ITHERM_FE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), NUMNOD,WEIGHT(*),ITABG(*),LENG,
     .        NODGLOB(*),NODTAG(*)
      INTEGER ,intent(in) :: ITHERM_FE
      my_real
     .   X(3,*),TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER,DIMENSION(:),ALLOCATABLE::NODTAGLOB
      my_real,DIMENSION(:),ALLOCATABLE::TEMPG
      CHARACTER*100 LINE
C-----------------------------------------------
      call my_alloc(NODTAGLOB, LENG, "NODTAGLOB")
      call my_alloc(TEMPG, LENG, "TEMPG")
C-----------------------------------------------
      IF (ITHERM_FE == 0) RETURN

      IF (NSPMD > 1) THEN
        CALL SPMD_DSTAT_GATH(TEMP,NODGLOB,WEIGHT,TEMPG,NODTAG,
     .                        NODTAGLOB)
        IF (ISPMD/=0) RETURN
      ENDIF

      IF (IZIPSTRS == 0) THEN
        WRITE(IUGEO,'(A)')'/INITEMP/1'
        WRITE(IUGEO,'(A)')'INITIAL TEMPERATURE'
        WRITE(IUGEO,'(2A)')'#    TEMP               NODID'
        WRITE(IUGEO,'(A)')'                                       1'
      ELSE
        WRITE(LINE,'(A)')'/INITEMP/1'
               CALL STRS_TXT50(LINE,100)
        WRITE(LINE,'(A)')'INITIAL TEMPERATURE'
               CALL STRS_TXT50(LINE,100)
        WRITE(LINE,'(A)')
     .      '#    TEMP               NODID'
               CALL STRS_TXT50(LINE,100)
        WRITE(LINE,'(A)')'                                       1'
               CALL STRS_TXT50(LINE,100)
      ENDIF

      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
           IF(NODTAG(I) /= 0) THEN
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(1PE20.13,I10)')
     .         TEMP(I),ITAB(I)
             ELSE
               WRITE(LINE,'(1PE20.13,I10)')
     .         TEMP(I),ITAB(I)
               CALL STRS_TXT50(LINE,100)
             END IF
           END IF
        END DO
      ELSE
        DO I=1,NUMNODG
          IF(NODTAGLOB(I) /= 0)THEN
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(1PE20.13,I10)')
     .         TEMPG(I),ITABG(I)
             ELSE
               WRITE(LINE,'(1PE20.13,I10)')
     .         TEMPG(I),ITABG(I)
               CALL STRS_TXT50(LINE,100)
             END IF
          END IF
        END DO
      ENDIF
C-----------------------------------------------
      call my_dealloc(NODTAGLOB)
      call my_dealloc(TEMPG)
C-----------------------------------------------
      RETURN
      END

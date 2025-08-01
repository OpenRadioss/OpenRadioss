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
!||    stat_n_vel         ../engine/source/output/sta/state_n_vel.F
!||--- called by ------------------------------------------------------
!||    genstat            ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||    spmd_dstat_vgath   ../engine/source/mpi/output/spmd_stat.F
!||    strs_txt50         ../engine/source/output/sta/sta_txt.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod       ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE STAT_N_VEL(NUMNOD ,ITAB   ,ITABG ,LENG ,NODGLOB,
     .                      WEIGHT ,NODTAG ,V     ,VR   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
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
      INTEGER ITAB(*),NUMNOD,WEIGHT(*),ITABG(*),LENG,
     .        NODGLOB(*),NODTAG(*)
      my_real
     .   V(3,*),VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      INTEGER,DIMENSION(:),ALLOCATABLE :: NODTAGLOB
      my_real,DIMENSION(:,:),ALLOCATABLE :: VG
      my_real,DIMENSION(:,:),ALLOCATABLE :: VRG
      CHARACTER(LEN=100) :: LINE
      INTEGER LEN_LINE
C-----------------------------------------------
      CALL MY_ALLOC(NODTAGLOB,LENG)
      CALL MY_ALLOC(VG,3,LENG)
      CALL MY_ALLOC(VRG,3,LENG)
C-----------------------------------------------
!
      IF (NSPMD > 1) THEN
        CALL SPMD_DSTAT_VGATH(V,NODGLOB,WEIGHT,VG,NODTAG,
     .                       NODTAGLOB)
        CALL SPMD_DSTAT_VGATH(VR,NODGLOB,WEIGHT,VRG,NODTAG,
     .                       NODTAGLOB)
        IF (ISPMD /= 0) RETURN
      ENDIF

      IF (IZIPSTRS == 0) THEN
        WRITE(IUGEO,'(A)') '/INIVEL/NODE/1'
        WRITE(IUGEO,'(A)') 'INITIAL NODAL VELOCITY'
      ELSE
        ! Write STATE File in GZIP format
        WRITE(LINE,'(A)') '/INIVEL/NODE/1'
        LEN_LINE=LEN_TRIM(LINE)
        CALL STRS_TXT50(LINE,LEN_LINE)
!
        WRITE(LINE,'(A)') 'INITIAL NODAL VELOCITY'
        LEN_LINE=LEN_TRIM(LINE)
        CALL STRS_TXT50(LINE,LEN_LINE)
      ENDIF
!
      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
           IF(NODTAG(I) /= 0) THEN
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(2I10,1P3E20.9)') ITAB(I),0,(V(J,I),J=1,3)
               IF (IRODDL == 0)THEN
                 WRITE(IUGEO,'(20X)')
               ELSE
                 WRITE(IUGEO,'(20X,1P3E20.9)') (VR(J,I),J=1,3)
               END IF
             ELSE
               ! Write STATE File in GZIP format
               WRITE(LINE,'(2I10,1P3E20.9)')  ITAB(I),0,(V(J,I),J=1,3)
               LEN_LINE=LEN_TRIM(LINE)
               CALL STRS_TXT50(LINE,LEN_LINE)

               IF (IRODDL == 0) THEN
                 WRITE(LINE,'(20X)')
                 LEN_LINE=LEN_TRIM(LINE)
                 CALL STRS_TXT50(LINE,LEN_LINE)
               ELSE
                 WRITE(LINE,'(20X,1P3E20.9)')(VR(J,I),J=1,3)
                 LEN_LINE=LEN_TRIM(LINE)
                 CALL STRS_TXT50(LINE,LEN_LINE)
               END IF

             END IF
           END IF
        END DO
      ELSE
        DO I=1,NUMNODG
          IF(NODTAGLOB(I) /= 0)THEN
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(2I10,1P3E20.9)') ITABG(I),0,(VG(J,I),J=1,3)
               IF (IRODDL == 0) THEN
                 WRITE(IUGEO,'(20X)')
               ELSE
                 WRITE(IUGEO,'(20X,1P3E20.9)') (VRG(J,I),J=1,3)
               END IF
             ELSE
               ! Write STATE File in GZIP format
               WRITE(LINE,'(2I10,1P3E20.9)')  ITABG(I),0,(VG(J,I),J=1,3)
               LEN_LINE=LEN_TRIM(LINE)
               CALL STRS_TXT50(LINE,LEN_LINE)

               IF (IRODDL == 0) THEN
                 WRITE(LINE,'(20X)')
                 LEN_LINE=LEN_TRIM(LINE)
                 CALL STRS_TXT50(LINE,LEN_LINE)
               ELSE
                 WRITE(LINE,'(20X,1P3E20.9)')          (VRG(J,I),J=1,3)
                 LEN_LINE=LEN_TRIM(LINE)
                 CALL STRS_TXT50(LINE,LEN_LINE)
               END IF

             END IF
          END IF
        END DO
      ENDIF
C-----------------------------------------------
      DEALLOCATE(NODTAGLOB)
      DEALLOCATE(VG)
      DEALLOCATE(VRG)
      RETURN
      END

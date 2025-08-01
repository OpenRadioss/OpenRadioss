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
!||    elbuf_ini       ../engine/source/elements/elbuf/elbuf_ini.F
!||--- called by ------------------------------------------------------
!||    rdresb          ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    allocbuf_auto   ../engine/source/elements/elbuf/allocbuf_auto.F
!||--- uses       -----------------------------------------------------
!||    mat_elem_mod    ../common_source/modules/mat_elem/mat_elem_mod.F90
!||====================================================================
      SUBROUTINE ELBUF_INI(MAT_ELEM ,NGROUP ,IPARG)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)            :: NGROUP
      INTEGER ,INTENT(INOUT)         :: IPARG(NPARG,NGROUP)
      TYPE(MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ERR,NG,BUFLEN,ITY,NPT,IXFEM,IXEL,ISNOD,NEWLEN,INLOC,ISOLNOD
      INTEGER IDAMP_FREQ_RANGE
C=======================================================================
c      Element Buffer Routine (Solid) 
c------
c      1) taille du tableau ELBUF pour restart
c      2) allocation de la structure ELBUF_STR
C=======================================================================
      ERR = 0
      ALLOCATE (MAT_ELEM%ELBUF(NGROUP), STAT=ERR)
      IF (ERR /= 0) THEN
      ENDIF 
c-------------------------------------------------
      DO NG = 1,NGROUP                                                 
        ITY    = IPARG(5,NG)
        NPT    = IPARG(6,NG)                                   
        ISOLNOD= IPARG(28,NG)
        BUFLEN = IPARG(66,NG)
        IXFEM  = IPARG(54,NG)
        INLOC  = IPARG(78,NG)
        IDAMP_FREQ_RANGE = IPARG(93,NG)

        NEWLEN = BUFLEN         
        IF(ITY==1)THEN
          ISNOD=IPARG(28,NG)
        ELSE
          ISNOD=0
        END IF
c          print*,'  NG =',ng
        IF (ITY==1 .or. ITY==2   .or. ITY==51 .or. ITY==3 .or.
     .      ITY==7 .or. ITY==101 .or. ITY==4  .or. ITY==5 .or.
     .      ITY==6 .or. ITY==100)THEN
          CALL ALLOCBUF_AUTO(MAT_ELEM%ELBUF(NG),BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC,ISOLNOD,
     .                       IDAMP_FREQ_RANGE)
          IPARG(66,NG)=NEWLEN
        END IF
      ENDDO   ! NG = 1,NGROUP
C-----------
      IF (ICRACK3D > 0) THEN
        ERR = 0
        ALLOCATE (MAT_ELEM%XFEM_TAB(NGROUP,NXEL), STAT=ERR)
c        IF (ERR /= 0) THEN
c        ENDIF
        DO IXEL=1,NXEL
          DO NG = 1,NGROUP
            ITY    = IPARG(5,NG)
            NPT    = IPARG(6,NG)
            ISOLNOD= IPARG(28,NG)
            BUFLEN = IPARG(66,NG)
            IXFEM  = IPARG(54,NG)
            INLOC  = IPARG(78,NG)
            IDAMP_FREQ_RANGE = IPARG(93,NG)
            NEWLEN = BUFLEN
c----
c      if xfem, count buffer's length only in the groups with xfem phantom elements  
c----
            IF (IXFEM > 0 .and. (ITY==3 .or. ITY==7)) THEN
              CALL ALLOCBUF_AUTO(MAT_ELEM%XFEM_TAB(NG,IXEL),BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC,
     .                           ISOLNOD,IDAMP_FREQ_RANGE)
              IPARG(66,NG)=NEWLEN
            ENDIF
          ENDDO
        ENDDO
      ELSE
        ALLOCATE (MAT_ELEM%XFEM_TAB(0,0), STAT=ERR)  
      ENDIF
C-----------
      RETURN
      END SUBROUTINE ELBUF_INI

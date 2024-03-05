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
Chd|====================================================================
Chd|  FILL_SURF_ELLIPSE             source/model/sets/fill_gr_surf_ellipse.F
Chd|-- called by -----------
Chd|        FILL_IGR                      source/model/sets/fill_igr.F  
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE FILL_SURF_ELLIPSE(SET,IGRSURF,IGRS,BUFSF,LISURF1,NSURF)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Merge SET%SURFACE into Radioss Surface 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure - Current SET
C     IGRSURF       SURFACES 
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SETDEF_MOD
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IGRS
      TYPE (SURF_)  , TARGET ,INTENT(INOUT):: IGRSURF(*)
      TYPE (SET_)   , INTENT(INOUT) :: SET
      INTEGER, INTENT(IN) :: LISURF1,NSURF
      MY_REAL, INTENT(INOUT) :: BUFSF(LISURF1*(NSURF+NSETS))
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSEG,J,IAD
      CHARACTER MESS*40
      DATA MESS/'SET SURF GROUP DEFINITION               '/
C-----------------------------------------------
!
!     create new  (IGRSURF, etc) from elems of /SET
!
      NSEG = SET%NB_ELLIPSE
!      IF (NSEG == 0) RETURN      ! create a Surface if empty
!---
      IGRS = IGRS + 1 ! increment NSURF = IGRS + 1
!
      IGRSURF(IGRS)%ID = SET%SET_ID
      IGRSURF(IGRS)%TITLE = SET%TITLE
      IGRSURF(IGRS)%NSEG = NSEG
!
      IGRSURF(IGRS)%TYPE = 0
      IGRSURF(IGRS)%ID_MADYMO = 0
      IGRSURF(IGRS)%IAD_BUFR = 0
      IGRSURF(IGRS)%NB_MADYMO = 0
      IGRSURF(IGRS)%TYPE_MADYMO = 0
      IGRSURF(IGRS)%LEVEL = 1
      IGRSURF(IGRS)%TH_SURF = 0
      IGRSURF(IGRS)%ISH4N3N = 0
      IGRSURF(IGRS)%NSEG_R2R_ALL = 0
      IGRSURF(IGRS)%NSEG_R2R_SHARE = 0
      IGRSURF(IGRS)%IAD_IGE = 0
      IGRSURF(IGRS)%NSEG_IGE = 0
!
!     not printout empty group
!
      IF (NSEG == 0) IGRSURF(IGRS)%SET_GROUP = 1
!
!
      IF (NSEG > 0) THEN
!
        CALL MY_ALLOC(IGRSURF(IGRS)%NODES,NSEG,4)
        IGRSURF(IGRS)%NODES(1:NSEG,1:4) = 0
        CALL MY_ALLOC(IGRSURF(IGRS)%ELTYP,NSEG)
        IGRSURF(IGRS)%ELTYP(1:NSEG) = 0
        CALL MY_ALLOC(IGRSURF(IGRS)%ELEM,NSEG)
        IGRSURF(IGRS)%ELEM(1:NSEG) = 0
!
        IGRSURF(IGRS)%TYPE = 101                                          
        IGRSURF(IGRS)%IAD_BUFR = SET%ELLIPSE_IAD_BUFR
        IAD=SET%ELLIPSE_IAD_BUFR
        IGRSURF(IGRS)%ID_MADYMO = SET%ELLIPSE_ID_MADYMO
        DO J=1,9
          BUFSF(IAD+7+J-1)=SET%ELLIPSE_SKEW(J)
        ENDDO

        BUFSF(IAD+1)=SET%ELLIPSE_A
        BUFSF(IAD+2)=SET%ELLIPSE_B
        BUFSF(IAD+3)=SET%ELLIPSE_C
        BUFSF(IAD+4)=SET%ELLIPSE_XC                                                  
        BUFSF(IAD+5)=SET%ELLIPSE_YC                                                  
        BUFSF(IAD+6)=SET%ELLIPSE_ZC                                                 
        !Init application point for force and momentum     
        !/* ellipsoides : defining center ! */      
        BUFSF(IAD+16)=SET%ELLIPSE_XC                                               
        BUFSF(IAD+17)=SET%ELLIPSE_YC                                                
        BUFSF(IAD+18)=SET%ELLIPSE_ZC 
                                                
        BUFSF(IAD+36)=SET%ELLIPSE_N

!
      ENDIF ! IF (NSEG > 0)

      SET%SET_NSURF_ID = IGRS
      SET%HAS_SURF_SEG = NSEG
C-----
      RETURN
      END
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
Chd|  SURFTAG                       source/groups/surftag.F       
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
      SUBROUTINE SURFTAG(NUMEL,IX,NIX,NIX1,NIX2,IELTYP,IPARTE,
     .                   TAGBUF,ISU,NSEG,FLAG,NINDX,
     .                   INDX,SURF_ELM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SURF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IELTYP,
     .      TAGBUF(*),IPARTE(*),NSEG,FLAG
      INTEGER :: NINDX
      INTEGER, DIMENSION(*) :: INDX
      TYPE(PART_TYPE), DIMENSION(*) :: SURF_ELM
!
      TYPE (SURF_) :: ISU
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       
!       ----------------
!       NINDX : number of tagged part 
!       INDX  : tagged part     
!       ----------------
!       SURF_ELM : PART_TYPE structur 
!                  %NSHELL or %NTRI : number of element per part
!                  %SHELL_PART or %TRI_PART : ID of the element 
!       ----------------
!       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,K,INV4(4),INV3(3)
      DATA INV4/4,3,2,1/
      DATA INV3/3,2,1/
      LOGICAL :: FILL_REVERSED
      INTEGER :: NUM_PART,NUM_ELM,ID_PART
      INTEGER :: JS_PART,JS_ELM
C=======================================================================
      FILL_REVERSED = .FALSE.
      IF (ALLOCATED(ISU%REVERSED)) THEN
         FILL_REVERSED = .TRUE.
      ENDIF

      NUM_PART = NINDX

      DO JS_PART=1,NUM_PART
       ID_PART = INDX(JS_PART)
       IF(IELTYP==3) NUM_ELM = SURF_ELM(ID_PART)%NSHELL
       IF(IELTYP==4) NUM_ELM = SURF_ELM(ID_PART)%NTRUSS    
       IF(IELTYP==5) NUM_ELM = SURF_ELM(ID_PART)%NBEAM
       IF(IELTYP==6) NUM_ELM = SURF_ELM(ID_PART)%NSPRING       
       IF(IELTYP==7) NUM_ELM = SURF_ELM(ID_PART)%NTRI
       DO JS_ELM=1,NUM_ELM
        IF(IELTYP==3) JJ = SURF_ELM(ID_PART)%SHELL_PART(JS_ELM)
        IF(IELTYP==4) JJ = SURF_ELM(ID_PART)%TRUSS_PART(JS_ELM)
        IF(IELTYP==5) JJ = SURF_ELM(ID_PART)%BEAM_PART(JS_ELM)
        IF(IELTYP==6) JJ = SURF_ELM(ID_PART)%SPRING_PART(JS_ELM)
        IF(IELTYP==7) JJ = SURF_ELM(ID_PART)%TRI_PART(JS_ELM)
!      DO JJ=1,NUMEL
!        IF (IABS(TAGBUF(IPARTE(JJ))) == 1)THEN
          NSEG=NSEG+1                           
          IF (FLAG == 1) THEN
            IF(TAGBUF(IPARTE(JJ)) == 1)THEN
               IF (FILL_REVERSED) ISU%REVERSED(NSEG) = 0
               DO K=NIX1,NIX2                      
                ISU%NODES(NSEG,K-1) = IX(K,JJ)
              ENDDO                               
            ELSEIF(TAGBUF(IPARTE(JJ)) == -1)THEN
               IF (FILL_REVERSED) ISU%REVERSED(NSEG) = 1
              IF (IELTYP == 3) THEN
                 DO K=NIX2,NIX1,-1
                   ISU%NODES(NSEG,INV4(K-1)) = IX(K,JJ)
                 ENDDO
               ELSEIF (IELTYP == 7) THEN
                 DO K=NIX2,NIX1,-1
                   ISU%NODES(NSEG,INV3(K-1)) = IX(K,JJ)
                 ENDDO
               ENDIF ! IF (IELTYP == 3)
            ENDIF                                 
            IF (NIX2-NIX1 == 2)THEN
              ISU%NODES(NSEG,4) = ISU%NODES(NSEG,3)
            ENDIF                                 
            ISU%ELTYP(NSEG) = IELTYP
            ISU%ELEM(NSEG)  = JJ
          ENDIF
!        ENDIF
       ENDDO    ! JS_ELM=1,NUM_ELM
      ENDDO     ! JS_PART=1,NUM_PART
!       ENDDO
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  SURFTAGADM                    source/groups/surftag.F       
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SURFTAGADM(NUMEL,IX,NIX,NIX1,NIX2,IELTYP,IPARTE,
     .                      TAGBUF,IGRSURF,NSEG,IPART,
     .                      KSHTREE,SHTREE,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IELTYP,
     1      TAGBUF(*),IPARTE(*),IADPART,NSEG,FLAG,
     2      IPART(LIPART1,*),KSHTREE,SHTREE(KSHTREE,*)
!
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,K,IP,NLEV,MY_LEV,INV4(4),INV3(3)
cmi+2
      DATA INV4/4,3,2,1/
      DATA INV3/3,2,1/
C-----------------------------------------------
C     only surfaces made of shells or 3-node shells (ieltyp=3 or 7)
      DO JJ=1,NUMEL
        IP=IPARTE(JJ)
        NLEV  =IPART(10,IP)
        MY_LEV=SHTREE(3,JJ)
        IF(MY_LEV < 0) MY_LEV=-(MY_LEV+1)
        IF(MY_LEV==NLEV)THEN
          IF(IABS(TAGBUF(IPARTE(JJ))) == 1)THEN
            NSEG=NSEG+1
            IF (FLAG == 1) THEN
              IF(TAGBUF(IPARTE(JJ)) == 1)THEN
                DO K=NIX1,NIX2
                  IGRSURF%NODES(NSEG,K-1) = IX(K,JJ)
                ENDDO
              ELSEIF(TAGBUF(IPARTE(JJ)) == -1)THEN
                DO K=NIX2,NIX1,-1
                 IF (IELTYP == 3) THEN
                  IGRSURF%NODES(NSEG,INV4(K-1)) = IX(K,JJ)
                 ELSEIF (IELTYP == 7) THEN
                  IGRSURF%NODES(NSEG,INV3(K-1)) = IX(K,JJ)
                 ENDIF ! IF (IELTYP == 3)
                ENDDO
              ENDIF
              IF(NIX2-NIX1 == 2)THEN
                IGRSURF%NODES(NSEG,4) = IGRSURF%NODES(NSEG,3)
              ENDIF
              IGRSURF%ELTYP(NSEG) = IELTYP
              IGRSURF%ELEM(NSEG)  = JJ
            ENDIF
          ENDIF
        ENDIF
      END DO
CC-----------
      RETURN
      END

Chd|====================================================================
Chd|  SURFTAGX                      source/groups/surftag.F       
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SURFTAGX(NUMEL,IXX,KXX,NIXX,IELTYP,IPARTE,
     .                    TAGBUF,IGRSLIN,NSEG,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,IXX(*),KXX(NIXX,*),IELTYP,NIXX,
     .      TAGBUF(*),IPARTE(*),IADPART,NSEG,FLAG
!
      TYPE (SURF_) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,K,NIX1,NIX2,ISEG
C=======================================================================
      DO JJ=1,NUMEL
        IF (IABS(TAGBUF(IPARTE(JJ))) == 1)THEN
            NSEG=NSEG+KXX(3,JJ) - 1                      
            ISEG = NSEG-(KXX(3,JJ) - 1)
          IF (FLAG == 1) THEN
            NIX1 = KXX(4,JJ)
            NIX2 = KXX(4,JJ) + KXX(3,JJ) - 1 
            IF(TAGBUF(IPARTE(JJ)) == 1)THEN       
              DO K=1,KXX(3,JJ) - 1  ! loop over Nstrand segments
                IGRSLIN%NODES(ISEG+K,1) = IXX(KXX(4,JJ)+K-1)
                IGRSLIN%NODES(ISEG+K,2) = IXX(KXX(4,JJ)+K)
                IGRSLIN%ELTYP(ISEG+K) = IELTYP
                IGRSLIN%ELEM(ISEG+K)  = JJ
              ENDDO                                            
c            ELSEIF(TAGBUF(IPARTE(JJ)) == -1)THEN  
c              DO K=NIX2,NIX1,-1                   
c                IBUFSSG(IAD)=IX(K,JJ)             
c                IAD=IAD+1                         
c              ENDDO                               
            ENDIF                            
          ENDIF
        ENDIF
      ENDDO
C-----------
      RETURN
      END


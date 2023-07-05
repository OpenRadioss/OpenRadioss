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
Chd|  SET_ELGROUP_PARAM             source/elements/shell/coque/set_elgroup_param.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUP_PARAM_MOD               ../common_source/modules/mat_elem/group_param_mod.F
Chd|====================================================================
      SUBROUTINE SET_ELGROUP_PARAM(
     .           GROUP_PARAM_TAB,IPARG    ,NGROUP   ,N2D      ,
     .           IPM            ,IGEO     ,PM       ,GEO      ,
     .           BUFMAT         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUP_PARAM_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP,N2D
      INTEGER ,DIMENSION(NPARG,NGROUP)   :: IPARG
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) :: IPM
      INTEGER ,DIMENSION(NPROPGI,NUMGEO) :: IGEO
      my_real ,DIMENSION(NPROPM,NUMMAT)  :: PM
      my_real ,DIMENSION(NPROPG,NUMGEO)  :: GEO
      my_real ,DIMENSION(SBUFMAT)        :: BUFMAT
      TYPE(GROUP_PARAM_) ,DIMENSION(NGROUP) :: GROUP_PARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,ITY,IGTYP,IDEF,IGMAT,IHBE,ISH3N,IEL,PID,IMAT,ILAW,IADD,
     .        NIP,ISMSTR,ICOAT
      my_real DM,TCOAT
C=======================================================================
      DO NG=1,NGROUP
        ITY   = IPARG(5,NG)
        IF (N2D == 0 .and. (ITY == 3 .or. ITY == 7)) THEN
          ILAW  = IPARG(1,NG)
          IMAT  = IPARG(18,NG)
          PID   = IPARG(62,NG)
          IEL   = IPARG(3,NG)+1
          IGTYP = IPARG(38,NG)
          DM    = GEO(16,PID)
          IDEF  = IGEO(31,PID)
          IGMAT = IGEO(98,PID)
c          IHBE  = IGEO(10,PID)   ! should be the same, GEO(171) is obsolete, check !!!
          IHBE  = NINT(GEO(171,PID))
          ISH3N = IGEO(18,PID)
c
c         Set default membrane shell viscosity parameter (DM)         
c
          IF (DM == ZERO) THEN
            IF (ILAW == 65 ) THEN
              DM = FIVEEM2
            ELSEIF (ILAW == 19 .or. ILAW == 119) THEN
              IF (ITY == 3 .and. IHBE == 23) THEN
                DM = ZEP015
              ELSE
                DM = FOURTH
              END IF
            ELSEIF (ILAW == 58 .or. ILAW == 158) THEN
              IF (IHBE == 11) THEN
                DM = ONE_FIFTH
              ELSE
                DM = THREE_OVER_4*EM01
              ENDIF
            ELSEIF (IHBE == 23 .AND. (IGTYP==1.OR.IGTYP==9)) THEN
              DM = ZEP015
            ELSEIF (IHBE == 23) THEN
              IF (ILAW==2.OR.ILAW==27.OR.ILAW==36.OR.ILAW==32.OR.ILAW==43) THEN
                DM = ZEP015
              ENDIF
            ENDIF
          ENDIF
c------   specific default dm for QEPH,QBAT
          IF (IHBE == 23 .AND. ILAW==25) THEN                                             
            IF (DM == ZERO) DM=EM01
          ENDIF
c
          IF (IGMAT > 0) THEN  ! .and. (ILAW /= 15 .AND. ILAW /= 27)) THEN
            IF (DM == ZERO) DM = FIVEEM2
          ELSEIF (ILAW == 1 .or. ILAW == 2 .or. ILAW == 22 ) THEN
            IF (IDEF == 1) DM = ZERO
          ELSEIF ( ILAW == 15 .or. ILAW == 27 .or. ILAW == 25) THEN
            IF (DM == ZERO) DM = FIVEEM2
          ENDIF !(IGMAT > 0
c          
          GROUP_PARAM_TAB(NG)%VISC_DM = DM  
c
c------   special settings for law119
c          
          IF (ILAW == 119) THEN
            IADD = MAX(1,IPM(7,IMAT))-1
            ICOAT = NINT(BUFMAT(IADD+14))
            TCOAT = BUFMAT(IADD+16)
            ISMSTR = 11   ! force small strain formulation for law 119 regardless of input
            IHBE   = 11   ! force full shell integration (Batoz) for law 119 regardless of input
            IF (ICOAT > ZERO .AND. TCOAT > ZERO) THEN 
              NIP = 3
              ! update shell thickness in the property to take coating into account
              GEO(1,PID) = GEO(1,PID) + TWO*TCOAT
            ELSE
              NIP = 1
            END IF
            IPARG(6,NG) = NIP
            IPARG(9,NG) = ISMSTR
            IPARG(23,NG)= IHBE
          END IF
           

        ELSE
c
          GROUP_PARAM_TAB(NG)%VISC_DM = ZERO  
c
        ENDIF
c
        GROUP_PARAM_TAB(NG)%VISC_DN = ZERO  
      
      ENDDO
c-----------
      RETURN
      END

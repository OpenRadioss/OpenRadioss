Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    set_elgroup_param   ../starter/source/elements/shell/coque/set_elgroup_param.F
!||--- called by ------------------------------------------------------
!||    lectur              ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
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
            SELECT CASE (ILAW)
              CASE (65)
                DM = FIVEEM2
              CASE (19, 119)
                IF (ITY == 3 .AND. IHBE == 23) THEN
                  DM = ZEP015
                ELSE
                  DM = FOURTH
                END IF
              CASE (58, 158)
                IF (IHBE == 11) THEN
                  DM = ONE_FIFTH
                ELSE
                  DM = THREE_OVER_4 * EM01
                END IF
              CASE (25, 123, 125, 127,132)
                IF (IHBE == 23) DM = EM01
              CASE (42, 69)
                IF (IHBE == 11) THEN
                  DM = ZEP04
                ELSE
                  DM = ZEP03
                END IF
              CASE DEFAULT
                IF (IHBE == 23 .AND. (IGTYP == 1 .or. IGTYP == 9)) DM = ZEP015
            END SELECT
          END IF
c------   specific composite IGMAT > 0
          IF (IGMAT > 0) THEN  ! .and. (ILAW /= 15 .AND. ILAW /= 27)) THEN
            IF (DM == ZERO) DM = FIVEEM2
          ELSEIF ( ILAW == 15 .or. ILAW == 27 .or. ILAW == 25  .or. ILAW == 123
     .                        .or. ILAW==125  .or. ILAW == 127 .or. ILAW == 132) THEN
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

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
Chd|  CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|        CCTOGLOB                      source/elements/shell/coqueba/cmatc3.F
Chd|        GEPM_LC                       source/elements/shell/coqueba/cmatc3.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE CMATC3(JFT    ,JLT     ,PM     ,MAT     ,GEO      ,
     1                  PID    ,AREA    ,THK0   ,THK02   ,THK      , 
     2                  THKE   ,VOLG    ,MTN    ,NPT     ,ITHK     ,
     3                  HM     ,HF      ,HC     ,HZ      ,IGTYP    ,
     4                  IORTH  ,HMOR    ,HFOR   ,DIR     ,IGEO     ,
     5                  IDRIL  ,IHBE    ,HMFOR  ,GS      ,ISUBSTACK,
     6                  STACK  ,ELBUF_STR,NLAY  ,DRAPE  ,NFT       ,
     .                  NEL   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
CC#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,MTN  , NPT,ITHK,IGTYP,IORTH,ISUBSTACK,NLAY,NFT
      INTEGER MAT(*), PID(*),IGEO(NPROPGI,*),IDRIL,IHBE
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), AREA(*),
     .   THK0(*),THK02(*),THK(*),THKE(*), DIR(*),
     .   VOLG(*),HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),HZ(*),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .   HMFOR(MVSIZ,6),GS(*)
      TYPE (STACK_PLY) :: STACK
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
      TYPE (DRAPE_) :: DRAPE
C-----------------------------------------------
c FUNCTION:   stiffness modulus matrix build
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   PM(NPROPM,MID)    - input Material data
c  I   MAT(NEL) ,MTN     - Material id :Mid and Material type id
c  I   GEO(NPROPG,PID)   - input geometrical property data
c  I   IGEO(NPROPGI,PID) - input geometrical property data (integer)
c  I   PID(NEL)          - Pid
c  I   IGTYP,IORTH       - Geo. property type
c  I   VOLG,AREA         - element volume,AREA (total)
c  O   THK0,THK02        - element thickness  and thickness^2
c  I   THK ,THKE         - element updated and initial thickness  
c  I   NPT,ITHK          - num. integrating point in thickness,updated thickness flag
c  I   DIR               - orthotropic directions
c  O   IORTH             - flag for orthopic material (full matrix)
c  O   HM(4,NEL)         - membrane stiffness modulus (plane stress)
c                          HM(1:D11,2:D22,3:D12,4:G);----
c  O   HF(4,NEL)         - bending stiffness modulus (plane stress) same than HM
c                        -HF=integration(t^2*HM) explicitly of thickness
c  O   HC(2,NEL)         - transverse shear modulus HC(1:G23,2:G13)
c  O   HZ(NEL)           -drilling dof modulus
c  I   IDRIL             - flag of using drilling dof
c  O   HMOR(2,NEL)       - suppermentary membrane modulus for orthotropic (D13,D23)
c  O   HFOR(2,NEL)       - suppermentary bending modulus for orthotropic (D13,D23)
c  O   HMFOR(6,NEL)      - suppermentary membrane-bending coupling modulus for orthotropic 
c                          (1:D11,2:D22,3:D12,4:G,5:D13,6:D23)
c  O   GS(NEL)           - out of plane shear isotropic shear modulus (for QEPH hourglass part)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IPID,J,J2,J3,JJ,MATLY(MVSIZ*100),NEL,L,IGMAT,IPGMAT
C     REAL
      my_real
     .   SHF(MVSIZ),NU(MVSIZ),G(MVSIZ),YM(MVSIZ),A11(MVSIZ),A12(MVSIZ),
     .   E11,E22,NU12,G31,G23,A22,WMC,FACG,COEF,WM
      my_real
     .   THKLY(MVSIZ*100),POSLY(MVSIZ,100),FAC(MVSIZ),THK_LY(JLT-JFT+1,NPT),
     .   HMLY(MVSIZ,4),HCLY(MVSIZ,2), HMORLY(MVSIZ,2),SFAC(MVSIZ)
C-----------------------------------------------
      COEF =EM01
      NEL = JLT-JFT+1
C      IF (IHBE.GE.21.AND.IHBE.LE.29) COEF =EM03 
      IF(ITHK.GT.0.AND.ISMDISP.EQ.0)THEN
        DO I=JFT,JLT
          THK0(I)=THK(I)
        ENDDO
       ELSE
        DO I=JFT,JLT
          THK0(I)=THKE(I)
        ENDDO
      ENDIF
       IGMAT = IGEO(98,PID(1))
       IPGMAT = 700
      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
        DO I=JFT,JLT
          THK02(I) = THK0(I)*THK0(I)
          VOLG(I) = THK0(I)*AREA(I)
          IPID=PID(I)
          MX = PID(I)
          YM(I)  = GEO(IPGMAT +2 ,MX) 
          NU(I)  = GEO(IPGMAT +3 ,MX)
          G(I)   = GEO(IPGMAT +4 ,MX) 
          A11(I) = GEO(IPGMAT +5 ,MX)
          A12(I) = GEO(IPGMAT +6 ,MX) 
        ENDDO
      ELSE
C
        MX  =MAT(JFT)
        DO I=JFT,JLT
          THK02(I) = THK0(I)*THK0(I)
          VOLG(I) = THK0(I)*AREA(I)
          IPID=PID(I)
          YM(I) =PM(20,MX)
          NU(I)  =PM(21,MX)
          G(I)  =PM(22,MX)
          A11(I) =PM(24,MX)
          A12(I) =PM(25,MX)
        ENDDO
        IF (MTN==78)THEN
          CALL GET_ETFAC_S(NEL,SFAC,MTN)
          DO I=JFT,JLT
            YM(I) =SFAC(I)*YM(I)
            G(I)  =SFAC(I)*G(I)
            A11(I)=SFAC(I)*A11(I)
            A12(I)=SFAC(I)*A12(I)
          ENDDO
        END IF
      END IF
      IF(NPT.EQ.1) THEN
        DO I=JFT,JLT
         SHF(I)=0.
        ENDDO
      ELSE
        DO I=JFT,JLT
          SHF(I)=GEO(38,PID(I))
        ENDDO
      ENDIF
      DO I=JFT,JLT
         GS(I)=G(I)*SHF(I)
      ENDDO
C----this will do only for QEPH and change also in CNCOEF!!!look at starter first      
      IF(MTN.GE.24)THEN
        DO I=JFT,JLT
          A12(I)  =NU(I)*A11(I)
        ENDDO
      ENDIF
      IF (MTN.EQ.19.OR.MTN.EQ.15.OR.MTN.EQ.25) THEN
       IORTH=1
      ELSE
       IORTH=0
      ENDIF
      IF (IORTH.EQ.1) THEN
        DO I=JFT,JLT
         HMFOR(I,1)=ZERO
         HMFOR(I,2)=ZERO
         HMFOR(I,3)=ZERO
         HMFOR(I,4)=ZERO
         HMFOR(I,5)=ZERO
         HMFOR(I,6)=ZERO
        ENDDO
       IF (MTN.EQ.19) THEN
        CALL GEPM_LC(JFT,JLT,MAT,PM,SHF,HM,HC)
        CALL CCTOGLOB(JFT,JLT,HM,HC,HMOR,DIR,NEL)
        DO I=JFT,JLT
         HF(I,1)=ONE_OVER_12*HM(I,1)
         HF(I,2)=ONE_OVER_12*HM(I,2)
         HF(I,3)=ONE_OVER_12*HM(I,3)
         HF(I,4)=ONE_OVER_12*HM(I,4)
         HFOR(I,1)=ONE_OVER_12*HMOR(I,1)
         HFOR(I,2)=ONE_OVER_12*HMOR(I,2)
         HZ(I)= MAX(HF(I,1),HF(I,2),HF(I,4))*KZ_TOL
        ENDDO
       ELSEIF (MTN.EQ.15.OR.MTN.EQ.25) THEN
        IF (IGTYP.EQ.9) THEN
         CALL GEPM_LC(JFT,JLT,MAT,PM,SHF,HM,HC)
         CALL CCTOGLOB(JFT,JLT,HM,HC,HMOR,DIR,NEL)
         DO I=JFT,JLT
          HF(I,1)=ONE_OVER_12*HM(I,1)
          HF(I,2)=ONE_OVER_12*HM(I,2)
          HF(I,3)=ONE_OVER_12*HM(I,3)
          HF(I,4)=ONE_OVER_12*HM(I,4)
          HFOR(I,1)=ONE_OVER_12*HMOR(I,1)
          HFOR(I,2)=ONE_OVER_12*HMOR(I,2)
          HZ(I)= MAX(HF(I,1),HF(I,2),HF(I,4))*KZ_TOL
         ENDDO
        ELSEIF(IGTYP == 10.OR.IGTYP == 11.OR.IGTYP == 17.OR.
     .         IGTYP==51 .OR. IGTYP == 52)THEN
C         INTEGRATION PAR COUCHES
          CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO    ,  
     .                MAT      ,PID      ,THKLY    ,MATLY    ,POSLY   ,  
     .                IGTYP    ,0        ,0        ,NLAY     ,NPT     ,  
     .                ISUBSTACK,STACK    ,DRAPE   ,NFT       ,THK     ,
     .                NEL      ,THK_LY   )                               
         DO I=JFT,JLT
           HM(I,1)=ZERO
           HM(I,2)=ZERO
           HM(I,3)=ZERO
           HM(I,4)=ZERO
           HC(I,1)=ZERO
           HC(I,2)=ZERO
           HF(I,1)=ZERO
           HF(I,2)=ZERO
           HF(I,3)=ZERO
           HF(I,4)=ZERO
           HMOR(I,1)=ZERO
           HMOR(I,2)=ZERO
           HFOR(I,1)=ZERO
           HFOR(I,2)=ZERO
         ENDDO
        IF(IGTYP.EQ.10)THEN
         DO J=1,NPT
          J2=1+(J-1)*JLT
          J3=1+(J-1)*JLT*2
          CALL GEPM_LC(JFT,JLT,MATLY(J2),PM,SHF,HMLY,HCLY)
          CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
          DO I=JFT,JLT
           JJ = J2 - 1 + I
           WMC=POSLY(I,J)*POSLY(I,J)*THKLY(JJ)
           HM(I,1)=HM(I,1)+THKLY(JJ)*HMLY(I,1)
           HM(I,2)=HM(I,2)+THKLY(JJ)*HMLY(I,2)
           HM(I,3)=HM(I,3)+THKLY(JJ)*HMLY(I,3)
           HM(I,4)=HM(I,4)+THKLY(JJ)*HMLY(I,4)
           HC(I,1)=HC(I,1)+THKLY(JJ)*HCLY(I,1)
           HC(I,2)=HC(I,2)+THKLY(JJ)*HCLY(I,2)
           HMOR(I,1)=HMOR(I,1)+THKLY(JJ)*HMORLY(I,1)
           HMOR(I,2)=HMOR(I,2)+THKLY(JJ)*HMORLY(I,2)
           HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
           HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
           HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
           HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
           HFOR(I,1)=HFOR(I,1)+WMC*HMORLY(I,1)
           HFOR(I,2)=HFOR(I,2)+WMC*HMORLY(I,2)
          ENDDO
         ENDDO 
        ELSE
         DO J=1,NPT
          J2=1+(J-1)*JLT
          J3=1+(J-1)*JLT*2
          CALL GEPM_LC(JFT,JLT,MATLY(J2),PM,SHF,HMLY,HCLY)
          CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
          DO I=JFT,JLT
           JJ = J2 - 1 + I
           WM = POSLY(I,J)*THKLY(JJ)
           WMC= POSLY(I,J)*WM
           HM(I,1)=HM(I,1)+THKLY(JJ)*HMLY(I,1)
           HM(I,2)=HM(I,2)+THKLY(JJ)*HMLY(I,2)
           HM(I,3)=HM(I,3)+THKLY(JJ)*HMLY(I,3)
           HM(I,4)=HM(I,4)+THKLY(JJ)*HMLY(I,4)
           HC(I,1)=HC(I,1)+THKLY(JJ)*HCLY(I,1)
           HC(I,2)=HC(I,2)+THKLY(JJ)*HCLY(I,2)
           HMOR(I,1)=HMOR(I,1)+THKLY(JJ)*HMORLY(I,1)
           HMOR(I,2)=HMOR(I,2)+THKLY(JJ)*HMORLY(I,2)
           HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
           HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
           HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
           HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
           HFOR(I,1)=HFOR(I,1)+WMC*HMORLY(I,1)
           HFOR(I,2)=HFOR(I,2)+WMC*HMORLY(I,2)
           HMFOR(I,1)=HMFOR(I,1)+WM*HMLY(I,1)
           HMFOR(I,2)=HMFOR(I,2)+WM*HMLY(I,2)
           HMFOR(I,3)=HMFOR(I,3)+WM*HMLY(I,3)
           HMFOR(I,4)=HMFOR(I,4)+WM*HMLY(I,4)
           HMFOR(I,5)=HMFOR(I,5)+WM*HMORLY(I,1)
           HMFOR(I,6)=HMFOR(I,6)+WM*HMORLY(I,2)
          ENDDO
         ENDDO 
        END IF !(IGTYP.EQ.10)
         DO I=JFT,JLT
          HZ(I)= MAX(HF(I,1),HF(I,2),HF(I,4))*KZ_TOL
         ENDDO
        ENDIF
       ENDIF
      ELSE
C-----by layer
       IF (MTN == 27) THEN
          CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO    ,  
     .                MAT      ,PID      ,THKLY    ,MATLY    ,POSLY   ,  
     .                IGTYP    ,0        ,0        ,NLAY     ,NPT     ,  
     .                ISUBSTACK,STACK    ,DRAPE   ,NFT       ,THK     ,                               
     .                JLT      ,THK_LY   )                               
         DO I=JFT,JLT
          HM(I,1)=A11(I)
          HM(I,2)=A11(I)
          HM(I,3)=A12(I)
          HM(I,4)=G(I)
          HF(I,1)=ZERO
          HF(I,2)=ZERO
          HF(I,3)=ZERO
          HF(I,4)=ZERO
          HC(I,1)=GS(I)
          HC(I,2)=GS(I)
         ENDDO
         DO J=1,NPT
          DO I=JFT,JLT
           J2=1+(J-1)*JLT
           JJ = J2 - 1 + I
           WM = POSLY(I,J)*THKLY(JJ)
           WMC= POSLY(I,J)*WM
           HF(I,1)=HF(I,1)+WMC*HM(I,1)
           HF(I,2)=HF(I,2)+WMC*HM(I,2)
           HF(I,3)=HF(I,3)+WMC*HM(I,3)
           HF(I,4)=HF(I,4)+WMC*HM(I,4)
          ENDDO
         END DO !J=1,NPT
         DO I=JFT,JLT
          HZ(I)= HF(I,1)*KZ_TOL
         ENDDO
       ELSE
C
        DO I=JFT,JLT
         HM(I,1)=A11(I)
         HM(I,2)=A11(I)
         HM(I,3)=A12(I)
         HM(I,4)=G(I)
         HF(I,1)=ONE_OVER_12*HM(I,1)
         HF(I,2)=ONE_OVER_12*HM(I,2)
         HF(I,3)=ONE_OVER_12*HM(I,3)
         HF(I,4)=ONE_OVER_12*HM(I,4)
         HC(I,1)=GS(I)
         HC(I,2)=GS(I)
         HZ(I)= HF(I,1)*KZ_TOL
        ENDDO
       END IF !(MTN == 27) THEN
      ENDIF
       IF (IDRIL.GT.0) THEN
          FACG = COEF*MIN(ONE,KZ_TOL*2000)
         DO I=JFT,JLT
C-------allows changing module by KZ_TOL----
          HZ(I)= G(I)*FACG
C          HZ(I)= HM(I,4)*FACG
         ENDDO
       END IF !(IDRIL.GT.0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  CCTOGLOB                      source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCTOGLOB(JFT,JLT,HM,HC,HMOR,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   DIR(NEL,2),HM(MVSIZ,4),HC(MVSIZ,2),HMOR(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .  M2(MVSIZ),N2(MVSIZ),MN(MVSIZ),M4(MVSIZ),N4(MVSIZ),
     .  MN2(MVSIZ),CM(6),CC(2),T1,T2,T3
C-----------------------------------------------
      DO I=JFT,JLT
       M2(I)= DIR(I,1)*DIR(I,1)
       N2(I)= DIR(I,2)*DIR(I,2)
       M4(I)= M2(I)*M2(I)
       N4(I)= N2(I)*N2(I)
       MN(I)= DIR(I,1)*DIR(I,2)
       MN2(I)= MN(I)*MN(I)
      ENDDO
      DO I=JFT,JLT
       T1 = TWO*MN2(I)*HM(I,3)+FOUR*MN2(I)*HM(I,4)
       CM(1)=M4(I)*HM(I,1)+N4(I)*HM(I,2)+T1
       CM(2)=N4(I)*HM(I,1)+M4(I)*HM(I,2)+T1
       T2 = MN2(I)*(HM(I,1)+HM(I,2))
       CM(3)=T2+(M4(I)+N4(I))*HM(I,3)-FOUR*MN2(I)*HM(I,4)
       CM(4)=T2-TWO*MN2(I)*(HM(I,3)+HM(I,4))+
     .        (M4(I)+N4(I))*HM(I,4)
       T3 = MN(I)*(HM(I,3)+TWO*HM(I,4))
       HMOR(I,1)=MN(I)*(M2(I)*HM(I,1)-N2(I)*HM(I,2))+
     .        T3*(N2(I)-M2(I))
       HMOR(I,2)=MN(I)*(N2(I)*HM(I,1)-M2(I)*HM(I,2))+
     .        T3*(M2(I)-N2(I))
       HM(I,1)=CM(1)
       HM(I,2)=CM(2)
       HM(I,3)=CM(3)
       HM(I,4)=CM(4)
      ENDDO
C
      DO I=JFT,JLT
       CM(1)= M2(I)*HC(I,1)+N2(I)*HC(I,2)
       CM(2)= N2(I)*HC(I,1)+M2(I)*HC(I,2)
       HC(I,1)=CM(1)
       HC(I,2)=CM(2)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GEPM_LC                       source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GEPM_LC(JFT,JLT,MAT,PM,SHF,HM,HC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,MAT(*)
C     REAL
      my_real
     .   HM(MVSIZ,4),HC(MVSIZ,2), PM(NPROPM,*), SHF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,MX
C     REAL
      my_real
     .   G,A11,A12,E11,E22,NU12,G31,G23,A22
C-----------------------------------------------
         MX  =MAT(JFT)
         NU12=(ONE-PM(35,MX)*PM(36,MX))
         E11  =PM(33,MX)
         E22  =PM(34,MX)
         A11  =E11/NU12
         A22  = E22/NU12
         A12  =PM(36,MX)*A11
         G    =PM(37,MX)
         G23  =PM(38,MX)
         G31  =PM(39,MX)
         DO I=JFT,JLT
          HM(I,1)=A11
          HM(I,2)=A22
          HM(I,3)=A12
          HM(I,4)=G
          HC(I,1)=G23*SHF(I)
          HC(I,2)=G31*SHF(I)
         ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  PUTSIGNORC3                   source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PUTSIGNORC3(JFT ,JLT ,IR,IS,IT,G_IMP ,SIGNOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT ,IR,IS,IT
C     REAL
      my_real
     .   G_IMP(*),SIGNOR(MVSIZ,5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K
C     REAL

      TYPE(L_KTBUFEP_)    , POINTER :: SKBUF
C-----------------------------------------------
       IF (IKT==0) RETURN
       SKBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IR,IS,IT)
        DO I=JFT ,JLT
         J=5*(I-1)
         SKBUF%A_KT(I)=G_IMP(I)
         SKBUF%SIGE(J+1)=SIGNOR(I,1)
         SKBUF%SIGE(J+2)=SIGNOR(I,2)
         SKBUF%SIGE(J+3)=SIGNOR(I,3)
         SKBUF%SIGE(J+4)=SIGNOR(I,4)
         SKBUF%SIGE(J+5)=SIGNOR(I,5)
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|-- calls ---------------
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE CMATIP3(JFT    ,JLT     ,PM     ,MAT     ,PID    ,
     1                   MTN    ,NPT     ,HM     ,HF      ,IORTH  ,
     2                   HMOR   ,HFOR    ,HMFOR  ,IPG     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "com20_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,MTN  , NPT,IORTH,IPG
      INTEGER MAT(*), PID(*)
C     REAL
      my_real
     .   PM(NPROPM,*),HM(MVSIZ,4),HF(MVSIZ,4),HMOR(MVSIZ,2),HFOR(MVSIZ,2),HMFOR(MVSIZ,6)
C-----------------------------------------------
c FUNCTION:   stiffness modulus matrix build per ipg (on the surface)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   PM(NPROPM,MID)    - input Material data
c  I   MAT(NEL) ,MTN     - Material id :Mid and Material type id
c  I   PID(NEL)          - Pid
c  I   NPT,              - num. integrating point in thickness
c  I   IPG               - PG on surface (for one-point integration shell should input 1)
c  O   IORTH             - flag for orthopic material (full matrix)
c  O   HM(4,NEL)         - membrane stiffness modulus (plane stress)
c                          HM(1:C11,2:C22,3:C12,4:G);----
c  O   HF(4,NEL)         - bending stiffness modulus (plane stress) same than HM
c                        -HF=integration(t^2*HM) explicitly of thickness
c  O   HMOR(2,NEL)       - suppermentary membrane modulus for orthotropic (D13,D23)
c  O   HFOR(2,NEL)       - suppermentary bending modulus for orthotropic (D13,D23)
c  O   HMFOR(6,NEL)      - suppermentary membrane-bending coupling modulus for orthotropic 
c                          (1:C11,2:C22,3:C12,4:G,5:C13,6:C23)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IPID,J,J2,J3,JJ,L,IR,IS,IT,IPLAS,IR4(4),IS4(4),NEL
C     REAL
      my_real
     .   NU,G,YM,A11,A12,BETA,
     .   THETA1,THETA2,HK(MVSIZ),HH(MVSIZ),DHK(MVSIZ),DHH(MVSIZ),
     .   GAMA(MVSIZ),C11(MVSIZ),C12(MVSIZ),C33(MVSIZ),SIG(MVSIZ,3),
     .   SIGM(MVSIZ,3),COEF,WM0,WF0,AA,B11,B12,D11,D12,DET,F_Y2,SPS,
     .   NORM,CEP11,CEP12,CEP13,CEP23,CEP22,CEP33,FAC(MVSIZ),
     .   NORM_1(MVSIZ)
      TYPE(L_KTBUFEP_)    , POINTER :: LBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
      DATA IR4/1,2,2,1/,IS4/1,1,2,2/
C----[C]=|A11 A12 0|  [C]^-1=|B11 B12 0 |  [P]=|2/3  -1/3  0 |
C--------|A12 A11 0|         |B12 B11 0 |      |-1/3  2/3  0 |
C--------| 0   0  G|         | 0   0 G_1|      | 0     0   2 |
C----[C]eff=[ C^-1+a*P]^-1 = |C11 C12 0  |
C---------                   |C12 C11 0  |
C-------                     | 0   0  C33|
C-----------------------------------------------
      IF (IKT==0 .OR.(MTN /= 2 .AND. MTN /= 36)) RETURN
C
      NEL = JLT-JFT+1
      CALL GET_ETFAC_S(NEL,FAC,MTN)
        IPLAS =0
        DO I=JFT,JLT
         IF(FAC(I) < ONE ) IPLAS =1
        ENDDO
c ---  make the 0-th iteration always elastic
       IF(IPLAS==0 .OR. ITER_NL==0 ) RETURN
C
       MX  =MAT(JFT)
       YM =PM(20,MX)
       NU  =PM(21,MX)
       G  =PM(22,MX)
       A11 =PM(24,MX)
       A12 =PM(25,MX)
       IF(MTN.GE.24)THEN
          A12  =NU*A11
       ENDIF
C---------- or IORTH=2 in this case to dispense HMFOR
       DO I=JFT,JLT
         HM(I,1)=ZERO
         HM(I,2)=ZERO
         HM(I,3)=ZERO
         HM(I,4)=ZERO
         HF(I,1)=ZERO
         HF(I,2)=ZERO
         HF(I,3)=ZERO
         HF(I,4)=ZERO
         HMOR(I,1)=ZERO
         HMOR(I,2)=ZERO
         HFOR(I,1)=ZERO
         HFOR(I,2)=ZERO
         HMFOR(I,1)=ZERO
         HMFOR(I,2)=ZERO
         HMFOR(I,3)=ZERO
         HMFOR(I,4)=ZERO
         HMFOR(I,5)=ZERO
         HMFOR(I,6)=ZERO
        ENDDO
C-----------------------------------------------
        MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
c        IR = IR4(IPG)
c        IS = IS4(IPG)
        IR = 1
        IS = IPG
C-------------tag plastified elements->using fac(I)----
C------------still elastic-----
       DO I=JFT,JLT
        IF (FAC(I)==ONE ) THEN
         HM(I,1)=A11
         HM(I,2)=A11
         HM(I,3)=A12
         HM(I,4)=G
         HF(I,1)=ONE_OVER_12*HM(I,1)
         HF(I,2)=ONE_OVER_12*HM(I,2)
         HF(I,3)=ONE_OVER_12*HM(I,3)
         HF(I,4)=ONE_OVER_12*HM(I,4)
        END IF !(FAC(I)==ONE ) THEN
       ENDDO
C-------------through thickness-----
       DO IT = 1 ,NPT
        LBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IR,IS,IT)
        IF (MTAG%L_A_KT>0) THEN
         DO I=JFT,JLT
c ...... SIG contains normalized deviatoric stresses ...
          GAMA(I)= LBUF%A_KT(I)
          IF (GAMA(I) > ZERO) THEN
           J=5*(I-1)
           SIG(I,1)= LBUF%SIGE(J+1)
           SIG(I,2)= LBUF%SIGE(J+2)
           SIG(I,3)= LBUF%SIGE(J+3)
           HK(I)= LBUF%SIGE(J+4)
           HH(I)= LBUF%SIGE(J+5)
           DHK(I)= TWO_THIRD*HK(I)
           DHH(I)= TWO_THIRD*HH(I)
          END IF
         ENDDO
        ELSE
         DO I=JFT,JLT
          GAMA(I)= ZERO
         ENDDO
        END IF !(MTAG%L_A_KT>0) THEN
C----calcul [C]^-1(<-B),[C]eff------------------
        DO I=JFT,JLT
         IF (GAMA(I) >ZERO) THEN
          B11=ONE/YM
          B12=-NU/YM
          AA = GAMA(I)/(ONE+GAMA(I)*DHK(I))
          D11= B11+TWO_THIRD*AA
          D12= B12-THIRD*AA
          DET=ONE/(D11*D11-D12*D12)
          C11(I)=DET*D11
          C12(I)=-DET*D12
          C33(I)=G/(ONE+G*AA*TWO)
C    calcul beta, {n}
          F_Y2= SIG(I,1)*(TWO*SIG(I,1)+SIG(I,2))+
     .          SIG(I,2)*(SIG(I,1)+TWO*SIG(I,2))+
     .          HALF*SIG(I,3)*SIG(I,3)
          SIGM(I,1)= C11(I)*SIG(I,1)+C12(I)*SIG(I,2)
          SIGM(I,2)= C12(I)*SIG(I,1)+C11(I)*SIG(I,2)
          SIGM(I,3)= C33(I)*SIG(I,3)
          SPS =SIG(I,1)*SIGM(I,1)+SIG(I,2)*SIGM(I,2)+SIG(I,3)*SIGM(I,3)
C----------------BETA
          THETA1=ONE+DHK(I)*GAMA(I)
          THETA2=ONE-DHH(I)*GAMA(I)
          COEF=F_Y2*THETA1/THETA2
          BETA=COEF*(DHH(I)*THETA1+DHK(I)*THETA2)+SPS
C----------factor norm----
          IF (ABS(BETA)<=EM20) THEN
           NORM_1(I)=ZERO
          ELSE
           NORM_1(I)= ONE/BETA
          END IF
         END IF
        ENDDO
c    update HM,HMOR,HF,HFOR
        WF0 = WF(IT,NPT)
        WM0 = Z0(IT,NPT)*WM(IT,NPT)
        DO I=JFT,JLT
         IF (GAMA(I) >ZERO) THEN
          DET=NORM_1(I)
          CEP11=C11(I)-SIGM(I,1)*SIGM(I,1)*DET
          CEP22=C11(I)-SIGM(I,2)*SIGM(I,2)*DET
          CEP12=C12(I)-SIGM(I,1)*SIGM(I,2)*DET
          CEP13=      -SIGM(I,1)*SIGM(I,3)*DET
          CEP23=      -SIGM(I,2)*SIGM(I,3)*DET
          CEP33=C33(I)-SIGM(I,3)*SIGM(I,3)*DET
          HM(I,1)=HM(I,1)+WF0*CEP11
          HM(I,2)=HM(I,2)+WF0*CEP22
          HM(I,3)=HM(I,3)+WF0*CEP12
          HM(I,4)=HM(I,4)+WF0*CEP33
          HMOR(I,1)=HMOR(I,1)+WF0*CEP13
          HMOR(I,2)=HMOR(I,2)+WF0*CEP23
          HF(I,1)=HF(I,1)+WM0*CEP11
          HF(I,2)=HF(I,2)+WM0*CEP22
          HF(I,3)=HF(I,3)+WM0*CEP12
          HF(I,4)=HF(I,4)+WM0*CEP33
          HFOR(I,1)=HFOR(I,1)+WM0*CEP13
          HFOR(I,2)=HFOR(I,2)+WM0*CEP23
          IF (IORTH == 0) IORTH = 1
         ELSEIF (FAC(I)/= ONE) THEN
          HM(I,1)=HM(I,1)+WF0*A11
          HM(I,2)=HM(I,2)+WF0*A11
          HM(I,3)=HM(I,3)+WF0*A12
          HM(I,4)=HM(I,4)+WF0*G
          HF(I,1)=HF(I,1)+WM0*A11
          HF(I,2)=HF(I,2)+WM0*A11
          HF(I,3)=HF(I,3)+WM0*A12
          HF(I,4)=HF(I,4)+WM0*G
         END IF
        ENDDO
C      
       END DO !IT = 1 ,NPT

      RETURN
      END
Chd|====================================================================
Chd|  CMATCH3                       source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE CMATCH3(JFT    ,JLT     ,PM     ,MAT     ,GEO   ,
     1                  PID     ,MTN     ,IDRIL  ,IGEO    ,HM    ,
     2                  HF      ,HZ      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,MTN
      INTEGER MAT(*), PID(*),IGEO(NPROPGI,*),IDRIL
C     REAL
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), HM(MVSIZ,4),HF(MVSIZ,4),HZ(*)
C-----------------------------------------------
c FUNCTION:   stiffness modulus matrix for hourglass part
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   PM(NPROPM,MID)    - input Material data
c  I   MAT(NEL) ,MTN     - Material id :Mid and Material type id
c  I   GEO(NPROPG,PID)   - input geometrical property data
c  I   IGEO(NPROPGI,PID) - input geometrical property data (integer)
c  I   PID(NEL)          - Pid
c  O   HM(NEL,4)         - membrane stiffness modulus (plane stress)
c                          HM(1:D11,2:D22,3:D12,4:G);----
c  O   HF(NEL,4)         - bending stiffness modulus (plane stress) same than HM
c                        -HF=integration(t^2*HM) explicitly of thickness
c  O   HZ(NEL)           -drilling dof modulus
c  I   IDRIL             - flag of using drilling dof
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IPID,J,J2,J3,JJ,NEL,L,IGMAT,IPGMAT,IGTYP
C     REAL
      my_real
     .   NU(MVSIZ),G(MVSIZ),YM(MVSIZ),A11(MVSIZ),A12(MVSIZ),
     .   E11,E22,NU12,G31,G23,A22,WMC,FACG,COEF,WM
      my_real
     .   FAC(MVSIZ)
C-----------------------------------------------
C       NEL = JLT-JFT+1
C      CALL GET_ETFAC_S(NEL,FAC,MTN)
C------elastic for the moment
       DO I=JFT,JLT
         FAC(I)  = ONE
       ENDDO
       IGTYP = IGEO(11,PID(1))
       IGMAT = IGEO(98,PID(1))
       IPGMAT = 700
      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
        DO I=JFT,JLT
          MX = PID(I)
          YM(I)  = GEO(IPGMAT +2 ,MX) 
          NU(I)  = GEO(IPGMAT +3 ,MX)
          G(I)   = GEO(IPGMAT +4 ,MX) 
          A11(I) = GEO(IPGMAT +5 ,MX)
          A12(I) = GEO(IPGMAT +6 ,MX) 
        ENDDO
      ELSE
C
        MX  =MAT(JFT)
        DO I=JFT,JLT
          YM(I) =PM(20,MX)
          NU(I)  =PM(21,MX)
          G(I)  =PM(22,MX)
          A11(I) =PM(24,MX)
          A12(I) =PM(25,MX)
        ENDDO
      END IF
C----this will do only for QEPH and change also in CNCOEF!!!look at starter first
      IF(MTN.GE.24)THEN
        DO I=JFT,JLT
          A12(I)  =NU(I)*A11(I)
        ENDDO
      ENDIF
C ---isotrope--only--
      DO I=JFT,JLT
         HM(I,1)=A11(I)*FAC(I)
         HM(I,2)=HM(I,1)
         HM(I,3)=A12(I)*FAC(I)
         HM(I,4)=G(I)*FAC(I)
         HF(I,1)=ONE_OVER_12*HM(I,1)
         HF(I,2)=ONE_OVER_12*HM(I,2)
         HF(I,3)=ONE_OVER_12*HM(I,3)
         HF(I,4)=ONE_OVER_12*HM(I,4)
C-----------elastic only------------
         HZ(I)= ONE_OVER_12*A11(I)*KZ_TOL
       ENDDO
C
       IF (IDRIL.GT.0) THEN
          COEF=EM01
          FACG = COEF*MIN(ONE,KZ_TOL*2000)
         DO I=JFT,JLT
C-------elastic only; allows changing module by KZ_TOL----
          HZ(I)= G(I)*FACG
         ENDDO
       END IF !(IDRIL.GT.0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  CDMSTIF                       source/elements/shell/coqueba/cmatc3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDMSTIF(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     1                   HM  ,HF    ,HC   ,HZ   ,HMOR ,
     2                   HFOR ,HMFOR,IPLAT,DM   ,DF   ,
     3                   GM   ,GF   ,DHZ  ,DMOR ,DFOR ,
     4                   DMF  ,IDRIL,IORTH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*),IDRIL,IORTH
      MY_REAL 
     .   VOL(*),THK0(*),THK2(*),
     .    HM(4,*),HF(4,*),HZ(*),HC(2,*),HMOR(2,*),HFOR(2,*),
     .    HMFOR(6,*),DM(2,2,*),DF(2,2,*),DHZ(*)   ,
     .    DMOR(2,*),DFOR(2,*),DMF(3,3,*),GM(*),GF(*)
C-----------------------------------------------
c FUNCTION:   stiffness modulus matrix after integration
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,M,EP
C     REAL
      my_real
     .   C1,C2,FAC
C-----------------------------------------------
#include "vectorize.inc"
C-----------------------
       DO M=JFT,JLT 
        EP=IPLAT(M)
        C2=VOL(EP)
        C1=THK2(EP)*C2
        DM(1,1,M)=HM(1,EP)*C2
        DM(2,2,M)=HM(2,EP)*C2
        DM(1,2,M)=HM(3,EP)*C2
        DM(2,1,M)=DM(1,2,M)
        GM(M) =HM(4,EP)*C2
        DF(1,1,M)=HF(1,EP)*C1
        DF(2,2,M)=HF(2,EP)*C1
        DF(1,2,M)=HF(3,EP)*C1
        DF(2,1,M)=DF(1,2,M)
        GF(M) =HF(4,EP)*C1
        DHZ(M)=HZ(EP)*C1
       ENDDO
C       
       IF (IDRIL>0) THEN
        DO M=JFT,JLT 
         EP=IPLAT(M)
         C2=VOL(EP)
         DHZ(M)=HZ(EP)*FOURTH*C2
        ENDDO
       END IF
C       
       IF (IORTH >0 ) THEN
        DO M=JFT,JLT 
         EP=IPLAT(M)
 	     C2=VOL(EP)
         C1=THK2(EP)*C2
         DMOR(1,M)=HMOR(1,EP)*C2
         DMOR(2,M)=HMOR(2,EP)*C2
         DFOR(1,M)=HFOR(1,EP)*C1
         DFOR(2,M)=HFOR(2,EP)*C1
        ENDDO
        DO M=JFT,JLT 
         EP=IPLAT(M)
 	     C2=VOL(EP)*THK0(EP)
         DMF(1,1,M)=HMFOR(1,EP)*C2
         DMF(2,2,M)=HMFOR(2,EP)*C2
         DMF(1,2,M)=HMFOR(3,EP)*C2
         DMF(1,3,M)=HMFOR(5,EP)*C2
         DMF(2,3,M)=HMFOR(6,EP)*C2
         DMF(2,1,M)=DMF(1,2,M)
         DMF(3,1,M)=DMF(1,3,M)
         DMF(3,2,M)=DMF(2,3,M)
         DMF(3,3,M)=HMFOR(4,EP)*C2
        ENDDO
       END IF !(IORTH >0 ) THEN
C
      RETURN
      END

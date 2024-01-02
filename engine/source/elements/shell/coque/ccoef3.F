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
Chd|  CCOEF3                        source/elements/shell/coque/ccoef3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCOEF3(
     1            JFT     ,JLT     ,PM     ,MAT      ,GEO     ,
     2            PID     ,OFF     ,AREA   ,STI      ,STIR    ,
     3            SHF     ,THK0    ,THK02  ,NU       ,
     4            G       ,YM      ,A11    ,A12      ,THK     ,
     5            SSP     ,RHO     ,H1     ,H2       ,H3      ,
     6            VOL0    ,VOL00   ,ALPE   ,GS       ,MTN     ,
     7            ITHK    ,ISMSTR  ,NPT    ,KFTS     ,
     8            SRH1    ,SRH2    ,SRH3   ,IGEO    ,
     9            A11R    ,ISUBSTACK, PM_STACK )
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
      INTEGER JFT, JLT, MTN,ITHK,
     .            ISMSTR,NPT ,KFTS ,ISUBSTACK
      INTEGER MAT(MVSIZ), PID(MVSIZ), IGEO(NPROPGI,*)
C     REAL
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), OFF(*), AREA(*),
     .   STI(*),STIR(*),SHF(*),THK0(*),THK02(*),THK(*),
     .   NU(*),G(*),YM(*),A11(*),A12(*),GS(*),
     .   H1(*),H2(*),H3(*),VOL0(*),SSP(*),RHO(*),ALPE(*),VOL00(*),
     .   SRH1(*)    ,SRH2(*)   ,SRH3(*),A11R(*),PM_STACK(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISH,MX,IPID,J,IGTYP,IPGMAT,IGMAT
C     REAL
      my_real
     .   FAC1(MVSIZ),FSH, FAC1TMP,
     .   RHO_1, YM_1, NU_1, G_1, A11_1, A12_1, SSP_1      
C=======================================================================
      IF(ITHK>0.AND.ISMSTR/=3)THEN
        DO I=JFT,JLT
          ALPE(I)=ONE
C --- A CORRIGER : VOL00(I) = THK0(I)*AREA0(I)
          VOL00(I) = THK0(I)*AREA(I)
          THK0(I)=THK(I)
          VOL0(I) = THK0(I)*AREA(I)
          THK02(I) = THK0(I)*THK0(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          ALPE(I)=ONE
C --- A CORRIGER : VOL00(I) = THK0(I)*AREA0(I)
          VOL00(I) = THK0(I)*AREA(I)
          VOL0(I) = VOL00(I)
          THK02(I) = THK0(I)*THK0(I)
        ENDDO
      ENDIF
C
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IPGMAT = 700

      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
        MX = MAT(JFT)
        DO I=JFT,JLT
          MX = PID(I)
          RHO(I) = GEO(IPGMAT +1 ,MX)   
          YM(I)  = GEO(IPGMAT +2 ,MX) 
          NU(I)  = GEO(IPGMAT +3 ,MX)
          G(I)   = GEO(IPGMAT +4 ,MX) 
          A11(I) = GEO(IPGMAT +5 ,MX)
          A12(I) = GEO(IPGMAT +6 ,MX)  
          A11R(I)= GEO(IPGMAT +7 ,MX)
          SSP(I) = GEO(IPGMAT +9 ,MX) 
        ENDDO
      ELSEIF(IGTYP == 52 .OR.
     .  ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
        DO I=JFT,JLT
          RHO(I) = PM_STACK(1 ,ISUBSTACK)   
          YM(I)  = PM_STACK(2 ,ISUBSTACK) 
          NU(I)  = PM_STACK(3 ,ISUBSTACK)
          G(I)   = PM_STACK(4 ,ISUBSTACK) 
          A11(I) = PM_STACK(5 ,ISUBSTACK)
          A12(I) = PM_STACK(6 ,ISUBSTACK)  
          A11R(I)= PM_STACK(7 ,ISUBSTACK)
          SSP(I) = PM_STACK(9 ,ISUBSTACK) 
        ENDDO
         
      ELSE
         MX = MAT(JFT)
         RHO_1=PM(1,MX)
         YM_1  =PM(20,MX)
         NU_1  =PM(21,MX)
         G_1   =PM(22,MX)
         A11_1 =PM(24,MX)
         A12_1 =PM(25,MX)
         SSP_1 =PM(27,MX)
         DO I=JFT,JLT
           RHO(I)=RHO_1
           YM(I)  =YM_1
           NU(I)  =NU_1
           G(I)   =G_1
           A11(I) =A11_1
           A12(I) =A12_1
           SSP(I) =SSP_1
         ENDDO
      ENDIF !igtyp 
C
      DO I=JFT,JLT
       IPID=PID(I)
       H1(I) =GEO(13,IPID)
       H2(I) =GEO(14,IPID)
       H3(I) =GEO(15,IPID)
       SRH1(I)=GEO(18,IPID)
       SRH2(I)=GEO(19,IPID)
       SRH3(I)=GEO(20,IPID)
      ENDDO
C    
      IF(NPT==1) THEN
        DO I=JFT,JLT
         SHF(I)=ZERO
        ENDDO
      ELSE
        DO I=JFT,JLT
C ds021  41i 26/9/00  +++
          FAC1TMP  =2.*(1.+NU(I))*THK02(I)
          ISH   =NINT(GEO(37,PID(I)))
          FSH   =GEO(38,PID(I))
          SHF(I)=FSH*(1. - ISH +
     .      ISH*FAC1TMP / (FSH*AREA(I)+FAC1TMP) )
          FAC1(I) = FAC1TMP
C ds021  41i 26/9/00  ---
        ENDDO
      ENDIF
      DO I=JFT,JLT
        GS(I)=G(I)*SHF(I)
      ENDDO
C
      RETURN
      END

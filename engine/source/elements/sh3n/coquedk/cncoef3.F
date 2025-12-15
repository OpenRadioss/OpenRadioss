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
!||    cncoef3b      ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||--- called by ------------------------------------------------------
!||    czforc3       ../engine/source/elements/shell/coquez/czforc3.F
!||    czforc3_crk   ../engine/source/elements/xfem/czforc3_crk.F
!||====================================================================
      SUBROUTINE CNCOEF3B(JFT    ,JLT     ,PM     ,MAT    ,GEO    ,
     2                    PID    ,AREA    ,SHF    ,THK0   ,
     3                    THK02  ,NU      ,G      ,YM     ,
     4                    A11    ,A12     ,THK    ,THKE   ,SSP    ,
     5                    RHO    ,VOLG    ,GS     ,MTN    ,ITHK   ,
     6                    NPT    ,DT1C    ,DT1    ,IHBE   ,AMU    ,
     7                    GSR    ,A11SR   ,A12SR  ,NUSR   ,SHFSR  ,
     8                    KRZ    ,IGEO    ,A11R  ,ISUBSTACK, PM_STACK,
     9                    UPARAM ,DIRA    ,DIRB   ,UVAR   ,FAC58  ,
     A                    NEL    ,ZOFFSET )
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
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,MTN,ITHK,NPT,IHBE,ISUBSTACK
      INTEGER , INTENT(IN) :: NEL
      INTEGER MAT(*), PID(*), IGEO(NPROPGI,*)
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), AREA(*),
     .   SHF(*),THK0(*),THK02(*),THK(*),THKE(*),
     .   NU(*),G(*),YM(*),A11(*),A12(*),AMU(*),
     .   VOLG(*),SSP(*),RHO(*),GS(*),DT1C(*),DT1,
     .   GSR(*), A11SR(*), A12SR(*), NUSR(*), SHFSR(*),KRZ(*),
     .   A11R(*),PM_STACK(20,*),UPARAM(*),
     .   DIRA(JLT,*),DIRB(JLT,*),UVAR(JLT,*),FAC58(MVSIZ,2)
       my_real , INTENT(OUT) , DIMENSION(NEL):: ZOFFSET 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IPID,IGTYP,IPGMAT,IGMAT,IPOS
      my_real FAC1TMP,KFAC,DN,K58(3),
     . RFAC,RFAT,R1,R2,S1,S2,T1,T2,T3,RS1,RS2,
     . R12,S12,R22,S22,E11,E22,K58I, Z0
C-----------------------------------------------
      IF(ITHK>0.AND.ISMDISP==0)THEN
        DO I=JFT,JLT
          THK0(I)=MAX(EM20,THK(I))
        ENDDO
       ELSE
        DO I=JFT,JLT
          THK0(I)=THKE(I)
        ENDDO
      ENDIF
C------explicit KFAC=1.0e-3 for quad 1.0e-2 for T3(could be 1.0e-2 for all)--
C------implicit KFAC=0.1 for all----
      IF(IMPL_S>0)THEN
       KFAC= EM01*MIN(ONE,KZ_TOL*2000)
      ELSE
       KFAC= EM03
      ENDIF
C
       IGTYP = IGEO(11,PID(1))
       IGMAT = IGEO(98,PID(1))
       IPGMAT = 700
      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
           DO I=JFT,JLT
               THK02(I) = THK0(I)*THK0(I)
               VOLG(I) = THK0(I)*AREA(I)
               DT1C(I) = DT1
               IPID=PID(I)
               MX = PID(I)
               RHO(I) = GEO(IPGMAT +1 ,MX) 
               YM(I)  = GEO(IPGMAT +2 ,MX) 
               NU(I)  = GEO(IPGMAT +3 ,MX)
               G(I)   = GEO(IPGMAT +4 ,MX) 
               A11(I) = GEO(IPGMAT +5 ,MX)
               A12(I) = GEO(IPGMAT +6 ,MX) 
               A11R(I)= GEO(IPGMAT +7 ,MX)
               SSP(I) = GEO(IPGMAT +9 ,MX)
               GSR(I)  =GEO(IPGMAT +10 ,MX)
               A11SR(I)=GEO(IPGMAT +11 ,MX)
               A12SR(I)=GEO(IPGMAT +12 ,MX)
               NUSR(I) =GEO(IPGMAT +13 ,MX)
               KRZ(I) =KFAC*G(I) 
!!               IZ(I) = GEO(198,PID(I)) ! ---> sum(ti*(ti/2 + zi**2) 
          ENDDO
      ELSEIF(IGTYP == 52 .OR. 
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0 )) THEN
           DO I=JFT,JLT
               THK02(I) = THK0(I)*THK0(I)
               VOLG(I) = THK0(I)*AREA(I)
               DT1C(I) = DT1
               IPID=PID(I)
               RHO(I) = PM_STACK(1 ,ISUBSTACK) 
               YM(I)  = PM_STACK(2 ,ISUBSTACK) 
               NU(I)  = PM_STACK(3 ,ISUBSTACK)
               G(I)   = PM_STACK(4 ,ISUBSTACK) 
               A11(I) = PM_STACK(5 ,ISUBSTACK)
               A12(I) = PM_STACK(6 ,ISUBSTACK) 
               A11R(I)= PM_STACK(7 ,ISUBSTACK)
               SSP(I) = PM_STACK(9 ,ISUBSTACK)
               GSR(I)  =PM_STACK(10 ,ISUBSTACK)
               A11SR(I)=PM_STACK(11 ,ISUBSTACK)
               A12SR(I)=PM_STACK(12 ,ISUBSTACK)
               NUSR(I) =PM_STACK(13 ,ISUBSTACK)
               KRZ(I) =KFAC*G(I)
          ENDDO 
      ELSEIF(MTN == 58 .or. MTN == 158) THEN
           MX  =MAT(JFT)
C---- due to too high young update (Starter) w/ input func            
           FAC1TMP = PM(23,MX)/PM(20,MX)
           K58(1) = UPARAM( 9)  ! young dir1
           K58(2) = UPARAM(10)  ! young dir2
           K58(3) = MAX(UPARAM(13),UPARAM(14))
           K58I = EM02
           IF (FAC1TMP <ONE) K58I = HALF*K58I     
           FAC58(JFT:JLT,1:2) = K58I
         IF(NPT==1) THEN
           DO I=JFT,JLT                                             
             R1 = DIRA(I,1)                                         
             S1 = DIRA(I,2)                                         
             R2 = DIRB(I,1)                                         
             S2 = DIRB(I,2)                                         
             RS1= R1*S1                                             
             RS2= R2*S2                                             
             R12= R1*R1                                             
             R22= R2*R2                                             
             S12= S1*S1                                             
             S22= S2*S2                                             
             T1 = K58(1)                                         
             T2 = K58(2)                                         
             T3 = K58(3)                                         
             E11 = R12*T1 + R22*T2
             E22 = S12*T1 + S22*T2
             YM(I)  = MAX(E11,E22)
             G(I)  = HALF*FAC1TMP*YM(I)
             NU(I)  = ZERO
             NUSR(I) =EM01
C---- for dt compute -> will be updated by cndt in case of dtnoda        
             A11(I) = YM(I)
             A12(I) = NU(I)*A11(I)
             RFAC = EXP(UVAR(I,4)) 
             RFAT = EXP(UVAR(I,5))
C----  FAC58(I,1:2) could be different values, but too complicated     
             IF (UVAR(I,11)/=ZERO.AND.UVAR(I,12)/=ZERO) THEN
               FAC58(I,1:2) = EM01*K58I
             ELSEIF (MIN(RFAC,RFAT)>ONE) THEN
               FAC58(I,1:2) = 1.2*K58I
           END IF
           ENDDO                                                    
         ELSE
           DO I=JFT,JLT                                             
             E11 = K58(1)                                         
             E22 = K58(2)                                         
             YM(I)  = MAX(E11,E22)
             G(I)  = HALF*YM(I)
             NU(I)  = ZERO
             NUSR(I) =EM01
C---- for dt compute       
             A11(I) = YM(I)
             A12(I) = NU(I)*A11(I)
           ENDDO                                                    
           END IF
           MX  =MAT(JFT)
           DO I=JFT,JLT
             THK02(I) = THK0(I)*THK0(I)
             VOLG(I) = THK0(I)*AREA(I)
             DT1C(I) = DT1
             RHO(I)=PM(1,MX)
             KRZ(I) =KFAC*G(I)
             GSR(I)  =SQRT(G(I))
C----- used in mem damping
             RFAC = MAX(FAC58(I,1),FAC58(I,2))             
             A11SR(I)=SQRT(RFAC*YM(I))
             A12SR(I)=NUSR(I)*A11SR(I)
           ENDDO 
            
      ELSE
          MX  =MAT(JFT)
          DO I=JFT,JLT
               THK02(I) = THK0(I)*THK0(I)
               VOLG(I) = THK0(I)*AREA(I)
               DT1C(I) = DT1
               RHO(I)=PM(1,MX)
               IPID=PID(I)
               YM(I) =PM(20,MX)
               NU(I)  =PM(21,MX)
               G(I)  =PM(22,MX)
               A11(I) =PM(24,MX)
               A12(I) =PM(25,MX)
               SSP(I) =PM(27,MX)
               GSR(I)  =PM(12,MX)
               A11SR(I)=PM(13,MX)
               A12SR(I)=PM(14,MX)
               NUSR(I) =PM(190,MX)
               KRZ(I) =KFAC*G(I)
          ENDDO
      ENDIF
      IF(NPT==1) THEN
        DO I=JFT,JLT
         SHF(I)=ZERO
         SHFSR(I)=ZERO
        ENDDO
      ELSE
        DO I=JFT,JLT
          SHF(I)=GEO(38,PID(I))
          SHFSR(I)=GEO(100,PID(I))
        ENDDO
      ENDIF
      DO I=JFT,JLT
         GS(I)=G(I)*SHF(I)
      ENDDO
      IF (MTN == 58  .or. MTN == 158) THEN
        CONTINUE
      ELSEIF(MTN>=24)THEN
        DO I=JFT,JLT
          A12(I)  =NU(I)*A11(I)
          A12SR(I)=NUSR(I)*A11SR(I)
        ENDDO
      ENDIF
c
c---  Coefficient Visco
c
      IF (IMPL_S == 1) THEN
        DN = ZERO
      ELSE
        DN = GEO(13,PID(1))
        IF(DN == ZERO) DN = ZEP01 + FIVEEM3 ! 0.015 default value
      ENDIF
      AMU(JFT:JLT) = DN
      Z0 = GEO(199,PID(1))
      ZOFFSET(JFT:JLT)  = ZERO
      SELECT CASE(IGTYP)
        CASE (1,9,10,11,16)
           DO I=JFT,JLT
            ZOFFSET(I) = Z0*THK0(I)
           ENDDO 
        CASE (17,51,52)
            IPOS   = IGEO(99,PID(1))
             IF(IPOS == 2) THEN
               DO I=JFT,JLT
                 ZOFFSET(I) = Z0 - HALF*THK0(I)
               ENDDO 
              ELSEIF (IPOS== 3 .OR. IPOS == 4) THEN
               DO I=JFT,JLT
                 Z0= HALF*THK0(I)
                 ZOFFSET (I) = Z0
               ENDDO  
             ENDIF    
        CASE DEFAULT 
            ZOFFSET(JFT:JLT)  = ZERO
      END SELECT
c-----------
      RETURN
      END

!||====================================================================
!||    cncoef3     ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||--- called by ------------------------------------------------------
!||    cbaforc3    ../engine/source/elements/shell/coqueba/cbaforc3.F
!||    cdk6forc3   ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!||    cdkforc3    ../engine/source/elements/sh3n/coquedk/cdkforc3.F
!||====================================================================
      SUBROUTINE CNCOEF3(JFT     ,JLT     ,PM      ,MAT     ,GEO     ,
     2                   PID     ,OFF     ,AREA    ,SHF     ,THK0    ,
     3                   THK02   ,NU      ,G       ,YM      ,
     4                   A11     ,A12     ,THK     ,THKE    ,SSP     ,
     5                   RHO     ,VOLG    ,GS      ,MTN     ,ITHK    ,
     6                   NPT     ,DT1C    ,DT1     ,IHBE    ,AMU     ,
     7                   KRZ     ,IGEO    ,A11R    ,ISUBSTACK,PM_STACK,
     8                   NEL     ,ZOFFSET)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,MTN,ITHK,NPT,IHBE,ISUBSTACK 
      INTEGER MAT(*), PID(*), IGEO(NPROPGI,*)
      INTEGER , INTENT(IN) :: NEL
C     REAL
      my_real GEO(NPROPG,*), PM(NPROPM,*), OFF(*), AREA(*),
     .   SHF(*),THK0(*),THK02(*),THK(*),THKE(*),
     .   NU(*),G(*),YM(*),A11(*),A12(*),AMU(*),
     .   VOLG(*),SSP(*),RHO(*),GS(*),DT1C(*),DT1,KRZ(*),
     .   A11R(*),PM_STACK(20,*)
      my_real,  DIMENSION(NEL) , INTENT(OUT) :: ZOFFSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IPID,IGTYP,IPGMAT,IGMAT,IPOS
C     REAL
      my_real KFAC,DN , Z0
C-----------------------------------------------
      IF(ITHK>0.AND.ISMDISP==0)THEN
        DO I=JFT,JLT
          THK0(I)=THK(I)
        ENDDO
       ELSE
        DO I=JFT,JLT
          THK0(I)=THKE(I)
        ENDDO
      ENDIF
C      
      IF(IMPL_S>0)THEN
       KFAC= EM01*MIN(ONE,KZ_TOL*2000)
      ELSE
       KFAC= EM03
      ENDIF
C
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IPGMAT = 700
      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
         DO I=JFT,JLT
             THK02(I) = THK0(I)*THK0(I)
             VOLG(I) = THK0(I)*AREA(I)
             DT1C(I) = DT1
             IPID=PID(I)
             RHO(I) = GEO(IPGMAT +1 ,IPID) 
             YM(I)  = GEO(IPGMAT +2 ,IPID) 
             NU(I)  = GEO(IPGMAT +3 ,IPID)
             G(I)   = GEO(IPGMAT +4 ,IPID) 
             A11(I) = GEO(IPGMAT +5 ,IPID)
             A12(I) = GEO(IPGMAT +6 ,IPID) 
             A11R(I)= GEO(IPGMAT +7 ,IPID)
             SSP(I) = GEO(IPGMAT +9 ,IPID)
             KRZ(I) =KFAC*G(I)
          ENDDO
      ELSEIF(IGTYP == 52 .OR. 
     .     ((IGTYP == 17 .OR. IGTYP == 51 ) .AND. IGMAT > 0)) THEN
         DO I=JFT,JLT
             THK02(I) = THK0(I)*THK0(I)
             VOLG(I) = THK0(I)*AREA(I)
             DT1C(I) = DT1
             RHO(I) = PM_STACK(1 ,ISUBSTACK) 
             YM(I)  = PM_STACK(2 ,ISUBSTACK) 
             NU(I)  = PM_STACK(3 ,ISUBSTACK)
             G(I)   = PM_STACK(4 ,ISUBSTACK) 
             A11(I) = PM_STACK(5 ,ISUBSTACK)
             A12(I) = PM_STACK(6 ,ISUBSTACK) 
             A11R(I)= PM_STACK(7 ,ISUBSTACK)
             SSP(I) = PM_STACK(9 ,ISUBSTACK)
             KRZ(I) =KFAC*G(I)
          ENDDO       
                  
      ELSE
           MX  =MAT(JFT)
           DO I=JFT,JLT
             THK02(I) = THK0(I)*THK0(I)
             VOLG(I) = THK0(I)*AREA(I)
             DT1C(I) = DT1
             RHO(I)=PM(1,MX)
             IPID=PID(I)
             YM(I) =PM(20,MX)
             NU(I)  =PM(21,MX)
             G(I)  =PM(22,MX)
             A11(I) =PM(24,MX)
             A12(I) =PM(25,MX)
             SSP(I) =PM(27,MX)
             KRZ(I) =KFAC*G(I)
           ENDDO
          
      ENDIF
      IF(NPT==1) THEN
        DO I=JFT,JLT
         SHF(I)=ZERO
        ENDDO
      ELSE
        DO I=JFT,JLT
          SHF(I)=GEO(38,PID(I))
        ENDDO
      ENDIF
      DO I=JFT,JLT
         GS(I)=G(I)*SHF(I)
      ENDDO
      IF(MTN>=24)THEN
        DO I=JFT,JLT
          A12(I)  =NU(I)*A11(I)
        ENDDO
      ENDIF
c
c---  Coefficient Visco  => DN should be defined in starter already
c
      IF (IMPL_S == 1) THEN
        DN = ZERO
      ELSE
        DN = GEO(13,PID(1))
        IF(DN == ZERO ) THEN
            IF (IHBE == 11)THEN
              DN = EM3
            ELSEIF(IHBE == 30)THEN
              DN = EM4
            ENDIF
        ENDIF    
      ENDIF
      AMU(JFT:JLT) = DN
      Z0 = GEO(199,PID(1))
      ZOFFSET(JFT:JLT)  = ZERO
      SELECT CASE(IGTYP)
        CASE (1,9,10,11,16)
           DO I=JFT,JLT
            ZOFFSET(I) = Z0*THK0(I)
           ENDDO 
        CASE (17,51,52)
            IPOS   = IGEO(99,PID(1))
             IF(IPOS == 2) THEN
               DO I=JFT,JLT
                 ZOFFSET(I) = Z0 - HALF*THK0(I)
               ENDDO 
              ELSEIF (IPOS== 3 .OR. IPOS == 4) THEN
               DO I=JFT,JLT
                 Z0= HALF*THK0(I)
                 ZOFFSET (I) = Z0
               ENDDO  
             ENDIF    
        CASE DEFAULT 
          ZOFFSET(JFT:JLT)  = ZERO
      END SELECT
c-----------
      RETURN
      END
!||====================================================================
!||    c3coefrz3     ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||--- called by ------------------------------------------------------
!||    c3forc3       ../engine/source/elements/sh3n/coque3n/c3forc3.F
!||    c3forc3_crk   ../engine/source/elements/xfem/c3forc3_crk.F
!||====================================================================
      SUBROUTINE C3COEFRZ3(JFT    ,JLT     ,G  ,KRZ   ,AREA   ,THK   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   G(*),KRZ(*),AREA(*),THK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real KFAC
C-----------------------------------------------
      IF(IMPL_S>0)THEN
        KFAC= EM01*MIN(ONE,KZ_TOL*2000)
      ELSE
        KFAC= EM02
      ENDIF
C
      DO I=JFT,JLT
        KRZ(I) =KFAC*G(I)
      ENDDO

      RETURN
      END
!||====================================================================
!||    cncoefort      ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||--- called by ------------------------------------------------------
!||    czforc3        ../engine/source/elements/shell/coquez/czforc3.F
!||    czforc3_crk    ../engine/source/elements/xfem/czforc3_crk.F
!||--- calls      -----------------------------------------------------
!||    cctoglob       ../engine/source/elements/shell/coqueba/cmatc3.F
!||    gepm_lc        ../engine/source/elements/shell/coqueba/cmatc3.F
!||    layini         ../engine/source/elements/shell/coque/layini.F
!||--- uses       -----------------------------------------------------
!||    drape_mod      ../engine/share/modules/drape_mod.F
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    mat_elem_mod   ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    stack_mod      ../engine/share/modules/stack_mod.F
!||====================================================================
      SUBROUTINE CNCOEFORT(JFT    ,JLT     ,PM        ,MAT     ,GEO    ,
     1                     PID    ,MTN     ,NPT       ,HM      ,HF     ,
     2                     HC     ,HMFOR   ,IORTH     ,DIR     ,IGEO   ,
     3                     ISUBSTACK,STACK ,ELBUF_STR ,NLAY    ,THK    ,
     4                     DRAPE  ,NFT     ,NEL       ,INDX_DRAPE, THKE,
     5                     SEDRAPE,   NUMEL_DRAPE     ,MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
      USE MAT_ELEM_MOD
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
      INTEGER JFT, JLT  ,MTN  , NPT,IORTH,NLAY,NEL,NFT
      INTEGER ,    INTENT(IN)     ::        SEDRAPE,NUMEL_DRAPE
      INTEGER MAT(*), PID(*) ,IGEO(NPROPGI,*)
      INTEGER, DIMENSION(SEDRAPE) :: INDX_DRAPE
C     REAL
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*), DIR(*),
     .   HM(MVSIZ,6),HF(MVSIZ,6),HC(MVSIZ,2),HMFOR(MVSIZ,6),THK(*)
      my_real, DIMENSION(NEL), INTENT(IN) :: THKE
      TYPE (STACK_PLY) :: STACK
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
      TYPE (DRAPE_) :: DRAPE(NUMEL_DRAPE)
      TYPE (MAT_ELEM_) ,INTENT(IN) :: MAT_ELEM
C-----------------------------------------------
c FUNCTION:   stiffness modulus matrix build For hourglass stress compute
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
c  I   NPT               - num. integrating point in thickness
c  I   DIR               - orthotropic directions
c  O   IORTH             - flag for orthopic material (full matrix)
c  O   HM(NEL,6)         - membrane stiffness modulus (plane stress)
c                          HM(1:D11,2:D22,3:D12,4:G 5:D13,6:D23);
c  O   HF(NEL,6)         - bending stiffness modulus (plane stress) same than HM
c                         -HF=integration(t^2*HM) explicitly of thickness
c  O   HC(NEL,2)         - transverse shear modulus HC(1:G23,2:G13)
c  O   HMFOR(NEL,6)      - suppermentary membrane-bending coupling modulus for orthotropic 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,J1,J2,J3,JJ,IGTYP,
     .        ISUBSTACK,IGMAT,IPOS,IPT_ALL,ILAY,IPT,IT,NPTT,
     .        LAYNPT_MAX, NLAY_MAX,ILAW_PLY
      INTEGER, DIMENSION(:)   , ALLOCATABLE :: MATLY   !! 
      my_real, DIMENSION(:)   , ALLOCATABLE :: THKLY   !! 
      my_real, DIMENSION(:,:) , ALLOCATABLE :: POSLY,THK_LY
      my_real
     . WMC,WM,A11,NU,A12,G
      my_real
     .   HMOR(MVSIZ,2),HMLY(MVSIZ,4),HCLY(MVSIZ,2), 
     .   HMORLY(MVSIZ,2),SHF(MVSIZ),IZZ(MVSIZ),IZ(MVSIZ)
C--------IORTH=2 -> HMFOR couplage non-null----------------
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IPOS  = IGEO(99,PID(1))
      IORTH = 0
      ! Npt_max
      LAYNPT_MAX = 1
      IF(IGTYP == 51 .OR. IGTYP == 52) THEN
        DO ILAY=1,NLAY
           LAYNPT_MAX = MAX(LAYNPT_MAX , ELBUF_STR%BUFLY(ILAY)%NPTT)
        ENDDO  
      ENDIF
      NLAY_MAX   = MAX(NLAY,NPT, ELBUF_STR%NLAY)
      ALLOCATE(MATLY(MVSIZ*NLAY_MAX), THKLY(MVSIZ*NLAY_MAX*LAYNPT_MAX),
     .         POSLY(MVSIZ,NLAY_MAX*LAYNPT_MAX),THK_LY(NEL,NLAY_MAX*LAYNPT_MAX))
      IF (IGTYP == 11 .OR. IGTYP == 17 ) THEN
           CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO    , 
     .                    MAT      ,PID      ,THKLY    ,MATLY    ,POSLY   , 
     .                    IGTYP    ,0        ,0        ,NLAY     ,NPT     , 
     .                    ISUBSTACK,STACK    ,DRAPE    ,NFT      ,THKE    ,
     .                    JLT      ,THK_LY   ,INDX_DRAPE, SEDRAPE,NUMEL_DRAPE)
           DO J=1,NPT
                J2=1+(J-1)*JLT
                 MX  = MATLY(J2)
                 ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                 IF(ILAW_PLY == 15. OR. ILAW_PLY == 25 .or. ILAW_PLY == 123 .or. 
     .              ILAW_PLY == 125 .or. ILAW_PLY == 127 .or. ILAW_PLY == 132 ) THEN
                     IORTH = 1
                     EXIT 
                 ENDIF
           ENDDO
      ELSEIF( IGTYP == 51 .OR. IGTYP == 52) THEN
          CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO    , 
     .                    MAT      ,PID      ,THKLY    ,MATLY    ,POSLY   , 
     .                    IGTYP    ,0        ,0        ,NLAY     ,NPT     , 
     .                    ISUBSTACK,STACK    ,DRAPE    ,NFT      ,THKE    ,
     .                    JLT      ,THK_LY   ,INDX_DRAPE, SEDRAPE,NUMEL_DRAPE)           
          DO ILAY=1,NLAY
                    J1 = 1+(ILAY-1)*JLT       ! JMLY
                    MX  = MATLY(J1)
                    ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                     IF( ILAW_PLY == 15.  or. ILAW_PLY == 25 .or. ILAW_PLY == 123 .or. 
     .                   ILAW_PLY == 125 .or. ILAW_PLY == 127.or. ILAW_PLY == 132) THEN
                      IORTH  = 1
                      EXIT
                    ENDIF
           ENDDO
      ELSEIF(MTN == 19 .OR. MTN == 15  .OR.  MTN == 25 .OR. MTN == 119 .OR. MTN == 123 .or. 
     .       MTN == 125 .OR. MTN == 127.OR. MTN == 132 ) THEN
        IORTH=1
      ELSE
        IORTH=0
      ENDIF
C----------unify the factor ONE_OVER_12 after       
      IF (IORTH == 1) THEN
        HMFOR(JFT:JLT,1:6)=ZERO
        IF (NPT == 1) THEN
          DO I=JFT,JLT
            SHF(I)=ZERO
          ENDDO
        ELSE
          DO I=JFT,JLT
            SHF(I)=GEO(38,PID(I))
          ENDDO
        ENDIF
        IF ((MTN == 19).OR.(MTN == 119)) THEN
          CALL GEPM_LC(JFT,JLT,MAT,PM,SHF,HMLY,HC)
          CALL CCTOGLOB(JFT,JLT,HMLY,HC,HMOR,DIR,NEL)
          DO I=JFT,JLT
            HM(I,1)=HMLY(I,1)
            HM(I,2)=HMLY(I,2)
            HM(I,3)=HMLY(I,3)
            HM(I,4)=HMLY(I,4)
            HM(I,5)=HMOR(I,1)
            HM(I,6)=HMOR(I,2)
            HF(I,1)=ONE_OVER_12*HMLY(I,1)
            HF(I,2)=ONE_OVER_12*HMLY(I,2)
            HF(I,3)=ONE_OVER_12*HMLY(I,3)
            HF(I,4)=ONE_OVER_12*HMLY(I,4)
            HF(I,5)=ONE_OVER_12*HMOR(I,1)
            HF(I,6)=ONE_OVER_12*HMOR(I,2)
          ENDDO
        ELSEIF ((MTN == 15 .OR. MTN == 25 .OR. MTN == 123 .or. 
     .           MTN == 125. OR. MTN == 127 .OR. MTN == 132) .AND. 
     .           IGTYP == 9 .OR. IGTYP == 10 ) THEN
          SELECT CASE (IGTYP)
            CASE(9)
              CALL GEPM_LC(JFT,JLT,MAT,PM,SHF,HM,HC)
              CALL CCTOGLOB(JFT,JLT,HM,HC,HMOR,DIR,NEL)
              DO I=JFT,JLT
                HM(I,5)=HMOR(I,1)
                HM(I,6)=HMOR(I,2)
                HF(I,1)=ONE_OVER_12*HM(I,1)
                HF(I,2)=ONE_OVER_12*HM(I,2)
                HF(I,3)=ONE_OVER_12*HM(I,3)
                HF(I,4)=ONE_OVER_12*HM(I,4)
                HF(I,5)=ONE_OVER_12*HMOR(I,1)
                HF(I,6)=ONE_OVER_12*HMOR(I,2)
              ENDDO
            CASE(10)
                CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO    , 
     .                    MAT      ,PID      ,THKLY    ,MATLY    ,POSLY   , 
     .                    IGTYP    ,0        ,0        ,NLAY     ,NPT     , 
     .                    ISUBSTACK,STACK    ,DRAPE    ,NFT      ,THKE    ,
     .                    JLT      ,THK_LY   ,INDX_DRAPE, SEDRAPE,NUMEL_DRAPE)                              
                HM(JFT:JLT,1:6)=ZERO
                HF(JFT:JLT,1:6)=ZERO
                HC(JFT:JLT,1:2)=ZERO
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
                    HM(I,5)=HM(I,5)+THKLY(JJ)*HMORLY(I,1)
                    HM(I,6)=HM(I,6)+THKLY(JJ)*HMORLY(I,2)
                    HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
                    HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
                    HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
                    HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
                    HF(I,5)=HF(I,5)+WMC*HMORLY(I,1)
                    HF(I,6)=HF(I,6)+WMC*HMORLY(I,2)
                  ENDDO
                ENDDO 
             END SELECT ! IGTYP = 9, 10, 16  
            ELSEIF(IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN                            
              HM(JFT:JLT,1:6)=ZERO
              HF(JFT:JLT,1:6)=ZERO
              HC(JFT:JLT,1:2)=ZERO
              IORTH=2
              IF ((IGTYP == 11 .OR. IGTYP == 17).AND. IGMAT > 0) THEN         
                  DO I=JFT,JLT
                    IZZ(I) = ZERO
                    IZ(I) = ZERO
                  ENDDO
C             
                  DO J=1,NPT
                    J2=1+(J-1)*JLT
                    J3=1+(J-1)*JLT*2
                    MX  = MATLY(J2)
                    ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                    IF(ILAW_PLY == 15 .OR. ILAW_PLY == 25 .OR. ILAW_PLY == 123 .or. 
     .                ILAW_PLY == 125 .or. ILAW_PLY == 127.OR. ILAW_PLY == 132) THEN
                       CALL GEPM_LC(JFT,JLT,MATLY(J2),PM,SHF,HMLY,HCLY)
                    ELSE
                        NU   =PM(21,MX)
                       !! E    =PM(21,MX)
                        G    =PM(22,MX)
                        A11  =PM(24,MX) ! E/(one - nu*nu)
                        A12  = NU*A11
                        DO I=JFT,JLT
                           HMLY(I,1)=A11
                           HMLY(I,2)=A11
                           HMLY(I,3)=A12
                           HMLY(I,4)=G
                           HCLY(I,1)=G*SHF(I)
                           HCLY(I,2)=G*SHF(I)
                        ENDDO
                    ENDIF   
                    CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
                    DO I=JFT,JLT
                      JJ = J2 - 1 + I
                      WM = POSLY(I,J)*THKLY(JJ)
                      WMC= POSLY(I,J)*WM + ONE_OVER_12*THKLY(JJ)**3
                      HM(I,1)=HM(I,1)+THKLY(JJ)*HMLY(I,1)
                      HM(I,2)=HM(I,2)+THKLY(JJ)*HMLY(I,2)
                      HM(I,3)=HM(I,3)+THKLY(JJ)*HMLY(I,3)
                      HM(I,4)=HM(I,4)+THKLY(JJ)*HMLY(I,4)
                      HC(I,1)=HC(I,1)+THKLY(JJ)*HCLY(I,1)
                      HC(I,2)=HC(I,2)+THKLY(JJ)*HCLY(I,2)
                      HM(I,5)=HM(I,5)+THKLY(JJ)*HMORLY(I,1)
                      HM(I,6)=HM(I,6)+THKLY(JJ)*HMORLY(I,2)                    
                      IZZ(I) = IZZ(I) + WMC 
                      IZ(I) = IZ(I) + WM 
C                
                      HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
                      HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
                      HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
                      HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
                      HF(I,5)=HF(I,5)+WMC*HMORLY(I,1)
                      HF(I,6)=HF(I,6)+WMC*HMORLY(I,2)
C-----------           
                      HMFOR(I,1)=HMFOR(I,1)+WM*HMLY(I,1)
                      HMFOR(I,2)=HMFOR(I,2)+WM*HMLY(I,2)
                      HMFOR(I,3)=HMFOR(I,3)+WM*HMLY(I,3)
                      HMFOR(I,4)=HMFOR(I,4)+WM*HMLY(I,4)
                      HMFOR(I,5)=HMFOR(I,5)+WM*HMORLY(I,1)
                      HMFOR(I,6)=HMFOR(I,6)+WM*HMORLY(I,2)
                    ENDDO
                  ENDDO 
C----------HM is calculated as mean value not need be modified when IPOS >0 (HF supposed the same)
                ELSEIF(IGTYP == 11 .OR. IGTYP == 17) THEN
C             
                  DO J=1,NPT
                    J2=1+(J-1)*JLT
                    J3=1+(J-1)*JLT*2
                    MX  = MATLY(J2)
                    ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                     IF(ILAW_PLY == 15 .OR. ILAW_PLY == 25 .OR. ILAW_PLY == 123 .or.
     .                  ILAW_PLY == 125 .or. ILAW_PLY == 127 .OR. ILAW_PLY == 132) THEN
                       CALL GEPM_LC(JFT,JLT,MATLY(J2),PM,SHF,HMLY,HCLY)
                    ELSE
                        NU   =PM(21,MX)
                       !! E    =PM(21,MX)
                        G    =PM(22,MX)
                        A11  =PM(24,MX) ! E/(one - nu*nu)
                        A12  = NU*A11
                        DO I=JFT,JLT
                           HMLY(I,1)=A11
                           HMLY(I,2)=A11
                           HMLY(I,3)=A12
                           HMLY(I,4)=G
                           HCLY(I,1)=G*SHF(I)
                           HCLY(I,2)=G*SHF(I)
                        ENDDO
                    ENDIF   
                    CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
                    DO I=JFT,JLT
                      JJ = J2 - 1 + I
                      WM = POSLY(I,J)*THKLY(JJ)
                      WMC= POSLY(I,J)*WM + ONE_OVER_12*THKLY(JJ)**3
                      HM(I,1)=HM(I,1)+THKLY(JJ)*HMLY(I,1)
                      HM(I,2)=HM(I,2)+THKLY(JJ)*HMLY(I,2)
                      HM(I,3)=HM(I,3)+THKLY(JJ)*HMLY(I,3)
                      HM(I,4)=HM(I,4)+THKLY(JJ)*HMLY(I,4)
                      HC(I,1)=HC(I,1)+THKLY(JJ)*HCLY(I,1)
                      HC(I,2)=HC(I,2)+THKLY(JJ)*HCLY(I,2)
                      HM(I,5)=HM(I,5)+THKLY(JJ)*HMORLY(I,1)
                      HM(I,6)=HM(I,6)+THKLY(JJ)*HMORLY(I,2)
C                
                      HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
                      HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
                      HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
                      HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
                      HF(I,5)=HF(I,5)+WMC*HMORLY(I,1)
                      HF(I,6)=HF(I,6)+WMC*HMORLY(I,2)
C-----------           
                      HMFOR(I,1)=HMFOR(I,1)+WM*HMLY(I,1)
                      HMFOR(I,2)=HMFOR(I,2)+WM*HMLY(I,2)
                      HMFOR(I,3)=HMFOR(I,3)+WM*HMLY(I,3)
                      HMFOR(I,4)=HMFOR(I,4)+WM*HMLY(I,4)
                      HMFOR(I,5)=HMFOR(I,5)+WM*HMORLY(I,1)
                      HMFOR(I,6)=HMFOR(I,6)+WM*HMORLY(I,2)
                    ENDDO
                  ENDDO             
C                        
              ELSEIF(IGTYP == 52 .OR. (IGTYP == 51 .AND. IGMAT > 0)) THEN
              
                IPT_ALL = 0
                DO I=JFT,JLT
                    IZZ(I) = ZERO
                    IZ(I) = ZERO
                ENDDO
                DO ILAY=1,NLAY
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT
                    J1 = 1+(ILAY-1)*JLT       ! JMLY
                    J2 = 1+(IPT-1)*JLT        ! THKLY
                    J3 = 1+(ILAY-1)*JLT*2     ! JDIR
                    J = IPT                   ! JPOS
                    MX  = MATLY(J1)
                    ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                   IF(ILAW_PLY == 15 .OR. ILAW_PLY == 25 .OR. ILAW_PLY == 123 .or. 
     .                ILAW_PLY == 125 .or. ILAW_PLY == 127 .OR. ILAW_PLY == 132  ) THEN
                      CALL GEPM_LC(JFT,JLT,MATLY(J1),PM,SHF,HMLY,HCLY)
                    ELSE
                        NU   =PM(21,MX)
                       !! E    =PM(21,MX)
                        G    =PM(22,MX)
                        A11  =PM(24,MX) ! E/(one - nu*nu)
                        A12  = NU*A11
                        DO I=JFT,JLT
                           HMLY(I,1)=A11
                           HMLY(I,2)=A11
                           HMLY(I,3)=A12
                           HMLY(I,4)=G
                           HCLY(I,1)=G*SHF(I)
                           HCLY(I,2)=G*SHF(I)
                        ENDDO
                    ENDIF   
                    CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
C
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
                      HM(I,5)=HM(I,5)+THKLY(JJ)*HMORLY(I,1)
                      HM(I,6)=HM(I,6)+THKLY(JJ)*HMORLY(I,2)
C                
                      HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
                      HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
                      HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
                      HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
                      HF(I,5)=HF(I,5)+WMC*HMORLY(I,1)
                      HF(I,6)=HF(I,6)+WMC*HMORLY(I,2)
C-----------           
                      HMFOR(I,1)=HMFOR(I,1)+WM*HMLY(I,1)
                      HMFOR(I,2)=HMFOR(I,2)+WM*HMLY(I,2)
                      HMFOR(I,3)=HMFOR(I,3)+WM*HMLY(I,3)
                      HMFOR(I,4)=HMFOR(I,4)+WM*HMLY(I,4)
                      HMFOR(I,5)=HMFOR(I,5)+WM*HMORLY(I,1)
                      HMFOR(I,6)=HMFOR(I,6)+WM*HMORLY(I,2)
                      IZZ(I) = IZZ(I) + WMC 
                      IZ(I) = IZ(I) + WM 
                    ENDDO
                  ENDDO  !  DO J=1,NPTT
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO  !  DO ILAY=1,NLAY
              ELSE ! IGTYP== 51
                IPT_ALL = 0
                DO ILAY=1,NLAY
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT
                    J1 = 1+(ILAY-1)*JLT       ! JMLY
                    J2 = 1+(IPT-1)*JLT        ! THKY
                    J3 = 1+(ILAY-1)*JLT*2     ! JDIR
                    J = IPT                   ! POS
                    MX  = MATLY(J1)
                    ILAW_PLY = MAT_ELEM%MAT_PARAM(MX)%ILAW 
                     IF(ILAW_PLY == 15 .OR. ILAW_PLY == 25 .OR. ILAW_PLY == 123 .or. 
     .                  ILAW_PLY == 125 .or. ILAW_PLY == 127 .OR. ILAW_PLY == 132 ) THEN
                      CALL GEPM_LC(JFT,JLT,MATLY(J1),PM,SHF,HMLY,HCLY)
                    ELSE
                        NU   =PM(21,MX)
                       !! E    =PM(21,MX)
                        G    =PM(22,MX)
                        A11  =PM(24,MX) ! E/(one - nu*nu)
                        A12  = NU*A11
                        DO I=JFT,JLT
                           HMLY(I,1)=A11
                           HMLY(I,2)=A11
                           HMLY(I,3)=A12
                           HMLY(I,4)=G
                           HCLY(I,1)=G*SHF(I)
                           HCLY(I,2)=G*SHF(I)
                        ENDDO
                    ENDIF   
                    CALL CCTOGLOB(JFT,JLT,HMLY,HCLY,HMORLY,DIR(J3),NEL)
C
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
                      HM(I,5)=HM(I,5)+THKLY(JJ)*HMORLY(I,1)
                      HM(I,6)=HM(I,6)+THKLY(JJ)*HMORLY(I,2)
C                
                      HF(I,1)=HF(I,1)+WMC*HMLY(I,1)
                      HF(I,2)=HF(I,2)+WMC*HMLY(I,2)
                      HF(I,3)=HF(I,3)+WMC*HMLY(I,3)
                      HF(I,4)=HF(I,4)+WMC*HMLY(I,4)
                      HF(I,5)=HF(I,5)+WMC*HMORLY(I,1)
                      HF(I,6)=HF(I,6)+WMC*HMORLY(I,2)
C-----------           
                      HMFOR(I,1)=HMFOR(I,1)+WM*HMLY(I,1)
                      HMFOR(I,2)=HMFOR(I,2)+WM*HMLY(I,2)
                      HMFOR(I,3)=HMFOR(I,3)+WM*HMLY(I,3)
                      HMFOR(I,4)=HMFOR(I,4)+WM*HMLY(I,4)
                      HMFOR(I,5)=HMFOR(I,5)+WM*HMORLY(I,1)
                      HMFOR(I,6)=HMFOR(I,6)+WM*HMORLY(I,2)
                    ENDDO
                  ENDDO  !  DO J=1,NPTT
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO  !  DO ILAY=1,NLAY
              ENDIF ! igmat + igtyp == 11
        ENDIF !IF (MTN==19)
      ENDIF    ! IF (IORTH==1) THEN
      DEALLOCATE(MATLY, THKLY, POSLY, THK_LY)
C
      RETURN
      END

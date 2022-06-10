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
Chd|  MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S6CKEL3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        GETTRANS                      source/elements/solid/solide8z/mmats.F
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        MSTIFORTH                     source/elements/solid/solide8z/mmats.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE MMATS(JFT     ,JLT     ,PM    ,MAT    ,HH    ,
     .                  MTN    ,IORTH   ,IPM   ,IGEO   ,GAMA  ,
     .                  UPARAM ,CC      ,CG    ,G33    ,JHBE  ,
     .                  SIG    ,EPS     ,NPPT  ,NEL    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,IORTH ,JHBE,MTN,NPPT,NEL
      INTEGER MAT(*),IPM(NPROPMI,*),IGEO(NPROPGI,*)
C     REAL
      my_real
     .   PM(NPROPM,*),HH(2,*),CC(3,3,*),UPARAM(*),
     .   GAMA(MVSIZ,6),CG(3,3,*),G33(3,3,*),SIG(NEL,6),EPS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IAD,J,K,IPLAST
C     REAL
      my_real
     .   NU,LAMDA,GG,C1,QC(9,MVSIZ),QCG(9,MVSIZ),QG(9,MVSIZ),
     .   QGC(9,MVSIZ),G3(3,MVSIZ),TT,TV,CA,CB,CN,G(MVSIZ),QH(MVSIZ),
     .   FAC(MVSIZ),SMIN,S1,NU12,NU21,EFAC
C-----------------------------------------------
      CALL GET_ETFAC_S(NEL,FAC,MTN)
      IF (IORTH.GT.0) THEN
       CALL GETTRANS(JFT,JLT,GAMA,QC,QCG,QGC,QG)
       IF (MTN.EQ.14.OR.MTN.EQ.12) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(1,1,I) =PM(40,MX)
         CC(2,2,I) =PM(43,MX)
         CC(3,3,I) =PM(45,MX)
         CC(1,2,I) =PM(41,MX)
         CC(2,3,I) =PM(44,MX)
         CC(1,3,I) =PM(42,MX)
         G3(1,I) =PM(46,MX)
         G3(2,I) =PM(47,MX)
         G3(3,I) =PM(48,MX)
        ENDDO
        CALL MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                 QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN.EQ.25) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         NU12 =PM(35,MX)
         NU21 =PM(36,MX)
         S1 = ONE-NU12*NU21
         CC(1,1,I) =PM(33,MX)/MAX(EM20,S1)
         CC(2,2,I) =PM(34,MX)/MAX(EM20,S1)
         CC(3,3,I) =PM(186,MX)
         CC(1,2,I) =HALF*(NU21*CC(1,1,I)+NU12*CC(2,2,I))
         CC(2,3,I) =ZERO
         CC(1,3,I) =ZERO
         G3(1,I) =PM(37,MX)
         G3(2,I) =PM(38,MX)
         G3(3,I) =PM(39,MX)
        ENDDO
        CALL MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                 QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN.EQ.28.OR.MTN.EQ.50.OR.MTN.EQ.68) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(1,1,I) = UPARAM(1)
         CC(2,2,I) = UPARAM(2)
         CC(3,3,I) = UPARAM(3)
         CC(1,2,I) = ZERO
         CC(2,3,I) = ZERO
         CC(1,3,I) = ZERO
         G3(1,I) = UPARAM(4)
         G3(2,I) = UPARAM(5)
         G3(3,I) = UPARAM(6)
        ENDDO
        CALL MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                 QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSEIF (MTN.EQ.53) THEN
        DO I=JFT,JLT
         MX  =MAT(I)
         CC(1,1,I) = UPARAM(1)
         CC(2,2,I) = UPARAM(2)
         CC(3,3,I) = CC(2,2,I)
         CC(1,2,I) = ZERO
         CC(2,3,I) = ZERO
         CC(1,3,I) = ZERO
         G3(1,I) = UPARAM(3)
         G3(2,I) = UPARAM(4)
         G3(3,I) = G3(1,I)
        ENDDO
        CALL MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                 QG     ,CC    ,G3  ,G33  ,CG     ) 
       ELSE
C----------isotrope effective       
        IORTH = 0
        DO I=JFT,JLT
         MX  =MAT(I)
         NU =PM(21,MX)
         C1 =THREE*PM(32,MX)/(ONE+NU)
         LAMDA=C1*NU
C-------GG:2G---HH(2,I):G-------
         GG =C1*(ONE-TWO*NU)
         HH(1,I)=LAMDA
         HH(2,I)=GG*HALF
        ENDDO
       ENDIF
      ELSE 
C-------iso------
C------hyperelastic------      
       IF (MTN.EQ.42) THEN
        EFAC=TWOP5
        DO I=JFT,JLT
         MX  =MAT(I)
         LAMDA=EFAC*FAC(I)*UPARAM(11)
C-------GG:2G---HH(2,I):G-------
         GG =EFAC*FAC(I)*PM(32,MX)
         HH(1,I)=LAMDA
         HH(2,I)=GG*HALF
        ENDDO
       ELSEIF (MTN.EQ.62.OR.MTN.EQ.62.OR.MTN.EQ.69.OR.MTN.EQ.82) THEN
        EFAC=TWOP5
        DO I=JFT,JLT
         MX  =MAT(I)
         NU =PM(21,MX)
         C1 =THREE*PM(32,MX)/(ONE+NU)
         LAMDA=C1*NU*FAC(I)
C-------GG:2G---HH(2,I):G-------
         GG =C1*(ONE-TWO*NU)*FAC(I)
         HH(1,I)=LAMDA
         HH(2,I)=GG*HALF
        ENDDO
       ELSE
        DO I=JFT,JLT
         MX  =MAT(I)
         NU =PM(21,MX)
         C1 =THREE*PM(32,MX)/(ONE+NU)
         LAMDA=C1*NU
C-------GG:2G---HH(2,I):G-------
         GG =C1*(ONE-TWO*NU)
         HH(1,I)=LAMDA
         HH(2,I)=GG*HALF
        ENDDO
       ENDIF
      ENDIF    !   IF (IORTH.GT.0) THEN
C-------------KT Condinuum---sig:mean value-------------
      IF (IKT.EQ.2) THEN
C-------------KT Condinuum---sig:per ip------------
      ELSEIF (IKT.EQ.3) THEN
       SELECT CASE (MTN)
       CASE(1)                                            
       CASE(2,36)                                   
        IPLAST =0
        DO I=JFT,JLT
         MX      =MAT(I)
candr ---- make the 0-th iteration always elastic
           IF(FAC(I).LT.ONE.AND.ITER_NL.GT.0) THEN
C          QH(I)= FAC(I)*TWO*HH(2,I)
          IPLAST =1
C         ELSE
C          QH(I)=ZERO
         ENDIF
        ENDDO
C        IF (IPLAST.GT.0) THEN
C          DO I=JFT,JLT
C           G33(1,1,I)=QH(I)
C          ENDDO
C        ENDIF 
        IF (IORTH.EQ.0.AND.IPLAST.EQ.1) IORTH=1
       CASE(10)                                        
       END SELECT      
C-------------KT Consistent--sig: per ip-------------
      ELSEIF (IKT.EQ.4) THEN
       S1=ONE*NPPT
       SELECT CASE (MTN)
       CASE(1)                                            
       CASE(2,36)                                   
        IPLAST =0
        DO I=JFT,JLT
candr ---  make the 0-th iteration always elastic
           IF(FAC(I).LT.ONE.AND.ITER_NL.GT.0) IPLAST =1
        ENDDO
        IF (IORTH.EQ.0.AND.IPLAST.EQ.1) IORTH=1
       CASE(10)                                        
       END SELECT      
      ENDIF !IF (IKT.EQ.2) THEN
 
C
      RETURN
      END
Chd|====================================================================
Chd|  GETTRANS                      source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETTRANS(JFT    ,JLT   ,GAMA ,QC  ,QCG  ,
     .                    QGC    ,QG   ) 
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
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   GAMA(MVSIZ,6),QC(3,3,*),QCG(3,3,*),QGC(3,3,*),QG(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   E1(MVSIZ,3),E2(MVSIZ,3),E3(MVSIZ,3)
C-----------------------------------------------
       DO J= 1,3
        DO I=JFT,JLT
          E1(I,J)=GAMA(I,J)
          E2(I,J)=GAMA(I,J+3)
        ENDDO
       ENDDO
       DO I=JFT,JLT
          E3(I,1)=E1(I,2)*E2(I,3)-E1(I,3)*E2(I,2)
          E3(I,2)=E1(I,3)*E2(I,1)-E1(I,1)*E2(I,3)
          E3(I,3)=E1(I,1)*E2(I,2)-E1(I,2)*E2(I,1)
       ENDDO
C	
       DO J= 1,3
        DO I=JFT,JLT
         QC(1,J,I)=E1(I,J)*E1(I,J)
         QC(2,J,I)=E2(I,J)*E2(I,J)
         QC(3,J,I)=E3(I,J)*E3(I,J)
C
         QGC(1,J,I)=E1(I,J)*E2(I,J)
         QGC(2,J,I)=E2(I,J)*E3(I,J)
         QGC(3,J,I)=E1(I,J)*E3(I,J)
        ENDDO
       ENDDO
       DO J= 1,3
        K = J+1
        IF (K.GT.3) K = 1
        DO I=JFT,JLT
         QG(1,J,I)=E1(I,J)*E2(I,K)+E2(I,J)*E1(I,K)
         QG(2,J,I)=E2(I,J)*E3(I,K)+E3(I,J)*E2(I,K)
         QG(3,J,I)=E3(I,J)*E1(I,K)+E1(I,J)*E3(I,K)
C
         QCG(1,J,I)=TWO*E1(I,J)*E1(I,K)
         QCG(2,J,I)=TWO*E2(I,J)*E2(I,K)
         QCG(3,J,I)=TWO*E3(I,J)*E3(I,K)
        ENDDO
       ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  MSTIFORTH                     source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE MSTIFORTH(JFT    ,JLT   ,QC  ,QCG  ,QGC    ,
     .                     QG     ,CC    ,G3  ,G33  ,CG     ) 
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
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   QC(3,3,*),QCG(3,3,*),QGC(3,3,*),QG(3,3,*),
     .   G3(3,*),CC(3,3,*),CG(3,3,*),G33(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ISYM
C     REAL
      my_real
     .   A(3,3,MVSIZ),B(3,3,MVSIZ)
C-----------------------------------------------
       ISYM = 0
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          A(K,J,I)=CC(J,K,I)
          B(K,J,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QC   ,A ,QCG,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QGC  ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= 1,3
        DO I=JFT,JLT
          CG(J,K,I)=HALF*A(J,K,I)+TWO*B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C---------------[G']->[G33]----
       ISYM = 1
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QCG  ,A ,QCG,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QG   ,B ,QG ,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          G33(J,K,I)=FOURTH*A(J,K,I)+B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C---------------[C']->[CC]----
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          A(J,K,I)=CC(J,K,I)
          B(J,K,I)=ZERO
        ENDDO
       ENDDO
       ENDDO
C      
       DO J= 1,3
        DO I=JFT,JLT
          B(J,J,I)=G3(J,I)
        ENDDO
       ENDDO
      CALL CBATRAN3(JFT   ,JLT    ,QC   ,A ,QC ,ISYM) 
      CALL CBATRAN3(JFT   ,JLT    ,QGC  ,B ,QGC,ISYM) 
C      
       DO J= 1,3
       DO K= J,3
        DO I=JFT,JLT
          CC(J,K,I)=A(J,K,I)+FOUR*B(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C       
       DO J= 1,3
       DO K= J+1,3
        DO I=JFT,JLT
          CC(K,J,I)=CC(J,K,I)
          G33(K,J,I)=G33(J,K,I)
        ENDDO
       ENDDO
       ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8ZVONM3                      source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZVONM3(LFT ,LLT ,DD  ,G   ,SIG  ,
     .                    DD1 ,GT  ,HT  ,NEL)
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
      INTEGER LFT,LLT,NEL
C     REAL
      my_real
     .   DD(3,3,*),G(*),SIG(NEL,6),DD1(3,3,*),GT(3,3,*),HT(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   SS(3,MVSIZ),ST(3,MVSIZ),P,SIGY49,DS(3,MVSIZ),
     .   SS2,DS2,ST2,NORM(MVSIZ),NORMI,G2
C----------  verifier-------------------------------------
      DO I=LFT,LLT
       G2 = TWO*G(I)
      DO J=1,3
       GT(J,J,I)=G2
      ENDDO
      ENDDO
C----------HT(i)>0 
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        SS(1,I)=SIG(I,1)+P
        SS(2,I)=SIG(I,2)+P
        SS(3,I)=SIG(I,3)+P
        ST(1,I)=SIG(I,4)
        ST(2,I)=SIG(I,5)
        ST(3,I)=SIG(I,6)
       ENDIF 
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        DS(1,I)=DD(1,1,I)*SS(1,I)+DD(1,2,I)*SS(2,I)+DD(1,3,I)*SS(3,I)
        DS(2,I)=DD(2,1,I)*SS(1,I)+DD(2,2,I)*SS(2,I)+DD(2,3,I)*SS(3,I)
        DS(3,I)=DD(3,1,I)*SS(1,I)+DD(3,2,I)*SS(2,I)+DD(3,3,I)*SS(3,I)
       ENDIF
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        SS2=SS(1,I)**2+SS(2,I)**2+SS(3,I)**2
        ST2=ST(1,I)**2+ST(2,I)**2+ST(3,I)**2
        SIGY49=TWO_THIRD*SS2+TWO_THIRD*ST2
c        SIGY49=TWO_THIRD*SS2+FOUR_OVER_3*ST2
        DS2=DS(1,I)*SS(1,I)+DS(2,I)*SS(2,I)+DS(3,I)*SS(3,I)
        NORM(I)=SQRT(DS2+ST2*G2+SIGY49*HT(I))
c        NORM(I)=SQRT(DS2+ST2*G(I)+SIGY49*HT(I))
       ENDIF
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        NORMI = SQRT(HALF)/MAX(EM20,NORM(I))
        DO J=1,3
         DS(J,I)=DS(J,I)*NORMI
         ST(J,I)=ST(J,I)*G(I)*NORMI
        ENDDO
       ENDIF 
      ENDDO
C-----------GT->u-------- 
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        DO J=1,3
        DO K=J,3
         DD(J,K,I)=DD(J,K,I)-DS(J,I)*DS(K,I)
         GT(J,K,I)=(GT(J,K,I)-ST(J,I)*ST(K,I))*HALF
         DD(K,J,I)=DD(J,K,I)
         GT(K,J,I)=GT(J,K,I)
        ENDDO
        ENDDO
        DO J=1,3
        DO K=1,3
         DD1(J,K,I)=-DS(J,I)*ST(K,I)*HALF
        ENDDO
        ENDDO
        ENDIF 
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  PUT_ETFAC                     source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|-- calls ---------------
Chd|        IKTMAT_INI                    source/implicit/imp_init.F    
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PUT_ETFAC(NEL     ,ET    ,MTN)
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MTN
C     REAL
      my_real
     .   ET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,MPT,NPTS,IKTMAT
C     REAL
      my_real
     .   ETF
C-----------------------------------------------
      CALL IKTMAT_INI(MTN,IKTMAT)
      IF (IMCONV < 0.OR.IKTMAT==0) RETURN
       IF (MTN==2.OR.MTN==36) THEN
        DO I = 1,NEL
         ETF=KTBUF_STR(NG_IMP)%ETFAC(I)
         KTBUF_STR(NG_IMP)%ETFAC(I) = MIN(ETF,ET(I))
        ENDDO
       ELSE
        DO I = 1,NEL
         ETF=KTBUF_STR(NG_IMP)%ETFAC(I)
         KTBUF_STR(NG_IMP)%ETFAC(I) = MAX(ETF,ET(I))
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        IKTMAT_INI                    source/implicit/imp_init.F    
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GET_ETFAC_S(NEL,SFAC,MTN)
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
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MTN
C     REAL
      my_real
     .   SFAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J,IKTMAT
C     REAL
C-----------------------------------------------
       CALL IKTMAT_INI(MTN,IKTMAT)
        DO I = 1,NEL
          SFAC(I)= ONE
        ENDDO
       IF (IKTMAT==0.OR.INCONV < 0) RETURN
C
        DO I = 1,NEL
         SFAC(I)=MAX(EM5,KTBUF_STR(NG_IMP)%ETFAC(I))
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  NUL_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IKTMAT_INI                    source/implicit/imp_init.F    
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE NUL_ETFAC_S(NEL,MTN)
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
#include "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MTN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J,IKTMAT
C     REAL
C-----------------------------------------------
       CALL IKTMAT_INI(MTN,IKTMAT)
       IF (IKTMAT > 0.AND.IMCONV>=0) THEN
        DO I = 1,NEL
          KTBUF_STR(NG_IMP)%ETFAC(I)=ONE
        ENDDO
       ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  SKTVONM3                      source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SKTVONM3(LFT ,LLT ,DD  ,HH   ,SIG  ,
     .                    DD1 ,GT  ,HT  ,NEL  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NEL
C     REAL
      my_real
     .   DD(3,3,*),HH(2,*),SIG(NEL,6),DD1(3,3,*),GT(3,3,*),HT(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   SS(3,MVSIZ),ST(3,MVSIZ),P,SIGY2,G(MVSIZ),
     .   SS2,DS2,ST2,NORM(MVSIZ),NORMI,G2,TT,TV
      my_real    
     .   EW(3,MVSIZ),TOL,LAMDA(MVSIZ),EV(3,3),KK(3,3)
C
      TOL=EM5
C----------  verifier-------------------------------------
      DO I=LFT,LLT
         G(I) = HH(2,I)
         G2 = TWO*G(I)
         TV=HH(1,I)
         TT=TV+G2
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,3,I)=TV
         DD(2,1,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
         DO J=1,3
          GT(J,J,I)=G2
         ENDDO
      ENDDO
C----------HT(i)>0 
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        SS(1,I)=SIG(I,1)+P
        SS(2,I)=SIG(I,2)+P
        SS(3,I)=SIG(I,3)+P
        ST(1,I)=SIG(I,4)
        ST(2,I)=SIG(I,5)
        ST(3,I)=SIG(I,6)
       ENDIF 
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        SS2=(SS(1,I)**2+SS(2,I)**2+SS(3,I)**2)*HALF
        ST2=ST(1,I)**2+ST(2,I)**2+ST(3,I)**2
        SIGY2=MAX(EM20,THREE*(SS2+ST2))
        DS2=THREE*G(I)+HT(I)
        NORM(I)=THREE*G(I)/SQRT(DS2*SIGY2)
       ENDIF
      ENDDO
C      
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        DO J=1,3
         SS(J,I)=SS(J,I)*NORM(I)
         ST(J,I)=ST(J,I)*NORM(I)
        ENDDO
       ENDIF 
      ENDDO
C-----------GT->u-------- 
      DO I=LFT,LLT
       IF (HT(I).GT.ZERO) THEN
        DO J=1,3
        DO K=J,3
         DD(J,K,I)=DD(J,K,I)-SS(J,I)*SS(K,I)
         GT(J,K,I)=GT(J,K,I)*HALF-ST(J,I)*ST(K,I)
         DD(K,J,I)=DD(J,K,I)
         GT(K,J,I)=GT(J,K,I)
        ENDDO
        ENDDO
        DO J=1,3
        DO K=1,3
         DD1(J,K,I)=-SS(J,I)*ST(K,I)
        ENDDO
        ENDDO
       ENDIF 
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        SKTVONM4                      source/elements/solid/solide8z/mmats.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SKTCONS2(JFT ,JLT ,CC  ,LAMDA ,G   ,CG  ,G33 ,IR,IS,IT)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,IR,IS,IT
C     REAL
      my_real
     .   LAMDA(*),G(*),CC(3,3,*),CG(3,3,*),G33(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K,IPLAST,NEL
C     REAL
      my_real
     .   NU,SIG(6,MVSIZ),ALPHA(MVSIZ),G2,TT,TV
      TYPE(L_KTBUFEP_)    , POINTER :: LBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
C-----------------------------------------------
        MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
        LBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IR,IS,IT)
        IF (MTAG%L_A_KT>0) THEN
         DO I=JFT,JLT
c ...... SIG contains normalized deviatoric stresses ...
          J=6*(I-1)
          SIG(1,I)= LBUF%SIGE(J+1)
          SIG(2,I)= LBUF%SIGE(J+2)
          SIG(3,I)= LBUF%SIGE(J+3)
          SIG(4,I)= LBUF%SIGE(J+4)
          SIG(5,I)= LBUF%SIGE(J+5)
          SIG(6,I)= LBUF%SIGE(J+6)
c .....  ALPHA(I) is the alpha constant of the radial return..
          ALPHA(I)= LBUF%A_KT(I)
         ENDDO
        ELSE
         DO I=JFT,JLT
          ALPHA(I)= ONE
         ENDDO
        END IF !(MTAG%L_A_KT>0) THEN
        DO I=JFT,JLT
         TV=LAMDA(I)+TWO_THIRD*G(I)*(ONE-ALPHA(I))
         TT=TV+TWO*G(I)*ALPHA(I)
         CC(1,1,I)=TT
         CC(2,2,I)=TT
         CC(3,3,I)=TT
         CC(1,2,I)=TV
         CC(1,3,I)=TV
         CC(2,3,I)=TV
         CC(2,1,I)=TV
         CC(3,1,I)=TV
         CC(3,2,I)=TV
        ENDDO
         DO J = 1, 3 
         DO K = 1, 3 
          DO I=JFT,JLT
           CG(J,K,I)=ZERO
           G33(J,K,I)=ZERO
          ENDDO
         ENDDO
         ENDDO
        DO I=JFT,JLT
         G2 = G(I)*ALPHA(I)
         G33(1,1,I)=G2
         G33(2,2,I)=G2
         G33(3,3,I)=G2
        ENDDO
        CALL SKTVONM4(LFT ,LLT ,CC  ,SIG  ,CG  ,
     .                G33 ,ALPHA  )
C
      RETURN
      END
Chd|====================================================================
Chd|  SKTVONM4                      source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SKTVONM4(LFT ,LLT ,DD  ,SIG  ,DD1 ,
     .                    GT  ,R   )
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
      INTEGER LFT,LLT
C     REAL
      my_real
     .   DD(3,3,*),SIG(6,*),DD1(3,3,*),GT(3,3,*),R(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C---------deviatoric normalized stress: sig
      INTEGER I,J,K
C     REAL
      my_real
     .   SS(3,MVSIZ),ST(3,MVSIZ)
C----------  verifier-------------------------------------
C----------HT(i)>0 
      DO I=LFT,LLT
c .....(R(I) is the alpha parameter of the radial return .. 
       IF (R(I).NE.ONE) THEN
c ..... SS(), ST() contain deviatoric stresses ..
        SS(1,I)=SIG(1,I)
        SS(2,I)=SIG(2,I)
        SS(3,I)=SIG(3,I)
        ST(1,I)=SIG(4,I)
        ST(2,I)=SIG(5,I)
        ST(3,I)=SIG(6,I)
c        print *,'DS,ST='
c        print *,SS(1,I),SS(2,I),SS(3,I)
c        print *,ST(1,I),ST(2,I),ST(3,I)
       ENDIF 
      ENDDO
C-----------GT->u-------- 
      DO I=LFT,LLT
c .....(R(I) is the alpha parameter of the radial return ..
c         print *,'[Ce]='
c         print *,DD(1,1,I),DD(1,2,I),GT(1,1,I)
       IF (R(I).NE.ONE) THEN
        DO J=1,3
        DO K=J,3
         DD(J,K,I)=DD(J,K,I)-SS(J,I)*SS(K,I)
         GT(J,K,I)=GT(J,K,I)-ST(J,I)*ST(K,I)
         DD(K,J,I)=DD(J,K,I)
         GT(K,J,I)=GT(J,K,I)
        ENDDO
C         IF (DD(J,J,I).LT.EM10.OR.GT(J,J,I).LT.EM10) THEN
C          print *,'GT(J,J,I)=',GT(J,J,I)
          DD(J,J,I)=MAX(EM10,DD(J,J,I))
          GT(J,J,I)=MAX(EM10,GT(J,J,I))
C         ENDIF
        ENDDO
        DO J=1,3
        DO K=1,3
         DD1(J,K,I)=-SS(J,I)*ST(K,I)
        ENDDO
        ENDDO
c         print *,'[Ct]='
c         print *,DD(1,1,I),DD(1,2,I),GT(1,1,I)
c         print *,DD1(1,1,I),DD1(2,1,I)
        ENDIF 
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GETHKT3                       source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GETHKT3(JFT ,JLT ,HT ,IR,IS,IT)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  ,IR,IS,IT
C     REAL
      my_real
     .   HT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K
C     REAL

      TYPE(L_KTBUFEP_)    , POINTER :: LBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
C-----------------------------------------------
        MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
        LBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IR,IS,IT)
        IF (MTAG%L_A_KT>0) THEN
         DO I=JFT,JLT
          HT(I)= LBUF%A_KT(I)
          IF (HT(I)==ONE) HT(I)= ZERO
         ENDDO
        ELSE
         DO I=JFT,JLT
          HT(I)= ZERO
         ENDDO
        END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S6CKEL3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        GETHKT3                       source/elements/solid/solide8z/mmats.F
Chd|        SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|        SKTVONM3                      source/elements/solid/solide8z/mmats.F
Chd|====================================================================
      SUBROUTINE MMSTIFS(PM    ,MAT  ,HH    ,VOL   ,ICSIG ,
     .                   DD    ,GG   ,DG    ,G33   ,DM    ,
     .                   GM    ,DGM  ,IORTH ,SIG   ,IR    ,
     .                   IS    ,IT   ,NEL   )
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
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICSIG,IORTH,IR,IS,IT,NEL
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*),HH(2,*),DD(3,3,*),GG(*),VOL(*),
     .   DG(3,3,*),G33(3,3,*),DM(3,3,*),GM(3,3,*),DGM(3,3,*),
     .   SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ICS,ICT,ICR,J,K
C     REAL
      my_real
     .   TT,TV,TT0,G(MVSIZ),QH(MVSIZ),EE,SFAC
C-----------------------------------------------
      SFAC=ONE
      IF (ISMDISP>0) SFAC=EM05
      IF (IORTH.GT.0) THEN
       IF (IKT.EQ.3) THEN
          DO I=LFT,LLT
C           QH(I)=GM(1,1,I)
candr           write(*,*)' QH(I)=', QH(I)!.. contains H modulus ..
candr ... H module is wrong here, it is due to an 'unclean'
candr ... passing it from routine 'mmats'
candr ... setting it 'by hand' proves that everything else
candr ... is correct
candr     QH(I)=1949.29d0 ! .. H for tension-torsion test ..

c           G(I) = HH(2,I)
          ENDDO
         DO J = 1, 3
         DO K = 1, 3
          DO I=LFT,LLT
           DGM(J,K,I)=ZERO
           GM(J,K,I)=ZERO
          ENDDO
         ENDDO
         ENDDO
         CALL GETHKT3(LFT ,LLT ,QH ,IR,IS,IT)
         CALL SKTVONM3(LFT ,LLT ,DM  ,HH   ,SIG  ,
     .                 DGM ,GM  ,QH  ,NEL  )
       ELSEIF (IKT.EQ.4) THEN
          DO I=LFT,LLT
           QH(I)= HH(1,I)
           G(I) = HH(2,I)
          ENDDO
        CALL SKTCONS2(LFT ,LLT ,DM  ,QH  ,G   ,DGM ,GM  ,IR,IS,IT)
       END IF
        DO J = 1, 3
        DO K = J, 3
         DO I=LFT,LLT
          DD(J,K,I)=DM(J,K,I)*VOL(I)
          G33(J,K,I)=GM(J,K,I)*VOL(I)
          DD(K,J,I)=DD(J,K,I)
          G33(K,J,I)=G33(J,K,I)
         ENDDO
        ENDDO
        ENDDO
candr (DG is non-symmetric)
        DO J = 1, 3
        DO K = 1, 3
         DO I=LFT,LLT
          DG(J,K,I)=DGM(J,K,I)*VOL(I)
         ENDDO
        ENDDO
        ENDDO

      ELSE
      IF (JHBE.EQ.15) THEN
       DO I=LFT,LLT
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT0=ONE/(TV+TWO*GG(I))
         TT=FOUR*GG(I)*(TV+GG(I))*TT0
         TV=TWO*GG(I)*TV*TT0
         EE=GG(I)*(THREE*HH(1,I)+TWO*HH(2,I))/(HH(1,I)+HH(2,I))
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=SFAC*EE
         DD(1,2,I)=TV
         DD(1,3,I)=ZERO
         DD(2,1,I)=TV
         DD(2,3,I)=ZERO
         DD(3,1,I)=ZERO
         DD(3,2,I)=ZERO
       ENDDO
      ELSEIF (JHBE.EQ.14.AND.ICSIG.GT.0) THEN
       ICS=ICSIG/100
       ICT=MOD(ICSIG/10,10)
       ICR=MOD(ICSIG,10)
       DO I=LFT,LLT
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT0=ONE/(TV+TWO*GG(I))
         TT=FOUR*GG(I)*(TV+GG(I))*TT0
         TV=TWO*GG(I)*TV*TT0
         EE=GG(I)*(THREE*HH(1,I)+TWO*HH(2,I))/(HH(1,I)+HH(2,I))
         DD(1,1,I)=TT
         DD(2,2,I)=TV
         DD(3,3,I)=SFAC*EE
       ENDDO
       IF (ICS.EQ.1) THEN
        DO I=LFT,LLT
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=ZERO
         DD(2,1,I)=TV
         DD(2,3,I)=ZERO
         DD(3,1,I)=ZERO
         DD(3,2,I)=ZERO
        ENDDO
       ELSEIF (ICT.EQ.1) THEN
        DO I=LFT,LLT
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=TT
         DD(2,2,I)=DD(3,3,I)
         DD(3,3,I)=TT
         DD(1,2,I)=ZERO
         DD(1,3,I)=TV
         DD(2,1,I)=ZERO
         DD(2,3,I)=ZERO
         DD(3,1,I)=TV
         DD(3,2,I)=ZERO
        ENDDO
       ELSEIF (ICR.EQ.1) THEN
        DO I=LFT,LLT
         TT=DD(1,1,I)
         TV=DD(2,2,I)
         DD(1,1,I)=DD(3,3,I)
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=ZERO
         DD(1,3,I)=ZERO
         DD(2,1,I)=ZERO
         DD(2,3,I)=TV
         DD(3,1,I)=ZERO
         DD(3,2,I)=TV
        ENDDO
       ENDIF
      ELSE
       DO I=LFT,LLT
         TV=HH(1,I)*VOL(I)
         GG(I)=HH(2,I)*VOL(I)
         TT=TV+TWO*GG(I)
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
       ENDDO
      ENDIF
      END IF !(IORTH.GT.0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  PUTSIGNOR3                    source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PUTSIGNOR3(JFT ,JLT ,MTN,IR,IS,IT,AL_IMP ,SIGNOR)
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
      INTEGER JFT, JLT ,MTN,IR,IS,IT
C     REAL
      my_real
     .   AL_IMP(*),SIGNOR(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K,IPTR,IPTS,IPTT
C     REAL

      TYPE(L_KTBUFEP_)    , POINTER :: SKBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
C-----------------------------------------------
       IF (IKT==0) RETURN
       MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
       IF (MTAG%L_A_KT>0) THEN
        IPTR=MAX(1,IR)
        IPTS=MAX(1,IS)
        IPTT=MAX(1,IT)
        SKBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IPTR,IPTS,IPTT)
        DO I=JFT ,JLT
         J=6*(I-1)
         SKBUF%A_KT(I)=AL_IMP(I)
         SKBUF%SIGE(J+1)=SIGNOR(I,1)
         SKBUF%SIGE(J+2)=SIGNOR(I,2)
         SKBUF%SIGE(J+3)=SIGNOR(I,3)
         SKBUF%SIGE(J+4)=SIGNOR(I,4)
         SKBUF%SIGE(J+5)=SIGNOR(I,5)
         SKBUF%SIGE(J+6)=SIGNOR(I,6)
        END DO
       END IF
C
      RETURN
      END


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
Chd|  MMATS                         source/elements/solid/solide8z/mmats.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        GETTRANS                      source/elements/solid/solide8z/gettrans.F
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/get_etfac_s.F
Chd|        MSTIFORTH                     source/elements/solid/solide8z/mstiforth.F
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
      IF (IORTH>0) THEN
       CALL GETTRANS(JFT,JLT,GAMA,QC,QCG,QGC,QG)
       IF (MTN==14.OR.MTN==12) THEN
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
       ELSEIF (MTN==25) THEN
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
       ELSEIF (MTN==28.OR.MTN==50.OR.MTN==68) THEN
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
       ELSEIF (MTN==53) THEN
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
       IF (MTN==42) THEN
        EFAC=TWOP5
        DO I=JFT,JLT
         MX  =MAT(I)
         LAMDA=EFAC*FAC(I)*UPARAM(11)
C-------GG:2G---HH(2,I):G-------
         GG =EFAC*FAC(I)*PM(32,MX)
         HH(1,I)=LAMDA
         HH(2,I)=GG*HALF
        ENDDO
       ELSEIF (MTN==62.OR.MTN==62.OR.MTN==69.OR.MTN==82) THEN
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
      ENDIF    !   IF (IORTH>0) THEN
C-------------KT Condinuum---sig:mean value-------------
      IF (IKT==2) THEN
C-------------KT Condinuum---sig:per ip------------
      ELSEIF (IKT==3) THEN
       SELECT CASE (MTN)
       CASE(1)                                            
       CASE(2,36)                                   
        IPLAST =0
        DO I=JFT,JLT
         MX      =MAT(I)
candr ---- make the 0-th iteration always elastic
           IF(FAC(I)<ONE.AND.ITER_NL>0) THEN
C          QH(I)= FAC(I)*TWO*HH(2,I)
          IPLAST =1
C         ELSE
C          QH(I)=ZERO
         ENDIF
        ENDDO
C        IF (IPLAST>0) THEN
C          DO I=JFT,JLT
C           G33(1,1,I)=QH(I)
C          ENDDO
C        ENDIF 
        IF (IORTH==0.AND.IPLAST==1) IORTH=1
       CASE(10)                                        
       END SELECT      
C-------------KT Consistent--sig: per ip-------------
      ELSEIF (IKT==4) THEN
       S1=ONE*NPPT
       SELECT CASE (MTN)
       CASE(1)                                            
       CASE(2,36)                                   
        IPLAST =0
        DO I=JFT,JLT
candr ---  make the 0-th iteration always elastic
           IF(FAC(I)<ONE.AND.ITER_NL>0) IPLAST =1
        ENDDO
        IF (IORTH==0.AND.IPLAST==1) IORTH=1
       CASE(10)                                        
       END SELECT      
      ENDIF !IF (IKT==2) THEN
 
C
      RETURN
      END

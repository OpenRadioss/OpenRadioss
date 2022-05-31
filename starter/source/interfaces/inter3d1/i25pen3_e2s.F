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
Chd|  I25PEN3_E2S                   source/interfaces/inter3d1/i25pen3_e2s.F
Chd|-- called by -----------
Chd|        I25STO_E2S                    source/interfaces/inter3d1/i25sto_e2s.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25PEN3_E2S(JLT ,CAND_S ,CAND_M  ,DRAD  ,IGAP0   ,
     .                    NEDGE  ,LEDGE  ,MARGE   ,GAP_M ,GAP_M_L ,
     .                    GAPE   ,GAP_E_L,IGAP    ,X     ,IRECT   ,
     .                    PENE  ,ADMSR   ,EDG_BISECTOR,VTX_BISECTOR,ITAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "scr05_c.inc"
C#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IGAP0, NEDGE, IGAP
      INTEGER IRECT(4,*), CAND_S(*), CAND_M(*), LEDGE(NLEDGE,*), ADMSR(4,*), ITAB(*)
      my_real
     .     DRAD, MARGE,
     .     X(3,*), GAP_M(*), GAP_M_L(*), GAPE(*), GAP_E_L(*), PENE(MVSIZ)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NI, N1, N2, K, IE, JE, JL, I1, I2, I3, I4, NLS, NLT, LIST(MVSIZ)
      my_real
     .     XXS1(MVSIZ) ,XXS2(MVSIZ) ,YYS1(MVSIZ) ,YYS2(MVSIZ) ,ZZS1(MVSIZ) ,ZZS2(MVSIZ) ,
     .     XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,XX4,YY4,ZZ4,
     .     XXA,YYA,ZZA,XXB,YYB,ZZB,
     .     XMAXS,YMAXS,ZMAXS,XMAXM,YMAXM,ZMAXM,DX,DY,DZ,
     .     XMINS,YMINS,ZMINS,XMINM,YMINM,ZMINM,GAPV(MVSIZ),
     .     X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ), 
     .     X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ), 
     .     X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ), 
     .     X4(MVSIZ), Y4(MVSIZ), Z4(MVSIZ),
     .     X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ), X42(MVSIZ), Y42(MVSIZ),
     .     Z42(MVSIZ), X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), 
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ), SUMA
C-----------------------------------------------
      DO I=1,JLT
        IE = CAND_M(I)
        IF(CAND_S(I) <= NEDGE) THEN
          JE = CAND_S(I)
          GAPV(I)=GAPE(JE) ! Peut etre considere comme nul pour l instant
C
          IF(IGAP == 3)
     .       GAPV(I)=MIN(GAP_M_L(IE)+GAP_E_L(JE),GAPV(I)) ! under-estimated ...
        ENDIF
        GAPV(I)=MAX(DRAD,GAPV(I))+MARGE
      ENDDO

      DO I=1,JLT
        IF(CAND_S(I) <= NEDGE) THEN
          N1 = LEDGE(5,CAND_S(I))
          N2 = LEDGE(6,CAND_S(I))

          XXS1(I) = X(1,N1)
          YYS1(I) = X(2,N1)
          ZZS1(I) = X(3,N1)
          XXS2(I) = X(1,N2)
          YYS2(I) = X(2,N2)
          ZZS2(I) = X(3,N2)
        END IF
      ENDDO

      DO I=1,JLT
        IE = CAND_M(I)

        I1 = IRECT(1,CAND_M(I))
        I2 = IRECT(2,CAND_M(I))
        I3 = IRECT(3,CAND_M(I))
        I4 = IRECT(4,CAND_M(I))
        X1(I) = X(1,I1)
        Y1(I) = X(2,I1)
        Z1(I) = X(3,I1)
        X2(I) = X(1,I2)
        Y2(I) = X(2,I2)
        Z2(I) = X(3,I2)
        X3(I) = X(1,I3)
        Y3(I) = X(2,I3)
        Z3(I) = X(3,I3)
        X4(I) = X(1,I4)
        Y4(I) = X(2,I4)
        Z4(I) = X(3,I4)
      END DO

      DO I=1,JLT
        X21(I)=X2(I)-X1(I)                              
        Y21(I)=Y2(I)-Y1(I)                              
        Z21(I)=Z2(I)-Z1(I)                              
        X31(I)=X3(I)-X1(I)                              
        Y31(I)=Y3(I)-Y1(I)                              
        Z31(I)=Z3(I)-Z1(I)                              
        X42(I)=X4(I)-X2(I)                              
        Y42(I)=Y4(I)-Y2(I)                              
        Z42(I)=Z4(I)-Z2(I)                              
C
        E3X(I)=Y31(I)*Z42(I)-Z31(I)*Y42(I)              
        E3Y(I)=Z31(I)*X42(I)-X31(I)*Z42(I)              
        E3Z(I)=X31(I)*Y42(I)-Y31(I)*X42(I)              
        SUMA=E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)  
        SUMA=MAX(SQRT(SUMA),EM20)                       
        E3X(I)=E3X(I)/SUMA                              
        E3Y(I)=E3Y(I)/SUMA                              
        E3Z(I)=E3Z(I)/SUMA                              
      END DO
C
      DO I=1,JLT
        SUMA= X21(I)*E3X(I)+Y21(I)*E3Y(I)+Z21(I)*E3Z(I)  
        E1X(I)= X21(I)-E3X(I)*SUMA                       
        E1Y(I)= Y21(I)-E3Y(I)*SUMA                       
        E1Z(I)= Z21(I)-E3Z(I)*SUMA                       
      ENDDO                                              
C
      DO I=1,JLT
        SUMA=E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)  
        SUMA=MAX(SQRT(SUMA),EM20)                       
        E1X(I)=E1X(I)/SUMA                              
        E1Y(I)=E1Y(I)/SUMA                              
        E1Z(I)=E1Z(I)/SUMA                              
      ENDDO                                             
C
      DO I=1,JLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)                
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)                
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)                
        SUMA  =E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)  
        SUMA  =MAX(SQRT(SUMA),EM20)                       
        E2X(I)=E2X(I)/SUMA                                
        E2Y(I)=E2Y(I)/SUMA                                
        E2Z(I)=E2Z(I)/SUMA                                
      ENDDO                                               
C
      NLS=0
      DO I=1,JLT
        XX1=E1X(I)*X1(I)+E1Y(I)*Y1(I)+E1Z(I)*Z1(I)
        XX2=E1X(I)*X2(I)+E1Y(I)*Y2(I)+E1Z(I)*Z2(I)
        XX3=E1X(I)*X3(I)+E1Y(I)*Y3(I)+E1Z(I)*Z3(I)
        XX4=E1X(I)*X4(I)+E1Y(I)*Y4(I)+E1Z(I)*Z4(I)
        XMINM=MIN(XX1,XX2,XX3,XX4)
        XMAXM=MAX(XX1,XX2,XX3,XX4)
	DX=EM02*(XMAXM-XMINM)
        XMINM=XMINM-DX-GAPV(I)
        XMAXM=XMAXM+DX+GAPV(I)
        XXA=E1X(I)*XXS1(I)+E1Y(I)*YYS1(I)+E1Z(I)*ZZS1(I)
        XXB=E1X(I)*XXS2(I)+E1Y(I)*YYS2(I)+E1Z(I)*ZZS2(I)
        XMINS = MIN(XXA,XXB)
        XMAXS = MAX(XXA,XXB)
        IF (XMAXS.GE.XMINM.AND.XMAXM.GE.XMINS) THEN
         NLS=NLS+1
         LIST(NLS)=I
        ENDIF
      ENDDO                                               

      NLT=NLS
      NLS=0
      DO K=1,NLT
        I=LIST(K)
        YY1=E2X(I)*X1(I)+E2Y(I)*Y1(I)+E2Z(I)*Z1(I)
        YY2=E2X(I)*X2(I)+E2Y(I)*Y2(I)+E2Z(I)*Z2(I)
        YY3=E2X(I)*X3(I)+E2Y(I)*Y3(I)+E2Z(I)*Z3(I)
        YY4=E2X(I)*X4(I)+E2Y(I)*Y4(I)+E2Z(I)*Z4(I)
        YMINM=MIN(YY1,YY2,YY3,YY4)
        YMAXM=MAX(YY1,YY2,YY3,YY4)
	DY=EM02*(YMAXM-YMINM)
        YMINM=YMINM-DY-GAPV(I)
        YMAXM=YMAXM+DY+GAPV(I)
        YYA=E2X(I)*XXS1(I)+E2Y(I)*YYS1(I)+E2Z(I)*ZZS1(I)
        YYB=E2X(I)*XXS2(I)+E2Y(I)*YYS2(I)+E2Z(I)*ZZS2(I)
        YMINS = MIN(YYA,YYB)
        YMAXS = MAX(YYA,YYB)
        IF (YMAXS.GE.YMINM.AND.YMAXM.GE.YMINS) THEN
         NLS=NLS+1
         LIST(NLS)=I
        ENDIF
      ENDDO                                               

      NLT=NLS
      NLS=0
      DO K=1,NLT
        I=LIST(K)
        ZZ1=E3X(I)*X1(I)+E3Y(I)*Y1(I)+E3Z(I)*Z1(I)
        ZZ2=E3X(I)*X2(I)+E3Y(I)*Y2(I)+E3Z(I)*Z2(I)
        ZZ3=E3X(I)*X3(I)+E3Y(I)*Y3(I)+E3Z(I)*Z3(I)
        ZZ4=E3X(I)*X4(I)+E3Y(I)*Y4(I)+E3Z(I)*Z4(I)
        ZMINM=MIN(ZZ1,ZZ2,ZZ3,ZZ4)
        ZMAXM=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
	DZ=EM02*(ZMAXM-ZMINM)
        ZMINM=ZMINM-DZ-GAPV(I)
        ZMAXM=ZMAXM+DZ+GAPV(I)
        ZZA=E3X(I)*XXS1(I)+E3Y(I)*YYS1(I)+E3Z(I)*ZZS1(I)
        ZZB=E3X(I)*XXS2(I)+E3Y(I)*YYS2(I)+E3Z(I)*ZZS2(I)
        ZMINS = MIN(ZZA,ZZB)
        ZMAXS = MAX(ZZA,ZZB)
        IF (ZMAXS.GE.ZMINM.AND.ZMAXM.GE.ZMINS) THEN
         NLS=NLS+1
         LIST(NLS)=I
        ENDIF
      ENDDO                                               

      PENE(1:JLT)=ZERO

      NLT=NLS
#include      "vectorize.inc"
      DO K=1,NLT
        I=LIST(K)
        PENE(I)=ONE
      ENDDO                                               
C
      RETURN
      END

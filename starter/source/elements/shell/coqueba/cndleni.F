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
Chd|  CNDLENI                       source/elements/shell/coqueba/cndleni.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE CNDLENI(PM      ,GEO     ,STIFN    ,STIFR   ,IXC     ,
     .           THK     ,IHBE    ,IGEO    ,SH4TREE ,ALDT    ,                 
     .           UPARAM  ,IPM     ,NLAY    ,PM_STACK, ISUBSTACK,               
     .           STRC    ,AREA    ,IMAT    ,IPROP   ,DTEL    ,  
     .           X2L     ,X3L     ,X4L     ,Y2L     ,Y3L     ,Y4L    ,
     .           IGEO_STACK       ,GROUP_PARAM)        
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD            
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
#include      "remesh_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMAT, IPROP
      INTEGER IXC(NIXC,*),IGEO(NPROPGI,*),IHBE, SH4TREE(KSH4TREE,*),
     .   IPM(NPROPMI,*),NLAY,ISUBSTACK,IGEO_STACK(3* NPT_STACK+2,*)
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),STIFN(*),STIFR(*),THK(*),ALDT(*),
     .   UPARAM(*),PM_STACK(20,*),STRC(*), DTEL(MVSIZ),
     .   X2L(MVSIZ),X3L(MVSIZ),X4L(MVSIZ),Y2L(MVSIZ),Y3L(MVSIZ),Y4L(MVSIZ)
      TYPE(GRP_PARAM_STRUCT_)  :: GROUP_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IMT, IPMAT, IGTYP,IADB,
     .        I1,I3,IPTHK,IPPOS,I2,MATLY,IPGMAT,IGMAT,IPOS,NIP,MLAWLY
      my_real
     .   AREA(MVSIZ),SSP(MVSIZ), AL1(MVSIZ), 
     .   AL2(MVSIZ), AL3(MVSIZ), AL4(MVSIZ), AL5(MVSIZ),
     .   AL6(MVSIZ), AL(MVSIZ), ALMIN(MVSIZ),LXYZ0(2),COREL(2,4)
      my_real
     .   VISCMX,A11,A11R,A12,B1,B2,VV,STI,STIR,VISCDEF,RHO,YOUNG,NU,
     .   X13,X24,Y13,Y24,L13,L24,C1,C2,THKLY,POSLY,
     .   FAC,VISCE,RX,RY,SX,SY,S1,FAC1,FAC2,FACI,FAC11,BULK,GMAX
C=======================================================================
      FAC = TWO
C
      IGTYP = NINT(GEO(12,IPROP))
      IGMAT = IGEO(98,IPROP)
      IPGMAT = 700
      SSP(LFT:LLT) = ZERO
C
      IF ((IGTYP == 11 .AND. IGMAT < 0) .OR. IGTYP == 16 ) THEN
             IPMAT = 100
             NIP = NPT
             IF (MTN<=28) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT = IGEO(IPMAT+N,IPROP)
                   SSP(I)=MAX(SSP(I),PM(27,IMT))
                 ENDDO
               ENDDO
             ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT  = IGEO(IPMAT+N,IPROP)
                   IADB = IPM(7,IMT)-1                                      
                   BULK = UPARAM(IADB+11)                                     
                   NU   = UPARAM(IADB+14)
                   GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)
     .                  + UPARAM(IADB+2)*UPARAM(IADB+7)
     .                  + UPARAM(IADB+3)*UPARAM(IADB+8)  
     .                  + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .                  + UPARAM(IADB+5)*UPARAM(IADB+10) 
                   RHO  = PM(1,IMT)
                   A11  = GMAX*(ONE + NU)/(ONE - NU**2)
                   SSP(I)=MAX(SSP(I), SQRT(A11/RHO)) 
                 ENDDO
               ENDDO
             ELSEIF (MTN == 65) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT =IGEO(IPMAT+N,IPROP)
                   RHO  =PM(1,IMT)
                   YOUNG=PM(20,IMT)
                   SSP(I)=MAX(SSP(I), SQRT(YOUNG/RHO)) 
                 ENDDO
               ENDDO
             ELSE
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT =IGEO(IPMAT+N,IPROP)
                   RHO  =PM(1,IMT)
                   YOUNG=PM(20,IMT)
                   NU   =PM(21,IMT)
                   SSP(I)=MAX(SSP(I), SQRT(YOUNG/(ONE-NU*NU)/RHO)) 
                 ENDDO
               ENDDO
             ENDIF
CCC      
       ELSEIF (IGTYP == 17 .AND. IGMAT < 0) THEN  
             NIP = NPT
             IPMAT = 2 + NIP
             IF (MTN<=28) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT = IGEO_STACK(IPMAT + N,ISUBSTACK)
                   SSP(I)=MAX(SSP(I),PM(27,IMT))
                 ENDDO
               ENDDO
             ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT  = IGEO_STACK(IPMAT + N,ISUBSTACK)
                   IADB = IPM(7,IMT)-1                                      
                   BULK = UPARAM(IADB+11)                                     
                   NU   = UPARAM(IADB+14)
                   GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)
     .                  + UPARAM(IADB+2)*UPARAM(IADB+7)
     .                  + UPARAM(IADB+3)*UPARAM(IADB+8)  
     .                  + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .                  + UPARAM(IADB+5)*UPARAM(IADB+10) 
                   RHO  = PM(1,IMT)
                   A11  = GMAX*(ONE + NU)/(ONE - NU**2)
                   SSP(I)=MAX(SSP(I), SQRT(A11/RHO)) 
                 ENDDO
               ENDDO
             ELSEIF (MTN == 65) THEN
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT =IGEO_STACK(IPMAT + N,ISUBSTACK)
                   RHO  =PM(1,IMT)
                   YOUNG=PM(20,IMT)
                   SSP(I)=MAX(SSP(I), SQRT(YOUNG/RHO)) 
                 ENDDO
               ENDDO
             ELSE
               DO I=LFT,LLT
                 DO N=1,NIP
                   J=I+(N-1)*LLT
                   IMT =IGEO_STACK(IPMAT + N,ISUBSTACK)
                   RHO  =PM(1,IMT)
                   YOUNG=PM(20,IMT)
                   NU   =PM(21,IMT)
                   SSP(I)=MAX(SSP(I), SQRT(YOUNG/(ONE-NU*NU)/RHO)) 
                 ENDDO
               ENDDO
             ENDIF
      ELSEIF (IGTYP == 51 .AND. IGMAT <  0) THEN
          NIP = NLAY 
          IPMAT = 2 + NLAY
          DO I=LFT,LLT
            DO N=1,NIP
              IMT = IGEO_STACK(IPMAT + N,ISUBSTACK)
              MLAWLY = NINT(PM(19,IMT))
              IF (MLAWLY <= 28) THEN
                SSP(I)=MAX(SSP(I),PM(27,IMT))
              ELSEIF (MLAWLY == 42 .OR. MLAWLY  == 69) THEN
                IADB = IPM(7,IMT)-1                                         
                BULK = UPARAM(IADB+11)                                        
                NU = UPARAM(IADB+14)
                GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)
     .               + UPARAM(IADB+2)*UPARAM(IADB+7)
     .               + UPARAM(IADB+3)*UPARAM(IADB+8)  
     .               + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .               + UPARAM(IADB+5)*UPARAM(IADB+10) 
                RHO  = PM(1,IMT)
                A11  = GMAX*(ONE + NU)/(ONE - NU**2)
                SSP(I)=MAX(SSP(I), SQRT(A11/RHO))
              ELSEIF (MLAWLY == 65) THEN
                RHO  =PM(1,IMT)
                YOUNG=PM(20,IMT)
                SSP(I)=MAX(SSP(I), SQRT(YOUNG/RHO))
              ELSE
                RHO  =PM(1,IMT)
                YOUNG=PM(20,IMT)
                NU   =PM(21,IMT)
                SSP(I)=MAX(SSP(I), SQRT(YOUNG/(ONE-NU*NU)/RHO))
              ENDIF
              ENDDO
            ENDDO
      ELSEIF (IGTYP == 11 .AND. IGMAT > 0) THEN
          DO I=LFT,LLT
            SSP(I) = GEO(IPGMAT +9 ,IPROP)
          ENDDO  
      ELSEIF (IGTYP == 52 .OR. 
     .      ((IGTYP == 51 .OR. IGTYP == 17 ).AND. IGMAT > 0)) THEN
          DO I=LFT,LLT
             SSP(I) = PM_STACK(9 ,ISUBSTACK)
          ENDDO
      ELSEIF (MTN<=28)THEN
          DO I=LFT,LLT
            SSP(I)=PM(27,IMAT)
          ENDDO
      ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
          DO I=LFT,LLT
            IADB = IPM(7,IMAT)-1                                             
            BULK = UPARAM(IADB+11)                                             
            NU = UPARAM(IADB+14)         
            GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)               
     .           + UPARAM(IADB+2)*UPARAM(IADB+7) 
     .           + UPARAM(IADB+3)*UPARAM(IADB+8) 
     .           + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .           + UPARAM(IADB+5)*UPARAM(IADB+10) 
             RHO  = PM(1,IMAT) 
             A11 = GMAX*(ONE + NU)/(ONE - NU**2)                                
             SSP(I)=MAX(SSP(I), SQRT(A11/RHO))    
          ENDDO
      ELSEIF (MTN == 65) THEN
          DO I=LFT,LLT
            RHO  =PM(1,IMAT)
            YOUNG=PM(20,IMAT)
            SSP(I)=SQRT(YOUNG/RHO)
          ENDDO
      ELSE
          DO I=LFT,LLT
            RHO  =PM(1,IMAT)
            YOUNG=PM(20,IMAT)
            NU   =PM(21,IMAT)
            SSP(I)=SQRT(YOUNG/(ONE-NU*NU)/RHO)
          ENDDO
      ENDIF
C-------longueur caracteristique--------------
      FAC11=FIVE_OVER_4
      IF (IHBE == 11) FAC11=FOUR_OVER_3
      DO I=LFT,LLT
        LXYZ0(1)=FOURTH*(X2L(I)+X3L(I)+X4L(I))
        LXYZ0(2)=FOURTH*(Y2L(I)+Y3L(I)+Y4L(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=X2L(I)-LXYZ0(1)
        COREL(1,3)=X3L(I)-LXYZ0(1)
        COREL(1,4)=X4L(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=Y2L(I)-LXYZ0(2)
        COREL(2,3)=Y3L(I)-LXYZ0(2)
        COREL(2,4)=Y4L(I)-LXYZ0(2)
        X13=(COREL(1,1)-COREL(1,3))*HALF
        X24=(COREL(1,2)-COREL(1,4))*HALF
        Y13=(COREL(2,1)-COREL(2,3))*HALF
        Y24=(COREL(2,2)-COREL(2,4))*HALF

C
        L13=X13*X13+Y13*Y13
        L24=X24*X24+Y24*Y24
        AL1(I)=MAX(L13,L24)
        C1 =COREL(1,2)*COREL(2,4)-COREL(2,2)*COREL(1,4)
        C2 =COREL(1,1)*COREL(2,3)-COREL(2,1)*COREL(1,3)
        AL2(I) =MAX(ABS(C1),ABS(C2))/AREA(I)
        RX=X24-X13
        RY=Y24-Y13
        SX=-X24-X13
        SY=-Y24-Y13
        C1=SQRT(RX*RX+RY*RY)
        C2=SQRT(SX*SX+SY*SY)
        S1=FOURTH*(MAX(C1,C2)/MIN(C1,C2)-ONE)
        FAC1=MIN(HALF,S1)+ONE
        FAC2=AREA(I)/(C1*C2)
        FAC2=3.413*MAX(ZERO,FAC2-0.7071)
        FAC2=0.78+0.22*FAC2*FAC2*FAC2
        FACI=TWO*FAC1*FAC2
        S1 = SQRT(FACI*(FAC11+AL2(I))*AL1(I))
        S1 = MAX(S1,EM20)
        AL1(I)=S1
      ENDDO 
c-------------------------------------------------------
      IF (IHBE == 11) THEN
       DO I=LFT,LLT
        ALMIN(I)= AREA(I)/AL1(I)
       ENDDO 
       VISCE=EM3
      ELSEIF (IHBE == 23) THEN
       DO I=LFT,LLT
        ALMIN(I)= AREA(I)/AL1(I)
       ENDDO 
       VISCE=ZEP015
      ELSE
       DO I=LFT,LLT
        ALMIN(I)= AREA(I)/SQRT(FAC*AL1(I))
       ENDDO 
       VISCE=ZERO
      ENDIF
C
      IF(MTN == 19)THEN
          VISCDEF=FOURTH
      ELSEIF(MTN == 25.OR.MTN == 27)THEN
          VISCDEF=FIVEEM2
      ELSE
          VISCDEF=ZERO
      ENDIF
c
      VISCMX = GROUP_PARAM%VISC_DM
      VISCE  = GEO(13,IPROP)
      IF (VISCMX == ZERO) VISCMX = VISCDEF
      IF (MTN == 1 .OR.MTN == 2 .OR. MTN == 3.OR.
     .    MTN == 22.OR.MTN == 23.OR.MTN == 91) VISCMX = ZERO
      VISCMX = MAX(VISCMX,VISCE)
      VISCMX = SQRT(ONE + VISCMX*VISCMX)-VISCMX
c
      DO I=LFT,LLT
        DTEL(I)= ALMIN(I)*VISCMX/SSP(I)
        ALDT(I)= ALMIN(I)
      ENDDO
C----------------------------------------------------------
C     DT NODAL
C----------------------------------------------------------       
      IPGMAT = 700
      IF(NADMESH == 0)THEN
       IF(IGTYP  == 11 .AND. IGMAT > 0)THEN
         DO I=LFT,LLT
             A11  =GEO(IPGMAT +5 ,IPROP)
             A11R =GEO(IPGMAT +7 ,IPROP) 
             VV = VISCMX * ALMIN(I)
             VV = VV*VV
             FAC = HALF*AREA(I)*THK(I) / VV
             STI = FAC * A11 
             STIR = ONE_OVER_12*FAC*A11R*THK(I)**2
             STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
             STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
             STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
             STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
             STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
             STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
             STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
             STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
             STRC(I) = STIR
         ENDDO 
       ELSEIF(IGTYP == 52 .OR.
     .       ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
         DO I=LFT,LLT
              A11  = PM_STACK(5 ,ISUBSTACK)
              A11R = PM_STACK(7 ,ISUBSTACK)
              VV = VISCMX * ALMIN(I)
              VV = VV*VV
              FAC = HALF*AREA(I)*THK(I) / VV
              STI = FAC * A11 
              STIR = ONE_OVER_12*FAC*A11R*THK(I)**2
              STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
              STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
              STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
              STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
              STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
              STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
              STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
              STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
              STRC(I) = STIR
          ENDDO
       ELSE
         DO I=LFT,LLT
             A11 =PM(24,IMAT)
             VV = VISCMX * ALMIN(I)
             VV = VV*VV
             STI = HALF*THK(I) *AREA(I)* A11 / VV
             STIR = STI * THK(I)*THK(I) * ONE_OVER_12
             STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
             STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
             STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
             STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
             STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
             STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
             STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
             STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
             STRC(I) = STIR
         ENDDO
       ENDIF  
      ELSE
        IF(IGTYP == 11 .AND. IGMAT > 0) THEN
          DO I=LFT,LLT
             N=NFT+I
             IF(SH4TREE(3,N) >= 0)THEN
                A11  = GEO(IPGMAT +5 ,IPROP)
                A11R = GEO(IPGMAT +7 ,IPROP) 
                VV = VISCMX * ALMIN(I)
                VV = VV*VV
                FAC = HALF*AREA(I)*THK(I) / VV
                STI = FAC * A11 
                STIR = ONE_OVER_12*FAC*A11R*THK(I)**2
                STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
                STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
                STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
                STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
                STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
                STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
                STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
                STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
                STRC(I) = STIR
             END IF
           END DO
        ELSEIF(IGTYP == 52 .OR. 
     .       ((IGTYP == 17.OR. IGTYP == 51) .AND. IGMAT > 0 )) THEN
             DO I=LFT,LLT
              N=NFT+I
              IF(SH4TREE(3,N) >= 0)THEN
                 A11  = PM_STACK(5 ,ISUBSTACK)
                 A11R = PM_STACK(7 ,ISUBSTACK) 
                VV = VISCMX * ALMIN(I)
                VV = VV*VV
                FAC = HALF*AREA(I)*THK(I) / VV
                STI = FAC * A11 
                STIR = ONE_OVER_12*FAC*A11R*THK(I)**2
                STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
                STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
                STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
                STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
                STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
                STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
                STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
                STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
                STRC(I) = STIR
               END IF
             END DO      
         ELSE
            DO I=LFT,LLT
             N=NFT+I
             IF(SH4TREE(3,N) >= 0)THEN
                A11 =PM(24,IMAT)
                VV = VISCMX * ALMIN(I)
                VV = VV*VV
                STI = HALF*THK(I) *AREA(I)* A11 / VV
                STIR = STI * THK(I)*THK(I) * ONE_OVER_12
                STIFN(IXC(2,I))=STIFN(IXC(2,I))+STI
                STIFN(IXC(3,I))=STIFN(IXC(3,I))+STI
                STIFN(IXC(4,I))=STIFN(IXC(4,I))+STI
                STIFN(IXC(5,I))=STIFN(IXC(5,I))+STI
                STIFR(IXC(2,I))=STIFR(IXC(2,I))+STIR
                STIFR(IXC(3,I))=STIFR(IXC(3,I))+STIR
                STIFR(IXC(4,I))=STIFR(IXC(4,I))+STIR
                STIFR(IXC(5,I))=STIFR(IXC(5,I))+STIR
                STRC(I) = STIR
             END IF
         END DO
        ENDIF 
      END IF
C----------------------------------------------------------
      IF (ISMSTR == 3) THEN
        IF (GEO(5,IPROP) /=ZERO) GEO(5,IPROP)= MIN(GEO(5,IPROP),DTEL(I))
      ENDIF
c-----------
      RETURN
      END

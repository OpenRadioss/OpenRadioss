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
Chd|  CDLENI                        source/elements/shell/coque/cdleni.F
Chd|-- called by -----------
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE CDLENI(PM  ,GEO ,STIFN ,STIFR,IXC,
     .                  PX1 ,PX2 ,PY1   ,PY2  ,THK,
     .                  IGEO,DT  ,SH4TREE,ALDT,UPARAM ,
     .                  IPM ,NLAY,PM_STACK,ISUBSTACK,STRC,
     .                  AREA    ,IMAT    ,IPROP   ,
     .                  X2L ,X3L ,X4L  ,Y2L ,Y3L  ,Y4L     ,
     .                  IGEO_STACK ,GROUP_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUP_PARAM_MOD            
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
      INTEGER IMAT,IPROP
      INTEGER IXC(NIXC,*), IGEO(NPROPGI,*), SH4TREE(KSH4TREE,*),
     .   IPM(NPROPMI,*),NLAY,ISUBSTACK,IGEO_STACK(4*NPT_STACK+2,*)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),STIFN(*),STIFR(*),UPARAM(*),
     .   PX1(*),PX2(*),PY1(*),PY2(*),THK(*),DT(*),ALDT(*),PM_STACK(20,*),
     .   AREA(MVSIZ), STRC(*),
     .   X2L(MVSIZ),X3L(MVSIZ),X4L(MVSIZ),Y2L(MVSIZ),Y3L(MVSIZ),Y4L(MVSIZ)
      TYPE (GROUP_PARAM_)  :: GROUP_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IMT, IPMAT, IGTYP,IPPID,IADB,
     .        I1,I3,IPTHK,IPPOS,I2,MATLY,IGMAT,IPGMAT,IPOS,NIP,MLAWLY
      my_real
     .   SSP(MVSIZ), AL1(MVSIZ),AL(MVSIZ), ALMIN(MVSIZ),
     .   AL2(MVSIZ), AL3(MVSIZ), AL4(MVSIZ), AL5(MVSIZ), AL6(MVSIZ)
      my_real
     .   VISCMX,A11,A11R,A12,B1,B2,C1,VV,STI,STIR,VISCDEF,DTDYN,RHO, 
     .   YOUNG,NU,BULK,GMAX,THKLY,POSLY,FAC
C======================================================================|
      IGTYP = NINT(GEO(12,IPROP))
      IGMAT = IGEO(98,IPROP)
      IPGMAT = 700
      SSP(LFT:LLT) = ZERO
c
      IF ((IGTYP == 11 .AND. IGMAT < 0) .OR. IGTYP == 16) THEN
          IPMAT = 100
         IF (MTN <= 28) THEN
          DO I=LFT,LLT
            DO N=1,NPT
              J=I+(N-1)*LLT
              IMT = IGEO(IPMAT+N,IPROP)
              SSP(I)=MAX(SSP(I),PM(27,IMT))
            ENDDO
          ENDDO
        ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
          DO I=LFT,LLT
            DO N=1,NPT
              J=I+(N-1)*LLT
              IMT = IGEO(IPMAT+N,IPROP)
              IADB = IPM(7,IMT)-1
              BULK = UPARAM(IADB+11)
              NU =  UPARAM(IADB+14)
              GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)
     .             + UPARAM(IADB+2)*UPARAM(IADB+7)
     .             + UPARAM(IADB+3)*UPARAM(IADB+8)
     .             + UPARAM(IADB+4)*UPARAM(IADB+9)
     .             + UPARAM(IADB+5)*UPARAM(IADB+10)
              RHO  = PM(1,IMT)
              A11 = GMAX*(ONE + NU)/(ONE - NU**2)
              SSP(I)=MAX(SSP(I), SQRT(A11/RHO)) 
            ENDDO
          ENDDO
        ELSEIF (MTN == 65) THEN
          DO I=LFT,LLT
            DO N=1,NPT
              J=I+(N-1)*LLT
              IMT = IGEO(IPMAT+N,IPROP)
              RHO  =PM(1,IMT)
              YOUNG=PM(20,IMT)
              SSP(I)=MAX(SSP(I), SQRT(YOUNG/RHO)) 
            ENDDO
          ENDDO
        ELSE
          DO I=LFT,LLT
            DO N=1,NPT
              J=I+(N-1)*LLT
              IMT = IGEO(IPMAT+N,IPROP)
              RHO  =PM(1,IMT)
              YOUNG=PM(20,IMT)
              NU   =PM(21,IMT)
              SSP(I)=MAX(SSP(I), SQRT(YOUNG/(ONE-NU*NU)/RHO)) 
            ENDDO
          ENDDO
        ENDIF
      ELSEIF(IGTYP == 11 .AND. IGMAT > 0) THEN
         DO I=LFT,LLT
            SSP(I) = GEO(IPGMAT +9 ,IPROP)
         ENDDO  
      ELSEIF(IGTYP == 52 .OR. 
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
         DO I=LFT,LLT
          SSP(I) = PM_STACK(9 ,ISUBSTACK)
        ENDDO
      ELSEIF(IGTYP == 17 .AND. IGMAT <  0) THEN
        IPPID = 100
        NIP = NPT
        IPMAT = 2 + NIP 
        IF(MTN<=28)THEN
          DO I=LFT,LLT
            DO N=1,NIP
              J=I+(N-1)*LLT
              IMT  = IGEO_STACK(IPMAT + N,ISUBSTACK)
              SSP(I)=MAX(SSP(I),PM(27,IMT))
            ENDDO
          ENDDO
        ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
          DO I=LFT,LLT
            DO N=1,NIP
              J=I+(N-1)*LLT
              IMT = IGEO_STACK(IPMAT + N,ISUBSTACK)
              IADB = IPM(7,IMT)-1                                           
              BULK = UPARAM(IADB+11)
              NU   =  UPARAM(IADB+14)
              GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)
     .             + UPARAM(IADB+2)*UPARAM(IADB+7)               
     .             + UPARAM(IADB+3)*UPARAM(IADB+8)          
     .             + UPARAM(IADB+4)*UPARAM(IADB+9)             
     .             + UPARAM(IADB+5)*UPARAM(IADB+10) 
              RHO  = PM(1,IMT) 
              A11 = GMAX*(ONE + NU)/(ONE - NU**2)
              SSP(I)=MAX(SSP(I), SQRT(A11/RHO)) 
            ENDDO
          ENDDO
        ELSEIF (MTN == 65) THEN
          DO I=LFT,LLT
            DO N=1,NIP
              J=I+(N-1)*LLT
              IMT = IGEO_STACK(IPMAT + N,ISUBSTACK)
              RHO  =PM(1,IMT)
              YOUNG=PM(20,IMT)
              SSP(I)=MAX(SSP(I), SQRT(YOUNG/RHO)) 
            ENDDO
          ENDDO
        ELSE
          DO I=LFT,LLT
            DO N=1,NIP
              J=I+(N-1)*LLT
              IMT = IGEO_STACK(IPMAT + N,ISUBSTACK)
              RHO  =PM(1,IMT)
              YOUNG=PM(20,IMT)
              NU   =PM(21,IMT)
              SSP(I)=MAX(SSP(I), SQRT(YOUNG/(ONE-NU*NU)/RHO)) 
            ENDDO
          ENDDO
        ENDIF
      ELSEIF(IGTYP == 51 .AND. IGMAT <  0) THEN
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
     .             + UPARAM(IADB+2)*UPARAM(IADB+7)
     .             + UPARAM(IADB+3)*UPARAM(IADB+8)  
     .             + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .             + UPARAM(IADB+5)*UPARAM(IADB+10) 
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
c  
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
     .         + UPARAM(IADB+2)*UPARAM(IADB+7)  
     .         + UPARAM(IADB+3)*UPARAM(IADB+8) 
     .         + UPARAM(IADB+4)*UPARAM(IADB+9)
     .         + UPARAM(IADB+5)*UPARAM(IADB+10) 
           RHO  = PM(1,IMAT)  
           A11 = GMAX*(ONE + NU)/(ONE - NU**2)                                  
           SSP(I)=MAX(SSP(I), SQRT(A11/RHO))     
        ENDDO
      ELSEIF (MTN == 65) THEN
        DO I=LFT,LLT
          RHO   =PM(1,IMAT)
          YOUNG =PM(20,IMAT)
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
C
      DO 20 I=LFT,LLT
          AL1(I)= X2L(I)       * X2L(I)       + Y2L(I)       * Y2L(I)
          AL2(I)=(X3L(I)-X2L(I))*(X3L(I)-X2L(I))+(Y3L(I)-Y2L(I))*(Y3L(I)-Y2L(I))
          AL3(I)=(X4L(I)-X3L(I))*(X4L(I)-X3L(I))+(Y4L(I)-Y3L(I))*(Y4L(I)-Y3L(I))
          AL4(I)= X4L(I)       * X4L(I)       + Y4L(I)       * Y4L(I)
          AL5(I)=(X4L(I)-X2L(I))*(X4L(I)-X2L(I))+(Y4L(I)-Y2L(I))*(Y4L(I)-Y2L(I))
          AL6(I)= X3L(I)       * X3L(I)       + Y3L(I)       * Y3L(I)
   20 CONTINUE
C
      DO 30 I=LFT,LLT
          AL(I)= MIN(AL1(I),AL2(I),AL3(I),AL4(I),AL5(I),AL6(I))
          IF(AL3(I) == ZERO) AL(I)= MIN(AL1(I),AL2(I),AL4(I))
          ALMIN(I)=SQRT(AL(I))
   30 CONTINUE
C
      IF(MTN == 19)THEN
          VISCDEF=FOURTH
      ELSEIF(MTN == 25.OR.MTN == 27)THEN
          VISCDEF=FIVEEM2
      ELSE
          VISCDEF=ZERO
      ENDIF
C       
       VISCMX = GROUP_PARAM%VISC_DM
       IF (VISCMX == ZERO) VISCMX = VISCDEF
       IF (MTN == 1 .OR.MTN == 2.OR.MTN == 3.OR.
     .     MTN == 22.OR.MTN == 23) VISCMX=ZERO
       VISCMX  = SQRT(ONE + VISCMX*VISCMX) - VISCMX
       DO I=LFT,LLT
            DTDYN   = AREA(I)/SQRT(MAX(AL5(I),AL6(I)))
            ALDT(I) = MAX(DTDYN,ALMIN(I))
            DT(I)   = ALDT(I)*VISCMX/SSP(I)
      ENDDO
C----------------------------------------------------------
C     DT NODAL
C----------------------------------------------------------     
      IPGMAT = 700 
      IF(NADMESH == 0)THEN
        IF (IGTYP == 11 .AND. IGMAT > 0) THEN
          DO I=LFT,LLT
               A11  =GEO(IPGMAT + 5,IPROP)
               A11R =GEO(IPGMAT + 7,IPROP)
               B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
               B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
               VV = VISCMX * VISCMX
               FAC = MAX(B1,B2) / (AREA(I) * VV)
               STI =  FAC * THK(I) * A11
               STIR = FAC*A11R * THK(I)*(THK(I)**2 +  AREA(I))*ONE_OVER_12
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
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0 )) THEN 
         DO I=LFT,LLT
               A11  = PM_STACK(5 ,ISUBSTACK)
               A11R = PM_STACK(7 ,ISUBSTACK)
                B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
               B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
               VV = VISCMX * VISCMX
               FAC = MAX(B1,B2) / (AREA(I) * VV)
               STI =  FAC * THK(I) * A11
               STIR = FAC*A11R * THK(I)*(THK(I)**2 +  AREA(I))*ONE_OVER_12
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
            B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
            B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
            VV = VISCMX * VISCMX
            STI =  MAX(B1,B2)
     .           * THK(I) * A11 / (AREA(I) * VV)
            STIR = STI * (THK(I)*THK(I) + AREA(I)) / 12.
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
               A11  =GEO(IPGMAT + 5,IPROP)
               A11R =GEO(IPGMAT + 7,IPROP)
               B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
               B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
               VV = VISCMX * VISCMX
               FAC = MAX(B1,B2) / (AREA(I) * VV)
               STI =  FAC * THK(I) * A11
               STIR = FAC * A11R * THK(I)*(THK(I)**2 +  AREA(I))*ONE_OVER_12
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
     .         ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0 )) THEN 
           DO I=LFT,LLT
             N=NFT+I
             IF(SH4TREE(3,N) >= 0)THEN
               A11  = PM_STACK(5 ,ISUBSTACK)
               A11R = PM_STACK(7 ,ISUBSTACK)
               B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
               B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
               VV = VISCMX * VISCMX
               FAC = MAX(B1,B2) / (AREA(I) * VV)
               STI =  FAC * THK(I) * A11
               STIR = FAC * A11R * THK(I)*(THK(I)**2 +  AREA(I))*ONE_OVER_12
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
              B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
              B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
              VV = VISCMX * VISCMX
              STI =  MAX(B1,B2)
     .             * THK(I) * A11 / (AREA(I) * VV)
              STIR = STI * (THK(I)*THK(I) + AREA(I)) / 12.
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
      IF(ISMSTR == 3)THEN
        DO I=LFT,LLT
         IF(GEO(5,IPROP)/=ZERO)GEO(5,IPROP)= MIN(GEO(5,IPROP),DT(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
	         PX1(I)= ZERO
          PX2(I)= ZERO
          PY1(I)= ZERO
          PY2(I)= ZERO
        ENDDO
      ENDIF
C
      RETURN
      END

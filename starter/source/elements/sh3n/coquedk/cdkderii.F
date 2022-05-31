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
Chd|  CDKDERII                      source/elements/sh3n/coquedk/cdkderii.F
Chd|-- called by -----------
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE CDKDERII(JFT   ,JLT,PM ,GEO ,PX2,PY2,PX3,PY3,
     .                    STIFN ,STIFR  ,IXTG ,THK ,SH3TREE ,
     .                    ALDT  ,UPARAM ,IPM  ,IGEO,PM_STACK,
     .                    ISUBSTACK,STRTG,GROUP_PARAM, 
     .                    IMAT,IPROP,AREA,   DT  ,
     .                    X1G ,X2G ,X3G ,Y1G ,Y2G ,Y3G ,
     .                    Z1G ,Z2G ,Z3G ,E1X ,E2X ,E3X ,
     .                    E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
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
#include      "scr03_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ILEV,ISUBSTACK,IMAT,IPROP
      INTEGER IXTG(NIXTG,*), SH3TREE(KSH3TREE,*),IPM(NPROPMI,*),
     .        IGEO(NPROPGI,*),PM_STACK(20,*)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), PX2(*),PX3(*),PY2(*),PY3(*),
     .   STIFN(*),STIFR(*),THK(*),ALDT(*),UPARAM(*),STRTG(*),
     .   X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ),
     .   Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ),
     .   Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
      TYPE (GRP_PARAM_STRUCT_)  :: GROUP_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NG, N,IADB,I1,I3,IPTHK,IPPOS,IGTYP,I2,
     .        MATLY,IGMAT,IPGMAT,IPOS
      my_real
     .   X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .   X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ),
     .   X32G(MVSIZ), Y32G(MVSIZ), Z32G(MVSIZ),
     .   X2(MVSIZ), X3(MVSIZ), Y2(MVSIZ),Y3(MVSIZ),
     .   DT(MVSIZ), AREA(MVSIZ),FAC, ALMIN, 
     .   VISCMX, A11, G, STI,STIR,SHF,VISCDEF,BULK,GMAX,
     .   AL1, AL2, AL3, ALMAX, SSP,YOUNG,NU,RHO,AREAI,
     .   C1,IZ,THICKT,THKLY,POSLY,A1THK,C1THK,
     .   GTHK,A11R,A12,E,ETHK,NUTHK,A12THK,RHOG
C=======================================================================
      DO I=JFT,JLT
        X21G(I) = X2G(I)-X1G(I)
        Y21G(I) = Y2G(I)-Y1G(I)
        Z21G(I) = Z2G(I)-Z1G(I)
        X31G(I) = X3G(I)-X1G(I)
        Y31G(I) = Y3G(I)-Y1G(I)
        Z31G(I) = Z3G(I)-Z1G(I)
        X32G(I) = X3G(I)-X2G(I)
        Y32G(I) = Y3G(I)-Y2G(I)
        Z32G(I) = Z3G(I)-Z2G(I)
      ENDDO
c
      DO I=JFT,JLT
        X2(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
        Y2(I)=E2X(I)*X21G(I)+E2Y(I)*Y21G(I)+E2Z(I)*Z21G(I)
        Y3(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
        X3(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
      ENDDO
C       
C global material
C
      IGTYP = IGEO(11,IPROP) 
      IGMAT = IGEO(98,IPROP) 
      IPGMAT = 700 
C      
      IF(MTN == 19)THEN
          VISCDEF=FOURTH
      ELSEIF(MTN == 25.OR.MTN == 27)THEN
          VISCDEF=FIVEEM2
      ELSE
          VISCDEF=ZERO
      ENDIF
c
      DO 40 I=JFT,JLT
        AL1 = X2(I) * X2(I) + Y2(I) * Y2(I)     
        AL2 = (X3(I)-X2(I)) * (X3(I)-X2(I)) + 
     .        (Y3(I)-Y2(I)) * (Y3(I)-Y2(I))
        AL3 = X3(I) * X3(I) + Y3(I) * Y3(I)
        ALMAX = MAX(AL1,AL2,AL3)
        NU   =PM(21,IMAT)
        ALMIN = MIN(AL1,AL2,AL3)
        FAC =ONE+ZEP6*(1+NU)*THK(I)*THK(I)/ALMIN
        ALMAX = ALMAX*FAC
        IF(IGTYP == 11 .AND. IGMAT > 0) THEN
          SSP = GEO(IPGMAT +9 ,IPROP)
        ELSEIF(IGTYP == 52 .OR. 
     .       ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
          SSP = PM_STACK(9 ,ISUBSTACK)
        ELSE
          IF(MTN.LE.28)THEN
          SSP=PM(27,IMAT)
         ELSEIF (MTN == 42 .OR. MTN  == 69) THEN
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
          SSP  = MAX(SSP, SQRT(A11/RHO))
         ELSEIF (MTN == 65) THEN
          RHO  =PM(1,IMAT)
          YOUNG=PM(20,IMAT)
          SSP=SQRT(YOUNG/RHO)
         ELSE
          RHO  =PM(1,IMAT)
          YOUNG=PM(20,IMAT)
          NU   =PM(21,IMAT)
          SSP=SQRT(YOUNG/(ONE-NU*NU)/RHO)
         ENDIF
        ENDIF 
        VISCMX = GROUP_PARAM%VISC_DM
        IF (VISCMX == ZERO) VISCMX = VISCDEF
        IF(MTN == 1.OR.MTN == 2.OR.MTN == 3.OR.
     .       MTN == 22.OR.MTN == 23)VISCMX=ZERO
        VISCMX=SQRT(1.+VISCMX*VISCMX)-VISCMX
        ALDT(I)= TWO*AREA(I)*VISCMX / SQRT(ALMAX)
        DT(I) = ALDT(I) / SSP
   40 CONTINUE
C-----------------    
C     DT NODAL
C-----------------         
      IPGMAT = 700    
      IF(NADMESH==0)THEN
       IF(IGTYP == 11 .AND. IGMAT > 0) THEN
           DO I=JFT,JLT
            A11  = GEO(IPGMAT +  5 ,IPROP)
            A11R = GEO(IPGMAT +  7 ,IPROP) 
            G   =  GEO(IPGMAT +  4 ,IPROP)
            FAC = AREA(I)* THK(I) / (ALDT(I))**2
            STI = FAC * A11 
            STIR =ONE_OVER_12*FAC* A11R*THK(I)**2
            STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
            STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
            STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
            STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
            STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
            STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
            STRTG(I) = STIR
           END DO
       ELSEIF(IGTYP == 52 .OR. 
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
           DO I=JFT,JLT
            A11  = PM_STACK(5 ,ISUBSTACK)
            A11R = PM_STACK(7 ,ISUBSTACK)
            G    = PM_STACK(4 ,ISUBSTACK)
            FAC = AREA(I)* THK(I) / (ALDT(I))**2
            STI = FAC * A11 
            STIR =ONE_OVER_12*FAC* A11R*THK(I)**2
            STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
            STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
            STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
            STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
            STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
            STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
            STRTG(I) = STIR
           END DO
       ELSE
           DO I=JFT,JLT
            A11 =GEO(IPGMAT +5 ,IPROP)
            A11R =GEO(IPGMAT +7 ,IPROP) 
            G   =GEO(IPGMAT +4 ,IPROP)
            FAC  =AREA(I)* THK(I) / (ALDT(I))**2
            STI  =FAC*  A11 
            STIR =ONE_OVER_12*FAC* A11R*THK(I)**2 
            STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
            STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
            STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
            STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
            STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
            STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
            STRTG(I) = STIR
           END DO
       ENDIF 
      ELSE
       IF(IGTYP == 11 .AND. IGMAT > 0 )THEN
            DO I=JFT,JLT
             N=NFT+I
             IF(SH3TREE(3,N) >= 0)THEN
              A11  =GEO(IPGMAT +5 ,IPROP)
              A11R =GEO(IPGMAT +7 ,IPROP) 
              G    =GEO(IPGMAT +4 ,IPROP)
!!              STI = AREA(I) * THK(I) * A11 / (ALDT(I))**2
!!              STIR = STI * THK(I) * THK(I) / 12.
              FAC  =AREA(I)* THK(I) / (ALDT(I))**2
              STI  =FAC*  A11 
              STIR =ONE_OVER_12*FAC* A11R*THK(I)**2  
              STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
              STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
              STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
              STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
              STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
              STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
              STRTG(I) = STIR
             END IF
            END DO
          ELSEIF(IGTYP == 52 .OR. 
     .         ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
            DO I=JFT,JLT
             N=NFT+I
             IF(SH3TREE(3,N) >= 0)THEN
              A11  = PM_STACK(5 ,ISUBSTACK)
              A11R = PM_STACK(7 ,ISUBSTACK)
              G    = PM_STACK(4 ,ISUBSTACK)
              FAC  =AREA(I)* THK(I) / (ALDT(I))**2
              STI  =FAC*  A11 
              STIR =ONE_OVER_12*FAC* A11R*THK(I)**2  
              STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
              STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
              STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
              STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
              STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
              STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
              STRTG(I) = STIR
             END IF
            END DO  
          ELSE
             DO I=JFT,JLT
             N=NFT+I
             IF(SH3TREE(3,N) >= 0)THEN
              A11 =PM(24,IMAT)
              G   =PM(22,IMAT)
              STI = AREA(I) * THK(I) * A11 / (ALDT(I))**2
              STIR = STI * THK(I) * THK(I) / 12. 
              STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
              STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
              STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
              STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
              STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
              STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
              STRTG(I) = STIR
             END IF
            END DO
          ENDIF   
      END IF
C
C---------------------------------------------------------
      IF(ISMSTR.NE.3)THEN
       DO 50 I=JFT,JLT
        PX2(I) = ZERO
        PY2(I) = ZERO
        PX3(I) = ZERO
        PY3(I) = ZERO
 50    CONTINUE
      ELSE
C---------------------------------------------------------
C
       DO I=JFT,JLT
        AREAI=HALF/AREA(I)
        PX2(I)=Y3(I)*AREAI
        PY2(I)=-X3(I)*AREAI
        PX3(I)=-Y2(I)*AREAI
        PY3(I)=X2(I)*AREAI
       ENDDO
C
       DO 80 I=JFT,JLT
         NG=IPROP
         IF (GEO(5,NG) == ZERO) GOTO 80
         GEO(5,NG)= MIN(GEO(5,NG),DT(I))
 80    CONTINUE
      ENDIF
C
C---------------------------------------------------------
      RETURN
C
      END

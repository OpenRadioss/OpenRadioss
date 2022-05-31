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
Chd|  C3DERII                       source/elements/sh3n/coque3n/c3derii.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE C3DERII(JFT,JLT,PM    ,GEO  ,PX1,
     .                   PY1,PY2,STIFN ,STIFR,IXTG,
     .                   THK,SH3TREE   ,ALDT ,UPARAM  ,IPM ,IGEO,
     .                   PM_STACK, ISUBSTACK,STRTG,IMAT,IPROP,
     .                   AREA ,DT  ,X31G,Y31G,Z31G,
     .                   E1X  ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,
     .                   E1Z  ,E2Z ,E3Z ,X2  ,X3  ,Y3  ,
     .                   GROUP_PARAM)
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
     .         IGEO(NPROPGI,*)
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), PX1(*),PY1(*),PY2(*),
     .   STIFN(*),STIFR(*),THK(*),ALDT(*),UPARAM(*),PM_STACK(20,*),STRTG(*)
      my_real AREA(MVSIZ),DT(MVSIZ),
     .     X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ),
     .     X2(MVSIZ), X3(MVSIZ), Y3(MVSIZ),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
      TYPE (GRP_PARAM_STRUCT_)  :: GROUP_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IADB,I1,I3,IPTHK,IPPOS,IGTYP,I2,
     .        IPMAT,MATLY,IPGMAT,IGMAT,IPOS
      my_real ALDTV(MVSIZ)
      my_real VISCMX, A11, G, STI,STIR,SHF,VISCDEF,
     .     AL1, AL2, AL3, ALMAX, SSP,YOUNG,NU,RHO,BULK,GMAX,
     .     C1,IZ,THICKT,THKLY,POSLY,A1THK,C1THK,
     .     GTHK,A11R,FAC,A12,E,ETHK,NUTHK,A12THK,RHOG
C=======================================================================
      SSP = ZERO 
      
      IGTYP = IGEO(11,IPROP)
      IGMAT = IGEO(98,IPROP) ! global material
      IPGMAT = 700

      DO I=JFT,JLT
        Y3(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
        X3(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
      ENDDO
C
      IF(MTN.EQ.19)THEN
         VISCDEF=FOURTH
       ELSEIF(MTN.EQ.25.OR.MTN.EQ.27)THEN
          VISCDEF=FIVEEM2
      ELSE
           VISCDEF=ZERO
      ENDIF
      DO 40 I=JFT,JLT
        AL1 = X2(I) * X2(I)      
        AL2 = (X3(I)-X2(I)) * (X3(I)-X2(I)) + Y3(I) * Y3(I)
        AL3 = X3(I) * X3(I) + Y3(I) * Y3(I)
        ALMAX = MAX(AL1,AL2,AL3)
        IF(IGTYP == 11 .AND. IGMAT > 0) THEN
          SSP  = GEO(IPGMAT +9 ,IPROP)
        ELSEIF(IGTYP == 52 .OR. 
     .       ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0 )) THEN
           SSP = PM_STACK(9 ,ISUBSTACK)
        ELSE
         IF (MTN.LE.28) THEN
          SSP=PM(27,IMAT)
         ELSEIF (MTN == 42 .OR. MTN  == 69 ) THEN
          IADB = IPM(7,IMAT)-1                                               
          BULK = UPARAM(IADB+11) 
          NU   = UPARAM(IADB+14)                             
          GMAX = UPARAM(IADB+1)*UPARAM(IADB+6)                 
     .         + UPARAM(IADB+2)*UPARAM(IADB+7)
     .         + UPARAM(IADB+3)*UPARAM(IADB+8) 
     .         + UPARAM(IADB+4)*UPARAM(IADB+9) 
     .         + UPARAM(IADB+5)*UPARAM(IADB+10) 
          RHO  = PM(1,IMAT)      
          A11 = GMAX*(ONE + NU)/(ONE - NU**2)                             
          SSP=MAX(SSP, SQRT(A11/RHO))     
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
        IF(MTN.EQ.1.OR.MTN.EQ.2.OR.MTN.EQ.3.OR.
     .       MTN.EQ.22.OR.MTN.EQ.23) VISCMX=ZERO
        VISCMX=SQRT(ONE+VISCMX*VISCMX)-VISCMX
        ALDT(I)= TWO*AREA(I) / SQRT(ALMAX)
        ALDTV(I)= ALDT(I)*VISCMX
        DT(I) = ALDTV(I) / SSP
  40  CONTINUE
C-------------------------
C     DT NODAL
C-------------------------
      IPGMAT = 700 
      IF(NADMESH.EQ.0)THEN
       IF(IGTYP == 11 .AND. IGMAT > 0) THEN
         VISCMX = GROUP_PARAM%VISC_DM
         DO I=JFT,JLT
           G    =GEO(IPGMAT  +4,IMAT)
           A11  =GEO(IPGMAT + 5,IPROP)
           A11R =GEO(IPGMAT + 7,IPROP)
           SHF=1.
           FAC= AREA(I) / (ALDTV(I))**2
           STI = FAC* THK(I) * A11
           STIR = FAC*A11R* (ONE_OVER_12*THK(I)**3
     .               + HALF * SHF * AREA(I) * G/A11) 
           STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
           STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
           STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
           STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
           STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
           STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
           STRTG(I) = STIR
          END DO
       ELSEIF(IGTYP == 52 .OR. 
     .      ((IGTYP == 17 .OR. IGTYP == 51 ).AND. IGMAT > 0 )) THEN
         VISCMX = GROUP_PARAM%VISC_DM
         DO I=JFT,JLT
           G    = PM_STACK(4 ,ISUBSTACK)
           A11  = PM_STACK(5 ,ISUBSTACK)
           A11R = PM_STACK(7 ,ISUBSTACK)
           SHF=1.
           FAC= AREA(I) / (ALDTV(I))**2
           STI = FAC* THK(I) * A11
           STIR = FAC*A11R* (ONE_OVER_12*THK(I)**3
     .               + HALF * SHF * AREA(I) * G/A11) 
           STIFN(IXTG(2,I))=STIFN(IXTG(2,I))+STI
           STIFN(IXTG(3,I))=STIFN(IXTG(3,I))+STI
           STIFN(IXTG(4,I))=STIFN(IXTG(4,I))+STI
           STIFR(IXTG(2,I))=STIFR(IXTG(2,I))+STIR
           STIFR(IXTG(3,I))=STIFR(IXTG(3,I))+STIR
           STIFR(IXTG(4,I))=STIFR(IXTG(4,I))+STIR
           STRTG(I) = STIR
          END DO  
       ELSE
          VISCMX = GROUP_PARAM%VISC_DM
          DO I=JFT,JLT
           A11 =PM(24,IMAT)
           G   =PM(22,IMAT)
           SHF=1.
           STI = AREA(I) * THK(I) * A11 / (ALDTV(I))**2
           STIR = STI * (THK(I) * THK(I) * ONE_OVER_12
     .               + HALF * SHF * AREA(I) * G/A11) 
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
      IF(IGTYP == 11 .AND. IGMAT > 0) THEN
        VISCMX = GROUP_PARAM%VISC_DM
        DO I=JFT,JLT
          N=NFT+I
          IF(SH3TREE(3,N) >= 0)THEN
           G    =GEO(IPGMAT  +4,IMAT)
           A11  =GEO(IPGMAT + 5,IPROP)
           A11R =GEO(IPGMAT + 7,IPROP)
           SHF=1.
           FAC= AREA(I) / (ALDTV(I))**2
           STI = FAC* THK(I) * A11
           STIR = FAC*A11R* (ONE_OVER_12*THK(I)**3
     .               + HALF * SHF * AREA(I) * G/A11) 
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
     . ((IGTYP == 17 .OR. IGTYP == 51 ).AND. IGMAT > 0 ) ) THEN
        VISCMX = GROUP_PARAM%VISC_DM
        DO I=JFT,JLT
          N=NFT+I
          IF(SH3TREE(3,N) >= 0)THEN
          
           G    = PM_STACK(4 ,ISUBSTACK)
           A11  = PM_STACK(5 ,ISUBSTACK)
           A11R = PM_STACK(7 ,ISUBSTACK)
           SHF=1.
           FAC= AREA(I) / (ALDTV(I))**2
           STI = FAC* THK(I) * A11
           STIR = FAC*A11R* (ONE_OVER_12*THK(I)**3
     .               + HALF * SHF * AREA(I) * G/A11) 
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
         VISCMX = GROUP_PARAM%VISC_DM
         DO I=JFT,JLT
          N=NFT+I
          IF(SH3TREE(3,N) >= 0)THEN
           A11 =PM(24,IMAT)
           G   =PM(22,IMAT)
           SHF=1.
           STI = AREA(I) * THK(I) * A11 / (ALDTV(I))**2
           STIR = STI * (THK(I) * THK(I) * ONE_OVER_12
     .               + HALF * SHF * AREA(I) * G/A11) 
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
       PX1(I) = ZERO
       PY1(I) = ZERO
       PY2(I) = ZERO
 50    CONTINUE
      ELSE
C---------------------------------------------------------
C
       DO I=JFT,JLT
        PX1(I) = -HALF*Y3(I)
        PY1(I) = HALF*(X3(I)-X2(I))
        PY2(I) = -HALF*X3(I)
       ENDDO
C
       DO 80 I=JFT,JLT
        IF(GEO(5,IPROP).EQ.ZERO)GOTO 80
         GEO(5,IPROP)= MIN(GEO(5,IPROP),DT(I))
 80    CONTINUE
      ENDIF
C
C---------------------------------------------------------
      RETURN
C
      END

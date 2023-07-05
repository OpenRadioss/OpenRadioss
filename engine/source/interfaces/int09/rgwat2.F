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
Chd|  RGWAT2                        source/interfaces/int09/rgwat2.F
Chd|-- called by -----------
Chd|        RGWATH                        source/interfaces/int09/rgwath.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_EXTAG                    source/mpi/fluid/spmd_cfd.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE RGWAT2(
     1               X        ,NELW ,NE      ,IXQ    ,
     4               ELBUF_TAB,IPARG,PM      ,NTAG   ,TEMP   ,
     5               TSTIF    ,E    ,IAD_ELEM,FR_ELEM        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NE
      INTEGER IPARG(NPARG,*), NELW(*) ,IXQ(NIXQ,*),
     .  NTAG(*), IAD_ELEM(2,*), FR_ELEM(*)
      my_real
     .  PM(NPROPM,*), X(3,*),E(*),
     .  TEMP,TSTIF
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N1, N2, IE, NG, MAT, IFA, LENR, 
     .        IFACE(2,4)
      my_real
     .  Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4,
     .  NY, NZ, DY, DZ, DD, GRAD, PHI, TEMPE, VOL,
     .  TSTIFE, COEF,EE
      INTEGER :: LLT ,NFT ,MTN ,IAD ,ITY ,NPT ,JALE ,ISMSTR ,JEUL ,JTUR ,JTHE ,JLAG ,JMULT ,JHBE
      INTEGER :: JIVF, NVAUX, JPOR, JCVT, JCLOSE, JPLASOL, IREP, IINT, IGTYP
      INTEGER :: ISORTH, ISORTHG, ISRAT, ISROT, ICSEN, IFAILURE, JSMS


      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
c---
      DATA IFACE/ 2, 3, 3, 4, 4, 5, 5, 2/
C----------------------
C     DECOMPTE DES ELEMENTS PAR NOEUD
C     POUR ENERGIE DE FROTTEMENT
C----------------------
      DO 100 IE=1,NE
        II  = NELW(IE)/10
        IFA = NELW(IE) - 10*II
        N1  = IXQ(IFACE(1,IFA),II)
        N2  = IXQ(IFACE(2,IFA),II)
        IF(NTAG(N1)>0) NTAG(N1) = NTAG(N1) + 1
        IF(NTAG(N2)>0) NTAG(N2) = NTAG(N2) + 1
 100  CONTINUE
C
C Comm SPMD NTAG : cumul aux points frontiere + prise en compte tag initial
C
      IF(IMACH==3.AND.NSPMD>1)THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXTAG(NTAG,IAD_ELEM,FR_ELEM,LENR)
      END IF
C----------------------
C     PONT THERMIQUE
C----------------------
      DO 600 IE=1,NE
        II  = NELW(IE)/10
        IFA = NELW(IE)-10*II
        N1  = IXQ(IFACE(1,IFA),II)
        N2  = IXQ(IFACE(2,IFA),II)
        IF(NTAG(N1)+NTAG(N2)>0)THEN
C---------------------------------
C         RECHERCHE DE L'ELEMENT DANS LE BUFFER
C---------------------------------
          DO 200 NG=II/NVSIZ,NGROUP
            CALL INITBUF(IPARG    ,NG      ,                   
     2            MTN     ,LLT     ,NFT     ,IAD     ,ITY     , 
     3            NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    , 
     4            JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    , 
     5            NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL , 
     6            IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   , 
     7            ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            IF(ITY/=2)          GO TO 200
            IF(II>NFT+LLT)     GO TO 200
            IF(IPARG(8,NG)==1)  GO TO 600
            IF(JTHE/=1)         GO TO 600
            I = II - NFT
            GOTO 250
  200     CONTINUE
  250     CONTINUE
c          
          GBUF => ELBUF_TAB(NG)%GBUF
c
          VOL  = GBUF%VOL(I)
          TEMPE= GBUF%TEMP(I)
C
          EE  = ZERO
          PHI = ZERO
C----------------------
C     ENERGIE DE FROTTEMENT
C----------------------
          IF(NTAG(N1)>1) EE = EE + E(N1) / (NTAG(N1)-1)
          IF(NTAG(N2)>1) EE = EE + E(N2) / (NTAG(N2)-1)
C----------------------
C     CONDUCTION
C----------------------
            Y1=X(2,N1)
            Z1=X(3,N1)
C
            Y2=X(2,N2)
            Z2=X(3,N2)
C------------------------------------------
C         CALCUL DE LA SURFACE VECTORIELLE 
C------------------------------------------
            NY= (Z2-Z1)
            NZ=-(Y2-Y1)
C--------+---------+---------+---------+---------+---------+---------+--
C         CALCUL DE LA DISTANCE ENTRE CENTRE ET SURFACE ( * 4. )
C-------------------------------------------------------------
            DY = TWO*(Y1 + Y2)
     .          -X(2,IXQ(2,II))-X(2,IXQ(3,II))
     .          -X(2,IXQ(4,II))-X(2,IXQ(5,II))
C
            DZ = TWO*(Z1 + Z2)
     .          -X(3,IXQ(2,II))-X(3,IXQ(3,II))
     .          -X(3,IXQ(4,II))-X(3,IXQ(5,II))
C
            DD= DY**2+DZ**2
C---------------------------------
C         CALCUL DU GRADIENT * SURFACE
C---------------------------------
            GRAD = FOUR*(DY*NY+DZ*NZ) / MAX(EM15,DD)
            MAT  =IXQ(1,IE)
            IF(TEMPE<=PM(80,MAT))THEN
             COEF=PM(75,MAT)+PM(76,MAT)*TEMPE
            ELSE
             COEF=PM(77,MAT)+PM(78,MAT)*TEMPE
            ENDIF
            TSTIFE = COEF * GRAD
C---------------------------------
C         CALCUL DU FLUX
C---------------------------------
            PHI = TSTIFE*TSTIF*(TEMP-TEMPE)
     2           / MAX(EM20,(TSTIFE+TSTIF))
            PHI = PHI * DT1
     +          * ( MIN(NTAG(N1),1) + MIN(NTAG(N2),1) )
     +          / TWO
C---------------------------------
C         ENERGIE / VOLUME
C---------------------------------
          PHI = (PHI + EE) / MAX(VOL,EM20)
          GBUF%EINT(I) = GBUF%EINT(I) + PHI
        ENDIF
 600  CONTINUE
C
      RETURN
      END

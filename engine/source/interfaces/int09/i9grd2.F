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
Chd|  I9GRD2                        source/interfaces/int09/i9grd2.F
Chd|-- called by -----------
Chd|        I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE I9GRD2(IERR  ,AREA  ,TSTIF    ,T      ,VOL     ,
     2                  II    ,X     ,IXQ      ,IX     ,
     3                  IPARG ,PM    ,ELBUF_TAB,IGROU  ,IELN    )
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
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER II, IGROU, IELN, IERR, IX(4), IXQ(NIXQ),IPARG(NPARG,*) 
C     REAL
      my_real
     .   DIST, AREA, TSTIF, T, VOL, X(3,*), PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N1, N2, N3, N4, IE, NG,MAT, IFA
      my_real
     .  Y1, Y2, Z1, Z2,NY, NZ, DY, DZ,NORM,COND
C-----------------------------------------------
      IERR = 0
C---------------------------------
C         RECHERCHE DE L'ELEMENT DANS LE BUFFER
C---------------------------------
          DO 200 NG=1,NGROUP
            CALL INITBUF (IPARG    ,NG      ,                  
     2            MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3            NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4            JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5            NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6            IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7            ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            IF(ITY/=2)          GO TO 200
            IF(II>NFT+LLT)     GO TO 200
            IF(IPARG(8,NG)==1.OR.JTHE/=1)THEN
              IERR = 1
              RETURN
            ENDIF
            I = II - NFT
            GOTO 250
  200     CONTINUE
              IERR = 1
              RETURN
  250     CONTINUE

          IGROU = NG
          IELN  = I
          VOL   = ELBUF_TAB(NG)%GBUF%VOL(I)
C----------------------
C     CONDUCTION
C----------------------
            N1=IX(1)
            N2=IX(2)
C
            Y1=X(2,N1)
            Z1=X(3,N1)
C
            Y2=X(2,N2)
            Z2=X(3,N2)
C
C------------------------------------------
C         CALCUL DE LA SURFACE VECTORIELLE (*1.)
C------------------------------------------
            NY= (Z2-Z1)
            NZ=-(Y2-Y1)
            NORM = SQRT(NY**2 + NZ**2)
C--------+---------+---------+---------+---------+---------+---------+--
C         CALCUL DE LA DISTANCE ENTRE CENTRE ET SURFACE ( * 4. )
C-------------------------------------------------------------
            DY = TWO*(Y1 + Y2)
     .          -X(2,IXQ(2))-X(2,IXQ(3))
     .          -X(2,IXQ(4))-X(2,IXQ(5))
C
            DZ = TWO*(Z1 + Z2)
     .          -X(3,IXQ(2))-X(3,IXQ(3))
     .          -X(3,IXQ(4))-X(3,IXQ(5))
C----------------------------------------------------------
C         CALCUL DISTANCE ET 1/2 SURFACE(SURFACE NODALE))
C----------------------------------------------------------
            DIST = FOURTH*(DY*NY+DZ*NZ) / MAX(EM15,NORM) 
            AREA = HALF*NORM
C--------------------------------------------
C         CALCUL DE LA RESISTANCE THERMIQUE
C--------------------------------------------
            T = ELBUF_TAB(NG)%GBUF%TEMP(I)
            MAT  =IXQ(1)
            IF(T<=PM(80,MAT))THEN
             COND=PM(75,MAT)+PM(76,MAT)*T
            ELSE
             COND=PM(77,MAT)+PM(78,MAT)*T
            ENDIF
            TSTIF = DIST / COND
C
 600  CONTINUE
C
      RETURN
      END

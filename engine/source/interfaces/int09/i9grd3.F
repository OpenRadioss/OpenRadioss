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
Chd|  I9GRD3                        source/interfaces/int09/i9grd3.F
Chd|-- called by -----------
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE I9GRD3(IERR  ,AREA  ,TSTIF    ,T      ,VOL     ,
     2                  II    ,X     ,IXS      ,IX     ,
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER II, IGROU, IELN , IERR, IX(4), IXS(NIXS),IPARG(NPARG,NGROUP) 
      my_real
     .   AREA, TSTIF, T, VOL, X(3,NUMNOD), PM(NPROPM,NUMMAT)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N1, N2, N3, N4, IE, NG,MAT, IFA
      my_real
     .  X1, X2, X3, X4, Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4,
     .  NX, NY, NZ, DX, DY, DZ, NORM, DIST, COND
C-----------------------------------------------
      IERR = 0
C---------------------------------
C         RECHERCHE DE L'ELEMENT DANS LE BUFFER
C---------------------------------
          DO 200 NG=II/NVSIZ+1,NGROUP
          CALL INITBUF(IPARG    ,NG      ,                    
     2          MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            IF(ITY/=1)          GO TO 200
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
            N3=IX(3)
            N4=IX(4)
C
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
C
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
C
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
C
            X4=X(1,N4)
            Y4=X(2,N4)
            Z4=X(3,N4)
C------------------------------------------
C         CALCUL DE LA SURFACE VECTORIELLE (*2.)
C------------------------------------------
            NX=(Y1-Y3)*(Z2-Z4) - (Z1-Z3)*(Y2-Y4)
            NY=(Z1-Z3)*(X2-X4) - (X1-X3)*(Z2-Z4)
            NZ=(X1-X3)*(Y2-Y4) - (Y1-Y3)*(X2-X4)
            NORM = SQRT(NX**2 + NY**2 + NZ**2)
C--------+---------+---------+---------+---------+---------+---------+--
C         CALCUL DE LA DISTANCE ENTRE CENTRE ET SURFACE ( * 8. )
C-------------------------------------------------------------
            DX = TWO*(X1 + X2 + X3 + X4)
     .          -X(1,IXS(2))-X(1,IXS(3))
     .          -X(1,IXS(4))-X(1,IXS(5))
     .          -X(1,IXS(6))-X(1,IXS(7))
     .          -X(1,IXS(8))-X(1,IXS(9))
C
            DY = TWO*(Y1 + Y2 + Y3 + Y4)
     .          -X(2,IXS(2))-X(2,IXS(3))
     .          -X(2,IXS(4))-X(2,IXS(5))
     .          -X(2,IXS(6))-X(2,IXS(7))
     .          -X(2,IXS(8))-X(2,IXS(9))
C
            DZ = TWO*(Z1 + Z2 + Z3 + Z4)
     .          -X(3,IXS(2))-X(3,IXS(3))
     .          -X(3,IXS(4))-X(3,IXS(5))
     .          -X(3,IXS(6))-X(3,IXS(7))
     .          -X(3,IXS(8))-X(3,IXS(9))
C
C---------------------------------
C         CALCUL DISTANCE ET 1/4 SURFACE(SURFFACE NODALE))
C---------------------------------
            DIST = ONE_OVER_8*(DX*NX+DY*NY+DZ*NZ) / MAX(EM15,NORM) 
            AREA = ONE_OVER_8*NORM
C---------------------------------
C         CALCUL DE LA CONDUCTIBILITE
C---------------------------------
            T = ELBUF_TAB(NG)%GBUF%TEMP(I)
            MAT  =IXS(1)
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

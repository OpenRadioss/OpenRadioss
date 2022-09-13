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
C======================================================================|
Chd|====================================================================
Chd|  INIT_INT                      source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_INT(I,J)
C !!!! I est reel a l'appel
      INTEGER I,J
      I = J
      RETURN
      END
C======================================================================|
Chd|====================================================================
Chd|  INIT_INTV                     source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_INTV(INTV, LEN)
#include      "implicit_f.inc"
      INTEGER LEN, INTV(*)
      INTEGER I
      DO I = 1,LEN
        INTV(I) = 0
      ENDDO
      RETURN
      END
C======================================================================|

Chd|====================================================================
Chd|  LTAG_BCS                      source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE LTAG_BCS(COMNTAG,NGRNOD,IGRNOD,IBCSLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSLAG(5,*),COMNTAG(*),NGRNOD
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IGR,NN
C======================================================================|
      DO I=1,NBCSLAG
        IGR = IBCSLAG(1,I)
        DO N=1,IGRNOD(IGR)%NENTITY
          NN=IGRNOD(IGR)%ENTITY(N)
          COMNTAG(NN) = COMNTAG(NN) + 1
        ENDDO
      ENDDO
C---
      RETURN
      END

Chd|====================================================================
Chd|  LTAG_I2MAIN                   source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        LTAG_I2                       source/tools/lagmul/lag_ntag.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LTAG_I2MAIN(COMNTAG ,IPARI   ,INTBUF_TAB   )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), COMNTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTY,ILAGM,NSN,NMN,NRTS,NRTM,NIN

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C======================================================================|
      DO NIN=1,NINTER
        IF (IPARI(33,NIN)==1.AND.IPARI(7,NIN)==2) THEN
          NRTS  = IPARI(3,NIN)
          NRTM  = IPARI(4,NIN)
          NSN   = IPARI(5,NIN)
          NMN   = IPARI(6,NIN)
          CALL LTAG_I2(INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,INTBUF_TAB(NIN)%IRTLM,COMNTAG,NSN  )
        ENDIF
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  LTAG_I2                       source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LTAG_I2MAIN                   source/tools/lagmul/lag_ntag.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LTAG_I2(IRECT    ,NSV      ,IRTL     ,COMNTAG, NSN    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IRECT(4,*), NSV(*), IRTL(*), COMNTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, JJ, L, NN, NIR, N4
C-----------------------------------------------
      DO II=1,NSN
        L  = IRTL(II)
        NN = NSV(II)
        COMNTAG(NN) = COMNTAG(NN) + 1
        NIR = 3
        DO JJ=1,3
          NN = IRECT(JJ,L)
          COMNTAG(NN) = COMNTAG(NN) + 1
        ENDDO
        N4 = IRECT(4,L)
        IF (N4/=IRECT(3,L)) COMNTAG(N4) = COMNTAG(N4) + 1
      ENDDO
C---
      RETURN
      END

Chd|====================================================================
Chd|  LTAG_FXV                      source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LTAG_FXV(COMNTAG ,IBFV   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*), COMNTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN
C======================================================================|
      DO N=1,NFXVEL
        IF (IBFV(8,N)/=0) THEN
          NN = IABS(IBFV(1,N))
          COMNTAG(NN) = COMNTAG(NN) + 1
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  LTAG_GJNT                     source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LTAG_GJNT(COMNTAG ,GJBUFI )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER GJBUFI(LKJNI,*), COMNTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JTYP,N0,N1,N2,N3
C======================================================================|
      DO I=1,NGJOINT
        JTYP= GJBUFI(2,I)        
        N0  = GJBUFI(3,I)        
        N1  = GJBUFI(4,I)        
        N2  = GJBUFI(5,I)
        N3  = GJBUFI(6,I)
        COMNTAG(N0) = COMNTAG(N0) + 1
        COMNTAG(N1) = COMNTAG(N1) + 1
        COMNTAG(N2) = COMNTAG(N2) + 1
        IF (JTYP==2) COMNTAG(N3) = COMNTAG(N3) + 1
      ENDDO
C---
      RETURN
      END
      
Chd|====================================================================
Chd|  LTAG_MPC                      source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LTAG_MPC(COMNTAG  ,IMPCNC   ,IMPCNN   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMPCNC(*), IMPCNN(*), COMNTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, KF, NN
C======================================================================|
      KF = 0
      DO I=1,NUMMPC
        DO J=1,IMPCNC(I)
          KF  = KF+1
          NN  = IMPCNN(KF)
          COMNTAG(NN) = COMNTAG(NN) + 1
        ENDDO
      ENDDO
C---
      RETURN
      END
      
Chd|====================================================================
Chd|  LTAG_RBY                      source/tools/lagmul/lag_ntag.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LTAG_RBY(COMNTAG, NPBYL  ,LPBYL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER COMNTAG(*), NPBYL(NNPBY,*), LPBYL(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NN,MSL,TNSL
C======================================================================|
      TNSL = 0
      DO N = 1,NRBYLAG
        MSL  = NPBYL(2,N)
        DO I=1,MSL
          NN = LPBYL(TNSL+I)
          COMNTAG(NN) = COMNTAG(NN) + 1
        ENDDO
        TNSL = TNSL + MSL*3
      ENDDO
C---
      RETURN
      END

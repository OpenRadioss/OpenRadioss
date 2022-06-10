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
Chd|  A4MASS3                       source/elements/solid/solide4/a4mass3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE A4MASS3(MS,RHO,
     .         VOLU,NC1,NC2,NC3,NC4,
     .         MSNF,OFF)
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   MS(*), RHO(*),VOLU(*),MSNF(*),OFF(*)
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .        DMASS(MVSIZ)
C-----------------------------------------------
      DO  I=LFT,LLT
        DMASS(I)=FOURTH*RHO(I)*VOLU(I)*OFF(I)
      ENDDO
C
#include "lockon.inc"
      DO I=LFT,LLT
       MS(NC1(I))=MS(NC1(I)) + DMASS(I)
       MSNF(NC1(I))=MSNF(NC1(I)) + DMASS(I)
      ENDDO
#include "lockoff.inc"
C
#include "lockon.inc"
      DO I=LFT,LLT
       MSNF(NC2(I))=MSNF(NC2(I)) + DMASS(I)
       MS(NC2(I))=MS(NC2(I)) + DMASS(I)
      ENDDO
#include "lockoff.inc"
C
#include "lockon.inc"
      DO I=LFT,LLT
       MS(NC3(I))=MS(NC3(I)) + DMASS(I)
       MSNF(NC3(I))=MSNF(NC3(I)) + DMASS(I)
      ENDDO
#include "lockoff.inc"
C
#include "lockon.inc"
      DO I=LFT,LLT
       MS(NC4(I))=MS(NC4(I)) + DMASS(I)
       MSNF(NC4(I))=MSNF(NC4(I)) + DMASS(I)
      ENDDO
#include "lockoff.inc"
C
C
      RETURN
      END
Chd|====================================================================
Chd|  A4MASS3P                      source/elements/solid/solide4/a4mass3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE A4MASS3P(FSKYM,RHO,VOLU,IADS,OFF)
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   FSKYM(*), RHO(*),VOLU(*),OFF(*)
      INTEGER IADS(8,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K
C     REAL
      my_real
     .        DMASS(MVSIZ)
C-----------------------------------------------
      DO  I=LFT,LLT
        DMASS(I)=FOURTH*RHO(I)*VOLU(I)*OFF(I)
      ENDDO
C
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(1,II)
        FSKYM(K)= DMASS(I)
        K = IADS(3,II)
        FSKYM(K)= DMASS(I)
        K = IADS(6,II)
        FSKYM(K)= DMASS(I)
        K = IADS(5,II)
        FSKYM(K)= DMASS(I)
      ENDDO
C
      RETURN
      END

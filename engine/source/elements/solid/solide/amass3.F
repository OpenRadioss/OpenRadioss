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
Chd|  AMASS3                        source/elements/solid/solide/amass3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE AMASS3(MS,RHO,VOLGP,TAG22,
     .         VOLU,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,MSNF,NVC,OFF,IXS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   MS(*), RHO(*),VOLU(*),VOLGP(LVEUL,*),
     .   MSNF(*),OFF(*),TAG22(*)
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      INTEGER NVC,IXS(NIXS,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG,MCELL,NIN,ICELL,NCELL,IB,J
      my_real DMASS(MVSIZ),FAC
      INTEGER NVC1,NVC2,NVC3,NVC4,NVC5,NVC6,NVC7,NVC8
      my_real TNOD(MVSIZ,8)
C-----------------------------------------------
      NVC1 =  NVC / 128
      NVC2 = (NVC-NVC1*128) / 64
      NVC3 = (NVC-NVC1*128-NVC2*64) / 32
      NVC4 = (NVC-NVC1*128-NVC2*64-NVC3*32)/16
      NVC5 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16)/8
      NVC6 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8)/4
      NVC7 = (NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4)/2
      NVC8 =  NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4-NVC7*2
      IFLAG=INTEG8*JEUL

      NIN = 1
      FAC = ZERO

      !--------------------------------------------------------------------------!
      ! COMPUTE ELEMENTARY MASS AND ITS DISTRIBUTION ON NODES.                   !
      ! FOR CUT CELL TNOD(1:8,I) ENABLE TO DISTRIBUTE MASS TO RELATED NODES ONLY !
      !--------------------------------------------------------------------------!
      IF(IFLAG/=1)THEN
        IF(INT22==0)THEN  
        !DEFAULT FORMULATION    
         TNOD(LFT:LLT,1:8) = ONE
         DO  I=LFT,LLT
          DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
         ENDDO
         !INTERFACE 22 : CUT CELLS
        ELSE
         DO I=LFT,LLT
          IB = NINT(TAG22(I))
          IF(IB>0)THEN
            NCELL = BRICK_LIST(NIN,IB)%NBCUT
            IF(NCELL==0)THEN
              TNOD(I,1:8) = ONE
              DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
              CYCLE
            ENDIF
            TNOD(I,1:8) = ZERO
            MCELL = BRICK_LIST(NIN,IB)%mainID
            IF(MCELL==0)CYCLE ! next Main Cell            
            FAC = BRICK_LIST(NIN,IB)%POLY(MCELL)%NumNOD
            IF(FAC>0)THEN
              FAC=ONE/FAC
              DO J=1,8                                                                                        
                ICELL=BRICK_LIST(NIN,IB)%NODE(J)%WhichCell                                                    
                IF(ICELL/=MCELL)CYCLE                                                                         
                TNOD(I,J) = ONE                                              !this node is used by polyhedron  
              ENDDO               
            ELSE
              FAC = ZERO
            ENDIF                                                                                          
          ELSE
            FAC = ONE_OVER_8 
            TNOD(I,1:8) = ONE                                                !uncut cell distributes centroid mass to all 8 nodes
          ENDIF
          DMASS(I)=FAC*RHO(I)*VOLU(I)*OFF(I)
         ENDDO
        ENDIF      
      ENDIF!IFLAG=1
C
#include "lockon.inc"
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(1,I)*OFF(I)
        ENDDO
      ENDIF

      !---------------------------------------------!
      ! NODAL MASS : ELEMENTARY MASS IS DISTRIBUTED !
      !---------------------------------------------!
      IF(NVC1==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC1(I))   = MS(NC1(I))   + TNOD(I,1)*DMASS(I)                !by default all nodes are tagged. For cut cells only nodes defining the polyhedron is tagged
         MSNF(NC1(I)) = MSNF(NC1(I)) + TNOD(I,1)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC1(I))   = MS(NC1(I))   + TNOD(I,1)*DMASS(I)
         MSNF(NC1(I)) = MSNF(NC1(I)) + TNOD(I,1)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(2,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC2==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC2(I))   = MS(NC2(I))   + TNOD(I,2)*DMASS(I)
         MSNF(NC2(I)) = MSNF(NC2(I)) + TNOD(I,2)*DMASS(I)
       ENDDO
      ELSE
        DO I=LFT,LLT
         MS(NC2(I))   = MS(NC2(I))   + TNOD(I,2)*DMASS(I)
         MSNF(NC2(I)) = MSNF(NC2(I)) + TNOD(I,2)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(3,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC3==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC3(I))   = MS(NC3(I))   + TNOD(I,3)*DMASS(I)
         MSNF(NC3(I)) = MSNF(NC3(I)) + TNOD(I,3)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC3(I))   = MS(NC3(I))   + TNOD(I,3)*DMASS(I)
         MSNF(NC3(I)) = MSNF(NC3(I)) + TNOD(I,3)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(4,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC4==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC4(I))   = MS(NC4(I))   + TNOD(I,4)*DMASS(I)
         MSNF(NC4(I)) = MSNF(NC4(I)) + TNOD(I,4)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC4(I))   = MS(NC4(I))   + TNOD(I,4)*DMASS(I)
         MSNF(NC4(I)) = MSNF(NC4(I)) + TNOD(I,4)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(5,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC5==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC5(I))   = MS(NC5(I))   + TNOD(I,5)*DMASS(I)
         MSNF(NC5(I)) = MSNF(NC5(I)) + TNOD(I,5)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC5(I))   = MS(NC5(I))   + TNOD(I,5)*DMASS(I)
         MSNF(NC5(I)) = MSNF(NC5(I)) + TNOD(I,5)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(6,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC6==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC6(I))   = MS(NC6(I))   + TNOD(I,6)*DMASS(I)
         MSNF(NC6(I)) = MSNF(NC6(I)) + TNOD(I,6)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC6(I))   = MS(NC6(I))   + TNOD(I,6)*DMASS(I)
         MSNF(NC6(I)) = MSNF(NC6(I)) + TNOD(I,6)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
        DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(7,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC7==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC7(I))   = MS(NC7(I))   + TNOD(I,7)*DMASS(I)
         MSNF(NC7(I)) = MSNF(NC7(I)) + TNOD(I,7)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC7(I))   = MS(NC7(I))   + TNOD(I,7)*DMASS(I)
         MSNF(NC7(I)) = MSNF(NC7(I)) + TNOD(I,7)*DMASS(I)
       ENDDO
      ENDIF
C
      IF(IFLAG/=0)THEN
       DO  I=LFT,LLT
         DMASS(I)=RHO(I)*VOLGP(8,I)*OFF(I)
        ENDDO
      ENDIF
      IF(NVC8==0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         MS(NC8(I))   = MS(NC8(I))   + TNOD(I,8)*DMASS(I)
         MSNF(NC8(I)) = MSNF(NC8(I)) + TNOD(I,8)*DMASS(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         MS(NC8(I))   = MS(NC8(I))   + TNOD(I,8)*DMASS(I)
         MSNF(NC8(I)) = MSNF(NC8(I)) + TNOD(I,8)*DMASS(I)
       ENDDO
      ENDIF
#include "lockoff.inc"
C
      RETURN
      END

Chd|====================================================================
Chd|  AMASS3P                       source/elements/solid/solide/amass3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE AMASS3P(FSKYM,RHO,VOLGP,TAG22,VOLU,IADS,OFF,IXS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   FSKYM(*), RHO(*),VOLU(*),VOLGP(LVEUL,*), OFF(*),TAG22(*)
      INTEGER IADS(8,*),IXS(NIXS,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG,II, K, NIN, NCELL, ICELL, IB, J,MCELL
      my_real TNOD(MVSIZ,8)      
      my_real DMASS(MVSIZ),FAC
C-----------------------------------------------
      IFLAG=INTEG8*JEUL
      NIN = 1
      FAC = ZERO

      !--------------------------------------------------------------------------!
      ! COMPUTE ELEMENTARY MASS AND ITS DISTRIBUTION ON NODES.                   !
      ! FOR CUT CELL TNOD(1:8,I) ENABLE TO DISTRIBUTE MASS TO RELATED NODES ONLY !
      !--------------------------------------------------------------------------!
      IF(IFLAG/=1)THEN
        IF(INT22==0)THEN  
        !DEFAULT FORMULATION    
         TNOD(LFT:LLT,1:8) = ONE
         DO  I=LFT,LLT
          DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
         ENDDO
         !INTERFACE 22 : CUT CELLS
        ELSE
         DO I=LFT,LLT
          IB = NINT(TAG22(I))
          IF(IB>0)THEN
            NCELL = BRICK_LIST(NIN,IB)%NBCUT
            IF(NCELL==0)THEN
              TNOD(I,1:8) = ONE
              DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
              CYCLE
            ENDIF
            TNOD(I,1:8) = ZERO
            MCELL = BRICK_LIST(NIN,IB)%mainID
            IF(MCELL==0)CYCLE ! next Main Cell            
            FAC = BRICK_LIST(NIN,IB)%POLY(MCELL)%NumNOD
            IF(FAC>0)THEN
              FAC=ONE/FAC
              DO J=1,8                                                                                        
                ICELL=BRICK_LIST(NIN,IB)%NODE(J)%WhichCell                                                    
                IF(ICELL/=MCELL)CYCLE                                                                         
                TNOD(I,J) = ONE                                              !this node is used by polyhedron  
              ENDDO               
            ELSE
              FAC = ZERO
            ENDIF                                                                                          
          ELSE
            FAC = ONE_OVER_8 
            TNOD(I,1:8) = ONE                                                !uncut cell distributes centroid mass to all 8 nodes
          ENDIF
          DMASS(I)=FAC*RHO(I)*VOLU(I)*OFF(I)
         ENDDO
        ENDIF      
      ENDIF!IFLAG=1


      
      !---------------------------------------------!
      ! NODAL MASS : ELEMENTARY MASS IS DISTRIBUTED !
      !---------------------------------------------!      
      IF(IFLAG==1)THEN
       TNOD(LFT:LLT,1:8) = ONE       
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(1,I)*OFF(I)
       ENDDO
      ENDIF      
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(1,II)
        FSKYM(K)= TNOD(I,1)*DMASS(I)                                        !by default all nodes are tagged. For cut cells only nodes defining the polyhedron is tagged
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(2,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT 
        II = I +NFT
        K = IADS(2,II)
        FSKYM(K)= TNOD(I,2)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(3,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(3,II)
        FSKYM(K)= TNOD(I,3)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(4,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(4,II)
        FSKYM(K)= TNOD(I,4)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(5,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(5,II)
        FSKYM(K)= TNOD(I,5)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(6,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(6,II)
        FSKYM(K)= TNOD(I,6)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(7,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(7,II)
        FSKYM(K)= TNOD(I,7)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=LFT,LLT
        DMASS(I)=RHO(I)*VOLGP(8,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=LFT,LLT
        II = I +NFT
        K = IADS(8,II)
        FSKYM(K)= TNOD(I,8)*DMASS(I)
      ENDDO
C
      RETURN
      END

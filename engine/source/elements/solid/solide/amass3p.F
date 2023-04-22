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
Chd|  AMASS3P                       source/elements/solid/solide/amass3p.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE AMASS3P(
     1   FSKYM,   RHO,     VOLGP,   TAG22,
     2   VOLU,    IADS,    OFF,     IXS,
     3   NEL,     NFT,     JEUL)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JEUL
      my_real
     .   FSKYM(*), RHO(*),VOLU(*),VOLGP(LVEUL,*), OFF(*),TAG22(*)
      INTEGER IADS(8,*),IXS(NIXS,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
         TNOD(1:NEL,1:8) = ONE
         DO  I=1,NEL
          DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
         ENDDO
         !INTERFACE 22 : CUT CELLS
        ELSE
         DO I=1,NEL
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
       TNOD(1:NEL,1:8) = ONE       
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(1,I)*OFF(I)
       ENDDO
      ENDIF      
      DO I=1,NEL
        II = I +NFT
        K = IADS(1,II)
        FSKYM(K)= TNOD(I,1)*DMASS(I)                                        !by default all nodes are tagged. For cut cells only nodes defining the polyhedron is tagged
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(2,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL 
        II = I +NFT
        K = IADS(2,II)
        FSKYM(K)= TNOD(I,2)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(3,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(3,II)
        FSKYM(K)= TNOD(I,3)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(4,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(4,II)
        FSKYM(K)= TNOD(I,4)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(5,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(5,II)
        FSKYM(K)= TNOD(I,5)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(6,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(6,II)
        FSKYM(K)= TNOD(I,6)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(7,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(7,II)
        FSKYM(K)= TNOD(I,7)*DMASS(I)
      ENDDO
C
      IF(IFLAG==1)THEN
       DO  I=1,NEL
        DMASS(I)=RHO(I)*VOLGP(8,I)*OFF(I)
       ENDDO
      ENDIF
      DO I=1,NEL
        II = I +NFT
        K = IADS(8,II)
        FSKYM(K)= TNOD(I,8)*DMASS(I)
      ENDDO
C
      RETURN
      END

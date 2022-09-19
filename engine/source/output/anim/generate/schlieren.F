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
Chd|  SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|-- called by -----------
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SCHLIEREN(
     1                     EVAR    , IX     , X           ,V       ,W,
     2                     IPARG   , WA_L   , ELBUF_TAB   ,ALE_CONNECTIVITY ,VOL,
     3                     PM      , NG     , NIX         ,IOFF,
     4                     ITYP    )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This subroutine outputs data for schlieren.
C schlieren is eta = exp (-C ||grad(rho)||)
C C is a constant which help user to adjust "brightness"
C RADIOSS outputs density gradient which is recuired to output schlieren.
C 'C' cosntant must be tuned during post-treatment then
C is it introduced with HV result math.
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     IALEL > 0 
C        where IALEL =IPARG(7,NG)+IPARG(11,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "flowcom.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IX(NIX,*),IPARG(NPARG,*),NIX,IOFF,NG
      my_real :: WA_L(*),X(3,*),V(3,*),W(3,*),EVAR(*),VOL(*),PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITYP, J, MLW
      INTEGER IV(6), IE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      my_real :: GRAD(3) , Xi(8), Yi(8), Zi(8) , SCH
      INTEGER :: MAT , NC(8)
      my_real :: N(3,6,MVSIZ), RHO(6),  VALVOIS(6), AREA(MVSIZ), NX, A1, A2
      INTEGER :: IAD2, LGTH
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine write nodal shadowgraph (schlieren)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

      JALE = IPARG(07,NG)
      JEUL = IPARG(11,NG)
      MLW  = IPARG(01,NG)

      !-------------------------------------!
      !     COMPUTE NORMAL ON FACES
      !-------------------------------------!
      IF(N2D == 0)THEN !3d solids
        DO I=LFT,LLT
          IE=I+NFT
         !---8 local node numbers NC1 TO NC8 for 3D solid element IE ---!
          NC(1)=IX(2,IE)
          NC(2)=IX(3,IE)
          NC(3)=IX(4,IE)
          NC(4)=IX(5,IE)
          NC(5)=IX(6,IE)
          NC(6)=IX(7,IE)
          NC(7)=IX(8,IE)
          NC(8)=IX(9,IE)
          !---Coordinates of the 8 nodes
          Xi(1)=X(1,NC(1))
          Yi(1)=X(2,NC(1))
          Zi(1)=X(3,NC(1))
          !
          Xi(2)=X(1,NC(2))
          Yi(2)=X(2,NC(2))
          Zi(2)=X(3,NC(2))
          !
          Xi(3)=X(1,NC(3))
          Yi(3)=X(2,NC(3))
          Zi(3)=X(3,NC(3))
          !
          Xi(4)=X(1,NC(4))
          Yi(4)=X(2,NC(4))
          Zi(4)=X(3,NC(4))
          !
          Xi(5)=X(1,NC(5))
          Yi(5)=X(2,NC(5))
          Zi(5)=X(3,NC(5))
          !
          Xi(6)=X(1,NC(6))
          Yi(6)=X(2,NC(6))
          Zi(6)=X(3,NC(6))
          !
          Xi(7)=X(1,NC(7))
          Yi(7)=X(2,NC(7))
          Zi(7)=X(3,NC(7))
          !
          Xi(8)=X(1,NC(8))
          Yi(8)=X(2,NC(8))
          Zi(8)=X(3,NC(8))
          !
          N(1,1,I)=(Yi(3)-Yi(1))*(Zi(2)-Zi(4)) - (Zi(3)-Zi(1))*(Yi(2)-Yi(4))
          N(2,1,I)=(Zi(3)-Zi(1))*(Xi(2)-Xi(4)) - (Xi(3)-Xi(1))*(Zi(2)-Zi(4))
          N(3,1,I)=(Xi(3)-Xi(1))*(Yi(2)-Yi(4)) - (Yi(3)-Yi(1))*(Xi(2)-Xi(4))
          ! Face)-2
          N(1,2,I)=(Yi(7)-Yi(4))*(Zi(3)-Zi(8)) - (Zi(7)-Zi(4))*(Yi(3)-Yi(8))
          N(2,2,I)=(Zi(7)-Zi(4))*(Xi(3)-Xi(8)) - (Xi(7)-Xi(4))*(Zi(3)-Zi(8))
          N(3,2,I)=(Xi(7)-Xi(4))*(Yi(3)-Yi(8)) - (Yi(7)-Yi(4))*(Xi(3)-Xi(8))
          ! Face)-3
          N(1,3,I)=(Yi(6)-Yi(8))*(Zi(7)-Zi(5)) - (Zi(6)-Zi(8))*(Yi(7)-Yi(5))
          N(2,3,I)=(Zi(6)-Zi(8))*(Xi(7)-Xi(5)) - (Xi(6)-Xi(8))*(Zi(7)-Zi(5))
          N(3,3,I)=(Xi(6)-Xi(8))*(Yi(7)-Yi(5)) - (Yi(6)-Yi(8))*(Xi(7)-Xi(5))
          ! Face)-4
          N(1,4,I)=(Yi(2)-Yi(5))*(Zi(6)-Zi(1)) - (Zi(2)-Zi(5))*(Yi(6)-Yi(1))
          N(2,4,I)=(Zi(2)-Zi(5))*(Xi(6)-Xi(1)) - (Xi(2)-Xi(5))*(Zi(6)-Zi(1))
          N(3,4,I)=(Xi(2)-Xi(5))*(Yi(6)-Yi(1)) - (Yi(2)-Yi(5))*(Xi(6)-Xi(1))
          ! Face)-5
          N(1,5,I)=(Yi(7)-Yi(2))*(Zi(6)-Zi(3)) - (Zi(7)-Zi(2))*(Yi(6)-Yi(3))
          N(2,5,I)=(Zi(7)-Zi(2))*(Xi(6)-Xi(3)) - (Xi(7)-Xi(2))*(Zi(6)-Zi(3))
          N(3,5,I)=(Xi(7)-Xi(2))*(Yi(6)-Yi(3)) - (Yi(7)-Yi(2))*(Xi(6)-Xi(3))
          ! Face)-6
          N(1,6,I)=(Yi(8)-Yi(1))*(Zi(4)-Zi(5)) - (Zi(8)-Zi(1))*(Yi(4)-Yi(5))
          N(2,6,I)=(Zi(8)-Zi(1))*(Xi(4)-Xi(5)) - (Xi(8)-Xi(1))*(Zi(4)-Zi(5))
          N(3,6,I)=(Xi(8)-Xi(1))*(Yi(4)-Yi(5)) - (Yi(8)-Yi(1))*(Xi(4)-Xi(5))
          !
          N(1:3,1,I) = HALF * N(1:3,1,I)
          N(1:3,2,I) = HALF * N(1:3,2,I)
          N(1:3,3,I) = HALF * N(1:3,3,I)
          N(1:3,4,I) = HALF * N(1:3,4,I)
          N(1:3,5,I) = HALF * N(1:3,5,I)
          N(1:3,6,I) = HALF * N(1:3,6,I)
        ENDDO
      ELSEIF(ITYP==2)THEN !quads
        DO I=LFT,LLT
          IE=I+NFT
          !---4 local node numbers NC1 TO NC4 for 2D solid element IE ---!
          NC(1)=IX(2,IE)
          NC(2)=IX(3,IE)
          NC(3)=IX(4,IE)
          NC(4)=IX(5,IE)
          !
          !---Coordinates of the 8 nodes
          Xi(1)=ZERO
          Yi(1)=X(2,NC(1))
          Zi(1)=X(3,NC(1))
          !
          Xi(2)=ZERO
          Yi(2)=X(2,NC(2))
          Zi(2)=X(3,NC(2))
          !
          Xi(3)=ZERO
          Yi(3)=X(2,NC(3))
          Zi(3)=X(3,NC(3))
          !
          Xi(4)=ZERO
          Yi(4)=X(2,NC(4))
          Zi(4)=X(3,NC(4))
          !
          ! Face)-1
          N(1,1,I) = ZERO
          N(2,1,I) = (Zi(2)-Zi(1))
          N(3,1,I) =-(Yi(2)-Yi(1))
          ! Face)-2
          N(1,2,I) = ZERO
          N(2,2,I) = (Zi(3)-Zi(2))
          N(3,2,I) =-(Yi(3)-Yi(2))
          ! Face)-3
          N(1,3,I) = ZERO
          N(2,3,I) = (Zi(4)-Zi(3))
          N(3,3,I) =-(Yi(4)-Yi(3))
          ! Face)-4
          N(1,4,I) = ZERO
          N(2,4,I) = (Zi(1)-Zi(4))
          N(3,4,I) =-(Yi(1)-Yi(4))
          !
          N(1:3,5:6,I)=ZERO
          !
          IF(MLW /= 151)THEN
            A1 =Yi(2)*(Zi(3)-Zi(4))+Yi(3)*(Zi(4)-Zi(2))+Yi(4)*(Zi(2)-Zi(3))
            A2 =Yi(2)*(Zi(4)-Zi(1))+Yi(4)*(Zi(1)-Zi(2))+Yi(1)*(Zi(2)-Zi(4))
            AREA(I)=(A1+A2)* HALF
          ELSE
           GBUF => ELBUF_TAB(NG)%GBUF
           AREA(I)=GBUF%AREA(I)
          ENDIF
        ENDDO
      ELSEIF(ITYP==7)THEN !triangles
        DO I=LFT,LLT
          IE=I+NFT
          !---3 local node numbers NC1 TO NC3 for 2D solid element IE ---!
          NC(1)=IX(2,IE)
          NC(2)=IX(3,IE)
          NC(3)=IX(4,IE)
          !---Coordinates of the 8 nodes
          Xi(1)=ZERO
          Yi(1)=X(2,NC(1))
          Zi(1)=X(3,NC(1))
          !
          Xi(2)=ZERO
          Yi(2)=X(2,NC(2))
          Zi(2)=X(3,NC(2))
          !
          Xi(3)=ZERO
          Yi(3)=X(2,NC(3))
          Zi(3)=X(3,NC(3))
          !
          ! Face)-1
          N(1,1,I) = ZERO
          N(2,1,I) = (Zi(2)-Zi(1))
          N(3,1,I) =-(Yi(2)-Yi(1))
          ! Face)-2
          N(1,2,I) = ZERO
          N(2,2,I) = (Zi(3)-Zi(2))
          N(3,2,I) =-(Yi(3)-Yi(2))
          ! Face)-3
          N(1,3,I) = ZERO
          N(2,3,I) = (Zi(1)-Zi(3))
          N(3,3,I) =-(Yi(1)-Yi(3))
          !
          N(1:3,4:6,I)=ZERO
          !
          IF(MLW /= 151)THEN
            NX = HALF * ((Yi(2) - Yi(1)) * (Zi(3) - Zi(1)) - (Zi(2) - Zi(1)) * (Yi(3) - Yi(1)))
            AREA(I)=ABS(NX)  !SQRT(NX*NX+NY*NY+NZ*NZ)
          ELSE
           GBUF => ELBUF_TAB(NG)%GBUF
           AREA(I)=GBUF%AREA(I)
          ENDIF
        ENDDO
      ELSE
          N(1:3,1:6,LFT:LLT) = ZERO
          AREA(LFT:LLT)=ONE
      ENDIF

      !-------------------------------------!
      !     FACE RECONSTRUCTION
      !-------------------------------------!
      IF(N2D == 0)THEN
        DO I=LFT,LLT
          IE=I+NFT
          IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          DO J=1,LGTH
             IV(J) = ALE_CONNECTIVITY%ee_connect%connected(IAD2 + J - 1)
            IF(IV(J) > 0)THEN
              VALVOIS(J) = WA_L(IV(J))
            ELSE
              VALVOIS(J) = WA_L(IE)
            ENDIF
          ENDDO
          RHO(1) = HALF*( WA_L(IE) + VALVOIS(1) )
          RHO(2) = HALF*( WA_L(IE) + VALVOIS(2) )
          RHO(3) = HALF*( WA_L(IE) + VALVOIS(3) )
          RHO(4) = HALF*( WA_L(IE) + VALVOIS(4) )
          RHO(5) = HALF*( WA_L(IE) + VALVOIS(5) )
          RHO(6) = HALF*( WA_L(IE) + VALVOIS(6) )
          !-------------------------------------!
          !  FVM GRADIENT (GREEN OSTROGRADSKI)
          !-------------------------------------!
          GRAD(1:3) = ZERO
          GRAD(1:3) = GRAD(1:3) + RHO(1)*N(1:3,1,I)
          GRAD(1:3) = GRAD(1:3) + RHO(2)*N(1:3,2,I)
          GRAD(1:3) = GRAD(1:3) + RHO(3)*N(1:3,3,I)
          GRAD(1:3) = GRAD(1:3) + RHO(4)*N(1:3,4,I)
          GRAD(1:3) = GRAD(1:3) + RHO(5)*N(1:3,5,I)
          GRAD(1:3) = GRAD(1:3) + RHO(6)*N(1:3,6,I)
          GRAD(1:3) = GRAD(1:3) / VOL(I)
          !-------------------------------------!
          !             SCHLIEREN
          !-------------------------------------!
          SCH = SQRT(SUM(GRAD(1:3)*GRAD(1:3)))
          SCH = EXP(-SCH)
          EVAR(I) = SCH
        ENDDO!next I
      ELSEIF(ITYP==2)THEN
        GBUF => ELBUF_TAB(NG)%GBUF
        DO I=LFT,LLT
          IE=I+NFT
          IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-IAD2
          DO J=1,LGTH
            IV(J)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + J - 1)
            IF(IV(J) > 0)THEN
              VALVOIS(J) = WA_L(IV(J))
            ELSE
              VALVOIS(J) = WA_L(IE)
            ENDIF
          ENDDO
          RHO(1) = HALF*( WA_L(IE) + VALVOIS(1) )
          RHO(2) = HALF*( WA_L(IE) + VALVOIS(2) )
          RHO(3) = HALF*( WA_L(IE) + VALVOIS(3) )
          RHO(4) = HALF*( WA_L(IE) + VALVOIS(4) )
          !-------------------------------------!
          !  FVM GRADIENT (GREEN OSTROGRADSKI)
          !-------------------------------------!
          GRAD(1:3) = ZERO
          GRAD(2:3) = GRAD(2:3) + RHO(1)*N(2:3,1,I)
          GRAD(2:3) = GRAD(2:3) + RHO(2)*N(2:3,2,I)
          GRAD(2:3) = GRAD(2:3) + RHO(3)*N(2:3,3,I)
          GRAD(2:3) = GRAD(2:3) + RHO(4)*N(2:3,4,I)
          GRAD(2:3) = GRAD(2:3) /  AREA(I)
          !-------------------------------------!
          !             SCHLIEREN
          !-------------------------------------!
          SCH = SQRT(SUM(GRAD(2:3)*GRAD(2:3)))
          SCH = EXP(-SCH)
          EVAR(I) = SCH
        ENDDO!next I
      ELSEIF(ITYP==7)THEN
        GBUF => ELBUF_TAB(NG)%GBUF
        DO I=LFT,LLT
          IE=I+NFT
          IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          DO J=1,3
            IV(J)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + J - 1)
            IF(IV(J) > 0)THEN
              VALVOIS(J) = WA_L(IV(J))
            ELSE
              VALVOIS(J) = WA_L(IE)
            ENDIF
          ENDDO
          RHO(1) = HALF*( WA_L(IE) + VALVOIS(1) )
          RHO(2) = HALF*( WA_L(IE) + VALVOIS(2) )
          RHO(3) = HALF*( WA_L(IE) + VALVOIS(3) )
          !-------------------------------------!
          !  FVM GRADIENT (GREEN OSTROGRADSKI)
          !-------------------------------------!
          GRAD(1:3) = ZERO
          GRAD(2:3) = GRAD(2:3) + RHO(1)*N(2:3,1,I)
          GRAD(2:3) = GRAD(2:3) + RHO(2)*N(2:3,2,I)
          GRAD(2:3) = GRAD(2:3) + RHO(3)*N(2:3,3,I)
          GRAD(2:3) = GRAD(2:3) / AREA(I)
          !-------------------------------------!
          !             SCHLIEREN
          !-------------------------------------!
          SCH = SQRT(SUM(GRAD(2:3)*GRAD(2:3)))
          SCH = EXP(-SCH)
          EVAR(I) = SCH
        ENDDO!next I
      ENDIF

      END SUBROUTINE SCHLIEREN


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
Chd|  MULTI_CONNECTIVITY            source/multifluid/multi_connectivity.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_CONNECTIVITY( INDX_S,INDX_Q,INDX_TG,
     1                               FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,
     2                               IXS,IXQ,IXTG,CEP,ALE_CONNECTIVITY,BOOL_ALE_TG )
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
!$COMMENT
!       MULTI_CONNECTIVITY description :
!           creation of reverse connectivity
!           element / element 
!       MULTI_CONNECTIVITY organization :
!           for each IE element, find the connected element
!           loop over the surfaces of the connected element
!           and save the ID of the surface of the
!           connected element  
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       BOOL_ALE_TG : logical, true only if 2d model + MULTI_FVM used
!       INDX_xxx : integer ; dimension=NUMELxxx ; index for the surface 
!                  of the remote connected element
!       FACE_ELM_xxx : integer ; dimension=(6/4/3*NUMELxxx,2) ; surface 
!                  of the remote connected element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        LOGICAL, INTENT(in) :: BOOL_ALE_TG
        INTEGER, DIMENSION(NUMELS), INTENT(inout) :: INDX_S
        INTEGER, DIMENSION(NUMELQ), INTENT(inout) :: INDX_Q
        INTEGER, DIMENSION(NUMELTG), INTENT(inout) :: INDX_TG
        INTEGER, DIMENSION(6*NUMELS,2), INTENT(inout) :: FACE_ELM_S
        INTEGER, DIMENSION(4*NUMELQ,2), INTENT(inout) :: FACE_ELM_Q
        INTEGER, DIMENSION(3*NUMELTG,2), INTENT(inout) :: FACE_ELM_TG
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY


        INTEGER, DIMENSION(NIXS,*), INTENT(in) :: IXS
        INTEGER, DIMENSION(NIXQ,*), INTENT(in) :: IXQ
        INTEGER, DIMENSION(NIXTG,*), INTENT(in) :: IXTG

        INTEGER, DIMENSION(*), INTENT(in) :: CEP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: IE,I,IV,J,SHIFT,JJ,IAD1, LGTH, IAD2, LGTH2
        INTEGER :: CURRENT_PROC,PROC,J_SAVE
! --------------------------------------
!   solid
        DO IE=1,NUMELS
           IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
           LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1) - ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
            I = IE
            CURRENT_PROC = CEP(I)
            ! loop over the surface of the IE element
            DO J = 1, LGTH
                IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1) ! id of the connected element IV
            
                IF(IV>0) THEN
                    PROC = CEP(IV)
                    J_SAVE = -1
                    IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                    LGTH2 =  ALE_CONNECTIVITY%ee_connect%iad_connect(IV+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                    ! save the connected surface if IE & IV are on 2 different proc
                    IF(CURRENT_PROC/=PROC) THEN
                        ! loop over the surface of the IV connected element 
                        DO JJ=1,LGTH2
                            IF(ALE_CONNECTIVITY%ee_connect%connected(IAD2 + JJ - 1)==IE) THEN
                                J_SAVE = JJ !   find the connected surface
                                EXIT
                            ENDIF
                        ENDDO
                        INDX_S(IE) = INDX_S(IE) + 1
                        FACE_ELM_S( 6*(IE-1)+INDX_S(IE),1 ) = J_SAVE
                        FACE_ELM_S( 6*(IE-1)+INDX_S(IE),2 ) = IXS(NIXS,IV)
                    ENDIF
                ENDIF
            ENDDO              
        ENDDO
! --------------------------------------
!   quad
        SHIFT = NUMELS
        DO IE=1,NUMELQ
            I = IE + SHIFT
            IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
            LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - ALE_CONNECTIVITY%ee_connect%iad_connect(I)
            CURRENT_PROC = CEP(I)
            ! loop over the surface of the IE element
            DO J = 1, LGTH
                IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1) ! id of the connected element IV
                IF(IV>0) THEN
                    PROC = CEP(IV)
                    J_SAVE = -1
                    IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                    LGTH2 =  ALE_CONNECTIVITY%ee_connect%iad_connect(IV+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                    ! save the connected surface if IE & IV are on 2 different proc
                    IF(CURRENT_PROC/=PROC) THEN
                        ! loop over the surface of the IV connected element 
                        DO JJ=1,LGTH2
                            IF(ALE_CONNECTIVITY%ee_connect%connected(IAD2 + JJ - 1)==IE) THEN
                                J_SAVE = JJ !   find the connected surface
                                EXIT
                            ENDIF
                        ENDDO
                        ! save the connected surface if IE & IV are on 2 different proc
                        INDX_Q(IE) = INDX_Q(IE) + 1
                        FACE_ELM_Q( 4*(IE-1)+INDX_Q(IE),1 ) = J_SAVE
                        FACE_ELM_Q( 4*(IE-1)+INDX_Q(IE),2 ) = IXQ(NIXQ,IV)
                    ENDIF
                ENDIF
            ENDDO              
        ENDDO
! --------------------------------------
!   triangle
        IF(BOOL_ALE_TG) THEN
            SHIFT = NUMELS + NUMELQ + NUMELC + NUMELT + NUMELP + NUMELR
            DO IE=1,NUMELTG           
                I = IE + SHIFT
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                CURRENT_PROC = CEP(I)
                ! loop over the surface of the IE element
                DO J = 1, LGTH
                    IV =  ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)! id of the connected element IV
                    IF(IV>0) THEN
                        PROC = CEP(IV)
                        J_SAVE = -1 
                        IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                        LGTH2 =  ALE_CONNECTIVITY%ee_connect%iad_connect(IV+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IV)
                        ! save the connected surface if IE & IV are on 2 different proc
                        IF(CURRENT_PROC/=PROC) THEN
                            ! loop over the surface of the IV connected element 
                            DO JJ=1,LGTH2
                                IF(ALE_CONNECTIVITY%ee_connect%connected(IAD2 + JJ - 1)==IE) THEN
                                    J_SAVE = JJ !   find the connected surface
                                    EXIT
                                ENDIF
                            ENDDO
                            INDX_TG(IE) = INDX_TG(IE) + 1
                            FACE_ELM_TG( 3*(IE-1)+INDX_TG(IE),1 ) = J_SAVE
                            FACE_ELM_TG( 3*(IE-1)+INDX_TG(IE),2 ) = IXTG(NIXTG,IV)
                        ENDIF
                    ENDIF
                ENDDO              
            ENDDO
        ENDIF
! --------------------------------------
        RETURN
        END SUBROUTINE MULTI_CONNECTIVITY

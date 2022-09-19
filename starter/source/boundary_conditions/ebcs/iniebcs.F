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
Chd|  INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FINDELE                       source/boundary_conditions/ebcs/findele.F
Chd|        ICINVS                        source/boundary_conditions/ebcs/iniebcs.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale_ebcs_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIEBCS(ALE_CONNECTIVITY, IFLAG,IGRSURF, IXS, IXQ, IXTG,
     .     PM, IGEO, X, LSENSOR, IVOLU, MULTI_FVM_IS_USED, EBCS_TAB, EBCS_TAG_CELL_SPMD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE ALE_EBCS_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER IXS(SIXS),IXQ(SIXQ),IXTG(SIXTG), LSENSOR(NSENSI,NSENSOR),IVOLU(NIMV,*)
      LOGICAL, INTENT(IN) :: MULTI_FVM_IS_USED
      INTEGER, INTENT(INOUT) :: EBCS_TAG_CELL_SPMD(NUMELQ+NUMELTG+NUMELS)
      my_real PM(NPROPM,NUMMAT),X(SX)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
      INTEGER,INTENT(IN) :: IFLAG, IGEO(NPROPGI,NUMGEO)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: MWA
      INTEGER I,JBUF,TYP,ID,ISU,NSEG,IDSU,IAD,K1,K2,K3,SENS,VOLU,J,NOD,KK,RBUF,LENMWA,ERR,ICELL
      INTEGER :: SIZ
      CLASS (T_EBCS), POINTER :: EBCS
C=======================================================================
      IF (N2D == 0) THEN
         LENMWA = NUMNOD+2+8*NUMELS
         ALLOCATE(MWA(LENMWA),  STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF
         CALL ICINVS(8, NUMELS, NIXS, IXS, MWA, MWA(1+(2+NUMNOD)))
      ELSEIF (NUMELQ /= 0) THEN
         LENMWA = NUMNOD + 2 + 4 * NUMELQ
         ALLOCATE(MWA(LENMWA), STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF
         CALL ICINVS(4, NUMELQ, NIXQ, IXQ, MWA, MWA(1+(2+NUMNOD)))
      ELSEIF (NUMELTG /= 0 .AND. MULTI_FVM_IS_USED) THEN
         LENMWA = NUMNOD + 2 + 3 * NUMELTG
         ALLOCATE(MWA(LENMWA), STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF
         CALL ICINVS(3, NUMELTG, NIXTG, IXTG, MWA, MWA(1+(2+NUMNOD)))
      ENDIF

      DO I = 1, NEBCS
         EBCS => EBCS_TAB%tab(i)%poly
         TYP = EBCS%type
         ISU = EBCS%surf_id
         ID = EBCS%ebcs_id
         NSEG = EBCS%nb_elem
         SELECT TYPE (EBCS)
           TYPE IS (t_ebcs_gradp0)
             !     vold
             EBCS%has_vold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(EBCS%nb_node))
             EBCS%vold(1:EBCS%nb_node) = ZERO
             !     pold
             EBCS%has_pold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%pold(EBCS%nb_node))
             EBCS%pold(1:EBCS%nb_node) = ZERO
             !     p0
             EBCS%has_p0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%p0(EBCS%nb_node))
             EBCS%p0(1:EBCS%nb_node) = ZERO
             !     iface
             EBCS%has_iface = .TRUE.
             IF(IFLAG==0)ALLOCATE(EBCS%iface(EBCS%nb_elem))
             EBCS%iface(1:EBCS%nb_elem) = 0
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_iniv)
             !     reso
             EBCS%has_reso = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%reso(3, EBCS%nb_node))
             EBCS%reso(1:3, 1:EBCS%nb_node) = ZERO
             !     ro0
             EBCS%has_ro0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%ro0(EBCS%nb_elem))
             EBCS%ro0(1:EBCS%nb_elem) = ZERO
             !     en0
             EBCS%has_en0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%en0(EBCS%nb_elem))
             EBCS%en0(1:EBCS%nb_elem) = ZERO
             !     v0
             EBCS%has_v0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%v0(3, EBCS%nb_node))
             EBCS%v0(1:3, 1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_pres)
             !     vold
             EBCS%has_vold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(EBCS%nb_node))
             EBCS%vold(1:EBCS%nb_node) = ZERO
             !     pold
             EBCS%has_pold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%pold(EBCS%nb_node))
             EBCS%pold(1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_valvin)
             !     vold
             EBCS%has_vold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(EBCS%nb_node))
             EBCS%vold(1:EBCS%nb_node) = ZERO
             !     pold
             EBCS%has_pold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%pold(EBCS%nb_node))
             EBCS%pold(1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_valvout)
             !     vold
             EBCS%has_vold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(EBCS%nb_node))
             EBCS%vold(1:EBCS%nb_node) = ZERO
             !     pold
             EBCS%has_pold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%pold(EBCS%nb_node))
             EBCS%pold(1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS(t_ebcs_vel)
             !     reso
             EBCS%has_reso = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%reso(3, EBCS%nb_node))
             EBCS%reso(1:3, 1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS(t_ebcs_normv)
             !     reso
             EBCS%has_reso = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%reso(3, EBCS%nb_node))
             EBCS%reso(1:3, 1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_inip)
             !     ro0
             EBCS%has_ro0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%ro0(EBCS%nb_elem))
             EBCS%ro0(1:EBCS%nb_elem) = ZERO
             !     en0
             EBCS%has_en0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%en0(EBCS%nb_elem))
             EBCS%en0(1:EBCS%nb_elem) = ZERO
             !     p0
             EBCS%has_p0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%p0(EBCS%nb_node))
             EBCS%p0(1:EBCS%nb_node) = ZERO
             !     vold
             EBCS%has_vold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(EBCS%nb_node))
             EBCS%vold(1:EBCS%nb_node) = ZERO
             !     pold
             EBCS%has_pold = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%pold(EBCS%nb_node))
             EBCS%pold(1:EBCS%nb_node) = ZERO
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
           TYPE IS (t_ebcs_monvol)
             VOLU = EBCS%monvol_id
             SENS = EBCS%sensor_id
             DO J = 1, NVOLU
                IF (VOLU == IVOLU(1,J))THEN
                   VOLU = IVOLU(1,J)
                   EBCS%monvol_id = VOLU
                ENDIF
             ENDDO
             IF(IFLAG==1)THEN
               DO J = 1, NSENSOR
                  IF(SENS == LSENSOR(1,J))THEN
                     SENS = LSENSOR(1, J)
                     EBCS%monvol_id = SENS
                  ENDIF
               ENDDO
             ENDIF
           TYPE IS (t_ebcs_inlet)
             EBCS%has_iface = .TRUE.
             IF(IFLAG==0)ALLOCATE(EBCS%iface(EBCS%nb_elem))
             EBCS%iface(1:EBCS%nb_elem) = 0
           TYPE IS (t_ebcs_fluxout)
             EBCS%has_iface = .TRUE.
             IF(IFLAG==0)ALLOCATE(EBCS%iface(EBCS%nb_elem))
             EBCS%iface(1:EBCS%nb_elem) = 0
           TYPE IS (t_ebcs_nrf)
             IF(EBCS%is_multifluid) THEN
                SIZ = EBCS%nb_elem
             ELSE
                SIZ = EBCS%nb_node
             ENDIF

             !     iface
             EBCS%has_iface = .TRUE.
             IF(IFLAG==0)ALLOCATE(EBCS%iface(NSEG))
             EBCS%iface(1:NSEG) = 0
             !     vold
             EBCS%has_vold = .TRUE.
             IF(IFLAG==0)ALLOCATE(EBCS%vold(SIZ))
             EBCS%vold(1:SIZ) = 0
             !     Pold
             EBCS%has_Pold = .TRUE.
             IF(IFLAG==0) ALLOCATE(EBCS%Pold(SIZ))
             EBCS%Pold(1:SIZ) = 0
             !     la
             EBCS%has_la = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%la(3, EBCS%nb_node))
             EBCS%la(1:3, 1:EBCS%nb_node) = ZERO
             !     dp0
             EBCS%has_dp0 = .true.
             IF(IFLAG==0)ALLOCATE(EBCS%dp0(NSEG))
             EBCS%dp0(1:NSEG) = ZERO

        END SELECT

        IF(ISU>0)THEN
          IDSU = IGRSURF(ISU)%ID
          IF (N2D == 0) THEN
             CALL FINDELE(ALE_CONNECTIVITY, 8, NIXS, 6, IDSU,ID,NSEG,IXS,
     .            EBCS%iseg, EBCS%ielem, EBCS%itype,EBCS%iface,
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)
          ELSEIF (NUMELQ /= 0) THEN
             CALL FINDELE(ALE_CONNECTIVITY, 4, NIXQ, 4, IDSU,ID,NSEG,IXQ,
     .            EBCS%iseg, EBCS%ielem,EBCS%itype, EBCS%iface,
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)
          ELSEIF (NUMELTG /= 0 .AND. MULTI_FVM_IS_USED) THEN
             CALL FINDELE(ALE_CONNECTIVITY, 3, NIXTG, 3, IDSU,ID,NSEG,IXTG,
     .            EBCS%iseg, EBCS%ielem,EBCS%itype, EBCS%iface,
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)
          ENDIF

          IF(.NOT.EBCS%is_multifluid .AND. (TYP == 0 .OR. TYP == 10))THEN
            !2D quads & trias EBCS : all on domain 1 (ispmd=0)  --> TAG with  EBCS_TAG_CELL_SPMD
            !-------------------------------------------------
            EBCS_TAG_CELL_SPMD(1:NUMELQ+NUMELTG+NUMELS)=0
            DO KK=1,EBCS%nb_elem
              ICELL = EBCS%ielem(KK)
              K1=0
              IF (EBCS%itype(KK)==4)K1=0
              IF (EBCS%itype(KK)==3)K1=NUMELQ
              IF (EBCS%itype(KK)==8)K1=NUMELQ+NUMELTG
              EBCS_TAG_CELL_SPMD(K1+ICELL)=1    !tag elem to send to domain #1
            ENDDO
            !-------------------------------------------------

          ENDIF

        ENDIF
      ENDDO

      IF (ALLOCATED(MWA)) DEALLOCATE(MWA)
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  ICINVS                        source/boundary_conditions/ebcs/iniebcs.F
Chd|-- called by -----------
Chd|        INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ICINVS(NNODE, NELEM, NIX, IX, IADD, INVC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NNODE, NELEM, NIX, IX(NIX, *)
      INTEGER, INTENT(OUT) :: IADD(*), INVC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------`-------------------------
      INTEGER I, J, N
C-----------------------------------------------
C-----------------------------------------------
      IADD(1) = 1
      IADD(2) = 1
C
      DO I=3,NUMNOD+1
          IADD(I)=0
      ENDDO
C
      DO J = 2, 1 + NNODE
        DO I=1,NELEM
          N = IX(J,I) + 2
          IADD(N)=IADD(N)+1
        ENDDO
      ENDDO
C
      DO I=3,NUMNOD+1
          IADD(I)=IADD(I)+IADD(I-1)
      ENDDO
      DO J=2, 1 + NNODE
        DO I=1,NELEM
          N = IX(J,I) + 1
          INVC(IADD(N)) = I
          IADD(N) = IADD(N) + 1
        ENDDO
      ENDDO
      RETURN
      END


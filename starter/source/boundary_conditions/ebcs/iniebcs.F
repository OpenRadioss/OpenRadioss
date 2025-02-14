Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    iniebcs                ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    findele                ../starter/source/boundary_conditions/ebcs/findele.F
      !||    icinvs                 ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    iface                  ../starter/source/ale/ale3d/iface.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE INIEBCS(ALE_CONNECTIVITY, IFLAG,IGRSURF, IXS, IXQ, IXTG, 
     .     PM, IGEO, X, SENSORS, IVOLU, MULTI_FVM_IS_USED, EBCS_TAB, EBCS_TAG_CELL_SPMD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE ALE_EBCS_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      INTEGER,INTENT(IN),TARGET :: IXS(NIXS,SIXS/NIXS),IXQ(NIXQ,SIXQ/NIXQ),IXTG(NIXTG,SIXTG/NIXTG)
      INTEGER, INTENT(IN) :: IVOLU(NIMV,*)
      LOGICAL, INTENT(IN) :: MULTI_FVM_IS_USED
      INTEGER, INTENT(INOUT) :: EBCS_TAG_CELL_SPMD(NUMELQ+NUMELTG+NUMELS)
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT),X(SX)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), TARGET, INTENT(INOUT) :: EBCS_TAB
      INTEGER,INTENT(IN) :: IFLAG, IGEO(NPROPGI,NUMGEO)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: MWA 
      INTEGER I,TYP,ID,ISU,NSEG,IDSU,K1,SENS,VOLU,J,KK,LENMWA,ERR,ICELL
      INTEGER :: SIZ
      CLASS (T_EBCS), POINTER :: EBCS
      INTEGER :: JALE_FROM_MAT, JALE_FROM_PROP, IS_ALE_EULER  !< ALE/EULER framework flag
      INTEGER IMID, IPID !< material and property iid
      INTEGER, DIMENSION(:, :), POINTER :: IX
      INTEGER :: NIX
C=======================================================================
      EBCS_TAG_CELL_SPMD(1:NUMELQ+NUMELTG+NUMELS)=0
      NULLIFY(IX)
      NIX = 0
      IF (N2D == 0) THEN
         LENMWA = NUMNOD+2+8*NUMELS
         ALLOCATE(MWA(LENMWA),  STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF      
         CALL ICINVS(8, NUMELS, NIXS, IXS, MWA, MWA(1+(2+NUMNOD)))
          IX => IXS(1:NIXS, 1:NUMELS)
          NIX = NIXS
      ELSEIF (NUMELQ /= 0) THEN
         LENMWA = NUMNOD + 2 + 4 * NUMELQ
         ALLOCATE(MWA(LENMWA), STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF      
         CALL ICINVS(4, NUMELQ, NIXQ, IXQ, MWA, MWA(1+(2+NUMNOD)))
         IX => IXQ(1:NIXQ, 1:NUMELQ)
         NIX = NIXQ
      ELSEIF (NUMELTG /= 0 .AND. MULTI_FVM_IS_USED) THEN
         LENMWA = NUMNOD + 2 + 3 * NUMELTG
         ALLOCATE(MWA(LENMWA), STAT=ERR)
         IF(ERR /= 0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='EBCS')
         ENDIF      
         CALL ICINVS(3, NUMELTG, NIXTG, IXTG, MWA, MWA(1+(2+NUMNOD)))
         IX => IXTG(1:NIXTG, 1:NUMELTG)
         NIX = NIXTG
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
               DO J = 1, SENSORS%NSENSOR
                  IF (SENS == SENSORS%SENSOR_TAB(J)%SENS_ID)THEN
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

           TYPE IS (t_ebcs_propergol)
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
             CALL FINDELE(ALE_CONNECTIVITY, 8, NIXS, IDSU,ID,NSEG,IXS,
     .            EBCS%iseg, EBCS%ielem, EBCS%itype,EBCS%iface, 
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)
          ELSEIF (NUMELQ /= 0) THEN
             CALL FINDELE(ALE_CONNECTIVITY, 4, NIXQ, IDSU,ID,NSEG,IXQ,
     .            EBCS%iseg, EBCS%ielem,EBCS%itype, EBCS%iface,
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)           
          ELSEIF (NUMELTG /= 0 .AND. MULTI_FVM_IS_USED) THEN
             CALL FINDELE(ALE_CONNECTIVITY, 3, NIXTG, IDSU,ID,NSEG,IXTG,
     .            EBCS%iseg, EBCS%ielem,EBCS%itype, EBCS%iface,
     .            IGRSURF(ISU)%NODES,MWA,MWA(1+(2+NUMNOD)),PM,X,TYP,IGEO)                  
          ENDIF
          
          IF(.NOT.EBCS%is_multifluid .AND. (TYP == 0 .OR. TYP == 10 .OR. TYP == 11))THEN
            !2D quads & trias EBCS : all on domain 1 (ispmd=0)  --> TAG with  EBCS_TAG_CELL_SPMD
            !-------------------------------------------------                      
            DO KK=1,EBCS%nb_elem              
              ICELL = EBCS%ielem(KK) 
              K1=0
              IF (EBCS%itype(KK)==4)K1=0
              IF (EBCS%itype(KK)==3)K1=NUMELQ 
              IF (EBCS%itype(KK)==8)K1=NUMELQ+NUMELTG             
              IF(TYP/=10 .AND. TYP /= 11) EBCS_TAG_CELL_SPMD(K1+ICELL)=1    !tag elem to send to domain #1
            ENDDO                                                
            !-------------------------------------------------   
                                                                 
          ENDIF

          ! check that based element has ALE or EULER framework
          DO KK=1,EBCS%NB_ELEM
            ICELL = EBCS%IELEM(KK)
            IMID=IX(1,ICELL)
            IPID=IX(NIX-1,ICELL)
            JALE_FROM_MAT = INT(PM(72,IMID))
            JALE_FROM_PROP = IGEO(62,IPID)
            IS_ALE_EULER = JALE_FROM_MAT + JALE_FROM_PROP
            IF(IS_ALE_EULER == 0)THEN
              !error message
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1 = EBCS%ebcs_id,C1 = TRIM(ebcs%title),
     .                    C2 = "EBCS ARE ONLY COMPATIBLE WITH ALE OR EULER FRAMEWORK")
              exit
            ENDIF
          ENDDO


                       
        ENDIF
      ENDDO
            
      IF (ALLOCATED(MWA)) DEALLOCATE(MWA)
C-----------
      RETURN
      END
C
      !||====================================================================
      !||    icinvs    ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||--- called by ------------------------------------------------------
      !||    iniebcs   ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||====================================================================
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NNODE, NELEM, NIX, IX(NIX, *)
      INTEGER, INTENT(OUT) :: IADD(*), INVC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
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
 

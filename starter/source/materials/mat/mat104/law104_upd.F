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
!||    law104_upd         ../starter/source/materials/mat/mat104/law104_upd.F
!||--- called by ------------------------------------------------------
!||    updmat             ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod       ../starter/share/modules1/elbuftag_mod.F
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE LAW104_UPD(IFAILG  ,NUPARAM ,NUPARF  ,UPARAM  ,UPARF   ,
     .                      NLOC_DMG,IMAT    ,MLAW_TAG,IPM     ,MATPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE ELBUFTAG_MOD 
      USE MATPARAM_DEF_MOD
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
      INTEGER IFAILG,NUPARAM,NUPARF,IMAT,IPM(NPROPMI,*)
      my_real, DIMENSION(NUPARF) , INTENT(IN)  :: UPARF
      my_real, DIMENSION(NUPARAM), INTENT(INOUT) :: UPARAM
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE(MLAW_TAG_),INTENT(INOUT)    :: MLAW_TAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)    :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILOC,IGURSON
      my_real 
     .  Q1,Q2,Q3,EPN,AS,KW,F0,FC,FR,RLEN,HKHI
C=======================================================================
       IGURSON = 0
      IF (IFAILG == 1) THEN
        ILOC  = MATPARAM%NLOC
        !IGURSON = 0  no damage (default) 
        IF (ILOC == 0) THEN
          IGURSON = 1 ! local damage
        ELSEIF (ILOC == 1) THEN
          IGURSON = 2 ! Non local Forest (Micromorphic)
        ELSEIF (ILOC == 2) THEN
          IGURSON = 3 ! Non local (Peerling)
          MLAW_TAG%NUVAR = MLAW_TAG%NUVAR + 1
        ENDIF
        IPM(8,IMAT) = MLAW_TAG%NUVAR
        ! Gurson yield criterion parameters
        Q1    = UPARF(2) 
        Q2    = UPARF(3) 
        Q3    = UPARF(4) 
        ! Trigger plastic strain for damage nucleation
        EPN   = UPARF(5) 
        ! Nucleation rate
        AS    = UPARF(6) 
        ! Nahshon-Hutchinson shear parameter
        KW    = UPARF(7) 
        ! Void volume fraction at fracture
        FR    = UPARF(8)
        ! Critical void volume fraction 
        FC    = UPARF(9)
        ! Initial void volume fraction
        F0    = UPARF(10)
        ! Non-local internal length
        RLEN  = UPARF(11)
        ! Micromorphic penalty parameter
        HKHI  = UPARF(12)
c
        ! Storage of the non-local internal length
        IF (ILOC>0) THEN
          NLOC_DMG%LEN(IMAT) = MAX(NLOC_DMG%LEN(IMAT), RLEN)
          CALL GET_LEMAX(NLOC_DMG%LE_MAX(IMAT),NLOC_DMG%LEN(IMAT))
          MLAW_TAG%G_PLANL  = 1
          MLAW_TAG%L_PLANL  = 1
          MLAW_TAG%G_EPSDNL = 1
          MLAW_TAG%L_EPSDNL = 1
        ENDIF
c
        ! Tag for damage output
        ! -> Number of output modes (stored in DMG(NEL,I), I>1)
        MATPARAM%NMOD  = 5
        ! Total number of damage outputs
        ! -> DMG(NEL,1) = Global damage output 
        ! -> DMG(NEL,2:NMOD+1) = Damage modes output
        MLAW_TAG%G_DMG = 1 + MATPARAM%NMOD
        MLAW_TAG%L_DMG = 1 + MATPARAM%NMOD
        ! -> Modes allocation and definition
        ALLOCATE(MATPARAM%MODE(MATPARAM%NMOD))
        MATPARAM%MODE(1) = "Void growth volume fraction fg"
        MATPARAM%MODE(2) = "Nucleation volume fraction fn"
        MATPARAM%MODE(3) = "Shear growth volume fraction fsh"
        MATPARAM%MODE(4) = "Total void volume fraction ft"
        MATPARAM%MODE(5) = "Effective void volume fraction f*"
c
        ! Storage of damage parameters
        UPARAM(30) = IGURSON
        UPARAM(31) = Q1
        UPARAM(32) = Q2
        UPARAM(33) = Q3
        UPARAM(34) = EPN
        UPARAM(35) = AS
        UPARAM(36) = KW
        UPARAM(37) = FR
        UPARAM(38) = FC
        UPARAM(39) = F0
        UPARAM(40) = HKHI 
      ENDIF
c-----------
      RETURN
      END

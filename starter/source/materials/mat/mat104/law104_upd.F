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
Chd|  LAW104_UPD                    source/materials/mat/mat104/law104_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        GET_LEMAX                     ../common_source/modules/nlocal_reg_mod.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
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
      IF (IFAILG == 1) THEN
        ILOC  = NINT(UPARF(1))
        !IGURSON = 0  no damage (default) 
        IF (ILOC == 0) THEN
          IGURSON = 1 ! local damage
          MLAW_TAG%NUVAR = MLAW_TAG%NUVAR + 5
        ELSEIF (ILOC == 1) THEN
          IGURSON = 2 ! Non local Forest (Micromorphic)
          MLAW_TAG%NUVAR = MLAW_TAG%NUVAR + 7
        ELSEIF (ILOC == 2) THEN
          IGURSON = 3 ! Non local (Peerling)
          MLAW_TAG%NUVAR = MLAW_TAG%NUVAR + 8
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
        MLAW_TAG%G_DMG    = 1
        MLAW_TAG%L_DMG    = 1
c
        ! Update MATPARAM 
        CALL INIT_MAT_KEYWORD(MATPARAM ,"COMPRESSIBLE")
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

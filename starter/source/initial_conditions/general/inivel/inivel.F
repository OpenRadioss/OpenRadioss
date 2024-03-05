Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE INIVEL(V       ,VR      ,SVR ,ITABM1  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C----------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: ITABM1(2*NUMNOD)
      INTEGER  ,INTENT(IN) :: SVR
      my_real,INTENT(INOUT) :: V(3,NUMNOD),VR(3,SVR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real
     .   V1, V2, V3
      CHARACTER LLINE*ncharline
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  USRTOS
C=======================================================================
C--------------------------------------------------
C     V INI INI Ynn FILE
C--------------------------------------------------
      IF (ISIGI==3.OR.ISIGI==4.OR.ISIGI==5) THEN
  120   READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
  122   IF(LLINE(1:31)/='/NODAL     /VECTOR    /VELOCITY')GOTO 120
        READ(IIN4,FMT='(A)',END=199,ERR=199)LLINE
C
  125   READ(IIN4,FMT='(A)',END=130,ERR=199)LLINE
        IF(LLINE(1:1)=='#')GOTO 125
        IF(LLINE(1:1)=='/')GOTO 130
          IF (IOUTP_FMT==2) THEN
            READ(LLINE,'(I8,3F16.0)')N,V1,V2,V3
          ELSE
            READ(LLINE,'(I10,3F20.0)')N,V1,V2,V3
          ENDIF
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            V(1,I) = V1
            V(2,I) = V2
            V(3,I) = V3
          END IF
        GOTO 125
 130    CONTINUE
 199    CONTINUE
        REWIND(IIN4)
C-------------------
  220   READ(IIN4,FMT='(A)',END=299,ERR=299)LLINE
  222   IF(LLINE(1:31)/='/NODAL     /VECTOR    /VELO_ROT')GOTO 220
        READ(IIN4,FMT='(A)',END=299,ERR=299)LLINE
C
  225   READ(IIN4,FMT='(A)',END=230,ERR=299)LLINE
        IF(LLINE(1:1)=='#')GOTO 225
        IF(LLINE(1:1)=='/')GOTO 230
          IF (IOUTP_FMT==2) THEN
            READ(LLINE,'(I8,3F16.0)')N,V1,V2,V3
          ELSE
            READ(LLINE,'(I10,3F20.0)')N,V1,V2,V3
          ENDIF
          I = USRTOS(N,ITABM1)
          IF(I/=0)THEN
            VR(1,I) = V1
            VR(2,I) = V2
            VR(3,I) = V3
          END IF
        GOTO 225
 230    CONTINUE
 299    CONTINUE
        REWIND(IIN4)
      ENDIF
!
!-----------
      RETURN
      END



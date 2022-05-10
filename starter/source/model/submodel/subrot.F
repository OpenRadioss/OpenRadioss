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
Chd|  SUBROTVECT                    source/model/submodel/subrot.F
Chd|-- called by -----------
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|-- calls ---------------
Chd|        EULER_VROT                    source/model/submodel/euler_vrot.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SUBROTVECT (X,Y,Z,RTRANS,SUB_ID,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  SUB_ID
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   X,Y,Z, RTRANS(NTRANSF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,L,K,CPTZERO,IDSUBMODEL,SUB_LEVEL,CUR_SUBMOD
      my_real
     .   ROT(9),P(3),X0(3)
C======================================================================   
      X0 = ZERO
      IDSUBMODEL = 0
      IF(SUB_ID .NE. 0) THEN
        DO K= 1,NSUBMOD
          IF(LSUBMODEL(K)%NOSUBMOD == SUB_ID) THEN
            IDSUBMODEL = K
            EXIT
          ENDIF
        ENDDO
      ENDIF
      CUR_SUBMOD = IDSUBMODEL
      SUB_LEVEL = LSUBMODEL(IDSUBMODEL)%LEVEL
      DO WHILE (SUB_LEVEL .NE. 0)
        DO I=1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
          IF (LSUBMODEL(CUR_SUBMOD)%IDTRANS(I) .NE. 0) THEN
            CPTZERO = 0
            DO L=1,9
              ROT(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),L+2)
              IF (ROT(L) == ZERO ) CPTZERO = CPTZERO + 1
            ENDDO
            IF(CPTZERO == 9) CYCLE
            P(1) = X
            P(2) = Y
            P(3) = Z
            CALL EULER_VROT (X0,P,ROT)
            X = P(1)
            Y = P(2)
            Z = P(3)
          ENDIF
        ENDDO
        SUB_LEVEL = SUB_LEVEL - 1
        CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
      ENDDO

C---
      RETURN
      END
Chd|====================================================================
Chd|  SUBROTPOINT                   source/model/submodel/subrot.F
Chd|-- called by -----------
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|-- calls ---------------
Chd|        EULER_VROT                    source/model/submodel/euler_vrot.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SUBROTPOINT (X,Y,Z,RTRANS,SUB_ID,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  SUB_ID
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   X,Y,Z, RTRANS(NTRANSF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,K,IDSUBMODEL,ITY,SUB_LEVEL,CUR_SUBMOD
      my_real
     .   ROT(9),P(3),X0(3),TX,TY,TZ,XP,YP,ZP,XCOLD(3),XCNEW(3)
C====================================================================== 
      IDSUBMODEL = 0
      IF(SUB_ID .NE. 0) THEN
        DO K= 1,NSUBMOD
          IF(LSUBMODEL(K)%NOSUBMOD == SUB_ID) THEN
            IDSUBMODEL = K
            EXIT
          ENDIF
        ENDDO
      ENDIF
      CUR_SUBMOD = IDSUBMODEL
      SUB_LEVEL = LSUBMODEL(IDSUBMODEL)%LEVEL
      DO WHILE (SUB_LEVEL .NE. 0)
        DO I=1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
          IF (LSUBMODEL(CUR_SUBMOD)%IDTRANS(I) .NE. 0) THEN
            ITY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),2)
            IF (ITY ==1 )THEN
              TX = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),15)
              TY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),16)
              TZ = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),17)
              X = X + TX
              Y = Y + TY
              Z = Z + TZ
            ELSEIF(ITY ==2 )THEN
              DO L=1,3
        	X0(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),L+11)
              ENDDO
              DO L=1,9
        	ROT(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),L+2)
              ENDDO
              P(1) = X
              P(2) = Y
              P(3) = Z
              CALL EULER_VROT (X0,P,ROT)
              X = P(1)
              Y = P(2)
              Z = P(3)
            ELSEIF(ITY ==3 )THEN
              TX = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),15)
              TY = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),16)
              TZ = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),17)
              DO L=1,9
        	ROT(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),L+2)
              ENDDO
              XP = ROT(1)*X + ROT(4)*Y + ROT(7)*Z + TX
              YP = ROT(2)*X + ROT(5)*Y + ROT(8)*Z + TY
              ZP = ROT(3)*X + ROT(6)*Y + ROT(9)*Z + TZ
              X = XP
              Y = YP
              Z = ZP		
            ELSEIF(ITY ==4 )THEN
              DO K=1,3
                XCOLD(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),K+11)
              ENDDO
              DO K=1,3
                XCNEW(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),K+14)
              ENDDO
              DO K=1,9
                ROT(K)=RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),K+2)
              ENDDO
              XP = X - XCOLD(1)
              YP = Y - XCOLD(2)
              ZP = Z - XCOLD(3)
              X  = XCNEW(1) + ROT(1)*XP + ROT(4)*YP + ROT(7)*ZP                        
              Y  = XCNEW(2) + ROT(2)*XP + ROT(5)*YP + ROT(8)*ZP                         
              Z  = XCNEW(3) + ROT(3)*XP + ROT(6)*YP + ROT(9)*ZP                         
            ENDIF
          ENDIF
        ENDDO
        SUB_LEVEL = SUB_LEVEL - 1
        CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
      ENDDO

C---
      RETURN
      END
Chd|====================================================================
Chd|  SUBROTTENS                    source/model/submodel/subrot.F
Chd|-- called by -----------
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|-- calls ---------------
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SUBROTTENS (TENS,RTRANS,SUB_ID,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  SUB_ID
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   TENS(6), RTRANS(NTRANSF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,L,K,CPTZERO,IDSUBMODEL,SUB_LEVEL,CUR_SUBMOD
      my_real
     .   ROT(9),P(3),X0(3),L11,L22,L33,L12,L23,L13,S11,S12,
     .   S13,S21,S22,S23,S31,S32,S33,R11,R12,
     .   R13,R21,R22,R23,R31,R32,R33   
C======================================================================   
      X0 = ZERO
      IDSUBMODEL = 0
      IF(SUB_ID .NE. 0) THEN
        DO K= 1,NSUBMOD
          IF(LSUBMODEL(K)%NOSUBMOD == SUB_ID) THEN
            IDSUBMODEL = K
            EXIT
          ENDIF
        ENDDO
      ENDIF
      CUR_SUBMOD = IDSUBMODEL
      SUB_LEVEL = LSUBMODEL(IDSUBMODEL)%LEVEL
      DO WHILE (SUB_LEVEL .NE. 0)
        DO I=1,LSUBMODEL(CUR_SUBMOD)%NBTRANS
          IF (LSUBMODEL(CUR_SUBMOD)%IDTRANS(I) .NE. 0) THEN
            CPTZERO = 0
            DO L=1,9
              ROT(L) = RTRANS(LSUBMODEL(CUR_SUBMOD)%IDTRANS(I),L+2)
              IF (ROT(L) == ZERO ) CPTZERO = CPTZERO + 1
            ENDDO
            IF(CPTZERO == 9) CYCLE
             R11 = ROT(1)
             R12 = ROT(2)
             R13 = ROT(3)
             R21 = ROT(4)
             R22 = ROT(5)
             R23 = ROT(6)
             R31 = ROT(7)
             R32 = ROT(8)
             R33 = ROT(9)
             L11    =TENS(1)
             L22    =TENS(2)
             L33    =TENS(3)
             L12    =TENS(4)
             L23    =TENS(5)
             L13    =TENS(6)
             S11    =L11*R11+L12*R12+L13*R13 
             S12    =L11*R21+L12*R22+L13*R23 
             S13    =L11*R31+L12*R32+L13*R33
             S21    =L12*R11+L22*R12+L23*R13
             S22    =L12*R21+L22*R22+L23*R23
             S23    =L12*R31+L22*R32+L23*R33 
             S31    =L13*R11+L23*R12+L33*R13
             S32    =L13*R21+L23*R22+L33*R23
             S33    =L13*R31+L23*R32+L33*R33
             TENS(1)=R11*S11+R12*S21+R13*S31
             TENS(2)=R21*S12+R22*S22+R23*S32
             TENS(3)=R31*S13+R32*S23+R33*S33
             TENS(4)=R11*S12+R12*S22+R13*S32
             TENS(5)=R21*S13+R22*S23+R23*S33
             TENS(6)=R11*S13+R12*S23+R13*S33
          ENDIF
        ENDDO
        SUB_LEVEL = SUB_LEVEL - 1
        CUR_SUBMOD = LSUBMODEL(CUR_SUBMOD)%IFATHER
      ENDDO

C---
      RETURN
      END

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
Chd|  C_JOINT_SMS                   source/constraints/general/cyl_joint/write_count_joint_sms.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_JOINT_SMS(LJOINT,PROC,LJOINT_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LJOINT_L,
     .          LJOINT(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, K, NSN, J
C
      LJOINT_L = NJOINT
      K = 1
      DO N = 1, NJOINT
        NSN=LJOINT(K)
        DO J = 1, NSN
          NN = LJOINT(K+J)
          IF(NLOCAL(NN,PROC+1)==1)THEN
            LJOINT_L = LJOINT_L + 1
          ENDIF
        ENDDO
        K = K + NSN + 1
      ENDDO
C
      RETURN
      END SUBROUTINE C_JOINT_SMS
C
Chd|====================================================================
Chd|  W_JOINT_SMS                   source/constraints/general/cyl_joint/write_count_joint_sms.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_JOINT_SMS(LJOINT,CEP,CEL,PROC,
     +                   NODLOCAL,LJOINT_L,LEN_IA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LJOINT_L, LEN_IA, NODLOCAL(*),
     .          LJOINT(*), CEP(*), CEL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, K, J, NSN, K_L, NSN_L, LJOIN_L(LJOINT_L)
C
      K_L = 1
      K = 1
      DO N = 1, NJOINT
        NSN=LJOINT(K)
        NSN_L = 0
        DO J = 1, NSN
          NN = LJOINT(K+J)
          IF(NLOCAL(NN,PROC+1)==1)THEN
            NSN_L = NSN_L + 1
            LJOIN_L(K_L+NSN_L) = NODLOCAL(NN)
          ENDIF
        ENDDO
        LJOIN_L(K_L) = NSN_L
        K_L = K_L + NSN_L + 1
        K = K + NSN + 1
      ENDDO
C
      CALL WRITE_I_C(LJOIN_L,LJOINT_L)
      LEN_IA = LEN_IA + LJOINT_L
C
      RETURN
      END SUBROUTINE W_JOINT_SMS

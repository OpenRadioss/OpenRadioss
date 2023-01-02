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
Chd|  ZEROR                         source/system/zero.F          
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|        IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|        INT_MATV                      source/implicit/imp_int_k.F   
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        MONV_M3                       source/airbag/monv_imp0.F     
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZEROR(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,N
       A(1,I)=ZERO
       A(2,I)=ZERO
       A(3,I)=ZERO
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  ZERO1                         source/system/zero.F          
Chd|-- called by -----------
Chd|        AUTSPC                        source/constraints/general/bcs/bc_imp0.F
Chd|        CLCEIG                        source/constraints/general/bcs/bc_imp0.F
Chd|        I2RECU0                       source/interfaces/interf/i2_imp2.F
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        IND_SLD                       source/mpi/implicit/imp_fri.F 
Chd|        NRMLZAUSPC                    source/constraints/general/bcs/bc_imp0.F
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3UF                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3UM                        source/constraints/general/rbe3/rbe3f.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        SET_IND_FR                    source/mpi/implicit/imp_fri.F 
Chd|        SMS_RBE3T1                    source/ams/sms_rbe3.F         
Chd|        SMS_RBE3_PREC                 source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZERO1(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,N
       A(I)=ZERO
      ENDDO
      RETURN
      END

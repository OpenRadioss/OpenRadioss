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
Chd|  RAD2R_MOD                     share/modules/rad2r.F         
Chd|-- called by -----------
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|        R2R_INPUT_INIT                source/coupling/rad2rad/r2r_input_init.F
Chd|        R2R_SENDKINE                  source/coupling/rad2rad/r2r_getdata.F
Chd|        R2R_TAGEL                     source/coupling/rad2rad/r2r_exchange.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        SEND_MASS_RBY_SPMD            source/coupling/rad2rad/r2r_init.F
Chd|        SPMD_EXCH_WORK                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IDEF                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IGET2                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_IGET4                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RBY                  source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RSET                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RSET3                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RSET3B               source/mpi/r2r/spmd_r2r.F     
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE RAD2R_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------

      INTEGER, DIMENSION(:), ALLOCATABLE ::      
     .      SOCKET,DBNO,NBELTN_R2R,TCNELT,TCNELTDB,TYPLNK,  
     .      OFFSET,NBELT_R2R,ROTLNK,RBYLNK,TAG_RBY,ADD_RBY,
     .      KINLNK,TAGEL_R2R_SEND,NLLNK,NBDOF_NL,IADD_NL       

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .      DBN,NBEL,NBELN,TBCNEL,TBCNELDB
     
      DOUBLE PRECISION, ALLOCATABLE ::
     .      R2R_KINE(:,:) 
     
      INTEGER R2R_FDR,R2R_FDW,R2R_IPID
          
C-----------------------------------------------
      END MODULE RAD2R_MOD

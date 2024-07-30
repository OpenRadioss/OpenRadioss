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
      !||====================================================================
      !||    rad2r_mod            ../engine/share/modules/rad2r.F
      !||--- called by ------------------------------------------------------
      !||    init_link_spmd       ../engine/source/coupling/rad2rad/r2r_init.F
      !||    r2r_exchange         ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    r2r_getdata          ../engine/source/coupling/rad2rad/r2r_getdata.F
      !||    r2r_init             ../engine/source/coupling/rad2rad/r2r_init.F
      !||    r2r_input_init       ../engine/source/coupling/rad2rad/r2r_input_init.F
      !||    r2r_sendkine         ../engine/source/coupling/rad2rad/r2r_getdata.F
      !||    r2r_tagel            ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    radioss2             ../engine/source/engine/radioss2.F
      !||    resol                ../engine/source/engine/resol.F
      !||    send_mass_rby_spmd   ../engine/source/coupling/rad2rad/r2r_init.F
      !||    spmd_exch_work       ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_idef        ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_iget2       ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_iget4       ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_rby         ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_rset        ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_rset3       ../engine/source/mpi/r2r/spmd_r2r.F
      !||    spmd_r2r_rset3b      ../engine/source/mpi/r2r/spmd_r2r.F
      !||    tagoff3n             ../engine/source/interfaces/interf/chkstfn3.F
      !||====================================================================
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
     
      INTEGER R2R_FDR,R2R_FDW,R2R_IPID,DD_R2R_NL(2)
          
C-----------------------------------------------
      END MODULE RAD2R_MOD

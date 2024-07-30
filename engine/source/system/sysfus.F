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
C     REAL
#include "my_real.inc"
      !||====================================================================
      !||    sysfus        ../engine/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    animx         ../engine/source/output/anim/generate/animx.F
      !||    lcbcsf        ../engine/source/constraints/general/bcs/lcbcsf.F
      !||    leccut        ../engine/source/tools/sect/leccut.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../engine/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../engine/share/message_module/message_mod.F
      !||====================================================================
       my_real  FUNCTION SYSFUS(IU,ITABM1,NUMNOD,MESS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU, NUMNOD
      CHARACTER MESS*40
      INTEGER ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
C-----------------------------------------------
C
      JINF=1
      JSUP=NUMNOD
C  045      J=NUMNOD/2
      J=MIN(1,NUMNOD/2)
   10 IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
        CALL ANCMSG(MSGID=187,ANMODE=ANINFO,
     .            I1=IU,C1=MESS)
        IERR=IERR+1
        SYSFUS=1.1
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         SYSFUS=ITABM1(J+NUMNOD)+0.1
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C
C     REAL
      !||====================================================================
      !||    sysfus2                                  ../engine/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    ale51_spmd2                              ../engine/source/ale/ale51/ale51_spmd2.F
      !||    ale51_spmd3                              ../engine/source/ale/ale51/ale51_spmd3.F
      !||    chkstfn3n                                ../engine/source/interfaces/interf/chkstfn3.F
      !||    count_remote_nb_elem_edge                ../engine/source/interfaces/interf/count_remote_nb_elem_edge.F
      !||    find_edge_from_remote_proc               ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
      !||    find_surface_from_remote_proc            ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
      !||    forcefingeo                              ../engine/source/loads/general/forcefingeo.F
      !||    fr_rlale                                 ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fr_rlink1                                ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    genh3d                                   ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    lcbcsf                                   ../engine/source/constraints/general/bcs/lcbcsf.F
      !||    lecnoise                                 ../engine/source/general_controls/computation/lecnoise.F
      !||    spmd_exchmsr_idel                        ../engine/source/mpi/interfaces/spmd_exchmsr_idel.F
      !||    spmd_exchseg_idel                        ../engine/source/mpi/kinematic_conditions/spmd_exchseg_idel.F
      !||    tagoff3n                                 ../engine/source/interfaces/interf/chkstfn3.F
      !||====================================================================
      INTEGER FUNCTION SYSFUS2(IU,ITABM1,NUMNOD)
C cette fonction retourne le no interne correspondant a un no user ou 0
C si le noeud n'existe pas
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU, NUMNOD
      INTEGER ITABM1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
C-----------------------------------------------
C
      ! Check exit parameters

      ! 1st NUMNOD=0
      IF (NUMNOD==0) THEN
        SYSFUS2=0
        RETURN
      END IF

      ! 2nd NodeID is lower than smallest NodeID.
      IF ( IU-ITABM1(1)<0 ) THEN
        SYSFUS2=0
        RETURN
      ENDIF


      JINF=1
      JSUP=NUMNOD
      J=MIN(1,NUMNOD/2)
   10 IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
C     >CAS NOEUD non trouve
        SYSFUS2=0
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         SYSFUS2=ITABM1(J+NUMNOD)
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END

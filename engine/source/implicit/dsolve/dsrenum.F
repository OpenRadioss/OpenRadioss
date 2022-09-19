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
Chd|  DSRENUM                       source/implicit/dsolve/dsrenum.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSRENUM(GRAPHE, IDDL, NDOF, NDDL0, NDDL_INI0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*), NDOF(*), NDDL0, NDDL_INI0
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NDDL, J, INO, IDL, JJ, LDDLN(2,NDDL0),
     .        RENUM(NDDL_INI0), NDF, NDI, K, LDDLG(NDDL0), NDDL1
C
      DO I=1,NSPROC
C
         NDDL=GRAPHE(I)%NDDL_INI
         NDDL1=0
         DO J=1,NDDL
            INO=GRAPHE(I)%LSDDL_INI(1,J)
            IDL=GRAPHE(I)%LSDDL_INI(2,J)
            IF (NDOF(INO)/=0) NDDL1=NDDL1+1
            JJ=IDDL(INO)+IDL
            RENUM(J)=JJ
            LDDLN(1,JJ)=INO
            LDDLN(2,JJ)=IDL
            LDDLG(JJ)=GRAPHE(I)%LSDDL_GLOB_INI(J)
         ENDDO
         IF (NDDL1/=NDDL0) THEN
            CALL ANCMSG(MSGID=61,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF
         GRAPHE(I)%NDDL_INI=NDDL1
         DO J=1,NDDL1
            GRAPHE(I)%LSDDL_INI(1,J)=LDDLN(1,J)
            GRAPHE(I)%LSDDL_INI(2,J)=LDDLN(2,J)
            GRAPHE(I)%LSDDL_GLOB_INI(J)=LDDLG(J)
         ENDDO
         DO J=1,GRAPHE(I)%NSUPEL
            NDF=GRAPHE(I)%DGRAPH(J)%NDDL_F_INI
            NDI=GRAPHE(I)%DGRAPH(J)%NDDL_I_INI
            DO K=1,NDF+NDI
               GRAPHE(I)%DGRAPH(J)%DDLS_INI(K)=
     .              RENUM(GRAPHE(I)%DGRAPH(J)%DDLS_INI(K))
            ENDDO
         ENDDO
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSRENUM0                      source/implicit/dsolve/dsrenum.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRENUM0(GRAPHE, NDDL_INI0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL_INI0
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      NDDL_INI0 = 0
      DO I=1,NSPROC
         NDDL_INI0=MAX(GRAPHE(I)%NDDL_INI,NDDL_INI0)
      END DO
C
      RETURN
      END

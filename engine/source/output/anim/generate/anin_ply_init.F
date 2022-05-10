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
Chd|  GET_PLY_NODID                 source/output/anim/generate/anin_ply_init.F
Chd|-- called by -----------
Chd|        PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      INTEGER FUNCTION GET_PLY_NODID(IPLY,NODID,OFFSET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,NODID,OFFSET
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,BINF,BSUP,MILIEU,SIZE,EMPL,TAILLE
C-----------------------------------------------
      SIZE=PLYNOD(IPLY)%PLYNUMNODS

      BINF=1
      BSUP=SIZE

      IF (PLYNOD(IPLY)%NODES(BINF)==NODID)THEN
        EMPL=BINF
        GOTO 100
      ENDIF

      IF (PLYNOD(IPLY)%NODES(BSUP)==NODID)THEN
        EMPL=BSUP
        GOTO 100
      ENDIF

      TAILLE = BSUP-BINF+1
      DO WHILE ( TAILLE > 5)
         MILIEU = (BSUP-BINF)/2 + BINF

         IF (PLYNOD(IPLY)%NODES(MILIEU) == NODID) THEN
           EMPL = MILIEU
           GOTO 100
         ENDIF
         IF ( PLYNOD(IPLY)%NODES(MILIEU) < NODID) THEN
           BINF = MILIEU
         ELSE
           BSUP = MILIEU
         ENDIF
         TAILLE = BSUP-BINF+1
      ENDDO

      DO I=BINF,BSUP
        IF  (PLYNOD(IPLY)%NODES(I) == NODID) THEN
           EMPL = I
           GOTO 100
        ENDIF
      ENDDO

 100  CONTINUE

      GET_PLY_NODID = PLYNOD(IPLY)%PLYNODID(EMPL)+OFFSET

      RETURN
      END
Chd|====================================================================
Chd|  GET_PLY_NOD                   source/output/anim/generate/anin_ply_init.F
Chd|-- called by -----------
Chd|        PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      INTEGER FUNCTION GET_PLY_NOD(IPLY,NODID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,NODID
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,BINF,BSUP,MILIEU,SIZE,EMPL,TAILLE
C-----------------------------------------------
      SIZE=PLYNOD(IPLY)%PLYNUMNODS

      BINF=1
      BSUP=SIZE

      IF (PLYNOD(IPLY)%NODES(BINF)==NODID)THEN
        EMPL=BINF
        GOTO 100
      ENDIF

      IF (PLYNOD(IPLY)%NODES(BSUP)==NODID)THEN
        EMPL=BSUP
        GOTO 100
      ENDIF

      TAILLE = BSUP-BINF+1
      DO WHILE ( TAILLE > 5)
         MILIEU = (BSUP-BINF)/2 + BINF

         IF (PLYNOD(IPLY)%NODES(MILIEU) == NODID) THEN
           EMPL = MILIEU
           GOTO 100
         ENDIF
         IF ( PLYNOD(IPLY)%NODES(MILIEU) < NODID) THEN
           BINF = MILIEU
         ELSE
           BSUP = MILIEU
         ENDIF
         TAILLE = BSUP-BINF+1
      ENDDO

      DO I=BINF,BSUP
        IF  (PLYNOD(IPLY)%NODES(I) == NODID) THEN
           EMPL = I
           GOTO 100
        ENDIF
      ENDDO

 100  CONTINUE

      GET_PLY_NOD = EMPL

      RETURN
      END


Chd|====================================================================
Chd|  GET_PLY_ELEM                  source/output/anim/generate/anin_ply_init.F
Chd|-- called by -----------
Chd|        DELNUMBC_PLY                  source/output/anim/generate/delnumbc_ply.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      INTEGER FUNCTION GET_PLY_ELEM(IPLY,ELEMID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,ELEMID
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,BINF,BSUP,MILIEU,SIZE,EMPL,TAILLE
C-----------------------------------------------
      SIZE=PLYSHELL(IPLY)%PLYNUMSHELL

      BINF=1
      BSUP=SIZE

      IF (PLYSHELL(IPLY)%SHID(BINF)==ELEMID)THEN
        EMPL=BINF
        GOTO 100
      ENDIF

      IF (PLYSHELL(IPLY)%SHID(BSUP)==ELEMID)THEN
        EMPL=BSUP
        GOTO 100
      ENDIF

      TAILLE = BSUP-BINF+1
      DO WHILE ( TAILLE > 5)
         MILIEU = (BSUP-BINF)/2 + BINF

         IF (PLYSHELL(IPLY)%SHID(MILIEU) == ELEMID) THEN
           EMPL = MILIEU
           GOTO 100
         ENDIF
         IF ( PLYSHELL(IPLY)%SHID(MILIEU) < ELEMID) THEN
           BINF = MILIEU
         ELSE
           BSUP = MILIEU
         ENDIF
         TAILLE = BSUP-BINF+1
      ENDDO

      DO I=BINF,BSUP
        IF  (PLYSHELL(IPLY)%SHID(I) == ELEMID) THEN
           EMPL = I
           GOTO 100
        ENDIF
      ENDDO

 100  CONTINUE

      GET_PLY_ELEM = EMPL

      RETURN
      END



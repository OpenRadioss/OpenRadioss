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
Chd|  I10BUCE                       source/interfaces/intsort/i10buce.F
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I10TRI                        source/interfaces/intsort/i10tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE I10BUCE(
     1   X       ,IRECT   ,NSV     ,NMN     ,NRTM    ,
     2   NSN     ,NCONT   ,CAND_E  ,CAND_N  ,GAP     ,
     3   NOINT   ,II_STOK ,TZINF   ,MAXBOX  ,MINBOX  ,
     4   NB_N_B  ,ESHIFT  ,BMINMA  ,MWAG    ,ILD     ,
     7   NCONTACT,NSNROLD ,STFN    ,NIN     ,IGAP    ,
     8   GAP_S   ,NSNR    ,RENUM   ,STF     ,GAP_M   ,
     9   GAPMIN  ,GAPMAX  ,I_MEM         )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT, NIN, IGAP, NSNR, NSNROLD
      INTEGER IRECT(4,*),NSV(*),MWAG(*)
      INTEGER CAND_E(*),CAND_N(*),RENUM(*)
      INTEGER ILD,NB_N_B,ESHIFT,NCONTACT,NCONT,I_MEM,II_STOK
     .        
C     REAL
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,GAPMIN, GAPMAX, BMINMA(6)
      my_real
     .   X(3,*),STFN(*), GAP_S(*), STF(*), GAP_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
      INTEGER I, J,I_ADD, IP0, IP1, MAXSIZ,
     .        ADD(2,I_ADD_MAX), LOC_PROC, N,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6,I_ADD_MAX-1),MARGE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C----- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C
C     POINTEUR  NOM                            TAILLE
C     P0........CAND_A:Adresse de N ds CAND_N  NSN + 3[+ NSNROLD dans le cas SPMD]
C     P1........Elt Bas Pile                   NRTM
C     P2........Elt PILE                       2*NRTM
C     P21.......BPN                            NSN
C     P22.......PN                             NSN
C     P31.......ADDI                           2*I_ADD_MAX
      MAXSIZ = 3*(NRTM+100)
      IP0 = 1
      IP1 = IP0 + NSN + NSNROLD + 3
C
C
C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
C
      ADD(1,1) = 0
      ADD(2,1) = 0
      ADD(1,2) = 0
      ADD(2,2) = 0
      I_ADD = 1
      XYZM(1,I_ADD) = BMINMA(4)
      XYZM(2,I_ADD) = BMINMA(5)
      XYZM(3,I_ADD) = BMINMA(6)
      XYZM(4,I_ADD) = BMINMA(1)
      XYZM(5,I_ADD) = BMINMA(2)
      XYZM(6,I_ADD) = BMINMA(3)
      I_MEM = 0
C
C
C-----DEBUT DE LA PHASE DE TRI 
C 
C       SEPARER B ET N EN TWO
      MARGE = TZINF-GAP  ! il s agit bien de la marge
      CALL I10TRI(
     1   ADD     ,NSN     ,RENUM    ,NSNR     ,NRTM     ,    
     2   IRECT   ,X       ,XYZM     ,IGAP     ,GAP      ,
     3   I_ADD   ,NSV     ,MAXSIZ   ,II_STOK  ,CAND_N   ,  
     4   CAND_E  ,NCONTACT,NOINT    ,TZINF    ,MAXBOX   ,
     5   MINBOX  ,I_MEM   ,NB_N_B   ,I_ADD_MAX,MWAG(IP0),
     6   ESHIFT  ,NSNROLD ,STF      ,STFN     ,GAP_S    ,
     7   GAP_M   ,GAPMIN  ,GAPMAX   ,MARGE    ,NIN      )
C---------------------------------
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF (I_MEM == 2) RETURN
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM==2) THEN
        IF(DEBUG(1)>=1) THEN
          IWARN = IWARN+1
#include "lockon.inc"
          WRITE(ISTDO,*)' **WARNING INTERFACE/MEMORY'
          WRITE(IOUT,*)' **WARNING INTERFACE NB:',NOINT
          WRITE(IOUT,*)'       TOO MANY POSSIBLE IMPACTS'
          WRITE(IOUT,*)'       SIZE OF INFLUENCE ZONE IS'
          WRITE(IOUT,*)'       MULTIPLIED BY 0.75'
#include "lockoff.inc"
        ENDIF
        TZINF = THREE_OVER_4*TZINF
C ne pas dimunuer la taille des boite
C        MINBOX= THREE_OVER_4*MINBOX
C        MAXBOX= THREE_OVER_4*MAXBOX
        IF( TZINF<=GAP ) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM==3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=90,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END

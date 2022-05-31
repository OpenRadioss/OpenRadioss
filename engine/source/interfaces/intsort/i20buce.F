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
Chd|  I20BUCE                       source/interfaces/intsort/i20buce.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I20TRI                        source/interfaces/intsort/i20tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE I20BUCE(
     1   XA       ,IRECT   ,NSV     ,INACTI  ,CAND_P  ,
     2   NMN     ,NRTM    ,NSN     ,CAND_E  ,CAND_N ,
     3   GAP     ,NOINT   ,II_STOK ,TZINF   ,MAXBOX ,
     4   MINBOX  ,MWAG    ,CURV_MAX,NCONTACT,BMINMA ,
     5   NB_N_B  ,ESHIFT  ,ILD     ,IFQ     ,IFPEN  ,
     6   STFA    ,NIN     ,STF     ,IGAP    ,GAP_S  ,
     7   NSNR    ,NCONT   ,RENUM   ,NSNROLD ,GAP_M  ,
     8   GAPMIN  ,GAPMAX  ,NUM_IMP ,NLN     ,NLG    ,
     9   GAP_SH  ,NBINFLG ,MBINFLG ,ISYM    ,I_MEM  )
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,IDT,INACTI,IFQ, NIN, NSNR, NSNROLD
      INTEGER IRECT(4,*),NSV(*),MWAG(*), RENUM(*),NUM_IMP,NLN,ISYM
      INTEGER CAND_E(*),CAND_N(*),IFPEN(*),NLG(*)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B, IGAP, NCONT,
     .        NBINFLG(*), MBINFLG(*),I_MEM,II_STOK
C     REAL ou REAL*8
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,
     .   GAPMIN, GAPMAX,
     .   BMINMA(6)
      my_real
     .   XA(3,*), CAND_P(*), STFA(*),
     .   STF(*), GAP_S(*), GAP_M(*), GAP_SH(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
C
      INTEGER I, J,  I_ADD, IP0, IP1, MAXSIZ,
     .        ADD(2,I_ADD_MAX), LOC_PROC, N, ISZNSNR,
     .        NSNFIOLD(NSPMD)
C     REAL ou REAL*8
      my_real
     .        XYZM(6,I_ADD_MAX-1), MARGE
      my_real
     .        XXX,YYY,ZZZ,CURV_MAX(NRTM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C     POINTEUR  NOM                 TAILLE
C     P0........                    NSN + 3 [+ NSNROLD dans le cas SPMD]
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            2*NRTM
C     P21.......BPN                 NSN
C     P22.......PN                  NSN
C     P31.......ADDI                2*I_ADD_MAX
      MAXSIZ = 3*(NRTM+100)
C 
      IP0 = 1
      IP1 = IP0 + NSN + NSNROLD + 3

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

      IF(IMACH.EQ.3.AND.
     +  (INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0
     +        .OR.NUM_IMP.GT.0))THEN
        ISZNSNR = NSNR
      ELSE
        ISZNSNR = 0
      END IF
C
C-----DEBUT DE LA PHASE DE TRI 
C
C       SEPARER B ET N EN TWO
C
      MARGE = TZINF-GAP  ! il s agit bien de la marge
      CALL I20TRI(
     1   ADD     ,NSN     ,RENUM    ,NSNR     ,ISZNSNR ,
     2   IRECT   ,XA      ,STF      ,STFA     ,XYZM    ,
     3   I_ADD   ,NSV     ,MAXSIZ   ,II_STOK  ,CAND_N  ,
     4   CAND_E  ,NCONTACT,NOINT    ,TZINF    ,MAXBOX  ,
     5   MINBOX  ,I_MEM   ,NB_N_B   ,I_ADD_MAX,ESHIFT  ,
     6   INACTI  ,IFQ     ,MWAG(IP0),CAND_P   ,IFPEN   ,
     7   NRTM    ,NSNROLD ,IGAP     ,GAP      ,GAP_S   ,
     6   GAP_M   ,GAPMIN  ,GAPMAX   ,MARGE    ,CURV_MAX, 
     7   NIN     ,GAP_SH  ,NBINFLG ,MBINFLG  ,ISYM     )
C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE
      IF (I_MEM == 2) RETURN
      IF(I_MEM.EQ.1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. NCONT) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.2) THEN
        IF(DEBUG(1).GE.1) THEN
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
        IF( TZINF.LE.GAP ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I20BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.3)THEN
        NB_N_B = NB_N_B + 1

        IF ( NB_N_B .GT. NCONT) THEN
          CALL ANCMSG(MSGID=99,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I20BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I20BUC_EDGE                   source/interfaces/intsort/i20buce.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I20TRI_EDGE                   source/interfaces/intsort/i20tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE I20BUC_EDGE(
     1   XA      ,IXLINS  ,IXLINM   ,NLG     ,
     2   NLINSA  ,NMNE     ,NLINMA  ,CAND_M  ,CAND_S  ,
     3   GAP     ,NOINT    ,II_STOKE,BMINMA  ,TZINF   ,
     4   MAXBOX  ,MINBOX   ,NB_N_B  , ESHIFT ,ILD     ,
     6   NCONTACT,ADDCM    ,CHAINE  ,NIN     ,ITAB    ,
     7   NLINSR  ,NCONT    ,GAP_S   ,STIFS   , PENIS  ,
     8   IGAP    ,STIFM    ,IAUTO   , I_MEM)
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMNE, NLINMA, NSNE, NOINT,IDT,NLINSA,NIN, NLINSR, INACTI,
     .        IGAP,IAUTO
      INTEGER IXLINS(2,*),IXLINM(2,*),ADDCM(*),CHAINE(2,*)
      INTEGER CAND_M(*),CAND_S(*),NLG(*)
      INTEGER ESHIFT,ILD, NB_N_B, NCONTACT, NCONT,
     .        ITAB(*), I_MEM,II_STOKE
C     REAL ou REAL*8
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,
     .   BMINMA(6)
      my_real
     .   XA(3,*),GAP_S(*),STIFS(*),PENIS(2,*),STIFM(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
C
      INTEGER I, J, N1, N2, I_ADD, MAXSIZ,
     .        ADD(2,I_ADD_MAX), N
C     REAL ou REAL*8
      my_real
     .        XYZM(6,I_ADD_MAX-1)
      INTEGER NB_OLD(2,I_ADD_MAX+1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
C
C-----2- TRI PAR BOITES 
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NLINM
C     P2........Elt PILE            3*NLINM
C     P21.......Elt Bas Pile        NLINS
C     P22.......Elt PILE            3*NLINS
      MAXSIZ = 3*(MAX(NLINMA,NLINSA+NLINSR)+100)

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

      DO I=1,NLINMA
        ADDCM(I)=0
      ENDDO
C
C
C-----DEBUT DE LA PHASE DE TRI 
C 
C       SEPARER B ET N EN TWO
      CALL I20TRI_EDGE(
     1   ADD     ,XA      ,NLG      ,
     2   IXLINS  ,IXLINM  ,NLINMA   ,NLINSR   ,
     3   XYZM    ,I_ADD   ,MAXSIZ   ,II_STOKE ,CAND_S ,
     4   CAND_M  ,NCONTACT,NOINT    ,TZINF    ,MAXBOX ,
     5   MINBOX  ,I_MEM   ,NB_N_B   ,I_ADD_MAX,ESHIFT ,
     6   ADDCM   ,CHAINE  ,NLINSA   ,ITAB     ,NB_OLD ,
     7   STIFS   ,STIFM   ,IAUTO    ,NIN      ) 
C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF (I_MEM == 1 .OR. I_MEM==2)RETURN
      IF(I_MEM.EQ.1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. MAX(NLINMA,NLINSA)) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.2) THEN
        IF(DEBUG(1).GE.1) THEN
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
        IF( TZINF.LE.GAP ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I20BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. MAX(NLINMA,NLINSA)) THEN
          CALL ANCMSG(MSGID=99,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I20BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END

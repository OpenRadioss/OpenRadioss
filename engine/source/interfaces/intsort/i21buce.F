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
Chd|  I21BUCE                       source/interfaces/intsort/i21buce.F
Chd|-- called by -----------
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I21TRI                        source/interfaces/intsort/i21tri.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I21BUCE(
     1   X        ,IRECT   ,NSV      ,INACTI  ,
     2   NRTM     ,NSN     ,CAND_E   ,CAND_N  ,GAP      ,
     3   NOINT   ,II_STOK  ,TZINF    , MAXBOX ,MINBOX   ,
     4   NCONTACT ,XMIN     ,XMAX    ,YMIN     ,
     5   YMAX    ,ZMIN     ,ZMAX     ,NB_N_B  ,ESHIFT   ,
     6   ILD     ,INIT     ,WEIGHT   ,STFN    ,NIN      ,
     7   STF      ,IGAP    ,GAP_S    ,GAPMIN  ,GAPMAX   ,
     8   ICURV    ,NUM_IMP ,XM0      ,NOD_NORMAL,
     9   DEPTH    ,MARGEREF,LXM      ,LYM     ,LZM      ,
     A   NRTM_L   ,XLOC    ,I_MEM    ,DRAD    ,NMN      ,
     B   INTTH    ,MNDD    ,MSR_L    ,ITASK   ,IRECTT   ,
     C   IFORM    ,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_GLOB_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, NSN, NOINT,IDT,INACTI,NIN,NRTM_L,NMN, IFORM
      INTEGER IRECT(4,*), NSV(*), NUM_IMP,MSR_L(*),MNDD(*)
      INTEGER CAND_E(*),CAND_N(*)
      INTEGER NCONTACT,ESHIFT,ILD,INIT,NB_N_B, IGAP,ICURV,
     .        WEIGHT(*),II_STOK,INTTH,ITASK,IRECTT(4,*)
C     REAL
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,
     .   XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN, GAPMIN, GAPMAX, DEPTH,
     .   MARGEREF, LXM, LYM, LZM
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD , DRAD
      my_real
     .   X(3,*), STFN(*), STF(*), GAP_S(*), 
     .   XM0(3,*), NOD_NORMAL(3,*), XLOC(3,*)
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
      INTEGER I, J, I_MEM, I_ADD, IP0, IP1, MAXSIZ,II,
     .        ADD(2,I_ADD_MAX), N,L,PP,J_STOK,IAD(NSPMD),
     .        TAG(NMN),NM(4), IERROR1,NODFI,PTR, IERROR2, IERROR3,
     .        IERROR4,LSKYFI
C     REAL
      my_real
     .        XYZM(6,I_ADD_MAX-1)
      my_real
     .        STF_L(NRTM)
      my_real
     .        XXX,YYY,ZZZ,CURV_MAX(NRTM),CURV_MAX_MAX, MARGE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C a revoir :
      CURV_MAX_MAX = ZERO
      IF(ICURV==3)THEN
          DO I=1,NRTM
            XXX=MAX(XM0(1,IRECT(1,I)),XM0(1,IRECT(2,I)),
     .              XM0(1,IRECT(3,I)),XM0(1,IRECT(4,I)))
     .         -MIN(XM0(1,IRECT(1,I)),XM0(1,IRECT(2,I)),
     .              XM0(1,IRECT(3,I)),XM0(1,IRECT(4,I)))
            YYY=MAX(XM0(2,IRECT(1,I)),XM0(2,IRECT(2,I)),
     .              XM0(2,IRECT(3,I)),XM0(2,IRECT(4,I)))
     .         -MIN(XM0(2,IRECT(1,I)),XM0(2,IRECT(2,I)),
     .              XM0(2,IRECT(3,I)),XM0(2,IRECT(4,I)))
            ZZZ=MAX(XM0(3,IRECT(1,I)),XM0(3,IRECT(2,I)),
     .              XM0(3,IRECT(3,I)),XM0(3,IRECT(4,I)))
     .         -MIN(XM0(3,IRECT(1,I)),XM0(3,IRECT(2,I)),
     .              XM0(3,IRECT(3,I)),XM0(3,IRECT(4,I)))
            CURV_MAX(I) = HALF * MAX(XXX,YYY,ZZZ)
            CURV_MAX_MAX = MAX(CURV_MAX_MAX,CURV_MAX(I))
          ENDDO
      ELSE
          DO I=1,NRTM
            CURV_MAX(I)=ZERO
          ENDDO
      ENDIF
C--------------------------------------------------
      IF (INIT.EQ.1) THEN
C premier appel a i21buce
C--------------------------------------------------
C      CAS RECALCUL DU TRI PAR BUCKETS
C--------------------------------------------------
        IF (DEBUG(3).GE.1) THEN
#include "lockon.inc"
          WRITE(ISTDO,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
     .                ' AT CYCLE ',NCYCLE
          WRITE(IOUT,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
     .                ' AT CYCLE ',NCYCLE
#include "lockoff.inc"
        ENDIF
C
C--------------------------------
C     CALCUL DES BORNES DU DOMAINE
C--------------------------------
        XMIN=EP30
        XMAX=-EP30
        YMIN=EP30
        YMAX=-EP30
        ZMIN=EP30
        ZMAX=-EP30
C
        DO I=1,NSN
          J=NSV(I)
C stfn = 0 <=> shooting nodes
          IF(STFN(I).NE.ZERO) THEN
           XMIN= MIN(XMIN,XLOC(1,I))
           YMIN= MIN(YMIN,XLOC(2,I))
           ZMIN= MIN(ZMIN,XLOC(3,I))
           XMAX= MAX(XMAX,XLOC(1,I))
           YMAX= MAX(YMAX,XLOC(2,I))
           ZMAX= MAX(ZMAX,XLOC(3,I))
          ENDIF
        ENDDO
C
        XMIN=XMIN-LXM
        YMIN=YMIN-LYM
        ZMIN=ZMIN-LZM
        XMAX=XMAX+LXM
        YMAX=YMAX+LYM
        ZMAX=ZMAX+LZM
C
        IF(ABS(ZMAX-ZMIN).GT.2*EP30.OR.
     +     ABS(YMAX-YMIN).GT.2*EP30.OR.
     +     ABS(XMAX-XMIN).GT.2*EP30)THEN
          IF (ISTAMPING == 1)THEN
            CALL ANCMSG(MSGID=101,ANMODE=ANINFO,
     .                  I1=NOINT,I2=NOINT)
          ELSE
            CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT,C1='(I21BUCE)')
          ENDIF
          CALL ARRET(2)
        END IF
        XMIN=XMIN-TZINF
        YMIN=YMIN-TZINF
        ZMIN=ZMIN-TZINF
        XMAX=XMAX+TZINF
        YMAX=YMAX+TZINF
        ZMAX=ZMAX+TZINF
C-----------------------------------------------
        NRTM_L=0
        DO I=1,NRTM
          STF_L(I)=ZERO
          IF(STF(I)/=ZERO)THEN
            DO J=1,4
              XXX=XM0(1,IRECT(J,I))
              YYY=XM0(2,IRECT(J,I))
              ZZZ=XM0(3,IRECT(J,I))
              IF(XMIN <= XXX .AND. XXX <= XMAX .AND.
     .           YMIN <= YYY .AND. YYY <= YMAX .AND.
     .           ZMIN <= ZZZ .AND. ZZZ <= ZMAX)THEN

                 NRTM_L=NRTM_L+1
                 STF_L(I)=ONE
                 EXIT

              END IF
            END DO
            END IF
        ENDDO
C
        NB_N_B = 1
      ENDIF
C Fin initialisation
C-----------------------------------------------
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C     POINTEUR  NOM                 TAILLE
C     P0........                    NSN + 3
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            2*NRTM
C     P21.......BPN                 NSN
C     P22.......PN                  NSN
C     P31.......ADDI                2*I_ADD_MAX
      MAXSIZ = 3*(NRTM_L+100)
C 
      IP0 = 1
      IP1 = IP0 + NSN  + 3
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
      XYZM(1,I_ADD) = XMIN
      XYZM(2,I_ADD) = YMIN
      XYZM(3,I_ADD) = ZMIN
      XYZM(4,I_ADD) = XMAX
      XYZM(5,I_ADD) = YMAX
      XYZM(6,I_ADD) = ZMAX
      I_MEM = 0
C
C-----DEBUT DE LA PHASE DE TRI 
C
C       SEPARER B ET N EN TWO
C
C     MARGE plus importante que dans le critere de tri.
      MARGE = TZINF - MAX(DEPTH,GAP + DGAPLOAD,DRAD)
      CALL I21TRI(
     1   ADD      ,NSN      ,IRECT   ,XLOC      ,STF_L    ,
     2   STFN     ,XYZM     ,I_ADD   ,MAXSIZ    ,II_STOK  ,
     3   CAND_N   ,CAND_E  ,NCONTACT  ,NOINT    ,TZINF    ,
     4   MAXBOX   ,MINBOX  ,I_MEM     ,NB_N_B   ,I_ADD_MAX,
     5   ESHIFT   ,INACTI  ,NRTM      ,IGAP     ,GAP      ,
     6   GAP_S    ,GAPMIN  ,GAPMAX    ,MARGE    ,CURV_MAX ,
     7   XM0      ,NOD_NORMAL,DEPTH   ,DRAD    ,DGAPLOAD )

      IF (I_MEM == 2) RETURN
C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF(I_MEM.EQ.1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. NSN) THEN
          IF (ISTAMPING == 1)THEN
            CALL ANCMSG(MSGID=101,ANMODE=ANINFO,
     .                  I1=NOINT,I2=NOINT)
          ELSE
            CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .                  I1=NOINT)
          ENDIF
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
        MINBOX= THREE_OVER_4*MINBOX
        MAXBOX= THREE_OVER_4*MAXBOX
        IF( TZINF.LE.MAX(DEPTH,GAP+ DGAPLOAD,DRAD) ) THEN
          IF (ISTAMPING == 1)THEN
            CALL ANCMSG(MSGID=101,ANMODE=ANINFO,
     .                  I1=NOINT,I2=NOINT)
          ELSE
            CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .                I1=NOINT,C1='(I21BUCE)')
          ENDIF
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. NSN) THEN
          IF (ISTAMPING == 1)THEN
            CALL ANCMSG(MSGID=101,ANMODE=ANINFO,
     .                  I1=NOINT,I2=NOINT)
          ELSE
            CALL ANCMSG(MSGID=99,ANMODE=ANINFO,
     .            I1=NOINT)
          ENDIF
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF             
C
      RETURN
      END

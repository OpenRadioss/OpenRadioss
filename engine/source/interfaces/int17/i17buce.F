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
Chd|  I17BUCE_PENA                  source/interfaces/int17/i17buce.F
Chd|-- called by -----------
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        I17TRI                        source/interfaces/int17/i17tri.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|====================================================================
       SUBROUTINE I17BUCE_PENA(
     1   NELES    ,IXS        ,IXS16    ,IXS20    ,NELEM    ,
     2   NME      ,LWAT       ,NMES     ,CAND_E   ,CAND_N   ,
     3   NOINT    ,I_STOK_GLOB,TZINF    ,MINBOX   ,EMINXM   ,
     4   XSAV     ,ITASK      ,X        ,V        ,A        ,
     5   MX_CAND  ,EMINXS     ,ESH_T    ,FROTS    ,KS       ,
     6   NIN      ,NMESR      ,NB_N_B   ,BMINMA   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ICONTACT_MOD
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
      INTEGER NME, NMES, NOINT,IDT,ITASK,MX_CAND,
     .        ESH_T, I_STOK_GLOB, NIN, NMESR, NB_N_B 
      INTEGER CAND_E(*),CAND_N(*),IXS(NIXS,*),IXS16(8,*),
     .        LWAT,NELEM(*),NELES(*),IXS20(12,*)
C     REAL
      my_real
     .   TZINF,MINBOX
      my_real
     .   X(3,*),EMINXM(6,*),EMINXS(6,*),XSAV(3,*),V(3,*) ,A(3,*),
     .   FROTS(7,*), KS(2,*),BMINMA(6)  
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
      INTEGER I, J, K, L, CONT,I_STOK   ,
     .        IP0, IP1, IP2, IP21, IP22, IP31, MAXSIZ,
     .        NMES_F,NMES_L, MAXSIZS, I_ADD
      INTEGER IERR1,IERR2
      my_real
     .        XYZM(6,I_ADD_MAX-1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C-----------------------------------------------
C     nouvelle phase de tri
C-----------------------------------------------
      CONT = 1
C-----------------------------------------------
C     Boucle sur les retris
C-----------------------------------------------
      DO WHILE (CONT/=0)
C -------------------------------------------------------------
C      CALCUL DES BORNES DU DOMAINE
C      avant I17TRI pour detection candidats remote et allocation en SPMD
C -------------------------------------------------------------
C   
        I_ADD = 1
        XYZM(1,I_ADD) = BMINMA(1)
        XYZM(2,I_ADD) = BMINMA(2)
        XYZM(3,I_ADD) = BMINMA(3)
        XYZM(4,I_ADD) = BMINMA(4)
        XYZM(5,I_ADD) = BMINMA(5)
        XYZM(6,I_ADD) = BMINMA(6)
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NME+100
C     P2........Elt PILE            3*(NME+100)
C     P21.......BPN                 NMES+100
C     P22.......PN                  3*(NMES+100)
        MAXSIZ = 3*(NME+100) 
        MAXSIZS = 3*(NMES+NMESR+100) 
C      Allocation directement dans i17tri pour tenir compte de NMESR
C      IP1 = 1
C      IP2 = IP1+NME+100
C      IP21= IP2+MAXSIZ
C      IP22= IP21+NMES+100
C      IP31= IP22+MAXSIZS
C -------------------------------------------------------------
C  Allocation tableau chaine apres calculs SPMD elt remote
C -------------------------------------------------------------
        IF(ITASK == 0)THEN
          ALLOCATE (ADCHAINE(NMES+NMESR),STAT=IERR1)
          ALLOCATE (CHAINE(2,MX_CAND),STAT=IERR2)
          IF(IERR1+IERR2 /= 0)THEN
            CALL ARRET(2)
          ENDIF
        ENDIF
C -------------------------------------------------------------
        CALL MY_BARRIER
C -------------------------------------------------------------
        NMES_F = 1 + ITASK*(NMES+NMESR) / NTHREAD
        NMES_L = (ITASK+1)*(NMES+NMESR) / NTHREAD
	ADCHAINE(NMES_F:NMES_L) = 0
C	ADCHAINE(NMES_F:NMES_L+NMESR) = 0
        CHAINE(1,1:MX_CAND) = 0
        CHAINE(2,1:MX_CAND) = 0
        MX_AD = 0
C -------------------------------------------------------------
        CALL MY_BARRIER
C -------------------------------------------------------------
        CALL I17TRI(
     2   TZINF   ,IXS     ,IXS16    ,IXS20    ,NELEM    ,
     3   NELES   ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,
     5   CONT    ,NB_N_B  ,EMINXM   ,I_STOK_GLOB,NME    ,
     6   ITASK   ,NOINT   ,X        ,V        ,A        ,
     7   MX_CAND ,EMINXS  ,ESH_T    ,MAXSIZS  ,I_ADD_MAX,
     8   XYZM    ,NMES    ,NMESR    ,NIN      )
C -------------------------------------------------------------
        CALL MY_BARRIER
        IF(ITASK == 0)THEN
          DEALLOCATE (ADCHAINE)
          DEALLOCATE (CHAINE)
        ENDIF      
C -------------------------------------------------------------
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I17BUCE                       source/interfaces/int17/i17buce.F
Chd|-- called by -----------
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I17TRI                        source/interfaces/int17/i17tri.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_TRI17BOX                 source/mpi/interfaces/spmd_tri17box.F
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE I17BUCE(
     1   NELES    ,IXS        ,IXS16    ,IXS20    ,NELEM    ,
     2   NME      ,LWAT       ,NMES     ,CAND_E   ,CAND_N   ,
     3   NOINT    ,I_STOK_GLOB,TZINF    ,MINBOX   ,EMINXM   ,
     4   XSAV     ,ITASK      ,X        ,V        ,A        ,
     5   MX_CAND  ,EMINXS     ,ESH_T    ,FROTS    ,KS       ,
     6   ISENDTO  ,IRCVFROM   ,WEIGHT   ,NIN      ,NMESR    ,
     7   VCOM     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ICONTACT_MOD
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
      INTEGER NME, NMES, NOINT,IDT,ITASK,MX_CAND,
     .        ESH_T, I_STOK_GLOB, NIN, NMESR 
      INTEGER CAND_E(*),CAND_N(*),IXS(NIXS,*),IXS16(8,*),
     .        LWAT,NELEM(*),NELES(*),IXS20(12,*),
     .        ISENDTO(*), IRCVFROM(*), WEIGHT(*)
C     REAL
      my_real
     .   TZINF,MINBOX
      my_real
     .   X(3,*),EMINXM(6,*),EMINXS(6,*),XSAV(3,*),V(3,*) ,A(3,*),
     .   FROTS(7,*), KS(2,*), VCOM(3,*)  
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
      INTEGER I, J, K, L, CONT,NB_N_B,I_STOK   ,
     .        IP0, IP1, IP2, IP21, IP22, IP31, MAXSIZ,
     .        NMES_F,NMES_L, MAXSIZS, I_ADD
      INTEGER IERR1,IERR2
      my_real
     .        XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     .        XYZM(6,I_ADD_MAX-1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
c fait dans ICOMCRIT
c      IF (DEBUG(3)>=1) THEN
c#include "lockon.inc"
c          WRITE(ISTDO,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
c     .                ' AT CYCLE ',NCYCLE
c          WRITE(IOUT,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
c     .                ' AT CYCLE ',NCYCLE
c#include "lockoff.inc"
c      ENDIF
C-----------------------------------------------
      NMES_F = 1 + ITASK*NMES / NTHREAD
      NMES_L = (ITASK+1)*NMES / NTHREAD
      DO K=1,8
          DO I=1+ESH_T,NME+ESH_T
            J=IXS(K+1,NELEM(I))
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
            J=IXS16(K,NELEM(I)-NUMELS8-NUMELS10-NUMELS20)
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          ENDDO
          DO I=NMES_F,NMES_L
            J=IXS(K+1,NELES(I))
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
            J=IXS16(K,NELES(I)-NUMELS8-NUMELS10-NUMELS20)
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          ENDDO
      ENDDO
C -------------------------------------------------------------
      CALL MY_BARRIER
C -------------------------------------------------------------
C-----------------------------------------------
      NB_N_B = 1
C Fin initialisation
C-----------------------------------------------
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C-----------------------------------------------
C     nouvelle phase de tri
C-----------------------------------------------
      CONT = 1
C-----------------------------------------------
C     Boucle sur les retris
C-----------------------------------------------
      DO WHILE (CONT/=0)
C -------------------------------------------------------------
C      CALCUL DES BORNES DU DOMAINE
C      avant I17TRI pour detection candidats remote et allocation en SPMD
C -------------------------------------------------------------
        XMIN = EP30
        YMIN = EP30
        ZMIN = EP30
        XMAX = -EP30
        YMAX = -EP30
        ZMAX = -EP30
C      
        DO L=1,NME                       ! NME = NME_T en SMP
          I = L + ESH_T
          XMIN = MIN( XMIN , EMINXM(1,I) )
          YMIN = MIN( YMIN , EMINXM(2,I) )
          ZMIN = MIN( ZMIN , EMINXM(3,I) )
          XMAX = MAX( XMAX , EMINXM(4,I) )
          YMAX = MAX( YMAX , EMINXM(5,I) )
          ZMAX = MAX( ZMAX , EMINXM(6,I) )
        ENDDO
C
        IF(ABS(ZMAX-ZMIN)>2*EP30.OR.
     +     ABS(YMAX-YMIN)>2*EP30.OR.
     +     ABS(XMAX-XMIN)>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        END IF
C   
        XMIN = XMIN - TZINF
        YMIN = YMIN - TZINF
        ZMIN = ZMIN - TZINF
        XMAX = XMAX + TZINF
        YMAX = YMAX + TZINF
        ZMAX = ZMAX + TZINF
        I_ADD = 1
        XYZM(1,I_ADD) = XMIN
        XYZM(2,I_ADD) = YMIN
        XYZM(3,I_ADD) = ZMIN
        XYZM(4,I_ADD) = XMAX
        XYZM(5,I_ADD) = YMAX
        XYZM(6,I_ADD) = ZMAX
        NMESR = 0
        IF(NSPMD>1)THEN
C
C recuperation des noeuds remote NMESR stockes dans XREM
C
          CALL SPMD_TRI17BOX(NELES   ,NMES    ,X       ,VCOM ,FROTS  ,
     2                       KS      ,XYZM    ,WEIGHT  ,NIN  ,ISENDTO,
     3                       IRCVFROM,NMESR   ,IXS     ,IXS16,EMINXS )
        END IF
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NME+100
C     P2........Elt PILE            3*(NME+100)
C     P21.......BPN                 NMES+100
C     P22.......PN                  3*(NMES+100)
        MAXSIZ = 3*(NME+100) 
        MAXSIZS = 3*(NMES+NMESR+100) 
C      Allocation directement dans i17tri pour tenir compte de NMESR
C      IP1 = 1
C      IP2 = IP1+NME+100
C      IP21= IP2+MAXSIZ
C      IP22= IP21+NMES+100
C      IP31= IP22+MAXSIZS
C -------------------------------------------------------------
C  Allocation tableau chaine apres calculs SPMD elt remote
C -------------------------------------------------------------
        IF(ITASK == 0)THEN
          ALLOCATE (ADCHAINE(NMES+NMESR),STAT=IERR1)
          ALLOCATE (CHAINE(2,MX_CAND),STAT=IERR2)
          IF(IERR1+IERR2 /= 0)THEN
            CALL ARRET(2)
          ENDIF
        ENDIF
C -------------------------------------------------------------
        CALL MY_BARRIER
C -------------------------------------------------------------
	ADCHAINE(NMES_F:NMES_L+NMESR) = 0
        CHAINE(1,1:MX_CAND) = 0
        CHAINE(2,1:MX_CAND) = 0
        MX_AD = 0
C -------------------------------------------------------------
        CALL MY_BARRIER
C -------------------------------------------------------------
        CALL I17TRI(
     2   TZINF   ,IXS     ,IXS16    ,IXS20    ,NELEM    ,
     3   NELES   ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,
     5   CONT    ,NB_N_B  ,EMINXM   ,I_STOK_GLOB,NME    ,
     6   ITASK   ,NOINT   ,X        ,V        ,A        ,
     7   MX_CAND ,EMINXS  ,ESH_T    ,MAXSIZS  ,I_ADD_MAX,
     8   XYZM    ,NMES    ,NMESR    ,NIN)
C -------------------------------------------------------------
        CALL MY_BARRIER
        IF(ITASK == 0)THEN
          DEALLOCATE (ADCHAINE)
          DEALLOCATE (CHAINE)
        ENDIF      
C -------------------------------------------------------------
      ENDDO
C
      RETURN
      END


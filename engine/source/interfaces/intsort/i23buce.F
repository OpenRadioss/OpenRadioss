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
Chd|  I23BUCE                       source/interfaces/intsort/i23buce.F
Chd|-- called by -----------
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I23TRIVOX                     source/interfaces/intsort/i23trivox.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I23BUCE(
     1   X        ,IRECT   ,NSV      ,INACTI  ,
     2   NRTM     ,NSN     ,CAND_E   ,CAND_N  ,GAP      ,
     3   NOINT   ,II_STOK  ,TZINF    , MAXBOX ,MINBOX   ,
     4   NCONTACT,NB_N_B   ,ESHIFT   ,CAND_P  ,NCONT    ,
     6   ILD     ,WEIGHT   ,STFN    ,NIN      ,
     7   STF      ,IGAP    ,GAP_S    ,GAPMIN  ,GAPMAX   ,
     8   ICURV    ,NUM_IMP ,ITASK    ,
     9   I_MEM    ,MSR     ,GAP_M    ,NSNR     ,CURV_MAX,
     A   RENUM    ,NSNROLD ,IFPEN   ,MWAG     ,BMINMA   ,
     B   NMN      ,IRECTG  ,BGAPSMX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, NSN, NOINT,IDT,INACTI,NIN,NSNR,NSNROLD,NMN,ITASK
      INTEGER IRECT(4,*), NSV(*), NUM_IMP, IRECTG(4,*)
      INTEGER CAND_E(*),CAND_N(*),MSR(*),MWAG(*),RENUM(*),IFPEN(*)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B, I_MEM,IGAP,ICURV,NCONT,
     .        WEIGHT(*),II_STOK
C     REAL
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,
     .   GAPMIN, GAPMAX, BMINMA(6),CURV_MAX(NRTM), BGAPSMX,
     .   LXM, LYM, LZM
      my_real
     .   X(3,*), STFN(*), STF(*), GAP_S(*), GAP_M(*),
     .   CAND_P(*)
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
      INTEGER I, J, I_ADD, IP0, IP1, MAXSIZ,
     .        ADD(2,I_ADD_MAX), LOC_PROC, N, ISZNSNR,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6,I_ADD_MAX-1), MARGE, AAA
C-----------------------------------------------
C     PROV
C-----------------------------------------------
cc      INTEGER INIVOXEL, VOXEL(LVOXEL),NBX,NBY,NBZ
c      INTEGER INIVOXEL, VOXEL(1),NBX,NBY,NBZ
cc      SAVE INIVOXEL, VOXEL
cc      DATA INIVOXEL /1/
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C----- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
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
C
C----- BORNES DU DOMAINE DEJA CALCULEES
C
      XYZM(1,I_ADD) = BMINMA(4)
      XYZM(2,I_ADD) = BMINMA(5)
      XYZM(3,I_ADD) = BMINMA(6)
      XYZM(4,I_ADD) = BMINMA(1)
      XYZM(5,I_ADD) = BMINMA(2)
      XYZM(6,I_ADD) = BMINMA(3)
      I_MEM = 0
C
      ISZNSNR = NSNR
C
C-----DEBUT DE LA PHASE DE TRI 
C
C       SEPARER B ET N EN TWO
C
       MARGE = TZINF - SQRT(THREE)*GAP
c      CALL I23TRI(
c     1   ADD      ,NSN      ,IRECT   ,X         ,STF      ,
c     2   STFN     ,XYZM     ,I_ADD   ,MAXSIZ    ,II_STOK  ,
c     3   CAND_N   ,CAND_E  ,NCONTACT  ,NOINT    ,TZINF    ,
c     4   MAXBOX   ,MINBOX  ,I_MEM     ,NB_N_B   ,I_ADD_MAX,
c     5   ESHIFT   ,INACTI  ,NRTM      ,IGAP     ,GAP      ,
c     6   GAP_S    ,GAPMIN  ,GAPMAX    ,MARGE    ,CURV_MAX ,
c     7   DEPTH   ,DRAD     ,MSR      ,GAP_M    ,
c     8   RENUM    ,NSNR     ,ISZNSNR  ,NSNROLD  ,MWAG(IP0),
c     9   IFPEN    ,CAND_P   ,NSV      ,ITAGP    ,IRECTG   )


      AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)

      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      ENDIF

C     initialisation complete de VOXEL
C (en // SMP il y a possibilite de redondance de traitement mais no pb)
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)

      CALL I23TRIVOX(
     1   NSN     ,RENUM    ,NSNR     ,ISZNSNR  ,I_MEM   ,
     2   IRECT   ,X        ,STF      ,STFN     ,XYZM    ,
     3   NSV     ,II_STOK  ,CAND_N   ,ESHIFT   ,CAND_E  ,
     4   NCONTACT,NOINT    ,TZINF    ,MSR      ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ     ,
     6   INACTI  ,MWAG(IP0),CAND_P   ,IFPEN   ,
     7   NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,
     8   GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,
     9   NIN     ,ITASK    ,BGAPSMX  )
 234  continue

C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF (I_MEM ==2) RETURN
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NSN) THEN
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
        RETURN
        TZINF = THREE_OVER_4*TZINF
C taille de boite non diminuee
C        MINBOX= THREE_OVER_4*MINBOX
C        MAXBOX= THREE_OVER_4*MAXBOX
        IF( TZINF<=GAP ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I23BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM==3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=100,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END

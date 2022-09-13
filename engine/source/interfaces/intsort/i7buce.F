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
Chd|  I7BUCE                        source/interfaces/intsort/i7buce.F
Chd|-- called by -----------
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        I7TRI                         source/interfaces/intsort/i7tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I7BUCE(
     1   X        ,IRECT   ,NSV          ,INACTI      ,CAND_P  ,
     2   NMN      ,NRTM    ,NSN          ,CAND_E      ,CAND_N  ,
     3   GAP      ,NOINT   ,II_STOK      ,NCONTACT    ,BMINMA  ,
     4   TZINF    ,MAXBOX  ,MINBOX       ,MWAG        ,CURV_MAX,
     5   NB_N_B   ,ESHIFT  ,ILD          ,IFQ         ,IFPEN   ,
     8   STFN     ,NIN     ,STF          ,IGAP        ,GAP_S   ,
     A   NSNR     ,NCONT   ,RENUM        ,NSNROLD     ,GAP_M   ,
     B   GAPMIN   ,GAPMAX  ,CURV_MAX_MAX ,NUM_IMP     ,GAP_S_L ,
     C   GAP_M_L  ,INTTH   ,ITASK        ,BGAPSMX     ,I_MEM   ,  
     D   KREMNOD  ,REMNOD  ,ITAB         ,FLAGREMNODE , DRAD ,
     E   ITIED    ,CAND_F  ,DGAPLOAD     )
C============================================================================
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
      INTEGER IRECT(4,*),NSV(*),MWAG(*), RENUM(*),NUM_IMP, ITASK
      INTEGER CAND_E(*),CAND_N(*),IFPEN(*),KREMNOD(*),REMNOD(*),ITAB(*)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B, IGAP, NCONT,INTTH,I_MEM,
     *        II_STOK, FLAGREMNODE, ITIED
C     REAL
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,CURV_MAX_MAX,
     .   GAPMIN, GAPMAX, BMINMA(12),CURV_MAX(NRTM),BGAPSMX
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      my_real
     .   X(3,*), CAND_P(*), STFN(*),
     .   STF(*), GAP_S(*), GAP_M(*), 
     .   GAP_S_L(*), GAP_M_L(*), CAND_F(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX,I_ADD
      PARAMETER (I_ADD_MAX = 1001) 
      INTEGER ADD(2,I_ADD_MAX)

      INTEGER I, J, IP0, IP1,
     .        LOC_PROC, N, ISZNSNR,
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
      INTEGER MAXSIZ                 


      MAXSIZ = 10 * (NRTM+100)

      ADD(1,1) = 0
      ADD(1,2) = 0
      ADD(2,1) = 0
      ADD(2,2) = 0


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

C 
      IP0 = 1
      IP1 = IP0 + NSN + NSNROLD + 3
C
C-----INITIALISATION 

C----- BORNES DU DOMAINE DEJA CALCULEES
C
      I_ADD = 1
      XYZM(1,I_ADD) = BMINMA(4)
      XYZM(2,I_ADD) = BMINMA(5)
      XYZM(3,I_ADD) = BMINMA(6)
      XYZM(4,I_ADD) = BMINMA(1)
      XYZM(5,I_ADD) = BMINMA(2)
      XYZM(6,I_ADD) = BMINMA(3)
C boite de tri
      I_MEM = 0
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.
     .   IFQ>0.OR.NUM_IMP>0.OR.ITIED/=0) THEN
        ISZNSNR = NSNR
      ELSE
        ISZNSNR = 0
      END IF

      MARGE = TZINF - (GAP+DGAPLOAD)
      CALL I7TRI(
     1      ADD   ,NSN    ,RENUM  ,NSNR     ,ISZNSNR  ,
     2      IRECT ,X      ,STF    ,STFN     ,XYZM     ,
     3      I_ADD ,NSV    ,MAXSIZ ,II_STOK  ,CAND_N   ,
     4      CAND_E,NCONTACT,NOINT  ,TZINF    ,MAXBOX   ,
     5      MINBOX,I_MEM  ,NB_N_B ,I_ADD_MAX,ESHIFT   ,
     6      INACTI,IFQ    ,MWAG(IP0), CAND_P   ,IFPEN    ,
     7      NRTM  ,NSNROLD,IGAP   ,GAP      ,GAP_S    ,
     8      GAP_M ,GAPMIN ,GAPMAX ,MARGE    ,CURV_MAX ,
     9      NIN    ,GAP_S_L,GAP_M_L,INTTH, DRAD  ,ITIED    ,
     A      CAND_F ,KREMNOD  ,REMNOD  ,FLAGREMNODE,DGAPLOAD)

 234  CONTINUE
c     WRITE(6,*) "IMEM=",I_MEM 
C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF (I_MEM /= 0) RETURN

C
      RETURN
      END

Chd|====================================================================
Chd|  I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|-- called by -----------
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I7BUCE_VOX(
     1   X        ,IRECT   ,NSV     ,INACTI   ,CAND_P  ,
     2   NMN      ,NRTM    ,NSN     ,CAND_E   ,CAND_N  ,
     3   GAP      ,NOINT   ,II_STOK ,NCONTACT ,BMINMA  ,
     4   TZINF    ,MAXBOX  ,MINBOX  ,MWAG     ,CURV_MAX,
     5   NB_N_B   ,ESHIFT  ,ILD     ,IFQ      ,IFPEN   ,
     8   STFN     ,NIN     ,STF     ,IGAP     ,GAP_S   ,
     A   NSNR     ,NCONT   ,RENUM   ,NSNROLD  ,GAP_M   ,
     B   GAPMIN   ,GAPMAX  ,CURV_MAX_MAX,NUM_IMP,GAP_S_L ,
     C   GAP_M_L  ,INTTH   ,ITASK   ,BGAPSMX  ,I_MEM   ,  
     D   KREMNOD  ,REMNOD  ,ITAB    ,FLAGREMNODE, DRAD ,
     E   ITIED    ,CAND_F  ,DGAPLOAD)
C============================================================================
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
      INTEGER IRECT(4,*),NSV(*),MWAG(*), RENUM(*),NUM_IMP, ITASK
      INTEGER CAND_E(*),CAND_N(*),IFPEN(*),KREMNOD(*),REMNOD(*),ITAB(*)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B, IGAP, NCONT,INTTH,I_MEM,
     *        II_STOK, FLAGREMNODE, ITIED
C     REAL
      my_real
     .   GAP,TZINF,MAXBOX,MINBOX,CURV_MAX_MAX,
     .   GAPMIN, GAPMAX, BMINMA(12),CURV_MAX(NRTM),BGAPSMX
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      my_real
     .   X(3,*), CAND_P(*), STFN(*),
     .   STF(*), GAP_S(*), GAP_M(*), 
     .   GAP_S_L(*), GAP_M_L(*), CAND_F(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP0, IP1,
     .        LOC_PROC, N, ISZNSNR,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6,2), MARGE, AAA
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

C 
      IP0 = 1
      IP1 = IP0 + NSN + NSNROLD + 3
C
C-----INITIALISATION 

C----- BORNES DU DOMAINE DEJA CALCULEES
C
      XYZM(1,1) = BMINMA(4)
      XYZM(2,1) = BMINMA(5)
      XYZM(3,1) = BMINMA(6)
      XYZM(4,1) = BMINMA(1)
      XYZM(5,1) = BMINMA(2)
      XYZM(6,1) = BMINMA(3)
C boite de tri
      XYZM(1,2) = BMINMA(10)
      XYZM(2,2) = BMINMA(11)
      XYZM(3,2) = BMINMA(12)
      XYZM(4,2) = BMINMA(7)
      XYZM(5,2) = BMINMA(8)
      XYZM(6,2) = BMINMA(9)
      I_MEM = 0
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.
     .   IFQ>0.OR.NUM_IMP>0.OR.ITIED/=0) THEN
        ISZNSNR = NSNR
      ELSE
        ISZNSNR = 0
      END IF
C
C-----DEBUT DE LA PHASE DE TRI 
C
      MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD)  ! il s agit bien de la marge

c      AAA = SQRT(NMN /
c     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
c     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
c     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))
c
c      AAA = 0.75*AAA
c
c      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
c      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
c      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
C
C Work on the reduce box

      IF( NMN /= 0 ) THEN
        AAA = SQRT(NMN /
     .           ((BMINMA(7)-BMINMA(10))*(BMINMA(8)-BMINMA(11))
     .           +(BMINMA(8)-BMINMA(11))*(BMINMA(9)-BMINMA(12))
     .           +(BMINMA(9)-BMINMA(12))*(BMINMA(7)-BMINMA(10))))
      ELSE
        AAA = 0
      ENDIF

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(7)-BMINMA(10)))
      NBY = NINT(AAA*(BMINMA(8)-BMINMA(11)))
      NBZ = NINT(AAA*(BMINMA(9)-BMINMA(12)))
C
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
       CALL I7TRIVOX(
     1   NSN     ,RENUM    ,NSNR     ,ISZNSNR  ,I_MEM   ,
     2   IRECT   ,X        ,STF      ,STFN     ,XYZM    ,
     3   NSV     ,II_STOK  ,CAND_N   ,ESHIFT   ,CAND_E  ,
     4   NCONTACT,NOINT    ,TZINF    ,GAP_S_L  ,GAP_M_L ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ      ,INTTH   ,
     6   INACTI  ,IFQ      ,MWAG(IP0),CAND_P   ,IFPEN   ,
     7   NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,
     8   GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,
     9   NIN     ,ITASK    ,BGAPSMX  ,KREMNOD  ,REMNOD  ,
     A   ITAB    ,FLAGREMNODE,DRAD   ,ITIED    ,CAND_F  ,
     B   DGAPLOAD)
 
 234  CONTINUE

C
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> TROP NIVEAUX PILE 
      IF (I_MEM ==2) RETURN
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .            I1=NOINT)
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
        IF( TZINF<=MAX(GAP+DGAPLOAD,DRAD)  ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I7BUCE)')
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

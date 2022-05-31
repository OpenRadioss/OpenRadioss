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
Chd|  I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I11TRIVOX                     source/interfaces/intsort/i11trivox.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI11                         share/modules/tri11_mod.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I11BUCE_VOX(
     1   X      ,IRECTS  ,IRECTM ,NRTS    ,NMN     ,
     2   NRTM   ,NSN     ,CAND_M ,CAND_S  ,MAXGAP  ,
     3   NOINT  ,II_STOK ,TZINF  ,MAXBOX  ,MINBOX  ,
     4   NB_N_B ,ESHIFT  ,ILD    ,BMINMA  ,NCONTACT,
     6   ADDCM  ,CHAINE  ,NIN    ,ITAB    ,NRTSR   ,
     7   NCONT  ,GAP_S  , STIFS  ,PENIS   ,IGAP    ,
     8   STIFM  ,IAUTO  , I_MEM  ,ITASK   ,IFORM   ,
     9   IFPEN  ,DRAD   , GAP_M  , GAP_S_L, 
     1   GAP_M_L ,GAPMIN, BGAPSMX, GAP,
     2   FLAGREMNODE,KREMNODE,REMNODE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX 
      USE TRI11   
      
C============================================================================
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
      INTEGER NMN, NRTM, NSN, NOINT,IDT,NRTS, NIN, NRTSR,
     .        IGAP,IAUTO, I_MEM, ITASK
      INTEGER IRECTS(2,*),IRECTM(2,*),ADDCM(*),CHAINE(2,*)
      INTEGER CAND_M(*),CAND_S(*),IFPEN(*),FLAGREMNODE,KREMNODE(*),REMNODE(*)
      INTEGER ESHIFT,ILD,NB_N_B, NCONTACT, NCONT, ITAB(*),
     .        IFORM,II_STOK
C     REAL ou REAL*8
      my_real
     .   TZINF,MAXBOX,MINBOX,BMINMA(6),DRAD,BGAPSMX
      my_real
     . MAXGAP,GAPMIN,GAP
      my_real
     .   X(3,*),STIFS(*),PENIS(2,*),STIFM(*),
     .   GAP_S(*),GAP_M(*),GAP_S_L(*),GAP_M_L(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX,ISZNSNR
      PARAMETER (I_ADD_MAX = 1001)
C
      INTEGER I, J, N1, N2, I_ADD, MAXSIZ,JJ,
     .        ADD(2,I_ADD_MAX), N
      my_real
     .        XYZM(6,I_ADD_MAX-1), MARGE, AAA
      my_real 
     . DD,DD1,MARGE_ST,DX1,DY1,DZ1
      INTEGER :: L
      INTEGER NB_OLD(2,I_ADD_MAX+1)
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8 
C-----------------------------------------------
C   definition from TRI7BOX module
C-----------------------------------------------      
C              
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C
C----- TRI PAR BOITES 
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            3*NRTM
C     P21.......Elt Bas Pile        NRTS
C     P22.......Elt PILE            3*NRTS
      MAXSIZ = 3*(MAX(NRTM,NRTS+NRTSR)+100)
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
      IF (IFORM .NE. 2) THEN
        ISZNSNR = 0
        DO I=1,NRTM
          ADDCM(I)=0
        ENDDO 
      ELSE
        ISZNSNR = NRTSR
      ENDIF    
C
C
C-----DEBUT DE LA PHASE DE TRI 
   
      MARGE = TZINF - MAX(MAXGAP,DRAD)

      IF( NMN .NE.  0 ) THEN
        AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))
      ELSE
        AAA = 0
      ENDIF

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

      IF(RES8 > LVOXEL8)THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
        NBX8 = NBX
        NBY8 = NBY
        NBZ8 = NBZ
        RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      END IF
C If still  too many voxels:
C Reduction of the number of voxels 
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      END IF
      
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
      
      !print *, "voxel search"
      
        CALL I11TRIVOX(
     1   IRECTS  ,IRECTM  ,X       ,NRTM    ,NRTSR   ,
     2   XYZM    ,II_STOK ,CAND_S  ,CAND_M  ,NCONTACT,
     3   NOINT   ,TZINF   ,I_MEM   ,ESHIFT  ,ADDCM   ,
     4   CHAINE  ,NRTS    ,ITAB    ,STIFS   ,STIFM   ,
     5   IAUTO   ,VOXEL1  ,NBX     ,NBY     ,NBZ     ,
     6   ITASK   ,IFPEN   ,IFORM   ,GAPMIN  ,DRAD    , 
     7   MARGE   ,GAP_S   ,GAP_M   ,GAP_S_L, GAP_M_L,
     8   BGAPSMX, IGAP    ,GAP     ,FLAGREMNODE,KREMNODE,
     9   REMNODE)    

        CALL MY_BARRIER

  100 CONTINUE

C
C     I_MEM = 1 ==> N/A
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     I_MEM = 3 ==> N/A


C === WRITE CANDIDATES IN fort.[900 + ISPMD] files
c     IF ( ) THEN
c       DO I = 1, II_STOK
c        IF(CAND_S(I) <= NRTS) THEN
c        WRITE(900+ISPMD,*) NOINT,ITAB(IRECTS(1,CAND_S(I))),
c    . ITAB(IRECTS(2,CAND_S(I))),ITAB(IRECTM(1,CAND_M(I))),ITAB(IRECTM(2,CAND_M(I)))
c        ELSE
c        WRITE(900+ISPMD,*) -NOINT,ITAB(IRECTS(1,CAND_S(I)))
c    . ,ITAB(IRECTS(2,CAND_S(I))),ITAB(IRECTM(1,CAND_M(I))),ITAB(IRECTM(2,CAND_M(I)))
c        ENDIF
c      ENDDO
c      CALL FLUSH(900+ISPMD)
c      STOP
c     ENDIF
            


      IF (I_MEM == 2) RETURN

      IF(I_MEM.EQ.1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. MAX(NRTM,NRTS)) THEN
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
        IF( TZINF.LE.MAX(MAXGAP,DRAD) ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I11BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM.EQ.3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. MAX(NRTM,NRTS)) THEN
          CALL ANCMSG(MSGID=99,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I11BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END

C --- --- --- Chd|====================================================================
C --- --- --- Chd|  I11BUCE                       src/intsort/i11buce.F         
C --- --- --- Chd|-- called by -----------
C --- --- --- Chd|        I11MAIN_TRI                   src/intsort/i11main_tri.F     
C --- --- --- Chd|-- calls ---------------
C --- --- --- Chd|        ANCMSG                        src/message/message.F         
C --- --- --- Chd|        ARRET                         src/sortie/arret.F            
C --- --- --- Chd|        I11TRI                        src/intsort/i11tri.F          
C --- --- --- Chd|        I11TRIVOX                     src/intsort/i11trivox.F       
C --- --- --- Chd|        MY_BARRIER                    src/spe/machine.F             
C --- --- --- Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
C --- --- --- Chd|        TRI11                         share/modules/tri11_mod.F     
C --- --- --- Chd|        TRI7BOX                       share/modules/tri7box.F       
C --- --- --- Chd|====================================================================
C --- --- ---        SUBROUTINE I11BUCE(
C --- --- ---      1   X      ,IRECTS  ,IRECTM ,NRTS    ,NMN     ,
C --- --- ---      2   NRTM   ,NSN     ,CAND_M ,CAND_S  ,GAP     ,
C --- --- ---      3   NOINT  ,II_STOK ,TZINF  ,MAXBOX  ,MINBOX  ,
C --- --- ---      4   NB_N_B ,ESHIFT  ,ILD    ,BMINMA  ,NCONTACT,
C --- --- ---      6   ADDCM  ,CHAINE  ,NIN    ,ITAB    ,NRTSR   ,
C --- --- ---      7   NCONT  ,GAP_S  , STIFS  ,PENIS   ,IGAP    ,
C --- --- ---      8   STIFM  ,IAUTO  , I_MEM  ,ITASK   ,IFORM   ,
C --- --- ---      9   IFPEN  ,DRAD   )
C --- --- --- C-----------------------------------------------
C --- --- --- C   M o d u l e s
C --- --- --- C-----------------------------------------------
C --- --- ---       USE MESSAGE_MOD
C --- --- ---       USE TRI7BOX 
C --- --- ---       USE TRI11   
C --- --- ---       
C --- --- --- C============================================================================
C --- --- --- C   I m p l i c i t   T y p e s
C --- --- --- C-----------------------------------------------
C --- --- --- #include      "implicit_f.inc"
C --- --- --- #include      "comlock.inc"
C --- --- --- C-----------------------------------------------
C --- --- --- C   C o m m o n   B l o c k s
C --- --- --- C-----------------------------------------------
C --- --- --- #include      "units_c.inc"
C --- --- --- #include      "warn_c.inc"
C --- --- --- #include      "com01_c.inc"
C --- --- --- #include      "com04_c.inc"
C --- --- --- #include      "com08_c.inc"
C --- --- --- #include      "scr05_c.inc"
C --- --- --- #include      "task_c.inc"
C --- --- --- #include      "param_c.inc"
C --- --- --- C-----------------------------------------------
C --- --- --- C   D u m m y   A r g u m e n t s
C --- --- --- C-----------------------------------------------
C --- --- ---       INTEGER NMN, NRTM, NSN, NOINT,IDT,NRTS, NIN, NRTSR,
C --- --- ---      .        IGAP,IAUTO, I_MEM, ITASK
C --- --- ---       INTEGER IRECTS(2,*),IRECTM(2,*),ADDCM(*),CHAINE(2,*)
C --- --- ---       INTEGER CAND_M(*),CAND_S(*),IFPEN(*)
C --- --- ---       INTEGER ESHIFT,ILD,NB_N_B, NCONTACT, NCONT, ITAB(*),
C --- --- ---      .        IFORM,II_STOK
C --- --- --- C     REAL ou REAL*8
C --- --- ---       my_real
C --- --- ---      .   GAP,TZINF,MAXBOX,MINBOX,BMINMA(6),DRAD
C --- --- ---       my_real
C --- --- ---      .   X(3,*),GAP_S(*),STIFS(*),PENIS(2,*),STIFM(*)
C --- --- --- C-----------------------------------------------
C --- --- --- C   G l o b a l   P a r a m e t e r s
C --- --- --- C-----------------------------------------------
C --- --- --- #include      "mvsiz_p.inc"
C --- --- --- C-----------------------------------------------
C --- --- --- C   L o c a l   V a r i a b l e s
C --- --- --- C-----------------------------------------------
C --- --- ---       INTEGER I_ADD_MAX,ISZNSNR
C --- --- ---       PARAMETER (I_ADD_MAX = 1001)
C --- --- --- C
C --- --- ---       INTEGER I, J, N1, N2, I_ADD, MAXSIZ,JJ,
C --- --- ---      .        ADD(2,I_ADD_MAX), N
C --- --- ---       my_real
C --- --- ---      .        XYZM(6,I_ADD_MAX-1), MARGE, AAA
C --- --- ---       INTEGER NB_OLD(2,I_ADD_MAX+1)
C --- --- ---       INTEGER NBX,NBY,NBZ
C --- --- ---       INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8 
C --- --- --- C-----------------------------------------------
C --- --- --- C   definition from TRI7BOX module
C --- --- --- C-----------------------------------------------      
C --- --- --- C              
C --- --- --- C-----------------------------------------------
C --- --- --- C   S o u r c e  L i n e s
C --- --- --- C-----------------------------------------------
C --- --- --- C-----------------------------------------------
C --- --- --- C
C --- --- --- C----- TRI PAR BOITES 
C --- --- --- C
C --- --- --- C-----------------------------------------------
C --- --- --- C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C --- --- --- C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C --- --- --- C
C --- --- --- C     POINTEUR  NOM                 TAILLE
C --- --- --- C     P1........Elt Bas Pile        NRTM
C --- --- --- C     P2........Elt PILE            3*NRTM
C --- --- --- C     P21.......Elt Bas Pile        NRTS
C --- --- --- C     P22.......Elt PILE            3*NRTS
C --- --- ---       MAXSIZ = 3*(MAX(NRTM,NRTS+NRTSR)+100)
C --- --- --- C
C --- --- --- C-----INITIALISATION DES ADRESSES ET X,Y,Z
C --- --- --- C
C --- --- --- C     ADDE     ADDN     X      Y      Z
C --- --- --- C     1        1        XMIN   YMIN   ZMIN
C --- --- --- C     1        1        XMAX   YMAX   ZMAX
C --- --- --- C
C --- --- ---       ADD(1,1) = 0
C --- --- ---       ADD(2,1) = 0
C --- --- ---       ADD(1,2) = 0
C --- --- ---       ADD(2,2) = 0
C --- --- ---       I_ADD = 1
C --- --- ---       XYZM(1,I_ADD) = BMINMA(4)
C --- --- ---       XYZM(2,I_ADD) = BMINMA(5)
C --- --- ---       XYZM(3,I_ADD) = BMINMA(6)
C --- --- ---       XYZM(4,I_ADD) = BMINMA(1)
C --- --- ---       XYZM(5,I_ADD) = BMINMA(2)
C --- --- ---       XYZM(6,I_ADD) = BMINMA(3)
C --- --- ---       I_MEM = 0
C --- --- --- C
C --- --- ---       IF (IFORM .NE. 2) THEN
C --- --- ---         ISZNSNR = 0
C --- --- ---         DO I=1,NRTM
C --- --- ---           ADDCM(I)=0
C --- --- ---         ENDDO 
C --- --- ---       ELSE
C --- --- ---         ISZNSNR = NRTSR
C --- --- ---       ENDIF    
C --- --- --- C
C --- --- --- C
C --- --- --- C-----DEBUT DE LA PHASE DE TRI 
C --- --- --- C 
C --- --- ---       GOTO 50 !commenter pour debtrancher le tri voxel
C --- --- ---       print *, "bucket search"
C --- --- ---       CALL I11TRI(
C --- --- ---      1   ADD     ,
C --- --- ---      2   IRECTS  ,IRECTM  ,X        ,NRTM     ,NRTSR  ,
C --- --- ---      3   XYZM    ,I_ADD   ,MAXSIZ   ,II_STOK  ,CAND_S ,
C --- --- ---      4   CAND_M  ,NCONTACT,NOINT    ,TZINF    ,MAXBOX ,
C --- --- ---      5   MINBOX  ,I_MEM   ,NB_N_B   ,I_ADD_MAX,ESHIFT ,
C --- --- ---      6   ADDCM   ,CHAINE  ,NRTS     ,ITAB     ,NB_OLD ,
C --- --- ---      7   STIFS   ,STIFM   ,IAUTO    ,NIN      ,IFPEN  ,
C --- --- ---      8   IFORM) 
C --- --- --- 
C --- --- ---       !cts on debranche la routine de tri par voxel pour le moment
C --- --- ---       GOTO 100    
C --- --- ---        
C --- --- ---    50 CONTINUE
C --- --- ---    
C --- --- ---       MARGE = TZINF - MAX(GAP,DRAD)
C --- --- --- 
C --- --- ---       AAA = SQRT(NMN /
C --- --- ---      .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
C --- --- ---      .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
C --- --- ---      .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))
C --- --- --- 
C --- --- ---       AAA = 0.75*AAA
C --- --- --- 
C --- --- ---       NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
C --- --- ---       NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
C --- --- ---       NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
C --- --- ---       NBX = MAX(NBX,1)
C --- --- ---       NBY = MAX(NBY,1)
C --- --- ---       NBZ = MAX(NBZ,1)
C --- --- --- 
C --- --- ---       NBX8=NBX
C --- --- ---       NBY8=NBY
C --- --- ---       NBZ8=NBZ
C --- --- ---       RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
C --- --- ---       LVOXEL8 = LVOXEL      
C --- --- --- 
C --- --- ---       IF(RES8 > LVOXEL8)THEN
C --- --- ---         AAA = LVOXEL
C --- --- ---         AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
C --- --- ---         AAA = AAA**(THIRD)
C --- --- ---         NBX = INT((NBX+2)*AAA)-2
C --- --- ---         NBY = INT((NBY+2)*AAA)-2
C --- --- ---         NBZ = INT((NBZ+2)*AAA)-2
C --- --- ---         NBX = MAX(NBX,1)
C --- --- ---         NBY = MAX(NBY,1)
C --- --- ---         NBZ = MAX(NBZ,1)
C --- --- --- 	NBX8 = NBX
C --- --- --- 	NBY8 = NBY
C --- --- --- 	NBZ8 = NBZ
C --- --- ---         RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
C --- --- ---       END IF
C --- --- ---           
C --- --- ---       IF(RES8 > LVOXEL8) THEN
C --- --- ---         stop 678
C --- --- ---       END IF
C --- --- ---       
C --- --- ---       DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
C --- --- ---         VOXEL1(I)=0
C --- --- ---       ENDDO
C --- --- ---       INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
C --- --- ---       
C --- --- ---       !print *, "voxel search"
C --- --- ---       
C --- --- ---         CALL I11TRIVOX(
C --- --- ---      1   IRECTS  ,IRECTM  ,X       ,NRTM    ,NRTSR   ,
C --- --- ---      2   XYZM    ,II_STOK ,CAND_S  ,CAND_M  ,NCONTACT,
C --- --- ---      3   NOINT   ,TZINF   ,I_MEM   ,ESHIFT  ,ADDCM   ,
C --- --- ---      4   CHAINE  ,NRTS    ,ITAB    ,STIFS   ,STIFM   ,
C --- --- ---      5   IAUTO   ,VOXEL1 ,NBX     ,NBY     ,NBZ     ,
C --- --- ---      6   ITASK   ,IFPEN  ,IFORM   )    
C --- --- --- 
C --- --- ---         CALL MY_BARRIER
C --- --- --- 
C --- --- ---   100 CONTINUE
C --- --- --- 
C --- --- --- C
C --- --- --- C     I_MEM = 1 ==> N/A
C --- --- --- C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C --- --- --- C     I_MEM = 3 ==> N/A
C --- --- --- 
C --- --- ---       IF (I_MEM == 2) RETURN
C --- --- ---       IF(I_MEM.EQ.1)THEN
C --- --- ---         NB_N_B = NB_N_B + 1
C --- --- ---         IF ( NB_N_B .GT. MAX(NRTM,NRTS)) THEN
C --- --- ---           CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
C --- --- ---      .                I1=NOINT)
C --- --- ---           CALL ARRET(2)
C --- --- ---         ENDIF
C --- --- ---         ILD = 1
C --- --- ---       ELSEIF(I_MEM.EQ.2) THEN
C --- --- ---         IF(DEBUG(1).GE.1) THEN
C --- --- ---           IWARN = IWARN+1
C --- --- --- #include "lockon.inc"
C --- --- ---           WRITE(ISTDO,*)' **WARNING INTERFACE/MEMORY'
C --- --- ---           WRITE(IOUT,*)' **WARNING INTERFACE NB:',NOINT
C --- --- ---           WRITE(IOUT,*)'       TOO MANY POSSIBLE IMPACTS'
C --- --- ---           WRITE(IOUT,*)'       SIZE OF INFLUENCE ZONE IS'
C --- --- ---           WRITE(IOUT,*)'       MULTIPLIED BY 0.75'
C --- --- --- #include "lockoff.inc"
C --- --- ---         ENDIF
C --- --- ---         TZINF = THREE_OVER_4*TZINF
C --- --- --- C ne pas dimunuer la taille des boite
C --- --- --- C        MINBOX= THREE_OVER_4*MINBOX
C --- --- --- C        MAXBOX= THREE_OVER_4*MAXBOX
C --- --- ---         IF( TZINF.LE.MAX(GAP,DRAD) ) THEN
C --- --- ---           CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
C --- --- ---      .            I1=NOINT,C1='(I11BUCE)')
C --- --- ---           CALL ARRET(2)
C --- --- ---         ENDIF
C --- --- ---         ILD = 1
C --- --- ---       ELSEIF(I_MEM.EQ.3)THEN
C --- --- ---         NB_N_B = NB_N_B + 1
C --- --- ---         IF ( NB_N_B .GT. MAX(NRTM,NRTS)) THEN
C --- --- ---           CALL ANCMSG(MSGID=99,ANMODE=ANINFO,
C --- --- ---      .            I1=NOINT,C1='(I11BUCE)')
C --- --- ---           CALL ARRET(2)
C --- --- ---         ENDIF
C --- --- ---         ILD = 1
C --- --- ---       ENDIF
C --- --- --- C
C --- --- ---       RETURN
C --- --- ---       END

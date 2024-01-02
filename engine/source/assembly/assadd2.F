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
C finalisation parith/on spmd (1ere partie effectuee dans Starter)
Chd|====================================================================
Chd|  ASSADD2                       source/assembly/assadd2.F     
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE ASSADD2(ADDCNE  ,INDSKY  ,FSKY  ,FSKYM    ,IAD_ELEM ,
     2             FR_ELEM ,FR_NBCC ,PROCNE,NISKYFI  ,ADDCNI2  ,        
     3             PROCNI2 ,IAD_I2M ,FR_I2M,FR_NBCCI2,INDSKYI2 ,        
     4             IADSDP  ,IADRCP  ,ISENDP,IRECVP   ,FTHESKY  ,        
     5             NISKYFIE,INOD_PXFEM,ADDCNE_PXFEM,PROCNE_PXFEM,
     6             ISENDP_PXFEM,IRECVP_PXFEM ,IADSDP_PXFEM,IADRCP_PXFEM,
     7             FR_NBCC1 ,INOD_CRKXFEM,ADDCNE_CRKXFEM,PROCNE_CRKXFEM,
     8     ISENDP_CRKXFEM,IRECVP_CRKXFEM,IADSDP_CRKXFEM,IADRCP_CRKXFEM,
     9             CONDNSKY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "scr18_c.inc"
      INTEGER MAXBLOC,NBLOC,NBVAL,NBCOL
      PARAMETER (MAXBLOC=1000)
      COMMON/PTMPARIT/NBLOC,NBVAL(1:MAXBLOC),NBCOL(1:MAXBLOC)
      INTEGER MAXBLOCI2,NBLOCI2,NBVALI2,NBCOLI2
      PARAMETER (MAXBLOCI2=1000)
      COMMON/PTMPARI2/NBLOCI2,NBVALI2(1:MAXBLOCI2),NBCOLI2(1:MAXBLOCI2)
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INDSKY(*),ADDCNE(*),IAD_ELEM(2,*),FR_NBCC(2,*),
     .        PROCNE(*),FR_ELEM(*), NISKYFI(*), ADDCNI2(*),NISKYFIE(*),
     .        PROCNI2(*), IAD_I2M(*), FR_I2M(*), FR_NBCCI2(2,*),
     .        INDSKYI2(*), IADSDP(*), IADRCP(*), ISENDP(*), IRECVP(*),
     .        PROCNE_PXFEM(*),ISENDP_PXFEM(*),IRECVP_PXFEM(*),
     .        IADSDP_PXFEM(*),IADRCP_PXFEM(*),FR_NBCC1(2,*),
     .        ADDCNE_PXFEM(*),INOD_PXFEM(*),ADDCNE_CRKXFEM(*),
     .        INOD_CRKXFEM(*),PROCNE_CRKXFEM(*),ISENDP_CRKXFEM(*),
     .        IRECVP_CRKXFEM(*),IADSDP_CRKXFEM(*),IADRCP_CRKXFEM(*)
      my_real 
     .        FSKY(8,LSKY),FSKYM(LSKY),FTHESKY(LSKY),CONDNSKY(LSKY)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, K, L, NC1, NC, LOC_PROC, NOD, CC, lsd, lrc,
     .        WORK(70000), INDEX(2*NUMNOD),LSD1,LRC1,NOD1,XFEM_REM_COUNT
C-----------------------------------------------
C   Init a ZERO FORCES NODALES 
C-----------------------------------------------
      NISKY = 0
      DO N = 1, NINTER
        NISKYFI(N) = 0
        NISKYFIE(N) = 0
      ENDDO
      DO K=1,8
        DO I = 1, LSKY
          FSKY(K,I)=ZERO
        ENDDO
      ENDDO
C
C
      IF(ITHERM_FE > 0 ) THEN
        DO I=1,LSKY 
         FTHESKY(I) = ZERO
       ENDDO
      ENDIF
C
      IF(NODADT_THERM > 0 ) THEN
        DO I=1,LSKY 
         CONDNSKY(I) = ZERO
       ENDDO
      ENDIF
C
      IF (N2D == 0 .AND. IALE+IEULER+ITHERM>0)THEN
        DO I = 1, LSKY
          FSKYM(I)=ZERO
        ENDDO
      ENDIF
C
C spmd cne et addcne deja obtenus
C iadx deja calcule dans RADIOSS Starter
C
C-----------------------------------------------
C Calcul des tailles des comms elementaires
C-----------------------------------------------
      DO I = 1, NSPMD+1
        FR_NBCC(1,I) = 0
        FR_NBCC(2,I) = 0
      ENDDO
      IF(IPLYXFEM > 0 ) THEN
        DO I = 1, NSPMD+1
           FR_NBCC1(1,I) = 0
           FR_NBCC1(2,I) = 0
        ENDDO 
      ENDIF
C
      IF(ICRACK3D > 0 .AND. NSPMD > 1)THEN
        DO I = 1, NSPMD+1
           FR_NBCC1(1,I) = 0
           FR_NBCC1(2,I) = 0
        ENDDO 
      ENDIF
C
      LOC_PROC = ISPMD+1
C
      LSD = 1
      LRC = 1  
C
      LSD1 = 1
      LRC1 = 1
      IF(IPLYXFEM == 0 .AND. ICRACK3D == 0) THEN
         DO I = 1, NSPMD
           IADSDP(I)=LSD
           IADRCP(I)=LRC
           IF(I/=LOC_PROC) THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   FR_NBCC(1,I) = FR_NBCC(1,I)+1
                   ISENDP(LSD) = CC
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   FR_NBCC(2,I) = FR_NBCC(2,I)+1
                   IRECVP(LRC) = CC
                   LRC = LRC + 1
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ENDDO
C
         IADSDP(NSPMD+1)=LSD
         IADRCP(NSPMD+1)=LRC
      ELSE IF (IPLYXFEM > 0) THEN
        DO I = 1, NSPMD
           IADSDP(I)=LSD
           IADRCP(I)=LRC
C           
           IADSDP_PXFEM(I)=LSD1
           IADRCP_PXFEM(I)=LRC1
           IF(I/=LOC_PROC) THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   FR_NBCC(1,I) = FR_NBCC(1,I)+1
                   ISENDP(LSD) = CC
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   FR_NBCC(2,I) = FR_NBCC(2,I)+1
                   IRECVP(LRC) = CC
                   LRC = LRC + 1
                 ENDIF
               ENDDO
               NOD1 = INOD_PXFEM(NOD)
               IF(NOD1 > 0 ) THEN
                   DO CC = ADDCNE_PXFEM(NOD1),ADDCNE_PXFEM(NOD1+1)-1
                    IF(PROCNE_PXFEM(CC)==LOC_PROC) THEN
                      FR_NBCC1(1,I) = FR_NBCC1(1,I)+1
                      ISENDP_PXFEM(LSD1) = CC
                      LSD1 = LSD1 + 1
                    ELSEIF(PROCNE_PXFEM(CC)==I) THEN
                      FR_NBCC1(2,I) = FR_NBCC1(2,I)+1
                      IRECVP_PXFEM(LRC1) = CC
                      LRC1 = LRC1 + 1
                    ENDIF
                  ENDDO  
               ENDIF
             ENDDO
           ENDIF
         ENDDO
         IADSDP(NSPMD+1)=LSD
         IADRCP(NSPMD+1)=LRC      
C      
         IADSDP_PXFEM(NSPMD+1)=LSD1
         IADRCP_PXFEM(NSPMD+1)=LRC1            
c
      ELSE IF (ICRACK3D > 0 .AND. NSPMD > 1) THEN
        DO I = 1, NSPMD
          IADSDP(I)=LSD                                                       
          IADRCP(I)=LRC                                                       
C                                                                             
          IADSDP_CRKXFEM(I)=LSD1                                              
          IADRCP_CRKXFEM(I)=LRC1                                              
          IF (I /= LOC_PROC) THEN                                             
            DO J = IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                              
              NOD = FR_ELEM(J)                                                
              DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1                             
                IF (PROCNE(CC) == LOC_PROC) THEN                               
                  FR_NBCC(1,I) = FR_NBCC(1,I)+1                               
                  ISENDP(LSD) = CC                                            
                  LSD = LSD + 1                                               
                ELSEIF (PROCNE(CC) == I) THEN                                  
                  FR_NBCC(2,I) = FR_NBCC(2,I)+1                               
                  IRECVP(LRC) = CC                                            
                  LRC = LRC + 1                                               
                ENDIF                                                         
              ENDDO                                                           
c---
              NOD1 = INOD_CRKXFEM(NOD)                                        
              IF (NOD1 > 0 ) THEN                                             
                         XFEM_REM_COUNT = 0                                            
                DO CC = ADDCNE_CRKXFEM(NOD1),ADDCNE_CRKXFEM(NOD1+1)-1         
                  IF (PROCNE_CRKXFEM(CC) == I) XFEM_REM_COUNT =                 
     .                                         XFEM_REM_COUNT+1                 
                        ENDDO                                                         
c
                        IF (XFEM_REM_COUNT /= 0) THEN                                 
                  DO CC = ADDCNE_CRKXFEM(NOD1),ADDCNE_CRKXFEM(NOD1+1)-1       
                    IF (PROCNE_CRKXFEM(CC)==LOC_PROC) THEN                  
                      FR_NBCC1(1,I) = FR_NBCC1(1,I)+1                         
                      ISENDP_CRKXFEM(LSD1) = CC                               
                      LSD1 = LSD1 + 1                                         
                    ELSEIF (PROCNE_CRKXFEM(CC)==I) THEN                     
                      FR_NBCC1(2,I) = FR_NBCC1(2,I)+1                         
                      IRECVP_CRKXFEM(LRC1) = CC                               
                      LRC1 = LRC1 + 1                                         
                    ENDIF                                                     
                  ENDDO                                                       
                        ENDIF                                                         
c                                                                              
              ENDIF                                                           
c---
            ENDDO !  loop over frontier nodes                                                            
          ENDIF   ! I /= LOC_PROC                                                           
        ENDDO     ! I = 1, NSPMD                   
        IADSDP(NSPMD+1)=LSD          
        IADRCP(NSPMD+1)=LRC          
C      
        IADSDP_CRKXFEM(NSPMD+1)=LSD1 
        IADRCP_CRKXFEM(NSPMD+1)=LRC1 
      ENDIF   
C-------------------------------------
      DO I = 1, NSPMD
        FR_NBCC(1,NSPMD+1) = FR_NBCC(1,NSPMD+1) +  FR_NBCC(1,I)
        FR_NBCC(2,NSPMD+1) = FR_NBCC(2,NSPMD+1) +  FR_NBCC(2,I)
      ENDDO
C      
      IF(IPLYXFEM > 0) THEN
          DO I = 1, NSPMD
            FR_NBCC1(1,NSPMD+1) = FR_NBCC1(1,NSPMD+1) +  FR_NBCC1(1,I)
            FR_NBCC1(2,NSPMD+1) = FR_NBCC1(2,NSPMD+1) +  FR_NBCC1(2,I)
          ENDDO
      ENDIF
C
      IF(ICRACK3D > 0 .AND. NSPMD > 1)THEN
          DO I = 1, NSPMD
            FR_NBCC1(1,NSPMD+1) = FR_NBCC1(1,NSPMD+1) +  FR_NBCC1(1,I)
            FR_NBCC1(2,NSPMD+1) = FR_NBCC1(2,NSPMD+1) +  FR_NBCC1(2,I)
          ENDDO
      ENDIF
C-----------------------------------------------
C Optimisation vectorielle de asspar4
C-----------------------------------------------
      IF (IVECTOR==1) THEN
C   index : 2*numnod
C   indsky : numnod
        DO N = 1, NUMNOD
          INDSKY(N) = ADDCNE(N+1) - ADDCNE(N)
          INDEX(N) = N
        ENDDO
        CALL MY_ORDERS(0,WORK,INDSKY,INDEX,NUMNOD,1)
        DO N = 1, NUMNOD
          INDSKY(N) = INDEX(N)
        ENDDO
C
C reperage des blocs
C    
        NC1 = -1
        I = 1
        NBLOC = 0
        DO WHILE (I<=NUMNOD)
          N = INDSKY(I)
          NC = ADDCNE(N+1)-ADDCNE(N)
          IF(NC==NC1) THEN
            NBVAL(NBLOC) = NBVAL(NBLOC)+1
          ELSE
            NC1 = NC
            NBLOC = NBLOC+1
            IF (NBLOC>MAXBLOC) THEN
              WRITE(IOUT,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
              WRITE(ISTDO,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
              TSTOP=ZERO
              RETURN
            ENDIF
            NBVAL(NBLOC) = 1
            NBCOL(NBLOC) = NC
          ENDIF
          I = I+1
        ENDDO
C fin ivector=1
      ENDIF
C
C Partie Interface type 2
C
      DO I = 1, NSPMD+1
        FR_NBCCI2(1,I) = 0
        FR_NBCCI2(2,I) = 0
      ENDDO

      IF(I2NSNT>0) THEN
C-----------------------------------------------
C Calcul des tailles des comms elementaires
C-----------------------------------------------
C
        LOC_PROC = ISPMD+1
        DO I = 1, NSPMD
          IF(I/=LOC_PROC) THEN
            DO J=IAD_I2M(I),IAD_I2M(I+1)-1
              NOD = FR_I2M(J)
              DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
                IF(PROCNI2(CC)==LOC_PROC) THEN
                  FR_NBCCI2(1,I) = FR_NBCCI2(1,I)+1
C                ELSE
                ELSEIF(PROCNI2(CC)==I) THEN
                  FR_NBCCI2(2,I) = FR_NBCCI2(2,I)+1
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        DO I = 1, NSPMD
          FR_NBCCI2(1,NSPMD+1) = FR_NBCCI2(1,NSPMD+1)+FR_NBCCI2(1,I)
          FR_NBCCI2(2,NSPMD+1) = FR_NBCCI2(2,NSPMD+1)+FR_NBCCI2(2,I)
        ENDDO
C-----------------------------------------------
C Optimisation vectorielle de asspari2
C-----------------------------------------------
        IF (IVECTOR==1) THEN
C index : 2*numnod
C indskyi2 : numnod
          DO N = 1, NUMNOD
            INDSKYI2(N) = ADDCNI2(N+1) - ADDCNI2(N)
            INDEX(N) = N
          ENDDO
          CALL MY_ORDERS(0,WORK,INDSKYI2,INDEX,NUMNOD,1)
          DO N = 1, NUMNOD
            INDSKYI2(N) = INDEX(N)
          ENDDO
C
C reperage des blocs
C    
          NC1 = -1
          I = 1
          NBLOCI2 = 0
          DO WHILE (I<=NUMNOD)
            N = INDSKYI2(I)
            NC = ADDCNI2(N+1)-ADDCNI2(N)
            IF(NC==NC1) THEN
              NBVALI2(NBLOCI2) = NBVALI2(NBLOCI2)+1
            ELSE
              NC1 = NC
              NBLOCI2 = NBLOCI2+1
              IF (NBLOCI2>MAXBLOC) THEN
                WRITE(IOUT,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
                WRITE(ISTDO,*)
     .        ' **ERROR**: MEMORY PROBLEM IN PARITH OPTION'
                TSTOP=ZERO
                  RETURN
              ENDIF
              NBVALI2(NBLOCI2) = 1
              NBCOLI2(NBLOCI2) = NC
            ENDIF
            I = I+1
          ENDDO
C fin ivector=1
        ENDIF
C fin interface type I2NSNT > 0
      END IF
C
      RETURN
      END
C 
Chd|====================================================================
Chd|  ASSINIT                       source/assembly/assadd2.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSINIT(
     1    ADDCNE, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE(*),IAD_ELEM(2,*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC
C-----------------------------------------------
C Calcul des tailles des comms optimis\E9es parith/on
C-----------------------------------------------
      IF(IMACH==3.AND.NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
              IF(PROCNE(CC)==LOC_PROC) THEN
                LSD = LSD + 1
              ELSEIF(PROCNE(CC)==I) THEN
                LRC = LRC + 1
              ENDIF
            ENDDO
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSINIT_PXFEM                 source/assembly/assadd2.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSINIT_PXFEM(
     1    ADDCNE,INOD, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE(*),IAD_ELEM(2,*),INOD(*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC,N
C-----------------------------------------------
C Calcul des tailles des comms optimis\E9es parith/on
C-----------------------------------------------
      IF(IMACH==3.AND.NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N = FR_ELEM(J)
            NOD = INOD(N)
            IF(NOD > 0 )THEN
               DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   LSD = LSD + 1
                 ELSEIF(PROCNE(CC)==I) THEN
                   LRC = LRC + 1
                 ENDIF
               ENDDO
            ENDIF 
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSINIT_CRKXFEM               source/assembly/assadd2.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSINIT_CRKXFEM(
     1    ADDCNE_CRKXFEM,INOD_CRKXFEM, IAD_ELEM, FR_ELEM, PROCNE, LISENDP, LIRECVP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE_CRKXFEM(*),IAD_ELEM(2,*),INOD_CRKXFEM(*),
     .        PROCNE(*),FR_ELEM(*), LISENDP, LIRECVP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, LOC_PROC, NOD, CC, LSD, LRC,N,
     .        LENR,K
C-----------------------------------------------
C Calcul des tailles des comms optimis\E9es parith/on
C-----------------------------------------------
      IF(IMACH==3.AND.NSPMD>0.AND.IPARIT>0)THEN
        LOC_PROC = ISPMD+1
C
        LSD = 0
        LRC = 0
        DO I = 1, NSPMD
         IF(I/=LOC_PROC) THEN
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N = FR_ELEM(J)
            NOD = INOD_CRKXFEM(N)
            IF(NOD > 0 )THEN    ! check if node is Xfem sur proc i /= proc_loc
               DO CC = ADDCNE_CRKXFEM(NOD),ADDCNE_CRKXFEM(NOD+1)-1
                 IF(PROCNE(CC)==LOC_PROC) THEN
                   LSD = LSD + 1           ! long de frontiere d'echange pour send
                 ELSEIF(PROCNE(CC)==I) THEN
                   LRC = LRC + 1           ! long de frontiere d'echange pour rcv
                 ENDIF
               ENDDO
            ENDIF 
          ENDDO
         ENDIF
        ENDDO
        LISENDP = LSD
        LIRECVP = LRC
      ELSE
        LISENDP = 0
        LIRECVP = 0
      END IF
C
      RETURN
      END

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
c
Chd|====================================================================
Chd|  SPMD_EXCH_CRKVEL              source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_CRKVEL(IAD_ELEM  ,FR_ELEM  ,INOD_CRK ,ITAB   ,
     .                            X         ,V        ,VR       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),INOD_CRK(*),ITAB(*)
      my_real X(*),V(*),VR(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,J,L1,L2,ILEV,ILAY,IXEL,IAD,KK,IL,IENR,EN0,ENR,
     .  NENR,SIZ,LEN1,LEN2,LENR,NNODX,NN,NSX,NCT,NC,COUNT,
     .  LOC_PROC,IERROR,MSGTYP1,MSGTYP2,MSGOFF1,MSGOFF2
      INTEGER STATUS(MPI_STATUS_SIZE),NODXSAV(NSPMD),
     .  REQ_R1(NSPMD),REQ_R2(NSPMD),REQ_S1(NSPMD),REQ_S2(NSPMD),
     .  IAD_SEND1(NSPMD+1),IAD_SEND2(NSPMD+1),
     .  IAD_RECV1(NSPMD+1),IAD_RECV2(NSPMD+1),NBSEND
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  RBUFI,SBUFI
      my_real, DIMENSION(:,:), ALLOCATABLE ::  RBUFR,SBUFR
c----
      DATA MSGOFF1/241/
      DATA MSGOFF2/242/
C=======================================================================
      LOC_PROC = ISPMD + 1
c
!      NENR = INT(IENRNOD/NLEVMAX)
      NENR = IENRNOD
      LENR = IAD_ELEM(1,NSPMD+1) - IAD_ELEM(1,1)
      LENR = LENR * NXLAYMAX * NENR
      ALLOCATE(RBUFI(3,LENR))
      ALLOCATE(SBUFI(3,LENR))
      ALLOCATE(RBUFR(9,LENR))
      ALLOCATE(SBUFR(9,LENR))
      RBUFI = 0
      SBUFI = 0
      RBUFR = ZERO
      SBUFR = ZERO
c      
      DO I=1,NSPMD                                          
        NNODX = 0                                           
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                
          IF (INOD_CRK(FR_ELEM(J)) > 0) THEN                      
            NNODX = NNODX + 1
          END IF                                            
        END DO    
        NODXSAV(I) = NNODX  ! number of Xfem nodes on the frontier     
      END DO   
c
c---  receive
c
      L1 = 1
      L2 = 1
      IAD_RECV1(1) = L1
      IAD_RECV2(1) = L2
c
      DO I=1,NSPMD                                          
        NBSEND = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)                                    
        NNODX = NODXSAV(I)                                      
        IF (NBSEND > 0) THEN                                  

          SIZ = NBSEND*NENR*NXLAYMAX
c---
          LEN1 = SIZ*3                 
          MSGTYP1 = MSGOFF1
          CALL MPI_IRECV(                                   
     S         RBUFI(1,L1),LEN1,MPI_INTEGER,IT_SPMD(I),MSGTYP1,  
     G         MPI_COMM_WORLD,REQ_R1(I),IERROR)                
          L1 = L1 + SIZ                                    
c---
          LEN2 = SIZ*9                 
          MSGTYP2 = MSGOFF2                                 
          CALL MPI_IRECV(                                   
     S         RBUFR(1,L2),LEN2,REAL,IT_SPMD(I),MSGTYP2,         
     G         MPI_COMM_WORLD,REQ_R2(I),IERROR)                
          L2 = L2 + SIZ                                    
c---
        ENDIF                                               
        IAD_RECV1(I+1) = L1                                 
        IAD_RECV2(I+1) = L2                                 
      END DO                                                
c
c---  send
c
      L1 = 1
      L2 = 1
      IAD_SEND1(1) = L1
      IAD_SEND2(1) = L2
c
      DO I=1,NSPMD
ccccc        IF (NODXSAV(I) > 0) THEN  
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NN  = FR_ELEM(J)
            NSX = INOD_CRK(NN)
            IF (NSX > 0) THEN
              DO ILAY=1,NXLAYMAX
                DO IENR=1,NENR   
                  IAD   = XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,IENR)  ! sender IAD
                  ILEV  = XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,IENR)  ! sender ILEV  
                  COUNT = XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,IENR)     
                  ENR = 0
                  IF (IAD > 0) ENR = CRKLVSET(ILEV)%ENR0(1,IAD) ! sender ENR
                  IF (ENR > 0 .and. ILEV > 0 .and. COUNT > 0) THEN
c                  IF (IAD > 0 .and. ILEV > 0 .and. COUNT > 0) THEN
                    SBUFI(1,L1) = IAD
                    SBUFI(2,L1) = ILEV
                    SBUFI(3,L1) = COUNT
                    SBUFR(1,L2) = CRKAVX(ILEV)%X(1,IAD)
                    SBUFR(2,L2) = CRKAVX(ILEV)%X(2,IAD)
                    SBUFR(3,L2) = CRKAVX(ILEV)%X(3,IAD)
                    SBUFR(4,L2) = CRKAVX(ILEV)%V(1,IAD)
                    SBUFR(5,L2) = CRKAVX(ILEV)%V(2,IAD)
                    SBUFR(6,L2) = CRKAVX(ILEV)%V(3,IAD)
                    SBUFR(7,L2) = CRKAVX(ILEV)%VR(1,IAD)
                    SBUFR(8,L2) = CRKAVX(ILEV)%VR(2,IAD)
                    SBUFR(9,L2) = CRKAVX(ILEV)%VR(3,IAD)
c                    if(ilay==1.and. (nsx==51.or.nsx==26))then
c                    write(*,'(A,3I5,3Z)')'SBUF,NSX,IAD,ILEV,V=',NSX,IAD,ILEV,SBUFR(1,L2),SBUFR(2,L2),SBUFR(3,L2)
c                    write(*,'(A,I)')'SBUF_sender: COUNT=',count
c                    endif
                    
                  ENDIF
                  L1 = L1 + 1                                    
                  L2 = L2 + 1
                END DO
              END DO
            ELSE
C               on envoie des zeros (initialise plus haut)
                L1 = L1+NENR*NXLAYMAX
                L2 = L2+NENR*NXLAYMAX
            ENDIF
           END DO                                                    
cccc        ENDIF
        IAD_SEND1(I+1) = L1
        IAD_SEND2(I+1) = L2
      ENDDO
c---
      DO I=1,NSPMD       
        IF( IAD_SEND1(I+1)-IAD_SEND1(I) > 0)THEN
c        IF (NODXSAV(I) > 0) THEN  
          MSGTYP1 = MSGOFF1                                 
          L1   = IAD_SEND1(I)                             
          SIZ  = IAD_SEND1(I+1)-IAD_SEND1(I)
          LEN1 = SIZ*3
          CALL MPI_ISEND(                                   
     S         SBUFI(1,L1),LEN1,MPI_INTEGER,IT_SPMD(I),MSGTYP1,  
     G         MPI_COMM_WORLD,REQ_S1(I),IERROR) 
c         ----------------------------------                
          MSGTYP2 = MSGOFF2                                 
          SIZ = IAD_SEND2(I+1)-IAD_SEND2(I)                
          L2  = IAD_SEND2(I)                               
          LEN2 = SIZ*9                 
          CALL MPI_ISEND(                                   
     S         SBUFR(1,L2),LEN2,REAL,IT_SPMD(I),MSGTYP2,         
     G         MPI_COMM_WORLD,REQ_S2(I),IERROR)                
        ENDIF                                               
      ENDDO                                                 
c
c---  uncompact received buffers
c
      DO I = 1,NSPMD                              
c        IF (NODXSAV(I) > 0) THEN  
        IF(IAD_RECV1(I+1)-IAD_RECV1(I) > 0)THEN
          CALL MPI_WAIT(REQ_R1(I),STATUS,IERROR)   
          CALL MPI_WAIT(REQ_R2(I),STATUS,IERROR)   
c
          L1 = IAD_RECV1(I)
          L2 = IAD_RECV2(I)
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1     
            NN  = FR_ELEM(J)
            NSX = INOD_CRK(NN) 
            IF (NSX > 0) THEN                     
              DO ILAY=1,NXLAYMAX
                DO IENR=1,NENR
                  IAD   = RBUFI(1,L1)  ! sender IAD
                  IL    = RBUFI(2,L1)  ! sender ILEV
                  COUNT = RBUFI(3,L1)
                  KK    = XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,IENR) ! receiver IAD
                  ILEV  = XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,IENR) ! receiver ILEV
                  IF (KK > 0) THEN
                    EN0 = CRKLVSET(ILEV)%ENR0(2,KK)  ! receiver ENR 
                    ENR = CRKLVSET(ILEV)%ENR0(1,KK)  ! receiver ENR
                  ELSE
                    EN0 = 0
                    ENR = 0
                  ENDIF
c--- 
c                  IF (EN0 > 0 .and. COUNT > 0 .and. IL > 0 .and. IAD > 0 .and. 
                  IF (EN0 <= 0 .and. ENR > 0 .and. COUNT > 0 .and. IL > 0 .and. IAD > 0 .and. 
     .                ILEV > 0 .and. KK > 0) THEN
     
c             write(*,'(A,4I5,3Z)') 'RCV_SPMD:ILEV,KK,ILS,IADS,VIT=',
c     .                RBUFR(5,L2), RBUFR(6,L2)  , RBUFR(7,L2)
     
                    CRKAVX(ILEV)%X(1,KK)  = RBUFR(1,L2)                  
                    CRKAVX(ILEV)%X(2,KK)  = RBUFR(2,L2)                  
                    CRKAVX(ILEV)%X(3,KK)  = RBUFR(3,L2)                  
                    CRKAVX(ILEV)%V(1,KK)  = RBUFR(4,L2)                  
                    CRKAVX(ILEV)%V(2,KK)  = RBUFR(5,L2)                  
                    CRKAVX(ILEV)%V(3,KK)  = RBUFR(6,L2)                  
                    CRKAVX(ILEV)%VR(1,KK) = RBUFR(7,L2)                  
                    CRKAVX(ILEV)%VR(2,KK) = RBUFR(8,L2)                  
                    CRKAVX(ILEV)%VR(3,KK) = RBUFR(9,L2)       
                    XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,IENR) = 0
                    XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,IENR) = 0
                  ENDIF
                  L1 = L1 + 1
                  L2 = L2 + 1                                 
c---
                ENDDO  ! IENR=1,NENR                                           
              ENDDO    ! ILAY=1,NXLAYMAX                                          
C si noeud n'est pas XFEM skipper la comm
            ELSE
                L1 = L1+NENR*NXLAYMAX
                L2 = L2+NENR*NXLAYMAX
            ENDIF      !  NSX > 0   
          ENDDO        !  J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                                            
       ENDIF                                                       
      ENDDO                                                       
c-----------
      DO I = 1,NSPMD                              
        IF (IAD_SEND1(I+1)-IAD_SEND1(I) > 0) THEN           
          CALL MPI_WAIT(REQ_S1(I),STATUS,IERROR)   
        ENDIF                                      
      ENDDO                                        
c
      DO I = 1, NSPMD                              
        IF (IAD_SEND2(I+1)-IAD_SEND2(I) > 0) THEN           
          CALL MPI_WAIT(REQ_S2(I),STATUS,IERROR)   
        ENDIF                                      
      ENDDO                                        
c-----------
      DEALLOCATE(RBUFI)
      DEALLOCATE(SBUFI)
      DEALLOCATE(RBUFR)
      DEALLOCATE(SBUFR)
c-----------
#endif
      RETURN
      END SUBROUTINE SPMD_EXCH_CRKVEL
c-----------

Chd|====================================================================
Chd|  SPMD_EXCH_TAGXP               source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_TAGXP(IAD_ELEM ,FR_ELEM ,INOD_CRK ,ITAB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),INOD_CRK(*),ITAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,L,ILAY,IAD,ILEV,KK,IL,EN1,IENR,ENR,SIZ,
     .  LEN,LENR,NNODX,NN,NSX,NCT,NC,
     .  LOC_PROC,IERROR,MSGTYP,MSGOFF,
     .  ENRL,ENRR,FLAGS,FLAGR,NENR
      INTEGER STATUS(MPI_STATUS_SIZE),NODXSAV(NSPMD),
     .  REQ_R(NSPMD),REQ_S(NSPMD),IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  RBUF,SBUF
c----
      DATA MSGOFF/241/
C=======================================================================
      LOC_PROC = ISPMD + 1
c
!      NENR = INT(IENRNOD/NLEVMAX)
      NENR = IENRNOD
      LENR = IAD_ELEM(1,NSPMD+1) - IAD_ELEM(1,1)
      LENR = LENR * NXLAYMAX * NENR
      ALLOCATE(RBUF(3,LENR))
      ALLOCATE(SBUF(3,LENR))
      RBUF = 0
      SBUF = 0
c      
      DO I=1,NSPMD                                          
        NNODX = 0                                           
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                
          IF (INOD_CRK(FR_ELEM(J)) > 0) THEN                      
            NNODX = NNODX + 1
          END IF                                            
        END DO    
        NODXSAV(I) = NNODX                                         
      END DO                                                
c
c---  receive
c
      L = 1
      IAD_RECV(1) = L
c
      DO I=1,NSPMD                                          
        NNODX = NODXSAV(I)                                      
        SIZ = NNODX*NENR*NXLAYMAX
c
        IF (NNODX > 0) THEN                                  
          MSGTYP = MSGOFF
          LEN = SIZ*3                                
          CALL MPI_IRECV(                                   
     S         RBUF(1,L),LEN,MPI_INTEGER,IT_SPMD(I),MSGTYP,  
     G         MPI_COMM_WORLD,REQ_R(I),IERROR)                
c
          L = L + SIZ                                    
        ENDIF                                               
        IAD_RECV(I+1) = L                                 
      END DO                                                
c
c---  send
c
      L = 1
      IAD_SEND(1) = L
      DO I=1,NSPMD
        IF (NODXSAV(I) > 0) THEN  
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NN  = FR_ELEM(J)
            NSX = INOD_CRK(NN)
            IF (NSX > 0) THEN
#include     "vectorize.inc"
              DO ILAY=1,NXLAYMAX
                DO IENR=1,NENR   
                  IAD  = XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,IENR)
                  ILEV = XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,IENR)  
                  ENRL = XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,IENR)  
                  IF (IAD > 0 .and. ILEV > 0 .and. ENRL == IENR) THEN
                    SBUF(1,L) = IAD
                    SBUF(2,L) = ILEV
                    SBUF(3,L) = ENRL
                  ELSE
                    SBUF(1,L) = 0
                    SBUF(2,L) = 0
                    SBUF(3,L) = 0
                  ENDIF
                  L = L + 1                                    
                END DO
              END DO
            ENDIF
          END DO                                                    
        ENDIF
        IAD_SEND(I+1) = L
      ENDDO
c---
      DO I=1,NSPMD                                          
        IF (NODXSAV(I) > 0) THEN  
          MSGTYP = MSGOFF                                 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L   = IAD_SEND(I)                             
          LEN = SIZ*3
          CALL MPI_ISEND(                                   
     S         SBUF(1,L),LEN,MPI_INTEGER,IT_SPMD(I),MSGTYP,  
     G         MPI_COMM_WORLD,REQ_S(I),IERROR) 
        ENDIF                                               
      ENDDO                                                 
c
c---  uncompact received buffers
c
      DO I = 1,NSPMD                              
        IF (NODXSAV(I) > 0) THEN  
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)   
c
          L = IAD_RECV(I)
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1     
            NN  = FR_ELEM(J)
            NSX = INOD_CRK(NN)
            IF (NSX > 0) THEN                     
              DO ILAY=1,NXLAYMAX
                DO IENR=1,NENR
                  IAD = RBUF(1,L)
                  IL  = RBUF(2,L)
                  ENR = RBUF(3,L)
                  IF (ENR > 0) THEN
                    XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,ENR) = IAD                                           
                    XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,ENR) = ILEV     
                    XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = ENR
                  ENDIF
                  L = L + 1
c---
                ENDDO  ! IENR=1,NENR                                           
              ENDDO    ! ILAY=1,NXLAYMAX                                          
            ENDIF      !  NSX > 0                                           
          ENDDO        !  J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1                                            
        ENDIF                                                       
      ENDDO                                                       
c-----------
      DO I = 1,NSPMD                              
        SIZ = IAD_SEND(I+1)-IAD_SEND(I)
        IF (SIZ > 0) THEN           
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)   
        ENDIF                                      
      ENDDO                                        
c-----------
      DEALLOCATE(RBUF)
      DEALLOCATE(SBUF)
c-----------
#endif
      RETURN
      END
c
Chd|====================================================================
Chd|  SPMD_EXCH_NODENR              source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|        SPMD_CRK_ADV                  source/elements/xfem/xfemfsky.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_NODENR(IAD_ELEM,FR_ELEM,SIZE,LENR,INOD_CRK,
     .                            ENRTAG,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),SIZE,LENR,INOD_CRK(*),FLAG,
     .        ENRTAG(NUMNOD,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,L,NB_NOD,NNOD,IENR,ENR,SIZN,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER RBUF(SIZE*LENR),SBUF(SIZE*LENR)
      DATA MSGOFF/229/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
#include      "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
cc          NNOD = INOD_CRK(NOD)
          DO IENR=1,SIZE
cc             SBUF(L+IENR-1) = 0
             IF(FLAG == 1)THEN
cc              IF(NNOD > 0)SBUF(L+IENR-1) =  CRKENR(IENR)%NODENR(NNOD)
              SBUF(L+IENR-1) = ENRTAG(NOD,IENR)
             ELSE IF(FLAG == 2)THEN
              SBUF(L+IENR-1) = 0
             ENDIF
          END DO
          L = L + SIZE
        END DO
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
      DO I = 1, NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
cc            NNOD = INOD_CRK(NOD)
            DO IENR=1,SIZE
               IF(FLAG==1)THEN
                 ENR = ENRTAG(NOD,IENR)
                 ENRTAG(NOD,IENR) = MAX(ENR,RBUF(L+IENR-1))
               ELSEIF(FLAG==2)THEN
cc                 ENR = 0
cc                 ENRTAG(NOD,IENR) = MIN(ENR,RBUF(L+IENR-1))
                 ENRTAG(NOD,IENR) = 0
               END IF
            END DO
            L = L + SIZE
          END DO
       ENDIF
      END DO
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
c-----------
#endif
      RETURN
      END SUBROUTINE SPMD_EXCH_NODENR
c-----------
C
Chd|====================================================================
Chd|  SPMD_EXCH_IEDGE               source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_IEDGE(IAD_EDGE,FR_EDGE,SIZE  ,LSDRC,FR_NBEDGE,
     .                           FLAG    ,CRKEDGE)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_EDGE(*),FR_EDGE(*),
     .        SIZE,LSDRC,FR_NBEDGE(*),FLAG
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,II,J,JJ,L0,L,CC,MSGTYP,LOC_PROC,IERROR,
     .        INDEX,SIZ,NBIRECV,IAD_RECV(NSPMD+1),
     .        STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD),
     .        REQ_S(NSPMD),IRINDEX(NSPMD),IED,ICUT,
     .        IBOUNDEDGE,NXLAY,ILAY,EN10,EN20,MSGOFF
      INTEGER RBUF(SIZE*LSDRC),
     .        SBUF(SIZE*LSDRC)
       DATA MSGOFF/228/
C=======================================================================
      LOC_PROC = ISPMD + 1
C
      NXLAY = INT(NLEVMAX/NXEL)
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(IAD_EDGE(I+1)-IAD_EDGE(I) > 0)THEN
          SIZ = SIZE*FR_NBEDGE(I)
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     .      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      ENDDO
c-----------------------------------------------------
      L = 1
c---
      IF (FLAG == 0) THEN
c---
        DO I=1,NSPMD
          IF (IAD_EDGE(I+1) > IAD_EDGE(I)) THEN
            L0 = L
#include "vectorize.inc"
            DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
              IED = FR_EDGE(J)
              IF(IED == 0)THEN
                DO ILAY=1,NXLAY
                   SBUF(L+ILAY-1) = 0
                ENDDO
              ELSE
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1) = CRKEDGE(ILAY)%IBORDEDGE(IED)
                ENDDO
              ENDIF
              L = L + SIZE
            END DO
            SIZ    = (IAD_EDGE(I+1)-IAD_EDGE(I))*SIZE
            MSGTYP = MSGOFF
            CALL MPI_ISEND(SBUF(L0),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                     MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
        ENDDO ! I=1,NSPMD
c---
      ELSE IF (FLAG == 1) THEN
c---
        DO I=1,NSPMD
          IF (IAD_EDGE(I+1) > IAD_EDGE(I)) THEN
            L0 = L
#include "vectorize.inc"
            DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
              IED = FR_EDGE(J)
              IF(IED == 0)THEN
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1) = 0
                ENDDO
              ELSE              
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1) = CRKEDGE(ILAY)%ICUTEDGE(IED)
                ENDDO
              ENDIF
              L = L + SIZE
            END DO
            SIZ    = (IAD_EDGE(I+1)-IAD_EDGE(I))*SIZE
            MSGTYP = MSGOFF
            CALL MPI_ISEND(SBUF(L0),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                     MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
        ENDDO ! I=1,NSPMD
c---
      ELSE IF (FLAG == 2) THEN
c---
        DO I=1,NSPMD
          IF (IAD_EDGE(I+1) > IAD_EDGE(I)) THEN
            L0 = L
#include "vectorize.inc"
            DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
              IED = FR_EDGE(J)
              IF(IED==0)THEN
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1) = 0
                ENDDO
              ELSE
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1) = CRKEDGE(ILAY)%ICUTEDGE(IED)
                ENDDO
              ENDIF
              L = L + SIZE
            END DO
            SIZ    = (IAD_EDGE(I+1)-IAD_EDGE(I))*SIZE
            MSGTYP = MSGOFF
            CALL MPI_ISEND(SBUF(L0),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                     MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
        ENDDO ! I=1,NSPMD
c---
      ELSE IF (FLAG == 3) THEN
c---
        DO I=1,NSPMD
          IF (IAD_EDGE(I+1) > IAD_EDGE(I)) THEN
            L0 = L
#include "vectorize.inc"
            DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
              IED = FR_EDGE(J)
              IF(IED==0)THEN
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1)         = 0
                  SBUF(L+ILAY-1+NXLAY)   = 0
                  SBUF(L+ILAY-1+NXLAY*2) = 0
                  SBUF(L+ILAY-1+NXLAY*3) = 0
                  SBUF(L+ILAY-1+NXLAY*4) = 0
                  SBUF(L+ILAY-1+NXLAY*5) = 0
                ENDDO

              ELSE
                DO ILAY=1,NXLAY
                  SBUF(L+ILAY-1)         = CRKEDGE(ILAY)%ICUTEDGE(IED)
                  SBUF(L+ILAY-1+NXLAY)   = CRKEDGE(ILAY)%EDGEENR(1,IED)
                  SBUF(L+ILAY-1+NXLAY*2) = CRKEDGE(ILAY)%EDGEENR(2,IED)
                  SBUF(L+ILAY-1+NXLAY*3) = CRKEDGE(ILAY)%EDGEICRK(IED)
                  SBUF(L+ILAY-1+NXLAY*4) = CRKEDGE(ILAY)%EDGETIP(1,IED)
                  SBUF(L+ILAY-1+NXLAY*5) = CRKEDGE(ILAY)%EDGETIP(2,IED)
                ENDDO
              ENDIF
              L = L + SIZE
            END DO
            SIZ    = (IAD_EDGE(I+1)-IAD_EDGE(I))*SIZE
            MSGTYP = MSGOFF
            CALL MPI_ISEND(SBUF(L0),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                     MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
        ENDDO ! I=1,NSPMD
c---
      END IF  ! FLAG 
c---
c--------------------
c     decompactage IRECV
c--------------------
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
#include    "vectorize.inc"
         DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
            IED = FR_EDGE(J)
            IF(IED/=0)THEN
            IF (FLAG == 0) THEN
              DO ILAY=1,NXLAY
                IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(IED)
                CRKEDGE(ILAY)%IBORDEDGE(IED) = 
     .                        MAX(RBUF(L+ILAY-1),IBOUNDEDGE)
              ENDDO
            ELSE IF (FLAG == 1) THEN
              DO ILAY=1,NXLAY
                ICUT = CRKEDGE(ILAY)%ICUTEDGE(IED)
                IF (ICUT + RBUF(L+ILAY-1) /= 4) THEN
                  CRKEDGE(ILAY)%ICUTEDGE(IED) = MAX(RBUF(L+ILAY-1),ICUT)
                ELSE
                  CRKEDGE(ILAY)%ICUTEDGE(IED) = 3
                END IF
              ENDDO
            ELSE IF (FLAG == 2) THEN
              DO ILAY=1,NXLAY
                ICUT = CRKEDGE(ILAY)%ICUTEDGE(IED)
                IF (ICUT > 0) CRKEDGE(ILAY)%ICUTEDGE(IED) = MIN(1,ICUT)
              ENDDO
            ELSE IF (FLAG == 3) THEN
             DO ILAY=1,NXLAY
               ICUT = CRKEDGE(ILAY)%ICUTEDGE(IED)
               EN10 = CRKEDGE(ILAY)%EDGEENR(1,IED)
               EN20 = CRKEDGE(ILAY)%EDGEENR(2,IED)
               IF (ICUT > 0) THEN
                 CRKEDGE(ILAY)%EDGEENR(1,IED)
     .                       = MAX(EN10,RBUF(L+ILAY-1+NXLAY))
                 CRKEDGE(ILAY)%EDGEENR(2,IED)
     .                       = MAX(EN20,RBUF(L+ILAY-1+2*NXLAY))

                 CRKEDGE(ILAY)%EDGEICRK(IED) = 
     .              MAX(CRKEDGE(ILAY)%EDGEICRK(IED),RBUF(L+ILAY-1+3*NXLAY))
                 CRKEDGE(ILAY)%EDGETIP(1,IED) = MAX(
     .               CRKEDGE(ILAY)%EDGETIP(1,IED),RBUF(L+ILAY-1+4*NXLAY))
                 CRKEDGE(ILAY)%EDGETIP(2,IED) = MAX(
     .               CRKEDGE(ILAY)%EDGETIP(2,IED),RBUF(L+ILAY-1+5*NXLAY))
               ENDIF
              ENDDO
            END IF
            ENDIF
            L = L + SIZE
         END DO
      END DO
c-----------
c     wait terminaison isend
c-----------
      DO I = 1, NSPMD
       IF(IAD_EDGE(I+1)-IAD_EDGE(I) > 0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
c-----------
#endif
      RETURN
      END SUBROUTINE SPMD_EXCH_IEDGE
c-----------
c
Chd|====================================================================
Chd|  SPMD_EXCH_REDGE               source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_REDGE(IAD_EDGE,FR_EDGE,SIZE ,LSDRC,FR_NBEDGE,CRKEDGE)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_EDGE(*),FR_EDGE(*),
     .        SIZE,LSDRC,FR_NBEDGE(*),FLAG
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,II,J,JJ,L0,L,CC,MSGTYP,LOC_PROC,IERROR,
     .        INDEX,SIZ,NBIRECV,IAD_RECV(NSPMD+1),
     .        STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD),
     .        REQ_S(NSPMD),IRINDEX(NSPMD),NXLAY,ILAY,IED,MSGOFF
      my_real
     .  RBUF(SIZE*LSDRC+NSPMD),SBUF(SIZE*LSDRC+NSPMD)
      DATA MSGOFF/230/
C=======================================================================
      LOC_PROC = ISPMD + 1
C
      NXLAY = INT(NLEVMAX/NXEL)
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(IAD_EDGE(I+1)-IAD_EDGE(I) > 0)THEN
          SIZ = SIZE*FR_NBEDGE(I)
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     .      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      ENDDO
C
      L = 1
C
      DO I=1,NSPMD
       IF(IAD_EDGE(I+1)-IAD_EDGE(I) > 0)THEN
         L0 = L
#include "vectorize.inc"
         DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
           IED = FR_EDGE(J)
           IF(IED > 0)THEN
             DO ILAY=1,NXLAY
               SBUF(L+ILAY-1) = CRKEDGE(ILAY)%RATIO(IED)
             ENDDO
           ENDIF
           L = L + SIZE
         END DO
C
C   echange messages
C
        SIZ = (IAD_EDGE(I+1)-IAD_EDGE(I))*SIZE
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     .    SBUF(L0),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .    MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
      ENDDO
C
C decompactage
C
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
#include    "vectorize.inc"
        DO J=IAD_EDGE(I),IAD_EDGE(I+1)-1
          IED = FR_EDGE(J)
          IF(IED >0)THEN
            DO ILAY=1,NXLAY
               IF (CRKEDGE(ILAY)%RATIO(IED) == ZERO) 
     .             CRKEDGE(ILAY)%RATIO(IED) = RBUF(L+ILAY-1)
            ENDDO
          ENDIF
          L = L + SIZE
        END DO
      END DO
C
C   wait terminaison isend
C
      DO I = 1, NSPMD
       IF(IAD_EDGE(I+1)-IAD_EDGE(I) > 0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
c-----------
#endif
      RETURN
      END SUBROUTINE SPMD_EXCH_REDGE
c-----------

Chd|====================================================================
Chd|  SPMD_EXCH_CRKAVX              source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_CRKAVX(
     .   IAD_ELEM,FR_ELEM,FR_NBCC1,LENS1 ,LENR1  ,
     .   IADSDP_CRK,IADRCP_CRK,ISENDP_CRK,IRECVP_CRK)
C--------------------------------------
       USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENS1,LENR1,IAD_ELEM(2,*),FR_ELEM(*),FR_NBCC1(2,*),
     .   IADSDP_CRK(*),IADRCP_CRK(*),ISENDP_CRK(*),IRECVP_CRK(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,II,L,L0,CC,IPT,SIZ,INDEX,LOC_PROC,IERROR,
     .        NB_NOD,NBIRECV,MSGTYP,MSGOFF,
     .        IAD_RECV(NSPMD+1),STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
c
      my_real RBUF(9*NLEVMAX*LENR1),SBUF(9*NLEVMAX*LENS1)
      DATA MSGOFF/243/
C=======================================================================
      LOC_PROC = ISPMD + 1
C
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF (IAD_ELEM(1,I+1) - IAD_ELEM(1,I) > 0) THEN
          SIZ =  9*NLEVMAX*FR_NBCC1(2,I)
          MSGTYP  = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      ENDDO
      L = 1
c-----------------------------------------------------------------------
      DO I=1,NSPMD
        IF (IAD_ELEM(1,I+1)-IAD_ELEM(1,I) > 0) THEN
          L0 = L
C
          DO J=IADSDP_CRK(I),IADSDP_CRK(I+1)-1
            CC = ISENDP_CRK(J)
            DO IPT = 1, NLEVMAX
              SBUF(L  ) = CRKAVX(IPT)%X(1,CC)
              SBUF(L+1) = CRKAVX(IPT)%X(2,CC)
              SBUF(L+2) = CRKAVX(IPT)%X(3,CC)
              SBUF(L+3) = CRKAVX(IPT)%V(1,CC)
              SBUF(L+4) = CRKAVX(IPT)%V(2,CC)
              SBUF(L+5) = CRKAVX(IPT)%V(3,CC)
              SBUF(L+6) = CRKAVX(IPT)%VR(1,CC)
              SBUF(L+7) = CRKAVX(IPT)%VR(2,CC)
              SBUF(L+8) = CRKAVX(IPT)%VR(3,CC)
c                     
              L = L + 9
            END DO
          ENDDO
c--------------------------------------------------------
c         echange messages
c--------------------------------------------------------
          SIZ = (IADSDP_CRK(I+1)-IADSDP_CRK(I))*NLEVMAX*9
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S      SBUF(L0),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
c
        ENDIF
      ENDDO
c--------------------------------------------------------
c     decompactage
c--------------------------------------------------------
      DO I=1,NSPMD
        IF (IAD_ELEM(1,I+1)-IAD_ELEM(1,I) > 0) THEN

          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
C
          L = IAD_RECV(I)
          DO J=IADRCP_CRK(I),IADRCP_CRK(I+1)-1
            CC = IRECVP_CRK(J)
            DO IPT = 1,NLEVMAX
              CRKAVX(IPT)%X(1,CC)  = RBUF(L)
              CRKAVX(IPT)%X(2,CC)  = RBUF(L+1)
              CRKAVX(IPT)%X(3,CC)  = RBUF(L+2)
              CRKAVX(IPT)%V(1,CC)  = RBUF(L+3)
              CRKAVX(IPT)%V(2,CC)  = RBUF(L+4)
              CRKAVX(IPT)%V(3,CC)  = RBUF(L+5)
              CRKAVX(IPT)%VR(1,CC) = RBUF(L+6)
              CRKAVX(IPT)%VR(2,CC) = RBUF(L+7)
              CRKAVX(IPT)%VR(3,CC) = RBUF(L+8)
c
              L = L + 9
            END DO
          END DO
        ENDIF
      END DO
c---------------------------
c     wait terminaison isend
c---------------------------
      DO I = 1, NSPMD
        IF (IAD_ELEM(1,I+1) - IAD_ELEM(1,I) > 0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
c-----------
#endif
      RETURN
      END
c
Chd|====================================================================
Chd|  SPMD_MAX_XFE_I                source/mpi/elements/spmd_xfem.F
Chd|-- called by -----------
Chd|        DELNUMBC_CRK                  source/output/anim/generate/delnumbc_crk.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MAX_XFE_I(INT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INT
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER RBUF(NSPMD)
      DATA MSGOFF/225/,MSGOFF2/226/
C=======================================================================
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF (ISPMD == 0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          INT = MAX(INT,RBUF(I))
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2
          CALL MPI_SEND(INT,SIZ,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF
        CALL MPI_SEND(INT,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(INT,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C-----------
#endif
      RETURN
      END




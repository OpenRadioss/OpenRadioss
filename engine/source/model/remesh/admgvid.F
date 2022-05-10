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
Chd|  ADMGVID                       source/model/remesh/admgvid.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMGVID(IPARG ,ELBUF_TAB,FSKYV,FSKY  ,FTHESKY,
     .                   IADC  ,IADTG    ,IFLG ,IGROUC,NGROUC ,
     .                   CONDNSKY )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IADC(4,*),  IADTG(3,*), IGROUC(*),
     .        NGROUC, IFLG
      my_real
     .   FSKYV(LSKY,8), FSKY(8,LSKY), FTHESKY(*),CONDNSKY(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NG,MLW,NEL, IG
C-----------------------------------------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
       NG = IGROUC(IG)
C--------
C     check group
C--------
       ITY     = IPARG(5,NG)
       MLW     = IPARG(1,NG)
       IF(MLW==0) CYCLE
       NEL     = IPARG(2,NG)
       NFT     = IPARG(3,NG)
       NPT     = IPARG(6,NG)
       JTHE    = IPARG(13,NG)
C--------
       LFT   = 1
       LLT   = MIN(NVSIZ,NEL)
       IPARG(8,NG)=1
C----6---------------------------------------------------------------7---------8
       DO I=LFT,LLT
         IF (ELBUF_TAB(NG)%GBUF%OFF(I) > ZERO) IPARG(8,NG)=0
       END DO
C----6---------------------------------------------------------------7---------8

       IF(IFLG/=0.AND.IPARIT/=0)THEN
        IF(IVECTOR==1)THEN
          IF(ITY==3)THEN
#include "vectorize.inc"
            DO I=LFT,LLT
              K=IADC(1,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
              K=IADC(2,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
              K=IADC(3,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
              K=IADC(4,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
            END DO
            IF(JTHE > 0 ) THEN
#include "vectorize.inc"
             DO I=LFT,LLT
               FTHESKY(IADC(1,NFT+I)) = ZERO
               FTHESKY(IADC(2,NFT+I)) = ZERO
               FTHESKY(IADC(3,NFT+I)) = ZERO
               FTHESKY(IADC(4,NFT+I)) = ZERO
             ENDDO         
            ENDIF
            IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
             DO I=LFT,LLT
               CONDNSKY(IADC(1,NFT+I)) = ZERO
               CONDNSKY(IADC(2,NFT+I)) = ZERO
               CONDNSKY(IADC(3,NFT+I)) = ZERO
               CONDNSKY(IADC(4,NFT+I)) = ZERO
             ENDDO    
            ENDIF
          ELSE
#include "vectorize.inc"
            DO I=LFT,LLT
              K=IADTG(1,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
              K=IADTG(2,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
              K=IADTG(3,NFT+I)
              FSKYV(K,1)=ZERO
              FSKYV(K,2)=ZERO
              FSKYV(K,3)=ZERO
              FSKYV(K,4)=ZERO
              FSKYV(K,5)=ZERO
              FSKYV(K,6)=ZERO
              FSKYV(K,7)=ZERO
              FSKYV(K,8)=ZERO
            END DO
            IF(JTHE > 0 ) THEN
#include "vectorize.inc"
             DO I=LFT,LLT
               FTHESKY(IADTG(1,NFT+I)) = ZERO
               FTHESKY(IADTG(2,NFT+I)) = ZERO
               FTHESKY(IADTG(3,NFT+I)) = ZERO
             ENDDO         
            ENDIF
            IF(NODADT_THERM > 0 ) THEN
#include "vectorize.inc"
             DO I=LFT,LLT
               CONDNSKY(IADTG(1,NFT+I)) = ZERO
               CONDNSKY(IADTG(2,NFT+I)) = ZERO
               CONDNSKY(IADTG(3,NFT+I)) = ZERO
             ENDDO    
            ENDIF
          END IF
        ELSE
          IF(ITY==3)THEN
            DO I=LFT,LLT
              DO J=1,4
               K=IADC(J,NFT+I)
               DO L=1,8
                FSKY(L,K)=ZERO
               END DO
              END DO
            END DO
            IF(JTHE > 0 ) THEN
             DO I=LFT,LLT
               FTHESKY(IADC(1,NFT+I)) = ZERO
               FTHESKY(IADC(2,NFT+I)) = ZERO
               FTHESKY(IADC(3,NFT+I)) = ZERO
               FTHESKY(IADC(4,NFT+I)) = ZERO
             ENDDO         
            ENDIF
            IF(NODADT_THERM > 0 ) THEN
             DO I=LFT,LLT
               CONDNSKY(IADC(1,NFT+I)) = ZERO
               CONDNSKY(IADC(2,NFT+I)) = ZERO
               CONDNSKY(IADC(3,NFT+I)) = ZERO
               CONDNSKY(IADC(4,NFT+I)) = ZERO
             ENDDO    
            ENDIF
          ELSE
            DO I=LFT,LLT
              DO J=1,3
               K=IADTG(J,NFT+I)
               DO L=1,8
                FSKY(L,K)=ZERO
               END DO
              END DO
            END DO
            IF(JTHE > 0 ) THEN
             DO I=LFT,LLT
               FTHESKY(IADTG(1,NFT+I)) = ZERO
               FTHESKY(IADTG(2,NFT+I)) = ZERO
               FTHESKY(IADTG(3,NFT+I)) = ZERO
             ENDDO         
            ENDIF
            IF(NODADT_THERM > 0 ) THEN
             DO I=LFT,LLT
               CONDNSKY(IADTG(1,NFT+I)) = ZERO
               CONDNSKY(IADTG(2,NFT+I)) = ZERO
               CONDNSKY(IADTG(3,NFT+I)) = ZERO
             ENDDO    
            ENDIF
          END IF
        END IF
       END IF
      END DO
!$OMP END DO
c      GOTO 100
c 101  CONTINUE

      RETURN
      END     

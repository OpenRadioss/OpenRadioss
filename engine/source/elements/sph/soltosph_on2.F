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
Chd|  SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SOLTOSPH_MOD                  share/modules/soltosph_mod.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SOLTOSPH_ON2(
     .   X      ,SPBUF    ,KXSP   ,IPARTSP ,ELBUF_TAB,
     .   IPARG  ,NGROUNC ,IGROUNC ,ITASK   ,IXSP     ,
     .   NOD2SP ,SOL2SPH ,WASPACT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE ELBUFDEF_MOD 
      USE MESSAGE_MOD 
      USE SOLTOSPH_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),
     .        IPARTSP(*), IPARG(NPARG,*), NGROUNC, 
     .        IGROUNC(*), ITASK, IXSP(KVOISPH,*), NOD2SP(*), 
     .        SOL2SPH(2,*), WASPACT(*)
      my_real
     .        X(3,*), SPBUF(NSPBUF,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  N, IP, KP, NG, MG, J, NP, KFT, IG, NELEM,
     .        NEL, OFFSET, NVOIS, M, JNOD, NN, IERROR
C                                                                    
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF, GBUFSP
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
      IF(ITASK==0)THEN
        ALLOCATE(WSPCLOUD(NUMSPH),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='(SOLIDS to SPH)')
          CALL ARRET(2)
        ENDIF
        WSPCLOUD(1:NUMSPH)=0
      END IF
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)
C
C look for groups to awake (active particles plus cloud active particles)       
        IF (IDDW>0) CALL STARTIMEG(NG)
        DO NELEM = 1,IPARG(2,NG),NVSIZ
          OFFSET = NELEM - 1
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-NELEM+1)
          IF(ITY==51) THEN
C-----------
            DO I=LFT,LLT
              NP=NFT+I
              IF(KXSP(2,NP)>0) THEN
                IPARG(8,NG)=0
                WSPCLOUD(NP)=1
              ELSEIF(KXSP(2,NP)<0) THEN
                NVOIS=KXSP(4,NP)
                DO J=1,NVOIS
                  JNOD=IXSP(J,NP)
                  IF(JNOD>0)THEN
                    M=NOD2SP(JNOD)
                    IF(KXSP(2,M)>0)THEN
                      IPARG(8,NG)=0
                      WSPCLOUD(NP)=1
                      EXIT
                    END IF
                  ELSE
                    NN = -JNOD
                    IF(NINT(XSPHR(13,NN))>0)THEN
                      IPARG(8,NG)=0
                      WSPCLOUD(NP)=1
                      EXIT
                    END IF
                  END IF
                END DO
              END IF
            ENDDO
          END IF
        END DO
        IF (IDDW>0) CALL STOPTIMEG(NG)
C--------
      END DO
!$OMP END DO
C-----------------------------------------------
      IF(ITASK==0)THEN
        DO NP=1,NUMSPH
          IF(WSPCLOUD(NP)/=0)THEN
            NSPHACT=NSPHACT+1
            WASPACT(NSPHACT)=NP
          END IF
        END DO
        DEALLOCATE(WSPCLOUD)
      END IF
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPH_ON2

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
Chd|  INIVOL_SET                    source/initial_conditions/inivol/inivol_set.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INIVOL_SET(UVAR,NUVAR,NEL,KVOL, MLW, ELBUF_TAB, NG,NBSUBMAT,MULTI_FVM,IXS,IXQ,IXTG,IDP,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE MULTI_FVM_MOD
      USE ELBUFDEF_MOD 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                 :: NEL, NUVAR, MLW, NG, NBSUBMAT,IDP,IPART(*)
      INTEGER,INTENT(INOUT)              :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG)
      my_real,INTENT(INOUT)              :: KVOL(NBSUBMAT,NEL)
      my_real,INTENT(INOUT)              :: UVAR(NEL,NUVAR)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN)  :: MULTI_FVM
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP),INTENT(INOUT) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      INTEGER                 :: I, IMAT, KK
      TYPE(G_BUFEL_) ,POINTER :: GBUF   
      TYPE(L_BUFEL_) ,POINTER :: LBUF   
      my_real                 :: P, P1,P2,P3,P4,SUMVF,RATIO
      LOGICAL                 :: PASSED
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
 
      !---CHECK UNOCCUPIPED SUBVOLUME AND FILL WITH PHASE 1. IF SUM(vfrac>1) display an error message.

      PASSED = .TRUE.
      DO I=1,NEL
       IF(IPART(I+NFT) /= IDP)CYCLE
        PASSED = .TRUE.
        SUMVF = SUM(KVOL(1:NBSUBMAT,I))
        IF(SUMVF>ONE+EM06)THEN
          !not expected, user input must be checked : error message
          SUMVF = SUM(KVOL(2:NBSUBMAT,I))
          IF(SUMVF <= ONE .AND. SUMVF > ZERO)THEN
            KVOL(1,I)=ONE-SUMVF
            PASSED = .TRUE.
          ELSE
            SUMVF = SUM(KVOL(1:NBSUBMAT,I))
            RATIO=ONE/SUMVF
            KVOL(1:NBSUBMAT,I)=RATIO*KVOL(1:NBSUBMAT,I)          
            PASSED=.TRUE.
          ENDIF
        ELSEIF(SUMVF<ONE-EM06)THEN
          !fill unoccupied subvolume with phase-1
          KVOL(1,I) = KVOL(1,I) + ONE-SUMVF
        ELSEIF(SUMVF>=ONE-EM06 .AND. SUMVF<=ONE+EM06)THEN
          !get rid of precision issue so that sumvf is exactly 1.000000
          RATIO=ONE/SUMVF
          KVOL(1:NBSUBMAT,I)=RATIO*KVOL(1:NBSUBMAT,I)
        ENDIF
      ENDDO

      IF(.NOT.PASSED)THEN
          !display error message : sumvf must be <= 1.000000
             CALL ANCMSG(MSGID=1597,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   R1=SUMVF,
     .                   I1=IXS(11,I+NFT)
     .                   )
       ELSE 
        !---set initial volumetric fraction using KVOL array    
        IF(MLW==51)THEN
          DO IMAT=1,4
            KK = N0PHAS + (IMAT-1)*NVPHAS        
            DO I=1,NEL
              IF(IPART(I+NFT) /= IDP)CYCLE                          
              UVAR(I,1+KK)   = KVOL(IMAT,I)  
              UVAR(I,23+KK)  = KVOL(IMAT,I)  
            ENDDO
          ENDDO
          DO I=1,NEL
            IF(IPART(I+NFT) /= IDP)CYCLE
            KK  = N0PHAS + (1-1)*NVPHAS
            P1  = UVAR(I,18+KK) 
            KK  = N0PHAS + (2-1)*NVPHAS
            P2  = UVAR(I,18+KK)
            KK  = N0PHAS + (3-1)*NVPHAS
            P3  = UVAR(I,18+KK)
            KK  = N0PHAS + (4-1)*NVPHAS
            P4  = UVAR(I,18+KK)
            SUMVF=SUM(KVOL(1:NBSUBMAT,I))
            P   = KVOL(1,I)*P1 + KVOL(2,I)*P2 + KVOL(3,I)*P3 + KVOL(4,I)*P4
            UVAR(I,4) = P
          ENDDO
        ELSEIF(MLW==37)THEN    
          DO I=1,NEL 
            IF(IPART(I+NFT) /= IDP)CYCLE                         
            UVAR(I,4)  = KVOL(1,I)  
            UVAR(I,5)  = KVOL(2,I)  
          ENDDO
        ELSEIF(MLW==151)THEN 
          GBUF => ELBUF_TAB(NG)%GBUF
          DO IMAT=1,MULTI_FVM%NBMAT   
            LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1,1,1)   
            DO I=1,NEL   
              IF(IPART(I+NFT) /= IDP)CYCLE
              LBUF%VOL(I) = KVOL(IMAT,I) * GBUF%VOL(I)                     
            ENDDO
          ENDDO
        ENDIF
      ENDIF !IF(.NOT.PASSED)


      RETURN
      END

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
Chd|  UPDATE_STRUCT_INT21           source/interfaces/int21/update_struct_int21.F
Chd|-- called by -----------
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE UPDATE_STRUCT_INT21(
     1                          CAND_E, II_STOK, IFORM , NIN   , NMN,
     2                          INTTH, MNDD   , MSR_L , IRECTT )
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
      INTEGER  NIN,NMN,IFORM
      INTEGER  MSR_L(*),MNDD(*)
      INTEGER CAND_E(*)
      INTEGER II_STOK,INTTH,IRECTT(4,*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP0, IP1, II,IFLAGLOADP,
     .        N,L,PP,J_STOK,IAD(NSPMD),
     .        TAG(NMN),NM(4),NODFI,PTR, IERROR1,IERROR2, IERROR3,
     .        IERROR4,LSKYFI

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C UPDATE STRUCTURE NMNSI and NMNFI FOR SPMD COMMUNICATION
C
       IFLAGLOADP = 0

        IF(NSPMD > 1.AND.(INTTH == 2.OR.IFLAGLOADP > 0)) THEN
            CALL MY_BARRIER
!$OMP MASTER
            TAG(1:NMN)=0
            NODFI = 0
            NMNFI(NIN)%P(1:NSPMD) = 0
            DO I=1,II_STOK 
                L = CAND_E(I)
                IF(L/=0) THEN             
                    NM(1:4) = IRECTT(1:4,L)
                    DO J=1,4
                        II = NM(J)
                        PP = MNDD(II)
                        IF(PP/=0.AND.TAG(II) == 0) THEN                     
                            NODFI = NODFI + 1
                            NMNFI(NIN)%P(PP) = NMNFI(NIN)%P(PP) +1
                            TAG(II)=1
                        ENDIF 
                    ENDDO
                ENDIF
            ENDDO 

            IF(ASSOCIATED( NMVFI(NIN)%P )) DEALLOCATE(NMVFI(NIN)%P)
            ALLOCATE(NMVFI(NIN)%P(NODFI),STAT=IERROR1)
C
            IAD(1)=1
            DO I=1,NSPMD-1
                IAD(I+1) = IAD(I)+NMNFI(NIN)%P(I)
            ENDDO

C
            TAG(1:NMN)=0
            DO I=1,II_STOK 
                L = CAND_E(I)
                IF(L/=0) THEN 
                    NM(1:4) = IRECTT(1:4,L)  
                    DO J=1,4
                        II=NM(J)
                        PP = MNDD(II)
                        IF(PP/=0.AND.TAG(II)==0)  THEN
                            PTR = IAD(PP)
                            NMVFI(NIN)%P(PTR) = II
                            TAG(II)=1
                            IAD(PP)=IAD(PP)+1
                            MSR_L(II) = -PTR
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO 
       

C
            IF(IFORM /= 0) THEN 
                IF(IPARIT==0) THEN
                    IF(NODFI>0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
                    DO I = 1, NODFI*NTHREAD
                        FTHEFI(NIN)%P(I) = ZERO
                    ENDDO
                ELSE
                    IF(ASSOCIATED(FTHESKYFI(NIN)%P)) DEALLOCATE(FTHESKYFI(NIN)%P)
                    LSKYFI = NODFI*MULTIMAX
                    NLSKYFI(NIN) = LSKYFI
                    IF(LSKYFI>0) THEN
                        ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
                        ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR4)
                    ENDIF
                ENDIF
            ENDIF

            IF (IFLAGLOADP > 0) THEN
                IF(NODFI>0) THEN 
                  IF(ASSOCIATED( TAGNCONTFI(NIN)%P )) DEALLOCATE(TAGNCONTFI(NIN)%P)
                   ALLOCATE(TAGNCONTFI(NIN)%P(NODFI),STAT=IERROR2)   
                     DO I = 1, NODFI
                        TAGNCONTFI(NIN)%P(I) = 0
                    ENDDO
                ENDIF            
            ENDIF
!$OMP END MASTER
            CALL MY_BARRIER     !   there isn't any implicit barrier at the end 
                                !   of an omp main condition
        ENDIF    

        RETURN
        END SUBROUTINE UPDATE_STRUCT_INT21

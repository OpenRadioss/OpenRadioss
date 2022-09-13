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
Chd|  SPMD_CHECK_TAG                source/mpi/ams/spmd_check_tag.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_SMST2               source/mpi/ams/spmd_exch_smst2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_CHECK_TAG(NIN,I_STOK,INTBUF_TAB,TAG,NSNR,
     .                          NSN,NTY,INACTI,IFQ,ITIED,NRTS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFDEF_MOD
      USE TRI7BOX
      USE MESSAGE_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
! ********************************************************
! *  variable  *  type  *  size  *  intent  *  feature
! *------------*--------*--------*----------*-------------
! *    NIN     * integ. *   1    *    in    *  which interface
! *   NSNR     * integ. *   1    *    in    *  size of TAG
! *   NTY      * integ. *   1    *    in    *  kind of interface
! *   INACTI   * integ. *   1    *    in    *  INACTI option
! *   IFQ      * integ. *   1    *    in    *  IFQ option
! *   ITIED    * integ. *   1    *    in    *  ITIED option
! *  I_STOK    * integ. *   1    *   inout  *  number of cand_a/e
! *    TAG     * integ. * NSNR   *    in    *  tag array
! * INTBUF_TAB * struct.*        *   inout  *  interface pointer
! *    NRTS    * integ. *   1    *    in    *  useful for interface typ11
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NIN,NSNR,NSN,NTY,INACTI,IFQ,ITIED,NRTS
      INTEGER, INTENT(INOUT) :: I_STOK
      INTEGER, DIMENSION(NSNR), INTENT(IN) :: TAG

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,K,SIZE_LOC,IERROR,NI,NSN_LOC
      INTEGER, DIMENSION(:), ALLOCATABLE :: CAND_N_LOC,CAND_E_LOC,IFPEN_LOC
      my_real, 
     .  DIMENSION(:), ALLOCATABLE :: CAND_P_LOC,CAND_FX_LOC,CAND_FY_LOC,CAND_FZ_LOC,
     .                               CAND_F_LOC
! ----------------------------------------------
!       we check the value of tag array on each nin interface 
!       and for each process
!       if NSVSI(NIN)P < 0 --> cand_n and cand_e are deleted
!       else cand_n and cand_e are kept
        IERROR = 0
        L = 1
        ! -----------------
        IF(I_STOK>0) THEN
           ! -----------------
            ALLOCATE( CAND_N_LOC( I_STOK ) , STAT=IERROR)
            IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
            ENDIF
            CAND_N_LOC(1:I_STOK) = 0
            ! ---------
            ALLOCATE( CAND_E_LOC( I_STOK ) , STAT=IERROR)
            IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
            ENDIF
            CAND_E_LOC(1:I_STOK) = 0 
            ! ---------
            IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7)) THEN
                ALLOCATE( CAND_P_LOC( I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                CAND_P_LOC(1:I_STOK) = 0      
            ENDIF
            ! ---------
            IF((NTY==7.OR.NTY==20).AND.IFQ>0) THEN
                ALLOCATE( CAND_FX_LOC( I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                CAND_FX_LOC(1:I_STOK) = 0 
                ! ---------
                ALLOCATE( CAND_FY_LOC( I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                CAND_FY_LOC(1:I_STOK) = 0 
                ! ---------
                ALLOCATE( CAND_FZ_LOC( I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                CAND_FZ_LOC(1:I_STOK) = 0
                ! ---------
                ALLOCATE( IFPEN_LOC( I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                IFPEN_LOC(1:I_STOK) = 0  
                ! ---------
            ENDIF 
            ! ---------
            IF(NTY==7.AND.ITIED/=0) THEN
                ALLOCATE( CAND_F_LOC( 8*I_STOK ) , STAT=IERROR)
                IF(IERROR/=0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                ENDIF
                CAND_F_LOC(1:8*I_STOK) = 0      
            ENDIF
            L = 1       
            IF(nty/=11) THEN
             NSN_LOC = NSN
            ELSE
             NSN_LOC = NRTS
            ENDIF
            DO K=1,I_STOK
              NI = INTBUF_TAB%CAND_N(K)
              IF(NI > NSN_LOC) THEN
              ! Remote 
               NI = NI - NSN_LOC
               IF(TAG(NI)>-1) THEN
                 CAND_N_LOC(L) = INTBUF_TAB%CAND_N(K)
                 CAND_E_LOC(L) = INTBUF_TAB%CAND_E(K)
                 IF(NTY==24.OR.NTY==25)THEN ! rien a faire (a TT=0)
                 ELSE
                   IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7)) 
     .              CAND_P_LOC(L) = INTBUF_TAB%CAND_P(K)
                   IF(IFQ>0) THEN
                    IFPEN_LOC(L) = INTBUF_TAB%IFPEN(K)
                    IF(NTY==20) THEN
                     CAND_FX_LOC(L) = INTBUF_TAB%CAND_FX(K)
                     CAND_FY_LOC(L) = INTBUF_TAB%CAND_FY(K)
                     CAND_FZ_LOC(L) = INTBUF_TAB%CAND_FZ(K)
                    ELSEIF(NTY==7) THEN
                     CAND_FX_LOC(L) = INTBUF_TAB%FTSAVX(K)
                     CAND_FY_LOC(L) = INTBUF_TAB%FTSAVY(K)
                     CAND_FZ_LOC(L) = INTBUF_TAB%FTSAVZ(K)
                    ENDIF
                   ENDIF
                   IF(NTY==7.AND.ITIED/=0) 
     .              CAND_F_LOC(8*(L-1)+1:8*(L-1)+8) = INTBUF_TAB%CAND_F(8*(K-1)+1:8*(K-1)+8)
                 ENDIF
                 L = L + 1
               ENDIF
              ELSE
              ! Local
                 CAND_N_LOC(L) = INTBUF_TAB%CAND_N(K)
                 CAND_E_LOC(L) = INTBUF_TAB%CAND_E(K)
                 IF(NTY==24.OR.NTY==25)THEN ! rien a faire (a TT=0) 
                 ELSE
                   IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7))
     .              CAND_P_LOC(L) = INTBUF_TAB%CAND_P(K)
                   IF(IFQ>0) THEN
                    IFPEN_LOC(L) = INTBUF_TAB%IFPEN(K)
                    IF(NTY==20) THEN
                    CAND_FX_LOC(L) = INTBUF_TAB%CAND_FX(K)
                    CAND_FY_LOC(L) = INTBUF_TAB%CAND_FY(K)
                    CAND_FZ_LOC(L) = INTBUF_TAB%CAND_FZ(K)
                    ELSEIF(NTY==7) THEN
                     CAND_FX_LOC(L) = INTBUF_TAB%FTSAVX(K)
                     CAND_FY_LOC(L) = INTBUF_TAB%FTSAVY(K)
                     CAND_FZ_LOC(L) = INTBUF_TAB%FTSAVZ(K)
                    ENDIF
                   ENDIF
                   IF(NTY==7.AND.ITIED/=0) 
     .              CAND_F_LOC(8*(L-1)+1:8*(L-1)+8) = INTBUF_TAB%CAND_F(8*(K-1)+1:8*(K-1)+8)
                 ENDIF
                 L = L + 1
              ENDIF
            ENDDO
            INTBUF_TAB%CAND_N(1:I_STOK) = 0
            INTBUF_TAB%CAND_E(1:I_STOK) = 0
            IF(NTY==24.OR.NTY==25)THEN ! rien a faire (a TT=0) 
            ELSE
              IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7))
     .           INTBUF_TAB%CAND_P(1:I_STOK) = 0
              IF(IFQ>0) THEN
               INTBUF_TAB%IFPEN(1:I_STOK) = 0
               IF(NTY==20) THEN
                INTBUF_TAB%CAND_FX(1:I_STOK) = 0
                INTBUF_TAB%CAND_FY(1:I_STOK) = 0
                INTBUF_TAB%CAND_FZ(1:I_STOK) = 0
               ELSEIF(NTY==7) THEN
                INTBUF_TAB%FTSAVX(1:I_STOK) = 0
                INTBUF_TAB%FTSAVY(1:I_STOK) = 0
                INTBUF_TAB%FTSAVZ(1:I_STOK) = 0
               ENDIF
              ENDIF
              IF(NTY==7.AND.ITIED/=0) 
     .         INTBUF_TAB%CAND_F(1:8*I_STOK) = ZERO
            ENDIF
            I_STOK = L - 1
            IF(I_STOK>0) THEN
             INTBUF_TAB%CAND_N(1:I_STOK) = CAND_N_LOC(1:I_STOK)
             INTBUF_TAB%CAND_E(1:I_STOK) = CAND_E_LOC(1:I_STOK)
             IF(NTY==24.OR.NTY==25)THEN ! rien a faire 
             ELSE
               IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7)) 
     .            INTBUF_TAB%CAND_P(1:I_STOK) = CAND_P_LOC(1:I_STOK)
               IF(IFQ>0) THEN
                INTBUF_TAB%IFPEN(1:I_STOK) = IFPEN_LOC(1:I_STOK)
                IF(NTY==20) THEN
                 INTBUF_TAB%CAND_FX(1:I_STOK) = CAND_FX_LOC(1:I_STOK)
                 INTBUF_TAB%CAND_FY(1:I_STOK) = CAND_FY_LOC(1:I_STOK)
                 INTBUF_TAB%CAND_FZ(1:I_STOK) = CAND_FZ_LOC(1:I_STOK)
                ELSEIF(NTY==7) THEN
                 INTBUF_TAB%FTSAVX(1:I_STOK) = CAND_FX_LOC(1:I_STOK)
                 INTBUF_TAB%FTSAVY(1:I_STOK) = CAND_FY_LOC(1:I_STOK)
                 INTBUF_TAB%FTSAVZ(1:I_STOK) = CAND_FZ_LOC(1:I_STOK)
                ENDIF
              ENDIF
              IF(NTY==7.AND.ITIED/=0) 
     .         INTBUF_TAB%CAND_F(1:8*I_STOK) = CAND_F_LOC(1:8*I_STOK)
            ENDIF
            ENDIF
            DEALLOCATE( CAND_N_LOC )
            DEALLOCATE( CAND_E_LOC )
            IF(NTY==24.OR.NTY==25)THEN ! rien a faire 
            ELSE
              IF((NTY==7.OR.NTY==20).AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7))
     .          DEALLOCATE( CAND_P_LOC )
              IF(IFQ>0) THEN
               DEALLOCATE( CAND_FX_LOC )
               DEALLOCATE( CAND_FY_LOC )
               DEALLOCATE( CAND_FZ_LOC )
               DEALLOCATE( IFPEN_LOC   )
              ENDIF
              IF(NTY==7.AND.ITIED/=0) 
     .          DEALLOCATE( CAND_F_LOC )
            ENDIF
           ! -----------------   
        ENDIF        ! i_sotk > 0
        ! -----------------    
        IF(IERROR/=0) THEN
           CALL ARRET(2)
        ENDIF 
      RETURN
      END SUBROUTINE


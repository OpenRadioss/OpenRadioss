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
Chd|  INTER_TRC_7                   source/interfaces/int07/inter_trc_7.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I7TRC                         source/interfaces/intsort/i7trc.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|====================================================================
        SUBROUTINE INTER_TRC_7( ITASK,NIN,IPARI,IND_IMP,
     1                          INTBUF_TAB,NB_INTER_SORTED,LIST_INTER_SORTED,INTER_STRUCT)
!$COMMENT
!       INTER_TRC_7 description
!       INTER_TRC_7 organization :      
!$ENDCOMMENT
C============================================================================
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK    ! thread id
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
        INTEGER, DIMENSION(*), INTENT(inout) :: IND_IMP    !   index for implicit option (pointer, cannot define the dimension)
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB    ! interface data
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NIN
        INTEGER :: INACTI,IFQ,NSN,NSNROLD,INACIMP
        INTEGER :: I,N,K,NN,NI,ITIED
        INTEGER :: I_STOK,CAND_T,CAND_TF
        INTEGER :: NUM_IMP
        INTEGER :: KK
C-----------------------------------------------

        NUM_IMP = 0
        DO KK=1,NB_INTER_SORTED
            NIN = LIST_INTER_SORTED(KK)
            INACTI = IPARI(22,NIN)
            IFQ = IPARI(31,NIN)
            ITIED = IPARI(85,NIN)

            INTER_STRUCT(NIN)%INACTI=INACTI
            INTER_STRUCT(NIN)%INACTII = INACTI

            NSN     =IPARI(5,NIN)
            IF( INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.
     .          NUM_IMP>0.OR.ITIED/=0 )THEN
                NSNROLD = IPARI(24,NIN)
            ELSE
                NSNROLD = 0
            ENDIF

            IF( INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.
     .          NUM_IMP>0.OR.ITIED/=0)THEN
                INTER_STRUCT(NIN)%INACTII=INACTI
                IF (NUM_IMP>0.AND.(INACTI/=5.AND.INACTI/=6.AND.IFQ<=0)) THEN
                    INACIMP = 0
                ELSE
                    INACIMP = 1
                ENDIF
    
                INTER_STRUCT(NIN)%I_SK_OLD = INTBUF_TAB(NIN)%I_STOK(1)
    
                IF( INTER_STRUCT(NIN)%SIZE_CAND_A<NSN+NSNROLD+3) THEN
                    IF(ALLOCATED(INTER_STRUCT(NIN)%CAND_A)) DEALLOCATE(INTER_STRUCT(NIN)%CAND_A)
                    ALLOCATE(INTER_STRUCT(NIN)%CAND_A(NSN+NSNROLD+3) )
                    INTER_STRUCT(NIN)%SIZE_CAND_A=NSN+NSNROLD+3
                ENDIF

C=============================================
C     LES NOEUDS DELETES DEVIENNENT NSN+NSNROLD+1 
C=============================================
C------for travelling nodes detected in imp_solv----- 
                I_STOK = INTBUF_TAB(NIN)%I_STOK(1)
                CALL I7TRC(
     1            NSN+NSNROLD      ,INTER_STRUCT(NIN)%I_SK_OLD,INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%CAND_E,
     2            INTBUF_TAB(NIN)%CAND_P,INTBUF_TAB(NIN)%FTSAVX,INTBUF_TAB(NIN)%FTSAVY,INTBUF_TAB(NIN)%FTSAVZ,
     3            INTER_STRUCT(NIN)%CAND_A,INTBUF_TAB(NIN)%IFPEN ,INACTI,IFQ,
     4            NUM_IMP          ,IND_IMP          ,INTBUF_TAB(NIN)%STFNS ,NIN              ,
     5            NSN ,ITIED,INTBUF_TAB(NIN)%CAND_F)

                IF(INTER_STRUCT(NIN)%I_SK_OLD==0) THEN
                    INTER_STRUCT(NIN)%INACTI=-ABS(INACTI)
                ENDIF
                INTBUF_TAB(NIN)%I_STOK(1)=INTER_STRUCT(NIN)%I_SK_OLD
            ELSE
                INTER_STRUCT(NIN)%I_SK_OLD=0
                INTBUF_TAB(NIN)%I_STOK(1)=0
                IF(.NOT.ALLOCATED(INTER_STRUCT(NIN)%CAND_A)) THEN
                    ALLOCATE(INTER_STRUCT(NIN)%CAND_A(0) )
                    INTER_STRUCT(NIN)%SIZE_CAND_A=0
                ENDIF
            ENDIF
        ENDDO

        RETURN
        END SUBROUTINE INTER_TRC_7

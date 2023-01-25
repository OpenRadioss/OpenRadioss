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
Chd|  INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTER_CURV_COMPUTATION        source/interfaces/generic/inter_curv_computation.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
        SUBROUTINE INTER_COUNT_NODE_CURV( NIN,ITASK,IPARI,INTBUF_TAB,
     1                                    X,WEIGHT,INTER_STRUCT)
!$COMMENT
!       INTER_COUNT_NODE_CURV description :
!       compute the number of main node & compute the curv for each interface       
!
!       INTER_CURV_COMPUTATION organization :
!       loop over the main nodes & secondary nodes to save the position if ICONV/=0
!       and count the number of active main nodes
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "impl1_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK,NIN
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB    ! interface data
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X            !   position
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: WEIGHT !   flag for secondary nodes (1= current proc is the master proc)
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: FIRST, LAST
        INTEGER :: I,J
        INTEGER :: NOD,SHIFT
        INTEGER :: NSN,NMN,NRTM,NRTM_T
        INTEGER :: ADRESS,ESHIFT
        INTEGER :: ICURV
        INTEGER :: NMN_L 
        INTEGER :: NOINT
        my_real :: CURV_MAX_LOCAL,CURV_MAX_MAX,TZINF
        my_real :: DX,DY,DZ
        my_real :: MX,MY,MZ
        my_real, DIMENSION(3) :: MAX_LIMIT,MIN_LIMIT
        my_real, DIMENSION(3) :: MAX_LIMIT_MAIN,MIN_LIMIT_MAIN
        my_real, DIMENSION(3) :: SIGMA,SIGMA2
!   ----------------------------------------
        INTER_STRUCT(NIN)%CURV_MAX_MAX = ZERO
        INTER_STRUCT(NIN)%NMN_G = 0

        MAX_LIMIT(1:3) = -EP30
        MIN_LIMIT(1:3) = EP30

        IF(ITASK==0) THEN
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1:3) = -EP30
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4:6) = EP30
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7:12) = ZERO
            NRTM = IPARI(4,NIN)
            IF(.NOT.ALLOCATED(INTER_STRUCT(NIN)%CURV_MAX)) ALLOCATE(INTER_STRUCT(NIN)%CURV_MAX(NRTM))
        ENDIF

        CALL MY_BARRIER()

        !   ------------------
        NSN = IPARI(5,NIN)
        NMN = IPARI(6,NIN)
        NMN_L = 0
        FIRST = 1+ITASK*NMN/NTHREAD
        LAST = (ITASK+1)*NMN/NTHREAD 
        SIGMA = ZERO
        SIGMA2 = ZERO        
        DO I=FIRST,LAST
            NOD = INTBUF_TAB(NIN)%MSR(I)
            IF(NOD>0) THEN
                DO J=1,3
                    MAX_LIMIT(J) = MAX(MAX_LIMIT(J),X(J,NOD))
                    MIN_LIMIT(J) = MIN(MIN_LIMIT(J),X(J,NOD))
                    SIGMA(J) = SIGMA(J) + X(J,NOD)
                    SIGMA2(J) = SIGMA2(J) + X(J,NOD)**2
                ENDDO
                NMN_L = NMN_L + 1
            ENDIF
            IF(INCONV==1) THEN
                IF( NOD>0 ) THEN
                    IF(NSN+NMN < NUMNOD+NUMFAKENODIGEO) THEN 
                        SHIFT = (I+NSN-1)*3
                    ELSE
                        SHIFT = (NOD-1)*3    
                    ENDIF
                    INTBUF_TAB(NIN)%XSAV(1+SHIFT)=X(1,NOD)  
                    INTBUF_TAB(NIN)%XSAV(2+SHIFT)=X(2,NOD)  
                    INTBUF_TAB(NIN)%XSAV(3+SHIFT)=X(3,NOD)                                    
                ENDIF
            ENDIF
        ENDDO
        MAX_LIMIT_MAIN(1:3) = MAX_LIMIT(1:3)
        MIN_LIMIT_MAIN(1:3) = MIN_LIMIT(1:3)

        !   ------------------
        !   MIN/MAX secondary node computation 

        IF(INCONV==1) THEN
            FIRST = 1+ITASK*NSN/NTHREAD
            LAST = (ITASK+1)*NSN/NTHREAD
            DO I=FIRST,LAST
                NOD = INTBUF_TAB(NIN)%NSV(I)
                IF(NSN+NMN < NUMNOD+NUMFAKENODIGEO) THEN 
                    SHIFT = (I-1)*3
                ELSE
                    SHIFT = (NOD-1)*3    
                ENDIF
                IF( NOD>0.AND.NOD<=(NUMNOD+NUMFAKENODIGEO) ) THEN                        
                    INTBUF_TAB(NIN)%XSAV(1+SHIFT)=X(1,NOD)  
                    INTBUF_TAB(NIN)%XSAV(2+SHIFT)=X(2,NOD)  
                    INTBUF_TAB(NIN)%XSAV(3+SHIFT)=X(3,NOD)                                    
                ENDIF
            ENDDO
        ENDIF
        ICURV = IPARI(39,NIN)
        NRTM = IPARI(4,NIN)
        NRTM_T = NRTM/NTHREAD
        ESHIFT = ITASK*NRTM_T
        ADRESS = 1 + ITASK*(IPARI(4,NIN)/NTHREAD)
        IF(ITASK==NTHREAD-1) NRTM_T= NRTM - ADRESS + 1
        CURV_MAX_LOCAL = ZERO
        CALL INTER_CURV_COMPUTATION(ICURV,NRTM_T,
     .            INTBUF_TAB(NIN)%IRECTM(1+4*ESHIFT),CURV_MAX_LOCAL,
     .            INTER_STRUCT(NIN)%CURV_MAX(ADRESS),X)


        !   ------------------
#include "lockon.inc"
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1),MAX_LIMIT_MAIN(1))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2),MAX_LIMIT_MAIN(2))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3) = MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3),MAX_LIMIT_MAIN(3))
        
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4),MIN_LIMIT_MAIN(1))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5),MIN_LIMIT_MAIN(2))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6) = MIN(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6),MIN_LIMIT_MAIN(3))

        INTER_STRUCT(NIN)%CURV_MAX_MAX = MAX(INTER_STRUCT(NIN)%CURV_MAX_MAX,CURV_MAX_LOCAL)

        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7) = INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7)+SIGMA(1)
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8) = INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8)+SIGMA(2)
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9) = INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9)+SIGMA(3)
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10)= INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10)+SIGMA2(1)
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11)= INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11)+SIGMA2(2)
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12)= INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12)+SIGMA2(3)
        INTER_STRUCT(NIN)%NMN_G = INTER_STRUCT(NIN)%NMN_G + NMN_L
#include "lockoff.inc"
        !   ------------------
        CALL MY_BARRIER

!$OMP MASTER
        IF(ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3))>2*EP30.OR.
     +         ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2))>2*EP30.OR.
     +         ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1))>2*EP30)THEN
            NOINT = IPARI(15,NIN)
            CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                    I1=NOINT,C1='(I7BUCE)')
            CALL ARRET(2)
        END IF
C
        TZINF = INTBUF_TAB(NIN)%VARIABLES(8)
        CURV_MAX_MAX = INTER_STRUCT(NIN)%CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1)+TZINF+CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2)+TZINF+CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3)+TZINF+CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4)-TZINF-CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5)-TZINF-CURV_MAX_MAX
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6)-TZINF-CURV_MAX_MAX

C Computation of standard deviation of X master
C use the formula dev = sum(xi²)-n.m²
C mean value m by direction
        MX=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7)/MAX(INTER_STRUCT(NIN)%NMN_G,1)
        MY=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8)/MAX(INTER_STRUCT(NIN)%NMN_G,1)
        MZ=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9)/MAX(INTER_STRUCT(NIN)%NMN_G,1)

C standard deviation by direction

        DX=SQRT(MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10)/MAX(INTER_STRUCT(NIN)%NMN_G,1)-MX**2,ZERO))
        DY=SQRT(MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11)/MAX(INTER_STRUCT(NIN)%NMN_G,1)-MY**2,ZERO))
        DZ=SQRT(MAX(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12)/MAX(INTER_STRUCT(NIN)%NMN_G,1)-MZ**2,ZERO))

C Computation of new boundary of the domain mean values +/- 2 sigma
C => 95% of the population for normal distribution
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7)  = MIN(MX+2*DX,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8)  = MIN(MY+2*DY,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9)  = MIN(MZ+2*DZ,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10) = MAX(MX-2*DX,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11) = MAX(MY-2*DY,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5))
        INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12) = MAX(MZ-2*DZ,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6))
C
        IF(ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7))<EM10)THEN
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(10)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(4)
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(7)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(1)
        END IF
        IF(ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8))<EM10)THEN
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(11)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(5)
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(8)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(2)
        END IF
        IF(ABS(INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12)-INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9))<EM10)THEN
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(12)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(6)
            INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(9)=INTER_STRUCT(NIN)%BOX_LIMIT_MAIN(3)
        END IF
!$OMP END MASTER
        !   ------------------
        CALL MY_BARRIER()

        RETURN
        END SUBROUTINE INTER_COUNT_NODE_CURV

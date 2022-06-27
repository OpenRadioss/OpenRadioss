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
Chd|  SPMD_CELL_SIZE_EXCHANGE                     source/mpi/interfaces/spmd_cell_size_exchange.F
Chd|-- called by -----------
Chd|        SPMD_CELL_LIST_EXCHANGE                    source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_CELL_SIZE_EXCHANGE(IRCVFROM,ISENDTO,WEIGHT,
     .                    IAD_ELEM,FR_ELEM,X,V,MS,TEMP,KINET,NODNX_SMS,ITAB,INTBUF_TAB,IPARI,NIN,REMOTE_PROC_ID,
     .                    ALREADY_SEND,INDEX_ALREADY_SEND,SORT_COMM,NODNX_SMS_SIZ,TEMP_SIZE)
!$COMMENT
!       SPMD_CELL_SIZE_EXCHANGE description :
!       check if the remote processor need some secondary nodes
!       and sent them if necessary
!       remote proc needs seondary nodes if one or several cells were colored by main nodes
!       if yes --> send all the seconcadry nodes of the cell
!                  secondary nodes must be sorted according to theirs global IDs (for parith/on purpose)
!       SPMD_CELL_SIZE_EXCHANGE organization :
!       loop over the cells of the remote proc and :
!           * check if 1 or several secondary nodes are in the currect remote proc & send the number of secondary nodes to the remote proc           
!           * if yes: - save the ID of the secondary nodes
!                     - sort the ID according to the global secondary node ID (parith/on)!                     
!                     - fill the buffer with the data of the secondary nodes (position/velocity...)                  
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE MESSAGE_MOD
        USE INTER_SORTING_MOD
        USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
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
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER,INTENT(in) :: REMOTE_PROC_ID
        INTEGER, INTENT(in) :: NODNX_SMS_SIZ ! size of NODNX_SMS
        INTEGER, INTENT(in) :: TEMP_SIZE     ! size of TEMP
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) ::  IPARI
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: WEIGHT
        INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
        INTEGER, DIMENSION(SFR_ELEM), INTENT(in) :: FR_ELEM
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X,V
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS
        my_real, DIMENSION(TEMP_SIZE), INTENT(in) :: TEMP
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET ! k energy 
        INTEGER, DIMENSION(NODNX_SMS_SIZ), INTENT(in) :: NODNX_SMS ! SMS array
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB
        LOGICAL, DIMENSION(NB_CELL_X,NB_CELL_Y,NB_CELL_Z), INTENT(inout) :: ALREADY_SEND
        INTEGER, DIMENSION(NB_CELL_X*NB_CELL_Y*NB_CELL_Z), INTENT(inout) :: INDEX_ALREADY_SEND
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI        
        INTEGER :: I,J,NOD,L,L2,KK,IJK,KJI
        INTEGER :: P,P_LOC
        INTEGER :: ADRESS,SHIFT_
        INTEGER :: ISIZ,RSIZ,IDEB,JDEB
        INTEGER :: NSN,NMN,IGAP,INTTH,INTFRIC,ITYP,ITIED
        INTEGER :: IFQ,INACTI

        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR

        INTEGER :: LOC_PROC
        INTEGER :: IX,IY,IZ,NB
        INTEGER, DIMENSION(NUMNOD) :: INDEX
        INTEGER :: ISHIFT,RSHIFT

        INTEGER :: MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
        INTEGER :: MSGTYP,INFO

        INTEGER :: ERROR_SORT
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_2,ITRI
        INTEGER, DIMENSION(:), ALLOCATABLE ::  WORK
        INTEGER :: CELL_X_ID,CELL_Y_ID,CELL_Z_ID
        INTEGER :: DISPL
        LOGICAL :: NEED_TO_RCV

        INTEGER :: NB_INDEX_ALREADY_SEND,VALUE,NB_SAVE
!   --------------------------------------------------------------------  
        DATA MSGOFF/6021/
        DATA MSGOFF2/6022/
        DATA MSGOFF3/6023/
        DATA MSGOFF4/6024/ 
        DATA MSGOFF5/6025/ 
        
        LOC_PROC = ISPMD + 1
        RSIZ = SORT_COMM(NIN)%RSIZ
        ISIZ = SORT_COMM(NIN)%ISIZ

        IGAP = IPARI(21,NIN)
        INTTH = IPARI(47,NIN)
        INTFRIC = IPARI(72,NIN)
        ITYP = IPARI(7,NIN)
        ITIED = IPARI(85,NIN)
        NMN = IPARI(6,NIN)
        NSN = IPARI(5,NIN)
        INACTI = IPARI(22,NIN)
        IFQ =IPARI(31,NIN)
        NB_INDEX_ALREADY_SEND= 0
        ! ---------------------------------
        IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.ISENDTO(NIN,LOC_PROC)/=0) THEN
            ! ---------------------------------
            ! only the proc with secondary nodes send theirs data
            IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN   !   local nsn >0

                P=SORT_COMM(NIN)%PROC_LIST(REMOTE_PROC_ID)   !   proc ID
                ! ----------------------------
                ! skip the frontier nodes with weight = 0
                DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
                    NOD = FR_ELEM(J)
                    WEIGHT(NOD) = WEIGHT(NOD)*(-1)
                ENDDO
                ! ----------------------------

                SORT_COMM(NIN)%NB(P) = 0
                NB = 0
                !   --------------------------
                IF(ITIED/=0.AND.ITYP==7) THEN
                    !   itied/=0 --> need to send all nodes 
                    DO I=1,NSN
                        NOD = INTBUF_TAB(NIN)%NSV(I)
                        IF(WEIGHT(NOD).EQ.1)THEN
                            IF(CANDF_SI(NIN)%P(I)/=0.AND.INTBUF_TAB(NIN)%STFNS(I).GT.ZERO)THEN
                                NB = NB + 1
                                INDEX(NB) = I
                            ENDIF
                        ENDIF
                    ENDDO
                ENDIF
                !   --------------------------
                !   loop over the cell of proc REMOTE_PROC_ID
                DISPL = SORT_COMM(NIN)%RCV_DISPLS_CELL(REMOTE_PROC_ID)
                SHIFT_ = SORT_COMM(NIN)%NB_CELL_PROC(REMOTE_PROC_ID)
                IJK = 0
                DO KJI=1,SORT_COMM(NIN)%NB_CELL_PROC(REMOTE_PROC_ID)
                    !   ----------------------
                    !   get the cell ID 
                    IJK = IJK + 1
                    VALUE = SORT_COMM(NIN)%CELL( DISPL + IJK )
                    CELL_Z_ID = ( VALUE - MOD(VALUE,1000000) ) / 1000000
                    VALUE = VALUE - CELL_Z_ID * 1000000
                    CELL_Y_ID = ( VALUE - MOD(VALUE,1000) ) / 1000
                    VALUE = VALUE - CELL_Y_ID * 1000
                    CELL_X_ID = VALUE

                    IF(.NOT.ALREADY_SEND(CELL_X_ID,CELL_Y_ID,CELL_Z_ID)) THEN
                        NB_INDEX_ALREADY_SEND = NB_INDEX_ALREADY_SEND + 1
                        INDEX_ALREADY_SEND(NB_INDEX_ALREADY_SEND) = CELL_X_ID+CELL_Y_ID*1000+CELL_Z_ID*1000000
                        ALREADY_SEND(CELL_X_ID,CELL_Y_ID,CELL_Z_ID) = .TRUE.

                        !   ----------------------
                        !   loop over the secondary nodes of cell ID (CELL_X_ID,CELL_Y_ID,CELL_Z_ID)
                        I = SORT_COMM(NIN)%VOXEL(CELL_X_ID,CELL_Y_ID,CELL_Z_ID)                    
                        DO WHILE(I/=0)                       
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            IF(WEIGHT(NOD).EQ.1)THEN
                                IF(INTBUF_TAB(NIN)%STFNS(I).GT.ZERO)THEN
                                    NB = NB + 1
                                    INDEX(NB) = I
                                ENDIF
                            ENDIF
                            I = SORT_COMM(NIN)%NEXT_NOD(I)
                        ENDDO ! WHILE(I/=0)
                        !   ----------------------
                    ENDIF
                ENDDO
                !   --------------------------
                !   flush ALREADY_SEND to FALSE
                DO I=1,NB_INDEX_ALREADY_SEND      
                    VALUE = INDEX_ALREADY_SEND(I)  
                    CELL_Z_ID = ( VALUE - MOD(VALUE,1000000) ) / 1000000
                    VALUE = VALUE - CELL_Z_ID * 1000000
                    CELL_Y_ID = ( VALUE - MOD(VALUE,1000) ) / 1000
                    VALUE = VALUE - CELL_Y_ID * 1000
                    CELL_X_ID = VALUE
                    ALREADY_SEND(CELL_X_ID,CELL_Y_ID,CELL_Z_ID) = .FALSE.
                ENDDO
                !   --------------------------
                !   need to sort the secondary nodes
                NB_SAVE = NB
                IF(NB_SAVE>1600) THEN
                    ALLOCATE( WORK(70000) )
                    ALLOCATE( ITRI(NB_SAVE) )
                    ALLOCATE( INDEX_2(2*NB_SAVE) )
                    DO I=1,NB_SAVE
                       ITRI(I) = INDEX(I)
                       INDEX_2(I) = I
                    ENDDO
                    CALL MY_ORDERS(0,WORK,ITRI,INDEX_2,NB_SAVE,1)
                    NB = 1
                    INDEX(NB) = ITRI(INDEX_2(1))
                    DO I=2,NB_SAVE
                       IF(ITRI(INDEX_2(I-1))/=ITRI(INDEX_2(I))) THEN
                            NB = NB + 1
                            INDEX(NB) = ITRI(INDEX_2(I))
                        ENDIF
                    ENDDO
                    DEALLOCATE( WORK )
                    DEALLOCATE( ITRI )
                    DEALLOCATE( INDEX_2 )
                ELSEIF(NB_SAVE>0) THEN
                    ALLOCATE( INDEX_2(NB_SAVE) )
                    CALL MYQSORT_INT(NB_SAVE, INDEX, INDEX_2, ERROR_SORT)
                    INDEX_2(1:NB_SAVE) = INDEX(1:NB_SAVE)
                    NB = 1
                    DO I=2,NB_SAVE
                        IF(INDEX(I)/=INDEX(I-1)) THEN
                            NB = NB + 1
                            INDEX_2(NB) = INDEX(I)
                        ENDIF
                    ENDDO
                    INDEX(1:NB) = INDEX_2(1:NB)
                    DEALLOCATE( INDEX_2 )
                ENDIF

                !   save the number of secondary nodes
                SORT_COMM(NIN)%NB(P) = NB
                !   --------------------------

                !   --------------------------
                DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
                    NOD = FR_ELEM(J)
                    WEIGHT(NOD) = WEIGHT(NOD)*(-1)
                ENDDO
                !   --------------------------

                !   --------------------------
                !   send the number of secondary nodes
                MSGTYP = MSGOFF3 
                SORT_COMM(NIN)%NBSEND_NB=SORT_COMM(NIN)%NBSEND_NB+1
                SORT_COMM(NIN)%SEND_NB(SORT_COMM(NIN)%NBSEND_NB)=P   !   proc with nmn>0
                CALL MPI_ISEND(SORT_COMM(NIN)%NB(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                         MPI_COMM_WORLD,SORT_COMM(NIN)%REQUEST_NB_S(SORT_COMM(NIN)%NBSEND_NB),IERROR)

                !   --------------------------
                !   buffer allocation & buffer initialization
                IF (NB.GT.0) THEN
                    ALLOCATE( SORT_COMM(NIN)%DATA_PROC(P)%RBUF(RSIZ*NB),STAT=IERROR)
                    ALLOCATE( SORT_COMM(NIN)%DATA_PROC(P)%IBUF(ISIZ*NB),STAT=IERROR)
                    IF(IERROR.NE.0) THEN
                        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                        CALL ARRET(2)
                    ENDIF

                    L = 0
                    L2= 0            
              
#include      "vectorize.inc"
                    DO J = 1, NB
                        I = INDEX(J)
                        NOD = INTBUF_TAB(NIN)%NSV(I)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+1) = X(1,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+2) = X(2,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+3) = X(3,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+4) = V(1,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+5) = V(2,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+6) = V(3,NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+7) = MS(NOD)
                        SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+8) = INTBUF_TAB(NIN)%STFNS(I)          
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+1) = I
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+2) = ITAB(NOD)        
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+3) = KINET(NOD)
!     save specifics IREM and XREM indexes for INT24 sorting
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+4) = 0 !IGAPXREMP
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+5) = 0 !I24XREMP        
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+6) = 0 !I24IREMP
                        L = L + RSIZ
                        L2 = L2 + ISIZ
                    END DO

c shift for real variables (prepare for next setting)            
                    RSHIFT = 9
c shift for integer variables (prepare for next setting) 
                    ISHIFT = 7 

c specific cases
c IGAP=1 or IGAP=2                 
                    IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN
                        L = 0            
                        IGAPXREMP = RSHIFT        
#include      "vectorize.inc"               
                        DO J = 1, NB
                            I = INDEX(J)   
                            SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+RSHIFT)= INTBUF_TAB(NIN)%GAP_S(I)
                            L = L + RSIZ           
                        ENDDO
                        RSHIFT = RSHIFT + 1                      
c IGAP=3                       
                    ELSEIF(IGAP.EQ.3)THEN 
                        L = 0         
                        IGAPXREMP = RSHIFT        
#include      "vectorize.inc"                 
                        DO J = 1, NB
                            I = INDEX(J)
                            SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+RSHIFT)  = INTBUF_TAB(NIN)%GAP_S(I)
                            SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+RSHIFT+1)= INTBUF_TAB(NIN)%GAP_SL(I)
                            L = L + RSIZ
                        END DO
                        RSHIFT = RSHIFT + 2
                    ENDIF
                         
C thermic
                    IF(INTTH.GT.0)THEN
                        L = 0
                        L2 = 0                     
#include      "vectorize.inc"                                          
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+RSHIFT)   = TEMP(NOD)
                            SORT_COMM(NIN)%DATA_PROC(P)%RBUF(L+RSHIFT+1) = INTBUF_TAB(NIN)%AREAS(I)
                            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+ISHIFT) = INTBUF_TAB(NIN)%IELEC(I)
                            L = L + RSIZ
                            L2 = L2 + ISIZ
                        END DO
                        RSHIFT = RSHIFT + 2
                        ISHIFT = ISHIFT + 1               
                    ENDIF
C Friction
                    IF(INTFRIC.GT.0)THEN
                        L2 = 0                     
#include      "vectorize.inc"                                          
                        DO J = 1, NB
                            I = INDEX(J)
                            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+ISHIFT) = INTBUF_TAB(NIN)%IPARTFRICS(I)
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 1               
                    ENDIF              
C -- IDTMINS==2
                    IF(IDTMINS==2)THEN
                        L2 = 0
#include      "vectorize.inc"                              
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+ISHIFT)  = NODNX_SMS(NOD)
                            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+ISHIFT+1)= NOD
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 2              
C -- IDTMINS_INT /= 0               
                    ELSEIF(IDTMINS_INT.NE.0)THEN
                        L2 = 0                 
#include      "vectorize.inc"              
                        DO J = 1, NB
                            I = INDEX(J)
                            NOD = INTBUF_TAB(NIN)%NSV(I)
                            SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+ISHIFT)= NOD
                            L2 = L2 + ISIZ
                        END DO
                        ISHIFT = ISHIFT + 1               
                    ENDIF                           
            !save specifics IREM and XREM indexes for INT24 sorting
                    L2 = 0
#include      "vectorize.inc"
                    DO J = 1, NB
                        I = INDEX(J)
                        NOD = INTBUF_TAB(NIN)%NSV(I)
                        !save specifics IREM and XREM indexes for INT24 sorting
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+4) = IGAPXREMP
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+5) = I24XREMP
                        SORT_COMM(NIN)%DATA_PROC(P)%IBUF(L2+6) = I24IREMP
                        L2 = L2 + ISIZ
                    END DO  
                ENDIF   !   if nb/=0
                !   ------------------------------
            ENDIF      
        ENDIF ! nsn>0 or nmn > 0
        ! ---------------------------------
#endif
        RETURN
        END SUBROUTINE SPMD_CELL_SIZE_EXCHANGE

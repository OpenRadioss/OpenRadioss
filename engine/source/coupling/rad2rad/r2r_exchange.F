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
Chd|  R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CHECK_DTNODA_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_IBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_STIFF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_STIFF_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_BLOCK_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_SEM_C                     source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_UNLOCK_THREADS_C          source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_DATA_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_DATA_NL_C                source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|        SEND_IBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_EXCH_R2R                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE R2R_EXCHANGE(
     1    IEXLNK   ,IGRNOD   ,DX       ,V        ,VR        ,      
     2    A        ,AR       ,MS       ,IN       ,STX       ,STR       ,                       
     3    R2R_ON   ,DD_R2R   ,WEIGHT   ,IAD_ELEM ,FR_ELEM   ,RBY       ,
     4    XDP      ,X        ,DD_R2R_ELEM , SDD_R2R_ELEM, OFF_SPH_R2R  ,
     5    NUMSPH_GLO_R2R,NLOC_DMG)                         
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD
      USE GROUPDEF_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXLNK(NR2R,NR2RLNK),
     .        WEIGHT(*), DD_R2R(NSPMD+1,*), IAD_ELEM(2,*), FR_ELEM(*),
     .        DD_R2R_ELEM(*),SDD_R2R_ELEM,OFF_SPH_R2R(*),NUMSPH_GLO_R2R     
      INTEGER R2R_ON,NGLOB,NB
C     REAL
      my_real 
     .   V(3,*), VR(3,*),  A(3,*)  , AR(3,*), DX(3,*),     
     .   MS(*) , IN(*)  ,  STX(*)  , STR(*), RBY(*), X(3,*)
      DOUBLE PRECISION XDP(3,*)               
!
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(NLOCAL_STR_), TARGET, INTENT(IN)  :: NLOC_DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IEX,IDP,IDG,NNG,OLD_ACTIV,BID,LENR,SIZE,NTOP
C
      INTEGER, DIMENSION(:), POINTER :: GRNOD
      my_real, POINTER, DIMENSION(:) :: MSNL,VNL,FNL
C=======================================================================

C------------------------------------------------------
C------------ Stock force and stifness for exch -------
C------------------------------------------------------

      IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN 
        DO IEX = 1, NR2RLNK
          IDG  = IEXLNK(1,IEX)                                  
          IDP  = IEXLNK(2,IEX)                                  
          NNG  = IGRNOD(IDG)%NENTITY
          GRNOD => IGRNOD(IDG)%ENTITY
	  IF (NLLNK(IEX)==1) THEN
C----------Non local coupling interface--------------------------
            MSNL  => NLOC_DMG%MASS(1:NLOC_DMG%L_NLOC)
            VNL  => NLOC_DMG%VNL(1:NLOC_DMG%L_NLOC) 
            FNL  => NLOC_DMG%FNL(1:NLOC_DMG%L_NLOC,1)  
            CALL SEND_DATA_NL_C(IDP,NBDOF_NL(IEX),IADD_NL,FNL,VNL ,     
     .                          MSNL,NCYCLE ,IEX)
	  ELSEIF (IRESP==1) THEN
C----------simple precision : DX is replaced by XDP--------------	                                    
            CALL SEND_DATA_C(
     .        IDP    ,NNG    ,GRNOD  ,A      ,AR     ,     
     .        STX    ,STR    ,V      ,VR     ,MS     ,IN     ,     
     .        XDP    ,X      ,TYPLNK(IEX),NCYCLE,RBY,TAG_RBY,
     .        ADD_RBY(IEX) ,RBYLNK(IEX),KINLNK(IEX),R2R_KINE,DT1,IEX,
     .        OFF_SPH_R2R  ,NUMSPH_GLO_R2R, NRBY)
          ELSE
C---------double precision---------------------------------------  
            CALL SEND_DATA_C(
     .        IDP    ,NNG    ,GRNOD  ,A      ,AR     ,     
     .        STX    ,STR    ,V      ,VR     ,MS     ,IN     ,     
     .        DX     ,X      ,TYPLNK(IEX),NCYCLE,RBY,TAG_RBY,
     .        ADD_RBY(IEX) ,RBYLNK(IEX),KINLNK(IEX),R2R_KINE,DT1,IEX,
     .        OFF_SPH_R2R  ,NUMSPH_GLO_R2R, NRBY)	  
	  ENDIF                                     
        END DO
      ELSE
        DO IEX = 1, NR2RLNK
          IDG  = IEXLNK(1,IEX)                                  
          IDP  = IEXLNK(2,IEX)                                  
          NNG  = IGRNOD(IDG)%NENTITY                                    
          GRNOD => IGRNOD(IDG)%ENTITY
	  IF (IRESP==1) THEN
C----------simple precision : DX is replaced by XDP--------------	  	                                      
             CALL SEND_DATA_SPMD(
     .           IDP,NNG,GRNOD        ,A                    ,AR     ,     
     .           STX,STR,V            ,VR                   ,MS     ,     
     .           IN ,XDP ,DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,
     .           TYPLNK(IEX),ROTLNK(IEX),RBYLNK(IEX),RBY,IEX)
	  ELSE     
C----------double precision -------------------------------------  	                                      
             CALL SEND_DATA_SPMD(
     .           IDP,NNG,GRNOD        ,A                    ,AR     ,     
     .           STX,STR,V            ,VR                   ,MS     ,     
     .           IN ,DX ,DD_R2R(1,IEX),DD_R2R(NSPMD+1,IEX),WEIGHT,
     .           TYPLNK(IEX),ROTLNK(IEX),RBYLNK(IEX),RBY,IEX)
	  ENDIF
        END DO                  
      ENDIF
      
C------------------------------------------------------     
      IF ((R2R_SIU==1).OR.(ISPMD==0)) CALL R2R_SEM_C()

C------------------------------------------------------
!$OMP PARALLEL
!$OMP MASTER
C------------------------------------------------------
C-------------- Exch flag activation--- ---------------
C------------------------------------------------------

      IF(NSPMD==1.OR.ISPMD==0)THEN
        CALL CHECK_DTNODA_C(I7KGLO)
        OLD_ACTIV = R2R_ACTIV
        R2R_ON    = 0  
C----- Check activation flag of current subdomain
        CALL GET_IBUF_C(R2R_ACTIV,1)

        IF (OLD_ACTIV == 1 .AND. R2R_ACTIV == 0) THEN
          WRITE(IOUT,*)' PROCESS DESACTIVATION'
        ENDIF 
C------------------------------------------------------
        IF (R2R_ACTIV /= -1) THEN                            
         DO WHILE (R2R_ACTIV == 0)       
          CALL GET_IBUF_C(R2R_ACTIV,1)  
         ENDDO
         IF (OLD_ACTIV == 0 .AND. R2R_ACTIV == 1) THEN
          WRITE(IOUT,*)' PROCESS ACTIVATION'
         ENDIF         
        END IF
C------------------------------------------------------
        IF (R2R_ACTIV == 1) THEN
           CALL GET_IBUF_C(R2R_ON,1)
C---------infos for th---------------------------------        
           IF (R2R_SIU==1) THEN            
             CALL SEND_IBUF_C(R2R_TH_FLAG,10)
             IF (IDDOM==0) CALL SEND_IBUF_C(R2R_TH_MAIN,10)         
           ENDIF      
        ENDIF              
      END IF
C------------------------------------------------------      
      IF(NSPMD>1)THEN
        CALL SPMD_IBCAST(R2R_ACTIV,R2R_ACTIV,1,1,0,2)
        CALL SPMD_IBCAST(I7KGLO,I7KGLO,1,1,0,2)
        IF (R2R_ACTIV == 1) CALL SPMD_IBCAST(R2R_ON,R2R_ON,1,1,0,2)	
      END IF
      IF (R2R_ACTIV == -1) THEN        
        TT = TSTOP
C        RETURN                    
      ENDIF   
      
C------------------------------------------------------
C-------------- Rad2rad activation----- ---------------
C------------------------------------------------------
      IF ((R2R_SIU==1).OR.(ISPMD==0)) THEN
        CALL R2R_SEM_C()       
        CALL GET_IBUF_C(BID,1)
        IF (R2R_SIU==1) NTOP = NTHREAD
        IF (R2R_SIU==0) NTOP = NTHREAD*NSPMD
        CALL R2R_UNLOCK_THREADS_C(NTOP)
      ENDIF  
C------------------------------------------------------
C------------------------------------------------------        
!$OMP END MASTER
      CALL R2R_BLOCK_C()              
!$OMP END PARALLEL        

C------------------------------------------------------
C------------ Get stifness from shared memory ---------
C------------------------------------------------------
        
      IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN         
        DO IEX = 1, NR2RLNK
          IDG  = IEXLNK(1,IEX)
          IDP  = IEXLNK(2,IEX)
          NNG  = IGRNOD(IDG)%NENTITY
          GRNOD => IGRNOD(IDG)%ENTITY
!
          CALL GET_STIFF_C(
     .       IDP    ,NNG    ,GRNOD,MS     ,IN     ,
     .       STX    ,STR, TYPLNK(IEX),NCYCLE,IEX)
        END DO

C-------New rad2rad HMPP - synchro SPMD- (not needed for NL coupling) -----       
        IF ((SDD_R2R_ELEM>0).AND.(NSPMD>1)) THEN 
         SIZE =  1 + IRODDL*1         
         LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
         IF (NCYCLE==0) THEN
           CALL SPMD_EXCH_R2R(
     1     A       ,AR     ,MS,IN ,MS   ,
     2     IAD_ELEM,FR_ELEM,SIZE ,
     3     LENR    ,DD_R2R,DD_R2R_ELEM,1)
         ENDIF                
         CALL SPMD_EXCH_R2R(
     1     A       ,AR     ,STX,STR ,MS   ,
     2     IAD_ELEM,FR_ELEM,SIZE ,
     3     LENR    ,DD_R2R,DD_R2R_ELEM,1) 
        ENDIF
                     
      ELSE      
C--------------------------------------------------------------------      
        DO IEX = 1, NR2RLNK
          IDG  = IEXLNK(1,IEX)
          IDP  = IEXLNK(2,IEX)
          NNG  = IGRNOD(IDG)%NENTITY
          GRNOD => IGRNOD(IDG)%ENTITY
          NB = DD_R2R(NSPMD+1,IEX)
	  IF (ISPMD==0) THEN
	    NGLOB=DD_R2R(NSPMD+1,IEX)+DBNO(IEX)	    	    
	  ELSE
	    NGLOB=NNG	    
	  ENDIF
          CALL GET_STIFF_SPMD(
     .      IDP     ,NNG    ,GRNOD        ,MS   ,IN    ,
     .      STX     ,STR    ,DD_R2R(1,IEX),NGLOB,WEIGHT,
     3      IAD_ELEM,FR_ELEM,NB,IEX,TYPLNK(IEX),ROTLNK(IEX))

        END DO     
      ENDIF     

C-----------------------------------------------------------------
      RETURN
      END SUBROUTINE R2R_EXCHANGE
C
Chd|====================================================================
Chd|  SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- called by -----------
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|        SEND_DATA_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RBY                  source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RGET                 source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RGET3                source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_R2R_RGET3_DP             source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE SEND_DATA_SPMD(
     1   IDP  ,NNG   ,GRNOD  ,A    ,AR    ,
     2   STX  ,STR   ,V      ,VR   ,MS    ,
     3   IN   ,DX    ,DD_R2R ,NGLOB,WEIGHT,
     4   TYP, FLAG_ROT, FLAG_RBY, RBY, IEX)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDP, NNG, NGLOB,i,TYP,
     .        GRNOD(*),FLAG_ROT, FLAG_RBY,
     .        WEIGHT(*), DD_R2R(*), IEX
C     REAL
      my_real
     .        A(3,*), AR(3,*), STX(*), STR(*), V(3,*), VR(3,*),
     .        MS(*), IN(*), RBY(*)
      DOUBLE PRECISION DX(3,*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,J
      my_real
     .        BUFR1(3,NGLOB), BUFR2(3,NGLOB), BUFR3(NGLOB),
     .        BUFR4(NGLOB), BUFR5(3,NGLOB), BUFR6(3,NGLOB),
     .        BUFR8(NGLOB), BUFR9(NGLOB),BUF_RBY(9,NGLOB)
      DOUBLE PRECISION BUFR7(3,NGLOB)       
C
C******************************************************************************C

      IF (TYP/=7) THEN      
      CALL SPMD_R2R_RGET3(A,NNG,GRNOD,DD_R2R,WEIGHT,BUFR1)
      CALL SPMD_R2R_RGET3(AR,NNG,GRNOD,DD_R2R,WEIGHT,BUFR2)
      IF (TYP==5) THEN
      CALL SPMD_R2R_RGET3_DP(DX,NNG,GRNOD,DD_R2R,WEIGHT,BUFR7)            
      CALL SPMD_R2R_RGET(STX,NNG,GRNOD,DD_R2R,WEIGHT,BUFR3)
      IF(FLAG_ROT /= 0)
     .  CALL SPMD_R2R_RGET(STR,NNG,GRNOD,DD_R2R,WEIGHT,BUFR4)
      ENDIF
      IF ((TYP<4).OR.(NCYCLE==0)) THEN
      CALL SPMD_R2R_RGET3(V,NNG,GRNOD,DD_R2R,WEIGHT,BUFR5)
      IF(FLAG_ROT /= 0)
     .  CALL SPMD_R2R_RGET3(VR,NNG,GRNOD,DD_R2R,WEIGHT,BUFR6)
      ENDIF
      CALL SPMD_R2R_RGET(MS,NNG,GRNOD,DD_R2R,WEIGHT,BUFR8)
      IF(FLAG_ROT /= 0)
     .  CALL SPMD_R2R_RGET(IN,NNG,GRNOD,DD_R2R,WEIGHT,BUFR9)
        IF (FLAG_RBY==1)
     .    CALL SPMD_R2R_RBY(RBY,NNG,GRNOD,DD_R2R,WEIGHT,IEX,BUF_RBY)                	
      ENDIF

      IF(ISPMD==0) THEN      
        CALL SEND_DATA_SPMD_C(IDP  ,NGLOB,BUFR1,BUFR2,BUFR3,
     2                      BUFR4,BUFR5,BUFR6,BUFR7,BUFR8,
     3                      BUFR9,BUF_RBY,FLAG_RBY,TYP,NCYCLE,IEX)
      ENDIF
C-----------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  GET_STIFF_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- called by -----------
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|        GET_STIFF_SPMD_C              source/coupling/rad2rad/rad2rad_c.c
Chd|        SPMD_R2R_RSET                 source/mpi/r2r/spmd_r2r.F     
Chd|====================================================================
      SUBROUTINE GET_STIFF_SPMD(
     1   IDP     ,NNG    ,GRNOD  ,MS   ,IN     ,
     2   STX     ,STR    ,DD_R2R ,NGLOB,WEIGHT ,
     3   IAD_ELEM,FR_ELEM,NB,IEX,TYP,FLAG_ROT)
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDP, NNG, NGLOB, GRNOD(*),IEX,TYP,
     .        WEIGHT(*), DD_R2R(*), IAD_ELEM(2,*), FR_ELEM(*),NB,
     .        FLAG_ROT     
      my_real MS(*), IN(*), STX(*), STR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LRBUF,i
      my_real
     .        BUFR1(NGLOB), BUFR2(NGLOB), BUFR3(NGLOB), BUFR4(NGLOB)
C
C******************************************************************************C
      IF(ISPMD == 0) THEN
      CALL GET_STIFF_SPMD_C(IDP,NB,BUFR1,BUFR2,BUFR3,BUFR4,
     .                      TYP,NCYCLE,IEX,NGLOB)	               		
      ENDIF

      IF (TYP==5) THEN     
      LRBUF = 2*2*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))+2*NSPMD	

      IF (NCYCLE==0) THEN
      CALL SPMD_R2R_RSET(MS   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                   BUFR1,IAD_ELEM,FR_ELEM,LRBUF,IEX)
      ENDIF              
      CALL SPMD_R2R_RSET(STX   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                   BUFR2,IAD_ELEM,FR_ELEM,LRBUF,IEX )
      IF(FLAG_ROT /= 0)THEN
        IF (NCYCLE==0) THEN      
          CALL SPMD_R2R_RSET(IN   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                   BUFR3,IAD_ELEM,FR_ELEM,LRBUF,IEX  )
        ENDIF     
        CALL SPMD_R2R_RSET(STR   ,NNG     ,GRNOD,DD_R2R,WEIGHT,
     .                   BUFR4,IAD_ELEM,FR_ELEM,LRBUF,IEX  )    
      END IF
      END IF

C-----------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  R2R_TAGEL                     source/coupling/rad2rad/r2r_exchange.F
Chd|-- called by -----------
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE R2R_TAGEL(NTAGEL_R2R_SEND,ID_EL,ID_NODE,ITY,OFUR,TAGEL_SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD   
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTAGEL_R2R_SEND,ID_EL,ID_NODE,ITY,OFUR,TAGEL_SIZE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,TAGEL_SIZE_OLD
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGEL_TEMP
C=======================================================================

      N = 3*NTAGEL_R2R_SEND
      NTAGEL_R2R_SEND = NTAGEL_R2R_SEND + 1

C----------Reallocation of tagel----------------------------------
      IF (3*NTAGEL_R2R_SEND>TAGEL_SIZE) THEN
C
        IF (TAGEL_SIZE>0) THEN
          ALLOCATE(TAGEL_TEMP(TAGEL_SIZE))
          DO I=1,TAGEL_SIZE
            TAGEL_TEMP(I) = TAGEL_R2R_SEND(I)
          END DO
        ENDIF 
C
        TAGEL_SIZE_OLD = TAGEL_SIZE
        TAGEL_SIZE = TAGEL_SIZE + 150
        IF (ALLOCATED(TAGEL_R2R_SEND)) DEALLOCATE(TAGEL_R2R_SEND)
        ALLOCATE(TAGEL_R2R_SEND(TAGEL_SIZE))
        DO I=1,TAGEL_SIZE_OLD
          TAGEL_R2R_SEND(I) = TAGEL_TEMP(I)
        END DO
C
      ENDIF
C------------------------------------------------------------------

      TAGEL_R2R_SEND(N+1) = ID_EL
      TAGEL_R2R_SEND(N+2) = ID_NODE
      TAGEL_R2R_SEND(N+3) = ITY

C-----------------------------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  R2R_EXCH_ITAG                 source/coupling/rad2rad/r2r_exchange.F
Chd|-- called by -----------
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|        EXCH_ITAG_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE R2R_EXCH_ITAG(IEXLNK,IGRNOD,ITAG, FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXLNK(NR2R,NR2RLNK),ITAG(*),FLAG
!
      TYPE (GROUP_)  , TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEX, IDP, IDG, NNG,NMOD_R2R, OFF
C
      INTEGER, DIMENSION(:), POINTER :: GRNOD
C=======================================================================

       OFF = 0

       IF (R2R_SIU==1) THEN      
C----------Echange of itag ( 0 reception / 1 emission) --------------
         DO IEX = 1, NR2RLNK
           IDP  = IEXLNK(2,IEX)
           IDG  = IEXLNK(1,IEX)
           NNG  = IGRNOD(IDG)%NENTITY
           GRNOD => IGRNOD(IDG)%ENTITY
           CALL EXCH_ITAG_C(IDP,NNG,GRNOD,ITAG,ITAG(NUMNOD+1),IEX,OFF,FLAG)
           OFF = OFF + NNG
         END DO
       ENDIF
C-----------------------------------------------------------------
      RETURN
      END

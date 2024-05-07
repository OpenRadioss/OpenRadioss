Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I22IDENT                      source/interfaces/int22/i22ident.F
Chd|-- called by -----------
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|-- calls ---------------
Chd|        I22GBIT                       source/interfaces/int22/i22ident.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22IDENT(
     1       IXS, X, ITASK, NIN, BUFBRIC)
C============================================================================      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C Determine intersection type
C---------local numbering of edges------------------
C                                                                                       
C                            +------------+------------+                                         
C         8+--------+7       | iEDGE(1,*) | iEDGE(2,*) |        +---12---+            +--------+ 
C         /|       /|        +------------+------------+       /|        /|          /|  3    /|   
C        / |      / |      1 +     1      +     2      +     11 10     9 6          / |      / |   
C      5+--------+6 |      2 +     1      +     4      +     +----8---+  |       6 +--------+  | 5
C       | 4|-----|--+3     3 +     1      +     5      +     |  |---5-|--+         |  |-----|--+ 
C       | /      | /       4 +     3      +     2      +     3 2      7 4          | /  4   | /       
C       |/       |/        5 +     3      +     4      +     |/       |/           |/       |/        
C       +--------+         6 +     3      +     7      +     +----1---+            +--------+         
C      1         2         7 +     6      +     2      +                                              
C                          8 +     6      +     5      +                               1         
C                          9 +     6      +     7      +                                             
C     LIST OF NODES       10 +     8      +     4      +   LIST OF EDGES       LIST OF FACES          
C     DEFINED WITH        11 +     8      +     5      +   DEFINED WITH        DEFINED WITH         
C     LOCAL IDs           12 +     8      +     7      +   LOCAL IDs               LOCAL IDs         
C                            +------------+------------+

C---------identification of cut cell---------
C
C           +---------------+ 
C          /|              /|
C         / |             / |   
C        /  |            /  |   
C       +---------------+   |   
C       |   +-----------|---+  
C       |  /            |  /    
C       T T             | /       -> edge 1,2,3 <=> 111000000000(3584) <=> TETRA summit 1
C       |/              |/     
C       +---T-----------+      
C      (1)                     
C
C   +====+===+===+===+===+===+===+===+===+===+===+===++
C   || 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12||   -> id EDGE : 1 a 12
C   ++---+---+---+---+---+---+---+---+---+---+---+---++
C   || T | T | T |   |   |   |   |   |   |   |   |   ||   -> I12bits : 0 ou 1  (tag intersection)
C   ++===+===+===+===+===+===+===+===+===+===+===+===++


C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE I22EDGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "subvolumes.inc"
C-----------------------------------------------s
      INTERFACE
        FUNCTION I22CHK(
     1                  SECtype, Nbits, Npqts)
          INTEGER :: Nbits, Npqts
          CHARACTER*(*) :: SECtype
          LOGICAL :: I22CHK
        END FUNCTION I22CHK
      END INTERFACE  
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), ITASK, NIN, BUFBRIC(*)
      my_real, intent(in) ::
     .                       X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JJ, K, L,S, NE, POS, IAD,NBCUT, Icode, Idble, IB
      INTEGER I_12BITS, PQTS(4), NPQTS, NBITS, SOM, I_bits(12)
      INTEGER NBF, NBL, ID, N, id1, id2
      INTEGER NFACE, NEDGE
      INTEGER :: MAXSOM
      INTEGER D, M
      INTEGER,POINTER,DIMENSION(:) :: pCODE, pTAG, pGnod
      CHARACTER*14,DIMENSION(:),POINTER ::pSEC
      LOGICAL LTag(18)
      CHARACTER*14    :: dbKEY1,   dbKEY2
      integer idb1(0:ncandb), idb2(0:ncandb)
      INTEGER :: tagTETRA(S_TETRA),tagPENTA(S_PENTA),tagPOLY3(S_POLY3),
     .           tagHEXAE(S_HEXAE),tagPOLY4(S_POLY4)                     !tag if there is a potential intersection
      INTEGER      :: MultICODE(S22_MAX), MultIDBLE(S22_MAX)
      CHARACTER*14 :: MultiSECtype(S22_MAX)
      INTEGER :: MultiSECid(S22_MAX)
      LOGICAL :: bool1, bool2
      INTEGER :: BasedOnUsedNodes
      INTEGER :: UsedNodes, Gnod
      INTEGER :: SecTypeList(0:106)
      INTEGER :: LIST(106), LIST_FIX(8),LIST_VAR(106)
      INTEGER :: SizeL   , SizeLFIX   ,SizeLVAR
      INTEGER :: NINTP , TAB(12)  
      INTEGER :: RESULT(8)   
      LOGICAL :: bFOUND, debug_outp
      INTEGER :: CODE, brickID, bAND, IE, Iremoved
      my_real :: POINT(3),CUTCOOR
      LOGICAL :: db_WRITE
      
C----------------------------------------------------------------

      LIST_FIX(1:8) = 0
      tagTETRA=0
      tagPENTA=0
      tagPOLY3=0
      tagHEXAE=0
      tagPOLY4=0 

C=======================================================================
C 1   INITIALISATION CODE BINAIRE 12bits
C=======================================================================
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
 
      DO I=NBF,NBL
       !KEEP TRIPLE POINTS, INTERPRETATION OF PARTITIONING IS MADE TAKING MOST COMPLEX POLY EVEN IF THERE ARE EXPECTED REMAINING INTERSECTION POINTS
    !        CALL REMOVE_DOUBLE_INTP(
    ! 1                              IXS, X, ITASK, NIN, BUFBRIC,
    ! 2                              I )      
        CALL I22GBIT( 
     1               I      , Icode  , Idble, Nbits, Npqts, 
     2               idb1(i), idb2(i), NIN )
        BRICK_LIST(NIN,I)%ICODE = Icode
        BRICK_LIST(NIN,I)%IDBLE = Idble
        BRICK_LIST(NIN,I)%NBITS = Nbits
        BRICK_LIST(NIN,I)%NPQTS = Npqts
        BRICK_LIST(NIN,I)%NBCUT = 0
      END DO

C=======================================================================
C 2 Debug
C=======================================================================
      !---------------------------------------------------------!
      ! DEBUG OUTPUT                                            !
      !---------------------------------------------------------!
      !INTERFACE 22 ONLY - OUTPUT---------------!
       debug_outp = .false.      
       if(ibug22_ident/=0)then
         if(ibug22_ident>0)then
           do ib=nbf,nbl
             ie=brick_list(nin,ib)%id
             if(ixs(11,ie)==ibug22_ident)then
               debug_outp=.true.
               exit
             endif
           enddo
         elseif(ibug22_ident==-1)then
           debug_outp = .true.
         endif
       endif
      if(itask==0.AND.debug_outp)then
        print *, ""
        print *, "  |----------i22ident.F-----------|"
        print *, "  |  IDENTIFICATION INTERSECTION  |"
        print *, "  |-------------------------------|"
      end if


      DO I=NBF,NBL
        !===================================================================
        ! 3 Potential Polyhedron Detection : stored in SecTypeList in [1,106]
        !===================================================================
        Iremoved = 0
   10   CONTINUE
        IB = I        
        SecTypeList(:) = 0
        UsedNodes = 0
        K = 1 !numero plan intersection pour %SECtype
        ICODE=BRICK_LIST(NIN,I)%ICODE
        IDBLE=BRICK_LIST(NIN,I)%IDBLE
        NBITS=BRICK_LIST(NIN,I)%Nbits
        NPQTS=BRICK_LIST(NIN,I)%Npqts
        BRICK_LIST(NIN,I)%Sectype(1:8) = '--------------'


        !-----------------------------------------------------------------------!
        !                   Listing all potential polyhedron                    !
        !-----------------------------------------------------------------------! 
               
        IF(NBITS<3)GOTO 50 !sous-variete de dim 1
        !------------------------!
        !        TETRA           !
        !------------------------!
        D = D_TETRA
        M = M_TETRA
        S = S_TETRA
        N = N_TETRA
        pCODE => bCODE(D:D+S-1)        !bincode
        pSEC  => StrCODE(D:D+S-1)      !sectype     
        DO J=1,S
          IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN
            SecTypeList(K) = D+J-1
            K = K+1             
          END IF
        END DO
        IF(NBITS==3.AND.(NPQTS==1.OR.NPQTS==3))GOTO 50 !pas d'autre intersection
        !------------------------!
        !        PENTA           !
        !------------------------!
        IF(NBITS>=4.AND.NPQTS>=3)THEN
          D = D_PENTA
          M = M_PENTA
          S = S_PENTA
          N = N_PENTA
          pCODE => bCODE(D:D+S-1)      !bincode
          pSEC  => StrCODE(D:D+S-1)    !sectype       
          DO J=1,S
            IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN  
              SecTypeList(K) = D+J-1   !code_id in [1,106]                     
              K = K+1            
            END IF           
          END DO
          !------------------------!                                
          !        POLY3           !                                
          !------------------------!                                
          IF(NBITS>=5)THEN   !NPQTS>=3 deja verifie                 
            D = D_POLY3                                             
            S = S_POLY3                                             
            M = M_POLY3                                             
            N = N_POLY3                                             
            pCODE => bCODE(D:D+S-1)    !bincode                     
            pSEC  => StrCODE(D:D+S-1)  !sectype                     
            DO J=1,S                                                
              IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN              
                SecTypeList(K) = D+J-1   !code_id in [1,106]         
                K = K+1                                             
              END IF                                                
            END DO                                                  
          END IF                                                    
          !------------------------!                                
          !        HEXAE           !                                
          !------------------------!                                
          IF(NPQTS==4)THEN   !NBIT>=4 deja verifie                  
            D = D_HEXAE                                             
            M = M_HEXAE                                             
            S = S_HEXAE * M                                         
            N = N_HEXAE                                             
            pCODE => bCODE(D:D+S-1)      !bincode                   
            pSEC  => StrCODE(D:D+S-1)    !sectype                   
            DO J=1,S                                                
              IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN              
                SecTypeList(K) = D+J-1   !code_id in [1,106]         
                K = K+1                                             
              END IF                                                
            END DO                                                  
          END IF                                                    
          !------------------------!                                
          !        POLY4           !                                
          !------------------------!                                
          IF(NBITS>=6)THEN   !NPQTS>=3 deja verifie                 
            D = D_POLY4                                             
            M = M_POLY4                                             
            S = S_POLY4 * M                                         
            N = N_POLY4                                             
            pCODE => bCODE(D:D+S-1)                                 
            pSEC  => StrCODE(D:D+S-1)                               
            DO J=1,S                                                
              IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN              
                SecTypeList(K) = D+J-1   !code_id in [1,106]         
                K = K+1                                             
              END IF                                                
            END DO                                                  
          END IF !(NBITS>=6)
          !------------------------!                                
          !        POLY4A          !                                
          !------------------------!                                
          IF(NBITS>=6)THEN   !NPQTS>=3 deja verifie                 
            D = D_POLY4A                                             
            M = M_POLY4A                                             
            S = S_POLY4A * M                                         
            N = N_POLY4A                                             
            pCODE => bCODE(D:D+S-1)                                 
            pSEC  => StrCODE(D:D+S-1)                               
            DO J=1,S                                                
              IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN              
                SecTypeList(K) = D+J-1   !code_id in [1,106]         
                K = K+1                                             
              END IF                                                
            END DO                                                  
          END IF !(NBITS>=6)    
          !------------------------!                                
          !        POLY4B          !                                
          !------------------------!                                
          IF(NBITS>=6)THEN   !NPQTS>=3 deja verifie                 
            D = D_POLY4B                                             
            M = M_POLY4B                                             
            S = S_POLY4B * M                                         
            N = N_POLY4B                                             
            pCODE => bCODE(D:D+S-1)                                 
            pSEC  => StrCODE(D:D+S-1)                               
            DO J=1,S                                                
              IF( IAND(ICODE,pCODE(J))==pCODE(J) )THEN              
                SecTypeList(K) = D+J-1   !code_id in [1,106]         
                K = K+1                                             
              END IF                                                
            END DO                                                  
          END IF !(NBITS>=6)                 
          !------------------------!                                
          !        POLYC           !                                
          !------------------------!                                
      !    IF(NBITS>=5 .AND. IDBLE>0)THEN   !NPQTS>=3 deja verifie                 
      !      D = D_POLYC                                             
      !      M = M_POLYC                                             
      !      S = S_POLYC * M                                         
      !      N = N_POLYC                                             
      !      pCODE => bCODE(D:D+S-1)                                 
      !      pSEC  => StrCODE(D:D+S-1)                               
      !      DO J=1,S       
      !        bAND  = IAND(ICODE,pCODE(J)) 
      !        bool1 = bAND==pCODE(J)                                  
      !        IF( bool1 )THEN 
      !           IF(BTEST(IDBLE,12-IABS(Gcorner(5,D+J-1))))THEN          
      !            SecTypeList(K) = D+J-1   !code_id in [1,106]         
      !            K = K+1   
      !          ENDIF                                          
      !        END IF                                                
      !      END DO                                                  
      !    END IF !(NBITS>=6)          
        END IF !(NBITS>=4.AND.NPQTS>=3)

   50   CONTINUE
        SecTypeList(0) = K - 1           !number of potential combination
        SizeL          = SecTypeList(0)

C=======================================================================
C 4 Output Potential Intersection for each Cut Cell
C=======================================================================

      
      DO K=1,SecTypeList(0)
        J       = SecTypeList(K)
        LIST(K) = J
      ENDDO
      
 
!      if(itask==0.AND.debug_outp)then
!        if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id))then
!          print *, "  CELL ID *  :",IXS(11,BRICK_LIST(NIN,I)%ID)      
!          write (*,FMT='(A,I12,A,12L1,A,I12,A,12L1)') , "ICODE =",ICODE," ", (BTEST(ICODE,12-K),K=1,12), 
!     .                                                "  IDBLE=", IDBLE, " ",(BTEST(IDBLE,12-K),K=1,12)
!          do K=1,SecTypeList(0)
!            J = SecTypeList(K)
!            print *, J, StrCODE(J)
!          enddo
!        endif
!      endif

       
            IF(SecTypeList(0)==0)CYCLE !next IB
                    IF(ICODE==0)CYCLE !next IB

      
C=======================================================================
C 5 Retain only consistent combination
C=======================================================================

!        TAB(1:12) = (/(BTEST(IDBLE,12-J),J=1,12)/)
!        NINTP = NBITS + (SUM(IABS(TAB))) 
        NINTP = NBITS + POPCNT(IDBLE)
        
        LIST_VAR(1:SizeL) = LIST(1:SizeL)
        SizeLVAR = SizeL
        SizeLFIX = 0
        
        RESULT(:)  = 0
        bFOUND     = .FALSE.
        
        !db
        brickID = IXS(11,BRICK_LIST(NIN,I)%ID)
        db_WRITE = .FALSE.        

        IF(SIZEL==1)THEN
          IF(ICODE/=IDBLE)THEN
            IF(IDBLE == 0)THEN
              RESULT(1) = LIST(1)
              RESULT(2) = 0
              bFOUND    = .TRUE.  
            ELSE
              print *, "**WARNING INTER22 : UNUSED INTERSECTION POINTS FOR THIS ELEMENT ",brickID
              db_WRITE = .TRUE.
              RESULT(1) = LIST(1)
              RESULT(2) = 0
              bFOUND    = .TRUE.           
            ENDIF
          ELSE
            !ICODE/=IDBLE
             RESULT(1) = LIST(1)
             RESULT(2) = LIST(1)
             bFOUND    = .TRUE.            
          ENDIF
        ELSEIF(SIZEL==2 .AND. ((LIST(1)>=45.AND.LIST(1)<=49) .OR. (LIST(1)>=51.AND.LIST(1)<=57)))THEN   !sigle hexae or poly4
          IF(LIST(2) == LIST(1)+1)THEN
            IF( ICODE==IDBLE )THEN            
              bFOUND      = .TRUE.
              RESULT(1:2) = LIST(1:2)
              RESULT(3)   = 0
            ELSEIF(IDBLE==0)THEN
              bFOUND      = .TRUE.
              RESULT(1) = LIST(1)
              RESULT(2)   = 0            
            ELSE          
              print *, "**WARNING INTER22 : UNUSED INTERSECTION POINTS FOR THIS ELEMENT ",brickID 
              db_WRITE = .TRUE.                                   
            ENDIF
          ENDIF
        ELSE!IF(ICODE/=IDBLE)THEN !including twice the same polyhedron (now it takes automatically the complmentary since previous ChangeList)
          CALL INT22LISTCOMBI(ITASK,LIST_FIX,SizeLFIX,LIST_VAR,SizeLVAR,NINTP,ICODE,IDBLE,0,RESULT,bFOUND)
          if((.NOT.bFOUND).AND.SIZEL==1)then
            bFOUND = .TRUE.
            RESULT(1) = LIST(1)
            RESULT(2)   = 0              
          elseif((.NOT.bFOUND).AND.SIZEL>1)then
   !         if( GetPolyhedraType(LIST(1)) /= GetPolyhedraType(LIST(2)) )then
              !!!!!!!print *, "   *** warning inter22 : simplifying intersection for cell id=",brickID
              bFOUND      = .TRUE.
              RESULT(1) = LIST(  MAXLOC(LIST(1:SIZEL),1) )
              RESULT(2) = 0                
              !CALL ARRET(2)
   !         else 
   !         CALL REMOVE_DOUBLE_INTP(
   !  1                              IXS, X, ITASK, NIN, BUFBRIC,
   !  2                              IB ) 
   !         print *, "  CELL ID EXITING REMOVING DOUBLE INTERP:",IXS(11,BRICK_LIST(NIN,I)%ID)      
   !         Iremoved = Iremoved +1
   !         IF(Iremoved<=1)GOTO 10
   !         endif            
          endif      
        ENDIF
        
        IF(db_WRITE .EQV. .TRUE.)THEN
          !!------output intersection points!!
          !print *, "   ",IXS(11,brick_list(nin,i)%id)
          DO J=1,12
            IAD          = (I-1)*12+J
            NBCUT        = EDGE_LIST(NIN,IAD)%NBCUT
            DO K=1,NBCUT
              !on ecrit les coordonnees des intersections aux edges
              CUTCOOR = EDGE_LIST(NIN,IAD)%CUTCOOR(K) 
              POINT(1:3) = X(1:3, EDGE_LIST(NIN,IAD)%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD)%VECTOR(1:3))                          
            END DO ! (DO K=1,NBCUT <=> NBCUT>0)
          ENDDO
          !!---------------        
        ENDIF
        
        IF(bFOUND)THEN
        
          !if(itask==0.AND.debug_outp)then
          !  if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id))print *, "    FINAL RESULT ="
          !  if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id))print *, "RESULT(1:8)=",RESULT(1:8)
          !endif
          J = 1
          CODE = RESULT(J)
          DO WHILE(CODE/=0)         
            BRICK_LIST(NIN,I)%SecID_Cell(J) = CODE
            BRICK_LIST(NIN,I)%SECTYPE(J)    = StrCODE(IABS(CODE))
            J = J + 1 
            IF(J==9)EXIT    
             CODE = RESULT(J)         
          ENDDO
          BRICK_LIST(NIN,I)%NBCUT           = J-1
        ELSE
    !      if(itask==0.AND.debug_outp)then
    !        if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id) .and.icode/=0)print *, 
    ! .                                                                   "    NO INTERSECTION DETECTED"  
    !      endif
        ENDIF
    !    if(itask==0.AND.debug_outp) then
    !    if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id))       then
    !      print *, ""
    !      print *, ""
    !      print *, ""                
    !    endif
    !    endif

C=======================================================================
C 6 Solve ambiguity
C=======================================================================
        !done in i22subol.F 
      
      
C=======================================================================
      END DO !I=NBF,NBL

C=======================================================================
C 5 DEBUG
C=======================================================================
      CALL MY_BARRIER  !pour affichage complet dans lordre par itask 0

      if(debug_outp)then
      if(ibug22_ident==-1 .or. ibug22_ident==ixs(11,brick_list(nin,ib)%id))then
      
       !idb1(i) is dependent on ITASK, cannot loop on I=1,NB
      
       call my_barrier
       if(itask==0)then
        do I=NBF,NBL
          ICODE=BRICK_LIST(NIN,I)%ICODE
          NBITS=BRICK_LIST(NIN,I)%Nbits
          NPQTS=BRICK_LIST(NIN,I)%Npqts
          print *, "  CELL ID -:",IXS(11,BRICK_LIST(NIN,I)%ID)
           WRITE(*,FMT='(A20,I10,A4,I10)')   "     edges add from ",idb1(i)," to ",idb2(i)  
           WRITE(*,FMT='(A11,I4,A20,I2,A1,I1,A1)') "     icode=", ICODE,  "   (nbits,npqts) = (", NBITS,",",NPQTS,")"  
           WRITE(*,FMT='(A11,I4,A20,I2,A1,I1,A1)') "     idble=", IDBLE
           WRITE(*,FMT='(A,I1)') "     num planes=" ,  BRICK_LIST(NIN,I)%NBCUT
          do j=1,BRICK_LIST(NIN,I)%NBCUT
            dbKEY1(:)=BRICK_LIST(NIN,I)%SECTYPE(j)
            if(dbKEY1(1:1)=='-')    then
              WRITE(*,FMT='(A)') "     --> NONE"
            else
              WRITE(*,FMT='(A,A)') "     -->",dbKEY1(1:14)         
            end if
          enddo
        end do        
       endif
       call my_barrier
       if(itask==1)then
        do I=NBF,NBL
          ICODE=BRICK_LIST(NIN,I)%ICODE
          NBITS=BRICK_LIST(NIN,I)%Nbits
          NPQTS=BRICK_LIST(NIN,I)%Npqts
          print *, "  BRIQUE ID -:",IXS(11,BRICK_LIST(NIN,I)%ID)
           WRITE(*,FMT='(A20,I10,A4,I10)')   "     edges add from ",idb1(i)," to ",idb2(i)  
           WRITE(*,FMT='(A11,I4,A20,I2,A1,I1,A1)') "     icode=", ICODE,  "   (nbits,npqts) = (", NBITS,",",NPQTS,")"  
           WRITE(*,FMT='(A,I1)') "     num planes=" ,  BRICK_LIST(NIN,I)%NBCUT
          do j=1,BRICK_LIST(NIN,I)%NBCUT
            dbKEY1(:)=BRICK_LIST(NIN,I)%SECTYPE(j)
            if(dbKEY1(1:1)=='-')    then
              WRITE(*,FMT='(A)') "     --> NONE"
            else
              WRITE(*,FMT='(A,A)') "     -->",dbKEY1(1:14)         
            end if
          enddo
        end do        
       endif
      ! call my_barrier
      ! if(itask==2)then
      ! endif
      ! ... 
      end if
      endif

C      if(debug_outp==-1)CALL MY_BARRIER  !for debug : program is stopping before the end of this last print

C=======================================================================
C 6 DETECTION D UNE SUPERPOSITIONDE PENTA OPPOSES
C=======================================================================


      RETURN

      END 




Chd|====================================================================
Chd|  I22GBIT                       source/interfaces/int22/i22ident.F
Chd|-- called by -----------
Chd|        I22IDENT                      source/interfaces/int22/i22ident.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22GBIT(
     1       IAD , Icode, Idble, Nbits, Npqts, 
     2       idb1, idb2, NIN )
C============================================================================      
C Get bit structure of a 12bits integer
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, intent(inout) ::
     .                       Icode, Idble, Nbits, Npqts
      INTEGER, intent(in) ::
     .                       IAD, NIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,Q, pqts(4), NBCUT, IADD, idb1, idb2
C----------------------------------------------------------------
          
      Icode    = 0
      Idble    = 0
      Nbits    = 0
      Npqts    = 0
      pqts(1:4)= 0
      idb1 = 12*NCANDB+1
      idb2 = 0

      DO Q=1,4
        DO I=1,3
          J=3*(Q-1)+I ! J in [1:12]
          IADD =(IAD-1)*12+J
          idb1=min(idb1,IADD)
          idb2=max(idb2,IADD)
          NBCUT = EDGE_LIST(NIN,IADD)%NBCUT 
          IF (NBCUT>0) THEN
            pqts(Q)  = 1
            Nbits    = Nbits + 1
            IF(NBCUT>1)Idble = IBSET(Idble,12-J)
            Icode    = IBSET(Icode,12-J)  !11:0
          END IF
        END DO ! I=1,3  
      END DO ! Q=1,4 
        
      Npqts=SUM(pqts(1:4))

      RETURN
      END




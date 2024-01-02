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

!> \brief OPTION /TH/SURF output for P and A 
!! \details  Subroutine to define for each segment 
!!   the list of th surfaces  where a pressure is applied 

Chd|====================================================================
Chd|  TH_SURF_LOAD_PRESSURE         source/output/th/th_surf_load_pressure.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE TH_SURF_LOAD_PRESSURE(IGRSURF  ,TH_SURF  , IPRES   ,ILOADP   ,LLOADP  ,
     .                                 SIZLOADP ,NLOADP   ,SLLOADP  ,NIBCLD   ,NPRELD  ,
     .                                 NSURF    ,NUMNOD   )
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
       USE TH_SURF_MOD , ONLY : TH_SURF_
       USE GROUPDEF_MOD , ONLY : SURF_
       USE MESSAGE_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: SIZLOADP ,NLOADP   ,SLLOADP  ,NIBCLD   ,NPRELD, NSURF ,NUMNOD
      INTEGER , INTENT(IN) :: ILOADP(SIZLOADP,NLOADP)     !< Integer tabs for load pressures (/PFLUID, /PBLAST, /LOAD/PRESSURE )
      INTEGER , INTENT(IN) :: LLOADP(SLLOADP)             !< List of segments of surface where load pressures is applied (/PFLUID, /PBLAST, /LOAD/PRESSURE )
      INTEGER , INTENT(IN) :: IPRES(NIBCLD,NPRELD)        !< List of segments and pressure options for /PLOAD
      TYPE (SURF_)   ,DIMENSION(NSURF), INTENT(IN) :: IGRSURF  !< Type for surfaces definition 
      TYPE (TH_SURF_), INTENT(INOUT) :: TH_SURF                !< Type for /TH/SURF and load pressures output tabs
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NOD1,NOD2,NOD3,NOD4,NSEGPL,NSEGPLOAD,IAD,
     .   NS,NL,NLOADPL,N,NN,N1,N2,N3,N4,NLOADPRESS,IS,SIZE_NEW,NSEGLOADP,NSEGLOADFB,ADSEG
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: TAG
C-----------------------------------------------

        TH_SURF%NSURF = 0
        DO N=1,NSURF
            IF(IGRSURF(N)%TH_SURF == 1) THEN
               TH_SURF%NSURF = TH_SURF%NSURF + 1
            ENDIF
        ENDDO
 
        NLOADPRESS = NPRELD + NLOADP

C     ! Initialisations

        TH_SURF%IOK = 0
        TH_SURF%S_PLOAD_KSEGS = 0
        TH_SURF%S_PLOAD_SEGS  = 0

        TH_SURF%S_LOADP_KSEGS = 0
        TH_SURF%S_LOADP_SEGS  = 0

        TH_SURF%PLOAD_FLAG  = 0
        TH_SURF%LOADP_FLAG  = 0

        IF(TH_SURF%NSURF >0 .AND.NLOADPRESS > 0 ) THEN
           
           TH_SURF%IOK = 1 

C     ! Initialisations and allocation

           NSEGPL = 0
           DO NL=1,NPRELD ! PLOAD option
              N4   = IPRES(4,NL)
              IF(N4/=-1) NSEGPL = NSEGPL + 1
           ENDDO
           NSEGPLOAD = NSEGPL

           IF(NSEGPL > 0) THEN
              CALL MY_ALLOC(TH_SURF%PLOAD_KSEGS,NSEGPL+1)
              CALL MY_ALLOC(TH_SURF%PLOAD_SEGS,TH_SURF%NSURF*NSEGPL)
              TH_SURF%PLOAD_KSEGS = 0
              TH_SURF%PLOAD_SEGS = 0         
           ENDIF
C
           NSEGPL = 0
           DO NL=1,NLOADP ! LOADP (PDFLUID, PBLAST, LOAD_HYD)
              NSEGPL = NSEGPL + ILOADP(1,NL)/4 
           ENDDO
           IF(NSEGPL > 0) THEN 
              CALL MY_ALLOC(TH_SURF%LOADP_KSEGS,NSEGPL+1)
              CALL MY_ALLOC(TH_SURF%LOADP_SEGS,TH_SURF%NSURF*NSEGPL)
              TH_SURF%LOADP_KSEGS = 0
              TH_SURF%LOADP_SEGS = 0
           ENDIF
           NSEGLOADP = NSEGPL
C
           CALL MY_ALLOC(TAG,NUMNOD)
           TAG(1:NUMNOD) = 0

C     ! list of th surfaces to which each segment of PLOAD is included

           NSEGPL = 0
           IF(NSEGPLOAD > 0) THEN
             DO NL=1,NPRELD 
                N1   = IPRES(1,NL)
                N2   = IPRES(2,NL)
                N3   = IPRES(3,NL)
                N4   = IPRES(4,NL)
                IF(N4/=-1)THEN
                   TAG(N1) = 1
                   TAG(N2) = 1
                   TAG(N3) = 1
                   TAG(N4) = 1
                   NSEGPL = NSEGPL + 1
                   NS = 0
                   ADSEG = TH_SURF%PLOAD_KSEGS(NSEGPL)     
                   DO N =1,NSURF      
                     IF(IGRSURF(N)%TH_SURF == 1) THEN  
                       NN = IGRSURF(N)%NSEG
                       DO I=1,NN              
                         NOD1=IGRSURF(N)%NODES(I,1)
                         NOD2=IGRSURF(N)%NODES(I,2)
                         NOD3=IGRSURF(N)%NODES(I,3)
                         NOD4=IGRSURF(N)%NODES(I,4) 
                         IF(TAG(NOD1)==1.AND.TAG(NOD2)==1.AND.TAG(NOD3)==1) THEN
                           NS = NS + 1
                           TH_SURF%PLOAD_SEGS(ADSEG+NS)  = N
                           EXIT   
                         ENDIF
                       ENDDO                   
                     ENDIF
                   ENDDO
                   TH_SURF%PLOAD_KSEGS(NSEGPL+1) = ADSEG  +NS
                   TAG(N1) = 0
                   TAG(N2) = 0
                   TAG(N3) = 0
                   TAG(N4) = 0
                ENDIF
             ENDDO
C
             IF(TH_SURF%PLOAD_KSEGS(NSEGPL+1) > 0) TH_SURF%PLOAD_FLAG = 1
           ENDIF
C
C     ! list of th surfaces to which each segment of LOADP (PDFLUID, PBLAST, LOAD_HYD) is included
           NSEGPL = 0
           IF(NSEGLOADP > 0) THEN
             DO NL=1,NLOADP !_HYD
                IAD = ILOADP(4,NL)
                DO N=1, ILOADP(1,NL)/4 
                   N1=LLOADP(IAD+4*(N-1))
                   N2=LLOADP(IAD+4*(N-1)+1)
                   N3=LLOADP(IAD+4*(N-1)+2)
                   N4=LLOADP(IAD+4*(N-1)+3)
                   TAG(N1) = 1
                   TAG(N2) = 1
                   TAG(N3) = 1
                   TAG(N4) = 1 
                   NSEGPL = NSEGPL + 1
                   NS = 0
                   ADSEG = TH_SURF%LOADP_KSEGS(NSEGPL)  
                   DO IS =1,NSURF      
                     IF(IGRSURF(IS)%TH_SURF == 1) THEN  
                       NN = IGRSURF(IS)%NSEG
                       DO I=1,NN              
                         NOD1=IGRSURF(IS)%NODES(I,1)
                         NOD2=IGRSURF(IS)%NODES(I,2)
                         NOD3=IGRSURF(IS)%NODES(I,3)
                         NOD4=IGRSURF(IS)%NODES(I,4)
                         IF(TAG(NOD1)==1.AND.TAG(NOD2)==1.AND.TAG(NOD3)==1) THEN
                            NS = NS + 1
                            TH_SURF%LOADP_SEGS(ADSEG+NS)  = IS
                            EXIT          
                         ENDIF 
                       ENDDO                   
                     ENDIF   
                   ENDDO 
                   TH_SURF%LOADP_KSEGS(NSEGPL+1) = ADSEG  +NS 
                   TAG(N1) = 0
                   TAG(N2) = 0
                   TAG(N3) = 0
                   TAG(N4) = 0
                ENDDO        
             ENDDO 
C
             IF(TH_SURF%LOADP_KSEGS(NSEGPL+1) > 0) TH_SURF%LOADP_FLAG = 1
           ENDIF
C
           IF(ALLOCATED(TAG)) DEALLOCATE(TAG)

C
C     ! Reallocate table to the right size

           IF(NSEGPLOAD > 0) THEN
C
              SIZE_NEW = TH_SURF%PLOAD_KSEGS(NSEGPLOAD + 1)
              TH_SURF%S_PLOAD_KSEGS= NSEGPLOAD + 1
              TH_SURF%S_PLOAD_SEGS= TH_SURF%PLOAD_KSEGS(NSEGPLOAD + 1)

              CALL MY_ALLOC (OLD_TAB,SIZE_NEW)
              DO I=1,SIZE_NEW
                 OLD_TAB(I)=TH_SURF%PLOAD_SEGS(I)
              ENDDO      

C            !reallocate with new size and copy saved values 
              IF(ALLOCATED(TH_SURF%PLOAD_SEGS)) DEALLOCATE(TH_SURF%PLOAD_SEGS)    
      
              CALL MY_ALLOC(TH_SURF%PLOAD_SEGS,TH_SURF%S_PLOAD_SEGS)
              DO I=1,SIZE_NEW
                 TH_SURF%PLOAD_SEGS(I) = OLD_TAB(I)
              ENDDO   
              IF(ALLOCATED(OLD_TAB)) DEALLOCATE(OLD_TAB)
C
           ENDIF

           IF(NSEGLOADP > 0) THEN

             SIZE_NEW = TH_SURF%LOADP_KSEGS( NSEGLOADP + 1)
             TH_SURF%S_LOADP_KSEGS= NSEGLOADP + 1
             TH_SURF%S_LOADP_SEGS= TH_SURF%LOADP_KSEGS(NSEGLOADP + 1)

             CALL MY_ALLOC (OLD_TAB,SIZE_NEW)
             DO I=1,SIZE_NEW
                OLD_TAB(I)=TH_SURF%LOADP_SEGS(I)
             ENDDO      
             IF(ALLOCATED(OLD_TAB)) DEALLOCATE(TH_SURF%LOADP_SEGS)

C            !reallocate with new size and copy saved values 
             CALL MY_ALLOC(TH_SURF%LOADP_SEGS,TH_SURF%LOADP_KSEGS( NSEGLOADP + 1))
             DO I=1,SIZE_NEW
                TH_SURF%LOADP_SEGS(I) = OLD_TAB(I)
             ENDDO   
             IF(ALLOCATED(OLD_TAB)) DEALLOCATE(OLD_TAB)

           ENDIF

        ENDIF

      RETURN
      END

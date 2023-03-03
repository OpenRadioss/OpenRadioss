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
#define NIVEAU_MAX 50
Chd|====================================================================
Chd|  TRACE_IN1                     source/system/trace_back.F    
Chd|-- called by -----------
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRACE_PILE                    source/system/trace_back.F    
Chd|====================================================================
      SUBROUTINE TRACE_IN1(MY_CHAR,ILEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*ncharline MY_CHAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILEN
      CHARACTER*ncharline LINE1
      INTEGER TRACE_PILE
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
C-----------------------------------------------
      IF(ITRACE/=1)RETURN
      WRITE(LINE1,'(A)')MY_CHAR(1:MIN(ILEN,ncharline))
      NIVEAU_PILE = TRACE_PILE(1)
      IF(NIVEAU_PILE>=NIVEAU_MAX)THEN
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        WRITE(IOUT,'(A,A)')
     .' ',LINE1
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        WRITE(IOUT,'(A)')
     .' |          Trace back error (pile level > max)                |'
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        NIVEAU_PILE = TRACE_PILE(-1)
        CALL TRACE_PRINT(LINE1,-2,1)
        CALL MY_EXIT(3)
      END IF
C-----------------------------------------------
      CALL TRACE_PRINT(LINE1,NIVEAU_PILE,0)
C-----------------------------------------------
      RETURN
C
      END
#define NSUB_MAX 127
Chd|====================================================================
Chd|  TRACE_IN                      source/system/trace_back.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRACE_PILE                    source/system/trace_back.F    
Chd|====================================================================
      SUBROUTINE TRACE_IN(NSUB,ITAB,ATAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSUB,ITAB(*)
      my_real
     .   ATAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*ncharline LINE1
      CHARACTER*ncharline LINES( NSUB_MAX )
      INTEGER TRACE_PILE,LL( NSUB_MAX ) ,TYP( NSUB_MAX )
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
      SAVE LINES,LL,TYP
      DATA LINES /
     .'RADIOSS STARTER','GLOBAL UNITS',
     .'HEADER','BEGIN','TRANSLATOR',
     .'ALTDOCTAG','CARD COUNTER',
     .'DECK READING','MODIF DECK',
     1'BEGINNING','UNITS','FUNCTIONS','TABLES',
     .'MATERIALS','NODES','SKEWS',
     .'PROPERTIES','PARTS','MULTIDOMAINS','ELEMENTS',
     .'ADAPTIVE MESHING','XFEM',
     2'CONNECTIVITY',
     .'SPH GROUPS',
     .'SUBMODELS',
     .'SUBSETS','BOXES',
     .'GROUPS',
     .'TRANSFORMATIONS',
     .'MERGING CNODES',
     .'REINIT CONNECTIVITY',
     .'RANDOM NOISE',
     .'BCS','ADAPTIVE MESHING BCS',
     3'SOLIDS SORT',
     .'DAMPING',
     .'MPCS 1',
     .'DYNAMIC STORAGE',
     .'GJOINTS',
     .'ALE NEIGHBOURS',
     .'XFEM CRACK',
     .'CFD BOUNDARY ELEMENTS',
     .'MULTI-PURPOSE ELEMENTS',
     .'SPH',
     4'SPH SYM',
     .'SPH I/O',
     .'FLUID NODAL MASSES',
     .'CONCENTRED LOADS',
     .'PRESSURE LOADS',
     .'IMPOSED VELOCITIES',
     .'INITIAL VELOCITIES',
     .'INITIAL TEMPERATURES',
     .'DETONATION POINTS',
     .'LASER IMPACTS',
     5'EXTERNAL SKIN FOR ANIMATION',
     .'ACCELEROMETERS',
     .'SENSORS',
     .'RIGID LINKS',
     .'USER WINDOW',
     .'RIGID BODIES ON','RBE2 ON',
     .'FLEXIBLE BODIES ON','XFEM ELEMENTS ON/OFF',
     .'CONTAINERS','INITIAL VOLUME FRACTION',
     .'IMPLICIT DOMAIN DECOMPOSITION',
     .'SMS INITIALIZATION',
     .'DOMAIN DECOMPOSITION',
     6'ELEMENTS GROUPS',
     .'REFERENCE METRICS',
     .'DOMAIN DECOMPOSITION ARRAYS',
     .'ELEMENT BUFFER ALLOCATION',
     .'GRAVITY',
     .'EXTERNAL COUPLING',
     .'SECONDARY VELOCITIES',
     .'RBE2',
     .'RBE3',
     .'ELEMENTS DEACTIVATION',
     7'SOLID ELEMENTS FACES',
     .'MPCS 2',
     .'OPTIMIZATION',
     .'INTERFACES',
     .'RIGID WALLS',
     .'RIGID WALLS ALE',
     .'ADDED MASSES',
     .'MADYMO LINKS',
     .'MADYMO EXFEM',
     .'RIGID ENTITES',
     8'EIGEN MODES',
     .'MULTI-LEVEL DIRECT SOLVER',
     .'COMPOSITE SHELLS',
     .'USER TO SYSTEM RENUMBERING',
     .'ELEMENT BUFFER INITIALIZATION',
     .'RIGID MATERIALS',
     .'DEALLOCATION',
     .'XFEM FOR COMPOSITE',
     .'RIGID BODY ELEMENT DEACTIVATION',
     .'STAMPING INITIALIZATION',
     9'INTERFACES STIFFNESS',
     .'LASER IMPACT PHASE 2',
     .'RIVETS',
     .'SECTIONS',
     .'JOINTS',
     .'BLOCK BOUNDARY MATERIAL NODES',
     .'POROUS NODES',
     .'MONITORED VOLUMES',
     .'MESHING',
     .'BEM FLOW',
     *'EULERIAN BOUNDARY CONDITIONS',
     .'LAGRANGE MULTIPLIERS',
     .'GRAVITY NODAL FORCES',
     .'XFEM DEACTIVATION',
     .'TIME HISTORY PARTS',
     .'TIME HISTORY GROUPS',
     .'DYNAMIC STORAGE MADYMO LINK',
     .'INTERFACE INITIALIZATION PHASE 2',
     .'RBE3 INITIALIZATION',
     .'KINEMATIC CONDITIONS CHECK',
     1'MASS ARRAY ALLOCATION',
     .'DOMAIN DECOMPOSITION PHASE 2',
     .'PROCESS BEM FOR SPMD',
     .'CLOSING TMP INPUT FILE',
     .'ANIMATION FILE WRITING',
     .'RESTART FILE(S) WRITING',
     .'CLOSING STARTER',
     .'    ',
     .'    '/
      DATA LL /
     .     15,12, 6, 5,
     .  10, 9,12,12,10,
     1   9, 5, 9, 6, 9, 5, 5,
     .  10, 5,12, 8,16, 4,
     2  12,10, 9, 7, 5, 6,
     .  15,14,19,12, 3,20,
     3  11, 7, 6,15, 7,
     .  14,10,21,22, 3,
     4   7, 7,18,16,14,
     .  18,18,20,17,13,
     5  27,14, 7,11,11,
     .  15, 7,18,20,10,22,29,18,20,
     6  15,17,27,25, 7,
     .  17,16, 4, 4,21,
     7  20, 6,12,10,11,
     .  15,12,12,12,13,
     8  11,25,16,26,29,
     .  15,12,18,31,23,
     9  20,20, 6, 8, 6,
     .  29,12,17, 7, 8,
     *  28,20,20,17,18,
     .  19,27,32,19,26,
     1  21,28,20,22,22,
     .  23,15, 4, 4/
      DATA TYP /
     .      1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     1   8, 1, 1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1, 1,
     2   1, 1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1, 1,
     3   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     4   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     5   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1, 1, 1, 1, 1,
     6   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     7   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     8   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     9   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     *   1, 1, 1, 1, 1,
     .   1, 1, 1, 1, 1,
     1   1, 1, 1, 1, 1,
     .   1, 1, 1, 1/
C-----------------------------------------------
      IF(ITRACE/=1)RETURN
      NIVEAU_PILE = TRACE_PILE(1)
      IF(NIVEAU_PILE>=NIVEAU_MAX-1)THEN
         CALL TRACE_PRINT(LINE1,-2,1)
         RETURN
      ENDIF
C-----------------------------------------------
      IF(NSUB<=0.OR.NSUB> NSUB_MAX )THEN
          LINE1=' ?????? '  
      ELSE
        GOTO(1,2,3)TYP(NSUB)
 1      CONTINUE
          WRITE(LINE1,'(A)')LINES(NSUB)(1:LL(NSUB))
          GOTO 999
 2      CONTINUE
          WRITE(LINE1,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),ITAB(1)
          GOTO 999
 3      CONTINUE
          WRITE(LINE1,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),
     .                              ITAB(1)+1,'/',ITAB(2)
          GOTO 999
C
 999    CONTINUE
      ENDIF
C-----------------------------------------------
      CALL TRACE_PRINT(LINE1,NSUB,0)
C-----------------------------------------------
      RETURN
C
      END
Chd|====================================================================
Chd|  TRACE_OUT                     source/system/trace_back.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRACE_PILE                    source/system/trace_back.F    
Chd|====================================================================
      SUBROUTINE TRACE_OUT(NSUB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSUB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TRACE_PILE
      CHARACTER(LEN=ncharline) :: LINE1
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
C-----------------------------------------------
      IF(ITRACE/=1)RETURN
      LINE1(1:ncharline)=' '
      CALL TRACE_PRINT(LINE1,NSUB,-1)
      NIVEAU_PILE = TRACE_PILE(-1)
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  TRACE_OUT1                    source/system/trace_back.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRACE_PILE                    source/system/trace_back.F    
Chd|====================================================================
      SUBROUTINE TRACE_OUT1()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TRACE_PILE
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
      INTEGER NSUB,IW
      CHARACTER(LEN=ncharline) :: LINE1
C-----------------------------------------------
      LINE1(1:ncharline)=' '
      IF(ITRACE/=1)RETURN
      NIVEAU_PILE = TRACE_PILE(-1)
      NSUB=TRACE_PILE(0)
      IF (NIVEAU_PILE<0) THEN
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        WRITE(IOUT,'(A)')
     .' |          Trace back error (pile level < 0)                  |'
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        NIVEAU_PILE = TRACE_PILE(1-TRACE_PILE(0))
        CALL TRACE_PRINT(LINE1,-2,1)
        CALL MY_EXIT(3)
      END IF
      IW = -1
      CALL TRACE_PRINT(LINE1,NSUB,IW)
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  TRACE_PILE                    source/system/trace_back.F    
Chd|-- called by -----------
Chd|        TRACE_IN                      source/system/trace_back.F    
Chd|        TRACE_IN1                     source/system/trace_back.F    
Chd|        TRACE_OUT                     source/system/trace_back.F    
Chd|        TRACE_OUT1                    source/system/trace_back.F    
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION TRACE_PILE(VAL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER VAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIVEAU_PILE,IT
      DATA NIVEAU_PILE/0/
      SAVE NIVEAU_PILE
C-----------------------------------------------
      NIVEAU_PILE = NIVEAU_PILE + VAL
      TRACE_PILE = NIVEAU_PILE
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  TRACE_CF                      source/system/trace_back.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|====================================================================
      SUBROUTINE TRACE_CF(SIGNAL,IW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIGNAL,IW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*ncharline LINE1
      INTEGER I
C-----------------------------------------------
      DO I=1,ncharline
        LINE1(I:I)=' '
      END DO
      CALL TRACE_PRINT(LINE1,SIGNAL,IW)
      RETURN
      END
Chd|====================================================================
Chd|  TRACE_PRINT                   source/system/trace_back.F    
Chd|-- called by -----------
Chd|        TRACE_CF                      source/system/trace_back.F    
Chd|        TRACE_IN                      source/system/trace_back.F    
Chd|        TRACE_IN1                     source/system/trace_back.F    
Chd|        TRACE_OUT                     source/system/trace_back.F    
Chd|        TRACE_OUT1                    source/system/trace_back.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        TRACE_PILE                    source/system/trace_back.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TRACE_PRINT(LINE1,NS_SIGNAL,IW)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*ncharline LINE1
      INTEGER NS_SIGNAL,IW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TRACE_PILE
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
      INTEGER CHECK_PILE(0:NIVEAU_MAX) 
      CHARACTER*ncharline BLANC
      CHARACTER*ncharline TEXT(0:NIVEAU_MAX)
      INTEGER I,IP,IL,NLIGNES,IT
      DATA CHECK_PILE/0,NIVEAU_MAX*0/
      SAVE TEXT,CHECK_PILE
C-----------------------------------------------
      DO I=1,ncharline
        BLANC(I:I)=' '
      END DO
      NIVEAU_PILE = TRACE_PILE(0)
C
      IF(IW==0) THEN
C------------------------------------------------------------------------
C       Ajout d'une ligne a TEXT (1 ou plusieurs X par descente de pile)
C------------------------------------------------------------------------
        TEXT(NIVEAU_PILE)(1:NIVEAU_PILE)=BLANC(1:NIVEAU_PILE)
        TEXT(NIVEAU_PILE)(NIVEAU_PILE+1:ncharline)=
     .            LINE1(1:ncharline-NIVEAU_PILE)
        CHECK_PILE(NIVEAU_PILE) = NS_SIGNAL
      ELSEIF(IW==-1) THEN
C------------------------------------------
C       remonte de pile 
C------------------------------------------
        IF(NS_SIGNAL==CHECK_PILE(NIVEAU_PILE))THEN
          TEXT(NIVEAU_PILE+1)=BLANC
        ELSE
          NS_SIGNAL = -1
          IW=1
        ENDIF
      ENDIF
C
      IF(IW>0) THEN
C------------------------------------------
C       Ecriture du Trace Back et Arret
C------------------------------------------
        IF(NIVEAU_PILE<=0)CALL MY_EXIT(3)
C
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        WRITE(IOUT,'(A)')
     .' |     ABNORMAL END                                            |'
        IF(NS_SIGNAL==-1)THEN
          WRITE(IOUT,'(A)')
     .' |          Trace back error (pile)                            |'
        ELSEIF(NS_SIGNAL==-2)THEN
          WRITE(IOUT,'(A)')
     .' |          Trace back error (pile level)                      |'
        ELSEIF(NS_SIGNAL==1)THEN
          WRITE(IOUT,'(A)')
     .' |          Bus Error                                          |'
        ELSEIF(NS_SIGNAL==2)THEN
          WRITE(IOUT,'(A)')
     .' |          Floating Point Exception                           |'
        ELSEIF(NS_SIGNAL==3)THEN
          WRITE(IOUT,'(A)')
     .' |          Segmentation Violation                             |'
        ENDIF
        WRITE(IOUT,'(A)')
     .' |     TRACE BACK :                                            |'
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
C
        DO I=MAX(1,NIVEAU_PILE),MIN(NIVEAU_PILE,NIVEAU_MAX)
          WRITE(IOUT,'(A)')TEXT(I)
        ENDDO
C
        WRITE(IOUT,'(A)')
     .' +=============================================================+'
        WRITE(ISTDO,'(A)')
     .' ** ERROR TRAPPED : REPORT TO LISTING FILE'
        CALL ANCMSG(MSGID=760,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANSTOP)
        CALL MY_EXIT(3)
      ENDIF
C
      RETURN
      END

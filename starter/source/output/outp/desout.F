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
Chd|  DESOUT                        source/output/outp/desout.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE DESOUT(X   ,IXS  ,IXQ  ,IXC  ,IXT  ,
     .                  IXP ,IXR  ,IXTG ,ITAB ,PM   ,
     .                  GEO ,MS   ,IXS10,IGEO ,IPM  ,
     .                  KXSP,IPART,IPARTSP)
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .   IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),IXS10(6,*),ITAB(*),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),
     .   KXSP(NISP,*),IPART(LIPART1,*),IPARTSP(*)
C     REAL
      my_real
     .   X(3,*),MS(*), PM(NPROPM,*), GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IWA(10), I, J, FILEN
      CHARACTER CARD*nchartitle, FILNAM*100   
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME   
C-----------------------------------------
      WRITE(CARD,'(20A4)')TEXT
C
      FILNAM=ROOTNAM(1:ROOTLEN)//'Y000'
      FILEN = ROOTLEN + 4
      IF(IROOTYY/=2)THEN       
       FILEN = ROOTLEN + 9
       FILNAM=ROOTNAM(1:ROOTLEN)//'_0000.sty'
      ENDIF  
      LEN_TMP_NAME = FILEN
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TMP_NAME)      

      LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_TMP_NAME   
C
      OPEN(UNIT=IUGEO,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')
C
      WRITE(IUGEO,'(2A)')'#RADIOSS OUTPUT FILE V21 ',FILNAM(1:FILEN)
      WRITE(IUGEO,'(A)')'/HEAD'
      WRITE(IUGEO,'(A)') CARD(1:72)
C=======================================================================
C     GLOBAL
C=======================================================================
      WRITE(IUGEO,'(A)')'/CONTROL'
      WRITE(IUGEO,'(A)')'Control information'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)')'#FORMAT: (3I8) '
        WRITE(IUGEO,'(A)')'# NUMMID  NUMPID  NUMNOD'
        WRITE(IUGEO,'(3I8)')NUMMAT,NUMGEO,NUMNOD
        WRITE(IUGEO,'(A)')'#FORMAT: (7I8) '
        WRITE(IUGEO,'(A)')
     .'# NUMSOL NUMQUAD NUMSHEL NUMTRUS NUMBEAM NUMSPRI NUMSH3N  NUMSPH'
        WRITE(IUGEO,'(8I8)')
     .    NUMELS, NUMELQ, NUMELC, NUMELT, NUMELP, NUMELR,NUMELTG, NUMSPH
      ELSE
        WRITE(IUGEO,'(A)')'#FORMAT: (3I10) '
        WRITE(IUGEO,'(A)')'#   NUMMID    NUMPID    NUMNOD'
        WRITE(IUGEO,FMT=FMT_3I)NUMMAT,NUMGEO,NUMNOD
        WRITE(IUGEO,'(A)')'#FORMAT: (7I10) '
        WRITE(IUGEO,'(A)')
     .    '#   NUMSOL   NUMQUAD   NUMSHEL   NUMTRUS   NUMBEAM'//
     .    '   NUMSPRI   NUMSH3N    NUMSPH'
        WRITE(IUGEO,FMT=FMT_8I)
     .    NUMELS, NUMELQ, NUMELC, NUMELT, NUMELP, NUMELR,NUMELTG, NUMSPH
      ENDIF
C=======================================================================
C     MID
C=======================================================================
      WRITE(IUGEO,'(A)')'/MID'
      WRITE(IUGEO,'(A)')'Material ID information'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)')'#FORMAT: (2I8,A40) '
      ELSE
        WRITE(IUGEO,'(A)')'#FORMAT: (2I10,A40) '
      ENDIF
      WRITE(IUGEO,'(2A)')'# SYSMID  USRMID',
     . '                                 MIDHEAD'
      DO I=1,NUMMAT
        CALL FRETITL2(CARD,IPM(NPROPMI-LTITR+1,I),LTITR)
        WRITE(IUGEO,'(2I8,A80)') I, IPM(1,I),CARD
      ENDDO
      IF (OUTYY_FMT==2) THEN
        DO I=1,NUMMAT
          CALL FRETITL2(CARD,IPM(NPROPMI-LTITR+1,I),LTITR)
          WRITE(IUGEO,'(2I8,A80)') I, IPM(1,I),CARD
        ENDDO
      ELSE
        DO I=1,NUMMAT
          CALL FRETITL2(CARD,IPM(NPROPMI-LTITR+1,I),LTITR)
          WRITE(IUGEO,'(2I10,A80)') I, IPM(1,I),CARD
        ENDDO
      ENDIF
C=======================================================================
C     PID
C=======================================================================
      WRITE(IUGEO,'(A)')'/PID'
      WRITE(IUGEO,'(A)')'Property ID information'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)')'#FORMAT: (2I8,A40) '
      ELSE
        WRITE(IUGEO,'(A)')'#FORMAT: (2I10,A40) '
      ENDIF
      WRITE(IUGEO,'(2A)')'# SYSPID  USRPID',
     . '                                 PIDHEAD'
      IF (OUTYY_FMT==2) THEN
        DO I=1,NUMGEO
          CALL FRETITL2(CARD,IGEO(NPROPGI-LTITR+1,I),LTITR)
          WRITE(IUGEO,'(2I8,A80)') I,IGEO(1,I),CARD
        ENDDO
      ELSE
        DO I=1,NUMGEO
          CALL FRETITL2(CARD,IGEO(NPROPGI-LTITR+1,I),LTITR)
          WRITE(IUGEO,'(2I10,A80)') I,IGEO(1,I),CARD
        ENDDO
      ENDIF
C=======================================================================
C     NODE
C=======================================================================
      WRITE(IUGEO,'(A)')'/NODE'
      WRITE(IUGEO,'(A)')'Nodes information'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)')'#FORMAT: (2I8,1P4E16.9) '
      ELSE
        WRITE(IUGEO,'(A)')'#FORMAT: (2I10,1P4G20.13) '
      ENDIF
      WRITE(IUGEO,'(3A)')'# SYSNOD  USRNOD',
     . '               X               Y               Z',
     . '            MASS'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(2I8,1P4E16.9)')
     .     (I,ITAB(I),X(1,I),X(2,I),X(3,I),MS(I),I=1,NUMNOD)
      ELSE
        WRITE(IUGEO,'(2I10,1P4G20.13)')
     .     (I,ITAB(I),X(1,I),X(2,I),X(3,I),MS(I),I=1,NUMNOD)
      ENDIF
C=======================================================================
C     SOLID
C=======================================================================
      IF(NUMELS10/=0)THEN
        WRITE(IUGEO,'(A)') '/SOLIDE'
        WRITE(IUGEO,'(A)')'3d Solid Elements'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)') '#FORMAT: (4I8/8X,8I8) '
        ELSE
          WRITE(IUGEO,'(A)') '#FORMAT: (4I10/8X,8I10) '
        ENDIF
        WRITE(IUGEO,'(A)') '# SYSSOL  USRSOL  SYSMID  SYSPID'
        WRITE(IUGEO,'(2A)')'#SYSNOD1 SYSNOD2 SYSNOD3 SYSNOD4',
     .                     ' SYSNOD5 SYSNOD6 SYSNOD7 SYSNOD8'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(4I8/8X,8I8)')
     .       (I,IXS(NIXS,I),IXS(1,I),IXS(NIXS-1,I),
     .       IXS(2,I),IXS(3,I),IXS(4,I),IXS(5,I),
     .       IXS(6,I),IXS(7,I),IXS(8,I),IXS(9,I),I=1,NUMELS-NUMELS10)
        ELSE
          WRITE(IUGEO,'(4I10/8X,8I10)')
     .       (I,IXS(NIXS,I),IXS(1,I),IXS(NIXS-1,I),
     .       IXS(2,I),IXS(3,I),IXS(4,I),IXS(5,I),
     .       IXS(6,I),IXS(7,I),IXS(8,I),IXS(9,I),I=1,NUMELS-NUMELS10)
        ENDIF
        WRITE(IUGEO,'(A)') '/TETRA10'
        WRITE(IUGEO,'(A)')'3d Solid Elements'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)') '#FORMAT: (4I8/8X,8I8/2I) '
        ELSE
          WRITE(IUGEO,'(A)') '#FORMAT: (4I10/8X,8I10/2I) '
        ENDIF
        WRITE(IUGEO,'(A)') '# SYSSOL  USRSOL  SYSMID  SYSPID'
        WRITE(IUGEO,'(2A)')'#SYSNOD1 SYSNOD2 SYSNOD3 SYSNOD4',
     .                     'SYSNOD5 SYSNOD6 SYSNOD7 SYSNOD8',
     .                     '#SYSNOD9 SYSNOD10'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(4I8/8X,10I8)')
     .     (NUMELS8+I,IXS(NIXS,NUMELS8+I),
     .     IXS(1,NUMELS8+I),IXS(NIXS-1,NUMELS8+I),        
     .     IXS(2,NUMELS8+I),IXS(4,NUMELS8+I),
     .     IXS(7,NUMELS8+I),IXS(6,NUMELS8+I),
     .     IXS10(1,I),IXS10(2,I),IXS10(3,I),IXS10(4,I),
     .     IXS10(5,I),IXS10(6,I) ,I=1,NUMELS10)
        ELSE
          WRITE(IUGEO,'(4I10/8X,10I10)')
     .     (NUMELS8+I,IXS(NIXS,NUMELS8+I),
     .     IXS(1,NUMELS8+I),IXS(NIXS-1,NUMELS8+I),        
     .     IXS(2,NUMELS8+I),IXS(4,NUMELS8+I),
     .     IXS(7,NUMELS8+I),IXS(6,NUMELS8+I),
     .     IXS10(1,I),IXS10(2,I),IXS10(3,I),IXS10(4,I),
     .     IXS10(5,I),IXS10(6,I) ,I=1,NUMELS10)
        ENDIF
      ELSE
        WRITE(IUGEO,'(A)') '/SOLIDE'
        WRITE(IUGEO,'(A)')'3d Solid Elements'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)') '#FORMAT: (4I8/8X,8I8) '
        ELSE
          WRITE(IUGEO,'(A)') '#FORMAT: (4I10/8X,8I10) '
        ENDIF
        WRITE(IUGEO,'(A)') '# SYSSOL  USRSOL  SYSMID  SYSPID'
        WRITE(IUGEO,'(2A)')'#SYSNOD1 SYSNOD2 SYSNOD3 SYSNOD4',
     .                     ' SYSNOD5 SYSNOD6 SYSNOD7 SYSNOD8'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(4I8/8X,8I8)')
     .     (I,IXS(NIXS,I),IXS(1,I),IXS(NIXS-1,I),
     .     IXS(2,I),IXS(3,I),IXS(4,I),IXS(5,I),
     .     IXS(6,I),IXS(7,I),IXS(8,I),IXS(9,I),I=1,NUMELS)
        ELSE
          WRITE(IUGEO,'(4I10/8X,8I10)')
     .     (I,IXS(NIXS,I),IXS(1,I),IXS(NIXS-1,I),
     .     IXS(2,I),IXS(3,I),IXS(4,I),IXS(5,I),
     .     IXS(6,I),IXS(7,I),IXS(8,I),IXS(9,I),I=1,NUMELS)
        ENDIF
      ENDIF
C=======================================================================
C     QUAD
C=======================================================================
      WRITE(IUGEO,'(A)') '/QUAD'
      WRITE(IUGEO,'(A)')'2d Solid Elements'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (8I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (8I10) '
      ENDIF
      WRITE(IUGEO,'(2A)')'#SYSQUAD USRQUAD  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2 SYSNOD3 SYSNOD4'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(8I8)') 
     .   (I,IXQ(NIXQ,I),IXQ(1,I),IXQ(NIXQ-1,I),
     .   IXQ(2,I),IXQ(3,I),IXQ(4,I),IXQ(5,I),I=1,NUMELQ)
      ELSE
        WRITE(IUGEO,'(8I10)') 
     .   (I,IXQ(NIXQ,I),IXQ(1,I),IXQ(NIXQ-1,I),
     .   IXQ(2,I),IXQ(3,I),IXQ(4,I),IXQ(5,I),I=1,NUMELQ)
      ENDIF
C=======================================================================
C     SHELL
C=======================================================================
      WRITE(IUGEO,'(A)') '/SHELL'
      WRITE(IUGEO,'(A)')'3d Shell Elements '
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (8I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (8I10) '
      ENDIF
      WRITE(IUGEO,'(2A)')'#SYSSHEL USRSHEL  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2 SYSNOD3 SYSNOD4'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(8I8)') 
     .   (I,IXC(NIXC,I),IXC(1,I),IXC(NIXC-1,I),
     .   IXC(2,I),IXC(3,I),IXC(4,I),IXC(5,I),I=1,NUMELC)
      ELSE
        WRITE(IUGEO,'(8I10)') 
     .   (I,IXC(NIXC,I),IXC(1,I),IXC(NIXC-1,I),
     .   IXC(2,I),IXC(3,I),IXC(4,I),IXC(5,I),I=1,NUMELC)
      ENDIF
C=======================================================================
C     TRUSS
C=======================================================================
      WRITE(IUGEO,'(A)') '/TRUSS'
      WRITE(IUGEO,'(A)')'3d Truss Elements'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (6I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (6I10) '
      ENDIF
      WRITE(IUGEO,'(2A)') '#SYSTRUS USRTRUS  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(6I8)') 
     .   (I,IXT(NIXT,I),IXT(1,I),IXT(NIXT-1,I),
     .   IXT(2,I),IXT(3,I),I=1,NUMELT)
      ELSE
        WRITE(IUGEO,'(6I10)') 
     .   (I,IXT(NIXT,I),IXT(1,I),IXT(NIXT-1,I),
     .   IXT(2,I),IXT(3,I),I=1,NUMELT)
      ENDIF
C=======================================================================
C     BEAM
C=======================================================================
      WRITE(IUGEO,'(A)') '/BEAM'
      WRITE(IUGEO,'(A)')'3d Beam Elements'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (7I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (7I10) '
      ENDIF
      WRITE(IUGEO,'(2A)')'#SYSBEAM USRBEAM  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2 SYSNOD3'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(7I8)') 
     .   (I,IXP(NIXP,I),IXP(1,I),IXP(NIXP-1,I),
     .   IXP(2,I),IXP(3,I),IXP(4,I),I=1,NUMELP)
      ELSE
        WRITE(IUGEO,'(7I10)') 
     .   (I,IXP(NIXP,I),IXP(1,I),IXP(NIXP-1,I),
     .   IXP(2,I),IXP(3,I),IXP(4,I),I=1,NUMELP)
      ENDIF
C=======================================================================
C     SPRING
C=======================================================================
      WRITE(IUGEO,'(A)') '/SPRING'
      WRITE(IUGEO,'(A)')'3d Spring Elements'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (6I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (6I10) '
      ENDIF
      WRITE(IUGEO,'(2A)')'#SYSSPRI USRSPRI  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(6I8)') 
     .   (I,IXR(NIXR,I),0,IXR(1,I),
     .   IXR(2,I),IXR(3,I),I=1,NUMELR)
      ELSE
        WRITE(IUGEO,'(6I10)') 
     .   (I,IXR(NIXR,I),0,IXR(1,I),
     .   IXR(2,I),IXR(3,I),I=1,NUMELR)
      ENDIF
C=======================================================================
C     SHELL3N
C=======================================================================
      WRITE(IUGEO,'(A)') '/SHELL3N'
      WRITE(IUGEO,'(A)')'3d Shell Elements (Triangle) '
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (7I8) '
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (7I10) '
      ENDIF
      WRITE(IUGEO,'(2A)')'#SYSSH3N USRSH3N  SYSMID  SYSPID',
     .                   ' SYSNOD1 SYSNOD2 SYSNOD3'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(7I8)') 
     .   (I,IXTG(NIXTG,I),IXTG(1,I),IXTG(NIXTG-1,I),
     .   IXTG(2,I),IXTG(3,I),IXTG(4,I),I=1,NUMELTG)
      ELSE
        WRITE(IUGEO,'(7I10)') 
     .   (I,IXTG(NIXTG,I),IXTG(1,I),IXTG(NIXTG-1,I),
     .   IXTG(2,I),IXTG(3,I),IXTG(4,I),I=1,NUMELTG)
      ENDIF
C=======================================================================
C
C     SPH
C=======================================================================
      WRITE(IUGEO,'(A)') '/SPHCEL'
      WRITE(IUGEO,'(A)')'SPH particles'
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(A)') '#FORMAT: (4I8/8X,I8) '
        WRITE(IUGEO,'(A)') '# SYSSPH  USRSPH  SYSMID  SYSPID'
        WRITE(IUGEO,'(A)')'#SYSNOD'
      ELSE
        WRITE(IUGEO,'(A)') '#FORMAT: (4I10/10X,I10) '
        WRITE(IUGEO,'(A)') '#   SYSSPH    USRSPH    SYSMID    SYSPID'
        WRITE(IUGEO,'(A)') '#   SYSNOD'
      ENDIF
      IF (OUTYY_FMT==2) THEN
        WRITE(IUGEO,'(4I8/8X,I8)') 
     .   (I,KXSP(NISP,I),IPART(1,IPARTSP(I)),
     .   IPART(2,IPARTSP(I)),KXSP(3,I),I=1,NUMSPH)
      ELSE
        WRITE(IUGEO,'(4I10/10X,I10)') 
     .   (I,KXSP(NISP,I),IPART(1,IPARTSP(I)),
     .   IPART(2,IPARTSP(I)),KXSP(3,I),I=1,NUMSPH)
      ENDIF
C=======================================================================
C     END
C=======================================================================
      WRITE(IUGEO,'(A)') '/ENDDATA'
C
      WRITE (IOUT,60) FILNAM(1:FILEN)
 60   FORMAT (/4X,14H PLOT    FILE:,1X,A,8H WRITTEN/
     .         4X,14H -------------/)
C
       CLOSE (UNIT=IUGEO,STATUS='KEEP')
C
      RETURN
      END

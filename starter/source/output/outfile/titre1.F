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
C==============================================================
C
C     SORTIE1.FOR
C
C==============================================================
Chd|====================================================================
Chd|  TITRE1                        source/output/outfile/titre1.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TITRE1
C------------------------------------
C     DEBUT => CONTROLS
C------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "titr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      TITRE(001)       ='----+----1----+----2----+----3----+----4----+----5'//          '----+----6----+-'
      TITRE(002)       =                 '--7----+----8----+----9----+----0'//          '----+----1----+----2----+----3--'
      TITRE(003)       =' REAL    COMMON LENGTH'
      TITRE(004)       =' INTEGER COMMON LENGTH'
      TITRE(005)       =       '     ------------------            '
      TITRE(006)       =       '     NORMAL TERMINATION            '
      TITRE(007)(1:40) ='     ERROR TERMINATION                  '
      TITRE(007)(41:60)='ERROR(S)            '
      TITRE(008)(1:40) ='     TERMINATION WITH WARNING           '
      TITRE(008)(41:60)='WARNING(S)          '
      TITRE(010)       =' .. CONTROL VARIABLES'
      TITRE(011)       =' .. MATERIALS'
      TITRE(012)       =' .. UNITS SYSTEM'
      TITRE(013)       =' .. PARALLEL RESTART FILE READING'
      TITRE(100)       =' '
      TITRE(101)       =' '
      TITRE(102)       =' '
      TITRE(103)       =' '
C------------------------------------
C     CONTROL
C------------------------------------
      TITRE(110)       ='    CONTROL  PARAMETERS'                                                  
      TITRE(111)       ='    -------------------'                                                  
      TITRE(112)       = '    DTDES : TIME STEP FOR DRAWINGS  . . . . . . . . . . .'               
      TITRE(113)       = '    DTHIS : TIME STEP FOR TIME HISTORIES  . . . . . . . .'               
      TITRE(114)       = '    DTINI : INITIAL TIME STEP . . . . . . . . . . . . . .'               
      TITRE(115)       = '    DTFAC : TIME STEP SCALE FACTOR  . . . . . . . . . . .'               
      TITRE(116)       = '    DTMIN : MINIMUM TIME STEP . . . . . . . . . . . . . .'               
      TITRE(117)       = '    IPRI  : PRINTOUT FLAG . . . . . . . . . . . . . . . .'               
      TITRE(118)       = '    IFORM8: I10 INPUT FORMAT FLAG. . . . . . . . . . . . '              
      TITRE(119)       = '    IHSH  : SHELL HOURGLASS SHAPE FUNCTION FLAG . . . . .'               
      TITRE(120)       = '    INVERS: MANUAL VERSION OF THIS INPUT DECK . . . . . .'               
      TITRE(121)       = '    IRLTYP: FLAG FOR TYPE OF R-FILE READ (MODIF). . . . .'               
      TITRE(122)       = '    IRETYP: FLAG FOR TYPE OF R-FILE WRITTEN . . . . . . .'               
      TITRE(123)       = '    IGTYP : FLAG FOR TYPE OF G-FILE . . . . . . . . . . .'               
      TITRE(124)       = '    XALEA : MAXIMUM NODAL RANDOM NOISE. . . . . . . . . .'               
      TITRE(125)       = '    NUMMAT: NUMBER OF MATERIALS . . . . . . . . . . . . .'               
      TITRE(126)       = '    NUMNOD: NUMBER OF NODAL POINTS. . . . . . . . . . . .'               
      TITRE(127)       = '    NUMSKW: NUMBER OF SKEW REFERENCE FRAMES . . . . . . .'               
      TITRE(128)       = '    NUMBCS: NUMBER OF BOUNDARY CONDITIONS . . . . . . . .'               
      TITRE(129)(1:35) = '    ID0A  : NODES,ELEMENTS READ ON '                               
      TITRE(129)(47:57)=                            ' ID0A . . .'                           
      TITRE(130)       = '    N2D3D : ANALYSIS TYPE: 0=3D,1=AXISYM,2=PLANE STRAIN .'               
      TITRE(131)       = '    NUMELQ: NUMBER OF 2D QUAD ELEMENTS. . . . . . . . . .'               
      TITRE(132)       = '    NUMELS: NUMBER OF 3D SOLID ELEMENTS . . . . . . . . .'               
      TITRE(133)       = '    NUMELC: NUMBER OF 3D SHELL ELEMENTS (4-NODES) . . . .'               
      TITRE(134)       = '    NUMELT: NUMBER OF 3D TRUSS ELEMENTS . . . . . . . . .'               
      TITRE(135)       = '    NUMGEO: NUMBER OF PROPERTY SETS . . . . . . . . . . .'               
      TITRE(136)       = '    NUMELP: NUMBER OF 3D BEAM  ELEMENTS . . . . . . . . .'               
      TITRE(137)       = '    NUMELR: NUMBER OF 3D SPRING ELEMENTS. . . . . . . . .'               
      TITRE(138)       = '    NFUNCT: NUMBER OF USER''S FUNCTIONS . . . . . . . . .'              
      TITRE(139)       = '    NCONLD: NUMBER OF NODES WITH CONCENTRATED LOADS . . .'               
      TITRE(140)       = '    NINVEL: NUMBER OF NODES WITH INITIAL VELOCITY . . . .'               
      TITRE(141)       = '    NDETPT: NUMBER OF DETONATION POINTS . . . . . . . . .'               
      TITRE(142)       = '    NPRELD: NUMBER OF SEGMENTS WITH PRESSURE. . . . . . .'               
      TITRE(143)       = '    IGRAX : X GRAVITY LOAD CURVE NUMBER . . . . . . . . .'               
      TITRE(144)       = '    IGRAY : Y GRAVITY LOAD CURVE NUMBER . . . . . . . . .'               
      TITRE(145)       = '    IGRAZ : Z GRAVITY LOAD CURVE NUMBER . . . . . . . . .'               
      TITRE(146)       = '    NINTER: NUMBER OF INTERFACES. . . . . . . . . . . . .'               
      TITRE(147)       = '    NRWALL: NUMBER OF RIGID WALLS . . . . . . . . . . . .'               
      TITRE(148)       = '    NRBODY: NUMBER OF RIGID BODIES. . . . . . . . . . . .'               
      TITRE(149)       = '    NCONX : NUMBER OF RIGID LINKS TO MADYMO BODIES. . . .'               
      TITRE(150)       = '    NODMAS: NUMBER OF ADDED NODAL MASSES. . . . . . . . .'               
      TITRE(151)       = '    NFXVEL: NUMBER OF FIXED VELOCITIES. . . . . . . . . .'               
      TITRE(152)       = '    NRIVET: NUMBER OF RIVETS OR SPOT WELDS. . . . . . . .'               
      TITRE(153)       = '    NSECT : NUMBER OF SECTIONS SAVED (TIME HISTORY PLOTS)'               
      TITRE(154)       = '                                                         ' !obsloete option removed        
      TITRE(155)       = '    NJOINT: NUMBER OF JOINTS. . . . . . . . . . . . . . .'               
      TITRE(156)       = '    NSNOD: NUMBER OF NODES SAVED (TIME HISTORY PLOTS) . .'               
      TITRE(157)       = '    NSELQ: NUMBER OF QUAD   ELEMENTS SAVED. . . . . . . .'               
      TITRE(158)       = '    NSELS: NUMBER OF SOLID  ELEMENTS SAVED. . . . . . . .'               
      TITRE(159)       = '    NSELC: NUMBER OF SHELL  ELEMENTS SAVED. . . . . . . .'               
      TITRE(160)       = '    NSELT: NUMBER OF TRUSS  ELEMENTS SAVED. . . . . . . .'               
      TITRE(161)       = '    NSELP: NUMBER OF BEAM   ELEMENTS SAVED. . . . . . . .'               
      TITRE(162)       = '    NSELR: NUMBER OF SPRING ELEMENTS SAVED. . . . . . . .'               
      TITRE(163)       = '    ALE  PARAMETERS                                 '                    
      TITRE(164)       = '    ---------------                                 '                    
      TITRE(165)       = '    ALPHA : DONEA PARAMETER . . . . . . . . . . . . . . .'               
      TITRE(166)       = '    GAMMA : DONEA PARAMETER . . . . . . . . . . . . . . .'               
      TITRE(167)       = '    VGX   : X GRID VELOCITY SCALE FACTOR. . . . . . . . .'               
      TITRE(168)       = '    VGY   : Y GRID VELOCITY SCALE FACTOR. . . . . . . . .'               
      TITRE(169)       = '    VGZ   : Z GRID VELOCITY SCALE FACTOR. . . . . . . . .'               
      TITRE(170)       = '    VOLMIN: MINIMUM VOLUME. . . . . . . . . . . . . . . .'               
      TITRE(171)       = '    NDETSG: NUMBER OF DETONATION SEGMENTS . . . . . . . .'               
      TITRE(172)       = '    NECRAN: NUMBER OF SHADOW LINES. . . . . . . . . . . .'               
      TITRE(173)       = '    NSMAT: NUMBER OF MATERIALS SAVED. . . . . . . . . . .'               
      TITRE(174)       = '    NSELR: NUMBER OF RIGID BODIES SAVED . . . . . . . . .'               
      TITRE(175)       = '    NINVEL: NUMBER OF INPUT CARDS FOR INITIAL VELOCITY. .'               
      TITRE(178)       = '    NLASER: NUMBER OF LASER IMPACT OPTIONS. . . . . . . .'               
      TITRE(179)       = '    ISNOD: FLAG TO SAVE NODES IN SKEW FRAME . . . . . . .'               
      TITRE(180)       = '    NUMELTG: NUMBER OF 3D SHELL ELEMENTS (3-NODES). . . .'               
      TITRE(181)       = '    NSELTG: NUMBER OF 3 NODES SHELL  ELEMENTS SAVED . . .'               
      TITRE(182)       = '    NUMELX: NUMBER OF MULTIPURPOSE ELEMENTS . . . . . . .'               
      TITRE(183)       = '    NIMPDISP: NUMBER OF IMPOSED DISPLACEMENTS . . . . . .'               
      TITRE(184)       = '    NIMPVEL: NUMBER OF IMPOSED VELOCITIES . . . . . . . .'               
      TITRE(185)       = '    NIMPACC: NUMBER OF IMPOSED ACCELERATIONS . . . . . . '               
      TITRE(186)       = '    NUMELIG3D: NUMBER OF ISO-GEOMETRIC ELEMENTS . . . . .'               
      TITRE(187)       = '    NDETCORD: NUMBER OF DETONATION CORD . . . . . . . . .'               
      TITRE(188)       = '    NDETPL  : NUMBER OF PLANAR DETONATION . . . . . . . .'               
      TITRE(189)       = '    NALEBCS : NUMBER OF ALE BOUNDARY CONDITIONS . . . . .'     
      TITRE(190)       = '    NINTERFRIC: NUMBER OF FRICTION INTERFACES . . . . . .'    
      TITRE(191)       = '    NUMELT3 : NUMBER OF 2D TRIA ELEMENTS. . . . . . . . .'                
                      

      RETURN
      END

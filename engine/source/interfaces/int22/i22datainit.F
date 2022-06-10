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
Chd|  I22DATAINIT                   source/interfaces/int22/i22datainit.F
Chd|-- called by -----------
Chd|        R_BUFBRIC_22                  source/interfaces/int22/r_bufbric_22.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22DATAINIT()
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD   
C=======================================================================   
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "subvolumes.inc"
#include      "i22edges.inc"
#include      "inter22.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "alefvm.inc"
!#include      "mmale51_c.inc" 
C-----------------------------------------------


C=======================================================================  
C               D A T A   D E F I N I T I O N S
C=======================================================================  

C=======================================================================
C definitions from subvolumes.inc
C=======================================================================
      INTEGER Itmpindex22,I,NIN,J,K, IFLG

      DATA LSECTYP/14/

      !----------------------------------------------------!
      !  COMBINATIONS NUMBER                               !
      !----------------------------------------------------!
      DATA S_TETRA /08/
      DATA S_PENTA /12/
      DATA S_POLY3 /24/
      DATA S_HEXAE /03/
      DATA S_POLY4 /04/
      DATA S_POLY4A/06/      
      DATA S_POLY4B/06/      
      DATA S_POLYC /24/      
      DATA S_POLYH(1:8) /08,12,24,03,04,06,06,24/ !generic form                  
      !----------------------------------------------------!
      S22_MAX = S_TETRA  + S_PENTA  + S_POLY3  +  S_HEXAE  +  S_POLY4
     .        + S_POLY4A + S_POLY4B + S_POLYC

      !----------------------------------------------------!
      !  MULTIPLICITIES                                    !
      !----------------------------------------------------!
      DATA M_TETRA /01/
      DATA M_PENTA /01/
      DATA M_POLY3 /01/
      DATA M_HEXAE /02/ !-> the same intersection points configuration leads to 2 different (opposite) graphes
      DATA M_POLY4 /02/ !-> the same intersection points configuration leads to 2 different (opposite) graphes
      DATA M_POLY4A/02/ !-> the same intersection points configuration leads to 2 different (opposite) graphes
      DATA M_POLY4B/02/ !-> the same intersection points configuration leads to 2 different (opposite) graphes            
      DATA M_POLYC /01/
      DATA M_POLYH(1:8) /01,01,01,02,02,02,02,01/ !generic form                  
      
      !----------------------------------------------------!
      !  COMBINATIONS INDEXES  (first index from 1:52)     !
      !----------------------------------------------------!
      DATA D_TETRA /01/
      DATA D_PENTA /09/
      DATA D_POLY3 /21/
      DATA D_HEXAE /45/
      DATA D_POLY4 /51/
      DATA D_POLY4A/59/
      DATA D_POLY4B/71/            
      DATA D_POLYC /83/      
      DATA D_POLYH(1:8) /01,09,21,45,51,59,71,83/ !generic form                  

      !----------------------------------------------------!
      !  main NODES NUMBER                               !
      !----------------------------------------------------!
      DATA N_TETRA /01/
      DATA N_PENTA /02/
      DATA N_POLY3 /03/
      DATA N_HEXAE /04/
      DATA N_POLY4 /04/
      DATA N_POLY4A/04/
      DATA N_POLY4B/04/            
      DATA N_POLYC /04/      
      DATA N_POLYH(1:8) /01,02,03,04,04,04,04,04/ !generic form            

      !----------------------------------------------------!
      !  NUMBER OF FACES                                   !
      !----------------------------------------------------!
      DATA F_TETRA /03/
      DATA F_PENTA /04/
      DATA F_POLY3 /05/
      DATA F_HEXAE /05/
      DATA F_POLY4 /06/
      DATA F_POLY4A/06/
      DATA F_POLY4B/06/            
      DATA F_POLYC /05/      
      DATA F_POLYH(1:8) /03,04,05,05,06,06,06,05/ !generic form      

      !----------------------------------------------------!
      !  NUMBER OF CORNER(EDGE)                            !
      !----------------------------------------------------!
      DATA C_TETRA /03/
      DATA C_PENTA /04/
      DATA C_POLY3 /05/
      DATA C_HEXAE /04/
      DATA C_POLY4 /06/
      DATA C_POLY4A/06/
      DATA C_POLY4B/06/            
      DATA C_POLYC /06/      
      DATA C_POLYH(1:8) /03,04,05,04,06,06,06,06/ !generic form

      !----------------------------------------------------!
      !  SUBVOLUMES COMBINATIONS                           !
      !----------------------------------------------------!
      !TETRA
      DATA bCODE(01) /B'111000000000'/    !3584 !1
      DATA bCODE(02) /B'000111000000'/    !0448 !2
      DATA bCODE(03) /B'000000111000'/    !0056 !3
      DATA bCODE(04) /B'000000000111'/    !0007 !4
      DATA bCODE(05) /B'100100100000'/    !2336 !5
      DATA bCODE(06) /B'010010000100'/    !1156 !6  
      DATA bCODE(07) /B'001000010010'/    !0530 !7
      DATA bCODE(08) /B'000001001001'/    !0073 !8
      !----------------------------------------------------!
      !PENTA
      DATA bCODE(09) /B'011100100000'/    !1824 !9
      DATA bCODE(10) /B'101010000100'/    !2692 !10
      DATA bCODE(11) /B'110000010010'/    !3090 !11
      DATA bCODE(12) /B'100011100000'/    !2272 !12
      DATA bCODE(13) /B'010101000100'/    !1348 !13
      DATA bCODE(14) /B'000110001001'/    !0393 !14
      DATA bCODE(15) /B'100100011000'/    !2328 !15
      DATA bCODE(16) /B'001000101010'/    !0554 !16
      DATA bCODE(17) /B'000001110001'/    !0113 !17
      DATA bCODE(18) /B'010010000011'/    !1155 !18
      DATA bCODE(19) /B'001000010101'/    !0533 !19
      DATA bCODE(20) /B'000001001110'/    !0078 !20
      !----------------------------------------------------!
      !POLY3
      DATA bCODE(21) /B'101101000100'/    !2884 !21
      DATA bCODE(22) /B'001110100100'/    !0932 !22
      DATA bCODE(23) /B'011011100000'/    !1760 !23
      DATA bCODE(24) /B'110001100100'/    !3172 !24
      DATA bCODE(25) /B'010101000011'/    !1347 !25
      DATA bCODE(26) /B'010011001010'/    !1226 !26
      DATA bCODE(27) /B'010100001101'/    !1293 !27
      DATA bCODE(28) /B'000110001110'/    !0398 !28
      DATA bCODE(29) /B'001001100011'/    !0611 !29
      DATA bCODE(30) /B'000001110110'/    !0118 !30
      DATA bCODE(31) /B'001001011100'/    !0604 !31
      DATA bCODE(32) /B'001000101101'/    !0557 !32
      DATA bCODE(33) /B'011100011000'/    !1816 !33
      DATA bCODE(34) /B'101100001010'/    !2826 !34
      DATA bCODE(35) /B'010100110010'/    !1330 !35
      DATA bCODE(36) /B'110000101010'/    !3114 !36
      DATA bCODE(37) /B'100010101001'/    !2217 !37
      DATA bCODE(38) /B'000110110001'/    !0433 !38
      DATA bCODE(39) /B'100011011000'/    !2264 !39
      DATA bCODE(40) /B'100101010001'/    !2385 !40
      DATA bCODE(41) /B'110000010101'/    !3093 !41
      DATA bCODE(42) /B'100010010110'/    !2198 !42
      DATA bCODE(43) /B'011010010001'/    !1681 !43
      DATA bCODE(44) /B'101010000011'/    !2691 !44
      !----------------------------------------------------!
      !HEXAE
      DATA bCODE(45) /B'100010010001'/    !2193 !45-46
      DATA bCODE(46) /B'100010010001'/    !2193      
      DATA bCODE(47) /B'010100001010'/    !1290 !47-48
      DATA bCODE(48) /B'010100001010'/    !1290      
      DATA bCODE(49) /B'001001100100'/    !0612 !49-50
      DATA bCODE(50) /B'001001100100'/    !0612      
      !----------------------------------------------------!
      !POLY4
      DATA bCODE(51) /B'011011011000'/    !1752 !51-52
      DATA bCODE(52) /B'011011011000'/    !1752      
      DATA bCODE(53) /B'101101000011'/    !2883 !53-54
      DATA bCODE(54) /B'101101000011'/    !2883      
      DATA bCODE(55) /B'110000101101'/    !3117 !55-56
      DATA bCODE(56) /B'110000101101'/    !3117 
      DATA bCODE(57) /B'000110110110'/    !0438 !57-58     
      DATA bCODE(58) /B'000110110110'/    !0438
      !----------------------------------------------------!
      !POLY4A
      DATA bCODE(59) /B'101011001010'/    !POLY4A 45 - 2762
      DATA bCODE(60) /B'101011001010'/    !POLY4A 26 - 2762      
      DATA bCODE(61) /B'010100110101'/    !POLY4A 46 - 1333 
      DATA bCODE(62) /B'010100110101'/    !POLY4A 25 - 1333    
      DATA bCODE(63) /B'011010101001'/    !POLY4A 36 - 1705
      DATA bCODE(64) /B'011010101001'/    !POLY4A 15 - 1705
      DATA bCODE(65) /B'100101010110'/    !POLY4A 35 - 2390
      DATA bCODE(66) /B'100101010110'/    !POLY4A 16 - 2390
      DATA bCODE(67) /B'110001100011'/    !POLY4A 34 - 3171
      DATA bCODE(68) /B'110001100011'/    !POLY4A 12 - 3171
      DATA bCODE(69) /B'001110011100'/    !POLY4A 23 - 0924
      DATA bCODE(70) /B'001110011100'/    !POLY4A 14 - 0924
      !POLY4B
      DATA bCODE(71) /B'011101010001'/    !POLY4B 45 - 1873
      DATA bCODE(72) /B'011101010001'/    !POLY4B 26 - 1873
      DATA bCODE(73) /B'100010101110'/    !POLY4B 46 - 2222
      DATA bCODE(74) /B'100010101110'/    !POLY4B 25 - 2222
      DATA bCODE(75) /B'110001011100'/    !POLY4B 36 - 3148
      DATA bCODE(76) /B'110001011100'/    !POLY4B 15 - 3148
      DATA bCODE(77) /B'001110100011'/    !POLY4B 35 - 0931 
      DATA bCODE(78) /B'001110100011'/    !POLY4B 16 - 0931
      DATA bCODE(79) /B'101100001101'/    !POLY4B 34 - 2829
      DATA bCODE(80) /B'101100001101'/    !POLY4B 12 - 2829
      DATA bCODE(81) /B'010011110010'/    !POLY4B 23 - 1266
      DATA bCODE(82) /B'010011110010'/    !POLY4B 14 - 1266
      !----------------------------------------------------!
      !POLYC
      DATA bCODE(83)  /B'100110010001'/    !POLYC R+.4
      DATA bCODE(84)  /B'100010110001'/    !POLYC R+.7   
      DATA bCODE(85)  /B'100010011001'/    !POLYC R+.9
      DATA bCODE(86)  /B'100011010001'/    !POLYC R+.6   
                        
      DATA bCODE(87)  /B'110010010001'/    !POLYC R-.2
      DATA bCODE(88)  /B'101010010001'/    !POLYC R-.3 
      DATA bCODE(89)  /B'100010010011'/    !POLYC R-.11
      DATA bCODE(90)  /B'100010010101'/    !POLYC R-.10
                        
      DATA bCODE(91)  /B'110100001010'/    !POLYC S+.1
      DATA bCODE(92)  /B'011100001010'/    !POLYC S+.3
      DATA bCODE(93)  /B'010100011010'/    !POLYC S+.8
      DATA bCODE(94)  /B'010100101010'/    !POLYC S+.7
                      
      DATA bCODE(95)  /B'010110001010'/    !POLYC S-.5
      DATA bCODE(96)  /B'010100001110'/    !POLYC S-.10
      DATA bCODE(97)  /B'010100001011'/    !POLYC S-.12
      DATA bCODE(98)  /B'010101001010'/    !POLYC S-.6
                        
      DATA bCODE(99)  /B'001011100100'/    !POLYC T+.5
      DATA bCODE(100) /B'001101100100'/    !POLYC T+.4
      DATA bCODE(101) /B'101001100100'/    !POLYC T+.1
      DATA bCODE(102) /B'011001100100'/    !POLYC T+.2
                        
      DATA bCODE(103) /B'001001100101'/    !POLYC T-.12
      DATA bCODE(104) /B'001001101100'/    !POLYC T-.9
      DATA bCODE(105) /B'001001110100'/    !POLYC T-.8
      DATA bCODE(106) /B'001001100110'/    !POLYC T-.11
      !----------------------------------------------------!

      !----------------------------------------------------!
      !  RETRIEVING POLYHEDRA FAMILY (1-5) FROM A 1-58 CODE!
      !----------------------------------------------------!
       GetPolyhedraType(01:08)  = 1 !Type1=TETRA
       GetPolyhedraType(09:20)  = 2 !Type2=PENTA       
       GetPolyhedraType(21:44)  = 3 !Type2=POLY3
       GetPolyhedraType(45:50)  = 4 !Type2=HEXAE
       GetPolyhedraType(51:58)  = 5 !Type2=POLY4 
       GetPolyhedraType(59:70)  = 5 !Type2=POLY4A 
       GetPolyhedraType(71:82)  = 5 !Type2=POLY4B              
       GetPolyhedraType(83:106) = 6 !Type2=POLYC

       GetNumTria(1)           = 1 !Type1=TETRA
       GetNumTria(2)           = 2 !Type2=PENTA       
       GetNumTria(3)           = 3 !Type2=POLY3
       GetNumTria(4)           = 2 !Type2=HEXAE
       GetNumTria(5)           = 4 !Type2=POLY4
       GetNumTria(6)           = 4 !Type2=POLY4A
       GetNumTria(7)           = 4 !Type2=POLY4B             
       GetNumTria(8)           = 4 !Type2=POLYC       
       
       Gtria(:, 1, 1) = (/1,2,3/)  !TETRA (1 triangle )

       Gtria(:, 1, 2) = (/1,2,3/)  !PENTA (2 triangles)
       Gtria(:, 2, 2) = (/1,3,4/)       

       Gtria(:, 1, 3) = (/2,3,4/)  !POLY3 (3 triangles)
       Gtria(:, 2, 3) = (/1,2,4/)                      
       Gtria(:, 3, 3) = (/1,4,5/)                             

       Gtria(:, 1, 4) = (/1,2,3/)  !HEXAE (2 traingles)
       Gtria(:, 2, 4) = (/1,3,4/) 
       
       Gtria(:, 1, 5) = (/1,6,5/)  !POLY4 (4 triangles)
       Gtria(:, 2, 5) = (/1,5,4/)                      
       Gtria(:, 3, 5) = (/1,3,4/)                             
       Gtria(:, 4, 5) = (/1,2,3/)

       Gtria(:, 1, 6) = (/2,3,4/)  !POLY4A (4 triangles)
       Gtria(:, 2, 6) = (/2,4,5/)                      
       Gtria(:, 3, 6) = (/2,5,1/)                             
       Gtria(:, 4, 6) = (/1,5,6/)
       
       Gtria(:, 1, 7) = (/2,3,4/)  !POLY4B (4 triangles)
       Gtria(:, 2, 7) = (/2,4,5/)                      
       Gtria(:, 3, 7) = (/2,5,1/)                             
       Gtria(:, 4, 7) = (/1,5,6/)
              
       Gtria(:, 1, 8) = (/1,5,4/)  !POLYC (4 triangles)
       Gtria(:, 2, 8) = (/1,5,6/)                      
       Gtria(:, 3, 8) = (/1,6,2/)                             
       Gtria(:, 4, 8) = (/2,3,6/)                                 
       
      !----------------------------------------------------!
      !  COMBINATIONS NAMES                                !
      !----------------------------------------------------!
      !TETRA
      StrCODE( 1) ='TETRA 1       '
      StrCODE( 2) ='TETRA 3       '
      StrCODE( 3) ='TETRA 6       '
      StrCODE( 4) ='TETRA 8       '
      StrCODE( 5) ='TETRA 2       '
      StrCODE( 6) ='TETRA 4       '
      StrCODE( 7) ='TETRA 5       '
      StrCODE( 8) ='TETRA 7       '
      !----------------------------------------------------!
      !PENTA
      StrCODE( 9) ='PENTA 1-2     '
      StrCODE(10) ='PENTA 1-4     '
      StrCODE(11) ='PENTA 1-5     '
      StrCODE(12) ='PENTA 2-3     '
      StrCODE(13) ='PENTA 3-4     '
      StrCODE(14) ='PENTA 3-7     '
      StrCODE(15) ='PENTA 2-6     '
      StrCODE(16) ='PENTA 5-6     '
      StrCODE(17) ='PENTA 6-7     '
      StrCODE(18) ='PENTA 4-8     '
      StrCODE(19) ='PENTA 5-8     '
      StrCODE(20) ='PENTA 7-8     '
      !----------------------------------------------------!
      !POLY3
      StrCODE(21) ='POLY3 1-4-3   '
      StrCODE(22) ='POLY3 2-1-4   '
      StrCODE(23) ='POLY3 3-2-1   '
      StrCODE(24) ='POLY3 4-3-2   '
      StrCODE(25) ='POLY3 3-4-8   '
      StrCODE(26) ='POLY3 4-8-7   '
      StrCODE(27) ='POLY3 7-3-4   '
      StrCODE(28) ='POLY3 8-7-3   '
      StrCODE(29) ='POLY3 5-6-7   '
      StrCODE(30) ='POLY3 6-7-8   '
      StrCODE(31) ='POLY3 7-8-5   '
      StrCODE(32) ='POLY3 8-5-6   '
      StrCODE(33) ='POLY3 1-2-6   '
      StrCODE(34) ='POLY3 2-6-5   '
      StrCODE(35) ='POLY3 5-1-2   '
      StrCODE(36) ='POLY3 6-5-1   '
      StrCODE(37) ='POLY3 2-3-7   '
      StrCODE(38) ='POLY3 3-7-6   '
      StrCODE(39) ='POLY3 6-2-3   '
      StrCODE(40) ='POLY3 7-6-2   '
      StrCODE(41) ='POLY3 1-5-8   '
      StrCODE(42) ='POLY3 4-1-5   '
      StrCODE(43) ='POLY3 5-8-4   '
      StrCODE(44) ='POLY3 8-4-1   '
      !----------------------------------------------------!
      !HEXAE
      StrCODE(45) ='HEXAE R+      '
      StrCODE(46) ='HEXAE R-      '      
      StrCODE(47) ='HEXAE S+      '
      StrCODE(48) ='HEXAE S-      '      
      StrCODE(49) ='HEXAE T+      '
      StrCODE(50) ='HEXAE T-      '      
      !----------------------------------------------------!
      !POLY4
      StrCODE(51) ='POLY4 1-2-6-3 ' 
      StrCODE(52) ='POLY4 4-8-7-5 '      
      StrCODE(53) ='POLY4 3-4-8-1 ' 
      StrCODE(54) ='POLY4 5-6-7-5 ' 
      StrCODE(55) ='POLY4 4-3-2-7 '           
      StrCODE(56) ='POLY4 2-3-7-4 '      
      StrCODE(57) ='POLY4 4-1-2-5 ' 
      StrCODE(58) ='POLY4 8-7-3-6 ' 
      !----------------------------------------------------!
      !POLY4A
      StrCODE(59) ='POLY4A 4-5    ' 
      StrCODE(60) ='POLY4A 2-6    '      
      StrCODE(61) ='POLY4A 4-6    ' 
      StrCODE(62) ='POLY4A 2-5    ' 
      StrCODE(63) ='POLY4A 3-6    '           
      StrCODE(64) ='POLY4A 1-5    '      
      StrCODE(65) ='POLY4A 3-5    ' 
      StrCODE(66) ='POLY4A 1-6    ' 
      StrCODE(67) ='POLY4A 3-4    '           
      StrCODE(68) ='POLY4A 1-2    '      
      StrCODE(69) ='POLY4A 2-3    ' 
      StrCODE(70) ='POLY4A 1-4    '   
      !----------------------------------------------------!
      !POLY4B
      StrCODE(71) ='POLY4B 4-5    ' 
      StrCODE(72) ='POLY4B 2-6    '      
      StrCODE(73) ='POLY4B 4-6    ' 
      StrCODE(74) ='POLY4B 2-5    ' 
      StrCODE(75) ='POLY4B 3-6    '           
      StrCODE(76) ='POLY4B 1-5    '      
      StrCODE(77) ='POLY4B 3-5    ' 
      StrCODE(78) ='POLY4B 1-6    ' 
      StrCODE(79) ='POLY4B 3-4    '           
      StrCODE(80) ='POLY4B 1-2    '      
      StrCODE(81) ='POLY4B 2-3    ' 
      StrCODE(82) ='POLY4B 1-4    '              
      !----------------------------------------------------!
      !POLYC
      StrCODE(83)  ='POLYC R+.04   '
      StrCODE(84)  ='POLYC R+.07   '
      StrCODE(85)  ='POLYC R+.09   '
      StrCODE(86)  ='POLYC R+.06   '
      StrCODE(87)  ='POLYC R-.02   ' 
      StrCODE(88)  ='POLYC R-.03   '       
      StrCODE(89)  ='POLYC R-.11   '
      StrCODE(90)  ='POLYC R-.10   ' 
      StrCODE(91)  ='POLYC S+.01   '
      StrCODE(92)  ='POLYC S+.03   '
      StrCODE(93)  ='POLYC S+.08   '
      StrCODE(94)  ='POLYC S+.07   ' 
      StrCODE(95)  ='POLYC S-.05   '
      StrCODE(96)  ='POLYC S-.10   '
      StrCODE(97)  ='POLYC S-.12   ' 
      StrCODE(98)  ='POLYC S-.06   '  
      StrCODE(99)  ='POLYC T+.05   '
      StrCODE(100) ='POLYC T+.04   '
      StrCODE(101) ='POLYC T+.01   '
      StrCODE(102) ='POLYC T+.02   ' 
      StrCODE(103) ='POLYC T-.12   '
      StrCODE(104) ='POLYC T-.09   '
      StrCODE(105) ='POLYC T-.08   ' 
      StrCODE(106) ='POLYC T-.11   '                   
      
      !----------------------------------------------------!
      !  SUBVOLUME 'G'RAPH : 'F'ACES                       !
      !----------------------------------------------------!
      !TETRA
      Gface(:, 1)=(/1,6,4,0,0,0/)
      Gface(:, 2)=(/2,1,5,0,0,0/)
      Gface(:, 3)=(/4,3,5,0,0,0/)
      Gface(:, 4)=(/2,3,6,0,0,0/)
      Gface(:, 5)=(/5,1,4,0,0,0/)
      Gface(:, 6)=(/6,1,2,0,0,0/)
      Gface(:, 7)=(/6,3,4,0,0,0/)
      Gface(:, 8)=(/5,3,2,0,0,0/)
      !----------------------------------------------------!
      !PENTA
      Gface(:, 9)=(/6,5,4,1,0,0/)
      Gface(:,10)=(/4,2,1,6,0,0/)
      Gface(:,11)=(/1,3,6,4,0,0/)
      Gface(:,12)=(/4,2,5,1,0,0/)
      Gface(:,13)=(/5,6,2,1,0,0/)
      Gface(:,14)=(/1,3,5,2,0,0/)
      Gface(:,15)=(/1,3,4,5,0,0/)
      Gface(:,16)=(/6,5,3,4,0,0/)
      Gface(:,17)=(/4,2,3,5,0,0/)
      Gface(:,18)=(/1,3,2,6,0,0/)
      Gface(:,19)=(/4,2,6,3,0,0/)
      Gface(:,20)=(/5,6,3,2,0,0/)
      !----------------------------------------------------!
      !POLY3
      Gface(:,21)=(/1,5,2,6,4,0/)
      Gface(:,22)=(/1,2,6,4,5,0/)
      Gface(:,23)=(/1,6,4,5,2,0/)
      Gface(:,24)=(/1,4,5,2,6,0/)
      Gface(:,25)=(/2,3,6,1,5,0/)
      Gface(:,26)=(/2,5,3,6,1,0/)
      Gface(:,27)=(/2,6,1,5,3,0/)
      Gface(:,28)=(/2,1,5,3,6,0/)
      Gface(:,29)=(/3,2,5,4,6,0/)
      Gface(:,30)=(/3,6,2,5,4,0/)
      Gface(:,31)=(/3,4,6,2,5,0/)
      Gface(:,32)=(/3,5,4,6,2,0/)
      Gface(:,33)=(/4,3,5,1,6,0/)
      Gface(:,34)=(/4,6,3,5,1,0/)
      Gface(:,35)=(/4,5,1,6,3,0/)
      Gface(:,36)=(/4,1,6,3,5,0/)
      Gface(:,37)=(/5,3,2,1,4,0/)
      Gface(:,38)=(/5,4,3,2,1,0/)
      Gface(:,39)=(/5,2,1,4,3,0/)
      Gface(:,40)=(/5,1,4,3,2,0/)
      Gface(:,41)=(/6,2,3,4,1,0/)
      Gface(:,42)=(/6,3,4,1,2,0/)
      Gface(:,43)=(/6,1,2,3,4,0/)
      Gface(:,44)=(/6,4,1,2,3,0/)
      !----------------------------------------------------!
      !HEXA
      Gface(:,45)=(/5,3,2,1,4,0/)
      Gface(:,46)=(/6,3,4,1,2,0/)
      Gface(:,47)=(/4,3,5,1,6,0/)
      Gface(:,48)=(/2,6,1,5,3,0/)
      Gface(:,49)=(/3,4,6,2,5,0/)
      Gface(:,50)=(/1,4,5,2,6,0/)
      !----------------------------------------------------!
      !POLY4
      Gface(:,51)=(/6,5,1,4,2,3/)
      Gface(:,52)=(/1,3,6,2,4,5/)     
      Gface(:,53)=(/5,6,1,2,4,3/)
      Gface(:,54)=(/6,5,4,3,1,2/)
      Gface(:,55)=(/6,5,2,1,3,4/)
      Gface(:,56)=(/4,2,1,5,6,3/)     
      Gface(:,57)=(/2,4,1,6,5,3/)
      Gface(:,58)=(/6,5,3,2,4,1/)
      !----------------------------------------------------!
      !POLY4A
      Gface(:,59)=(/6,3,5,2,1,4/)
      Gface(:,60)=(/4,1,2,5,3,6/)     
      Gface(:,61)=(/5,1,6,2,3,4/)
      Gface(:,62)=(/4,3,2,6,1,5/)
      Gface(:,63)=(/5,4,6,1,2,3/)
      Gface(:,64)=(/6,4,5,3,2,1/)     
      Gface(:,65)=(/1,4,3,6,2,5/)
      Gface(:,66)=(/3,4,1,5,2,6/)
      Gface(:,67)=(/1,6,3,2,5,4/)
      Gface(:,68)=(/4,5,2,3,5,1/)     
      Gface(:,69)=(/4,5,2,1,5,3/)
      Gface(:,70)=(/2,6,4,3,5,1/)      
      !----------------------------------------------------!
      !POLY4B
      Gface(:,71)=(/6,1,5,2,3,4/)
      Gface(:,72)=(/5,1,6,4,3,2/)     
      Gface(:,73)=(/2,1,4,5,3,6/)
      Gface(:,74)=(/4,1,2,6,3,5/)
      Gface(:,75)=(/1,4,3,5,2,6/)
      Gface(:,76)=(/3,4,1,6,2,5/)     
      Gface(:,77)=(/6,4,5,1,2,3/)
      Gface(:,78)=(/5,4,6,3,2,1/)
      Gface(:,79)=(/1,5,3,2,6,4/)
      Gface(:,80)=(/4,6,2,3,5,1/)     
      Gface(:,81)=(/1,6,3,4,5,2/)
      Gface(:,82)=(/3,6,1,2,5,4/)      
      !----------------------------------------------------!
      !POLYC
      Gface(:,83) =(/5,3,2,4,1,0/)     
      Gface(:,84) =(/5,2,1,3,4,0/)     
      Gface(:,85) =(/5,1,4,2,3,0/)     
      Gface(:,86) =(/5,4,3,1,2,0/)                       
      Gface(:,87) =(/6,3,2,4,1,0/)     
      Gface(:,88) =(/6,2,1,3,4,0/)     
      Gface(:,89) =(/6,1,4,2,3,0/)     
      Gface(:,90) =(/6,4,3,1,2,0/)      
      Gface(:,91) =(/4,3,5,6,1,0/)     
      Gface(:,92) =(/4,5,1,3,6,0/)     
      Gface(:,93) =(/4,1,6,5,3,0/)     
      Gface(:,94) =(/4,6,3,1,5,0/) 
      Gface(:,95) =(/2,3,5,6,1,0/)     
      Gface(:,96) =(/2,5,1,3,6,0/)     
      Gface(:,97) =(/2,1,6,5,3,0/)     
      Gface(:,98) =(/2,6,3,1,5,0/) 
      Gface(:,99) =(/1,4,5,6,2,0/)     
      Gface(:,100)=(/1,5,2,4,6,0/)      
      Gface(:,101)=(/1,2,6,5,4,0/)      
      Gface(:,102)=(/1,6,4,2,5,0/)  
      Gface(:,103)=(/3,4,5,6,2,0/)      
      Gface(:,104)=(/3,6,4,2,5,0/)      
      Gface(:,105)=(/3,2,6,5,4,0/)      
      Gface(:,106)=(/3,5,2,4,6,0/)                   
      !--------------------------- -------------------------!
      !  SUBVOLUME 'G'RAPH : 'N'NUMBERR of 'Pt'POINT        !  !enables to have number of point on local face of a given polyhedron. Check graph index1-6 corresponds to Fa,Fb,Fc,Fd,Fe,Ff
      !----------------------------------------------------!  
      DO I=01,08 ;GNPt(:,I)  = (/3,3,3,0,0,0/);ENDDO  
      DO I=09,20 ;GNPt(:,I)  = (/3,3,4,4,0,0/);ENDDO  
      DO I=21,44 ;GNPt(:,I)  = (/5,3,4,4,3,0/);ENDDO  
      DO I=45,50 ;GNPt(:,I)  = (/4,4,4,4,4,0/);ENDDO  
      DO I=51,58 ;GNPt(:,I)  = (/3,5,5,5,3,3/);ENDDO  
      DO I=59,70 ;GNPt(:,I)  = (/3,4,5,3,4,5/);ENDDO  
      DO I=71,82 ;GNPt(:,I)  = (/3,4,5,3,4,5/);ENDDO                
      DO I=83,106;GNPt(:,I)  = (/6,4,4,4,3,3/);ENDDO  
      
      DO I=01,08;GNnod(:,I)  = (/1,1,1,0,0,0/);ENDDO  
      DO I=09,20;GNnod(:,I)  = (/1,1,2,2,0,0/);ENDDO  
      DO I=21,44;GNnod(:,I)  = (/3,1,2,2,1,0/);ENDDO  
      DO I=45,50;GNnod(:,I)  = (/4,2,2,2,2,0/);ENDDO  
      DO I=51,58;GNnod(:,I)  = (/1,3,3,3,1,1/);ENDDO  
      DO I=59,70;GNnod(:,I)  = (/1,2,3,1,2,3/);ENDDO  
      DO I=71,82;GNnod(:,I)  = (/1,2,3,1,2,3/);ENDDO  
      DO I=83,106;GNnod(:,I) = (/4,2,2,2,2,0/);ENDDO                                  
      !----------------------------------------------------!
      !  SUBVOLUME 'G'RAPH : 'C'ORNER                      !  !EDGE id => intersection node id (CORNER).
      !----------------------------------------------------!  dernier indice = premier, puis utikser Gcorner dans i22anim en fonction de %SECID_Cell() = > plus de b-lock E-LSEIF
      !TETRA
      Gcorner(:, 1)=(/1,2,3,1,0,0,0/)      * -1
      Gcorner(:, 2)=(/6,5,4,6,0,0,0/)      * -1
      Gcorner(:, 3)=(/7,8,9,7,0,0,0/)      * -1
      Gcorner(:, 4)=(/10,12,11,10,0,0,0/)  * -1
      Gcorner(:, 5)=(/7,4,1,7,0,0,0/)
      Gcorner(:, 6)=(/10,2,5,10,0,0,0/)
      Gcorner(:, 7)=(/3,11,8,3,0,0,0/)
      Gcorner(:, 8)=(/6,9,12,6,0,0,0/)
      !----------------------------------------------------!
      !PENTA
      Gcorner(:, 9)=(/-2,-3,+7,+4,-2,0,0/)
      Gcorner(:,10)=(/-3,-1,+5,+10,-3,0,0/)
      Gcorner(:,11)=(/-1,-2,+11,+8,-1,0,0/)
      Gcorner(:,12)=(/+1,+7,-6,-5,+1,0,0/)
      Gcorner(:,13)=(/-4,-6,+10,+2,-4,0,0/)
      Gcorner(:,14)=(/-5,-4,+9,+12,-5,0,0/)
      Gcorner(:,15)=(/+4,+1,-8,-9,+4,0,0/)
      Gcorner(:,16)=(/+3,+11,-9,-7,+3,0,0/)
      Gcorner(:,17)=(/-7,-8,+12,+6,-7,0,0/)
      Gcorner(:,18)=(/+2,+5,-12,-11,+2,0,0/)
      Gcorner(:,19)=(/+8,+3,-10,-12,+8,0,0/)
      Gcorner(:,20)=(/+6,+9,-11,-10,+6,0,0/)
      !----------------------------------------------------!
      !POLY3
      Gcorner(:,21)=(/-3,-1,-4,-6,+10,-3,0/)
      Gcorner(:,22)=(/+7,+4,+5,+10,-3,+7,0/)
      Gcorner(:,23)=(/-6,-5,-2,-3,+7,-6,0/)
      Gcorner(:,24)=(/+10,+2,+1,+7,-6,+10,0/)
      Gcorner(:,25)=(/-4,-6,-12,-11,+2,-4,0/)
      Gcorner(:,26)=(/+2,+5,+6,+9,-11,+2,0/)
      Gcorner(:,27)=(/+9,+12,+10,+2,-4,+9,0/)
      Gcorner(:,28)=(/-11,-10,-5,-4,+9,-11,0/)
      Gcorner(:,29)=(/+3,+11,+12,+6,-7,+3,0/)
      Gcorner(:,30)=(/-7,-8,-11,-10,+6,-7,0/)
      Gcorner(:,31)=(/+6,+9,+8,+3,-10,+6,0/)
      Gcorner(:,32)=(/-10,-12,-9,-7,+3,-10,0/)
      Gcorner(:,33)=(/-2,-3,-8,-9,+4,-2,0/)
      Gcorner(:,34)=(/+4,+1,+3,+11,-9,+4,0/)
      Gcorner(:,35)=(/+11,8,+7,+4,-2,+11,0/)
      Gcorner(:,36)=(/-9,-7,-1,-2,+11,-9,0/)
      Gcorner(:,37)=(/+1,+7,+9,+12,-5,+1,0/)
      Gcorner(:,38)=(/-5,-4,-7,-8,+12,-5,0/)
      Gcorner(:,39)=(/-8,-9,-6,-5,+1,-8,0/)
      Gcorner(:,40)=(/+12,+6,+4,+1,-8,+12,0/)
      Gcorner(:,41)=(/-1,-2,-10,-12,+8,-1,0/)
      Gcorner(:,42)=(/+5,+10,+11,+8,-1,+5,0/)
      Gcorner(:,43)=(/+8,+3,+2,+5,-12,+8,0/)
      Gcorner(:,44)=(/-12,-11,-3,-1,+5,-12,0/)
      !----------------------------------------------------!
      !HEXAE
      Gcorner(:,45)=(/-8 ,+12,-5,+1,-8 ,0 ,0/)
      Gcorner(:,46)=(/-12,+8 ,-1,+5,-12,0 ,0/)
      Gcorner(:,47)=(/+11,-9,+4,-2,+11,0,0/)
      Gcorner(:,48)=(/-11,+2,-4,+9,-11,0,0/)
      Gcorner(:,49)=(/-7,+3,-10,+6,-7,0,0/)
      Gcorner(:,50)=(/-3,+7,-6,+10,-3,0,0/)
      !----------------------------------------------------!
      !POLY4
      Gcorner(:,51)=(/-02,-03,-08,-09,-06,-05,-02/) !ok
      Gcorner(:,52)=(/+02,+05,+06,+09,+08,+03,+02/) !ok
      Gcorner(:,53)=(/-04,-06,-12,-11,-03,-01,-04/) !ok
      Gcorner(:,54)=(/+03,+11,+12,+06,+04,+01,+03/) !ok
      Gcorner(:,55)=(/+10,+02,+01,+07,+09,+12,+10/) !ok
      Gcorner(:,56)=(/+01,+07,+09,+12,+10,+02,+01/) !ok
      Gcorner(:,57)=(/+05,+10,+11,+08,+07,+04,+05/) !ok
      Gcorner(:,58)=(/-11,-10,-05,-04,-07,-08,-11/) !ok
      !----------------------------------------------------!
      !POLY4A
      Gcorner(:,59)=(/+11,-09,-06,-05,+01,+03,0/)    
      Gcorner(:,60)=(/-01,+05,+06,+09,-11,-03,0/)      
      Gcorner(:,61)=(/+04,-02,-10,-12,+08,+07,0/)    
      Gcorner(:,62)=(/-08,+12,+10,+02,-04,-07,0/)                    
      Gcorner(:,63)=(/-07,+03,+02,+05,-12,-09,0/)    
      Gcorner(:,64)=(/-03,+07,+09,+12,-05,-02,0/)      
      Gcorner(:,65)=(/+01,-08,-11,-10,+06,+04,0/)    
      Gcorner(:,66)=(/+08,-01,-04,-06,+10,+11,0/)                    
      Gcorner(:,67)=(/-02,+11,+12,+06,-07,-01,0/)    
      Gcorner(:,68)=(/+07,-06,-12,-11,+02,+01,0/)    
      Gcorner(:,69)=(/+03,-10,-05,-04,+09,+08,0/)    
      Gcorner(:,70)=(/+10,-03,-08,-09,+04,+05,0/)    
      !----------------------------------------------------!
      !POLY4B
      Gcorner(:,71)=(/-02,+04,+06,+12,-08,-03,0/)          
      Gcorner(:,72)=(/-04,+02,+03,+08,-12,-06,0/)    
      Gcorner(:,73)=(/+05,-01,-07,-09,+11,+10,0/)      
      Gcorner(:,74)=(/+01,-05,-10,-11,+09,+07,0/)    
      Gcorner(:,75)=(/-01,+08,+09,+06,-10,-02,0/)                    
      Gcorner(:,76)=(/-08,+01,+02,+10,-06,-09,0/)    
      Gcorner(:,77)=(/+03,-07,-04,-05,+12,+11,0/)      
      Gcorner(:,78)=(/+07,-03,-11,-12,+05,+04,0/)    
      Gcorner(:,79)=(/+04,-09,-12,-10,+03,+01,0/)                    
      Gcorner(:,80)=(/-03,+10,+12,+09,-04,-01,0/)    
      Gcorner(:,81)=(/+02,-11,-08,-07,+06,+05,0/)    
      Gcorner(:,82)=(/+11,-02,-05,-06,+07,+08,0/)    
      !----------------------------------------------------!
      !POLYC
      Gcorner(:,83) =(/-08,+12,-05,+01,+04,-04,0/)    !POLYC R+.4
      Gcorner(:,84) =(/+12,-05,+01,-08,-07,+07,0/)    !POLYC R+.7    
      Gcorner(:,85) =(/-05,+01,-08,+12,+09,-09,0/)    !POLYC R+.9
      Gcorner(:,86) =(/+01,-08,+12,-05,-06,+06,0/)    !POLYC R+.6                  
      Gcorner(:,87) =(/+08,-12,+05,-01,-02,+02,0/)    !POLYC R-.2
      Gcorner(:,88) =(/-12,+05,-01,+08,+03,-03,0/)    !POLYC R-.3    
      Gcorner(:,89) =(/+05,-01,+08,-12,-11,+11,0/)    !POLYC R-.11
      Gcorner(:,90) =(/-01,+08,-12,+05,+10,-10,0/)    !POLYC R-.10                 
      Gcorner(:,91) =(/+11,-09,+04,-02,-01,+01,0/)    !POLYC S+.1
      Gcorner(:,92) =(/-09,+04,-02,+11,+03,-03,0/)    !POLYC S+.3
      Gcorner(:,93) =(/+04,-02,+11,-09,-08,+08,0/)    !POLYC S+.8
      Gcorner(:,94) =(/-02,+11,-09,+04,+07,-07,0/)    !POLYC S+.7
      Gcorner(:,95) =(/-11,+09,-04,+02,+05,-05,0/)    !POLYC S-.5
      Gcorner(:,96) =(/+09,-04,+02,-11,-10,+10,0/)    !POLYC S-.10
      Gcorner(:,97) =(/-04,+02,-11,+09,+12,-12,0/)    !POLYC S-.12
      Gcorner(:,98) =(/+02,-11,+09,-04,-04,+06,0/)    !POLYC S-.6
      Gcorner(:,99) =(/-03,+07,-06,+10,+05,-05,0/)    !POLYC T+.5
      Gcorner(:,100)=(/+07,-06,+10,-03,-04,+04,0/)    !POLYC T+.4
      Gcorner(:,101)=(/-06,+10,-03,+07,+01,-01,0/)    !POLYC T+.1
      Gcorner(:,102)=(/+10,-03,+07,-06,-02,+02,0/)    !POLYC T+.2
      Gcorner(:,103)=(/+03,-07,+06,-10,-12,+12,0/)    !POLYC T-.12
      Gcorner(:,104)=(/-07,+06,-10,+03,+09,-09,0/)    !POLYC T-.9
      Gcorner(:,105)=(/+06,-10,+03,-07,-08,+08,0/)    !POLYC T-.8
      Gcorner(:,106)=(/-10,+03,-07,+06,+11,-11,0/)    !POLYC T-.11     
      
      !----------------------------------------------------!
      !  SUBVOLUME 'G'RAPH : 'main 'N'ODES               !
      !----------------------------------------------------!
      !TETRA
      Gnode(:, 1)=(/1,0,0,0/) ; bGnode( 1)=B'10000000'
      Gnode(:, 2)=(/3,0,0,0/) ; bGnode( 2)=B'00100000'
      Gnode(:, 3)=(/6,0,0,0/) ; bGnode( 3)=B'00000100'
      Gnode(:, 4)=(/8,0,0,0/) ; bGnode( 4)=B'00000001'
      Gnode(:, 5)=(/2,0,0,0/) ; bGnode( 5)=B'01000000'
      Gnode(:, 6)=(/4,0,0,0/) ; bGnode( 6)=B'00010000'
      Gnode(:, 7)=(/5,0,0,0/) ; bGnode( 7)=B'00001000'
      Gnode(:, 8)=(/7,0,0,0/) ; bGnode( 8)=B'00000010'
      !----------------------------------------------------!
      !PENTA
      Gnode(:, 9)=(/1,2,0,0/) ; bGnode( 9)=B'11000000'
      Gnode(:,10)=(/1,4,0,0/) ; bGnode(10)=B'10010000'
      Gnode(:,11)=(/1,5,0,0/) ; bGnode(11)=B'10001000'
      Gnode(:,12)=(/2,3,0,0/) ; bGnode(12)=B'01100000'
      Gnode(:,13)=(/3,4,0,0/) ; bGnode(13)=B'00110000'
      Gnode(:,14)=(/3,7,0,0/) ; bGnode(14)=B'00100010'
      Gnode(:,15)=(/2,6,0,0/) ; bGnode(15)=B'01000100'
      Gnode(:,16)=(/5,6,0,0/) ; bGnode(16)=B'00001100'
      Gnode(:,17)=(/6,7,0,0/) ; bGnode(17)=B'00000110'
      Gnode(:,18)=(/4,8,0,0/) ; bGnode(18)=B'00010001'
      Gnode(:,19)=(/5,8,0,0/) ; bGnode(19)=B'00001001'
      Gnode(:,20)=(/7,8,0,0/) ; bGnode(20)=B'00000011'
      !----------------------------------------------------!
      !POLY3
      Gnode(:,21) =(/1,4,3,0/) ; bGnode(21)=B'10110000'
      Gnode(:,22) =(/2,1,4,0/) ; bGnode(22)=B'11010000'
      Gnode(:,23) =(/3,2,1,0/) ; bGnode(23)=B'11100000'
      Gnode(:,24) =(/4,3,2,0/) ; bGnode(24)=B'01110000'
      Gnode(:,25) =(/3,4,8,0/) ; bGnode(25)=B'00110001'
      Gnode(:,26) =(/4,8,7,0/) ; bGnode(26)=B'00010011'
      Gnode(:,27) =(/7,3,4,0/) ; bGnode(27)=B'00110010'
      Gnode(:,28) =(/8,7,3,0/) ; bGnode(28)=B'00100011'      
      Gnode(:,29) =(/5,6,7,0/) ; bGnode(29)=B'00001110'
      Gnode(:,30) =(/6,7,8,0/) ; bGnode(30)=B'00000111'
      Gnode(:,31) =(/7,8,5,0/) ; bGnode(31)=B'00001011'
      Gnode(:,32) =(/8,5,6,0/) ; bGnode(32)=B'00001101'      
      Gnode(:,33) =(/1,2,6,0/) ; bGnode(33)=B'11000100'
      Gnode(:,34) =(/2,6,5,0/) ; bGnode(34)=B'01001100'
      Gnode(:,35) =(/5,1,2,0/) ; bGnode(35)=B'11001000'
      Gnode(:,36) =(/6,5,1,0/) ; bGnode(36)=B'10001100'
      Gnode(:,37) =(/2,3,7,0/) ; bGnode(37)=B'01100010'
      Gnode(:,38) =(/3,7,6,0/) ; bGnode(38)=B'00100110'
      Gnode(:,39) =(/6,2,3,0/) ; bGnode(39)=B'01100100'
      Gnode(:,40) =(/7,6,2,0/) ; bGnode(40)=B'01000110'
      Gnode(:,41) =(/1,5,8,0/) ; bGnode(41)=B'10001001'
      Gnode(:,42) =(/4,1,5,0/) ; bGnode(42)=B'10011000'
      Gnode(:,43) =(/5,8,4,0/) ; bGnode(43)=B'00011001'
      Gnode(:,44) =(/8,4,1,0/) ; bGnode(44)=B'10010001'
      !----------------------------------------------------!
      !HEXAE
      Gnode(:,45)=(/6,7,3,2/) ; bGnode(45)=B'01100110'
      Gnode(:,46)=(/8,5,1,4/) ; bGnode(46)=B'10011001'
      Gnode(:,47)=(/5,6,2,1/) ; bGnode(47)=B'11001100'
      Gnode(:,48)=(/8,4,3,7/) ; bGnode(48)=B'00110011'
      Gnode(:,49)=(/6,5,8,7/) ; bGnode(49)=B'00001111'
      Gnode(:,50)=(/1,2,3,4/) ; bGnode(50)=B'11110000' 
      !----------------------------------------------------!
      !POLY4
      Gnode(:,51)=(/1,2,6,3/) ; bGnode(51)=B'11100100' 
      Gnode(:,52)=(/4,8,7,5/) ; bGnode(52)=B'00011011' 
      Gnode(:,53)=(/3,4,8,1/) ; bGnode(53)=B'10110001' 
      Gnode(:,54)=(/5,6,7,2/) ; bGnode(54)=B'01001110' 
      Gnode(:,55)=(/4,3,2,7/) ; bGnode(55)=B'01110010' 
      Gnode(:,56)=(/2,3,7,4/) ; bGnode(56)=B'01110010' 
      Gnode(:,57)=(/4,1,5,2/) ; bGnode(57)=B'11011000' 
      Gnode(:,58)=(/8,7,3,6/) ; bGnode(58)=B'00100111' 
      !----------------------------------------------------!
      !POLY4A
      Gnode(:,59)=(/5,6,2,3/) ; bGnode(59)=B'01101100' 
      Gnode(:,60)=(/1,4,8,7/) ; bGnode(60)=B'10010011'  
      Gnode(:,61)=(/2,1,5,8/) ; bGnode(61)=B'11001001' 
      Gnode(:,62)=(/6,7,3,4/) ; bGnode(62)=B'00110110'                                                                          
      Gnode(:,63)=(/6,5,8,4/) ; bGnode(63)=B'00011101' 
      Gnode(:,64)=(/1,2,3,7/) ; bGnode(64)=B'11100010' 
      Gnode(:,65)=(/2,6,7,8/) ; bGnode(65)=B'01000111' 
      Gnode(:,66)=(/5,1,4,3/) ; bGnode(66)=B'10111000'                                                      
      Gnode(:,67)=(/1,5,6,7/) ; bGnode(67)=B'10001110' 
      Gnode(:,68)=(/2,3,4,8/) ; bGnode(68)=B'01110001' 
      Gnode(:,69)=(/5,8,7,3/) ; bGnode(69)=B'00101011' 
      Gnode(:,70)=(/4,1,2,6/) ; bGnode(70)=B'11010100'                                                     
      !----------------------------------------------------!
      !POLY4B
      Gnode(:,71)=(/1,2,6,7/) ; bGnode(71)=B'11000110'   
      Gnode(:,72)=(/3,4,8,5/) ; bGnode(72)=B'00111001'   
      Gnode(:,73)=(/4,1,5,6/) ; bGnode(73)=B'10011100'   
      Gnode(:,74)=(/2,3,7,8/) ; bGnode(74)=B'01100011'                                               
      Gnode(:,75)=(/1,5,8,7/) ; bGnode(75)=B'10001011'   
      Gnode(:,76)=(/6,2,3,4/) ; bGnode(76)=B'01110100'   
      Gnode(:,77)=(/5,6,7,4/) ; bGnode(77)=B'00011110'   
      Gnode(:,78)=(/2,1,4,8/) ; bGnode(78)=B'11010001'                           
      Gnode(:,79)=(/2,6,5,8/) ; bGnode(79)=B'01001101'   
      Gnode(:,80)=(/1,4,3,7/) ; bGnode(80)=B'10110010'   
      Gnode(:,81)=(/4,8,7,6/) ; bGnode(81)=B'00010111'   
      Gnode(:,82)=(/5,1,2,3/) ; bGnode(82)=B'11101000'   
      !----------------------------------------------------!
      !POLYC
      Gnode(:,83) =(/6,7,3,2/) ; bGnode(83)  =B'01100110'          !POLYC R+.4
      Gnode(:,84) =(/7,3,2,6/) ; bGnode(84)  =B'01100110'          !POLYC R+.7    
      Gnode(:,85) =(/3,2,6,7/) ; bGnode(85)  =B'01100110'          !POLYC R+.9
      Gnode(:,86) =(/2,6,7,3/) ; bGnode(86)  =B'01100110'          !POLYC R+.6                                                                            
      Gnode(:,87) =(/5,8,4,1/) ; bGnode(87)  =B'10011001'          !POLYC R-.2
      Gnode(:,88) =(/8,4,1,5/) ; bGnode(88)  =B'10011001'          !POLYC R-.3 
      Gnode(:,89) =(/4,1,5,8/) ; bGnode(89)  =B'10011001'          !POLYC R-.11
      Gnode(:,90) =(/1,5,8,4/) ; bGnode(90)  =B'10011001'          !POLYC R-.10                                                       
      Gnode(:,91) =(/5,6,2,1/) ; bGnode(91)  =B'11001100'          !POLYC S+.1
      Gnode(:,92) =(/6,2,1,5/) ; bGnode(92)  =B'11001100'          !POLYC S+.3
      Gnode(:,93) =(/2,1,5,6/) ; bGnode(93)  =B'11001100'          !POLYC S+.8
      Gnode(:,94) =(/1,5,6,2/) ; bGnode(94)  =B'11001100'          !POLYC S+.7                                                       
      Gnode(:,95) =(/8,7,3,4/) ; bGnode(95)  =B'00110011'          !POLYC S-.5
      Gnode(:,96) =(/7,3,4,8/) ; bGnode(96)  =B'00110011'          !POLYC S-.10
      Gnode(:,97) =(/3,4,8,7/) ; bGnode(97)  =B'00110011'          !POLYC S-.12
      Gnode(:,98) =(/4,8,7,3/) ; bGnode(98)  =B'00110011'          !POLYC S-.6                                              
      Gnode(:,99) =(/1,2,3,4/) ; bGnode(99)  =B'11110000'          !POLYC T+.5
      Gnode(:,100)=(/2,3,4,1/) ; bGnode(100) =B'11110000'          !POLYC T+.4
      Gnode(:,101)=(/3,4,1,2/) ; bGnode(101) =B'11110000'          !POLYC T+.1
      Gnode(:,102)=(/4,1,2,3/) ; bGnode(102) =B'11110000'          !POLYC T+.2                          
      Gnode(:,103)=(/5,6,7,8/) ; bGnode(103) =B'00001111'          !POLYC T-.12
      Gnode(:,104)=(/8,5,6,7/) ; bGnode(104) =B'00001111'          !POLYC T-.9
      Gnode(:,105)=(/7,8,5,6/) ; bGnode(105) =B'00001111'          !POLYC T-.8
      Gnode(:,106)=(/6,7,8,5/) ; bGnode(106) =B'00001111'          !POLYC T-.11

      !----------------------------------------------------!
      !  NODE TAGS FOR EACH OF 58 COMBINATIONS             !
      !----------------------------------------------------!
                            !  1        10        20        30        40        50
                            !  |        |         |         |         |         |  
                            ! '1234567890123456789012345678901234567890123456789012345678'/ 
c      DATA b22CombiNode(01) /B'0000000000000000000000000000000000000000000000000000000000'/  !1,9,10,11,21,22,23,24,25,26,32,33,44,46,47,49,52,54,56,58
c      DATA b22CombiNode(02) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(03) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(04) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(05) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(06) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(07) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiNode(08) /B'0000000000000000000000000000000000000000000000000000000000'/                                    

c      DATA b22CombiEdge(01) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(02) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(03) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(04) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(05) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(06) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(07) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(08) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(09) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(10) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(11) /B'0000000000000000000000000000000000000000000000000000000000'/
c      DATA b22CombiEdge(12) /B'0000000000000000000000000000000000000000000000000000000000'/                    

C=======================================================================
C definitions from i22edges.inc
C=======================================================================

      !diagonal vectors
      iFACE(1:2,1,1)=(/1,3/); iFACE(1:2,2,1)=(/2,4/)   
      iFACE(1:2,1,2)=(/3,8/); iFACE(1:2,2,2)=(/4,7/) 
      iFACE(1:2,1,3)=(/5,7/); iFACE(1:2,2,3)=(/6,8/) 
      iFACE(1:2,1,4)=(/1,6/); iFACE(1:2,2,4)=(/2,5/) 
      iFACE(1:2,1,5)=(/2,7/); iFACE(1:2,2,5)=(/3,6/) 
      iFACE(1:2,1,6)=(/4,5/); iFACE(1:2,2,6)=(/1,8/)
      
      !face_id -> (node_id, node_id, node_id, node_id)
      nodFACE(1,1:4) = (/1,2,3,4/) 
      nodFACE(2,1:4) = (/4,3,7,8/) 
      nodFACE(3,1:4) = (/8,7,6,5/) 
      nodFACE(4,1:4) = (/5,6,2,1/) 
      nodFACE(5,1:4) = (/3,2,6,7/) 
      nodFACE(6,1:4) = (/5,1,4,8/)
      
      !same in binary
      bNodFACE(01) = B'000011110' !last digit is position 0 and is unused
      bNodFACE(02) = B'110011000'      
      bNodFACE(03) = B'111100000'
      bNodFACE(04) = B'001100110'
      bNodFACE(05) = B'011001100'
      bNodFACE(06) = B'100110010'
      
      !(node_id, face_id) -> Is_On_face
      IsNodeOnFace(1,1:6) = (/.TRUE. ,.FALSE.,.FALSE.,.TRUE .,.FALSE.,.TRUE. /)
      IsNodeOnFace(2,1:6) = (/.TRUE. ,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.FALSE./)
      IsNodeOnFace(3,1:6) = (/.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.TRUE. ,.FALSE./)
      IsNodeOnFace(4,1:6) = (/.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.TRUE. /)
      IsNodeOnFace(5,1:6) = (/.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.TRUE. /)
      IsNodeOnFace(6,1:6) = (/.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.FALSE./)
      IsNodeOnFace(7,1:6) = (/.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.TRUE. ,.FALSE./)
      IsNodeOnFace(8,1:6) = (/.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.TRUE. /)                                    

      !edge number -> (node_id, node_id)            
      iEDGE(1:2, 1)=(/1,2/) ; iEDGE(1:2,  4)=(/3,2/);
      iEDGE(1:2, 2)=(/1,4/) ; iEDGE(1:2,  5)=(/3,4/);
      iEDGE(1:2, 3)=(/1,5/) ; iEDGE(1:2,  6)=(/3,7/);
      iEDGE(1:2, 7)=(/6,2/) ; iEDGE(1:2, 10)=(/8,4/);
      iEDGE(1:2, 8)=(/6,5/) ; iEDGE(1:2, 11)=(/8,5/);
      iEDGE(1:2, 9)=(/6,7/) ; iEDGE(1:2, 12)=(/8,7/);
      
!      !Identification des diagonales.(si besoin)
!      iEDGE(1:2, 13)=(/1,3/) ; iEDGE(1:2, 16)=(/2,4/);
!      iEDGE(1:2, 14)=(/1,6/) ; iEDGE(1:2, 17)=(/2,5/);
!      iEDGE(1:2, 15)=(/1,8/) ; iEDGE(1:2, 18)=(/2,7/);
!      !
!      iEDGE(1:2, 19)=(/3,6/) ; iEDGE(1:2, 21)=(/4,7/)
!      iEDGE(1:2, 20)=(/3,8/) ; iEDGE(1:2, 22)=(/4,5/);
!      !
!      iEDGE(1:2, 23)=(/5,7/) ; iEDGE(1:2, 24)=(/6,8/); 
     
      ! node_id -> 3 adjacent edges
      i22WhichEdge(1, 1:3)=(/-01,-02,-03/)
      i22WhichEdge(2, 1:3)=(/ 01, 04, 07/)
      i22WhichEdge(3, 1:3)=(/-04,-05,-06/)
      i22WhichEdge(4, 1:3)=(/ 02, 05, 10/)
      i22WhichEdge(5, 1:3)=(/ 03, 08, 11/)
      i22WhichEdge(6, 1:3)=(/-07,-08,-09/)
      i22WhichEdge(7, 1:3)=(/ 06, 09, 12/)
      i22WhichEdge(8, 1:3)=(/-10,-11,-12/) 
      
      ! node_id -> 3 adjacent faces
      iFacesFromNode(1,1:3) = (/ 1,4,6 /)                                         
      iFacesFromNode(2,1:3) = (/ 1,4,5 /)  
      iFacesFromNode(3,1:3) = (/ 1,2,5 /)  
      iFacesFromNode(4,1:3) = (/ 1,2,6 /)  
      iFacesFromNode(5,1:3) = (/ 3,4,6 /)  
      iFacesFromNode(6,1:3) = (/ 3,4,5 /)  
      iFacesFromNode(7,1:3) = (/ 2,3,5 /)  
      iFacesFromNode(8,1:3) = (/ 2,3,6 /)                                            
      
      !(face_id,node_id) -> edge_id
      iGetEdge(1,1)=+03;      iGetEdge(1,2)=-07;      iGetEdge(1,3)=+06;      iGetEdge(1,4)=-10
      iGetEdge(2,4)=-02;      iGetEdge(2,3)=+04;      iGetEdge(2,7)=-09;      iGetEdge(2,8)=+11
      iGetEdge(3,8)=+10;      iGetEdge(3,7)=-06;      iGetEdge(3,6)=+07;      iGetEdge(3,5)=-03
      iGetEdge(4,5)=-11;      iGetEdge(4,6)=+09;      iGetEdge(4,2)=-04;      iGetEdge(4,1)=+02
      iGetEdge(5,3)=+05;      iGetEdge(5,2)=-01;      iGetEdge(5,6)=+08;      iGetEdge(5,7)=-12
      iGetEdge(6,5)=-08;      iGetEdge(6,1)=+01;      iGetEdge(6,4)=-05;      iGetEdge(6,8)=+12                             

      !(face_id) -> {edge_ids}
      iNormalEdgesList(1,1:4)= (/ +03,+06,-07,-10 /) 
      iNormalEdgesList(2,1:4)= (/ -02,+04,-09,+11 /)
      iNormalEdgesList(3,1:4)= (/ -03,-06,+07,+10 /)
      iNormalEdgesList(4,1:4)= (/ +02,-04,+09,-11 /)
      iNormalEdgesList(5,1:4)= (/ -01,+05,+08,-12 /)
      iNormalEdgesList(6,1:4)= (/ +01,-05,-08,+12 /)                              


      !(face_id,node_id) -> opposite node
      IGetOppositeNode(1,1)=05;    IGetOppositeNode(1,2)=06;      IGetOppositeNode(1,3)=07;      IGetOppositeNode(1,4)=08
      IGetOppositeNode(2,4)=01;    IGetOppositeNode(2,3)=02;      IGetOppositeNode(2,7)=06;      IGetOppositeNode(2,8)=05
      IGetOppositeNode(3,8)=04;    IGetOppositeNode(3,7)=03;      IGetOppositeNode(3,6)=02;      IGetOppositeNode(3,5)=01
      IGetOppositeNode(4,5)=08;    IGetOppositeNode(4,6)=07;      IGetOppositeNode(4,2)=03;      IGetOppositeNode(4,1)=04
      IGetOppositeNode(5,3)=04;    IGetOppositeNode(5,2)=01;      IGetOppositeNode(5,6)=05;      IGetOppositeNode(5,7)=08
      IGetOppositeNode(6,5)=06;    IGetOppositeNode(6,1)=02;      IGetOppositeNode(6,4)=03;      IGetOppositeNode(6,8)=07  
      
      !(face_id,node_id) -> (tangent node 1, tangent node 2)
      IGetTransvNodes(:,:,:)   = 0
      !     
      IGetTransvNodes(1,1,1:2) = (/4,2/) ; IGetTransvNodes(2,4,1:2) = (/3,8/) ; IGetTransvNodes(3,8,1:2) = (/5,7/) ;
      IGetTransvNodes(1,2,1:2) = (/1,3/) ; IGetTransvNodes(2,3,1:2) = (/7,4/) ; IGetTransvNodes(3,7,1:2) = (/8,6/) ;
      IGetTransvNodes(1,3,1:2) = (/2,4/) ; IGetTransvNodes(2,7,1:2) = (/8,3/) ; IGetTransvNodes(3,6,1:2) = (/7,5/) ;
      IGetTransvNodes(1,4,1:2) = (/3,1/) ; IGetTransvNodes(2,8,1:2) = (/4,7/) ; IGetTransvNodes(3,5,1:2) = (/6,8/) ;
      !
      IGetTransvNodes(4,5,1:2) = (/1,6/) ; IGetTransvNodes(5,3,1:2) = (/2,7/) ; IGetTransvNodes(6,1,1:2) = (/4,5/) ;
      IGetTransvNodes(4,6,1:2) = (/5,2/) ; IGetTransvNodes(5,2,1:2) = (/6,3/) ; IGetTransvNodes(6,4,1:2) = (/8,1/) ;
      IGetTransvNodes(4,2,1:2) = (/6,1/) ; IGetTransvNodes(5,6,1:2) = (/7,2/) ; IGetTransvNodes(6,8,1:2) = (/5,4/) ;
      IGetTransvNodes(4,1,1:2) = (/2,5/) ; IGetTransvNodes(5,7,1:2) = (/3,6/) ; IGetTransvNodes(6,5,1:2) = (/1,8/) ;      

      ! face_id -> (edge_id, edge_id, edge_id, edge_id)
      iGetEdgesFromFace(1,1:4) = (/01,02,05,04/)
      iGetEdgesFromFace(2,1:4) = (/05,10,12,06/)
      iGetEdgesFromFace(3,1:4) = (/12,11,08,09/)
      iGetEdgesFromFace(4,1:4) = (/01,03,08,07/)
      iGetEdgesFromFace(5,1:4) = (/04,06,09,07/)
      iGetEdgesFromFace(6,1:4) = (/02,03,11,10/) 
      
      ! edge_id -> edge_id, edge_id     
      iGetAdjFace( 01, 1:2 ) = (/ 01,04 /)
      iGetAdjFace( 02, 1:2 ) = (/ 01,06 /)
      iGetAdjFace( 03, 1:2 ) = (/ 04,06 /)
      iGetAdjFace( 04, 1:2 ) = (/ 01,05 /)
      iGetAdjFace( 05, 1:2 ) = (/ 01,02 /)
      iGetAdjFace( 06, 1:2 ) = (/ 02,05 /)
      iGetAdjFace( 07, 1:2 ) = (/ 04,05 /)
      iGetAdjFace( 08, 1:2 ) = (/ 03,04 /)
      iGetAdjFace( 09, 1:2 ) = (/ 03,05 /)
      iGetAdjFace( 10, 1:2 ) = (/ 02,06 /)
      iGetAdjFace( 11, 1:2 ) = (/ 03,06 /)
      iGetAdjFace( 12, 1:2 ) = (/ 02,03 /)
      
      
      !FACE_ID,EDGE_ID- > NODE_ID
      iGetNodeFromEdgeAndFace(:,:) =  0
      
      iGetNodeFromEdgeAndFace(1,03) = 1 ; iGetNodeFromEdgeAndFace(2,02) = 4
      iGetNodeFromEdgeAndFace(1,06) = 3 ; iGetNodeFromEdgeAndFace(2,04) = 3
      iGetNodeFromEdgeAndFace(1,07) = 6 ; iGetNodeFromEdgeAndFace(2,09) = 7
      iGetNodeFromEdgeAndFace(1,10) = 8 ; iGetNodeFromEdgeAndFace(2,11) = 8           

      iGetNodeFromEdgeAndFace(3,03) = 5 ; iGetNodeFromEdgeAndFace(4,02) = 1
      iGetNodeFromEdgeAndFace(3,06) = 7 ; iGetNodeFromEdgeAndFace(4,04) = 2
      iGetNodeFromEdgeAndFace(3,07) = 6 ; iGetNodeFromEdgeAndFace(4,09) = 6
      iGetNodeFromEdgeAndFace(3,10) = 8 ; iGetNodeFromEdgeAndFace(4,11) = 5
      
      iGetNodeFromEdgeAndFace(5,01) = 2 ; iGetNodeFromEdgeAndFace(6,01) = 1
      iGetNodeFromEdgeAndFace(5,05) = 3 ; iGetNodeFromEdgeAndFace(6,05) = 4 
      iGetNodeFromEdgeAndFace(5,08) = 6 ; iGetNodeFromEdgeAndFace(6,08) = 5 
      iGetNodeFromEdgeAndFace(5,12) = 7 ; iGetNodeFromEdgeAndFace(6,12) = 8 
      


      !iRightEdge(Face,nodeID)
      !next edge is clockwise edge when face is looked from cell centroid.
      ! "-" : means centripetal
      ! "+" : means centrifugal
      iLeftEdge(1,1)=+02 ;      iLeftEdge(1,2)=-01 ;      iLeftEdge(1,3)=+04 ;      iLeftEdge(1,4)=-05 ;  !1234  <-  nodFACE(1,1:4) = (/1,2,3,4/) 
      iLeftEdge(2,1)=-10 ;      iLeftEdge(2,2)=+05 ;      iLeftEdge(2,3)=-06 ;      iLeftEdge(2,4)=+12 ;  !4378  <-  nodFACE(2,1:4) = (/4,3,7,8/) 
      iLeftEdge(3,1)=+11 ;      iLeftEdge(3,2)=-12 ;      iLeftEdge(3,3)=+09 ;      iLeftEdge(3,4)=-08 ;  !8765  <-  nodFACE(3,1:4) = (/8,7,6,5/) 
      iLeftEdge(4,1)=-03 ;      iLeftEdge(4,2)=+08 ;      iLeftEdge(4,3)=-07 ;      iLeftEdge(4,4)=+01 ;  !5621  <-  nodFACE(4,1:4) = (/5,6,2,1/) 
      iLeftEdge(5,1)=+06 ;      iLeftEdge(5,2)=-04 ;      iLeftEdge(5,3)=+07 ;      iLeftEdge(5,4)=-09 ;  !3267  <-  nodFACE(5,1:4) = (/3,2,6,7/) 
      iLeftEdge(6,1)=-11 ;      iLeftEdge(6,2)=+03 ;      iLeftEdge(6,3)=-02 ;      iLeftEdge(6,4)=+10 ;  !5148  <-  nodFACE(6,1:4) = (/5,1,4,8/)             
      
      IF(INT22>0)THEN
        DO NIN=1,INT22
          DO J=1,12*I22LEN
            EDGE_LIST(NIN,J)%NBCUT = 0
          ENDDO
        ENDDO
      ENDIF
            
C=======================================================================
      IDT_INT22                                                       = 0
      dt22_min                                                        = EP30
      dx22_min                                                        = EP30
      dx22min_l(0:NTHREAD-1)                                          = EP30
      v22_max                                                         = ZERO
      v22max_l(0:NTHREAD-1)                                           = ZERO
      nCross22                                                        = FIVE
      CritMerge22                                                     = HALF                                                                         
      CritDeMerge22                                                   = HALF                                                                         
      CritDVol22                                                      = FOURTH                                                                          
      NADJ_F                                                          = 5                                                                              
      NBOLD(1)                                                        = NB                                                                             
      IIAD22(:,:)                                                     = 0                                                                              
      NBCUT_MAX                                                       = 8 + 6  !cut plane in cut cell, maximum reached with 8 tetra (all 8 nodes isolated)  +6 closure surface
      I22_DEGENERATED                                                 = 0                                                                              
      UVARL(0:NTHREAD-1,1:I22LEN,1:N0PHAS+TRIMAT*NVPHAS+I22LAW37)     = ZERO
      SuperCellVOL_L(0:NTHREAD-1,0:TRIMAT+I22LAW37,1:I22LEN)          = ZERO
      EINT_L(0:NTHREAD-1,1:I22LEN)                                    = ZERO
      MOM_L(1:3,0:NTHREAD-1,1:I22LEN)                                 = ZERO
      RHO_L(0:NTHREAD-1,1:I22LEN)                                     = ZERO
      SIG_L(0:NTHREAD-1,1:6,1:I22LEN)                                 = ZERO
      VOLD_L(0:NTHREAD-1,0:TRIMAT+I22LAW37,1:I22LEN)                  = ZERO      
      UNLINKED_CELLS_L(0:NTHREAD-1,1:2,1:I22LEN)                      = 0         
      N_UNLINKED_L(0:NTHREAD-1)                                       = 0         
      DO NIN=1,INT22
        OLD_SecndList(NIN,1:I22LEN)%Num                               = 0         
        OLD_SecndList(NIN,1:I22LEN)%NumSecndNodes                     = 0         
        DO J=1,24
          OLD_SecndList(NIN,1:I22LEN)%FM(J)                           = 0         
          OLD_SecndList(NIN,1:I22LEN)%FV(J)                           = 0         
          OLD_SecndList(NIN,1:I22LEN)%IV(J)                           = 0         
          OLD_SecndList(NIN,1:I22LEN)%IBV(J)                          = 0         
          OLD_SecndList(NIN,1:I22LEN)%ICELLv(J)                       = 0         
          OLD_SecndList(NIN,1:I22LEN)%VOL(J)                          = ZERO      
          OLD_SecndList(NIN,1:I22LEN)%NumNOD_Cell(J)                  = 0
          DO K=1,8
          OLD_SecndList(NIN,1:I22LEN)%ListNodID(J,K)                  = 0
          ENDDO
        ENDDO!next J
      ENDDO!next NIN
C=======================================================================

      !--------------------!
      !         ALL        !
      !   ON(1) / OFF(0)   !      
      !--------------------!      
      IFLG = 0 !IOUTP22

      IF(INT22 > 0)THEN
        IBUG22               =  0
        !-----------------------!                                                                             
        IBUG22_ccBufList     =  0
        IBUG22_antidiff      = -0 
        IBUG22_upwind        = -0
        IBUG22_tri           =  0
        IBUG22_trivox        =  0                                                                                   
        IBUG22_intersect     = -0
        IBUG22_subvol        =  12781
        IBUG22_aera          =  0
        IBUG22_ident         =  12781
        IBUG22_convec        =  0
        IBUG22_spe_convec    =  0
        IBUG22_eos           =  0
        IBUG22_dvol          = -0
        IBUG22_sigeps51      =  0
        IBUG22_ssp           =  0
        IBUG22_flux          = -0
        IBUG22_flux22        = -0
        IBUG22_volfrac       =  0
        IBUG22_conti         =  0
        IBUG22_fint          =  12781
        IBUG22_sinit         =  0
        IBUG22_nodalp        =  0
        IBUG22_amouil        =  0
        IBUG22_fcont         = -0
        IBUG22_fcontASS      = -0
        IBUG22_Swet          = -0
        IBUG22_destroy       = -0
        IBUG22_merge         = -0
        IBUG22_link_switch   = -0
        IBUG22_tracking      = -0
        IBUG22_prediction    = -0
        IBUG22_UndirectLink  = -0
        IBUG22_Truss         =  0
        IBUG22_OrphanNodes   =  0
        IBUG22_DtMin         =  0
        IBUG22_OUTP_CCbuffer =  0    !enables to output bricks in cut cell buffer (HyperMesh tcl script)            
        IBUG22_OUTP_IntPoint =  0    !enables to output intersection points (HyperMesh tcl script)                  
        IBUG22_NVAR          =  2    !-1: all, 1:mass, 2:energy                                                     
        IBUG22_ITRIMAT       =  1    !-1: all, 0:global, 1:submat1, 2:submat2, 3:submat3, 4:submat4   
        IBUG22_CC_LENGTH     =  0              
      ENDIF
      
      

      !---------------!            
      IF(INT22 > 0)THEN
        IBUG22_ccBufList     = IBUG22_ccBufList       * IFLG
        IBUG22_antidiff      = IBUG22_antidiff        * IFLG    
        IBUG22_upwind        = IBUG22_upwind          * IFLG    
        IBUG22_tri           = IBUG22_tri             * IFLG    
        IBUG22_trivox        = IBUG22_trivox          * IFLG    
        IBUG22_intersect     = IBUG22_intersect       * IFLG    
        IBUG22_subvol        = IBUG22_subvol          * IFLG    
        IBUG22_aera          = IBUG22_aera            * IFLG    
        IBUG22_ident         = IBUG22_ident           * IFLG    
        IBUG22_convec        = IBUG22_convec          * IFLG    
        IBUG22_spe_convec    = IBUG22_spe_convec      * IFLG    
        IBUG22_eos           = IBUG22_eos             * IFLG    
        IBUG22_dvol          = IBUG22_dvol            * IFLG    
        IBUG22_sigeps51      = IBUG22_sigeps51        * IFLG    
        IBUG22_ssp           = IBUG22_ssp             * IFLG    
        IBUG22_flux          = IBUG22_flux            * IFLG    
        IBUG22_flux22        = IBUG22_flux22          * IFLG    
        IBUG22_volfrac       = IBUG22_volfrac         * IFLG    
        IBUG22_conti         = IBUG22_conti           * IFLG    
        IBUG22_fint          = IBUG22_fint            * IFLG    
        IBUG22_sinit         = IBUG22_sinit           * IFLG    
        IBUG22_nodalp        = IBUG22_nodalp          * IFLG    
        IBUG22_amouil        = IBUG22_amouil          * IFLG    
        IBUG22_fcont         = IBUG22_fcont           * IFLG    
        IBUG22_fcontASS      = IBUG22_fcontASS        * IFLG    
        IBUG22_Swet          = IBUG22_Swet            * IFLG    
        IBUG22_destroy       = IBUG22_destroy         * IFLG    
        IBUG22_merge         = IBUG22_merge           * IFLG    
        IBUG22_link_switch   = IBUG22_link_switch     * IFLG    
        IBUG22_tracking      = IBUG22_tracking        * IFLG            
        IBUG22_prediction    = IBUG22_prediction      * IFLG            
        IBUG22_UndirectLink  = IBUG22_UndirectLink    * IFLG    
        IBUG22_Truss         = IBUG22_Truss           * IFLG    
        IBUG22_DtMin         = IBUG22_DtMin           * IFLG    
        IBUG22_OrphanNodes   = IBUG22_OrphanNodes     * IFLG         
        IBUG22_OUTP_CCbuffer = IBUG22_OUTP_CCbuffer   * IFLG    
        IBUG22_OUTP_IntPoint = IBUG22_OUTP_IntPoint   * IFLG    
        IBUG22_NVAR          = IBUG22_NVAR            * IFLG    
        IBUG22_ITRIMAT       = IBUG22_ITRIMAT         * IFLG   
        IBUG22_CC_LENGTH     = IBUG22_CC_LENGTH       * IFLG
      ENDIF      
      
      IF(INT22>0 .AND. IBUG22 == 1 .AND. IFLG.NE.0)THEN
        PRINT *, "=================="        
        PRINT *, "= Output Flags : ="
        PRINT *, "=================="        
        IF(IBUG22              /=0)print *,"IBUG22              "
        IF(IBUG22_tri          /=0)print *,"IBUG22_tri          "
        IF(IBUG22_trivox       /=0)print *,"IBUG22_trivox       "
        IF(IBUG22_intersect    /=0)print *,"IBUG22_intersect    "
        IF(IBUG22_subvol       /=0)print *,"IBUG22_subvol       "
        IF(IBUG22_aera         /=0)print *,"IBUG22_aera         "
        IF(IBUG22_ident        /=0)print *,"IBUG22_ident        "
        IF(IBUG22_convec       /=0)print *,"IBUG22_convec       "
        IF(IBUG22_eos          /=0)print *,"IBUG22_eos          "
        IF(IBUG22_dvol         /=0)print *,"IBUG22_dvol         "
        IF(IBUG22_sigeps51     /=0)print *,"IBUG22_sigeps51     "
        IF(IBUG22_ssp          /=0)print *,"IBUG22_ssp          "
        IF(IBUG22_flux         /=0)print *,"IBUG22_flux         "
        IF(IBUG22_flux22       /=0)print *,"IBUG22_flux22       "
        IF(IBUG22_volfrac      /=0)print *,"IBUG22_volfrac      "
        IF(IBUG22_conti        /=0)print *,"IBUG22_conti        "
        IF(IBUG22_fint         /=0)print *,"IBUG22_fint         "
        IF(IBUG22_sinit        /=0)print *,"IBUG22_sinit        "
        IF(IBUG22_nodalp       /=0)print *,"IBUG22_nodalp       "
        IF(IBUG22_amouil       /=0)print *,"IBUG22_amouil       "
        IF(IBUG22_spe_convec   /=0)print *,"IBUG22_spe_convec   "
        IF(IBUG22_fcont        /=0)print *,"IBUG22_fcont        " 
        IF(IBUG22_Swet         /=0)print *,"IBUG22_Swet         " 
        IF(IBUG22_destroy      /=0)print *,"IBUG22_destroy      "               
        IF(IBUG22_merge        /=0)print *,"IBUG22_merge        " 
        IF(IBUG22_UndirectLink /=0)print *,"IBUG22_UndirectLink "         
        IF(IBUG22_Truss        /=0)print *,"IBUG22_Truss        " 
        IF(IBUG22_DtMin        /=0)print *,"IBUG22_DtMin        "                 
        IF(IBUG22_OUTP_CCbuffer/=0)print *,"IBUG22_OUTP_CCbuffer" 
        IF(IBUG22_OUTP_IntPoint/=0)print *,"IBUG22_OUTP_IntPoint"                 
        print *, "=================="                                                                                                                     
      ENDIF

!      IF(NTHREAD.GT.1)THEN
!        print *, "**error inter22 : Multithreading not yet validated"
!        stop
!      ENDIF

      RETURN
      END



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
#ifndef MACRO_DEFINED
#define MACRO_DEFINED
C=================== IPARI    ==================
#define MACRO_JINBUF                          1
#define MACRO_JBUFIN                          2
#define MACRO_NRTS                            3
#define MACRO_NRTM                            4
#define MACRO_NSN                             5
#define MACRO_NMN                             6
#define MACRO_NTY                             7
#define MACRO_ITY                             7
#define MACRO_NST                             8
#define MACRO_NMT                             9
#define MACRO_JINSCR                         10
#define MACRO_IBC                            11
#define MACRO_IBUC                           12
#define MACRO_IDEF                           13
#define MACRO_IVSIZ                          14
#define MACRO_NOINT                          15
#define MACRO_IDEL                           17
#define MACRO_NCONT                          18
#define MACRO_ISINT                          19
#define MACRO_ILEV                           20
#define MACRO_IGAP                           21
#define MACRO_INACTI                         22
#define MACRO_MULTIMP                        23
#define MACRO_NSNR                           24
#define MACRO_IRS                            25
#define MACRO_HIERA                          26
#define MACRO_IADFIN                         27
#define MACRO_INTSEC                         28
#define MACRO_ICONT                          29
#define MACRO_MFROT                          30
#define MACRO_IFQ                            31
#define MACRO_IBAG                           32
#define MACRO_ILAGM                          33
#define MACRO_IGSTI                          34
#define MACRO_USRPID                         35
#define MACRO_NISUB                          36
#define MACRO_NISUBS                         37
#define MACRO_NISUBM                         38
#define MACRO_ICURV                          39
#define MACRO_NA1                            40
#define MACRO_NA2                            41
#define MACRO_ISYM                           42
#define MACRO_IUBUF                          43                            
#define MACRO_IADM                           44                  
#define MACRO_ISU1                           45
#define MACRO_ISU2                           46
#define MACRO_INTTH                          47                      
#define MACRO_IFORM                          48                         
#define MACRO_NRADM                          49                         
#define MACRO_IFNOR                          50                      
#define MACRO_NLINS                          51
#define MACRO_NLINS1                         51
#define MACRO_MLINM                          52
#define MACRO_NLINS2                         52
#define MACRO_NLINSA                         53
#define MACRO_MLINMA                         54
#define MACRO_NSNE                           55
#define MACRO_NMNE                           56
#define MACRO_NSNER                          57
#define MACRO_IEDGE                          58
#define MACRO_LINE1                          59
#define MACRO_LINE2                          60
#define MACRO_IDELKEEP                       61
#define MACRO_NREMNODE                       62
#define MACRO_FLAGREMNODE                    63
#define MACRO_IDSENS                         64
#define MACRO_INTKG                          65
#define MACRO_INTPLY                         66
#define MACRO_NADMSR                         67
#define MACRO_NEDGE                          68
#define MACRO_NREMNOR                        81
#define MACRO_IREM25I2                       83
#define MACRO_ITIED                          85
C ==================== DEBUG
#define MACRO_DEBUG_INTER 1
#define MACRO_DEBUG_RESTA 2
#define MACRO_DEBUG_CAND 3
#define MACRO_DEBUG_CORE 4
#define MACRO_DEBUG_ACC 5 
#define MACRO_DEBUG_MEM 6
#define MACRO_DEBUG_AMS 7
#define MACRO_DEBUG_TEMP 8
#define MACRO_DEBUG_PON 9
#define MACRO_DEBUG_CONV0 10
#define MACRO_DEBUG_ADYRE 11
#define MACRO_DEBUG_CHKSM 12
#define MACRO_DEBUG_NAN 13


C ==================== TIMER
#define MACRO_TIMER_RESOL          1
#define MACRO_TIMER_CONTSORT       2
#define MACRO_TIMER_ELEMENT        3
#define MACRO_TIMER_KIN            4
#define MACRO_TIMER_INTEG          5
#define MACRO_TIMER_P0             6
#define MACRO_TIMER_IO             7
#define MACRO_TIMER_CONTFOR        8
#define MACRO_TIMER_ASM            9
#define MACRO_TIMER_EXFOR          10
#define MACRO_TIMER_EXRBYF         11
#define MACRO_TIMER_EXRBYV         12
#define MACRO_TIMER_EXSPMDV        13
#define MACRO_TIMER_MADYMO         14
#define MACRO_TIMER_AMS            39
#define MACRO_TIMER_TMP1          150
#define MACRO_TIMER_TMP2          149

#define MACRO_TIMER_ALEMAIN        110
#define MACRO_TIMER_MULTIFVM       111
#define MACRO_TIMER_IFSUB0         112
#define MACRO_TIMER_MUSCL          113
#define MACRO_TIMER_SPMDCFD        114
#define MACRO_TIMER_FRIC           108

#define MACRO_TIMER_LIBH3D         120
#define MACRO_TIMER_SPMDH3D        121
#define MACRO_TIMER_GENH3D         122
#define MACRO_TIMER_GENH3D1         123
#define MACRO_TIMER_GENH3D2         124

#define MACRO_TIMER_SKEW            125
#define MACRO_TIMER_FVMBAG          126
#define MACRO_TIMER_FVMBAG1         127
#define MACRO_TIMER_MONVOL          128


#define MACRO_TIMER_T25SLIDING      129
#define MACRO_TIMER_T25NORM         106
#define MACRO_TIMER_T25STFE         130
#define MACRO_TIMER_T25VOX0         131
#define MACRO_TIMER_T25VOX0E2E      132
#define MACRO_TIMER_T25VOX          133
#define MACRO_TIMER_T25RNUM         134
#define MACRO_TIMER_T25RNUME        135
#define MACRO_TIMER_T25BUC          136
#define MACRO_TIMER_T25BUCE2E       137
#define MACRO_TIMER_T25TRCE         138









#endif


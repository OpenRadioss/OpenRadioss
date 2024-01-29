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
C ARCHINFO
C Champ 1 = taille de groupe (NVISIZ)
C Champ 2 = Machine Scalaire ou vectorielle (IVECTOR)

      DO I=1,128
         ARCHINFO(I,1)=0
         ARCHINFO(I,2)=0
      ENDDO
C   Machine generique - passe partout
      ARCHINFO(1,1)=32
      ARCHINFO(1,2)=0
      ARCHN(1)='Generic'
C   Aciennes machines CRAY (ymp, ymp_spmd, CPP_c90, t90, t90_i3e
      ARCHINFO(2,1)=512
      ARCHINFO(2,2)=1
      ARCHN(2)='CRAY SV1'
C   SGI65
      ARCHINFO(3,1)=128
      ARCHINFO(3,2)=0
      ARCHN(3)='SGI 65'
C   HP11 - PA
      ARCHINFO(4,1)=64
      ARCHINFO(4,2)=0
      ARCHN(4)='PA-RISC HP11'
C   Power4
      ARCHINFO(5,1)=32
      ARCHINFO(5,2)=0
      ARCHN(5)='aix64'
C   deca
      ARCHINFO(6,1)=32
      ARCHINFO(6,2)=0
      ARCHN(6)='DEC ALPHA'
C   VPP5000
      ARCHINFO(7,1)=1024
      ARCHINFO(7,2)=1
      ARCHN(7)='FUJITSU VPP5000'
C   SX6
      ARCHINFO(8,1)=4095
      ARCHINFO(8,2)=1
      ARCHN(8)='NEC SX6'
C   Pentium4
      ARCHINFO(9,1)=32
      ARCHINFO(9,2)=0
      ARCHN(9)='linux32 pgi'
C   LINUX IA64
      ARCHINFO(10,1)=256
      ARCHINFO(10,2)=0
      ARCHN(10)='linuxia64'
C   HP IA64
      ARCHINFO(11,1)=256
      ARCHINFO(11,2)=0
      ARCHN(11)='IA64 HPUX11'
C   Opteron
      ARCHINFO(12,1)=128
      ARCHINFO(12,2)=0
      ARCHN(12)='pgi_linux64'
C   Primepower
      ARCHINFO(13,1)=64
      ARCHINFO(13,2)=0
      ARCHN(13)='FUJITSU PRIMEPOWER'
C   CX1
      ARCHINFO(14,1)=511
      ARCHINFO(14,2)=1
      ARCHN(14)='CRAY X1'
C   SUN
      ARCHINFO(15,1)=32
      ARCHINFO(15,2)=0
      ARCHN(15)='SUN SPARC'
C   WNT
      ARCHINFO(16,1)=32
      ARCHINFO(16,2)=0
      ARCHN(16)='WNT'
C   alinux
      ARCHINFO(17,1)=32
      ARCHINFO(17,2)=0
      ARCHN(17)='ALPHA LINUX'
C   p4linux964_spmd
      ARCHINFO(18,1)=128
      ARCHINFO(18,2)=0
      ARCHN(18)='linux64'
C   wmr
      ARCHINFO(19,1)=32
      ARCHINFO(19,2)=0
      ARCHN(19)='MY RADIOSS WINDOWS'
C   lmr
      ARCHINFO(20,1)=32
      ARCHINFO(20,2)=0
      ARCHN(20)='MY RADIOSS LINUX'
C   win64_spmd
      ARCHINFO(21,1)=128
      ARCHINFO(21,2)=0
      ARCHN(21)='win64'
C   p4win64_spmd
      ARCHINFO(22,1)=128
      ARCHINFO(22,2)=0
      ARCHN(22)='win64'
C p4win32
      ARCHINFO(23,1)=64
      ARCHINFO(23,2)=0
      ARCHN(23)='win32'
C Sol10 X86-64
      ARCHINFO(24,1)=128
      ARCHINFO(24,2)=0
      ARCHN(24)='solarisx64'
C p4linux32
      ARCHINFO(25,1)=64
      ARCHINFO(25,2)=0
      ARCHN(25)='linux32'
C macosx64
      ARCHINFO(26,1)=128
      ARCHINFO(26,2)=0
      ARCHN(26)='macosx64'



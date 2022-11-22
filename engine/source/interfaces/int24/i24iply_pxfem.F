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
Chd|  I24IPLY_PXFEM                 source/interfaces/int24/i24iply_pxfem.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24IPLY_PXFEM(
     1                  JLT      ,CAND_E  ,MSEGTYP , IX1,IX2     ,
     2                  IX3     ,IX4    ,PENE,MS_PLY,INOD_PXFEM  ,
     3                  IPLY    ,ITAB) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
C-------------------------------------------f----
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, CAND_E(*),IX1(*), IX2(*), IX3(*), 
     .        IX4(*), INOD_PXFEM(*),
     .        IPLY(4,*),MSEGTYP(*),ITAB(*)
      my_real
     .     PENE(*),MS_PLY(NPLYXFE,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J1,J2,J3,J4,NN,ILY,ISEGTYP(MVSIZ),ITAG(MVSIZ),
     .        ISEG(MVSIZ),J5,J6,J7,J8,J9,J10,J11,J12
      my_real
     .     XO16, XO17, XO15, XO14, X163, X153, X152, X142, X141, 
     .     YO16, YO17, YO15, YO14, Y163, Y153, Y152, Y142, Y141, 
     .     ZO16, ZO17, ZO15, ZO14, Z163, Z153, Z152, Z142, Z141,
     .     X51 , Y51,Z51,X52,Y52,Z52,X53,Y53,Z53,X54,Y54,Z54  ,
     .     X164,Y164,Z164,X174,Y174,Z174,X171,Y171,Z171,NX1,
     .     NY1,NZ1,AAA,GAPN,NX2,NY2,NZ2,NX3,NY3,NZ3,DX,DY,DZ,
     .     NX4,NY4,NZ4,
     .     SX125(MVSIZ),SX235(MVSIZ),SX345(MVSIZ),SX415(MVSIZ),
     .     SY125(MVSIZ),SY235(MVSIZ),SY345(MVSIZ),SY415(MVSIZ),
     .     SZ125(MVSIZ),SZ235(MVSIZ),SZ345(MVSIZ),SZ415(MVSIZ),
     .     SX1250(MVSIZ),SX2350(MVSIZ),SX3450(MVSIZ),SX4150(MVSIZ),
     .     SY1250(MVSIZ),SY2350(MVSIZ),SY3450(MVSIZ),SY4150(MVSIZ),
     .     SZ1250(MVSIZ),SZ2350(MVSIZ),SZ3450(MVSIZ),SZ4150(MVSIZ),
     .     SX2114(MVSIZ),SX3215(MVSIZ),SX4316(MVSIZ),SX1417(MVSIZ),
     .     SY2114(MVSIZ),SY3215(MVSIZ),SY4316(MVSIZ),SY1417(MVSIZ),
     .     SZ2114(MVSIZ),SZ3215(MVSIZ),SZ4316(MVSIZ),SZ1417(MVSIZ),
     .     SX21140(MVSIZ),SX32150(MVSIZ),SX43160(MVSIZ),SX14170(MVSIZ),
     .     SY21140(MVSIZ),SY32150(MVSIZ),SY43160(MVSIZ),SY14170(MVSIZ),
     .     SZ21140(MVSIZ),SZ32150(MVSIZ),SZ43160(MVSIZ),SZ14170(MVSIZ),
     .     NXX(MVSIZ,17),NYY(MVSIZ,17),NZZ(MVSIZ,17),BBB,
     .     IPLYV(8,MVSIZ)
     
C --------------------------------------------------------------------- 
        DO I=1,JLT
          ISEGTYP(I) = MSEGTYP(CAND_E(I))
        ENDDO       
C--------------------------------------------------------
C
C for computing contact ply forces.
C
           DO I=1,JLT
C or by node segment if it needed            
             IPLY(1,I) = 0
             IPLY(2,I) = 0
             IPLY(3,I) = 0
             IPLY(4,I) = 0
             
             IF(PENE(I) == ZERO) CYCLE 
             IF(ISEGTYP(I) == ZERO) CYCLE 
              
             IF(ISEGTYP(I) > 0) THEN
              J1 = IX1(I)
              IF(J1 > 0) THEN
                NN = INOD_PXFEM(J1)                   
                IF(NN > 0 ) THEN 
                    ILY = NPLYMAX                     
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)  
                     ILY = ILY - 1                    
                    ENDDO                             
                    IPLY(1,I) = ILY     
                  ENDIF  
               ENDIF    
              J1 = IX2(I)
              IF(J1 > 0) THEN
              NN = INOD_PXFEM(J1)
              IF(NN > 0) THEN
                  ILY = NPLYMAX
                  DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY - 1
                  ENDDO
                  IPLY(2,I) = ILY
              ENDIF
              ENDIF
              J1 = IX3(I)
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0) THEN
                  ILY = NPLYMAX
                  DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                    ILY = ILY - 1
                  ENDDO    
                  IPLY(3,I) = ILY
              ENDIF
              ENDIF
              J1 = IX4(I)
              IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
              IF( NN > 0)  THEN
                 ILY = NPLYMAX
                 DO WHILE(MS_PLY(NN,ILY) == ZERO)
                  ILY = ILY - 1
                 ENDDO  
                 IPLY(4,I) = ILY
              ENDIF  
              ENDIF
             ELSE ! 
               J1= IX1(I)
               IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF( NN > 0)  THEN
                    ILY = 1
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1
                    ENDDO
                    IPLY(1,I) = ILY
              ENDIF
              ENDIF
              J1= IX2(I)
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF(NN > 0) THEN 
               ILY = 1                           
               DO WHILE(MS_PLY(NN,ILY) == ZERO)  
                  ILY = ILY + 1                  
               ENDDO                             
               IPLY(2,I) = ILY                   
              ENDIF   
             ENDIF                             
C                
              J1= IX3(I) 
              IF(J1 > 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0 ) THEN  
                ILY = 1
                DO WHILE(MS_PLY(NN,ILY) == ZERO)
                  ILY = ILY + 1
                ENDDO
                IPLY(3,I) = ILY
              ENDIF 
              ENDIF
C            
              J1= IX4(I)
              IF(J1> 0)THEN
              NN = INOD_PXFEM(J1)
              IF( NN > 0) THEN
                 ILY = 1
                 DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                  ILY = ILY + 1
                 ENDDO
                 IPLY(4,I) = ILY
              ENDIF 
              ENDIF
            ENDIF  
         ENDDO                
C         
         RETURN
         END

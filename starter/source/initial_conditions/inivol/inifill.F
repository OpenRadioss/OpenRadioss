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
      !||====================================================================
      !||    inifill         ../starter/source/initial_conditions/inivol/inifill.F
      !||--- called by ------------------------------------------------------
      !||    init_inivol     ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||--- calls      -----------------------------------------------------
      !||    iniphase        ../starter/source/initial_conditions/inivol/iniphase.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE INIFILL(NUMELS    ,NUMELTG,NUMELQ    ,NUMNOD   ,NPART    ,
     .                   NPROPMI   ,NUMMAT ,N2D       ,NGROUP   ,
     .                   IXS       ,IPM    ,IPART_    ,IPHASE   , SIPART_ ,
     .                   IDP       ,KVOL   ,BUFMAT    ,ITAGNSOL , ISOLNOD ,
     .                   NBIP      ,NTRACE ,PART_FILL ,NBSUBMAT , MLW     ,
     .                   ELBUF_TAB ,NG     ,MULTI_FVM ,IXQ      , IXTG    ,
     .                   ITYP      ,IMAT   ,SIPHASE   ,NUMEL_TOT, NEL     ,
     .                   NFT       ,SBUFMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD , ONLY : ELBUF_STRUCT_
      USE MULTI_FVM_MOD , ONLY : MULTI_FVM_STRUCT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMELS,NUMELTG,NUMELQ,NUMNOD,NPART,NPROPMI,NUMMAT,N2D,NGROUP,SIPART_,SBUFMAT
      INTEGER SIPHASE,IPM(NPROPMI,NUMMAT),IPHASE(NBSUBMAT+1,SIPHASE/(NBSUBMAT+1))
      INTEGER IPART_(SIPART_),IDP,ITAGNSOL(NUMNOD),NUMEL_TOT,NBIP(NBSUBMAT,NUMEL_TOT)
      INTEGER ISOLNOD,PART_FILL(NPART),NTRACE, ITYP, IMAT, NBSUBMAT, MLW, NG, NEL, NFT
      INTEGER,TARGET :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), IXTG(NIXTG, NUMELTG)
      INTEGER,POINTER :: pIXQ,pIXTG,pIXS
      my_real KVOL(NBSUBMAT,NUMEL_TOT),BUFMAT(SBUFMAT)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1,IADBUF,NPAR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      NF1 = NFT+1
      NPAR = IPM(9,IMAT)
      IADBUF = MAX(1,IPM(7,IMAT))
      
      NULLIFY(pIXS)
      NULLIFY(pIXQ)
      NULLIFY(pIXTG)
      IF(NUMELS>0)  pIXS  => IXS(1,NF1)            
      IF(NUMELQ>0)  pIXQ  => IXQ(1,NF1)    
      IF(N2D>0 .AND. NUMELTG>0) pIXTG => IXTG(1,NF1)
                   
C-----------
      CALL INIPHASE(N2D         ,NUMELS        ,NUMELTG        ,NUMELQ       ,
     .              NUMNOD      ,NGROUP        ,
     .              pIXS        ,IPART_(NF1)   ,IPHASE(1,NF1)  ,IDP          ,
     .              KVOL(1,NF1) ,NPAR          ,BUFMAT(IADBUF) ,NTRACE       ,
     .              ITAGNSOL    ,ISOLNOD       ,PART_FILL      ,NBIP(1,NF1)  ,
     .              NBSUBMAT    ,MLW           ,ELBUF_TAB      ,NG           ,
     .              MULTI_FVM   ,pIXQ          ,pIXTG          ,ITYP         ,
     .              NEL )
C-----------------------------------------------------------------
      RETURN
      END

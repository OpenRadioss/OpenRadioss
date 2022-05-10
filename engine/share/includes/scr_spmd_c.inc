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
Cow50c28 C.../COMGLOB/
Cow50c28 Cel sauvegarde de com02 (LCOM02 = 101), com03 (LCOM03 = 101),
Cow50c28 Cel com04 (LCOM04 = 62), rnur (LLRNUR = 2), lbufel et ngroup
Cow50c28 C
Cow50c28       INTEGER LCOM02G, LCOM03G, LCOM04G, LRNURG
Cow50c28 Cow41p+1      PARAMETER (LCOM02G = 101, LCOM03G = 101,
Cow50c28       PARAMETER (LCOM02G = 101, LCOM03G = 110,
Cow50c28      .           LCOM04G = 80 , LRNURG = 2   )
Cow50c28 Clm50c5     .           LCOM04G = 74 , LRNURG = 2   )
Cow50c28 Cel50b3     .           LCOM04G = 70 , LRNURG = 2   )
Cow50c28 Cel46b5     .           LCOM04G = 68 , LRNURG = 2   )
Cow50c28 Cow41m4     .           LCOM04G = 66 , LRNURG = 2   )
Cow50c28 Cow41j3     .           LCOM04G = 63 , LRNURG = 2   )
Cow50c28 CFPDev014     .           LCOM04G = 62 , LRNURG = 2   )
Cow50c28 Cel41i2     .           LCOM04G = 60 , LRNURG = 2   )
Cow50c28 Cel091     .          LCOM04G = 55 , LRNURG = 2   )
Cow50c28 
Cow50c28       COMMON /COMGLOB/ICOM02G(LCOM02G),ICOM03G(LCOM03G),
Cow50c28      .                ICOM04G(LCOM04G),IRNURG(LRNURG),
Cow50c28      .                NGROUPG,LBUFELG ,INTBAGG, LBUFSSG
Cow50c28 Cel41q4+1     .                NGROUPG,LBUFELG ,INTBAGG
Cow50c28 Cel41i1+1     .                NGROUPG,LBUFELG
Cow50c28       INTEGER ICOM02G, ICOM03G, ICOM04G, IRNURG, NGROUPG, LBUFELG,
Cow50c28      .        INTBAGG, LBUFSSG
Cel41p4+++
C.../COMCFD/
      INTEGER LCOMCFD
Cel46d1+1      PARAMETER (LCOMCFD = 9)
      PARAMETER (LCOMCFD = 10)
Cel variables specifiques CFD/SPMD
C
      COMMON /COMCFD/NSVOIS,NQVOIS ,NTGVOIS,NNVOIS ,NRCVVOIS,NSNDVOIS,
     .               ILAW11,NERVOIS,NESVOIS,NLWALE  ,NSEGCOM
Cel46d1     .               ILAW11,NERVOIS, NESVOIS, NLWALE
      INTEGER NSVOIS,NQVOIS ,NTGVOIS,NNVOIS ,NRCVVOIS,NSNDVOIS,
     .        ILAW11,NERVOIS,NESVOIS,NLWALE  ,NSEGCOM 
Cel46d1     .        ILAW11,NERVOIS, NESVOIS, NLWALE
Cel41p4---

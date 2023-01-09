//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include "hardware.inc"
#include <cstdint>
#include <cmath>


extern "C" void integer_to_IEEE_ASCII(int entier,unsigned char octet[4]);
extern "C" void IEEE_ASCII_to_integer(int *entier,unsigned char octet[4]);

extern "C" void real_to_IEEE_ASCII(float reel,unsigned char octet[4]);
extern "C" void IEEE_ASCII_to_real(float *reel,unsigned char octet[4]);

extern "C" void double_to_IEEE_ASCII(double *reel,unsigned char octet[1000][8],int len);
extern "C" void IEEE_ASCII_to_double(double *reel,unsigned char octet[1000][8],int len);



void integer_to_IEEE_ASCII(int entier,unsigned char octet[4])
{
	octet[3] = entier & 0xff;
	octet[2] = (entier >>  8)  & 0xff;
	octet[1] = (entier >> 16)  & 0xff;
	octet[0] = (entier >> 24)  & 0xff;
}

void IEEE_ASCII_to_integer(int *entier,unsigned char octet[4])
{
        int result, a, b;
        result = octet[0];
        result = (result << 8) + octet[1];
        result = (result << 8) + octet[2];
        result = (result << 8) + octet[3];
/* traitement special 64 bits */
        if((result & 0x80000000) ==  0x80000000)
        {
          a = (-1);
          b = 0xFFFFFFFF;
          result = result + a - b;
        }
        *entier = result;

} /* fin IEEE_ASCII_to_integer */

void real_to_IEEE_ASCII(float reel,unsigned char octet[4])
{
	int exposant,mantisse;
	if (reel < 0. ){
		if (reel > -1.17e-38 ){
			/* -0. */
			octet[0] = 0x80;
			octet[1] = 0x00;
			octet[2] = 0x00;
			octet[3] = 0x00;
			return;

		} else if( reel < -3.4e+38){ 

			/* -Infinity */
			octet[0] = 0xff;
			octet[1] = 0x80;
			octet[2] = 0x00;
			octet[3] = 0x00;
			return;

		} else {
			mantisse = (frexp(-(double)reel,&exposant) - 0.5)*1.6777216E7;
/*					*ldexp(1.,24); */
			exposant += 126;
			octet[0] = (exposant & 0xfe) >> 1 | 128;
		}
	} else {
		if (reel < 1.17e-38 ){

			/* +0. */
			octet[0] = 0x00;
			octet[1] = 0x00;
			octet[2] = 0x00;
			octet[3] = 0x00;
			return;

		} else if( reel > 3.4e+38){ 

			/* +Infinity */
			octet[0] = 0x7f;
			octet[1] = 0x80;
			octet[2] = 0x00;
			octet[3] = 0x00;
			return;

		} else {
			mantisse = (frexp((double)reel,&exposant) - 0.5)*1.6777216E7;
/*					*ldexp(1.,24); */
			exposant += 126;
			octet[0] = (exposant & 0xfe) >> 1;
		}
	}

	octet[1] = (exposant & 0x01) << 7 | (mantisse >> 16) & 0x7f ;
	octet[2] = (mantisse >> 8) & 0xff ;
	octet[3] =  mantisse & 0xff ;

}


void IEEE_ASCII_to_real(float *reel,unsigned char octet[4])
{
  int exposant;
  int signe;
  double mantisse, decalage;

  /* signe */
  signe = octet[0] & 0x80;
  if (signe==0)
  {
    signe = 1;
  }
  else
  {
    signe = -1;
  }  /* exposant */
  exposant  = (octet[0] & 0x7f) << 1;
  exposant += (octet[1] & 0x80) >> 7;

  if (exposant==0)  /* +0. ou -0. */
  {
    *reel = 0.;
    return;
  }

  exposant -= 126;

  /* mantisse */
  decalage = ldexp(1.,8);
  mantisse = (octet[1] & 0x7f);
  mantisse = mantisse * decalage + octet[2];
  mantisse = mantisse * decalage + octet[3];
  mantisse /= ldexp(1.,24);
  mantisse += 0.5;

  /* nombre */
  *reel = (float) signe * mantisse * ldexp(1.,exposant);

} /* fin IEEE_ASCII_to_real */



void double_to_IEEE_ASCII(double *reel,unsigned char octet[1000][8],int len)
{
        int exposant,i;
        unsigned int mantisse1, mantisse2;
        double dmantisse;
        for(i=0;i<len;i++)
        {
          if (reel[i] < 0. )
          {
                if (reel[i] > -4.e-308 ){ /* valeur exacte -2.22e-308 */
                        /* -0. */
                        octet[i][0] = 0x80;
                        octet[i][1] = 0x00;
                        octet[i][2] = 0x00;
                        octet[i][3] = 0x00;
                        octet[i][4] = 0x00;
                        octet[i][5] = 0x00;
                        octet[i][6] = 0x00;
                        octet[i][7] = 0x00;
                        continue;
                } else if( reel[i] < -1.e+308){ /* valeur exacte -1.79e+308 */
                        /* -Infinity */
                        octet[i][0] = 0xff;
                        octet[i][1] = 0xf0;
                        octet[i][2] = 0x00;
                        octet[i][3] = 0x00;
                        octet[i][4] = 0x00;
                        octet[i][5] = 0x00;
                        octet[i][6] = 0x00;
                        octet[i][7] = 0x00;
                        continue;
                } else {
                        dmantisse = (frexp(-reel[i],&exposant) - 0.5)*9.0071992547409920E15;
/*                                        *ldexp(1.,53) ; */
                        exposant += 1022;
                        octet[i][0] = ((exposant & 0x7ff) >> 4) | 128;
                }
          }
          else
          {
                if (reel[i] < 4.e-308 ){ /* valeur exacte 2.22e-308 */
                        /* +0. */
                        octet[i][0] = 0x00;
                        octet[i][1] = 0x00;
                        octet[i][2] = 0x00;
                        octet[i][3] = 0x00;
                        octet[i][4] = 0x00;
                        octet[i][5] = 0x00;
                        octet[i][6] = 0x00;
                        octet[i][7] = 0x00;
                        continue;
                } else if( reel[i] > 1.e+308){ /* valeur exacte 1.79e+308 */
                        /* +Infinity */
                        octet[i][0] = 0x7f;
                        octet[i][1] = 0xf0;
                        octet[i][2] = 0x00;
                        octet[i][3] = 0x00;
                        octet[i][4] = 0x00;
                        octet[i][5] = 0x00;
                        octet[i][6] = 0x00;
                        octet[i][7] = 0x00;
                        continue;

                } else {
                        dmantisse = (frexp(reel[i],&exposant) - 0.5)*9.0071992547409920E15;
/*                                        *ldexp(1.,53); */
                        exposant += 1022;
                        octet[i][0] = (exposant & 0x7ff) >> 4;
                }
          }
/*
          mantisse1 = (unsigned int) (dmantisse / ldexp(1.,32));
          mantisse2 = (unsigned int) (dmantisse - (double)mantisse1 * ldexp(1.,32));
*/
          mantisse1 = (unsigned int) (dmantisse / 4.294967296E9);
          mantisse2 = (unsigned int) (dmantisse - (double)mantisse1 * 4.294967296E9);
          octet[i][1] = ((exposant & 0x0f) << 4) | ((mantisse1 >> 16) & 0x0f);
          octet[i][2] = (mantisse1 >> 8) & 0xff ;
          octet[i][3] =  mantisse1 & 0xff ;
          octet[i][4] = (mantisse2 >> 24) & 0xff ;
          octet[i][5] = (mantisse2 >> 16) & 0xff ;
          octet[i][6] = (mantisse2 >> 8)  & 0xff ;
          octet[i][7] =  mantisse2 & 0xff ;
        } /* fin for */
}
/* fin double_to_IEEE_ASCII */


void IEEE_ASCII_to_double(double *reel,unsigned char octet[1000][8],int len)
{
  int exposant,i;
  int signe;
  double mantisse, decalage;
  for(i=0;i<len;i++)
  {
    /* signe */
    signe = octet[i][0] & 0x80;
    if (signe==0)
    {
      signe = 1;
    }
    else
    {
      signe = -1;
    }
    /* exposant */
    exposant  = (octet[i][0] & 0x7f) << 4;
    exposant += (octet[i][1] & 0xf0) >> 4;

    if (exposant==0)  /* +0. ou -0. */
    {
      reel[i] = 0.;
      continue;
    }

    exposant -= 1022;

    /* mantisse */
    /*  decalage = ldexp(1.,8); */
    decalage = 256.;
    mantisse = (octet[i][1] & 0x0f);
    mantisse = mantisse * decalage + octet[i][2];
    mantisse = mantisse * decalage + octet[i][3];
    mantisse = mantisse * decalage + octet[i][4];
    mantisse = mantisse * decalage + octet[i][5];
    mantisse = mantisse * decalage + octet[i][6];
    mantisse = mantisse * decalage + octet[i][7];
    /*  mantisse /= ldexp(1.,53); */
    mantisse /= 9.0071992547409920E15;
    mantisse += 0.5;

    /* nombre */
    reel[i] = signe * mantisse * ldexp(1.,exposant);
  } /* fin for */

} /* fin IEEE_ASCII_to_double */


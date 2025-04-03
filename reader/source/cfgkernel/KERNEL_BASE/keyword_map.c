/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#include <string.h>
#include "keyword_map.h"
#include "utils.h"


static int find_kwmap_pos(const keyword_map_t *kwmap_p,const char *skeyword,int min_pos,int max_pos);


keyword_map_t *new_kwmap(void) {
  return (keyword_map_t *)my_malloc(1,sizeof(keyword_map_t));
}


int find_kwmap_ikeyword(const keyword_map_t *kwmap_p,const char *skeyword) {
  int pos=find_kwmap_pos(kwmap_p,skeyword,0,kwmap_p->nb_pairs-1);
  if(pos>=kwmap_p->nb_pairs) return 0;
  
  if(strcmp(skeyword,(kwmap_p->pair_array)[pos]->skeyword)) return 0;
  return (kwmap_p->pair_array)[pos]->ikeyword;
}

const char *find_kwmap_skeyword(const keyword_map_t *kwmap_p,int ikeyword) {
  int i=0,found=0;
  while(!found && i<kwmap_p->nb_pairs) {
    if((kwmap_p->pair_array)[i]->ikeyword==ikeyword) found=1; else ++i;
  }
  return found ? (kwmap_p->pair_array)[i]->skeyword : NULL;
}

int add_kwmap_keyword(keyword_map_t *kwmap_p,const char *skeyword,int ikeyword){
  int              i,pos=find_kwmap_pos(kwmap_p,skeyword,0,kwmap_p->nb_pairs-1);
  keyword_pair_t **old_array,*pair_p;

  if(pos<kwmap_p->nb_pairs && !strcmp(skeyword,(kwmap_p->pair_array)[pos]->skeyword)) {
    return (kwmap_p->pair_array)[pos]->ikeyword;
  }

  old_array=kwmap_p->pair_array;
  kwmap_p->pair_array=(keyword_pair_t **)my_malloc(++(kwmap_p->nb_pairs),sizeof(keyword_pair_t *));

  for(i=0;i<pos;i++) (kwmap_p->pair_array)[i]=old_array[i];

  pair_p=(keyword_pair_t *)my_malloc(1,sizeof(keyword_pair_t));
  pair_p->skeyword=my_strcpy(pair_p->skeyword,skeyword);
  pair_p->ikeyword=ikeyword;
  (kwmap_p->pair_array)[pos]=pair_p;

  for(i=pos+1;i<kwmap_p->nb_pairs;i++) (kwmap_p->pair_array)[i]=old_array[i-1];
  my_free(old_array);

  return ikeyword;
}


int get_kwmap_size(const keyword_map_t *kwmap_p) {
  return kwmap_p->nb_pairs;
}


void get_kwmap_pair(const keyword_map_t *kwmap_p,int ind,char **skeyword_p,int *ikeyword_p) {
  const keyword_pair_t *pair_p=(kwmap_p->pair_array)[ind];
  *skeyword_p=pair_p->skeyword;
  *ikeyword_p=pair_p->ikeyword;
}


void clear_kwmap(keyword_map_t *kwmap_p) {
  int i;
  for(i=0;i<kwmap_p->nb_pairs;i++) {
    my_free((kwmap_p->pair_array)[i]->skeyword);
    my_free((kwmap_p->pair_array)[i]);
  }
  my_free(kwmap_p->pair_array);
  kwmap_p->nb_pairs=0;
}


/* static functions */

static int find_kwmap_pos(const keyword_map_t *kwmap_p,const char *skeyword,int min_pos,int max_pos) {
  int cmp_test;
  int mid_pos;

  if(kwmap_p->nb_pairs==0) return 0;
  if(strcmp(skeyword,(kwmap_p->pair_array)[max_pos]->skeyword)>0) return max_pos+1;
  if(min_pos==(max_pos-1)) {
    return strcmp(skeyword,(kwmap_p->pair_array)[min_pos]->skeyword)>0 ? max_pos : min_pos;
  } else if(min_pos==max_pos) {
    return strcmp(skeyword,(kwmap_p->pair_array)[max_pos]->skeyword)>0 ? max_pos+1 : max_pos;
  }
  mid_pos=(min_pos+max_pos)/2;
  cmp_test=strcmp(skeyword,(kwmap_p->pair_array)[mid_pos]->skeyword);
  if(cmp_test<0) return find_kwmap_pos(kwmap_p,skeyword,min_pos,mid_pos);
  if(cmp_test>0) return find_kwmap_pos(kwmap_p,skeyword,mid_pos,max_pos);
  return mid_pos;
}

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
#ifndef KEYWORD_MAP_H
#define KEYWORD_MAP_H


typedef struct keyword_pair_s {
  char *skeyword;
  int   ikeyword;
} keyword_pair_t;

typedef struct keyword_map_s {
  int              nb_pairs;
  keyword_pair_t **pair_array;
} keyword_map_t;


#ifdef __cplusplus
extern "C" {
#endif


/* 
   Creates a keyword_map_t 
*/
keyword_map_t *new_kwmap(void); 

/* 
   Finds a ikeyword from a skeyword
   Returns the ikeyword, or 0 if not found     
*/
int find_kwmap_ikeyword(const keyword_map_t *kwmap_p,const char *skeyword);

/* 
   Finds a skeyword from a ikeyword
   Returns the skeyword, or NULL if not found     
*/
const char *find_kwmap_skeyword(const keyword_map_t *kwmap_p,int ikeyword);

/* 
   Adds a new (skeyword,ikeyword) pair
   if the skeyword is already present, doesn't insert the new pair and returns the old ikeyword
   if the skeyword is not present, insert the pair and returns the ikeyword
*/
int add_kwmap_keyword(keyword_map_t *kwmap_p,const char *skeyword,int ikeyword);

/*
  Gets the number of contained pairs
*/
int get_kwmap_size(const keyword_map_t *kwmap_p);

/*
  Gets the pair at the position ind
*/
void get_kwmap_pair(const keyword_map_t *kwmap_p,int ind,char **skeyword_p,int *ikeyword_p);

/* 
   Clears the keyword_map_t
   Doesn't free kwmap_p
*/
void clear_kwmap(keyword_map_t *kwmap_p);


#ifdef __cplusplus
}
#endif


#endif /* KEYWORD_MAP_H */

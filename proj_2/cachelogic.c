// Brandon and Darshan
// GITHUB:
// Brandon: Dbrand098
// Darshan: DeeBul

#include "tips.h"
/* The following two functions are defined in util.c */

/* finds the highest 1 bit, and returns its position, else 0xFFFFFFFF */
unsigned int uint_log2(word w); 

/* return random int from 0..x-1 */
int randomint( int x );

/*
  This function allows the lfu information to be displayed

    assoc_index - the cache unit that contains the block to be modified
    block_index - the index of the block to be modified

  returns a string representation of the lfu information
 */
char* lfu_to_string(int assoc_index, int block_index)
{
  /* Buffer to print lfu information -- increase size as needed. */
  static char buffer[9];
  sprintf(buffer, "%u", cache[assoc_index].block[block_index].accessCount);

  return buffer;
}

/*
  This function allows the lru information to be displayed

    assoc_index - the cache unit that contains the block to be modified
    block_index - the index of the block to be modified

  returns a string representation of the lru information
 */
char* lru_to_string(int assoc_index, int block_index)
{
  /* Buffer to print lru information -- increase size as needed. */
  static char buffer[9];
  sprintf(buffer, "%u", cache[assoc_index].block[block_index].lru.value);

  return buffer;
}

/*
  This function initializes the lfu information

    assoc_index - the cache unit that contains the block to be modified
    block_number - the index of the block to be modified

*/
void init_lfu(int assoc_index, int block_index)
{
  cache[assoc_index].block[block_index].accessCount = 0;
}

/*
  This function initializes the lru information

    assoc_index - the cache unit that contains the block to be modified
    block_number - the index of the block to be modified

*/
void init_lru(int assoc_index, int block_index)
{
  cache[assoc_index].block[block_index].lru.value = 0;
}

/*
  This is the primary function you are filling out,
  You are free to add helper functions if you need them

  @param addr 32-bit byte address
  @param data a pointer to a SINGLE word (32-bits of data)
  @param we   if we == READ, then data used to return
              information back to CPU

              if we == WRITE, then data used to
              update Cache/DRAM
*/
/******************************************************************************************************
 *                              OUR CODE STARTS HERE                                                  *
 ******************************************************************************************************
*/


unsigned int tagMatch(unsigned int index, unsigned int tag) {
  for(int i = 0; i < assoc; i++) {
    if (cache[index].block[i].tag == tag) {
      return i;
    }
    else {
      return -1;
    }
  }
}


unsigned int replacementPolicy(unsigned int index) {
  switch(policy) {
    case 0: {
      // RANDOM: //for randomized replacement
      return randomint(assoc);
    }

    case 1: { // we need to fix lru replacement
      // LRU: {  //for LRU replacement policy

      int position = 0; 
      unsigned int max = 0;
      unsigned int tag; 

      // unsigned int curr;


      for(int i = 0; i < assoc; i++) {
        if(cache[index].block[i].valid == INVALID) {
        // if(cache[index].block[i].lru.value == 0) {
          position = i;
          // curr = cache[index].block[i].lru.value;  
          // printf("Position: %d", position); 
        // } 
        }
      }

      // for (int i = 0; i < assoc; i++) {
      //  if (curr) {
      //    cache[index].block[i].lru.value = 0;
      //    continue;
      //  }
      //  else {
      //    cache[index].block[i].lru.value++; 
      //    max = cache[index].block[i].lru.value; 

      //    if(cache[index].block[i].lru.value > max) {
      //      max = cache[index].block[i].lru.value; 
      //    }
      //  }
      // }


      // for (int j = 0; j < assoc; j++) {
      //  if(cache[index].block[j].lru.value == max) {
      //    cache[index].block[j].lru.value++;

      //    if(cache[index].block[j].lru.value > max) {
      //      // printf("MAX: %d", max);
      //      max = cache[index].block[j].lru.value;
      //    }
      //  }
      // }



      return position - 1;
  }
  }
}


void LRUupdate(unsigned int index, unsigned int block) { //Declared helper funtion - Darshan

  int max = 0;
  unsigned int curr = 0; 

  curr = max = cache[index].block[block].lru.value; 


  for (int i = 0; i < assoc; i++) {
    if(curr) {
      cache[index].block[block].lru.value = 0; 
      continue; 
    }
    else {
      cache[index].block[i].lru.value++;

      if (cache[index].block[i].lru.value > max) {
        max = cache[index].block[i].lru.value; 
      }
    }
  }

  // if(cache[index].block[block].lru.value == 0) {
  //   cache[index].block[block].lru.value = 1;
  // }

  // for (int i = 0; i < assoc; i++) {
  //   // printf("Forloop lru update:%d",i);
  //   if(cache[index].block[i].lru.value != 0 && i != 0 && cache[index].block[i].lru.value <= cache[index].block[block].lru.value) { // checks if the lru index is not the one that currently was used, thus the 0
  //     cache[index].block[i].lru.value = cache[index].block[i].lru.value + 1; // this will increment the ones that are not used
  //   }

  // }
}

// unsigned int blockCheck(unsigned int index){//helper function to compare the block
//   for(int i = 0; i < assoc; i++){
//     if(cache[index].block[i].data == cache[index].block[i].data){
//       //send some sort of signal back
//     }
//   }//end for loop

// }//end block check

void accessMemory(address addr, word* data, WriteEnable we)
{
  /* Declare variables here */

  unsigned int indexbits = uint_log2(set_count);        //declared index -Darshan
  unsigned int offsetbits = uint_log2(block_size);      //declared offset -Darshan
  unsigned int tagbits = 32 - indexbits - offsetbits;   //declared tag -Darshan

  // printf("TAGBITS: %d\n", uint_log2(tagbits));

  unsigned int tag = addr >> (offsetbits + indexbits);      //fixed
    // printf("Tag: %d\n", tag); wrong output

  unsigned int index = addr << tagbits;                     //fixed
               index = index >> (offsetbits + tagbits);     // needed to add offsetbits with tagbits because we shifted with tagbits first so we need to account for when shifting with offset bits
    // printf("Index: %d\n", index); wrong output 

  unsigned int offset = addr  << (tagbits + indexbits);      //fixed
               offset = offset >> (tagbits + indexbits);    //fixed
    
    int hit = tagMatch(index, tag); // determines if tag matches if not returns -1
    TransferUnit type = uint_log2(block_size); // need to gets bytes somehow so don't have to hard code it.Blocksize has transfer unit type therefore it dumps the core
    

  /* handle the case of no cache at all - leave this in */
  if(assoc == 0) {
    accessDRAM(addr, (byte*)data, QUADWORD_SIZE, we);
    return;
  }
  // ***********************************************************************************************************************************************************************************************************
  // Start of WRITE, READ, HIT, MISS
  // ***********************************************************************************************************************************************************************************************************

  if (we == READ) {
  int change = replacementPolicy(index);


    for (int i = 0; i < assoc; i++) {
      if (cache[index].block[i].valid == VALID) {
        if(hit != -1) {

          highlight_offset(index, i, offset, HIT);
          highlight_block(index, i);
          LRUupdate(index, i);
          memcpy((void*)data, cache[index].block[change].data + offset, type);

        }
      }
    }

    if (hit == -1) {
      highlight_offset(index, change, offset, MISS);
      LRUupdate(index, change);
      cache[index].block[change].valid = VALID;
      cache[index].block[change].tag = tag;
      accessDRAM(addr, cache[index].block[change].data, type, READ);
      highlight_block(index, change);
      memcpy((void*)data, cache[index].block[change].data + offset, type);
    }
  }

  if(we == WRITE) {
    int change = replacementPolicy(index);
 for (int i = 0; i < assoc; i++) {
      if (cache[index].block[i].valid == VALID) {
        if(hit != -1) {

          highlight_offset(index, i, offset, HIT);
          highlight_block(index, i);
          LRUupdate(index, i);
          memcpy((void*)data, cache[index].block[change].data + offset, type);

        }
      }
    }

    if (hit == -1) {
      highlight_offset(index, change, offset, MISS);
      LRUupdate(index, change);
      cache[index].block[change].valid = VALID;
      cache[index].block[change].tag = tag;
      accessDRAM(addr, cache[index].block[change].data, type, READ);
      highlight_block(index, change);
      memcpy((void*)data, cache[index].block[change].data + offset, type);
    }

    if(memory_sync_policy == WRITE_BACK) {
      
      if(cache[index].block[change].dirty == DIRTY) {
        accessDRAM(addr, cache[index].block[change].data, type, WRITE);
      }
        memcpy((void*)data, cache[index].block[change].data + offset, type);
        LRUupdate(index, change);
        cache[index].block[change].valid = VALID;
        cache[index].block[change].dirty = DIRTY;
        cache[index].block[change].tag = tag;
    }

    else if (memory_sync_policy == WRITE_THROUGH) {
      memcpy((void*)data, cache[index].block[change].data + offset, type);
      LRUupdate(index, change);
      cache[index].block[change].valid = VALID;
      cache[index].block[change].dirty = DIRTY;
      cache[index].block[change].tag = tag;
      accessDRAM(addr, cache[index].block[change].data, type, WRITE);

    }
  }



//   if (memory_sync_policy == WRITE_BACK) {

//   if(hit == -1) { // when there is a miss

//     accessDRAM(addr, cache[index].block[change].data, type, READ); 
//     cache[index].block[change].tag = tag; 
//     highlight_offset(index, change, offset, MISS);

//     memcpy((void*)data, cache[index].block[change].data + offset, type);
//     LRUupdate(index, change);
//     highlight_block(index, change);
//   }

 
//     if(hit != -1) {

//         highlight_offset(index, change, offset, HIT);

//         highlight_block(index, change);
//         LRUupdate(index, change);
//         memcpy((void*)data, cache[index].block[change].data + offset, type);
//     }


// //create a program that does not have jump or branch

//      // make more than 32 lines of code 
//   }

    // if(cache[index].block[tag].dirty == DIRTY && memory_sync_policy == WRITE_BACK) {
    //  accessDRAM(addr, cache[index].block[tag]. data, type, WRITE);
    // }

  
 //   if(memory_sync_policy == WRITE_BACK) {



  //  if (cache[index].block[change].dirty == DIRTY) {
  //    accessDRAM(addr, cache[index].block[change].data, type, WRITE);
  //  }

  //  memcpy(cache[index].block[change].data, (void*) data , type);
  //  LRUupdate(index, change);
  //  cache[index].block[change].tag = tag; 
  //  cache[index].block[change].dirty = DIRTY;
  //  cache[index].block[change].valid = VALID;
  // }


  //  case 1: {
  //    memcpy(cache[index].block[change].data, (void*) data, type); 
  //    LRUupdate(index, change); 
  //    cache[index].block[change].tag = tag; 
  //    cache[index].block[change].dirty = DIRTY; 
  //    cache[index].block[change].valid = VALID; 
  //    accessDRAM(addr, cache[index].block[change].data, type, WRITE);
  //  }

  // }
// how do we compare the blocks so then we can use accessdram?
// can we do the type thing that we just did? 
// how do we the highlight the block that has been a hit?

  /*
  You need to read/write between memory (via the accessDRAM() function) and
  the cache (via the cache[] global structure defined in tips.h)

  Remember to read tips.h for all the global variables that tell you the
  cache parameters

  The same code should handle random, LFU, and LRU policies. Test the policy
  variable (see tips.h) to decide which policy to execute. The LRU policy
  should be written such that no two blocks (when their valid bit is VALID)
  will ever be a candidate for replacement. In the case of a tie in the
  least number of accesses for LFU, you use the LRU information to determine
  which block to replace.

  Your cache should be able to support write-through mode (any writes to
  the cache get immediately copied to main memory also) and write-back mode
  (and writes to the cache only gets copied to main memory when the block
  is kicked out of the cache.

  Also, cache should do allocate-on-write. This means, a write operation
  will bring in an entire block if the block is not already in the cache.

  To properly work with the GUI, the code needs to tell the GUI code
  when to redraw and when to flash things. Descriptions of the animation
  functions can be found in tips.h
  */

  /* Start adding code here */


  /* This call to accessDRAM occurs when you modify any of the
     cache parameters. It is provided as a stop gap solution.
     At some point, ONCE YOU HAVE MORE OF YOUR CACHELOGIC IN PLACE,
     THIS LINE SHOULD BE REMOVED.
  */
  //accessDRAM(addr, (byte*)data, QUADWORD_SIZE, we);
}


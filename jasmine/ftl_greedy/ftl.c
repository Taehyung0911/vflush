// Copyright 2011 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// GreedyFTL source file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// - support POR
//  + fixed metadata area (Misc. block/Map block)
//  + logging entire FTL metadata when each ATA commands(idle/ready/standby) was issued
//

#include "jasmine.h"

//----------------------------------
// macro
//----------------------------------
#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK    (((PAGE_MAP_BYTES / NUM_BANKS) + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE)
#define META_BLKS_PER_BANK  (1 + 1 + MAPBLKS_PER_BANK) // include block #0, misc block
#define HASH_NUM			128
#define NUM_BITMAP			NUM_TAG_BUFFER / 32
// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT     ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)

#define MT_CF_BIT			((PAGE_MAP_BYTES / BYTES_PER_PAGE) / 32) + 1

//----------------------------------
// metadata structure
//----------------------------------
typedef struct _ftl_statistics
{
    UINT32 gc_cnt;
    UINT32 page_wcount; // page write count
}ftl_statistics;

typedef struct _misc_metadata
{
    UINT32 cur_write_vpn; // physical page for new write
    UINT32 cur_miscblk_vpn; // current write vpn for logging the misc. metadata
    UINT32 cur_mapblk_vpn[MAPBLKS_PER_BANK]; // current write vpn for logging the age mapping info.
    UINT32 gc_vblock; // vblock number for garbage collection
    UINT32 free_blk_cnt; // total number of free block count
    UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC
}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;

/* FIFO pointer addr */
#if TAG_BUFFER && SYNC_EVICTION
UINT32				  bitmap[NUM_BITMAP];
UINT32				  tag_list[4];
UINT32				  tag_last[4];
UINT32				  evictRatio[4];
UINT32				  syncFreq[4];
UINT32				  syncTotal;
UINT32				  tag_entries[4];
UINT32				  fifo_start;
UINT32				  fifo_end;
#elif TAG_BUFFER
UINT32				  bitmap[NUM_BITMAP];
UINT32				  tag_list[4];
UINT32				  tag_last[4];
UINT32				  fifo_start;
UINT32				  fifo_end;
#else
UINT32				  start_buf_id;
UINT32				  end_buf_id;
#endif
UINT32				  hash_list[HASH_NUM];
UINT32				  num_entries;

UINT32				  mapTableRef[MT_CF_BIT];
#if TAG_MAP_TABLE
UINT32				  map_index[4];
#endif


#if OPTION_UART_DEBUG
UINT8	uart = 1;
#else
UINT8	uart = 0;
#endif

#if SYNC_EVICTION
UINT8	syncEvic	= 1;
#else
UINT8	syncEvic	= 0;
#endif
UINT32	num_flushed_map = 0;
UINT16	flushed_map = 0; //
UINT32	map_full = 0;
UINT32  flush_period = 0;  // for periodic flushing DRAM data
/* ----------------- */

//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 ~ #31: page mapping table
// block #32: a free block for gc
// block #33~: user data blocks

//----------------------------------
// macro functions
//----------------------------------
#define is_full_all_blks(bank)  (g_misc_meta[bank].free_blk_cnt == 1)
#define inc_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt--)
#define dec_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt++)
#define inc_mapblk_vpn(bank, mapblk_lbn)    (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank)               (g_misc_meta[bank].cur_miscblk_vpn++)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn)             ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank)         (g_bad_blk_count[bank])
#define get_cur_write_vpn(bank)       (g_misc_meta[bank].cur_write_vpn)
#define set_new_write_vpn(bank, vpn)  (g_misc_meta[bank].cur_write_vpn = vpn)
#define get_gc_vblock(bank)           (g_misc_meta[bank].gc_vblock)
#define set_gc_vblock(bank, vblock)   (g_misc_meta[bank].gc_vblock = vblock)
#define set_lpn(bank, page_num, lpn)  (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num] = lpn)
#define get_lpn(bank, page_num)       (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num])
#define get_miscblk_vpn(bank)         (g_misc_meta[bank].cur_miscblk_vpn)
#define set_miscblk_vpn(bank, vpn)    (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define get_mapblk_vpn(bank, mapblk_lbn)      (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn])
#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static void   sanity_check(void);
static void   load_pmap_table(void);
static void   load_misc_metadata(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   logging_pmap_table(void);
static void   logging_misc_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
#if TAG_MAP_TABLE && TAG_BUFFER
static void   set_vpn(UINT32 const lpn, UINT32 const vpn, UINT8 const tag);
#else
static void   set_vpn(UINT32 const lpn, UINT32 const vpn);
#endif
static void   garbage_collection(UINT32 const bank);
static void   set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static BOOL32 check_format_mark(void);
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock);
static UINT32 get_vpn(UINT32 const lpn);
static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank);
/* Declare functions */
#if TAG_BUFFER //&& SYNC_EVICTION == 0
static void remove_fifo_node(node* const flush_node);
#endif
#if SYNC_EVICTION
static void evict_syncIntensive_tagList(void);
#endif
#if TAG_BUFFER
static void insert_page_with_tag(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors, UINT8 const tag);
static void tag_flush(UINT8 const tag);
static void remove_bitmap(UINT32 const buf_id);
static void node_info_update(UINT32 const lpn, UINT32 const buf_id, UINT8 const tag);
static UINT32 find_freeSpace(void);
static void evict_pages_from_fifo(UINT8 const evict_num);
static void remove_tag_node(node* const flush_node);
#else
static void insert_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
static void full_flush(UINT8 const tag);
static void node_info_update(UINT32 const lpn, UINT32 const buf_id);
static void evict_pages_to_flash(UINT8 const evict_num);
#endif
static void hash_update(UINT32 const lpn, UINT32 const buf_addr);
static void fill_page_holes(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
static UINT32 is_lpn_existed(UINT32 const lpn);
static void hash_invalidation(UINT32 const lpn);
#if TAG_MAP_TABLE
static void reflect_page(UINT32 const lpn, UINT32 const buf_addr, UINT8 const tag);
#else
static void reflect_page(UINT32 const lpn, UINT32 const buf_addr);
#endif
/* ----------------- */

static void sanity_check(void)
{
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
        + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES;

    if ((dram_requirement > DRAM_SIZE) || // DRAM metadata size check
        (sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
    {
        led_blink();
        while (1);
    }
}
static void build_bad_blk_list(void)
{
	UINT32 bank, num_entries, result, vblk_offset;
	scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

	mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

	disable_irq();

	flash_clear_irq();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
		SETREG(FCP_BANK, REAL_BANK(bank));
		SETREG(FCP_OPTION, FO_E);
		SETREG(FCP_DMA_ADDR, (UINT32) scan_list);
		SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
		SETREG(FCP_COL, 0);
		SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
		SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

		SETREG(FCP_ISSUE, NULL);
		while ((GETREG(WR_STAT) & 0x00000001) != 0);
		while (BSP_FSM(bank) != BANK_IDLE);

		num_entries = NULL;
		result = OK;

		if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
		{
			result = FAIL;
		}
		else
		{
			UINT32 i;

			num_entries = read_dram_16(&(scan_list->num_entries));

			if (num_entries > SCAN_LIST_ITEMS)
			{
				result = FAIL;
			}
			else
			{
				for (i = 0; i < num_entries; i++)
				{
					UINT16 entry = read_dram_16(scan_list->list + i);
					UINT16 pblk_offset = entry & 0x7FFF;

					if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
					{
						#if OPTION_REDUCED_CAPACITY == FALSE
						result = FAIL;
						#endif
					}
					else
					{
						write_dram_16(scan_list->list + i, pblk_offset);
					}
				}
			}
		}

		if (result == FAIL)
		{
			num_entries = 0;  // We cannot trust this scan list. Perhaps a software bug.
		}
		else
		{
			write_dram_16(&(scan_list->num_entries), 0);
		}

		g_bad_blk_count[bank] = 0;

		for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
		{
			BOOL32 bad = FALSE;

			#if OPTION_2_PLANE
			{
				UINT32 pblk_offset;

				pblk_offset = vblk_offset * NUM_PLANES;

                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}

				pblk_offset = vblk_offset * NUM_PLANES + 1;

                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#else
			{
                // fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
			#endif

			if (bad)
			{
				g_bad_blk_count[bank]++;
				set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
			}
		}
	}
}

void ftl_open(void)
{
    // debugging example 1 - use breakpoint statement!
    /* *(UINT32*)0xFFFFFFFE = 10; */

    /* UINT32 volatile g_break = 0; */
    /* while (g_break == 0); */
	UINT16 tmp	= 0;

	led(0);
    sanity_check();
    //----------------------------------------
    // read scan lists from NAND flash
    // and build bitmap of bad blocks
    //----------------------------------------
	build_bad_blk_list();

    //----------------------------------------
	// If necessary, do low-level format
	// format() should be called after loading scan lists, because format() calls is_bad_block().
    //----------------------------------------
/* 	if (check_format_mark() == FALSE) */
	if (TRUE)
	{
        uart_print("do format");
		format();
        uart_print("end format");
	}
    // load FTL metadata
    else
    {
        load_metadata();
    }
	g_ftl_read_buf_id = 0;
	g_ftl_write_buf_id = 0;

	/* clear variables */
	num_entries		= 0;
	while(tmp != HASH_NUM){
		hash_list[tmp] = NULL;
		tmp++;
	}

#if TAG_BUFFER
	for(tmp=0;tmp<NUM_BITMAP;tmp++)
		bitmap[tmp] = 0x00;
	for(tmp=0;tmp<4;tmp++){
		tag_list[tmp] = 0x00;
		tag_last[tmp] = 0x00;
	}
//#if SYNC_EVICTION == 0
	fifo_start	= 0;
	fifo_end	= 0;
//#endif
#if SYNC_EVICTION
	for(tmp=0;tmp<4;tmp++){
		tag_entries[tmp] = 0;
	}
#endif

#else
	start_buf_id	= 0;
	end_buf_id		= 0;
#endif

	uart_printf("MAPBLKS_PER_BANK: %d, NUM_BANKS: %d, MT_CF_BIT: %d",MAPBLKS_PER_BANK, NUM_BANKS, MT_CF_BIT);
//	ptimer_start();
//	uart_printf("page: %d", BYTES_PER_PAGE);
	/* --------------- */

    // This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
    flash_clear_irq();

    SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
	SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

	enable_irq();
}
#if TAG_BUFFER
void ftl_flush(UINT8 const tag)
#else
void ftl_flush(UINT8 const tag)
#endif
{
	//ptimer_start();
#if TAG_BUFFER
	tag_flush(tag);
#else
	full_flush(tag);
#endif
	//ptimer_stop_and_uart_print();

    //logging_pmap_table();
    //logging_misc_metadata();
    /* ptimer_stop_and_uart_print(); */
}
#if TAG_BUFFER == 0
static void full_flush(UINT8 const tag)
{
	node* flush_node = (node*) TAG_BUFFER_NODE_PTR(start_buf_id);
	UINT32 lpn = read_dram_32(&(flush_node->lpn));
	UINT16 tmp;
	UINT32 elapsed, elapsed_1;
	UINT32 flushnum = num_entries;
	//ptimer_start();
	//elapsed = ptimer_stat();
	while(num_entries && flush_node != NULL){
#if TAG_MAP_TABLE == 0
		reflect_page(lpn, TAG_BUFFER_PTR(start_buf_id));
#endif
		start_buf_id = (start_buf_id + 1) % NUM_TAG_BUFFER;
		flush_node = (node*) TAG_BUFFER_NODE_PTR(start_buf_id);
		lpn	= read_dram_32(&(flush_node->lpn));
		num_entries--;
	}
	
	//elapsed = ptimer_stat() - elapsed;
	//elapsed_1 = ptimer_stat();
	// clear hash value
	for(tmp=0; tmp < HASH_NUM; tmp++)
		hash_list[tmp] = NULL;

	start_buf_id	= 0;
	end_buf_id		= 0;
	
	flushed_map = 0;

	//num_entries = 0;

	//if(uart)
	//	ptimer_stop_and_uart_print();
	//if(uart)
	//	ptimer_start();
	logging_pmap_table();
	//logging_misc_metadata();
	//elapsed_1 = ptimer_stat() - elapsed_1;

	//uart_printf("%d, %d, %d", tag, elapsed, elapsed_1);
	//uart_printf("*%d*, %d, %d", tag, flushnum, flushed_map);
	num_entries=0;
	flushed_map=0;
}
#else
static void tag_flush(UINT8 const tag)
{
	node* flush_node = (node*) fifo_start;
	UINT32 lpn, buf_id;
	UINT16 tmp;

	UINT32 elapsed, elapsed_1;
	UINT16 flushed[3] = {0,};
	UINT32 minimum = 0;

	ptimer_start();
	elapsed = ptimer_stat();
#if SYNC_EVICTION
	if(ptimer_stat() < 10000000){
		syncTotal++;
		syncFreq[tag]++;
	}
	else{
		for(UINT8 i = 1; i < 4; i++){
			evictRatio[i] = syncFreq[i];
			syncFreq[i] = 0;
		}
		
		evictRatio[0] = syncTotal;
		syncTotal = 0;

	//	ptimer_start();
	}
#endif
	
	//elapsed = ptimer_stat();

//	minimum = num_entries;

	while(flush_node != NULL){ 		// flush buffer entries
		UINT8 tag_value	= (UINT8)read_dram_32(&(flush_node->tag));
		lpn		= read_dram_32(&(flush_node->lpn));
		buf_id	= read_dram_32(&(flush_node->buf_id));
#if TAG_MAP_TABLE
		reflect_page(lpn, TAG_BUFFER_PTR(buf_id), tag);
#else
		reflect_page(lpn, TAG_BUFFER_PTR(buf_id));
#endif
//		remove_tag_node(flush_node);
		remove_bitmap(buf_id);
		hash_invalidation(lpn);

//#if SYNC_EVICTION == 0
//			remove_fifo_node(flush_node);
//#endif
	
		flush_node = (node*) read_dram_32(&(flush_node->fifo_next));
		num_entries--;
		
		flushed[tag_value]++;
	}

	fifo_start=NULL;
	fifo_end=NULL;
	for(UINT8 i = 0; i < 3; i++){
		tag_list[i] = NULL;
		tag_last[i] = NULL;
	}
/*
	if(flushed[tag] < ((NUM_TAG_BUFFER * 5) / 100) && num_entries > 0){  // (/ 20) is 5% of the write buffer.
		minimum = ((NUM_TAG_BUFFER * 5) / 100) - flushed[tag];
		if(minimum < num_entries)
			evict_pages_from_fifo(minimum);
		else
			evict_pages_from_fifo(num_entries);
	}
*/
//	tag_list[tag]	= NULL;
//	tag_last[tag]	= NULL;
#if SYNC_EVICTION
	tag_entries[tag] = 0;
#endif

	elapsed = ptimer_stat() - elapsed;
	
	elapsed_1 = ptimer_stat();
#if TAG_MAP_TABLE
	//if(map_index[tag] != (BYTES_PER_PAGE / (sizeof(UINT32) * 2))){
	if(map_full != 0xffffffff){
		UINT32	mapblk_lbn = MAPBLKS_PER_BANK - 1;
		UINT32	bank = NUM_BANKS - (UINT32)tag - 1;
		UINT32	mapblk_vpn;

		inc_mapblk_vpn(bank, mapblk_lbn);
		mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
		
		if ((mapblk_vpn % PAGES_PER_BLK) == 0){ // if there is no free page, then erase old map block
			nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

			set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);

			mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
		}
		
		nand_page_ptprogram(bank, mapblk_vpn / PAGES_PER_BLK, mapblk_vpn % PAGES_PER_BLK, 0,
				SECTORS_PER_PAGE, LOGGING_SPACE_PTR(tag, 0));
		flash_finish();

		flushed_map = 1;
	}
	else{
		map_full = 0;
		flushed_map = 0;
		logging_pmap_table();
	}
#else
	flushed_map = 0;
	logging_pmap_table();
#endif
	elapsed_1 = ptimer_stat() - elapsed_1;
	uart_printf("%d %d %d %d %d %d", elapsed, elapsed_1, flushed[0], flushed[1], flushed[2], flushed_map);
	num_flushed_map = 0;
	//elapsed_1 = ptimer_stat() - elapsed_1;
	//uart_printf("%d, %d, %d", tag, elapsed, elapsed_1);
	//uart_printf("*%d*, %d, %d, %d, %d", tag, flushed[0], flushed[1], flushed[2],flushed_map);
	flushed_map = 0;
}
static void remove_bitmap(UINT32 const buf_id)
{
	UINT32	bit_num = buf_id / 32;
	UINT32	bit_offset = buf_id % 32;
	
	bitmap[bit_num] ^= (0x00000001 << bit_offset);

}
#endif
#if TAG_BUFFER// && SYNC_EVICTION == 0
static void remove_fifo_node(node* const flush_node)
{
	node* prev	= (node*) read_dram_32(&(flush_node->fifo_prev));
	node* next	= (node*) read_dram_32(&(flush_node->fifo_next));

	if(prev != NULL){
		if(next != NULL){
			write_dram_32(&(prev->fifo_next), (UINT32) next);
			write_dram_32(&(next->fifo_prev), (UINT32) prev);
		}
		else{
			write_dram_32(&(prev->fifo_next), NULL);
			fifo_end = (UINT32) prev;
		}
	}
	else{
		if(next != NULL){
			write_dram_32(&(next->fifo_prev), NULL);
			fifo_start = (UINT32) next;
		}
		else{
			fifo_start = NULL;
			fifo_end = NULL;
		}
	}
	
}
#endif
// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
    ASSERT(lba + num_sectors <= NUM_LSECTORS);
    ASSERT(num_sectors > 0);

    //ftl_write(lba, num_sectors);
}
#if TAG_BUFFER
void ftl_read(UINT32 const lba, UINT32 const num_sectors, UINT8 const tag)
#else
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
#endif
{
    UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, vpn;

	UINT32 lpn_addr;

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    while (remain_sects != 0)
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_read = remain_sects;
        }
        else
        {
            num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
        }

		/* Buffer hit check */
		if((lpn_addr = is_lpn_existed(lpn))){  // cache hit
			node* hit_node = (node*) lpn_addr;
			UINT32 next_read_buf_id	= (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;
			while(next_read_buf_id == GETREG(SATA_RBUF_PTR));

			mem_copy(RD_BUF_PTR(g_ftl_read_buf_id) + (sect_offset * BYTES_PER_SECTOR),
					TAG_BUFFER_PTR(read_dram_32(&(hit_node->buf_id))) + (sect_offset * BYTES_PER_SECTOR),
					num_sectors_to_read * BYTES_PER_SECTOR);
			flash_finish();

			SETREG(BM_STACK_RDSET, next_read_buf_id);
			SETREG(BM_STACK_RESET, 0x02);
			
			g_ftl_read_buf_id = next_read_buf_id;
			sect_offset = 0;
			remain_sects -= num_sectors_to_read;
			lpn++;
			
			continue;
		}
		/* ---------------- */
        bank = get_num_bank(lpn); // page striping
        vpn  = get_vpn(lpn);
        CHECK_VPAGE(vpn);

        if (vpn != NULL)
        {
            nand_page_ptread_to_host(bank,
                                     vpn / PAGES_PER_BLK,
                                     vpn % PAGES_PER_BLK,
                                     sect_offset,
                                     num_sectors_to_read);
        }
        // The host is requesting to read a logical page that has never been written to.
        else
        {
			UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

			#if OPTION_FTL_TEST == 0
			while (next_read_buf_id == GETREG(SATA_RBUF_PTR));	// wait if the read buffer is full (slow host)
			#endif

            // fix bug @ v.1.0.6
            // Send 0xFF...FF to host when the host request to read the sector that has never been written.
            // In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
            // However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
			mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset*BYTES_PER_SECTOR,
                         0xFFFFFFFF, num_sectors_to_read*BYTES_PER_SECTOR);

            flash_finish();

			SETREG(BM_STACK_RDSET, next_read_buf_id);	// change bm_read_limit
			SETREG(BM_STACK_RESET, 0x02);				// change bm_read_limit

			g_ftl_read_buf_id = next_read_buf_id;
        }
        sect_offset   = 0;
        remain_sects -= num_sectors_to_read;
        lpn++;
    }
}
#if TAG_BUFFER
void ftl_write(UINT32 const lba, UINT32 const num_sectors, UINT8 const tag)
#else
void ftl_write(UINT32 const lba, UINT32 const num_sectors)
#endif
{
    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;
    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    while (remain_sects != 0)
    {
        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_write = remain_sects;
        }
        else
        {
            num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
        }
        // single page write individually
        //write_page(lpn, sect_offset, num_sectors_to_write);
#if TAG_BUFFER
		insert_page_with_tag(lpn, sect_offset, num_sectors_to_write, tag); //TODO
#else
		insert_page(lpn, sect_offset, num_sectors_to_write); //TODO
#endif

        sect_offset   = 0;
        remain_sects -= num_sectors_to_write;
        lpn++;
    }
}
#if TAG_BUFFER
static void insert_page_with_tag(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors, UINT8 tag)
{
	// TODO maybe the size of tag value is 8 bytes
	UINT32 lpn_addr;
	UINT16 tmp;

	if((lpn_addr = is_lpn_existed(lpn)) == NULL){
		if(num_entries == NUM_TAG_BUFFER){
#if SYNC_EVICTION
			evict_syncIntensive_tagList();
#elif TAG_BUFFER
			evict_pages_from_fifo(64);	//FIXME evict entries from fifo_start		
#endif
		}
		if(num_sectors != SECTORS_PER_PAGE)
			fill_page_holes(lpn, sect_offset, num_sectors);
		lpn_addr = find_freeSpace();
		while(g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR));
		mem_copy(TAG_BUFFER_PTR(lpn_addr), WR_BUF_PTR(g_ftl_write_buf_id), BYTES_PER_PAGE);

		node_info_update(lpn, lpn_addr, tag); //TODO
		g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;
		SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
		SETREG(BM_STACK_RESET, 0x01);

		num_entries++;
#if SYNC_EVICTION
			tag_entries[tag]++;
#endif
	}
	else{
		node* hit_node = (node*) lpn_addr;
		while(g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR));
		mem_copy(TAG_BUFFER_PTR(read_dram_32(&(hit_node->buf_id))) + (sect_offset * BYTES_PER_SECTOR),
				WR_BUF_PTR(g_ftl_write_buf_id) + (sect_offset * BYTES_PER_SECTOR),
				num_sectors * BYTES_PER_SECTOR);
		g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;

		SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
		SETREG(BM_STACK_RESET, 0x01);

	}
}
#endif
#if SYNC_EVICTION
static void evict_syncIntensive_tagList(void)
{
	//TODO
	node* evict_node;
	UINT32	num_of_eviction = 64;
	UINT32	evictNUM[4];
	UINT8	evict_count, flag;

	flag = 0;

	for (UINT8 i = 1; i < 4; i++){
		float evict_ratio	= (float) evictRatio[i] / (float) evictRatio[0];
		float buffer_ratio	= (float) tag_entries[i] / (float) num_entries;
		//uart_printf("tag: %d, eR: %d, tS: %d, tE: %d, nE: %d", i, evictRatio[i], evictRatio[0], tag_entries[i], num_entries);
		//uart_printf("evict_ratio:%f, buffer_ratio:%f", evict_ratio, buffer_ratio);

		if (evict_ratio > buffer_ratio){
			evictNUM[i] = (int) (num_of_eviction * evict_ratio);

			evictNUM[i] = (evictNUM[i] <= tag_entries[i]) ? evictNUM[i] : tag_entries[i];

			//uart_printf("tag: %d, evict num: %d", i, evictNUM[i]);
		}
		else
			evictNUM[i] = 0;
	}
	//uart_printf("--------");
/*
	for(UINT8 i = 1; i < 4; i++){
		if(evictRatio[i] != 0){
			evictNUM[i] = num_of_eviction * evictRatio[i] / evictRatio[0];

			evictNUM[i] = (evictNUM[i] <= tag_entries[i]) ? evictNUM[i] : tag_entries[i];

			flag = 1;
		}
		else
			evictNUM[i] = 0;
	}

	// if there are no tags which trigger fsync, ,,,,, fifo?
	
	if(flag){
		evictNUM[0] = num_of_eviction - (evictNUM[1] + evictNUM[2] + evictNUM[3]);

		// compare with the number of nodes for each tag
	}
	else{
		evictNUM[0] = num_of_eviction / 4;
		evictNUM[1] = num_of_eviction / 4;
		evictNUM[2] = num_of_eviction / 4;
		evictNUM[3] = num_of_eviction / 4;
	}
*/ 	
	
	
	for(UINT8 a = 0; a < 3; a++){
		UINT8 i = 3 - a;

		if(evictNUM[i] == 0)
			continue;
		else
			num_of_eviction -= evictNUM[i];
			
		evict_node = (node*) tag_list[i];

/*		if(evict_node == 0x00){
			for(UINT8 k = i; k > 0; k--)
				evictNUM[k - 1] += evictNUM[i] / i;

			continue;
		}
*/
			

		for(UINT16 j = 0; j < evictNUM[i]; j++){
			UINT32 lpn	= read_dram_32(&(evict_node->lpn));
			UINT32 buf_id	= read_dram_32(&(evict_node->buf_id));
#if TAG_MAP_TABLE
			reflect_page(lpn, TAG_BUFFER_PTR(buf_id), i);
#else
			reflect_page(lpn, TAG_BUFFER_PTR(buf_id));
#endif
			hash_invalidation(lpn);
			remove_bitmap(buf_id);
			remove_fifo_node(evict_node);

			evict_node = (node*) read_dram_32(&(evict_node->tag_next));

			if(evict_node != NULL)
				tag_list[i]	= (UINT32) evict_node;
			else{
				tag_list[i] = NULL;
				tag_last[i] = NULL;
			}

			num_entries--;
			tag_entries[i]--;
/*
			if(evict_node == NULL && i != 0){
				for(UINT8 k= i; k>0; k--){
					evictNUM[i - 1] += (evictNUM[i] - j - 1) / i;
				}

				break;
			}
*/
		}
	}

	if(num_of_eviction)
		evict_pages_from_fifo(num_of_eviction);
}
#endif
#if TAG_BUFFER
static UINT32 find_freeSpace(void)
{
	// TODO marking target space to 1
	UINT16 temp, tmp;
	UINT32 free_space;

	for(temp=0; temp<NUM_BITMAP; temp++){
		if(bitmap[temp] != 0xFFFFFFFF){
			tmp = 0;
			while(1){
				if(bitmap[temp] & (0x01 << tmp))
					tmp++;
				else{
					bitmap[temp] ^= (0x01 << tmp);
					break;
				}
			}
			
			return (temp << 5) + tmp;
		}
		else
			continue;
	}

	return 0xffffffff;
}
#else
static void insert_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
	UINT32 lpn_addr;
	UINT16 tmp;

	if((lpn_addr = is_lpn_existed(lpn)) == NULL){	// cache miss
		if(num_entries == NUM_TAG_BUFFER){
			evict_pages_to_flash(2);	//
		}

		if(num_sectors != SECTORS_PER_PAGE)
			fill_page_holes(lpn, sect_offset, num_sectors);	// fill the page hole
		while(g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR));
		mem_copy(TAG_BUFFER_PTR(end_buf_id), WR_BUF_PTR(g_ftl_write_buf_id), BYTES_PER_PAGE);

		node_info_update(lpn, end_buf_id);

		end_buf_id = (end_buf_id + 1) % NUM_TAG_BUFFER;
		g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;

		SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
		SETREG(BM_STACK_RESET, 0x01);

		num_entries++;
	}
	else{	// cache hit
		node* hit_node = (node*) lpn_addr;
		while(g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR));
		mem_copy(TAG_BUFFER_PTR(read_dram_32(&(hit_node->buf_id))) + (sect_offset * BYTES_PER_SECTOR),
				WR_BUF_PTR(g_ftl_write_buf_id) + (sect_offset * BYTES_PER_SECTOR),
				num_sectors * BYTES_PER_SECTOR);
		g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;

		SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
		SETREG(BM_STACK_RESET, 0x01);
	}
}
#endif
#if TAG_BUFFER
static void node_info_update(UINT32 const lpn, UINT32 const buf_id, UINT8 const tag)
#else
static void node_info_update(UINT32 const lpn, UINT32 const buf_id)
#endif
{
	node* buf_node;
#if TAG_BUFFER
	node* last_node;
#endif
	buf_node = (node*)TAG_BUFFER_NODE_PTR(buf_id);
	
	write_dram_32(&(buf_node->lpn), lpn);
	write_dram_32(&(buf_node->hash_next), NULL);
	write_dram_32(&(buf_node->buf_id), buf_id);
#if TAG_BUFFER
	write_dram_32(&(buf_node->tag_next), NULL);
	if(tag_list[tag] == NULL){
		tag_list[tag] = (UINT32) buf_node;
		tag_last[tag] = (UINT32) buf_node;
	}
	else{
		last_node = (node*) tag_last[tag];
		
		write_dram_32(&(last_node->tag_next), (UINT32) buf_node);
		tag_last[tag] = (UINT32) buf_node;
	}
//#endif
//#if TAG_BUFFER && (SYNC_EVICTION == 0)
	//tag_last[tag] = (UINT32) buf_node;
	//if (!syncEvic){
	if(1){
		UINT32 _tag = (UINT32) tag;
		write_dram_32(&(buf_node->tag), _tag);

		if(fifo_start == NULL){
			fifo_start	= (UINT32) buf_node;
			fifo_end	= (UINT32) buf_node;
			write_dram_32(&(buf_node->fifo_next), NULL);
			write_dram_32(&(buf_node->fifo_prev), NULL);
		}
		else{
			last_node = (node*) fifo_end;
			write_dram_32(&(last_node->fifo_next), (UINT32) buf_node);
			write_dram_32(&(buf_node->fifo_next), NULL);
			write_dram_32(&(buf_node->fifo_prev), (UINT32) last_node);
			fifo_end = (UINT32) buf_node;
		}
	}
#endif
	hash_update(lpn, (UINT32)(buf_node));
	
}
static void hash_update(UINT32 const lpn, UINT32 const buf_addr)
{
	UINT32 key = lpn % HASH_NUM;
	node* cur_node = (node*) buf_addr;
	
	if(hash_list[key] == NULL){
		hash_list[key]	= buf_addr;
		write_dram_32(&(cur_node->hash_prev), NULL);
	}
	else{
		node* next_addr;
		node* hash_node = (node*) hash_list[key];
		while((next_addr = (node*) read_dram_32(&(hash_node->hash_next)))){
			hash_node = next_addr;
		}

		write_dram_32(&(hash_node->hash_next), buf_addr);
		write_dram_32(&(cur_node->hash_prev), hash_node);
	}
}
static void fill_page_holes(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
	UINT32 bank, old_vpn, vblock, page_num;

	bank	= get_num_bank(lpn);
	old_vpn	= get_vpn(lpn);

	if(old_vpn != NULL){
		vblock		= old_vpn / PAGES_PER_BLK;
		page_num	= old_vpn % PAGES_PER_BLK;

		nand_page_read(bank, vblock, page_num, FTL_BUF(bank));
		if(sect_offset != 0)
			mem_copy(WR_BUF_PTR(g_ftl_write_buf_id), FTL_BUF(bank), sect_offset * BYTES_PER_SECTOR);
		if((sect_offset + num_sectors) < SECTORS_PER_PAGE){
			UINT32 const rhole_base = (sect_offset + num_sectors) * BYTES_PER_SECTOR;
			mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
					FTL_BUF(bank) + rhole_base, BYTES_PER_PAGE - rhole_base);
		}
	}
	else{
		if(num_sectors != SECTORS_PER_PAGE){
			if(sect_offset != 0)
				mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id), 0xFFFFFFFF, sect_offset * BYTES_PER_SECTOR);
			if((sect_offset + num_sectors) < SECTORS_PER_PAGE){
				UINT32 const rhole_base = (sect_offset + num_sectors) * BYTES_PER_SECTOR;
				mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
						0xFFFFFFFF, BYTES_PER_PAGE - rhole_base);
			}
		}
	}
}
static UINT32 is_lpn_existed(UINT32 const lpn)
{
	UINT32 key	= lpn % HASH_NUM;
	
	if(hash_list[key] == NULL)
		return NULL;
	else{
		node* next_addr;
		node* hash_node	= (node*) hash_list[key];
		while(1){
			if(lpn == read_dram_32(&(hash_node->lpn))){
				return (UINT32) hash_node; //read_dram_32(&(hash_node->buf_id));
			}
			else if((next_addr = (node*) read_dram_32(&(hash_node->hash_next))) == NULL){
				return NULL;
			}
			else{
				hash_node = next_addr;
			}
		}
	}
}
/* the number of evicted pages is evict_num * 64  */
#if TAG_BUFFER //&& (SYNC_EVICTION == 0)
static void evict_pages_from_fifo(UINT8 const evict_num)
{
	node*	evict_node	= (node*) fifo_start;
	UINT32	lpn, buf_id, iter;

	for(iter=0; iter<evict_num; iter++){
		lpn		= read_dram_32(&(evict_node->lpn));
		buf_id	= read_dram_32(&(evict_node->buf_id));
#if TAG_MAP_TABLE
		reflect_page(lpn, TAG_BUFFER_PTR(buf_id), (UINT8)read_dram_32(&(evict_node->tag)));
#else
		reflect_page(lpn, TAG_BUFFER_PTR(buf_id));
#endif
		hash_invalidation(lpn);
		
		remove_tag_node(evict_node);
		remove_bitmap(buf_id);
// tag list update !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if SYNC_EVICTION
		tag_entries[read_dram_32(&(evict_node->tag))]--;
#endif

		evict_node	= (node*) read_dram_32(&(evict_node->fifo_next));

		num_entries--;
	}
	
	write_dram_32(&(evict_node->fifo_prev), NULL);
	if(num_entries != 0)
		fifo_start = (UINT32) evict_node;
	else
		fifo_start=NULL;
}
static void remove_tag_node(node* const flush_node)
{	
	node* next	= (node*) read_dram_32(&(flush_node->tag_next));
	UINT32 tag	= read_dram_32(&(flush_node->tag));
	
	if(next != NULL)
		tag_list[tag]	= (UINT32) next;
	else{
		tag_list[tag]	= NULL;
		tag_last[tag]	= NULL;
	}
}
#endif
#if TAG_BUFFER == 0
static void evict_pages_to_flash(UINT8 const evict_num)
{
	node* evict_node	= (node*) TAG_BUFFER_NODE_PTR(start_buf_id);
	UINT32	lpn			= read_dram_32(&(evict_node->lpn));
	UINT8	evict_count, iter;
	UINT32	start_point = start_buf_id;
	
	for(evict_count=0; evict_count<evict_num; evict_count++){
		for(iter=0; iter<32; iter++){
#if TAG_MAP_TABLE == 0
			reflect_page(lpn, TAG_BUFFER_PTR(start_buf_id));
#endif
			hash_invalidation(lpn);
			start_buf_id = (start_buf_id + 1) % NUM_TAG_BUFFER;
			evict_node	= (node*) TAG_BUFFER_NODE_PTR(start_buf_id);
			lpn	= read_dram_32(&(evict_node->lpn));

			num_entries--;
		}
	}
}
#endif
static void hash_invalidation(UINT32 const lpn)
{
	node* cur_node = (node*) is_lpn_existed(lpn);
	node* prev;
	node* next;

	ASSERT(cur_node != NULL);

	if((prev = (node*)read_dram_32(&(cur_node->hash_prev))) == NULL){
		if((next = (node*)read_dram_32(&(cur_node->hash_next))) == NULL)
			hash_list[lpn % HASH_NUM] = NULL;
		else{
			hash_list[lpn % HASH_NUM] = (UINT32) next;
			write_dram_32(&(next->hash_prev), NULL);
		}
	}
	else{
		if((next = (node*)read_dram_32(&(cur_node->hash_next))) == NULL)
			write_dram_32(&(prev->hash_next), NULL);
		else{
			write_dram_32(&(prev->hash_next), (UINT32) next);
			write_dram_32(&(next->hash_prev), (UINT32) prev);
		}
	}
}
#if TAG_MAP_TABLE
static void reflect_page(UINT32 const lpn, UINT32 const buf_addr, UINT8 const tag)
#else
static void reflect_page(UINT32 const lpn, UINT32 const buf_addr)
#endif
{
	UINT32 bank, old_vpn, new_vpn, vblock, page_num;
	
	bank	= get_num_bank(lpn);
	new_vpn	= assign_new_write_vpn(bank);
	old_vpn	= get_vpn(lpn);

	CHECK_VPAGE(old_vpn);
	CHECK_VPAGE(new_vpn);
	ASSERT(old_vpn != new_vpn);

	g_ftl_statistics[bank].page_wcount++;

	if(old_vpn != NULL){
		vblock		= old_vpn / PAGES_PER_BLK;
		page_num	= old_vpn % PAGES_PER_BLK;
		set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
	}
	vblock		= new_vpn / PAGES_PER_BLK;
	page_num	= new_vpn % PAGES_PER_BLK;
	ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

	nand_page_ptprogram(bank, vblock, page_num, 0, SECTORS_PER_PAGE, buf_addr);
	
	set_lpn(bank, page_num, lpn);
#if TAG_MAP_TABLE
	set_vpn(lpn, new_vpn, tag);
#else
	set_vpn(lpn, new_vpn);
#endif
	set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
	//flash_finish();
}
static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
    CHECK_LPAGE(lpn);
    ASSERT(sect_offset < SECTORS_PER_PAGE);
    ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

    UINT32 bank, old_vpn, new_vpn;
    UINT32 vblock, page_num, page_offset, column_cnt;

    bank        = get_num_bank(lpn); // page striping
    page_offset = sect_offset;
    column_cnt  = num_sectors;

    new_vpn  = assign_new_write_vpn(bank);
    old_vpn  = get_vpn(lpn);

    CHECK_VPAGE (old_vpn);
    CHECK_VPAGE (new_vpn);
    ASSERT(old_vpn != new_vpn);

    g_ftl_statistics[bank].page_wcount++;

    // if old data already exist,
    if (old_vpn != NULL)
    {
        vblock   = old_vpn / PAGES_PER_BLK;
        page_num = old_vpn % PAGES_PER_BLK;

        //--------------------------------------------------------------------------------------
        // `Partial programming'
        // we could not determine whether the new data is loaded in the SATA write buffer.
        // Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
        // And then, program whole valid data
        //--------------------------------------------------------------------------------------
        if (num_sectors != SECTORS_PER_PAGE)
        {
            // Performance optimization (but, not proved)
            // To reduce flash memory access, valid hole copy into SATA write buffer after reading whole page
            // Thus, in this case, we need just one full page read + one or two mem_copy
            if ((num_sectors <= 8) && (page_offset != 0))
            {
                // one page async read
                nand_page_read(bank,
                               vblock,
                               page_num,
                               FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                             FTL_BUF(bank) + rhole_base,
                             BYTES_PER_PAGE - rhole_base);
                }
            }
            // left/right hole async read operation (two partial page read)
            else
            {
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + column_cnt,
                                     SECTORS_PER_PAGE - (page_offset + column_cnt),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
            }
        }
        // full page write
        page_offset = 0;
        column_cnt  = SECTORS_PER_PAGE;
        // invalid old page (decrease vcount)
        set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
    }
    vblock   = new_vpn / PAGES_PER_BLK;
    page_num = new_vpn % PAGES_PER_BLK;
    ASSERT(get_vcount(bank,vblock) < (PAGES_PER_BLK - 1));

    // write new data (make sure that the new data is ready in the write buffer frame)
    // (c.f FO_B_SATA_W flag in flash.h)
    nand_page_ptprogram_from_host(bank,
                                  vblock,
                                  page_num,
                                  page_offset,
                                  column_cnt);
    // update metadata
    set_lpn(bank, page_num, lpn);
#if TAG_MAP_TABLE
    set_vpn(lpn, new_vpn, 0);
#else
	set_vpn(lpn, new_vpn);
#endif
    set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
}
// get vpn from PAGE_MAP
static UINT32 get_vpn(UINT32 const lpn)
{
    CHECK_LPAGE(lpn);
    return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
}
// set vpn to PAGE_MAP
#if TAG_MAP_TABLE
static void set_vpn(UINT32 const lpn, UINT32 const vpn, UINT8 const tag)
#else
static void set_vpn(UINT32 const lpn, UINT32 const vpn)
#endif
{
    UINT32	index = lpn / (BYTES_PER_PAGE / sizeof(UINT32));

	CHECK_LPAGE(lpn);
    ASSERT(vpn >= (META_BLKS_PER_BANK * PAGES_PER_BLK) && vpn < (VBLKS_PER_BANK * PAGES_PER_BLK));

	write_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32), vpn);
	
	mapTableRef[index / 32] = mapTableRef[index / 32] | (0x00000001 << (index % 32));
	
//	if(!(mapTableRef[index / 32] & (0x00000001 << (index % 32)))){
//		mapTableRef[index / 32] |= (0x00000001 << (index % 32));
//		num_flushed_map++;
//	}
	//uart_printf("%d: %x", index / 32,  mapTableRef[index / 32]);

#if TAG_MAP_TABLE
	if(map_index[tag] < (BYTES_PER_PAGE / (sizeof(UINT32) * 2))){
		write_dram_32(LOGGING_SPACE_PTR(tag, map_index[tag]), lpn);
		write_dram_32(LOGGING_SPACE_PTR(tag, map_index[tag]) + sizeof(UINT32), vpn);
		map_index[tag]++;
	}
	else{
		// do nothing
		map_full = 0xffffffff;
	}
#endif
}
// get valid page count of vblock
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock)
{
    UINT32 vcount;

    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));

    vcount = read_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

    return vcount;
}
// set valid page count of vblock
static void set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount)
{
    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

    write_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)), vcount);
}
static UINT32 assign_new_write_vpn(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 write_vpn;
    UINT32 vblock;

    write_vpn = get_cur_write_vpn(bank);
    vblock    = write_vpn / PAGES_PER_BLK;

    // NOTE: if next new write page's offset is
    // the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
    if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 2))
    {
        // then, because of the flash controller limitation
        // (prohibit accessing a spare area (i.e. OOB)),
        // thus, we persistenly write a lpn list into last page of vblock.
        mem_copy(FTL_BUF(bank), g_misc_meta[bank].lpn_list_of_cur_vblock, sizeof(UINT32) * PAGES_PER_BLK);
        // fix minor bug
        nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
                            ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank));

        mem_set_sram(g_misc_meta[bank].lpn_list_of_cur_vblock, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);

        inc_full_blk_cnt(bank);

        // do garbage collection if necessary
        if (is_full_all_blks(bank))
        {
            garbage_collection(bank);
            return get_cur_write_vpn(bank);
        }
        do
        {
            vblock++;

            ASSERT(vblock != VBLKS_PER_BANK);
        }while (get_vcount(bank, vblock) == VC_MAX);
    }
    // write page -> next block
    if (vblock != (write_vpn / PAGES_PER_BLK))
    {
        write_vpn = vblock * PAGES_PER_BLK;
    }
    else
    {
        write_vpn++;
    }
    set_new_write_vpn(bank, write_vpn);

    return write_vpn;
}
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
    {
        return FALSE;
    }
    return TRUE;
}
//------------------------------------------------------------
// if all blocks except one free block are full,
// do garbage collection for making at least one free page
//-------------------------------------------------------------
static void garbage_collection(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);
    g_ftl_statistics[bank].gc_cnt++;

    UINT32 src_lpn;
    UINT32 vt_vblock;
    UINT32 free_vpn;
    UINT32 vcount; // valid page count in victim block
    UINT32 src_page;
    UINT32 gc_vblock;

    g_ftl_statistics[bank].gc_cnt++;

    vt_vblock = get_vt_vblock(bank);   // get victim block
    vcount    = get_vcount(bank, vt_vblock);
    gc_vblock = get_gc_vblock(bank);
    free_vpn  = gc_vblock * PAGES_PER_BLK;

/*     uart_printf("garbage_collection bank %d, vblock %d",bank, vt_vblock); */

    ASSERT(vt_vblock != gc_vblock);
    ASSERT(vt_vblock >= META_BLKS_PER_BANK && vt_vblock < VBLKS_PER_BANK);
    ASSERT(vcount < (PAGES_PER_BLK - 1));
    ASSERT(get_vcount(bank, gc_vblock) == VC_MAX);
    ASSERT(!is_bad_block(bank, gc_vblock));

    // 1. load p2l list from last page offset of victim block (4B x PAGES_PER_BLK)
    // fix minor bug
    nand_page_ptread(bank, vt_vblock, PAGES_PER_BLK - 1, 0,
                     ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank), RETURN_WHEN_DONE);
    mem_copy(g_misc_meta[bank].lpn_list_of_cur_vblock, FTL_BUF(bank), sizeof(UINT32) * PAGES_PER_BLK);
    // 2. copy-back all valid pages to free space
    for (src_page = 0; src_page < (PAGES_PER_BLK - 1); src_page++)
    {
        // get lpn of victim block from a read lpn list
        src_lpn = get_lpn(bank, src_page);
        CHECK_VPAGE(get_vpn(src_lpn));

        // determine whether the page is valid or not
        if (get_vpn(src_lpn) !=
            ((vt_vblock * PAGES_PER_BLK) + src_page))
        {
            // invalid page
            continue;
        }
        ASSERT(get_lpn(bank, src_page) != INVALID);
        CHECK_LPAGE(src_lpn);
        // if the page is valid,
        // then do copy-back op. to free space
        nand_page_copyback(bank,
                           vt_vblock,
                           src_page,
                           free_vpn / PAGES_PER_BLK,
                           free_vpn % PAGES_PER_BLK);
        ASSERT((free_vpn / PAGES_PER_BLK) == gc_vblock);
        // update metadata
#if TAG_MAP_TABLE
        set_vpn(src_lpn, free_vpn, 0);
#else
		set_vpn(src_lpn, free_vpn);
#endif
        set_lpn(bank, (free_vpn % PAGES_PER_BLK), src_lpn);

        free_vpn++;
    }
#if OPTION_ENABLE_ASSERT
    if (vcount == 0)
    {
        ASSERT(free_vpn == (gc_vblock * PAGES_PER_BLK));
    }
#endif
    // 3. erase victim block
    nand_block_erase(bank, vt_vblock);
    ASSERT((free_vpn % PAGES_PER_BLK) < (PAGES_PER_BLK - 2));
    ASSERT((free_vpn % PAGES_PER_BLK == vcount));

/*     uart_printf("gc page count : %d", vcount); */

    // 4. update metadata
    set_vcount(bank, vt_vblock, VC_MAX);
    set_vcount(bank, gc_vblock, vcount);
    set_new_write_vpn(bank, free_vpn); // set a free page for new write
    set_gc_vblock(bank, vt_vblock); // next free block (reserve for GC)
    dec_full_blk_cnt(bank); // decrease full block count
    /* uart_print("garbage_collection end"); */
}
//-------------------------------------------------------------
// Victim selection policy: Greedy
//
// Select the block which contain minumum valid pages
//-------------------------------------------------------------
static UINT32 get_vt_vblock(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 vblock;

    // search the block which has mininum valid pages
    vblock = mem_search_min_max(VCOUNT_ADDR + (bank * VBLKS_PER_BANK * sizeof(UINT16)),
                                sizeof(UINT16),
                                VBLKS_PER_BANK,
                                MU_CMD_SEARCH_MIN_DRAM);

    ASSERT(is_bad_block(bank, vblock) == FALSE);
    ASSERT(vblock >= META_BLKS_PER_BANK && vblock < VBLKS_PER_BANK);
    ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

    return vblock;
}
static void format(void)
{
    UINT32 bank, vblock, vcount_val;

    ASSERT(NUM_MISC_META_SECT > 0);
    ASSERT(NUM_VCOUNT_SECT > 0);

    uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

    uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
    uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
    uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);

    //----------------------------------------
    // initialize DRAM metadata
    //----------------------------------------
    mem_set_dram(PAGE_MAP_ADDR, NULL, PAGE_MAP_BYTES);
    mem_set_dram(VCOUNT_ADDR, NULL, VCOUNT_BYTES);

    //----------------------------------------
    // erase all blocks except vblock #0
    //----------------------------------------
	for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
            vcount_val = VC_MAX;
            if (is_bad_block(bank, vblock) == FALSE)
			{
				nand_block_erase(bank, vblock);
                vcount_val = 0;
            }
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16),
                          vcount_val);
        }
    }
    //----------------------------------------
    // initialize SRAM metadata
    //----------------------------------------
    init_metadata_sram();

    // flush metadata to NAND
    logging_pmap_table();
    logging_misc_metadata();

    write_format_mark();
	led(1);
    uart_print("format complete");
}
static void init_metadata_sram(void)
{
    UINT32 bank;
    UINT32 vblock;
    UINT32 mapblk_lbn;

    //----------------------------------------
    // initialize misc. metadata
    //----------------------------------------
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
        g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
        // NOTE: vblock #0,1 don't use for user space
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);

        //----------------------------------------
        // assign misc. block
        //----------------------------------------
        // assumption: vblock #1 = fixed location.
        // Thus if vblock #1 is a bad block, it should be allocate another block.
        set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK - 1);
        ASSERT(is_bad_block(bank, MISCBLK_VBN) == FALSE);

        vblock = MISCBLK_VBN;

        //----------------------------------------
        // assign map block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < MAPBLKS_PER_BANK)
        {
            vblock++;
            ASSERT(vblock < VBLKS_PER_BANK);
            if (is_bad_block(bank, vblock) == FALSE)
            {
                set_mapblk_vpn(bank, mapblk_lbn, vblock * PAGES_PER_BLK);
                write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
                mapblk_lbn++;
            }
        }
        //----------------------------------------
        // assign free block for gc
        //----------------------------------------
        do
        {
            vblock++;
            // NOTE: free block should not be secleted as a victim @ first GC
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
            // set free block
            set_gc_vblock(bank, vblock);

            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);
        //----------------------------------------
        // assign free vpn for first new write
        //----------------------------------------
        do
        {
            vblock++;
            // 현재 next vblock부터 새로운 데이터를 저장을 시작
            set_new_write_vpn(bank, vblock * PAGES_PER_BLK);
            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);
    }
}
// logging misc + vcount metadata
static void logging_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_addr     = VCOUNT_ADDR;
    UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES; // entire vcount data
    UINT32 bank;

    flash_finish();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        inc_miscblk_vpn(bank);

        // note: if misc. meta block is full, just erase old block & write offset #0
        if ((get_miscblk_vpn(bank) / PAGES_PER_BLK) != MISCBLK_VBN)
        {
            nand_block_erase(bank, MISCBLK_VBN);
            set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK); // vpn = 128
        }
        // copy misc. metadata to FTL buffer
        mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

        // copy vcount metadata to FTL buffer
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(FTL_BUF(bank) + misc_meta_bytes, vcount_addr, vcount_bytes);
            vcount_addr += vcount_bytes;
        }
    }
    // logging the misc. metadata to nand flash
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        nand_page_ptprogram(bank,
                            get_miscblk_vpn(bank) / PAGES_PER_BLK,
                            get_miscblk_vpn(bank) % PAGES_PER_BLK,
                            0,
                            NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                            FTL_BUF(bank));
    }
    flash_finish();
}
static void logging_pmap_table(void)
{
    UINT32 pmap_addr  = PAGE_MAP_ADDR;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 mapblk_vpn;
    UINT32 bank;
    UINT32 pmap_boundary = PAGE_MAP_ADDR + PAGE_MAP_BYTES;
    BOOL32 finished = FALSE;

	UINT16  count = 0;

    for (UINT32 mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR ;
            }
			/* flush the only dirty pages */
			if (!(mapTableRef[count / 32] & (0x00000001 << (count % 32)))){
				count++;
				pmap_addr += pmap_bytes;
				continue;
			}


            inc_mapblk_vpn(bank, mapblk_lbn);

            mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);

            // note: if there is no free page, then erase old map block first.
            if ((mapblk_vpn % PAGES_PER_BLK) == 0)
            {
                // erase full map block
                nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

                // next vpn of mapblk is offset #0
                set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);
                mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
            }
            // copy the page mapping table to FTL buffer
            mem_copy(FTL_BUF(bank), pmap_addr, pmap_bytes);

            // logging update page mapping table into map_block
            nand_page_ptprogram(bank,
                                mapblk_vpn / PAGES_PER_BLK,
                                mapblk_vpn % PAGES_PER_BLK,
                                0,
                                pmap_bytes / BYTES_PER_SECTOR,
                                FTL_BUF(bank));
            pmap_addr += pmap_bytes;

			flushed_map++;
			count++;
	    
        }
        if (finished)
        {
            break;
        }
    }
    flash_finish();
	//uart_printf("pmap count: %d, num_flushed_map: %d", count, num_flushed_map);	
	for(count = 0; count < MT_CF_BIT; count++){
	//	uart_printf("mapTableRef[count]: %x",mapTableRef[count]);
		mapTableRef[count] = 0x00000000;
	}
#if TAG_MAP_TABLE
	for(count = 0; count < 4; count++){
		map_index[count] = 0;
		mem_set_dram(LOGGING_SPACE_PTR(count, 0), 0x00000000, BYTES_PER_PAGE);
	}
#endif
}
// load flushed FTL metadta
static void load_metadata(void)
{
    load_misc_metadata();
    load_pmap_table();
}
// misc + VCOUNT
static void load_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_addr     = VCOUNT_ADDR;
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;

    UINT32 load_flag = 0;
    UINT32 bank, page_num;
    UINT32 load_cnt = 0;

    flash_finish();

	disable_irq();
	flash_clear_irq();	// clear any flash interrupt flags that might have been set

    // scan valid metadata in descending order from last page offset
    for (page_num = PAGES_PER_BLK - 1; page_num != ((UINT32) -1); page_num--)
    {
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (load_flag & (0x1 << bank))
            {
                continue;
            }
            // read valid metadata from misc. metadata area
            nand_page_ptread(bank,
                             MISCBLK_VBN,
                             page_num,
                             0,
                             NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
        }
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (!(load_flag & (0x1 << bank)) && !(BSP_INTR(bank) & FIRQ_ALL_FF))
            {
                load_flag = load_flag | (0x1 << bank);
                load_cnt++;
            }
            CLR_BSP_INTR(bank, 0xFF);
        }
    }
    ASSERT(load_cnt == NUM_BANKS);

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        // misc. metadata
        mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));

        // vcount metadata
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(vcount_addr, FTL_BUF(bank) + misc_meta_bytes, vcount_bytes);
            vcount_addr += vcount_bytes;

        }
    }
	enable_irq();
}
static void load_pmap_table(void)
{
    UINT32 pmap_addr = PAGE_MAP_ADDR;
    UINT32 temp_page_addr;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 pmap_boundary = PAGE_MAP_ADDR + (NUM_LPAGES * sizeof(UINT32));
    UINT32 mapblk_lbn, bank;
    BOOL32 finished = FALSE;

    flash_finish();

    for (mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        temp_page_addr = pmap_addr; // backup page mapping addr

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // read page mapping table from map_block
            nand_page_ptread(bank,
                             get_mapblk_vpn(bank, mapblk_lbn) / PAGES_PER_BLK,
                             get_mapblk_vpn(bank, mapblk_lbn) % PAGES_PER_BLK,
                             0,
                             pmap_bytes / BYTES_PER_SECTOR,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
            pmap_addr += pmap_bytes;
        }
        flash_finish();

        pmap_bytes = BYTES_PER_PAGE;
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (temp_page_addr >= pmap_boundary)
            {
                break;
            }
            else if (temp_page_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                pmap_bytes = (pmap_boundary - temp_page_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // copy page mapping table to PMAP_ADDR from FTL buffer
            mem_copy(temp_page_addr, FTL_BUF(bank), pmap_bytes);

            temp_page_addr += pmap_bytes;
        }
        if (finished)
        {
            break;
        }
    }
}
static void write_format_mark(void)
{
	// This function writes a format mark to a page at (bank #0, block #0).

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

	mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

	SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because we have waited for all the banks to become idle before returning from format().
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the write operation
	while (BSP_FSM(0) != BANK_IDLE);
}
static BOOL32 check_format_mark(void)
{
	// This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

	#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
	#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
	UINT32 temp;

	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because scan list loading has been completed just before this function is called.
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the read operation
	while (BSP_FSM(0) != BANK_IDLE);

	// Now that the read operation is complete, we can check interrupt flags.
	temp = BSP_INTR(0) & FIRQ_ALL_FF;

	// clear interrupt flags
	CLR_BSP_INTR(0, 0xFF);

	if (temp != 0)
	{
		return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
	}
	else
	{
		return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
	}
}

// BSP interrupt service routine
void ftl_isr(void)
{
    UINT32 bank;
    UINT32 bsp_intr_flag;

    uart_print("BSP interrupt occured...");
    // interrupt pending clear (ICU)
    SETREG(APB_INT_STS, INTR_FLASH);

    for (bank = 0; bank < NUM_BANKS; bank++) {
        while (BSP_FSM(bank) != BANK_IDLE);
        // get interrupt flag from BSP
        bsp_intr_flag = BSP_INTR(bank);

        if (bsp_intr_flag == 0) {
            continue;
        }
        UINT32 fc = GETREG(BSP_CMD(bank));
        // BSP clear
        CLR_BSP_INTR(bank, bsp_intr_flag);

        // interrupt handling
		if (bsp_intr_flag & FIRQ_DATA_CORRUPT) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
            uart_print("FIRQ_DATA_CORRUPT occured...");
		}
		if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L)) {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
			if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG) {
                uart_print("find runtime bad block when block program...");
			}
			else {
                uart_printf("find runtime bad block when block erase...vblock #: %d", GETREG(BSP_ROW_H(bank)) / PAGES_PER_BLK);
				ASSERT(fc == FC_ERASE);
			}
		}
    }
}


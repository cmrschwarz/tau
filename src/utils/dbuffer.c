#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "dbuffer.h"

int dbuffer_init_with_capacity(dbuffer* db, ureg capacity){
	if(capacity <= 8)return -1;  // we do this so we don't have to bounds check in
								 // dbuffer_reserve_small
	db->start = malloc(capacity);
	db->end = db->start + capacity;
	db->head = db->start;
	return 0;
}

int dbuffer_init(dbuffer* db){
	return dbuffer_init_with_capacity(db, sizeof(ureg) * 4);
}
bool dbuffer_is_emtpy(dbuffer* db){
	return (db->start == db->head);
}
void dbuffer_fin(dbuffer* db){
    free(db->start);
}
ureg dbuffer_get_size(dbuffer* db){
	return db->head - db->start;
}

ureg dbuffer_get_capacity(dbuffer* db){
	return db->end - db->start;
}

ureg dbuffer_get_free_space(dbuffer* db){
	return db->end - db->head;
}

int dbuffer_set_capacity(dbuffer* db, ureg capacity){
	// set bigger capacity can be used to set smaller capacities, but it doesn't
	// do a bounds check.
	int r = dbuffer_set_bigger_capacity(db, capacity);
	if(r) return r;
	if (db->head > db->end) db->head = db->end;
	return 0;
}

int dbuffer_set_bigger_capacity(dbuffer* db, ureg capacity){
	uint8_t* temp = realloc(db->start, capacity);
	if(!temp)return -1;
	db->head = temp + (db->head - db->start);
	db->end = temp + capacity;
	db->start = temp;
	//printf("growing capacity to %llu\n", dbuffer_get_capacity(db));
	return 0;
}

int dbuffer_grow(dbuffer* db){
	return dbuffer_set_bigger_capacity(db, (db->end - db->start) << 1);
}


bool dbuffer_can_fit(dbuffer* db, ureg required_space){
	return (db->head + required_space <= db->end);
}

int dbuffer_reserve(dbuffer* db, ureg space){
	if (dbuffer_can_fit(db, space)){
		ureg capacity = dbuffer_get_capacity(db);
		if (capacity > space){
			capacity = capacity * 2;
		}
		else{
			capacity = dbuffer_get_size(db) + space;
		}
		return dbuffer_set_bigger_capacity(db, capacity);
	}
	return 0;
}

int dbuffer_reserve_small(dbuffer* db, ureg space){
	if (db->head + space > db->end) return dbuffer_grow(db);
	return 0;
}
void* dbuffer_claim(dbuffer* db, ureg space){
	if(dbuffer_reserve(db, space)) return NULL;
	void* r = db->head;
	db->head += space;
	return r;
}
void* dbuffer_claim_small(dbuffer* db, ureg space){
	if(dbuffer_reserve_small(db, space)) return NULL;
	void* r = db->head;
	db->head += space;
	return r;
}
void dbuffer_remove_last(dbuffer* db, ureg size){
	db->head -= size;
}

void dbuffer_remove_at(dbuffer* db, void* pos, ureg size){
	memmove(pos, (u8*)pos + size, db->head - (u8*)pos - size);
	db->head -= size;
}
int dbuffer_insert_at(dbuffer* db, const void* data, void* pos, ureg size){
	if (!dbuffer_can_fit(db, size)){
		int r = dbuffer_reserve(db, size);
		if (r) return r;
		ureg offs = (u8*)pos - db->start;
		pos = db->start + offs;
	}
	memmove((u8*)pos + size, pos, db->head - (u8*)pos);
	memcpy(pos, data, size);
	db->head += size;
	return 0;
}

void dbuffer_swap(dbuffer* db, void* posa, void* posb, ureg size){
	u8* a = posa;
	u8* b = posb;
	if (size < db->end - db->head){
		// if we have a big enough temp buffer after head anyway, use it
		memcpy(db->head, b, size);
		memcpy(b, a, size);
		memcpy(a, db->head, size);
	}
	else{
		// otherwise do manual swapping
		// if both pos's are aligned on a sizeof(ureg) boundary
		if (!(((ureg)a) & (sizeof(ureg) - 1)) &&
			!(((ureg)b) & (sizeof(ureg) - 1))   )
		{
			void* aend = a + size - size % sizeof(ureg);
			ureg temp;
			while (a != aend){
				temp = *(ureg*)a;
				*(ureg*)a = *(ureg*)b;
				*(ureg*)b = temp;
				a += sizeof(ureg);
				b += sizeof(ureg);
			}
		}
		void* aend = a + size;
		uint8_t temp;
		while (a != aend){
			temp = *a;
			*a = *b;
			*b = temp;
			a++;
			b++;
		}
	}
}

int dbuffer_append(dbuffer* db, const void* data, ureg size){
	if(dbuffer_reserve(db, size)) return -1;
	memcpy(db->head, data, size);
	db->head += size;
	return 0;
}

void dbuffer_get(dbuffer* db, void* target, void* pos, ureg size){
	memcpy(target, pos, size);
}

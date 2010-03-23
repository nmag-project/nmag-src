/* (C) 2006 Matteo Franchin
 * Implementation of hashtables
 */

typedef unsigned int (*HashFunction)(void *key, unsigned int key_size,
 unsigned int ht_size);
typedef int (*HashComparison)(void *key1, void *key2,
 unsigned int size1, unsigned int size2);

typedef struct ht {
  struct ht *next;
  void *key, *object;
  unsigned int key_size, object_size;
} HashItem;

typedef struct {
  int num_entries;
  /* function to get the hash from the key */
  HashFunction hash;
  /* function to compare two keys */
  HashComparison cmp;
  HashItem **item;
} Hashtable;

unsigned int default_hash(void *key, unsigned int key_size,
 unsigned int ht_size);
int default_cmp(void *key1, void *key2, unsigned int size1, unsigned int size2);
int default_action(HashItem *hi);
Hashtable *hashtable_new(unsigned int num_entries, HashFunction hash,
 HashComparison cmp);
int hashtable_add(Hashtable *ht, unsigned int branch, void *key,
 unsigned int key_size, void *object, unsigned int object_size);
int hashtable_iter(Hashtable *ht, int branch, void *key, unsigned int key_size,
 HashItem **result, int (*action)(HashItem *));
int hashtable_iter2(Hashtable *ht, int branch, int (*action)(HashItem *));
void hashtable_destroy(Hashtable *ht);
void hashtable_statistics(Hashtable *ht, FILE *out);

#define hashtable(num_entries) \
  hashtable_new(num_entries, (HashFunction) NULL, (HashComparison) NULL)

#define hashtable_insert(ht, key, key_size) \
 hashtable_add( \
    ht, \
    ht->hash(key, key_size, ht->num_entries), \
    key, key_size, \
    (void *) NULL, 0)

#define hashtable_insert_obj(ht, key, key_size, object, object_size) \
 hashtable_add( \
    ht, \
    ht->hash(key, key_size, ht->num_entries), \
    key, key_size, \
    object, object_size)

#define hashtable_find(ht, key, key_size, item) \
  hashtable_iter( \
    ht, \
    ht->hash(key, key_size, ht->num_entries), \
    key, key_size, \
    item, \
    default_action)

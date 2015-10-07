#include <stdbool.h>

/* Heap is a place to allocate objects */
struct heap {
    /* Memory pool used for allocation */
    void *pool;

    size_t current_offset;

    size_t pool_size;

    /*
     * Root object that can be used during garbage collection.
     * Should typically be equal to current stack frame.
     */
    struct object *root;
};


/*
 * Structure that describes allocatable object,
 * that can be garbage collected.
 * Object_descriptor provides metadata that can be used to traverse object graph.
 * Will be defined later
 */
struct object_descriptor;

/*
 * Every object allocated on heap should have
 * this structure as it's header (first member)
 *
 * Heap allocated objects can reference manually managed objects
 * and vice versa.
 *
 * Manually managed objects should be described as "not movable"
 */
struct object {
    /*
     * This field is modified during garbage collection.
     * It's used to point to object's new location.
     *
     * Normally heap has scavenged field of every object set to NULL.
     *
     * Manually managed objects are traversed during garbage collection
     * same way as heap-allocated objects are, but are not moved.
     *
     * Scavenged field is modified during garbage collection
     * in both manually managed and heap-allocated objects.
     * Manually managed objects should have scavenged set to NULL initially
     * to allow correct traversal during garbage collection.
     */
    struct object *scavenged;

    /* Object meta-data */
    struct object_descriptor *descriptor;
};

/*
 * Every object_descriptor should provide these methods
 * to use during garbage collection
 */
struct object_descriptor_methods {
    /*
     * Size of object to allocate and scavange during garbage collection.
     * Remember that this size should include trailing padding
     * to support proper data-alignment.
     *
     * To follow alignment requirement
     * you should probably always return a result of C's sizeof operator
     *
     * Manually managed objects are not required to provide truthful size.
     * Manually managed objects can declare it's size to be zero.
     */
    size_t (*get_object_size)(struct object_descriptor *descriptor);

    /*
     * Offsets of object's fields that lead to other heap objects
     */
    int *(*get_ref_field_offsets)(struct object_descriptor *descriptor);

    /*
     * Number of fields referencing other objects
     */
    int (*get_n_ref_fields)(struct object_descriptor *descriptor);

    /*
     * Check wheather object is movable.
     * Manually managed objects are not movable.
     * Heap allocated objects are always movable.
     */
    bool (*get_is_object_movable)(struct object_descriptor *descriptor);
};

/*
 * Object descriptor is object itself and
 * can potentially be garbage collected
 */
struct object_descriptor {
    struct object object;
    struct object_descriptor_methods *methods;
};


/*
 * Object descriptor that directly stores object size and field offsets.
 * This descriptor can be used for most objects
 * since their size is usually statically known.
 */
struct static_object_descriptor {
    struct object_descriptor base;

    /*
     * Size of object to allocate and scavange during garbage collection.
     * Remember that this size should include trailing padding
     * to support proper data-alignment.
     *
     * To follow alignment requirement
     * you should probably always store a result of C's sizeof operator
     */
    size_t object_size;

    /*
     * Number of fields referencing other objects
     */
    int n_ref_fields;

    /*
     * Offsets of object's fields that lead to other heap objects
     */
    int *ref_field_offsets;

    /*
     * Check wheather object is movable.
     * Manually managed objects are not movable.
     * Heap allocated objects are always movable.
     */
    bool is_object_movable;
};

/*
 * Object descriptor methods applicable to static descriptors
 */
extern struct object_descriptor_methods static_object_descriptor_methods;

/*
 * Descriptor for objects without any references.
 * Can be used as a descriptor for manually managed objects/descriptors
 */
extern struct object_descriptor *manually_managed_norefs_descriptor;

/*
 * Init new heap to use
 */
void heap_init(struct heap *heap);

/* Allocate single object described by given descriptor on the heap */
struct object *heap_allocate_raw_object(struct heap *heap, struct object_descriptor *descriptor);

/*
 * Allocate new static_object_descriptor that cab be used to describe other objects
 */
struct static_object_descriptor *heap_allocate_static_object_descriptor(struct heap *heap);


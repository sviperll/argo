#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "object.h"

void heap_garbage_collect(struct heap *heap, size_t new_pool_size);
void heap_finalize_scavenged(struct heap *heap, struct object *object);
struct object *heap_scavenge(struct heap *heap, struct object *object);

size_t static_object_descriptor_get_object_size(struct object_descriptor *base);
int *static_object_descriptor_get_ref_field_offsets(struct object_descriptor *base);
int static_object_descriptor_get_n_ref_fields(struct object_descriptor *base);
bool static_object_descriptor_get_is_object_movable(struct object_descriptor *base);

struct object_descriptor_methods static_object_descriptor_methods = {
    .get_object_size = static_object_descriptor_get_object_size,
    .get_ref_field_offsets = static_object_descriptor_get_ref_field_offsets,
    .get_n_ref_fields = static_object_descriptor_get_n_ref_fields,
    .get_is_object_movable = static_object_descriptor_get_is_object_movable
};

struct static_object_descriptor manually_managed_norefs_descriptor_value = {
    .base = {
        .object = {
            .scavenged = NULL,
            .descriptor = &manually_managed_norefs_descriptor_value.base
        },
        .methods = &static_object_descriptor_methods
    },
    .object_size = 0, /* Is not used for manually managed objects */
    .n_ref_fields = 0,
    .ref_field_offsets = NULL,
    .is_object_movable = false
};

struct object_descriptor *manually_managed_norefs_descriptor = &manually_managed_norefs_descriptor_value.base;

struct static_object_descriptor heap_allocated_static_object_descriptor_descriptor_value = {
    .base = {
        .object = {
            .scavenged = NULL,
            .descriptor = &manually_managed_norefs_descriptor_value.base
        },
        .methods = &static_object_descriptor_methods
    },
    .object_size = sizeof(struct static_object_descriptor),
    .n_ref_fields = 0,
    .ref_field_offsets = NULL,
    .is_object_movable = true
};

void heap_init(struct heap *heap) {
    heap->pool = NULL;
    heap->pool_size = 0;
    heap->current_offset = 0;
    heap->root = NULL;
}

struct object *heap_allocate_raw_object(struct heap *heap, struct object_descriptor *descriptor)
{
    fprintf(stderr, "DEBUG: allocating...\n");
    if (heap->pool == NULL) {
        heap->pool = malloc(10);
        heap->pool_size = 10;
        heap->current_offset = 0;
    }
    size_t size = descriptor->methods->get_object_size(descriptor);
    bool is_movable = descriptor->methods->get_is_object_movable(descriptor);

    if (!is_movable) {
        fprintf(stderr, "ERROR: unable to allocate not movable object\n");
        return NULL;
    }

    size_t final_heap_offset = heap->current_offset + size;
    if (final_heap_offset > heap->pool_size) {
        heap_garbage_collect(heap, final_heap_offset + final_heap_offset / 2);
    }
    if (heap->current_offset + size > heap->pool_size)
        return NULL;
    else {
        struct object *result = (struct object *)(void *)((char *)heap->pool + heap->current_offset);
        result->scavenged = NULL;
        result->descriptor = descriptor;
        heap->current_offset += size;
        return result;
    }
}

struct static_object_descriptor *heap_allocate_static_object_descriptor(struct heap *heap)
{
    return (struct static_object_descriptor *)heap_allocate_raw_object(heap, &heap_allocated_static_object_descriptor_descriptor_value.base);
}

void heap_garbage_collect(struct heap *heap, size_t pool_size)
{
    fprintf(stderr, "DEBUG: garbage collecting!\n");
    if (heap->pool != NULL) {
        void *old_pool = heap->pool;
        heap->pool = malloc(pool_size);
        heap->pool_size = pool_size;
        heap->current_offset = 0;
        heap->root = heap_scavenge(heap, heap->root);
        heap_finalize_scavenged(heap, heap->root);
        free(old_pool);
    }
}

struct object *heap_scavenge(struct heap *heap, struct object *object)
{
    if (object == NULL)
        return NULL;
    else {
        struct object *result = object->scavenged;
        if (result != NULL) /* Already scavenged */
            return result;
        else {
            struct object_descriptor *descriptor = object->descriptor;
            size_t size = descriptor->methods->get_object_size(descriptor);
            int *ref_field_offsets = descriptor->methods->get_ref_field_offsets(descriptor);
            int n_ref_fields = descriptor->methods->get_n_ref_fields(descriptor);
            bool is_object_movable = descriptor->methods->get_is_object_movable(descriptor);

            if (!is_object_movable) {
                fprintf(stderr, "DEBUG: traversing not movable object: %d\n", (int)(void *)object);
                result = object;
            } else {
                result = (struct object *)(void *)((char *)heap->pool + heap->current_offset);
                heap->current_offset += size;
                fprintf(stderr, "DEBUG: scavenging object: was %d, become %d\n", (int)(void *)object, (int)(void *)result);
                memcpy(result, object, size);
                result->scavenged = result;
            }
            object->scavenged = result; /* Mark as already scavenged */
            result->descriptor = (struct object_descriptor *)heap_scavenge(heap, &descriptor->object);
            for (int i = 0; i < n_ref_fields; i++) {
                struct object **dest_ref = (struct object **)((char *)result + ref_field_offsets[i]);
                struct object **src_ref = (struct object **)((char *)object + ref_field_offsets[i]);
                struct object *src_object = *src_ref;
                struct object *dest_object = heap_scavenge(heap, src_object);
                fprintf(stderr, "DEBUG: rewriting references: was %d, become %d\n", (int)(void *)src_object, (int)(void *)dest_object);
                *dest_ref = dest_object;
            }
            return result;
        }
    }
}

void heap_finalize_scavenged(struct heap *heap, struct object *object)
{
    if (object != NULL && object->scavenged != NULL) {
        struct object_descriptor *descriptor = object->descriptor;
        int *ref_field_offsets = descriptor->methods->get_ref_field_offsets(descriptor);
        int n_ref_fields = descriptor->methods->get_n_ref_fields(descriptor);

        fprintf(stderr, "DEBUG: finalizing object: %d\n", (int)(void *)object);
        object->scavenged = NULL;
        heap_finalize_scavenged(heap, &descriptor->object);
        for (int i = 0; i < n_ref_fields; i++) {
            struct object **ref = (struct object **)((char *)object + ref_field_offsets[i]);
            heap_finalize_scavenged(heap, *ref);
        }
    }
}

size_t static_object_descriptor_get_object_size(struct object_descriptor *base)
{
    struct static_object_descriptor *descriptor = (struct static_object_descriptor *)base;
    return descriptor->object_size;
}

int *static_object_descriptor_get_ref_field_offsets(struct object_descriptor *base)
{
    struct static_object_descriptor *descriptor = (struct static_object_descriptor *)base;
    return descriptor->ref_field_offsets;
}

int static_object_descriptor_get_n_ref_fields(struct object_descriptor *base)
{
    struct static_object_descriptor *descriptor = (struct static_object_descriptor *)base;
    return descriptor->n_ref_fields;
}

bool static_object_descriptor_get_is_object_movable(struct object_descriptor *base)
{
    struct static_object_descriptor *descriptor = (struct static_object_descriptor *)base;
    return descriptor->is_object_movable;
}



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "object.h"
#include "stack_frame.h"

char hello_chars[] = "HELLO";

struct hello_object {
    struct object object;
    char chars[6];
};

struct heap heap;

int main(int argc, char **argv)
{
    heap_init(&heap);
    STACK_FRAME_INIT(&heap, 4);

    struct static_object_descriptor hello_descriptor = {
       .base = {
           .object = {
               .scavenged = NULL,
               .descriptor = manually_managed_norefs_descriptor
           },
           .methods = &static_object_descriptor_methods
       },
       .object_size = sizeof(struct hello_object),
       .n_ref_fields = 0,
       .ref_field_offsets = NULL,
       .is_object_movable = true
   };

   STACK_FRAME_DECLARE(struct hello_object *, hello, (struct hello_object *)heap_allocate_raw_object(&heap, &hello_descriptor.base));
   strcpy(hello->chars, "HELLO");
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   STACK_FRAME_DECLARE(struct hello_object *, hello1, (struct hello_object *)heap_allocate_raw_object(&heap, &hello_descriptor.base));
   strcpy(hello1->chars, "HELL1");
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   STACK_FRAME_DECLARE(struct hello_object *, hello2, (struct hello_object *)heap_allocate_raw_object(&heap, &hello_descriptor.base));
   strcpy(hello2->chars, "HELL2");
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   STACK_FRAME_DECLARE(struct hello_object *, hello3, (struct hello_object *)heap_allocate_raw_object(&heap, &hello_descriptor.base));
   strcpy(hello3->chars, "HELL3");
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("hello3 = %d, hello3->chars = %s\n", (int)(void *)hello3, hello3->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);

   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("hello3 = %d, hello3->chars = %s\n", (int)(void *)hello3, hello3->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);


   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("hello3 = %d, hello3->chars = %s\n", (int)(void *)hello3, hello3->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);


   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("hello3 = %d, hello3->chars = %s\n", (int)(void *)hello3, hello3->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);


   /* Do not store reference */
   heap_allocate_raw_object(&heap, &hello_descriptor.base);
   printf("hello = %d, hello->chars = %s\n", (int)(void *)hello, hello->chars);
   printf("hello1 = %d, hello1->chars = %s\n", (int)(void *)hello1, hello1->chars);
   printf("hello2 = %d, hello2->chars = %s\n", (int)(void *)hello2, hello2->chars);
   printf("hello3 = %d, hello3->chars = %s\n", (int)(void *)hello3, hello3->chars);
   printf("heap = %d, heap->size = %d, heap->current_offset = %d\n", (int)heap.pool, heap.pool_size, heap.current_offset);
   return EXIT_SUCCESS;
}


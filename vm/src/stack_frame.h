#define STACK_FRAME_DECLARE(type, name, value) \
   type name = (value); \
   stack_frame_refs[stack_frame_descriptor.n_ref_fields++] = (char *)&name - (char *)&stack_frame

#define STACK_FRAME_INIT(heap, size) \
   int stack_frame_refs[(size) + 1]; \
   struct static_object_descriptor stack_frame_descriptor = { \
       .base = { \
           .object = { \
               .scavenged = NULL, \
               .descriptor = manually_managed_norefs_descriptor \
           }, \
           .methods = &static_object_descriptor_methods \
       }, \
       .object_size = sizeof(struct object), \
       .n_ref_fields = 0, \
       .ref_field_offsets = stack_frame_refs, \
       .is_object_movable = false \
   }; \
   struct object stack_frame = { \
       .scavenged = NULL, \
       .descriptor = &stack_frame_descriptor.base \
   }; \
   STACK_FRAME_DECLARE(struct object *, previous_stack_frame, (heap)->root); \
   (heap)->root = &stack_frame


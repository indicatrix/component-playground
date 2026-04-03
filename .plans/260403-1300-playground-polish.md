
# Playground polish

## Stuff to improve

- [ ] How do we pipe in labels now, particularly for Controls.int,
  Controls.float etc. In the Controls.builder case we provide labels (but we
  always have to). Maybe default to "Value"? And have it overridden by combinators?
- [ ] Would be great to have Component.component and
  Component.componentWithPortals as accessors. componentWithPortals will be
  identity. Since we're a library we possible want Component to be an opaque
  type.


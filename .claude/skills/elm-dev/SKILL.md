---
name: elm-dev
description: Instructions for developing elm code. Use when editing .elm files.
---

# General Guidance

- It is CRITICAL that you ask clarifying questions.
- It is CRITICAL that you provide a general plan before launching into
development.
- It is CRITICAL that you do things the hard way. Repetition is okay. Do not
just jam in something that type checks and break things.

# Development process

1. Ask clarifying questions
2. Provide a general plan before development. The plan should including type
   definitions and type annotations for key functions. Consider: type-first
   design, existing functions, error handling and testing strategy
3. Implement tests for difficult sections
4. Implement code
5. Run checks before committing

# Type-First Design

When designing types:
1. Make impossible states unrepresentable
2. Use custom types over primitives (avoid String/Int soup)
3. Prefer records with extensible fields where composition needed
4. Document invariants in type comments

# Designing for testing

Complex code sections should be easy to test, and anything requiring significant
setup should be so trivial as to not need testing (minimal conditionals) and
fail obviously.

# Finding documentation

Find documentation for elm functions
inside `.elm/0.19.1/packages/elm/*/*/src/<module path>.elm`. For example, you
can find the docs for `List.*`, inside
`.elm/0.19.1/packages/elm/core/1.0.5/src/List.elm`.

# Writing tests

Use elm-test framework. Structure tests as:
- One test file per module in `tests/`. Eg for src/EdgeSummary.elm add tests
to src/EdgeSummaryTests.elm.
- Use `describe` for grouping related tests
- Prefer plain tests with examples over `fuzz` tests. Usually this is enough for
  most application logic.
- Test edge cases: empty lists, Nothing values, etc.

Example pattern for a function `myFunction`:
```
import Expect
import Test exposing (Test, describe)

myFunctionTests : Test
myFunctionTests =
    let test name input expected =
            Test.test name <|
                \_ ->
                    Expect.equal (myFunction input)
                        expected
    in describe
        [ test "first example" ...
        -- Optional comment
        , test "second example" ...
        ]
```

# Before committing

After making edits in elm code:
1. Run `npx elm make --output /dev/null examples/src/Index.elm` to check the elm compiles.
2. Run `npx elm-test` to run elm tests.
3. Run `npx elm-review` to check for any warnings (unused variables etc).
4. Run `npx elm-format --yes src tests` to format our elm code.

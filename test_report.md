# Test Report

This report summarizes the results of LLM tests across different stacks.

## Detailed Test Results

| LLM                         | Stack       | Status     |   Cost |   Time (s) |   Steps |   Attempts |   Input Tokens |   Output Tokens |
|-----------------------------|-------------|------------|--------|------------|---------|------------|----------------|-----------------|
| google/gemini-2.0-flash-001 | C#          | ✅ Success | 0.0077 |     219.72 |       9 |          3 |          52384 |            6079 |
| google/gemini-2.0-flash-001 | C++         | ❌ Failure | 0.1946 |     669.78 |      29 |         10 |        1689869 |           64113 |
| google/gemini-2.0-flash-001 | Clojure     | ❌ Failure | 0.1552 |    1399.67 |      34 |         10 |        1286129 |           66423 |
| google/gemini-2.0-flash-001 | Common Lisp | ❌ Failure | 0.0727 |     490.38 |      38 |         10 |         627777 |           24770 |
| google/gemini-2.0-flash-001 | Crystal     | ❌ Failure | 0.0975 |     309.48 |      31 |         10 |         797544 |           44311 |
| google/gemini-2.0-flash-001 | D lang      | ❌ Failure | 0.0529 |     373.8  |      36 |         10 |         470257 |           14622 |
| google/gemini-2.0-flash-001 | Elixir      | ❌ Failure | 0.8215 |    1043.27 |      64 |         10 |        7456098 |          189622 |
| google/gemini-2.0-flash-001 | Go          | ❌ Failure | 0.0725 |     300.76 |      33 |         10 |         565248 |           39995 |
| google/gemini-2.0-flash-001 | Haskell     | ❌ Failure | 0.2169 |    1128.72 |      38 |         10 |        1887264 |           70525 |
| google/gemini-2.0-flash-001 | Idris       | ❌ Failure | 0.2028 |    1794.48 |      44 |         10 |        1802775 |           56409 |
| google/gemini-2.0-flash-001 | Java        | ✅ Success | 0.0065 |     188.61 |       7 |          2 |          47213 |            4349 |
| google/gemini-2.0-flash-001 | JavaScript  | ✅ Success | 0.0026 |      26.72 |       5 |          1 |          19922 |            1488 |
| google/gemini-2.0-flash-001 | Kotlin      | ✅ Success | 0.1416 |     501.42 |      28 |          2 |        1157038 |           64665 |
| google/gemini-2.0-flash-001 | Lua         | ❌ Failure | 0.0905 |     743.37 |      31 |         10 |         736579 |           42095 |
| google/gemini-2.0-flash-001 | Nim         | ❌ Failure | 0.0506 |     463.5  |      36 |         10 |         418704 |           21939 |
| google/gemini-2.0-flash-001 | OCAML       | ❌ Failure | 0.0622 |     293.16 |      34 |         10 |         508067 |           28604 |
| google/gemini-2.0-flash-001 | PHP         | ✅ Success | 0.0416 |     264.52 |      17 |          5 |         288476 |           31894 |
| google/gemini-2.0-flash-001 | Perl        | ❌ Failure | 0.2591 |     526.46 |      52 |         10 |        2355520 |           58851 |
| google/gemini-2.0-flash-001 | Python      | ✅ Success | 0.0495 |     194.33 |      12 |          1 |         338875 |           39073 |
| google/gemini-2.0-flash-001 | Raku        | ❌ Failure | 0.0393 |     871.5  |      24 |         10 |         326820 |           16521 |
| google/gemini-2.0-flash-001 | Ruby        | ✅ Success | 0.0134 |     227.34 |      13 |          5 |          90914 |           10684 |
| google/gemini-2.0-flash-001 | Rust        | ❌ Failure | 0.2936 |    1506.91 |      44 |         10 |        2591346 |           86054 |
| google/gemini-2.0-flash-001 | Scala       | ❌ Failure | 0.1308 |    1287.61 |      36 |         10 |        1043363 |           66262 |
| google/gemini-2.0-flash-001 | TypeScript  | ✅ Success | 0.0324 |     193.13 |      17 |          5 |         245897 |           19542 |

## Stack Success Summary

| Stack      |   Successful LLMs |
|------------|-------------------|
| C#         |                 1 |
| Java       |                 1 |
| JavaScript |                 1 |
| Kotlin     |                 1 |
| PHP        |                 1 |
| Python     |                 1 |
| Ruby       |                 1 |
| TypeScript |                 1 |

## LLM Success Summary

| LLM                         |   Successful Stacks |
|-----------------------------|---------------------|
| google/gemini-2.0-flash-001 |                   8 |

## Top 10 Cheapest (not free) Successful Tests

| LLM                         | Stack      |      Cost |
|-----------------------------|------------|-----------|
| google/gemini-2.0-flash-001 | JavaScript | 0.0025874 |
| google/gemini-2.0-flash-001 | Java       | 0.0064609 |
| google/gemini-2.0-flash-001 | C#         | 0.00767   |
| google/gemini-2.0-flash-001 | Ruby       | 0.013365  |
| google/gemini-2.0-flash-001 | TypeScript | 0.0324065 |
| google/gemini-2.0-flash-001 | PHP        | 0.0416052 |
| google/gemini-2.0-flash-001 | Python     | 0.0495167 |
| google/gemini-2.0-flash-001 | Kotlin     | 0.14157   |

## Top 10 Fastest Successful Tests

| LLM                         | Stack      |   Time (s) |
|-----------------------------|------------|------------|
| google/gemini-2.0-flash-001 | JavaScript |      26.72 |
| google/gemini-2.0-flash-001 | Java       |     188.61 |
| google/gemini-2.0-flash-001 | TypeScript |     193.13 |
| google/gemini-2.0-flash-001 | Python     |     194.33 |
| google/gemini-2.0-flash-001 | C#         |     219.72 |
| google/gemini-2.0-flash-001 | Ruby       |     227.34 |
| google/gemini-2.0-flash-001 | PHP        |     264.52 |
| google/gemini-2.0-flash-001 | Kotlin     |     501.42 |


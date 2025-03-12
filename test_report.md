# Test Report

This report summarizes the results of LLM tests across different stacks.

## Detailed Test Results

| LLM                                  | Stack       | Status     |   Cost |   Time (s) |   Steps |   Attempts |   Input Tokens |   Output Tokens |
|--------------------------------------|-------------|------------|--------|------------|---------|------------|----------------|-----------------|
| anthropic/claude-3.7-sonnet:thinking | C#          | ❌ Failure | 0.1133 |     380.73 |       5 |          3 |          14225 |            4707 |
| google/gemini-2.0-flash-001          | C#          | ❌ Failure | 0.0052 |     233.88 |       6 |          3 |          30461 |            5273 |
| openai/o3-mini-high                  | C#          | ✅ Success | 0.0767 |     139.54 |       6 |          3 |          17839 |           12983 |
| anthropic/claude-3.7-sonnet:thinking | C++         | ❌ Failure | 0.2644 |     749.56 |       7 |          3 |          49134 |            7798 |
| google/gemini-2.0-flash-001          | C++         | ❌ Failure | 0.0075 |     208.82 |       6 |          3 |          44933 |            7460 |
| openai/o3-mini-high                  | C++         | ❌ Failure | 0.0863 |     487.72 |       5 |          3 |          17239 |           15298 |
| anthropic/claude-3.7-sonnet:thinking | Clojure     | ❌ Failure | 0.1568 |     391.73 |       5 |          3 |          18475 |            6756 |
| google/gemini-2.0-flash-001          | Clojure     | ❌ Failure | 0.011  |     227.14 |      10 |          3 |          78219 |            8036 |
| openai/o3-mini-high                  | Clojure     | ❌ Failure | 0.0501 |     307.92 |       4 |          3 |           9081 |            9119 |
| anthropic/claude-3.7-sonnet:thinking | Common Lisp | ❌ Failure | 0.1672 |     958.44 |       5 |          3 |          25354 |            6075 |
| google/gemini-2.0-flash-001          | Common Lisp | ❌ Failure | 0.0037 |      90.4  |       7 |          3 |          28603 |            2063 |
| openai/o3-mini-high                  | Common Lisp | ❌ Failure | 0.1696 |     368.38 |       8 |          3 |          42909 |           27807 |
| anthropic/claude-3.7-sonnet:thinking | Crystal     | ❌ Failure | 0.2305 |     467.63 |       5 |          3 |          20912 |           11181 |
| google/gemini-2.0-flash-001          | Crystal     | ❌ Failure | 0.0142 |     105.53 |       9 |          3 |          83700 |           14608 |
| openai/o3-mini-high                  | Crystal     | ❌ Failure | 0.0669 |     184.31 |       6 |          3 |          17739 |           10774 |
| anthropic/claude-3.7-sonnet:thinking | D lang      | ❌ Failure | 0.4567 |     836.3  |       5 |          3 |          37321 |           22982 |
| google/gemini-2.0-flash-001          | D lang      | ❌ Failure | 0.006  |     138.17 |       7 |          3 |          37868 |            5474 |
| openai/o3-mini-high                  | D lang      | ❌ Failure | 0.1031 |     216.41 |       6 |          3 |          19416 |           18589 |
| anthropic/claude-3.7-sonnet:thinking | Elixir      | ❌ Failure | 0.3077 |     855.8  |       5 |          3 |          26919 |           15132 |
| google/gemini-2.0-flash-001          | Elixir      | ❌ Failure | 0.0033 |     177.94 |       5 |          3 |          17611 |            3765 |
| openai/o3-mini-high                  | Elixir      | ❌ Failure | 0.1086 |     261.49 |       7 |          3 |          34942 |           15940 |
| anthropic/claude-3.7-sonnet:thinking | Go          | ❌ Failure | 0.1763 |     328.04 |       5 |          3 |          30208 |            5713 |
| google/gemini-2.0-flash-001          | Go          | ❌ Failure | 0.009  |      99.56 |       7 |          3 |          56115 |            8502 |
| openai/o3-mini-high                  | Go          | ❌ Failure | 0.0983 |     145.97 |       6 |          3 |          21097 |           17059 |
| anthropic/claude-3.7-sonnet:thinking | Haskell     | ❌ Failure | 0.374  |     879.35 |       5 |          3 |          49805 |           14975 |
| google/gemini-2.0-flash-001          | Haskell     | ❌ Failure | 0.018  |     204.22 |      10 |          3 |         110417 |           17371 |
| openai/o3-mini-high                  | Haskell     | ❌ Failure | 0.0781 |     198.38 |       6 |          3 |          17087 |           13485 |
| anthropic/claude-3.7-sonnet:thinking | Idris       | ❌ Failure | 0.4875 |    1885.79 |       5 |          3 |          15996 |           29302 |
| google/gemini-2.0-flash-001          | Idris       | ❌ Failure | 0.0031 |     354.18 |       5 |          3 |          18198 |            3241 |
| openai/o3-mini-high                  | Idris       | ❌ Failure | 0.1957 |     618.03 |       5 |          3 |          33801 |           36020 |
| anthropic/claude-3.7-sonnet:thinking | Java        | ✅ Success | 0.249  |     256.15 |       5 |          2 |          21584 |           12285 |
| google/gemini-2.0-flash-001          | Java        | ❌ Failure | 0.0035 |     232.04 |       5 |          3 |          21306 |            3361 |
| openai/o3-mini-high                  | Java        | ✅ Success | 0.0599 |     123.39 |       3 |          1 |           9578 |           11227 |
| anthropic/claude-3.7-sonnet:thinking | JavaScript  | ✅ Success | 0.1769 |     336.27 |       5 |          3 |          17496 |            8294 |
| google/gemini-2.0-flash-001          | JavaScript  | ✅ Success | 0.0058 |     101.8  |       7 |          3 |          38393 |            4876 |
| openai/o3-mini-high                  | JavaScript  | ✅ Success | 0.0191 |      44.38 |       2 |          1 |           2789 |            3646 |
| anthropic/claude-3.7-sonnet:thinking | Kotlin      | ❌ Failure | 0.1604 |     397.74 |       5 |          3 |          15699 |            7551 |
| google/gemini-2.0-flash-001          | Kotlin      | ❌ Failure | 0.0091 |      82.84 |      12 |          3 |          76308 |            3602 |
| openai/o3-mini-high                  | Kotlin      | ❌ Failure | 0.1149 |     484.39 |       7 |          3 |          29219 |           18803 |
| anthropic/claude-3.7-sonnet:thinking | Lua         | ❌ Failure | 0.6267 |     850.09 |       5 |          3 |          15091 |           38761 |
| google/gemini-2.0-flash-001          | Lua         | ❌ Failure | 0.01   |     208.81 |       9 |          3 |          53342 |           11553 |
| openai/o3-mini-high                  | Lua         | ❌ Failure | 0.0697 |     290.73 |       4 |          3 |           9818 |           13380 |
| anthropic/claude-3.7-sonnet:thinking | Nim         | ❌ Failure | 0.5339 |     809.5  |       5 |          3 |          35573 |           28476 |
| google/gemini-2.0-flash-001          | Nim         | ❌ Failure | 0.004  |      88.77 |       6 |          3 |          27049 |            3234 |
| openai/o3-mini-high                  | Nim         | ❌ Failure | 0.1435 |     508.29 |       5 |          3 |          24630 |           26467 |
| anthropic/claude-3.7-sonnet:thinking | OCAML       | ❌ Failure | 0.3299 |     660.68 |       5 |          3 |          15072 |           18978 |
| google/gemini-2.0-flash-001          | OCAML       | ❌ Failure | 0.002  |     271.44 |       5 |          3 |          15839 |            1118 |
| openai/o3-mini-high                  | OCAML       | ❌ Failure | 0.0701 |     575.63 |       4 |          3 |          10527 |           13299 |
| anthropic/claude-3.7-sonnet:thinking | PHP         | ✅ Success | 0.3882 |     450.27 |       5 |          2 |          21420 |           21593 |
| google/gemini-2.0-flash-001          | PHP         | ❌ Failure | 0.0072 |     185.08 |      10 |          3 |          57185 |            3584 |
| openai/o3-mini-high                  | PHP         | ✅ Success | 0.084  |     333.56 |       5 |          3 |          17727 |           14664 |
| anthropic/claude-3.7-sonnet:thinking | Perl        | ✅ Success | 0.2508 |     263.88 |       5 |          1 |          22168 |           12288 |
| google/gemini-2.0-flash-001          | Perl        | ❌ Failure | 0.0043 |     394.74 |       5 |          3 |          26465 |            4183 |
| openai/o3-mini-high                  | Perl        | ✅ Success | 0.0292 |     207.68 |       2 |          1 |           2788 |            5928 |
| anthropic/claude-3.7-sonnet:thinking | Python      | ✅ Success | 0.1317 |     332.33 |       5 |          3 |          15431 |            5697 |
| google/gemini-2.0-flash-001          | Python      | ✅ Success | 0.007  |      57.29 |       5 |          1 |          33670 |            9004 |
| openai/o3-mini-high                  | Python      | ✅ Success | 0.0268 |      41.16 |       2 |          1 |           2788 |            5393 |
| anthropic/claude-3.7-sonnet:thinking | Raku        | ❌ Failure | 0.1937 |     673.74 |       5 |          3 |          33750 |            6161 |
| google/gemini-2.0-flash-001          | Raku        | ❌ Failure | 0.0032 |     133.41 |       5 |          3 |          23546 |            2134 |
| openai/o3-mini-high                  | Raku        | ❌ Failure | 0.1404 |     475.77 |       6 |          3 |          27388 |           25053 |
| anthropic/claude-3.7-sonnet:thinking | Ruby        | ✅ Success | 0.1253 |     461.71 |       5 |          3 |          17957 |            4761 |
| google/gemini-2.0-flash-001          | Ruby        | ❌ Failure | 0.0048 |     513.11 |       9 |          3 |          38793 |            2188 |
| openai/o3-mini-high                  | Ruby        | ❌ Failure | 0.0314 |     332.5  |       2 |          3 |           2887 |            6404 |
| anthropic/claude-3.7-sonnet:thinking | Rust        | ❌ Failure | 0.1529 |     676.38 |       5 |          3 |          16712 |            6849 |
| google/gemini-2.0-flash-001          | Rust        | ❌ Failure | 0.0066 |     577.63 |       6 |          3 |          36258 |            7319 |
| openai/o3-mini-high                  | Rust        | ❌ Failure | 0.138  |    1062.04 |      10 |          3 |          60505 |           16236 |
| anthropic/claude-3.7-sonnet:thinking | Scala       | ❌ Failure | 0.2927 |     554.45 |       5 |          3 |          14414 |           16631 |
| google/gemini-2.0-flash-001          | Scala       | ❌ Failure | 0.0061 |     121.08 |       6 |          3 |          34202 |            6581 |
| openai/o3-mini-high                  | Scala       | ❌ Failure | 0.0864 |     279.5  |       5 |          3 |          18295 |           15062 |
| anthropic/claude-3.7-sonnet:thinking | TypeScript  | ❌ Failure | 0.1908 |     437.84 |       5 |          3 |          18660 |            8986 |
| google/gemini-2.0-flash-001          | TypeScript  | ✅ Success | 0.0033 |      64.33 |       4 |          1 |          19597 |            3327 |
| openai/o3-mini-high                  | TypeScript  | ✅ Success | 0.0425 |     111.71 |       4 |          2 |           9250 |            7342 |

## Stack Success Summary

| Stack      |   Successful LLMs |
|------------|-------------------|
| C#         |                 1 |
| Java       |                 2 |
| JavaScript |                 3 |
| PHP        |                 2 |
| Perl       |                 2 |
| Python     |                 3 |
| Ruby       |                 1 |
| TypeScript |                 2 |

## LLM Success Summary

| LLM                                  |   Successful Stacks |
|--------------------------------------|---------------------|
| openai/o3-mini-high                  |                   7 |
| anthropic/claude-3.7-sonnet:thinking |                   6 |
| google/gemini-2.0-flash-001          |                   3 |

## Top 10 Cheapest (not free) Successful Tests

| LLM                         | Stack      |      Cost |
|-----------------------------|------------|-----------|
| google/gemini-2.0-flash-001 | TypeScript | 0.0032905 |
| google/gemini-2.0-flash-001 | JavaScript | 0.0057897 |
| google/gemini-2.0-flash-001 | Python     | 0.0069686 |
| openai/o3-mini-high         | JavaScript | 0.0191103 |
| openai/o3-mini-high         | Python     | 0.026796  |
| openai/o3-mini-high         | Perl       | 0.02915   |
| openai/o3-mini-high         | TypeScript | 0.0424798 |
| openai/o3-mini-high         | Java       | 0.0599346 |
| openai/o3-mini-high         | C#         | 0.0767481 |
| openai/o3-mini-high         | PHP        | 0.0840213 |

## Top 10 Fastest Successful Tests

| LLM                                  | Stack      |   Time (s) |
|--------------------------------------|------------|------------|
| openai/o3-mini-high                  | Python     |      41.16 |
| openai/o3-mini-high                  | JavaScript |      44.38 |
| google/gemini-2.0-flash-001          | Python     |      57.29 |
| google/gemini-2.0-flash-001          | TypeScript |      64.33 |
| google/gemini-2.0-flash-001          | JavaScript |     101.8  |
| openai/o3-mini-high                  | TypeScript |     111.71 |
| openai/o3-mini-high                  | Java       |     123.39 |
| openai/o3-mini-high                  | C#         |     139.54 |
| openai/o3-mini-high                  | Perl       |     207.68 |
| anthropic/claude-3.7-sonnet:thinking | Java       |     256.15 |


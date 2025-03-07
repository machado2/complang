# Test Report

Generated on: 2025-03-07 16:18:45

## Results

| LLM                               | Stack       | Status     |   Steps |   Attempts |   Time (s) |   Input Tokens |   Output Tokens |
|-----------------------------------|-------------|------------|---------|------------|------------|----------------|-----------------|
| anthropic/claude-3.7-sonnet       | C#          | ✅ Success |      10 |          1 |     119.69 |          68426 |            6410 |
| google/gemini-2.0-flash-001       | C#          | ❌ Failure |      18 |          3 |     136.1  |         118037 |            2903 |
| meta-llama/llama-3.3-70b-instruct | C#          | ❌ Failure |       7 |          3 |     339.33 |          42245 |            2900 |
| openai/gpt-4o-mini                | C#          | ❌ Failure |      17 |          3 |      81.25 |         121338 |            6264 |
| openai/o3-mini-high               | C#          | ✅ Success |       3 |          1 |     122.62 |           7993 |            8422 |
| anthropic/claude-3.7-sonnet       | C++         | ❌ Failure |      19 |          3 |     694.97 |         924792 |           35605 |
| google/gemini-2.0-flash-001       | C++         | ❌ Failure |       6 |          3 |     135.81 |          99873 |            3466 |
| meta-llama/llama-3.3-70b-instruct | C++         | ❌ Failure |      11 |          3 |     493.33 |          75197 |            6528 |
| openai/gpt-4o-mini                | C++         | ❌ Failure |      21 |          5 |     327.29 |        1250496 |            4068 |
| openai/o3-mini-high               | C++         | ❌ Failure |      18 |          5 |     564.73 |         516146 |           58647 |
| google/gemini-2.0-flash-001       | Clojure     | ❌ Failure |      10 |          3 |     293.96 |         256990 |            7463 |
| meta-llama/llama-3.3-70b-instruct | Clojure     | ❌ Failure |      16 |          3 |     339.86 |          96686 |            6000 |
| openai/gpt-4o-mini                | Clojure     | ❌ Failure |      13 |          3 |     161.16 |         128241 |            2902 |
| openai/o3-mini-high               | Clojure     | ✅ Success |      13 |          3 |     582.87 |         147030 |           43916 |
| google/gemini-2.0-flash-001       | Common Lisp | ❌ Failure |       9 |          3 |      77.98 |          84121 |            7883 |
| meta-llama/llama-3.3-70b-instruct | Common Lisp | ❌ Failure |      10 |          3 |     312.83 |          63799 |            5870 |
| openai/gpt-4o-mini                | Common Lisp | ❌ Failure |      12 |          3 |     335.47 |         121130 |            8305 |
| openai/o3-mini-high               | Common Lisp | ❌ Failure |       7 |          3 |     234.47 |          39665 |           20155 |
| google/gemini-2.0-flash-001       | D lang      | ❌ Failure |       9 |          3 |     121.19 |         123737 |            3469 |
| meta-llama/llama-3.3-70b-instruct | D lang      | ❌ Failure |      13 |          3 |     618.89 |          93163 |            9537 |
| openai/gpt-4o-mini                | D lang      | ❌ Failure |       9 |          3 |      30.61 |          35172 |            1801 |
| openai/o3-mini-high               | D lang      | ❌ Failure |       7 |          3 |     341.26 |          34980 |           25432 |
| google/gemini-2.0-flash-001       | Elixir      | ❌ Failure |       3 |          3 |     259.43 |          18180 |            4350 |
| meta-llama/llama-3.3-70b-instruct | Elixir      | ❌ Failure |      11 |          3 |     457.78 |          78096 |            7822 |
| openai/gpt-4o-mini                | Elixir      | ❌ Failure |      16 |          3 |     143.34 |         103814 |            4267 |
| openai/o3-mini-high               | Elixir      | ❌ Failure |      14 |          3 |     495.55 |         198051 |           57179 |
| google/gemini-2.0-flash-001       | Go          | ❌ Failure |      10 |          3 |      74.7  |         103226 |           10136 |
| meta-llama/llama-3.3-70b-instruct | Go          | ❌ Failure |      10 |          3 |     903.14 |          88306 |            8527 |
| openai/gpt-4o-mini                | Go          | ❌ Failure |      10 |          3 |      60.07 |          71136 |            5739 |
| openai/o3-mini-high               | Go          | ❌ Failure |      17 |          3 |    1011.69 |         244956 |           86327 |
| google/gemini-2.0-flash-001       | Haskell     | ❌ Failure |      12 |          3 |     157.78 |          78749 |            3515 |
| meta-llama/llama-3.3-70b-instruct | Haskell     | ❌ Failure |      17 |          3 |     457.41 |         116948 |            6797 |
| openai/gpt-4o-mini                | Haskell     | ❌ Failure |       3 |          3 |     245.85 |           8159 |            1049 |
| openai/o3-mini-high               | Haskell     | ❌ Failure |      14 |          3 |     716.76 |         251343 |           45638 |
| google/gemini-2.0-flash-001       | Idris       | ❌ Failure |       9 |          3 |      86.16 |          73224 |           14039 |
| meta-llama/llama-3.3-70b-instruct | Idris       | ❌ Failure |       8 |          3 |     304.86 |          27255 |            3657 |
| openai/gpt-4o-mini                | Idris       | ❌ Failure |       9 |          3 |     177.08 |          37372 |            2285 |
| openai/o3-mini-high               | Idris       | ❌ Failure |      15 |          3 |     523.61 |         253912 |           65545 |
| anthropic/claude-3.7-sonnet       | Java        | ✅ Success |      18 |          2 |     481.97 |         352789 |           29471 |
| google/gemini-2.0-flash-001       | Java        | ❌ Failure |      22 |          5 |     117.78 |         279969 |            9885 |
| meta-llama/llama-3.3-70b-instruct | Java        | ❌ Failure |      24 |          5 |     627.68 |         225451 |            8042 |
| openai/gpt-4o-mini                | Java        | ❌ Failure |      24 |          5 |     174.93 |         423908 |            5133 |
| openai/o3-mini-high               | Java        | ✅ Success |       4 |          2 |     221.35 |          10548 |            7854 |
| anthropic/claude-3.7-sonnet       | JavaScript  | ✅ Success |       7 |          1 |      78.37 |          37594 |            3283 |
| google/gemini-2.0-flash-001       | JavaScript  | ✅ Success |       7 |          2 |      70.32 |          42054 |            5630 |
| meta-llama/llama-3.3-70b-instruct | JavaScript  | ❌ Failure |      18 |          4 |    2064.8  |         132770 |            9089 |
| openai/gpt-4o-mini                | JavaScript  | ✅ Success |       7 |          2 |      57.79 |          23459 |            1401 |
| openai/o3-mini-high               | JavaScript  | ✅ Success |       2 |          1 |      45.81 |           2875 |            3370 |
| google/gemini-2.0-flash-001       | Kotlin      | ❌ Failure |      10 |          3 |      48.35 |          59427 |            5171 |
| meta-llama/llama-3.3-70b-instruct | Kotlin      | ❌ Failure |       9 |          3 |     547.04 |          50944 |            5692 |
| openai/gpt-4o-mini                | Kotlin      | ❌ Failure |      15 |          3 |      48.73 |          81447 |            3314 |
| openai/o3-mini-high               | Kotlin      | ✅ Success |      12 |          3 |     696.62 |         164604 |           46630 |
| google/gemini-2.0-flash-001       | OCAML       | ❌ Failure |      11 |          3 |     529.14 |         300881 |           32040 |
| meta-llama/llama-3.3-70b-instruct | OCAML       | ❌ Failure |      19 |          3 |    1029.52 |         172320 |           10373 |
| openai/gpt-4o-mini                | OCAML       | ❌ Failure |       9 |          3 |     183.82 |          54194 |            2387 |
| openai/o3-mini-high               | OCAML       | ❌ Failure |      12 |          3 |     697.53 |         127297 |           45054 |
| google/gemini-2.0-flash-001       | PHP         | ❌ Failure |      16 |          3 |     156.2  |         325005 |           17653 |
| meta-llama/llama-3.3-70b-instruct | PHP         | ❌ Failure |      14 |          3 |     384.63 |         271389 |            7623 |
| openai/gpt-4o-mini                | PHP         | ✅ Success |       6 |          2 |      64.7  |          36510 |            1742 |
| openai/o3-mini-high               | PHP         | ❌ Failure |      12 |          3 |     400.52 |         109912 |           12826 |
| google/gemini-2.0-flash-001       | Perl        | ❌ Failure |      20 |          3 |    1334.14 |        9848823 |           24207 |
| meta-llama/llama-3.3-70b-instruct | Perl        | ❌ Failure |       9 |          3 |     646.46 |          37460 |            4885 |
| openai/gpt-4o-mini                | Perl        | ❌ Failure |       9 |          3 |     671.23 |          40512 |            4077 |
| openai/o3-mini-high               | Perl        | ✅ Success |       6 |          2 |     138.11 |          27142 |           13568 |
| anthropic/claude-3.7-sonnet       | Python      | ✅ Success |      10 |          2 |     188.66 |          99718 |            9910 |
| google/gemini-2.0-flash-001       | Python      | ✅ Success |       4 |          1 |      39.81 |          20876 |            3759 |
| meta-llama/llama-3.3-70b-instruct | Python      | ✅ Success |       7 |          2 |     150.93 |          28065 |            2762 |
| openai/gpt-4o-mini                | Python      | ✅ Success |       8 |          2 |      67.22 |          31091 |            1705 |
| openai/o3-mini-high               | Python      | ✅ Success |       3 |          2 |     310.91 |           8252 |            9153 |
| google/gemini-2.0-flash-001       | Raku        | ❌ Failure |      10 |          3 |     171.99 |         265968 |           16737 |
| meta-llama/llama-3.3-70b-instruct | Raku        | ❌ Failure |      10 |          3 |     427.93 |          67952 |            8395 |
| openai/gpt-4o-mini                | Raku        | ❌ Failure |       5 |          3 |      81.06 |          13821 |            1216 |
| openai/o3-mini-high               | Raku        | ❌ Failure |       6 |          3 |     398.8  |          24155 |           13131 |
| google/gemini-2.0-flash-001       | Ruby        | ❌ Failure |       9 |          3 |     131.53 |          45153 |            5761 |
| meta-llama/llama-3.3-70b-instruct | Ruby        | ❌ Failure |      12 |          3 |     231.28 |          68070 |            4136 |
| openai/gpt-4o-mini                | Ruby        | ❌ Failure |      11 |          3 |     200.17 |          78180 |            7400 |
| openai/o3-mini-high               | Ruby        | ✅ Success |       4 |          2 |     133.56 |           8524 |            8471 |
| google/gemini-2.0-flash-001       | Rust        | ❌ Failure |      10 |          3 |     174.41 |          80498 |            3773 |
| meta-llama/llama-3.3-70b-instruct | Rust        | ❌ Failure |      18 |          3 |     510.2  |         186326 |            8579 |
| openai/gpt-4o-mini                | Rust        | ❌ Failure |       6 |          3 |     302    |          55285 |            2654 |
| openai/o3-mini-high               | Rust        | ❌ Failure |       6 |          3 |     101.93 |          26746 |           11402 |
| google/gemini-2.0-flash-001       | Scala       | ❌ Failure |      11 |          3 |     143.91 |         177362 |           17266 |
| meta-llama/llama-3.3-70b-instruct | Scala       | ❌ Failure |      11 |          3 |     907.88 |          98634 |            9854 |
| openai/gpt-4o-mini                | Scala       | ❌ Failure |       9 |          3 |      49.79 |          68935 |            2306 |
| openai/o3-mini-high               | Scala       | ❌ Failure |       7 |          3 |     146.42 |          26517 |           15597 |
| google/gemini-2.0-flash-001       | TypeScript  | ✅ Success |      12 |          3 |     166.44 |         164924 |           22553 |
| meta-llama/llama-3.3-70b-instruct | TypeScript  | ❌ Failure |      10 |          3 |     440.17 |          62877 |            5851 |
| openai/gpt-4o-mini                | TypeScript  | ❌ Failure |      13 |          3 |      70.2  |          71469 |            3646 |
| openai/o3-mini-high               | TypeScript  | ✅ Success |       4 |          2 |      92.05 |          10271 |            5477 |
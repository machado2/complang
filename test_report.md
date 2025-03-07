# Test Report

Generated on: 2025-03-06 23:08:19

## Results

| LLM                                    | Stack      | Status     |   Steps |   Attempts |   Time (s) |   Input Tokens |   Output Tokens |
|----------------------------------------|------------|------------|---------|------------|------------|----------------|-----------------|
| openai/o3-mini-high                    | Python     | ✅ Success |       3 |          2 |     310.91 |           8252 |            9153 |
| openai/gpt-4o-mini                     | Python     | ✅ Success |       8 |          2 |      67.22 |          31091 |            1705 |
| google/gemini-2.0-flash-001            | Python     | ✅ Success |       4 |          1 |      39.81 |          20876 |            3759 |
| deepseek/deepseek-r1-distill-llama-70b | Python     | ❌ Failure |      12 |          5 |     555.9  |            794 |            6185 |
| meta-llama/llama-3.3-70b-instruct      | Python     | ✅ Success |       7 |          2 |     150.93 |          28065 |            2762 |
| openai/o3-mini-high                    | Java       | ✅ Success |       4 |          2 |     221.35 |          10548 |            7854 |
| openai/gpt-4o-mini                     | Java       | ❌ Failure |      24 |          5 |     174.93 |         423908 |            5133 |
| google/gemini-2.0-flash-001            | Java       | ❌ Failure |      22 |          5 |     117.78 |         279969 |            9885 |
| deepseek/deepseek-r1-distill-llama-70b | Java       | ❌ Failure |      12 |          5 |     781.21 |            770 |            4786 |
| meta-llama/llama-3.3-70b-instruct      | Java       | ❌ Failure |      24 |          5 |     627.68 |         225451 |            8042 |
| openai/o3-mini-high                    | JavaScript | ✅ Success |       2 |          1 |      45.81 |           2875 |            3370 |
| openai/gpt-4o-mini                     | JavaScript | ✅ Success |       7 |          2 |      57.79 |          23459 |            1401 |
| google/gemini-2.0-flash-001            | JavaScript | ✅ Success |       7 |          2 |      70.32 |          42054 |            5630 |
| deepseek/deepseek-r1-distill-llama-70b | JavaScript | ❌ Failure |      12 |          5 |    1280.95 |            820 |            4303 |
| meta-llama/llama-3.3-70b-instruct      | JavaScript | ✅ Success |      18 |          4 |    2064.8  |         132770 |            9089 |
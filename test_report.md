# Test Report

Generated on: 2025-03-05 18:00:09

## Results

| LLM                         | Stack      | Status     |   Steps |   Time (s) |   Input Tokens |   Output Tokens | Directory                                              |
|-----------------------------|------------|------------|---------|------------|----------------|-----------------|--------------------------------------------------------|
| google/gemini-2.0-flash-001 | Python     | ✅ Success |      16 |     234.64 |         252558 |           25261 | ./test_projects\google_gemini_2_0_flash_001_python     |
| google/gemini-2.0-flash-001 | Java       | ❌ Failure |      36 |     437.8  |        1409175 |           26179 | ./test_projects\google_gemini_2_0_flash_001_java       |
| google/gemini-2.0-flash-001 | Go         | ❌ Failure |      46 |     567.76 |        3891212 |           84276 | ./test_projects\google_gemini_2_0_flash_001_go         |
| google/gemini-2.0-flash-001 | JavaScript | ✅ Success |      24 |     355.26 |        1171291 |           33089 | ./test_projects\google_gemini_2_0_flash_001_javascript |
| google/gemini-2.0-flash-001 | C#         | ✅ Success |       5 |      40.47 |          16804 |            2007 | ./test_projects\google_gemini_2_0_flash_001_c_x        |
| google/gemini-2.0-flash-001 | PHP        | ✅ Success |       8 |     262.96 |         113745 |            8699 | ./test_projects\google_gemini_2_0_flash_001_php        |
| google/gemini-2.0-flash-001 | Rust       | ❌ Failure |      35 |    2435.02 |        2516129 |           58268 | ./test_projects\google_gemini_2_0_flash_001_rust       |
| google/gemini-2.0-flash-001 | TypeScript | ✅ Success |      19 |     342.02 |         615025 |           47512 | ./test_projects\google_gemini_2_0_flash_001_typescript |
| google/gemini-2.0-flash-001 | Kotlin     | ❌ Failure |      36 |     739.79 |        1818956 |           74614 | ./test_projects\google_gemini_2_0_flash_001_kotlin     |
| google/gemini-2.0-flash-001 | Ruby       | ✅ Success |      19 |     337.27 |         397350 |           17782 | ./test_projects\google_gemini_2_0_flash_001_ruby       |
| google/gemini-2.0-flash-001 | Scala      | ❌ Failure |      35 |    1157.4  |        1737689 |           52113 | ./test_projects\google_gemini_2_0_flash_001_scala      |
| google/gemini-2.0-flash-001 | Zig        | ❌ Failure |      35 |     134.54 |         438770 |            8847 | ./test_projects\google_gemini_2_0_flash_001_zig        |
| google/gemini-2.0-flash-001 | Haskell    | ❌ Failure |      36 |    3227.73 |        2097137 |           44225 | ./test_projects\google_gemini_2_0_flash_001_haskell    |
| google/gemini-2.0-flash-001 | Perl       | ✅ Success |      22 |     338.02 |         638969 |           35481 | ./test_projects\google_gemini_2_0_flash_001_perl       |

## Failures

### google/gemini-2.0-flash-001 - Java
Feedback:
```
The tests failed because the `target/` directory was not created by the Maven build within the Docker container, resulting in an `lstat` error when attempting to copy the JAR file.
```

### google/gemini-2.0-flash-001 - Go
Feedback:
```
I was unable to complete this task due to issues with go modules dependency management in docker, and difficulties in getting the toolchain's file writing to work as intended
```

### google/gemini-2.0-flash-001 - Rust
Feedback:
```
The test failed because the Rust application within the Docker container could not establish a network connection with the PostgreSQL database, resulting in a "Connection refused" error. This indicates a configuration issue in the docker image or external to it.
```

### google/gemini-2.0-flash-001 - Kotlin
Feedback:
```
The test failed because the gradlew script within the Docker container couldn't find the `gradle` executable.
```

### google/gemini-2.0-flash-001 - Scala
Feedback:
```
The tests failed because the application.conf file contained carriage return characters which caused a parsing error. This can occur when copying files between operating systems with different line ending conventions (Windows uses CRLF whereas Linux and MacOS use LF). Additionally, the environment variable might not have been properly set. The API tests also timed out.
```

### google/gemini-2.0-flash-001 - Zig
Feedback:
```
The test failed because the Docker image build process could not find a suitable base image or method to install Zig, resulting in a 'not found' error.
```

### google/gemini-2.0-flash-001 - Haskell
Feedback:
```
Previous attempts failed primarily due to syntax errors in the generated Haskell code, specifically related to SQL queries, error messages, language extensions, and Docker build configurations.
```


# Test Report

Generated on: 2025-03-05 20:44:58

## Results

| LLM                         | Stack       | Status     |   Steps |   Time (s) |   Input Tokens |   Output Tokens | Directory                                               |
|-----------------------------|-------------|------------|---------|------------|----------------|-----------------|---------------------------------------------------------|
| google/gemini-2.0-flash-001 | Python      | ✅ Success |      16 |     234.64 |         252558 |           25261 | ./test_projects\google_gemini_2_0_flash_001_python      |
| google/gemini-2.0-flash-001 | Java        | ❌ Failure |      36 |     437.8  |        1409175 |           26179 | ./test_projects\google_gemini_2_0_flash_001_java        |
| google/gemini-2.0-flash-001 | Go          | ❌ Failure |      46 |     567.76 |        3891212 |           84276 | ./test_projects\google_gemini_2_0_flash_001_go          |
| google/gemini-2.0-flash-001 | JavaScript  | ✅ Success |      24 |     355.26 |        1171291 |           33089 | ./test_projects\google_gemini_2_0_flash_001_javascript  |
| google/gemini-2.0-flash-001 | C#          | ✅ Success |       5 |      40.47 |          16804 |            2007 | ./test_projects\google_gemini_2_0_flash_001_c_x         |
| google/gemini-2.0-flash-001 | PHP         | ✅ Success |       8 |     262.96 |         113745 |            8699 | ./test_projects\google_gemini_2_0_flash_001_php         |
| google/gemini-2.0-flash-001 | Rust        | ❌ Failure |      35 |    2435.02 |        2516129 |           58268 | ./test_projects\google_gemini_2_0_flash_001_rust        |
| google/gemini-2.0-flash-001 | TypeScript  | ✅ Success |      19 |     342.02 |         615025 |           47512 | ./test_projects\google_gemini_2_0_flash_001_typescript  |
| google/gemini-2.0-flash-001 | Kotlin      | ❌ Failure |      36 |     739.79 |        1818956 |           74614 | ./test_projects\google_gemini_2_0_flash_001_kotlin      |
| google/gemini-2.0-flash-001 | Ruby        | ✅ Success |      19 |     337.27 |         397350 |           17782 | ./test_projects\google_gemini_2_0_flash_001_ruby        |
| google/gemini-2.0-flash-001 | Scala       | ❌ Failure |      35 |    1157.4  |        1737689 |           52113 | ./test_projects\google_gemini_2_0_flash_001_scala       |
| google/gemini-2.0-flash-001 | Zig         | ❌ Failure |      35 |     134.54 |         438770 |            8847 | ./test_projects\google_gemini_2_0_flash_001_zig         |
| google/gemini-2.0-flash-001 | Haskell     | ❌ Failure |      36 |    3227.73 |        2097137 |           44225 | ./test_projects\google_gemini_2_0_flash_001_haskell     |
| google/gemini-2.0-flash-001 | Perl        | ✅ Success |      22 |     338.02 |         638969 |           35481 | ./test_projects\google_gemini_2_0_flash_001_perl        |
| google/gemini-2.0-flash-001 | Raku        | ❌ Failure |      30 |    2908.63 |        2445140 |           78091 | ./test_projects\google_gemini_2_0_flash_001_raku        |
| google/gemini-2.0-flash-001 | Clojure     | ❌ Failure |      35 |     682.26 |        2226404 |           59320 | ./test_projects\google_gemini_2_0_flash_001_clojure     |
| google/gemini-2.0-flash-001 | Common Lisp | ❌ Failure |      37 |     763.76 |        2334101 |           47723 | ./test_projects\google_gemini_2_0_flash_001_common_lisp |
| google/gemini-2.0-flash-001 | OCAML       | ❌ Failure |      35 |    1572.76 |        2552175 |           66914 | ./test_projects\google_gemini_2_0_flash_001_ocaml       |
| google/gemini-2.0-flash-001 | D lang      | ❌ Failure |      37 |     822.02 |        2915475 |           67721 | ./test_projects\google_gemini_2_0_flash_001_d_lang      |
| google/gemini-2.0-flash-001 | Elixir      | ❌ Failure |      36 |     357.59 |        1126642 |           35140 | ./test_projects\google_gemini_2_0_flash_001_elixir      |
| deepseek/deepseek-r1:free   | Python      | ❌ Failure |      65 |    2733.58 |          30492 |           50744 | ./test_projects\deepseek_deepseek_r1_free_python        |

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

### google/gemini-2.0-flash-001 - Raku
Feedback:
```
The Raku application failed to connect to the PostgreSQL database, preventing the CRUD API from functioning.
```

### google/gemini-2.0-flash-001 - Clojure
Feedback:
```
{'answer': "The application failed due to an IllegalStateException indicating that the 'app-routes' function could not be found, likely caused by incorrect macro variable resolution."}
```

### google/gemini-2.0-flash-001 - Common Lisp
Feedback:
```
The tests consistently timed out due to container networking issues, preventing API calls from reaching the server. Quicklisp setup and server startup were confirmed in logs, yet external access was blocked.
```

### google/gemini-2.0-flash-001 - OCAML
Feedback:
```
The test failed because the type signature for the `Postgresql.connection` constructor in `main.ml` expected `port` as a string, but it was provided as an integer.
```

### google/gemini-2.0-flash-001 - D lang
Feedback:
```
The test failed because the Docker build process couldn't add the D language repository. Specifically, the command `apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F3F896F3274BBD9BBBA59058710592E7FB7AF6CA` failed to retrieve the repository key, resulting in the error 'gpg: keyserver receive failed: No data'. This prevented `apt-get update` from properly recognizing the PPA, making it impossible to subsequently install `dmd` and `dub`.
```

### google/gemini-2.0-flash-001 - Elixir
Feedback:
```
The test failed because the setup_db.sh script could not be found during the Docker build process. This likely indicates an issue with file paths or permissions within the Dockerfile.
```

### deepseek/deepseek-r1:free - Python
Feedback:
```
Похоже, в вашем сообщении произошла ошибка: вместо ожидаемого текста отображаются технические метки `[object Object]`. Обычно это происходит, когда:

1. **Данные передаются в некорректном формате**  
   Например, если вы пытаетесь вывести объект JavaScript напрямую, вместо этого отображается `[object Object]`.  
   **Решение:** Используйте `JSON.stringify(obj)` для преобразования объекта в строку.

   ```javascript
   const data = { name: "Alice", age: 25 };
   console.log(JSON.stringify(data)); // {"name":"Alice","age":25}
   ```

2. **Ошибка в API или обработке данных**  
   Если данные приходят от сервера или другого источника, убедитесь, что они декодируются правильно.  
   **Пример:** Получение JSON-ответа от сервера и его парсинг:
   ```javascript
   fetch('https://api.example.com/data')
     .then(response => response.json()) // декодируем JSON
     .then(data => console.log(data));
   ```

3. **Проблема с отображением в интерфейсе**  
   В React/Vue/других фреймворках объекты нельзя выводить напрямую.  
   **Решение:** Достаньте конкретные поля объекта:
   ```jsx
   function UserInfo({ user }) {
     return <div>{user.name}</div>; // вместо {user}
   }
   ```

Если проблема не исчезла, уточните:
- В каком контексте возникает ошибка (браузер, Node.js, фреймворк)?
- Какой код вызывает эту проблему?
- Что именно вы пытаетесь сделать?
```


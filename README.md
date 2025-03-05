# LLM Code Generation Benchmark

This project evaluates the code generation capabilities of various Large Language Models (LLMs) by tasking them with creating a functional CRUD API in multiple programming languages. The generated code is containerized using Docker, connected to a PostgreSQL database, and automatically tested for correctness. The results are compiled into a detailed report, making it a powerful tool for benchmarking LLM performance across diverse programming stacks.

## Features

- Supports any LLM available in openrouter.ai
- Supports any programming language that the LLMs can generate code for
- Automatic generation of CRUD API code by LLMs
- Docker containerization and execution of generated code
- Connection to PostgreSQL for database operations
- Automated testing of `/users` endpoints (POST, GET, PUT, DELETE)
- Checkpoint saving to resume interrupted runs
- Detailed test report generation

## Requirements

- **Python 3.x**
- **Docker** (must be installed and running)
- **PostgreSQL** (running on `localhost:5432` with user `postgres`)
- **Python Packages:**
  - `psycopg2`
  - `requests`
  - `tabulate`
  - `smolagents`
- **Environment Variables:**
  - `OPENROUTER_API_KEY`: API key for accessing LLMs via OpenRouter
  - `PGPASSWORD`: Password for the PostgreSQL `postgres` user
- **Internet Connection**: Required to access LLMs via OpenRouter

## Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/machado2/complang.git
   cd complang
   ```

2. **Install Python Packages**:
   ```bash
   pip install psycopg2 requests tabulate smolagents
   ```
   *Note*: Additional packages may be required depending on your environment or updates to the project.

3. **Set Up Docker**:
   - Ensure Docker is installed and running. Verify with:
     ```bash
     docker --version
     ```

4. **Set Up PostgreSQL**:
   - Ensure PostgreSQL is running on `localhost:5432`.
   - Create a `postgres` user and set its password (this will be used in the `PGPASSWORD` environment variable).

## Configuration

Set the following environment variables:

- **`OPENROUTER_API_KEY`**:
  - Obtain your API key from [OpenRouter](https://openrouter.ai/keys).
  - Set it in your shell:
    ```bash
    export OPENROUTER_API_KEY="your-api-key-here"
    ```

- **`PGPASSWORD`**:
  - Set the password for the PostgreSQL `postgres` user:
    ```bash
    export PGPASSWORD="your-postgres-password"
    ```

## Usage

Run the main script to start the benchmarking process:

```bash
python main.py
```

### What Happens When You Run It?

- **Directory Creation**: Creates subdirectories for each LLM-stack combination under `./test_projects`.
- **Code Generation**: Uses each LLM to generate CRUD API code in the specified stack.
- **Docker Execution**: Builds and runs Docker containers for each project.
- **Testing**: Tests the `/users` endpoints for CRUD functionality.
- **Progress Saving**: Saves progress in `checkpoint.json` to resume interrupted runs.
- **Report Generation**: Outputs results in `test_report.md`.

### Notes
- **Resource Usage**: The process can be resource-intensive and time-consuming, depending on the number of LLMs and stacks tested. Run it on a machine with sufficient CPU, memory, and disk space.
- **Output Files**:
  - `checkpoint.json`: Tracks completed tests and results for resuming.
  - `test_report.md`: Contains a summary table and detailed feedback for failed tests.
  - `./test_projects/`: Stores generated code and Docker files for each test.

## Testing

The program automates testing as follows:

- **Docker Build**: Builds a container from the generated `Dockerfile` and code.
- **Database Setup**: Creates a unique PostgreSQL database for each test (e.g., `test_<llm>_<stack>`).
- **API Testing**: Tests the `/users` endpoints:
  - `POST /users`: Create a user
  - `GET /users`: List all users
  - `GET /users/{id}`: Retrieve a specific user
  - `PUT /users/{id}`: Update a user
  - `DELETE /users/{id}`: Delete a user
- **Cleanup**: Drops the database and stops the container after each test.

Feedback is provided for failed tests, including API responses and container logs, to aid in debugging (though this is primarily for the agent's internal use).


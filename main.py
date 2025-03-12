import os
import json
from textwrap import dedent
import requests
import time
import psycopg2
from psycopg2 import sql
import shutil
import socket
import re
import sys
from smolagents import CodeAgent, tool, LiteLLMModel, ToolCallingAgent
from tabulate import tabulate
from typing import List, Dict, Any
import subprocess
import time
import litellm
from generate_report import generate_report

# Constants

# LLMS = ["openai/o3-mini-high", "openai/gpt-4o-mini", "google/gemini-2.0-flash-001", "meta-llama/llama-3.3-70b-instruct", "google/gemini-2.0-pro-exp-02-05:free", "google/gemini-2.0-flash-thinking-exp:free"]]
# STACKS = ["Python", "Java", "JavaScript", "C++", "C#", "PHP", "Rust", "TypeScript", "Go", "Kotlin", "Ruby", "Scala", "Haskell", "Perl", "Raku", "Clojure", "Common Lisp", "OCAML", "D lang", "Elixir", "Idris"]

LLMS = ["openai/o3-mini-high", "anthropic/claude-3.7-sonnet:thinking", "google/gemini-2.0-flash-001"]
STACKS = ["Python", "Java", "JavaScript", "C++", "C#", "PHP", "Rust", "TypeScript", "Go", "Kotlin", "Ruby", "Scala", "Haskell", "Perl", "Raku", "Clojure", "Common Lisp", "OCAML", "D lang", "Elixir", "Idris", "Lua", "Crystal", "Nim"]

OPENROUTER_API_KEY = os.getenv("OPENROUTER_API_KEY")
DBNAME = "complang"
DBUSER = "testuser"
DBPASSWORD = "Saloon5-Moody-Observing"
BASE_DIR = "./test_projects"
MAX_STEPS = 3
MAX_ATTEMPTS = 3
BASE_PORT = 8080
CHECKPOINT_FILE = "./checkpoint.json"
REPORT_FILE = "test_report.md"

DATABASE_SQL = dedent("""
    -- Create the database
    CREATE DATABASE complang;

    -- Create the user with a simple password
    CREATE USER testuser WITH PASSWORD 'Saloon5-Moody-Observing';

    -- Grant CONNECT privilege on the database to the user
    GRANT CONNECT ON DATABASE complang TO testuser;

    -- Connect to the newly created database
    \connect complang

    -- Create the users table with the specified structure
    CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        name TEXT NOT NULL,
        email TEXT NOT NULL
    );

    -- Grant CRUD permissions (SELECT, INSERT, UPDATE, DELETE) on the users table to the user
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE users TO testuser;
    GRANT USAGE, SELECT ON SEQUENCE users_id_seq TO testuser;
    """)

# Verify environment variables
if not OPENROUTER_API_KEY or not DBPASSWORD:
    raise ValueError("Missing required environment variables: OPENROUTER_API_KEY and PGPASSWORD must be set.")

def find_free_port(start_port):
    """Find an available port starting from start_port."""
    port = start_port
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        while True:
            if s.connect_ex(('localhost', port)) != 0:
                return port
            port += 1

def sanitize_name(name):
    """Sanitize a name for use in file paths and database names."""
    newname = re.sub(r'[^a-zA-Z0-9]', '_', name.lower())
    if newname.endswith('_'):
        newname = newname + "x"
    return newname

def run_command(command_args, cwd=None, timeout=300, check=False):
    print(f"Executing: {' '.join(command_args)}...")
    return subprocess.run(command_args, cwd=cwd, timeout=timeout, capture_output=True, text=True, encoding="utf-8", errors="replace", check=check)
            
def stop_and_remove_docker(container_name):
    """Stop and remove a Docker container."""
    run_command(["docker", "stop", container_name], timeout=30)
    run_command(["docker", "rm", container_name], timeout=30)

def check_docker():
    """Check if Docker is available and running."""
    try:
        result = run_command(["docker", "info"])
        return result.returncode == 0
    except (subprocess.SubprocessError, FileNotFoundError):
        return False
    
# Docker and Testing Functions
def build_and_run_docker(directory, container_name, port):
    """Build and run a Docker container, returning success status and error message if any."""
    try:
        run_command(["docker", "build", "-t", f"api_{container_name}", "."], cwd=directory, check=True)
    except subprocess.CalledProcessError as e:
        return False, f"Docker build failed:\n{e.stderr}"
    
    try:
        run_command(
            ["docker", "run", "-d", "--name", container_name, "-p", f"{port}:8080", 
             "-e", f"PGPASSWORD={DBPASSWORD}", f"api_{container_name}"],
            cwd=directory, check=True
        )
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, f"Docker run failed:\n{e.stderr}"

def get_docker_logs(container_name):
    """Retrieve logs from a Docker container."""
    try:
        result = run_command(["docker", "logs", container_name])
        return result.stdout + result.stderr
    except subprocess.CalledProcessError:
        return "Unable to retrieve container logs."

def clear_database():
    conn = psycopg2.connect(dbname=DBNAME, user=DBUSER, password=DBPASSWORD, host="localhost", port=5432)
    conn.autocommit = True
    with conn.cursor() as cur:
        cur.execute(sql.SQL("DELETE FROM USERS"))
    conn.close()

def test_api(port):
    """Test the CRUD API endpoints and return results and detailed feedback."""
    base_url = f"http://localhost:{port}/users"
    results = {}
    feedback = []

    # Wait for API to be ready
    start_time = time.time()
    max_wait = 15  # Total seconds to wait
    poll_interval = 1  # Check every 1 second
    while time.time() - start_time < max_wait:
        try:
            if requests.get(base_url, timeout=2).status_code == 200:
                break
        except requests.RequestException:
            time.sleep(poll_interval)
    else:
        feedback.append(f"API did not respond within {max_wait} seconds on http://container_name:8080/users")
        return {"results": {"startup": False}, "feedback": "\n".join(feedback)}

    try:
        # POST
        post_resp = requests.post(base_url, json={"name": "Test", "email": "test@example.com"}, timeout=5)
        results["POST"] = post_resp.status_code in (200, 201)
        feedback.append(f"POST: {'✅' if results['POST'] else '❌'} Status {post_resp.status_code}, Response: {post_resp.text[:100]}")
        user_id = post_resp.json().get("id") if results["POST"] else None

        # GET all
        get_all_resp = requests.get(base_url, timeout=5)
        results["GET_ALL"] = get_all_resp.status_code == 200 and len(get_all_resp.json()) > 0
        feedback.append(f"GET_ALL: {'✅' if results['GET_ALL'] else '❌'} Status {get_all_resp.status_code}, Items: {len(get_all_resp.json())}")

        # GET by ID
        if user_id:
            get_id_resp = requests.get(f"{base_url}/{user_id}", timeout=5)
            results["GET_ID"] = get_id_resp.status_code == 200 and get_id_resp.json().get("name") == "Test"
            feedback.append(f"GET_ID: {'✅' if results['GET_ID'] else '❌'} Status {get_id_resp.status_code}, Name: {get_id_resp.json().get('name', 'N/A')}")
        
        # PUT
        if user_id:
            put_resp = requests.put(f"{base_url}/{user_id}", json={"name": "Updated", "email": "test@example.com"}, timeout=5)
            results["PUT"] = put_resp.status_code in (200, 204)
            feedback.append(f"PUT: {'✅' if results['PUT'] else '❌'} Status {put_resp.status_code}")

        # DELETE
        if user_id:
            del_resp = requests.delete(f"{base_url}/{user_id}", timeout=5)
            results["DELETE"] = del_resp.status_code in (200, 204)
            feedback.append(f"DELETE: {'✅' if results['DELETE'] else '❌'} Status {del_resp.status_code}")
    except requests.RequestException as e:
        feedback.append(f"API test error: {str(e)}")
    
    return {"results": results, "feedback": "\n".join(feedback)}

# Agent Tools
def get_tools(directory: str):
    """Define tools for the CodeAgent to manipulate files and test the solution."""

    @tool
    def list_files() -> List[str]:
        """
        Return a list with the filename of all files in the project directory.

        Returns:
            Dictionary with success status and list of file names.
        """
        files = [os.path.relpath(os.path.join(root, f), directory) 
                    for root, _, fs in os.walk(directory) for f in fs]
        return files

    @tool
    def read_file(filename: str) -> str | None:
        """
        Read the contents of a file or None if it doesn't exist.

        Args:
            filename: Path to the file relative to the project directory.

        Returns:
            The contents of the file or None
        """
        try:
            with open(os.path.join(directory, filename), "r", encoding="utf-8", newline="\n") as f:
                return f.read()
        except Exception as e:
            print(f"Error reading file {filename}: {e}")
            return None

    @tool
    def write_file(filename: str, content: str) -> bool:
        """
        Write or overwrite a file with the given content, creating directories if needed.

        Args:
            filename: Path to the file relative to the project directory.
            content: Content to write to the file.

        Returns:
            True if successful, False otherwise.
        """
        try:
            file_path = os.path.join(directory, filename)
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            with open(file_path, "w", encoding="utf-8", newline="\n") as f:
                f.write(content)
            return True
        except Exception as e:
            print(f"Error writing file {filename}: {e}")
            return False

    @tool
    def append_file(filename: str, content: str) -> bool:
        """
        Append content to the end of a file.

        Args:
            filename: Path to the file relative to the project directory.
            content: Content to append.

        Returns:
            True if successful, False otherwise.
        """
        try:
            with open(os.path.join(directory, filename), "a", encoding="utf-8", newline="\n") as f:
                f.write(content)
            return True
        except Exception as e:
            print(f"Error appending to file {filename}: {e}")
            return False

    @tool
    def delete_file(filename: str) -> bool:
        """
        Delete a file from the project directory.

        Args:
            filename: Path to the file relative to the project directory.

        Returns:
            True if successful, False otherwise.
        """
        try:
            os.remove(os.path.join(directory, filename))
            return True
        except Exception as e:
            print(f"Error deleting file {filename}: {e}")
            return False

    return [list_files, read_file, write_file, append_file, delete_file]

# Checkpoint management
def save_checkpoint(results: List[Dict[str, Any]]):
    """Save current progress to checkpoint file."""
    with open(CHECKPOINT_FILE, 'w') as f:
        json.dump({
            'results': results
        }, f, indent=2)

def load_checkpoint() -> List[Dict[str, Any]]:
    """Load progress from checkpoint file if exists."""
    if os.path.exists(CHECKPOINT_FILE):
        with open(CHECKPOINT_FILE, 'r') as f:
            data = json.load(f)
            return data['results']
    return []

def test_solution(dir: str, unique_id: str) -> tuple[bool, str]:
    container_name = f"test_{unique_id}"
    stop_and_remove_docker(container_name)
    clear_database()
    port = find_free_port(BASE_PORT)
    try:
        success, error = build_and_run_docker(dir, container_name, port)
        if not success:
            return False, f"Build/Run Error:\n{error}"
        clear_database()
        test_result = test_api(port)
        logs = get_docker_logs(container_name)
        if not all(test_result["results"].values()):
            return False, f"API Tests:\n{test_result['feedback']}\nContainer Logs:\n{logs}"
        return True, "All tests passed!"
    finally:
        stop_and_remove_docker(container_name)

def summarize_feedback(feedback: str) -> str:
    # summarize the feedback using an llm
    # 3 attempts
    feedback = feedback.replace('[', '\\[').replace(']', '\\]')
    for _ in range(3):
        try:
            print("Summarizing feedback...")
            model = LiteLLMModel(model_id="openrouter/google/gemini-2.0-flash-001")
            prompt = f"Summarize these feedback in a paragraph. Make them just a few lines with the relevant content. Don't try to guess how to fix it, just summarize it. Quote the relevant lines as is if it's short enough. This feedback is plain text, don't try to parse it.\n\n{feedback}"
            agent = CodeAgent(tools=[], model=model, max_steps=3, add_base_tools=False)
            return agent.run(prompt)
        except Exception as e:
            print(f"Error summarizing feedback: {str(e)}")
    raise ValueError("Failed to summarize feedback")
    

# Core Functionality
def test_stack(llm: str, stack: str, results: List[Dict[str, Any]], use_tool_calling_agent: bool):
    """Test an LLM with a specific stack."""

    if any(r["llm"] == llm and r["stack"] == stack for r in results):
        return # already tested, skip it
    
    unique_id = f"{sanitize_name(llm)}_{sanitize_name(stack)}"

    print(f"\n### Testing {llm} with {stack}")
    start_time = time.time()
    
    directory = os.path.join(BASE_DIR, unique_id)
    
    # Setup
    if os.path.exists(directory):
        shutil.rmtree(directory)
    os.makedirs(directory, exist_ok=True)

    # Configure agent
    tools = get_tools(directory)
    model = LiteLLMModel(model_id=f"openrouter/{llm}")
    if use_tool_calling_agent:
        agent = ToolCallingAgent(tools=tools, model=model, max_steps=MAX_STEPS, add_base_tools=False)
    else:
        agent = CodeAgent(tools=tools, model=model, max_steps=MAX_STEPS, add_base_tools=False)
    
    prompt = f"""
    ## TASK: Create a CRUD API using {stack} with these requirements:

    * Connects to a PostgreSQL database at host.docker.internal:5432, database '{DBNAME}', user '{DBUSER}', password from PGPASSWORD env var.
    * Runs in a Docker container, you need to create a Dockerfile for it
    * Listen on port 8080
    * Exposes /users endpoints with the following operations:
        - POST /users: Creates a user. Accepts JSON {{ "name": string, "email": string }}, returns 201 with {{ "id": int, "name": string, "email": string }}.
        - GET /users: Returns a list of all users as [{{"id": int, "name": string, "email": string}}, ...], status 200.
        - GET /users/{{id}}: Returns a single user as {{ "id": int, "name": string, "email": string }}, status 200, or 404 if not found.
        - PUT /users/{{id}}: Updates a user. Accepts {{ "name": string, "email": string }}, returns 200/204, or 404 if not found.
        - DELETE /users/{{id}}: Deletes a user, returns 200/204, or 404 if not found.

    ## Observations:

    * Use the write_file function to write the files, complete and ready to be tested
    * Your files will be tested automatically by a tool, so make sure they are correct and complete
    * Let files that are normally generated by tools (like package lock files and anything with a hash or checksum that would be hard to guess) be generated in the docker build process
    * The database is already created with the following script:

    ```
    {DATABASE_SQL}
    ```

    * Don't try to build or test it, it will be done automatically, and I'll call you back with feedback for another attempt

    """

    num_attempts = 0

    feedbacks = []

    # Run agent
    reset_agent = True
    for _ in range(MAX_ATTEMPTS):
        num_attempts += 1
        try:
            agent.run(prompt, reset=reset_agent)
            if agent.memory.steps[-1].error is None:
                reset_agent = False
            else:
                reset_agent = True
            success, feedback = test_solution(directory, unique_id)
            if success:
                break
        except Exception as e:
            success = False
            feedback = f"Error during testing: {str(e)}"
        
        feedback = summarize_feedback(feedback)

        print(f"Test {unique_id} attempt {num_attempts} failed.\nFeedback:\n{feedback}")

        prompt = prompt + dedent(f"""
        
        ----
                
        ## Attempt {num_attempts} failed. Feedback:
        ```
        {feedback}
        ```
        """)
        feedbacks.append(feedback)
    
    # Collect results
    token_counts = agent.monitor.get_total_token_counts()
    input_tokens = token_counts['input']
    output_tokens = token_counts['output']
    result = {
        "llm": llm,
        "stack": stack,
        "success": success,
        "steps": len(agent.memory.steps),
        "duration": time.time() - start_time,
        "directory": directory,
        "feedbacks": feedbacks,
        "input_tokens": input_tokens,
        "output_tokens": output_tokens,
        "attempts": num_attempts
    }
    results.append(result)
    save_checkpoint(results)
    generate_report()

    clear_database()

# Main Execution
def main():
    if not check_docker():
        print("Docker is not available. Please ensure it is installed and running.")
        sys.exit(1)

    results = load_checkpoint()
    os.makedirs(BASE_DIR, exist_ok=True)

    try:
        for stack in STACKS:
            for llm in LLMS:
                test_stack(llm, stack, results, False)
    except KeyboardInterrupt:
        print("\nInterrupted by user. Saving checkpoint and generating final report...")
    finally:
        save_checkpoint(results)
        generate_report()
        print(f"Checkpoint saved at '{CHECKPOINT_FILE}'. Report generated at '{REPORT_FILE}'.")
        print(f"Project files are in '{BASE_DIR}'.")

if __name__ == "__main__":
    main()
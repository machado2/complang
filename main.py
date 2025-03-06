import os
import json
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
from config import get_pg_password
from runcommand import check_docker, stop_and_remove_docker, build_and_run_docker, get_docker_logs

# Constants
LLMS = ["google/gemini-2.0-flash-001", "qwen/qwen-2.5-coder-32b-instruct", "openai/gpt-4o-mini", "meta-llama/llama-3.3-70b-instruct", "deepseek/deepseek-r1-distill-llama-70b"]
# STACKS = ["Python", "Java", "JavaScript", "C++", "C#", "PHP", "Rust", "TypeScript", "Kotlin", "Ruby", "Scala", "Zig", "Haskell", "Perl", "Raku", "Clojure", "Common Lisp", "OCAML", "D lang", "Elixir"]
STACKS = ["Python", "Java", "JavaScript"]
OPENROUTER_API_KEY = os.getenv("OPENROUTER_API_KEY")
PGPASSWORD = get_pg_password()
BASE_DIR = "./test_projects"
MAX_STEPS = 30
BASE_PORT = 8080
CHECKPOINT_FILE = "./checkpoint.json"
REPORT_FILE = "test_report.md"

# Global success flag
SUCCESS_FLAG = False

# Verify environment variables
if not OPENROUTER_API_KEY or not PGPASSWORD:
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

SUPPORTS_TOOLS = False

@tool
def test_tool_support() -> str:
    """Call it to confirm tool calls are working."""
    global SUPPORTS_TOOLS
    SUPPORTS_TOOLS = True
    return "Tool ran successfully"

def check_tool_support(model) -> bool:
    global SUPPORTS_TOOLS
    SUPPORTS_TOOLS = False
    agent = ToolCallingAgent(tools=[test_tool_support], model=model, max_steps=1)
    try:
        agent.run("Call test tool support")
        return SUPPORTS_TOOLS
    except Exception as e:
        return SUPPORTS_TOOLS

def create_database(db_name):
    """Create a PostgreSQL database."""
    conn = psycopg2.connect(dbname="postgres", user="postgres", password=PGPASSWORD, host="localhost", port=5432)
    conn.autocommit = True
    with conn.cursor() as cur:
        cur.execute(sql.SQL("CREATE DATABASE {}").format(sql.Identifier(db_name)))
    conn.close()

def drop_database(db_name):
    conn = psycopg2.connect(dbname="postgres", user="postgres", password=PGPASSWORD, host="localhost", port=5432)
    conn.autocommit = True
    with conn.cursor() as cur:
        cur.execute(sql.SQL("DROP DATABASE IF EXISTS {} WITH (FORCE)").format(sql.Identifier(db_name)))
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
def get_tools(directory: str, test_callback):
    """Define tools for the CodeAgent to manipulate files and test the solution."""

    @tool
    def list_files() -> Dict[str, Any]:
        """
        List all files in the project directory.

        Returns:
            Dictionary with success status and list of file names.
        """
        try:
            files = [os.path.relpath(os.path.join(root, f), directory) 
                     for root, _, fs in os.walk(directory) for f in fs]
            return {"success": True, "files": files}
        except Exception as e:
            return {"success": False, "error": str(e)}

    @tool
    def read_file(filename: str) -> Dict[str, Any]:
        """
        Read the contents of a file.

        Args:
            filename: Path to the file relative to the project directory.

        Returns:
            Dictionary with success status and file content or error.
        """
        try:
            with open(os.path.join(directory, filename), "r", encoding="utf-8", newline="\n") as f:
                return {"success": True, "content": f.read()}
        except Exception as e:
            return {"success": False, "error": str(e)}

    @tool
    def write_file(filename: str, content: str) -> Dict[str, Any]:
        """
        Write or overwrite a file with the given content.

        Args:
            filename: Path to the file relative to the project directory.
            content: Content to write to the file.

        Returns:
            Dictionary with success status and message or error.
        """
        try:
            file_path = os.path.join(directory, filename)
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            with open(file_path, "w", encoding="utf-8", newline="\n") as f:
                f.write(content)
            return {"success": True, "message": f"Wrote {filename}"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    @tool
    def append_file(filename: str, content: str) -> Dict[str, Any]:
        """
        Append content to the end of a file.

        Args:
            filename: Path to the file relative to the project directory.
            content: Content to append.

        Returns:
            Dictionary with success status and message or error.
        """
        try:
            with open(os.path.join(directory, filename), "a", encoding="utf-8", newline="\n") as f:
                f.write(content)
            return {"success": True, "message": f"Appended to {filename}"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    @tool
    def delete_file(filename: str) -> Dict[str, Any]:
        """
        Delete a file from the project directory.

        Args:
            filename: Path to the file relative to the project directory.

        Returns:
            Dictionary with success status and message or error.
        """
        try:
            os.remove(os.path.join(directory, filename))
            return {"success": True, "message": f"Deleted {filename}"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    @tool
    def test_solution() -> Dict[str, Any]:
        """
        Test the current project files by building a Docker container, running it, and checking the CRUD API endpoints.
        This tool uses your Dockerfile and app code to create a container, connects it to a PostgreSQL database, and tests
        the /users endpoints (POST, GET, PUT, DELETE). Use this to verify your solution works and get feedback to fix it.

        Returns:
            A dictionary with:
            - "success": bool - True if all API tests (POST, GET, PUT, DELETE) pass, False if any fail or if build/run errors occur.
            - "feedback": str - Details to guide your next steps. If "success" is True, it confirms the solution works (e.g., "All tests passed!").
            If "success" is False, it includes error messages (e.g., build failures, API test results, container logs) to help you debug.

        Usage:
            Call this after writing your Dockerfile and app code. If "success" is False, use the "feedback" to identify and fix issues
            (e.g., adjust files with write_file or append_file), then call test_solution again. Repeat until "success" is True or you
            run out of steps.
        """
        global SUCCESS_FLAG
        success, feedback = test_callback(directory)
        if success:
            SUCCESS_FLAG = True
        return {
            "success": success,  # True only if all tests pass
            "feedback": feedback  # Details for the agent to act on
        }

    return [list_files, read_file, write_file, append_file, delete_file, test_solution]

# Checkpoint management
def save_checkpoint(completed: set, results: List[Dict[str, Any]]):
    """Save current progress to checkpoint file."""
    with open(CHECKPOINT_FILE, 'w') as f:
        json.dump({
            'completed': list(completed),
            'results': results
        }, f, indent=2)

def load_checkpoint() -> tuple[set, List[Dict[str, Any]]]:
    """Load progress from checkpoint file if exists."""
    if os.path.exists(CHECKPOINT_FILE):
        with open(CHECKPOINT_FILE, 'r') as f:
            data = json.load(f)
            return set(data['completed']), data['results']
    return set(), []

# Core Functionality
def test_stack(llm: str, stack: str, results: List[Dict[str, Any]], supports_tools: bool, completed: set):
    """Test an LLM with a specific stack."""
    global SUCCESS_FLAG
    test_id = f"{llm}|{stack}"
    if test_id in completed:
        return  # Skip already completed tests
    # Reset success flag before test
    SUCCESS_FLAG = False

    print(f"\n### Testing {llm} with {stack}")
    start_time = time.time()
    unique_id = f"{sanitize_name(llm)}_{sanitize_name(stack)}"
    directory = os.path.join(BASE_DIR, unique_id)
    db_name = f"test_{unique_id}"
    container_name = f"test_{unique_id}"
    port = find_free_port(BASE_PORT)

    # Setup
    os.makedirs(directory, exist_ok=True)

    # Define test callback
    def test_callback(dir):
        global SUCCESS_FLAG
        SUCCESS_FLAG = False
        stop_and_remove_docker(container_name)
        try:
            success, error = build_and_run_docker(dir, container_name, port)
            if not success:
                return False, f"Build/Run Error:\n{error}"
            drop_database(db_name)
            create_database(db_name)
            test_result = test_api(port)
            logs = get_docker_logs(container_name)
            if not all(test_result["results"].values()):
                return False, f"API Tests:\n{test_result['feedback']}\nContainer Logs:\n{logs}"
            SUCCESS_FLAG = True
            return True, "All tests passed!"
        finally:
            stop_and_remove_docker(container_name)

    # Configure agent
    tools = get_tools(directory, test_callback)
    model = LiteLLMModel(model_id=f"openrouter/{llm}")
    if supports_tools:
        agent = ToolCallingAgent(tools=tools, model=model, max_steps=MAX_STEPS, add_base_tools=True)
    else:
        agent = CodeAgent(tools=tools, model=model, max_steps=MAX_STEPS, add_base_tools=True)
    

    prompt = f"""
    Create a CRUD API using {stack} that:
    - Connects to a PostgreSQL database at host.docker.internal:5432, database '{db_name}', user 'postgres', password from PGPASSWORD env var.
    - The database is empty on first run; detect this and create a 'users' table with columns: id (auto-incrementing primary key), name (text), email (text).
    - Runs in a Docker container on port 8080.
    - Exposes /users endpoints with the following operations:
    - POST /users: Creates a user. Accepts JSON {{ "name": string, "email": string }}, returns 201 with {{ "id": int, "name": string, "email": string }}.
    - GET /users: Returns a list of all users as [{{"id": int, "name": string, "email": string}}, ...], status 200.
    - GET /users/{{id}}: Returns a single user as {{ "id": int, "name": string, "email": string }}, status 200, or 404 if not found.
    - PUT /users/{{id}}: Updates a user. Accepts {{ "name": string, "email": string }}, returns 200/204, or 404 if not found.
    - DELETE /users/{{id}}: Deletes a user, returns 200/204, or 404 if not found.
    Use tools to:
    - Write files with write_file (e.g., Dockerfile, app code).
    - Modify files with append_file or overwrite with write_file.
    - Check files with list_files and read_file.
    - Delete files with delete_file if needed.
    - Test with test_solution, which builds the Docker container and tests all endpoints, returning feedback if any fail.
    Iterate using feedback until all tests pass or you exhaust steps.
    """

    # Run agent
    try:
        response = agent.run(prompt)
        success = SUCCESS_FLAG
        steps = len(agent.logs)
    except Exception as e:
        success = False
        steps = len(agent.logs) if 'agent' in locals() else 0
        response = str(e)

    # Collect results
    token_counts = agent.monitor.get_total_token_counts()
    input_tokens = token_counts['input']
    output_tokens = token_counts['output']
    result = {
        "llm": llm,
        "stack": stack,
        "success": success,
        "steps": steps,
        "duration": time.time() - start_time,
        "directory": directory,
        "feedback": response if not success else "Success",
        "input_tokens": input_tokens,
        "output_tokens": output_tokens
    }
    results.append(result)
    completed.add(test_id)
    save_checkpoint(completed, results)
    generate_report(results)

    # Cleanup
    drop_database(db_name)

def generate_report(results: List[Dict[str, Any]]):
    """Generate a report of test results."""
    with open(REPORT_FILE, "w", encoding="utf-8") as f:
        f.write("# Test Report\n\n")
        f.write(f"Generated on: {time.strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write("## Results\n\n")
        headers = ["LLM", "Stack", "Status", "Steps", "Time (s)", "Input Tokens", "Output Tokens"]
        table = [
            [r["llm"], r["stack"], "✅ Success" if r["success"] else "❌ Failure", 
             r["steps"], f"{r['duration']:.2f}", r["input_tokens"], r["output_tokens"]]
            for r in results
        ]
        f.write(tabulate(table, headers, tablefmt="github"))
        f.write("\n\n## Failures\n\n")
        for r in results:
            if not r["success"]:
                f.write(f"### {r['llm']} - {r['stack']}\nFeedback:\n```\n{r['feedback']}\n```\n\n")

# Main Execution
def main():
    if not check_docker():
        print("Docker is not available. Please ensure it is installed and running.")
        sys.exit(1)

    completed, results = load_checkpoint()
    if not completed:
        shutil.rmtree(BASE_DIR, ignore_errors=True)
        os.makedirs(BASE_DIR, exist_ok=True)
        results = []

    try:
        for llm in LLMS:
            # model_supports_tools = check_tool_support(llm)
            for stack in STACKS:
                test_stack(llm, stack, results, False, completed)
    except KeyboardInterrupt:
        print("\nInterrupted by user. Saving checkpoint and generating final report...")
    finally:
        save_checkpoint(completed, results)
        generate_report(results)
        print(f"Checkpoint saved at '{CHECKPOINT_FILE}'. Report generated at '{REPORT_FILE}'.")
        print(f"Project files are in '{BASE_DIR}'.")

if __name__ == "__main__":
    main()
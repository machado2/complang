import subprocess
import time
from config import get_pg_password

def run_command(command_args, cwd=None, timeout=None, check=False):
    print(f"Executing: {' '.join(command_args)}...")
    return subprocess.run(command_args, cwd=cwd, timeout=timeout, capture_output=True, text=True, encoding="utf-8", errors="replace", check=check)
            
def stop_and_remove_docker(container_name):
    """Stop and remove a Docker container."""
    run_command(["docker", "stop", container_name])
    run_command(["docker", "rm", container_name])

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
    PGPASSWORD = get_pg_password()
    try:
        run_command(["docker", "build", "-t", f"api_{container_name}", "."], cwd=directory, check=True)
    except subprocess.CalledProcessError as e:
        return False, f"Docker build failed:\n{e.stderr}"
    
    try:
        run_command(
            ["docker", "run", "-d", "--name", container_name, "-p", f"{port}:8080", 
             "-e", f"PGPASSWORD={PGPASSWORD}", f"api_{container_name}"],
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
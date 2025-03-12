import json
from tabulate import tabulate
import matplotlib.pyplot as plt

# --- Constants ---
LLM_PRICES = {
    "anthropic/claude-3.7-sonnet": {"input": 3.00, "output": 15.00},
    "anthropic/claude-3.7-sonnet:thinking": {"input": 3.00, "output": 15.00},
    "openai/o3-mini-high": {"input": 1.10, "output": 4.40},
    "openai/gpt-4o-mini": {"input": 0.15, "output": 0.60},
    "openai/gpt-4o": {"input": 2.5, "output": 10},
    "google/gemini-2.0-flash-001": {"input": 0.10, "output": 0.40},
    "meta-llama/llama-3.3-70b-instruct": {"input": 0.12, "output": 0.30},
}
DEFAULT_INPUT_TOKEN_PRICE_PER_1M = 0.00  # Default price if LLM price is not found
DEFAULT_OUTPUT_TOKEN_PRICE_PER_1M = 0.00 # Default price if LLM price is not found
CHECKPOINT_FILE = "checkpoint.json"
REPORT_FILE = "test_report.md"

def calculate_cost(llm_model, input_tokens, output_tokens):
    """Calculates the cost of a test based on token usage and LLM model prices."""
    if input_tokens is None or output_tokens is None:
        return 0

    prices = LLM_PRICES.get(llm_model)
    if prices:
        input_price_per_1M = prices["input"]
        output_price_per_1M = prices["output"]
    else:
        input_price_per_1M = DEFAULT_INPUT_TOKEN_PRICE_PER_1M
        output_price_per_1M = DEFAULT_OUTPUT_TOKEN_PRICE_PER_1M

    input_cost = (input_tokens / 1_000_000) * input_price_per_1M
    output_cost = (output_tokens / 1_000_000) * output_price_per_1M
    return input_cost + output_cost

def generate_report():
    """Generates a test report from checkpoint data."""

    try:
        with open(CHECKPOINT_FILE, 'r') as f:
            checkpoint_data = json.load(f)
            results = checkpoint_data['results']
    except FileNotFoundError:
        print(f"Error: {CHECKPOINT_FILE} not found. Please run the main test script first.")
        return
    except json.JSONDecodeError:
        print(f"Error: Invalid JSON format in {CHECKPOINT_FILE}. Please check the file content.")
        return
    
    results = sorted(results, key=lambda r: (r["stack"], r["llm"]))
    
    report_content = "# Test Report\n\n"
    report_content += "This report summarizes the results of LLM tests across different stacks.\n\n"

    # --- Detailed Test Results Table ---
    report_content += "## Detailed Test Results\n\n"
    headers = ["LLM", "Stack", "Status", "Cost", "Time (s)", "Steps", "Attempts", "Input Tokens", "Output Tokens"]
    table_data = []
    for r in results:
        status = "✅ Success" if r["success"] else "❌ Failure"
        cost = calculate_cost(r["llm"], r.get("input_tokens"), r.get("output_tokens")) # Pass llm_model to calculate_cost
        table_data.append([r["llm"], 
                           r["stack"],
                           status, 
                           f"{cost:.4f}", 
                           f"{r['duration']:.2f}", 
                           r["steps"], 
                           r["attempts"], 
                           r.get("input_tokens", "N/A"), 
                           r.get("output_tokens", "N/A")])

    report_content += tabulate(table_data, headers, tablefmt="github")
    report_content += "\n\n"

    # --- Stack Success Summary Table ---
    report_content += "## Stack Success Summary\n\n"
    stack_success_counts = {}
    for r in results:
        if r["success"]:
            stack = r["stack"]
            stack_success_counts[stack] = stack_success_counts.get(stack, 0) + 1

    stack_summary_data = []
    for stack, count in stack_success_counts.items():
        stack_summary_data.append([stack, count])

    stack_summary_headers = ["Stack", "Successful LLMs"]
    report_content += tabulate(stack_summary_data, stack_summary_headers, tablefmt="github")
    report_content += "\n\n"

    # --- LLM Success Summary Table ---
    report_content += "## LLM Success Summary\n\n"
    llm_success_counts = {}
    for r in results:
        if r["success"]:
            llm = r["llm"]
            llm_success_counts[llm] = llm_success_counts.get(llm, 0) + 1

    llm_summary_data = []
    for llm, count in llm_success_counts.items():
        llm_summary_data.append([llm, count])

    llm_summary_headers = ["LLM", "Successful Stacks"]
    report_content += tabulate(llm_summary_data, llm_summary_headers, tablefmt="github")
    report_content += "\n\n"

    # --- Top 10 Cheapest Succesful Tests ---
    report_content += "## Top 10 Cheapest (not free) Successful Tests\n\n"
    cheapest_successful_tests = []
    for r in results:
        if r["success"]:
            cost = calculate_cost(r["llm"], r.get("input_tokens"), r.get("output_tokens"))
            if cost > 0:
                cheapest_successful_tests.append([r["llm"], r["stack"], cost])
    cheapest_successful_tests = sorted(cheapest_successful_tests, key=lambda x: x[2])[0:10]
    cheapest_successful_tests_headers = ["LLM", "Stack", "Cost"]
    report_content += tabulate(cheapest_successful_tests, cheapest_successful_tests_headers, tablefmt="github")
    report_content += "\n\n"

    # --- Top 10 Fastest Succesful Tests ---
    report_content += "## Top 10 Fastest Successful Tests\n\n"
    fastest_successful_tests = []
    for r in results:
        if r["success"]:
            fastest_successful_tests.append([r["llm"], r["stack"], f"{r['duration']:.2f}"])
    fastest_successful_tests = sorted(fastest_successful_tests, key=lambda x: float(x[2]))[0:10]
    fastest_successful_tests_headers = ["LLM", "Stack", "Time (s)"]
    report_content += tabulate(fastest_successful_tests, fastest_successful_tests_headers, tablefmt="github")
    report_content += "\n\n"
    # --- Write Report to File ---
    try:
        with open(REPORT_FILE, "w", encoding="utf-8") as report_file:
            report_file.write(report_content)
        print(f"Report generated successfully at {REPORT_FILE}")
    except Exception as e:
        print(f"Error writing report to file: {e}")

if __name__ == "__main__":
    generate_report()
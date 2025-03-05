#!/bin/bash
set -e

cd /app/google_gemini_2_0_flash_001

# Add dependencies
mix local.hex --force
mix local.rebar --force
mix deps.get

# Configure database
echo "Database configuration..."
DATABASE_URL="ecto://postgres:postgres@host.docker.internal:5432/test_google_gemini_2_0_flash_001_elixir"
MIX_ENV=dev ecto.create --url "$DATABASE_URL"
MIX_ENV=dev ecto.migrate --url "$DATABASE_URL"

echo "Database setup complete."

echo "Listing files:"
ls -l

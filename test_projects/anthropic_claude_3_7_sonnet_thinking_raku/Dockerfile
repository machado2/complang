
FROM rakudo/rakudo:latest

WORKDIR /app

# Install PostgreSQL client libraries needed for DB::Pg
RUN apt-get update &&     apt-get install -y --no-install-recommends         libpq-dev         gcc         libc6-dev         make         &&     apt-get clean &&     rm -rf /var/lib/apt/lists/*

# Install zef package manager
RUN apt-get update &&     apt-get install -y --no-install-recommends         git         &&     apt-get clean &&     rm -rf /var/lib/apt/lists/*

# Install all required dependencies separately with full testing disabled
RUN zef install --/test Cro::HTTP &&     zef install --/test DB::Pg &&     zef install --/test JSON::Fast

# Copy the app file
COPY app.raku .

# Expose the port the app runs on
EXPOSE 8080

# Command to run the app
CMD ["raku", "app.raku"]

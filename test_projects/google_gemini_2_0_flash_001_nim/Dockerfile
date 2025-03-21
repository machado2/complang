
FROM nimlang/nim:alpine

WORKDIR /app

# Create .nimble directory
RUN mkdir -p /app/.nimble/pkgs

# Set NIM_PATH
ENV NIM_PATH=/app/.nimble/pkgs

# Install pkg-config
RUN apk add --no-cache pkgconfig

# Install dependencies
RUN nimble install -y jester
RUN nimble install -y jsony
RUN nimble install -y db_connector

# List NIM_PATH and its contents
RUN echo "NIM_PATH: $NIM_PATH" && ls -l $NIM_PATH

# Copy source files
COPY . .

# Compile the application
RUN nim c -o:app app.nim

# Expose port 8080
EXPOSE 8080

# Set the entrypoint
CMD ["./app"]

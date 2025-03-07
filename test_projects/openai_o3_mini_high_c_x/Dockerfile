# Use ASP.NET Core runtime as the base image for the runtime container
FROM mcr.microsoft.com/dotnet/aspnet:6.0 AS base
WORKDIR /app
EXPOSE 8080

# Build stage: use the .NET SDK to build the project
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["CrudApi.csproj", "./"]
RUN dotnet restore "./CrudApi.csproj"
COPY . .
RUN dotnet publish "CrudApi.csproj" -c Release -o /app/publish

# Final stage: build the runtime image
FROM base AS final
WORKDIR /app
COPY --from=build /app/publish .
# Set the URL binding to port 8080
ENV ASPNETCORE_URLS=http://+:8080
ENTRYPOINT ["dotnet", "CrudApi.dll"]

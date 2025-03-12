# Stage 1: Build
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["ComplangApi.csproj", "./"]
RUN dotnet restore "ComplangApi.csproj"
COPY . .
RUN dotnet publish "ComplangApi.csproj" -c Release -o /app/publish

# Stage 2: Run
FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app
COPY --from=build /app/publish .
EXPOSE 8080
ENTRYPOINT ["dotnet", "ComplangApi.dll"]

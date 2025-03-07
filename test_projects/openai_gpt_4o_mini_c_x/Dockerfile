# Use the official .NET SDK image
FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS base
WORKDIR /app
EXPOSE 8080

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["MyApi.csproj", "./"]
RUN dotnet restore "MyApi.csproj"
COPY . .
WORKDIR "/src/."
RUN dotnet build "MyApi.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "MyApi.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .

ENV PGPASSWORD=$PGPASSWORD

ENTRYPOINT ["dotnet", "MyApi.dll"]


FROM mcr.microsoft.com/dotnet/aspnet:7.0 AS base
WORKDIR /app
EXPOSE 8080

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["complang_api.csproj", "."]
RUN dotnet restore "./complang_api.csproj"
COPY . .
WORKDIR "/src/."
RUN dotnet build "complang_api.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "complang_api.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "complang_api.dll"]

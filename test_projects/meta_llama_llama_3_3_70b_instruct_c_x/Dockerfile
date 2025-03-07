
FROM mcr.microsoft.com/dotnet/core/sdk:3.1
WORKDIR /app
COPY *.csproj ./
RUN dotnet restore
COPY . .
RUN dotnet publish -c Release -o out
EXPOSE 8080
ENV ASPNETCORE_URLS=http://*:8080
ENV PGPASSWORD=Saloon5-Moody-Observing
ENTRYPOINT ["dotnet", "out/ComplangApi.dll"]

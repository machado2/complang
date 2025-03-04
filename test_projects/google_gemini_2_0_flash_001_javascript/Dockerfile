
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./

RUN npm install

COPY . .

EXPOSE 8080

ENV POSTGRES_HOST=host.docker.internal
ENV POSTGRES_PORT=5432
ENV POSTGRES_DB=test_google_gemini_2_0_flash_001_javascript
ENV POSTGRES_USER=postgres
ENV PGPASSWORD=${PGPASSWORD}

CMD ["npm", "start"]

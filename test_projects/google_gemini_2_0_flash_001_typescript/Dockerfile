FROM docker.io/library/node:18-alpine

WORKDIR /app

COPY package*.json ./

RUN npm install
RUN npm i --save-dev @types/pg

COPY . .

RUN npm run build

EXPOSE 8080

CMD ["node", "dist/index.js"]
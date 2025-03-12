
FROM python:3.9-slim-buster

WORKDIR /app

COPY app.py .

RUN pip install flask psycopg2-binary

EXPOSE 8080

CMD ["python", "app.py"]

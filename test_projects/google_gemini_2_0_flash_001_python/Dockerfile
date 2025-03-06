FROM tiangolo/uvicorn-gunicorn-fastapi:python3.9

COPY main.py /app/main.py

WORKDIR /app

ENV PGPASSWORD=Saloon5-Moody-Observing

RUN pip install psycopg2-binary

EXPOSE 8080

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8080"]
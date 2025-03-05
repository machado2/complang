FROM elixir:1.14

WORKDIR /app

COPY create_elixir_app.sh .
RUN chmod +x create_elixir_app.sh
RUN ./create_elixir_app.sh

COPY setup_db.sh .
RUN chmod +x setup_db.sh
RUN ./setup_db.sh

COPY start_server.sh ./google_gemini_2_0_flash_001/

RUN chmod +x ./google_gemini_2_0_flash_001/start_server.sh

EXPOSE 8080
CMD ["./google_gemini_2_0_flash_001/start_server.sh"]
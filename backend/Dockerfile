FROM debian:stretch

WORKDIR /app

RUN apt update && apt install libgmp10

COPY main /app/main

CMD /app/main

FROM debian:stretch

WORKDIR /app

RUN apt-get update && apt-get install libgmp10

COPY main /app/main

CMD /app/main

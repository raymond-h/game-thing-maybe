FROM debian:stretch

WORKDIR /app

RUN apt-get update && apt-get install libgmp10 netbase ca-certificates

COPY main /app/main

CMD /app/main

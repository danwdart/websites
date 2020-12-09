FROM fpco/stack-build:latest
ENV STACK_ROOT=/home/stackage/.stack
WORKDIR /app
COPY . .
RUN stack build
CMD ["stack", "run"]

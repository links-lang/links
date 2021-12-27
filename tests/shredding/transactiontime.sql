DROP TABLE IF EXISTS transactiontime;

CREATE TABLE transactiontime (
    "name" text,
    "address" text,
    "from_time" timestamp with time zone,
    "to_time" timestamp with time zone
);

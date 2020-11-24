CREATE TABLE `visits` (
    `id` INT PRIMARY KEY AUTOINCREMENT,
    `url` TEXT,
    `ua` TEXT,
    `ip` TEXT,
    `time` TIMESTAMP
);
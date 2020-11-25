CREATE DATABASE `visits`;
GRANT ALL ON `visits`.`*` TO `visits`;
CREATE TABLE IF NOT EXISTS `visits`.`visits` (
    `id` INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `url` TEXT NOT NULL,
    `ua` TEXT NOT NULL,
    `ip` TEXT NOT NULL,
    `time` TIMESTAMP NOT NULL
);